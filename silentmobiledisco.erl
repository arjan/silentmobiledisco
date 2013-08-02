%% @author Arjan Scherpenisse
%% @copyright 2013 Arjan Scherpenisse
%% Generated on 2013-07-26
%% @doc This site was based on the 'empty' skeleton.

%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(silentmobiledisco).
-author("Arjan Scherpenisse").
-behaviour(gen_server).

-mod_title("silentmobiledisco zotonic site").
-mod_description("An empty Zotonic site, to base your site on.").
-mod_prio(10).

-include_lib("zotonic.hrl").


%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-export([client_removed/1]).

-export([ws_call/4, ws_cast/4]).

-record(clientstate, {status, socket, play_id, connected_to}).

-record(state, {context, clients}).


%%====================================================================
%% support functions go here
%%====================================================================


%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    gen_server:start_link({local, name(Context)}, ?MODULE, Args, []).


ws_cast(disco_start, [], From, Context) ->
    lager:warning("disco start: ~p", [cookie(Context)]),
    gen_server:call(name(Context), {client_added, cookie(Context), From});

ws_cast(disco_stop, [], _From, Context) ->
    lager:warning("disco stop: ~p", [cookie(Context)]),
    gen_server:call(name(Context), {client_removed, cookie(Context)});

ws_cast(disco_skip, [], _From, Context) ->
    lager:warning("disco skip: ~p", [self()]),
    gen_server:call(name(Context), {set_waiting, cookie(Context)});

ws_cast(disco_buffering_done, [], _From, Context) ->
    gen_server:call(name(Context), {buffering_done, cookie(Context)});

ws_cast(set_session, KeyValues, _From, Context) ->
    lists:foreach(
      fun ({K, null}) -> z_context:set_session(list_to_atom(K), undefined, Context);
          ({K, V}) -> z_context:set_session(list_to_atom(K), V, Context) 
      end,
      KeyValues).

ws_call(rsc, [{"id", IdStr}], _From, Context) ->
    lager:warning("IdStr: ~p", [IdStr]),
    case m_rsc:rid(IdStr, Context) of
        undefined ->
            error;
        Id when is_integer(Id) ->
            z_convert:to_json(m_rsc_export:full(Id, Context))
    end;

ws_call(Cmd, _, _, _) ->
    unknown_call.

client_removed(Context) ->
    gen_server:call(name(Context), {client_removed, cookie(Context)}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    {ok, #state{
       context=z_context:new(Context),
       clients=orddict:new()
      }}.

%% @doc Trap unknown calls
handle_call({client_added, Sid, Pid}, _From, State) ->
    State1 = add_client(Sid, Pid, State),
    report_state(State1),
    {reply, ok, State1};

handle_call({client_removed, Sid}, _From, State) ->
    State1 = remove_client(Sid, State),
    report_state(State1),
    {reply, ok, State1};

handle_call({set_waiting, Pid}, _From, State) ->
    State1 = set_client_waiting(Pid, State),
    report_state(State1),
    {reply, ok, State1};

handle_call({buffering_done, Pid}, _From, State) ->
    State1 = set_buffering_done(Pid, State),
    report_state(State1),
    {reply, ok, State1};

handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.



%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================



set_client_waiting(Pid,State=#state{clients=C, context=Context}) ->
    {ok, ClientState} = orddict:find(Pid, C),
    case ClientState#clientstate.status of
        waiting ->
            %% no change
            State;
        S when S =:= buffering; S =:= playing ->
            %% set waiting, and set other client also waiting
            Clients1 = set_waiting(Pid, C, Context),
            Clients2 = set_waiting(ClientState#clientstate.connected_to, Clients1, Context),
            State#state{clients=Clients2}
    end.

add_client(Sid,Pid,State=#state{clients=C, context=Context}) ->
    S = #clientstate{status=waiting,socket=Pid},
    NewClients = orddict:store(Sid, S, C),
    send_client_state(Pid, S, Context),
    NewClients1 = try_pair(NewClients, Context),
    State#state{clients=NewClients1}.

remove_client(Sid,State=#state{clients=Clients, context=Context}) ->
    case orddict:find(Sid, Clients) of
        {ok, ClientState} ->        
            Clients1 = orddict:erase(Sid, Clients),
            Clients2 = case ClientState#clientstate.status of
                           waiting ->
                               %% no other client connected
                               Clients1;
                           S when S =:= buffering; S =:= playing ->
                               set_waiting(ClientState#clientstate.connected_to, Clients1, Context)
                       end,
            Clients3 = try_pair(Clients2, Context),
            State#state{clients=Clients3};
        error ->
            State
    end.

set_buffering_done(Sid, State=#state{clients=Clients, context=Context}) ->
    {ok, CS} = orddict:find(Sid, Clients),
    CS1 = CS#clientstate{
            status=playing
           },
    Clients1 = orddict:store(Sid, CS1, Clients),
    {ok, OtherCS} = orddict:find(CS1#clientstate.connected_to, Clients1),

    case OtherCS#clientstate.status of
        playing ->
            %% send to both at same time, causing synchronized playback
            send_client_state(CS1#clientstate.socket, CS1, Context),
            send_client_state(OtherCS#clientstate.socket, OtherCS, Context);
        buffering ->
            nop
    end,
    State#state{clients=Clients1}.

name(Context) ->
    z_utils:name_for_host(?MODULE, z_context:site(Context)).

send_client_state(Pid, S, Context) ->
    controller_websocket:websocket_send_data(Pid, encode_client_state(S, Context)).


encode_client_state(#clientstate{status=buffering, play_id=Id}, Context) ->
    M = m_media:get(Id, Context),
    mochijson:encode({struct, [{status, buffering},
                               {title, z_trans:trans(m_rsc:p(Id, title, Context), Context)},
                               {filename, proplists:get_value(filename, M)}
                              ]});

encode_client_state(#clientstate{status=S}, _) ->
    mochijson:encode({struct, [{status, S}]}).

try_pair(Clients, Context) ->
    case find_pair(Clients) of
        {A, B} when A =/= undefined andalso B =/= undefined ->
            %% find random song
            Id = find_random(Context),
            %% set both clients to playing
            Clients1 = set_buffering(A, B, Id, Clients, Context),
            Clients2 = set_buffering(B, A, Id, Clients1, Context),
            Clients2;
        {_, _} ->
            Clients
    end.
                       
find_pair(Clients) ->
    lists:foldl(
      fun({A, #clientstate{status=waiting}}, {undefined, undefined}) ->
              {A, undefined};
         ({B, #clientstate{status=waiting}}, {A, undefined}) ->
              {A, B};
         ({_, #clientstate{}}, Acc) ->
              Acc
      end,
      {undefined, undefined},
      Clients).

set_buffering(Sid, OtherSid, Id, Clients, Context) ->
    {ok, State} = orddict:find(Sid, Clients),
    State1 = State#clientstate{
               status=buffering,
               play_id=Id,
               connected_to=OtherSid
              },
    send_client_state(State#clientstate.socket, State1, Context),
    orddict:store(Sid, State1, Clients).


set_waiting(Sid, Clients, Context) ->
    {ok, State} = orddict:find(Sid, Clients),
    State1 = State#clientstate{
               status=waiting,
               play_id=undefined,
               connected_to=undefined
              },
    send_client_state(State#clientstate.socket, State1, Context),
    orddict:store(Sid, State1, Clients).

              
find_random(Context) ->
    hd(z_search:query_([{cat, audio}, {sort, "random"}], Context)).


report_state(#state{clients=Clients}) ->
    {Waiting, Buffering, Playing} =
        lists:foldl(
          fun({_, #clientstate{status=waiting}}, {W, B, P}) ->
                  {W+1, B, P};
             ({_, #clientstate{status=buffering}}, {W, B, P}) ->
                  {W, B+1, P};
             ({_, #clientstate{status=playing}}, {W, B, P}) ->
                  {W, B, P+1}
             end,
             {0, 0, 0},
          Clients),
    lager:warning("~p clients, waiting: ~p, buffering: ~p, playing: ~p", [length(Clients), Waiting, Buffering, Playing]).

cookie(Context) ->
    z_context:get_cookie("z_sid", Context).
