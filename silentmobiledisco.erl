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

-mod_title("silentmobiledisco zotonic site").
-mod_description("An empty Zotonic site, to base your site on.").
-mod_prio(10).

-include_lib("zotonic.hrl").

%% WS exports
-export([ws_call/4, ws_cast/4, ws_opened/2, ws_closed/2]).


%%====================================================================
%% WS API
%%====================================================================


ws_cast(disco_start, [{"name", Name}], From, Context) ->
    Player = player_id(Context),
    lager:warning("disco start: ~p", [Name]),
    m_disco_player:insert(
      Player,
      [{connected, true},
       {status, waiting},
       {secret_code, secret_code()},
       {ws, From},
       {name, Name}],
      Context),
    send_player_state(Player, Context),
    ok;

ws_cast(set_session, KeyValues, _From, Context) ->
    lists:foreach(
      fun ({K, null}) -> z_context:set_session(list_to_atom(K), undefined, Context);
          ({K, V}) -> z_context:set_session(list_to_atom(K), V, Context) 
      end,
      KeyValues).

ws_call(disco_init, [], _From, Context) ->
    case m_disco_player:get(player_id(Context), Context) of
        {ok, []} -> null;
        {ok, Player} ->
            proplists:get_value(name, Player)
    end;

ws_call(disco_stop, [], _From, Context) ->
    Player = player_id(Context),
    m_disco_player:set(Player, connected, false, Context),
    z_session:set(player_id, undefined, Context),
    ok;


ws_call(Cmd, _, _, _) ->
    unknown_call.

ws_opened(_From, Context) ->
    lager:warning("_From: ~p", [_From]),
    ok.

ws_closed(_From, Context) ->
    m_disco_player:set(player_id(Context), connected, false, Context),
    ok.



%%====================================================================
%% support functions
%%====================================================================


send_player_state(PlayerId, Context) ->
    {ok, Player} = m_disco_player:get(PlayerId, Context),
    WS = proplists:get_value(ws, Player),
    case is_pid(WS) andalso proplists:get_value(connected, Player) =:= true of
        true ->
            controller_websocket:websocket_send_data(WS, mochijson:encode(encode_player_json(Player, Context)));
        false ->
            nop
    end.

encode_player_json(Player, Context) ->
    ExtraProps = case proplists:get_value(status, Player) of
                     buffering ->
                         Id = proplists:get_value(song_id, Player),
                         M = m_media:get(Id, Context),
                         [{title, z_trans:trans(m_rsc:p(Id, title, Context), Context)},
                          {filename, proplists:get_value(filename, M)}];
                     _ ->
                         []
                 end,
    {struct, ExtraProps ++ proplists:delete(ws, Player)}.
              
find_random(Context) ->
    hd(z_search:query_([{cat, audio}, {sort, "random"}], Context)).

player_id(Context) ->
    case z_session:get(player_id, Context) of
        undefined ->
            Id = z_ids:id(),
            z_session:set(player_id, Id, Context),
            Id;
        I -> I
    end.

secret_code() ->
    [$0+random:uniform(9) || _ <- lists:seq(1,4)].
