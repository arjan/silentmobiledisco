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
-export([init/1, poll/1, ws_call/4, ws_cast/4, ws_opened/2, ws_closed/2]).


%%====================================================================
%% WS API
%%====================================================================

init(Context) ->
    case whereis(disco_poller) of
        undefined -> register(disco_poller, spawn_link(fun() -> poll(Context) end));
        _ -> nop
    end,
    ok.

ws_cast(disco_register, [{"name", Name}], From, Context) ->
    Player = player_id(Context),
    m_disco_player:insert(
      Player,
      [{connected, true},
       {status, registered},
       {connected_to, undefined},
       {ws, From},
       {name, Name}],
      Context),
    log("disco_register", [{player_id, Player}, {score, 0}], Context),
    broadcast_highscores(Context),
    ok;

ws_cast(disco_start, [], From, Context) ->
    Player = player_id(Context),
    m_disco_player:set(Player, [{connected, true}, {status, waiting}, {connected_to, undefined}, {ws, From}], Context),
    find_waiting(Player, Context),
    send_player_state(player_id(Context), Context),
    ok;

ws_cast(disco_buffering_done, [], _From, Context) ->
    m_disco_player:set(player_id(Context), [{status, playing}, {has_scored, false}, {secret_code, secret_code()}], Context),
    {ok, Other} = m_disco_player:get_other_player(player_id(Context), Context),
    case proplists:get_value(status, Other) of
        <<"playing">> ->
            send_player_state(player_id(Context), Context),
            send_player_state(proplists:get_value(id, Other), Context),
            ok;
        _ -> 
            ok
    end;

ws_cast(disco_attach_highscores, [], From, Context) ->
    z_notifier:observe(disco_highscores, From, Context),
    broadcast_highscores(Context),
    nop;

ws_cast(disco_song_end, [], _From, Context) ->
    Player = player_id(Context),
    Other = m_disco_player:get(Player, connected_to, Context),
    next_song(Player, Context),
    next_song(Other, Context),
    find_waiting(Player, Context);

ws_cast(disco_skip, [], _From, Context) ->
    skip_song(player_id(Context), Context).

ws_call(disco_connect, [], _From, Context) ->
    player_state(player_id(Context), Context);

ws_call(disco_init, [], _From, Context) ->
    Player = player_id(Context),
    m_disco_player:set(Player, [{status, registered}], Context),
    case m_disco_player:get(Player, Context) of
        {ok, []} -> null;
        {ok, PlayerProps} ->
            proplists:get_value(name, PlayerProps)
    end;

ws_call(disco_guess, [{"code", Code}], _From, Context) ->
    Player = player_id(Context),
    {ok, Other} = m_disco_player:get_other_player(Player, Context),
    case z_convert:to_list(proplists:get_value(secret_code, Other)) =:= Code of
        true ->
            PlayerB = proplists:get_value(id, Other),
            log("disco_score", [{player_id, Player}, {score, 10}], Context),
            log("disco_score", [{player_id, PlayerB}, {score, 10}], Context),
            m_disco_player:set(Player, [{has_scored, true}], Context),
            m_disco_player:set(PlayerB, [{has_scored, true}], Context),
            send_player_state(Player, Context),
            send_player_state(PlayerB, Context),
            true;
        false ->
            log("disco_score_fail", [{player_id, Player}, {score, -1}], Context),
            send_player_state(Player, Context),
            false
    end;
    
ws_call(disco_stop, [], _From, Context) ->
    player_stop(Context),
    z_session:set(player_id, undefined, Context),
    ok;

ws_call(_Cmd, _, _, _) ->
    unknown_call.

ws_opened(_From, _Context) ->
    ok.

ws_closed(_From, Context) ->
    player_stop(Context),
    ok.



%%====================================================================
%% support functions
%%====================================================================


send_player_state(PlayerId, Context) ->
    {ok, Player} = m_disco_player:get(PlayerId, Context),
    WS = proplists:get_value(ws, Player),
    case is_pid(WS) andalso proplists:get_value(connected, Player) =:= true of
        true ->
            controller_websocket:websocket_send_data(WS, mochijson:encode(player_state(PlayerId, Context)));
        false ->
            nop
    end.

player_state(PlayerId, Context) ->
    {ok, Player} = m_disco_player:get(PlayerId, Context),
    encode_player_json(Player, Context).
    
    
encode_player_json(Player, Context) ->
    ExtraProps = case proplists:get_value(status, Player) of
                     <<"buffering">> ->
                         Id = proplists:get_value(song_id, Player),
                         M = m_media:get(Id, Context),
                         [{title, z_trans:trans(m_rsc:p(Id, title, Context), Context)},
                          {filename, proplists:get_value(filename, M)}];
                     _ ->
                         []
                 end,
    {ok, Other} = m_disco_player:get(proplists:get_value(connected_to, Player), Context),
    {struct, [{connected_player, {struct, encode_player(Other)}}] ++ ExtraProps ++ encode_player(Player)}.

encode_player(Player) ->
    lists:map(fun({K, undefined}) -> {K, null}; (X) -> X end, proplists:delete(ws, Player)).
              
find_random_song(_PlayerId, Context) ->
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
    random:seed(erlang:now()),
    [$0+random:uniform(9) || _ <- lists:seq(1,4)].

log(Event, Props, Context) ->
    lager:warning(">> Event: ~p ~p", [Event, Props]),
    m_disco_log:add(Event, Props, Context).

find_waiting(Player, Context) ->
    SongId = find_random_song(Player, Context),
    case m_disco_player:find_and_connect(Player, SongId, Context) of
        undefined ->
            undefined;
        PlayerB ->
            log("song_start", [{player_id, Player}, {connected_to, PlayerB}, {song_id, SongId}], Context),
            send_player_state(PlayerB, Context)
    end,
    send_player_state(Player, Context).
    
player_stop(Context) ->
    player_stop(player_id(Context), Context).

player_stop(PlayerId, Context) ->
    {ok, Player} = m_disco_player:get(PlayerId, Context),
    case proplists:get_value(connected_to, Player) of
        undefined -> nop;
        B -> 
            m_disco_player:set(B, [{status, waiting}, {connected_to, undefined}], Context),
            case m_disco_player:get(B, connected, Context) of
                true -> find_waiting(B, Context);
                false -> nop
            end
    end,
    m_disco_player:set(PlayerId, [{ws, undefined}, {connected, false}], Context),
    broadcast_highscores(Context),
    log("disco_stop", [{player_id, PlayerId}], Context).    

skip_song(Player, Context) ->
    Other = m_disco_player:get(Player, connected_to, Context),

    log("disco_skip", [{player_id, Player}, {score, -1}], Context),

    m_disco_player:set(Player, [{status, waiting}, {connected_to, undefined}], Context),
    m_disco_player:set(Other, [{status, waiting}, {connected_to, undefined}], Context),
    send_player_state(Player, Context),
    send_player_state(Other, Context),
    find_waiting(Player, Context),
    ok.
    

next_song(Player, Context) ->
    case m_disco_player:get(Player, status, Context) of
        <<"playing">> ->
            log("disco_next", [{player_id, Player}, {score, 1}], Context),
            m_disco_player:set(Player, [{status, waiting}, {connected_to, undefined}], Context),
            send_player_state(Player, Context);
        _S ->
            lager:warning("Skip next_song for player in wrong state: ~p - ~p", [Player, _S])
    end.

broadcast_highscores(Context) ->
    z_notifier:notify({disco_highscores, m_disco_log:highscores(Context)}, Context).


poll(Context) ->
    timer:sleep(5000),
    All = z_db:q("SELECT id, props FROM disco_player WHERE connected = true", Context),
    lists:foreach(fun({Id, Props}) ->
                          case proplists:get_value(ws, Props) of
                              undefined -> nop;
                              P when is_pid(P) ->
                                  case erlang:is_process_alive(P) of
                                      true -> nop;
                                      false ->
                                          player_stop(Id, Context)
                                  end
                          end
                  end,
                  All),
    ?MODULE:poll(Context).
