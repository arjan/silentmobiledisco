-module(smd_admin).
-behaviour(ng_ws_handler).

-export([ws_call/5, ws_cast/4]).


%%====================================================================
%% WebSocket API functions
%%====================================================================

ws_call(_Call, _Args, _From, _ReplyId, _Context) ->
    lager:warning("Unknown call: ~p", [_Call]),
    {reply, ok}.



ws_cast(attach_highscores, [], From, Context) ->
    z_notifier:observe(disco_highscores, From, Context),
    smd_disco:broadcast_highscores(Context);

ws_cast(broadcast, [{"message", Message}], _From, Context) ->
    Pids = player_pids(Context),
    lists:foreach(fun(Pid) ->
                          smd_disco:send(Pid, [{message, Message}])
                     end,
                  Pids),
    lager:warning("Broadcasted message: ~p", [Message]),
    ok;

ws_cast(disco_end, [], _From, Context) ->
    Ids = active_players(Context),
    m_config:set_value(silentmobiledisco, disco_ended, true, Context),
    Song = hd(z_search:query_([{cat, audio}, {sort, "random"}], Context)),
    lists:foreach(fun(Id) ->
                          m_disco_player:set_final_song(Id, Song, Context),
                          smd_disco:send_player_state(Id, Context)
                  end,
                  Ids),
    lager:warning("Queued final song!"),
    ok;

ws_cast(disco_reset, [], _From, Context) ->
    Pids = player_pids(Context),
    m_config:set_value(silentmobiledisco, disco_ended, false, Context),
    m_disco_player:reset(Context),
    m_disco_log:reset(Context),
    lists:foreach(fun(Pid) ->
                          smd_disco:send(Pid, [{disco_reset, true}])
                     end,
                  Pids),
    ok.




%%====================================================================
%% support functions
%%====================================================================



active_players(Context) ->
    All = z_db:q("SELECT id, props FROM disco_player WHERE connected = true", Context),
    lists:foldl(fun({Id, Props}, Acc) ->
                        Pid = proplists:get_value(ws, Props),
                        case is_pid(Pid) and erlang:is_process_alive(Pid) of
                            true ->
                                [Id|Acc];
                            false ->
                                Acc
                        end
                end,
                [], All).

player_pids(Context) ->
    All = z_db:q("SELECT id, props FROM disco_player WHERE connected = true", Context),
    lists:foldl(fun({_, Props}, Acc) ->
                        Pid = proplists:get_value(ws, Props),
                        case is_pid(Pid) and erlang:is_process_alive(Pid) of
                            true ->
                                [Pid|Acc];
                            false ->
                                Acc
                        end
                end,
                [], All).
