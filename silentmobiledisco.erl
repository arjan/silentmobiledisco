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
-export([init/1, poll/1]).

%%====================================================================
%% WS API
%%====================================================================

init(Context) ->
    case whereis(disco_poller) of
        undefined -> register(disco_poller, spawn_link(fun() -> poll(Context) end));
        _ -> nop
    end,
    ok.

poll(Context) ->
    timer:sleep(5000),
    All = z_db:q("SELECT id, props FROM disco_player WHERE connected = true", Context),
    lists:foreach(fun({Id, Props}) ->
                          case proplists:get_value(ws, Props) of
                              undefined -> nop;
                              P when is_pid(P) ->
                                  case erlang:is_process_alive(P) of
                                      true ->
                                          nop;
                                      false ->
                                          smd_disco:player_stop(Id, Context)
                                  end
                          end
                  end,
                  All),
    ?MODULE:poll(Context).
