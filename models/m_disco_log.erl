-module(m_disco_log).

-include("zotonic.hrl").
-define(table, disco_log).

-export(
   [
    init/1,
    add/3,
    get_score/2,
    history/1,
    history/3,
    highscores/1
   ]).

init(Context) ->
    case z_db:table_exists(?table, Context) of
        false ->
            z_db:create_table(
              ?table, 
              [
               #column_def{name=player_id, type="character varying", length=32, is_nullable=false},
               #column_def{name=player_name, type="character varying", length=255, is_nullable=true},
               #column_def{name=event, type="character varying", length=255, is_nullable=false},
               #column_def{name=score, type="integer", is_nullable=true},
               #column_def{name=time, type="timestamp", is_nullable=false},
               #column_def{name=props, type="bytea", is_nullable=true}
              ], Context);
        true -> nop
    end,
    ok.

add(EventType, Props, Context) ->
    z_db:insert(?table, [{event, EventType}, {time, calendar:local_time()}|Props], Context).

get_score(Id, Context) ->
    case z_db:q1("SELECT SUM(score) FROM disco_log WHERE player_id = $1", [Id], Context) of
        undefined ->
             0;
        S ->
            S
    end.

history(Context) ->
    history(0, 10, Context).

history(Offset, Limit, Context) ->
    z_db:assoc("SELECT * FROM disco_log ORDER BY time DESC LIMIT $1 OFFSET $2", [Limit, Offset], Context).

highscores(Context) ->
    z_db:assoc("SELECT player_name, SUM(score) FROM disco_log GROUP BY player_name ORDER BY SUM(score) DESC", Context).
    
