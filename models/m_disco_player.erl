-module(m_disco_player).

-include("zotonic.hrl").
-define(table, disco_player).

-export(
   [
    init/1,
    get/2,
    set/3,
    set/4,
    insert/3
   ]).

init(Context) ->
    z_db:create_table(?table, 
                      [
                       #column_def{name=id, type="character varying", length=32, is_nullable=true, primary_key=true},
                       #column_def{name=name, type="character varying", length=255, is_nullable=false},
                       #column_def{name=connected, type="boolean", is_nullable=true}
                       #column_def{name=song_id, type="integer", is_nullable=true}
                       #column_def{name=has_scored, type="boolean", is_nullable=true}
                       #column_def{name=connected_to, type="string", length=32, is_nullable=true}
                      ], Context),
    ok.

insert(Id, Props, Context) ->
    case get(Id, Context) of
        {ok, []} ->
             nop;
        {ok, _} ->
            z_db:delete(?table, Id, Context)
    end,
    z_db:insert(?table, Id, Props, Context).

get(Id, Context) ->
    z_db:select(?table, Id, Context).

set(Id, Key, Value, Context) ->
    set(Id, [{Key, Value}], Context).

set(Id, Props, Context) ->
    z_db:update(?table, Id, Props, Context).






