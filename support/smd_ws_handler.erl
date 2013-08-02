-module(smd_ws_handler).

-export([websocket_init/1,
         websocket_message/3,
         websocket_info/2,
         websocket_terminate/2]).

%% @doc Called when the websocket is initialized.
websocket_init(Context) ->
    ok.

%% @doc Called when a message arrives on the websocket.
websocket_message(<<"stop">>, From, Context) ->
    silentmobiledisco:set_waiting(From, Context),
    ok;

%% @doc Called when a message arrives on the websocket.
websocket_message(<<"buffering_done">>, From, Context) ->
    silentmobiledisco:buffering_done(From, Context),
    ok;

websocket_message(<<"pos ", T/binary>>, From, Context) ->
    silentmobiledisco:set_current_time(From, z_convert:to_float(T), Context),
    ok;

websocket_message(<<"call:", ReplyId:8/binary, ":", Call/binary>>, From, Context) ->
    [CmdBin, PayloadBin] = binary:split(Call, <<":">>),
    Cmd = list_to_existing_atom(binary_to_list(CmdBin)),
    {struct, Payload} = mochijson:decode(PayloadBin),
    Handler = z_context:get(callbacks, Context),
    Reply = Handler:ws_call(Cmd, Payload, From, Context),
    Msg = mochijson:encode({struct, [{reply_id, ReplyId}, {reply, Reply}]}),
    controller_websocket:websocket_send_data(From, Msg),
    ok;

websocket_message(<<"cast:", Cast/binary>>, From, Context) ->
    [CmdBin, PayloadBin] = binary:split(Cast, <<":">>),
    Cmd = list_to_existing_atom(binary_to_list(CmdBin)),
    {struct, Payload} = mochijson:decode(PayloadBin),
    Handler = z_context:get(callbacks, Context),
    Handler:ws_cast(Cmd, Payload, From, Context),
    ok;

%% @doc Called when a message arrives on the websocket.
websocket_message(Msg, _From, _Context) ->
    lager:warning("Unhandled incoming message: ~p", [Msg]),
    ok.

websocket_info(Msg, _Context) ->
    lager:warning("info: ~p", [Msg]),
    ok.

%% @doc Called when the websocket terminates.
websocket_terminate(_Reason, Context) ->
    silentmobiledisco:client_removed(Context),
    ok.
