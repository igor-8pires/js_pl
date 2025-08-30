:- module(server,[]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/websocket)).

http_server(http_dispatch, [port(12345)]).

http_handler(root(.),          http_reply_file('index.html', []), []).
http_handler(root('Board.js'),  http_reply_file('Board.js',    []), []).
http_handler(root(time),
    http_upgrade_to_websocket(time_handler, []),
    [spawn([])]).

time_handler(WebSocket) :-
    ws_receive(WebSocket, Message, [format(json)]),  % This waits until a message is received.
    (
        Message.opcode == close
    ->
        true
    ;
        time_response(Response_dict),
        ws_send(WebSocket, json(Response_dict)),
        time_handler(WebSocket)
    ).

time_response(Response_dict) :-
    get_time(Time),
    format_time(string(Message), '%c', Time),
    dict_create(Data, _, [message-Message]),
    dict_create(Response_dict, _, [data-Data,format-json,opcode-text]).