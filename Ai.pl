:- module(server,[]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/websocket)).

:- initialization
    http_server(http_dispatch, [port(12345)]).

:- http_handler(root(.),          http_reply_file('index.html', []), []).
:- http_handler(root('board.html'), http_reply_file('board.html', []), []).
:- http_handler(root('Board.js'),  http_reply_file('Board.js',    []), []).
:- http_handler(root(ws),
    http_upgrade_to_websocket(check_board, []),
    [spawn([])]).


check_board(WebSocket) :-
    ws_receive(WebSocket, Message, [format(json)]),  % This waits until a message is received.
    (
        Message.opcode == close
    ->
        true
    ;
        check_board_response(Response_dict),
        ws_send(WebSocket, json(Response_dict)),
        check_board(WebSocket)
    ).

check_board_response(Response_dict) :-
    dict_create(Data, _, [status-'ok', valid-move-true]),
    dict_create(Response_dict, _, [data-Data,format-json,opcode-text]).

