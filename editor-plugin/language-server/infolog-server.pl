:- module('infolog-server', [start_server/1]).

:- use_module(library(sockets)).
:- use_module(library(lists)).
:- use_module(json).
:- use_module('infolog-handlers').

% Connection handling
start_server :-
    format("Starting Infolog server~n", []),
    socket_server_open(inet('',Port), Socket, [loopback(true)]),
    format("Listening on port ~s~n", [Port]),
    flush_output(user_output),
    socket_server_accept(Socket, _, Stream, [type(binary)]),
    handle_connection(Stream),
    socket_server_close(Socket).

handle_connection(Stream) :-
    peek_byte(Stream, B), B \= -1,
    read_single_message(Stream, Message),
    handle_message(Message, Response),
    send_response(Stream, Response),
    flush_output(user_output),
    flush_output(Stream),
    handle_connection(Stream).
handle_connection(Stream) :-
    peek_byte(Stream, B), B = -1,
    format("Connection closed.~n", []),
    flush_output(user_output).

% Message parsing
read_single_message(Stream, Message) :-
    read_header(Stream, Header, []),
    read_content(Stream, Header, Content),
    format("Received: ~s~n", [Content]),
    parse_json(Content, Message).

%% Header
read_header(Stream, Header, PreviousCodes) :-
    peek_byte(Stream, B),
    [B] = "\n", [R] = "\r",
    PreviousCodes = [R, B, R|_],
    get_byte(Stream, B),
    reverse([B|PreviousCodes], HeaderText),
    phrase(header(Header), HeaderText).
read_header(Stream, Header, PreviousCodes) :-
    get_byte(Stream, B),
    read_header(Stream, Header, [B|PreviousCodes]).

header(content-length(N)) --> "Content-Length: ", json_number(N), "\r\n\r\n".

content_length(content-length(ContentLength), ContentLength).

%% Content
read_content(Stream, Header, Content) :-
    content_length(Header, ContentLength),
    read_n_bytes(Stream, ContentLength, Content, []).

read_n_bytes(_, 0, Out, Acc) :- reverse(Acc, Out).
read_n_bytes(Stream, N, Out, Acc) :-
    get_byte(Stream, B),
    Nn is N - 1,
    read_n_bytes(Stream, Nn, Out, [B|Acc]).

% Message handling
handle_message(Message, Response) :-
    jsonrpc_message(Message, Method, ID, Params),
    message_handler(Method, Params, ID, Result, Error),
    jsonrpc_response(Result, Error, ID, Response).

jsonrpc_message(Message, Method, ID, Params) :-
    json_object_get(Message, 'method', Method),
    json_object_get(Message, 'id', ID),
    json_object_get(Message, 'params', Params).

jsonrpc_response(null, Error, ID, Response) :-
    json_object_create(
        ['jsonrpc'-'2.0',
         'id'-ID,
         'error'-Error], Response).
jsonrpc_response(Result, null, ID, Response) :-
    json_object_create(
        ['jsonrpc'-'2.0',
         'id'-ID,
         'result'-Result], Response).

send_response(Stream, Response) :-
    parse_json(ResponseStr, Response),
    write_bytes(Stream, ResponseStr).

write_bytes(_, []).
write_bytes(Stream, [Byte|Bytes]) :-
    put_byte(Stream, Byte),
    write_bytes(Stream, Bytes).