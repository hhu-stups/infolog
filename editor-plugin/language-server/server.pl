%:- module(language-server, []).

:- use_module(library(sockets)).
:- use_module(library(lists)).
:- use_module(json).

test_predicate :-
    format("Test goal successfully executed", []).

start_server :-
    Port = '11833',
    format("Starting Infolog server~n", []),
    socket_server_open(inet('',Port), Socket, [loopback(true)]),
    format("Listening on port ~s~n", [Port]),
    flush_output(user_output),
    %socket_server_accept(Socket, _, Stream, [type(text), encoding('UTF-8')]),
    socket_server_accept(Socket, _, Stream, [type(binary)]),
    handle_connection(Stream),
    socket_server_close(Socket).

handle_connection(Stream) :-
    peek_byte(Stream, B), B \= -1,
    read_single_message(Stream, Message),
    handle_message(Message, Response),
    send_response(Stream, Response),
    format("Request handled~n", []),
    flush_output(user_output),
    handle_connection(Stream).

handle_connection(Stream) :-
    peek_byte(Stream, B), B = -1,
    format("Connection closed.~n", []),
    flush_output(user_output).

read_single_message(Stream, Message) :-
    read_header(Stream, Header, []),
    Header = content-length(C),
    format("Content length is ~d~n", [C]).
    %read_content(Stream, Header, Content),
    %portray_content(Content).

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

handle_message(_,_).
send_response(_,_).
