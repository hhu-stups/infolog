%:- module(json, []).

:- use_module(library(assoc)).

json_value(V) --> json_object(V).
json_value(V) --> json_array(V).
json_value(V) --> json_number(V).
json_value(V) --> json_string(V).
json_value(V) --> json_true(V).
json_value(V) --> json_false(V).
json_value(V) --> json_null(V).

% Objects
json_object(O) --> "{", json_members(O), "}".

json_members(M) --> [],
                    { list_to_assoc([], M) }.
json_members(M) --> json_string(S), ":", json_value(V),
                    { list_to_assoc([S-V], M) }.
json_members(M) --> json_string(S), ":", json_value(V), ",", json_members(Ms),
                    { put_assoc(S, Ms, V, M) }.

% Strings
json_string(S) --> "\"", string_terminals(S), "\"".

string_terminals([]) --> "".
string_terminals([T|Ts]) --> string_terminal(T), string_terminals(Ts).
string_terminals([T|Ts]) --> "\\", escape_character(T), string_terminals(Ts).

string_terminal(T) --> [T], { \+ forbidden_character([T]) }.

escape_character(C) --> "\"", { char_code('\"', C) }.
escape_character(C) --> "\\", { char_code('\\', C) }.
escape_character(C) --> "/",  { char_code('/',  C) }.
escape_character(C) --> "b",  { char_code('\b', C) }.
escape_character(C) --> "f",  { char_code('\f', C) }.
escape_character(C) --> "n",  { char_code('\n', C) }.
escape_character(C) --> "r",  { char_code('\r', C) }.
escape_character(C) --> "t",  { char_code('\t', C) }.

forbidden_character("\"").
forbidden_character("\\").


json_array([]) --> "glarb".
json_number([]) --> "glarb".
json_true(true) --> "true".
json_false(false) --> "false".
json_null(null) --> "null".
