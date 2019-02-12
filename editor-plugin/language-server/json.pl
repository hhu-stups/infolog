:- module(json, [parse_json/2, json_number/3]).

:- use_module(library(assoc)).

parse_json(J, S) :- phrase(json_value(J), S).

json_value(V) --> json_object(V).
json_value(V) --> json_array(V).
json_value(V) --> json_number(V).
json_value(V) --> json_string(V).
json_value(V) --> json_true(V).
json_value(V) --> json_false(V).
json_value(V) --> json_null(V).

% Literals
json_true(true) --> "true".

json_false(false) --> "false".

json_null(null) --> "null".

% Objects
json_object(O) --> ws, "{", object_members(O), "}", ws.

object_members(M) --> [],
                    { list_to_assoc([], M) }.
object_members(M) --> ws, json_string(S), ws, ":", ws, json_value(V), ws,
                    { list_to_assoc([S-V], M) }.
object_members(M) --> ws, json_string(S), ws, ":", ws, json_value(V), ws, ",", object_members(Ms),
                    { put_assoc(S, Ms, V, M) }.

% Arrays
json_array(A) --> ws, "[", array_values(A), "]", ws.

array_values([])     --> [].
array_values([V])    --> ws, json_value(V), ws.
array_values([V|Vs]) --> ws, json_value(V), ws, ",", array_values(Vs).

% Numbers
% sanity checking is basic but should suffice since all input comes from our code
json_number(N) --> ws, number_digits(D), ws, { number_codes(N, D) }.

number_digits([D|Ds]) --> [D], number_digits(Ds), { valid_digit(D) }.
number_digits([])     --> [].

is_digit(D) :- [L] = "0", [U] = "9", (D >= L, D =< U).

valid_digit(D) :- is_digit(D).
valid_digit(D) :- [D] = "e" ; [D] = "E" ; [D] = "+" ; [D] = "-" ; [D] = ".".

% Strings
json_string(S) --> ws, "\"", string_terminals(S), "\"", ws.

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

% Insignificant whitespace
ws --> wsp, ws.
ws --> "".

wsp --> "\t".
wsp --> "\n".
wsp --> "\r".
wsp --> " ".
