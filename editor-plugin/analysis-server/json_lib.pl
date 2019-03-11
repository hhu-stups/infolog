:- module(json_lib, [parse_json/2,
                 json_object_create/2, json_object_get/3, json_object_put/4,
                 json_number/3]).

:- use_module(library(assoc)).
:- use_module(library(lists)).

parse_json(String, JsonValue) :- \+ var(String), var(JsonValue), phrase(json_value(JsonValue), String).
parse_json(String, JsonValue) :- var(String), \+ var(JsonValue), json_to_codes(JsonValue, String).

json_object_create(AList, Object) :- list_to_assoc(AList, Object).

json_object_get(Object, Key, Value) :-
    get_assoc(Key, Object, Value).

json_object_put(Object, Key, Value, NewObject) :-
    put_assoc(Key, Object, Value, NewObject).


% Codes -> Value
json_value(V) --> json_true(V).
json_value(V) --> json_false(V).
json_value(V) --> json_null(V).
json_value(V) --> json_object(V).
json_value(V) --> json_array(V).
json_value(V) --> json_number(V).
json_value(V) --> json_string(V).

%% Literals
json_true(true) --> "true".

json_false(false) --> "false".

json_null(null) --> "null".

%% Objects
json_object(O) --> ws, "{", object_members(O), "}", ws.

object_members(M) --> [],
                    { list_to_assoc([], M) }.
object_members(M) --> ws, json_string(S), ws, ":", ws, json_value(V), ws,
                    { list_to_assoc([S-V], M) }.
object_members(M) --> ws, json_string(S), ws, ":", ws, json_value(V), ws, ",", object_members(Ms),
                    { put_assoc(S, Ms, V, M) }.

%% Arrays
json_array(A) --> ws, "[", array_values(A), "]", ws.

array_values([])     --> [].
array_values([V])    --> ws, json_value(V), ws.
array_values([V|Vs]) --> ws, json_value(V), ws, ",", array_values(Vs).

%% Numbers
% sanity checking is basic but should suffice since all input comes from our code
json_number(N) --> ws, number_digits(D), ws, { D \= [], number_codes(N, D) }.

number_digits([D|Ds]) --> [D], number_digits(Ds), { valid_digit(D) }.
number_digits([])     --> [].

is_digit(D) :- [L] = "0", [U] = "9", (D >= L, D =< U).

valid_digit(D) :- is_digit(D).
valid_digit(D) :- [D] = "e" ; [D] = "E" ; [D] = "+" ; [D] = "-" ; [D] = ".".

%% Strings
json_string(S) --> ws, "\"", string_terminals(T), "\"", ws, { atom_codes(S, T) }.

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

%% Insignificant whitespace
ws --> wsp, ws.
ws --> "".

wsp --> "\t".
wsp --> "\n".
wsp --> "\r".
wsp --> " ".


% Value -> Codes
%% Literals
json_to_codes(true,  "true").
json_to_codes(false, "false").
json_to_codes(null,  "null").

%% Objects
json_to_codes(J, S) :-
    is_assoc(J),
    assoc_to_list(J, [First|Rest]),
    keyval_to_codes(First, FirstCodes),
    scanlist(scan_keyval_to_codes, Rest, FirstCodes, KeyValCodes),
    append(["{", KeyValCodes, "}"], S).
json_to_codes(J, S) :-
    is_assoc(J),
    assoc_to_list(J, []),
    S = "{}".

keyval_to_codes(Key-Val, Codes) :-
    json_to_codes(Val, ValCodes),
    atom_codes(Key, KeyCodes),
    append(["\"", KeyCodes, "\":", ValCodes], Codes).

scan_keyval_to_codes(KeyVal, Prev, Codes) :-
    keyval_to_codes(KeyVal, KeyValCodes),
    append([Prev, ",", KeyValCodes], Codes).

%% Arrays
json_to_codes(J, S) :-
    is_list(J),
    J = [First|Rest],
    json_to_codes(First, FirstCodes),
    scanlist(scan_values_to_codes, Rest, FirstCodes, ValCodes),
    append(["[", ValCodes, "]"], S).
json_to_codes(J, S) :-
    J = [],
    S = "[]".

scan_values_to_codes(Value, Prev, Codes) :-
    json_to_codes(Value, ValueCodes),
    append([Prev, ",", ValueCodes], Codes).

%% Numbers
json_to_codes(J, S) :-
    number(J),
    number_codes(J, S).

%% Strings
json_to_codes(J, S) :-
    atom(J),
    atom_codes(J, Codes),
    append(["\"", Codes, "\""], S).

%% Terms
json_to_codes(J, S) :-
    compound(J),
    J =.. [_, Term, Arity],
    atom_codes(Term, TermCodes),
    number_codes(Arity, ArityCodes),
    append(["\"", TermCodes, "/", ArityCodes, "\""], S).
