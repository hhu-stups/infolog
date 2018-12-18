:- module(tools,[myprint/1]).

myprint(X) :- format(user_output,'Out: ~w~n',[X]).

unused.
