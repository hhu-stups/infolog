:- module(infolog_tools, [print_location/1, print_information/1,
                          add_infolog_error/1,
                          unop/3, binop/4, ternop/5,
                          pairs_to_list/2
                          ]).

% various utilities:

% print location info
print_location(module_lines(Module,From,To)) :- !, format(' in ~w [~w - ~w] ',[Module,From,To]).
print_location(module_pred_lines(Module,Predicate,From,To)) :- !, format(' in ~w [~w - ~w defining ~w] ',[Module,From,To,Predicate]).
print_location(module_loc(Module)) :- !, format(' in module ~w ',[Module]).
print_location(unknown) :- !.
print_location(E) :- add_infolog_error(informat('Illegal location: ~w',E)).

% print an (error) information term
print_information(Info) :- print_information(Info,user_output).
print_information(informat(Msg,Args),Stream) :- !, format(Stream,Msg,Args).
print_information(string(Msg),Stream) :- !, format(Stream,'~w',[Msg]).  % we could also simply use informat(Msg,[]) instead
print_information(E,_Stream) :- add_infolog_error(informat('Illegal information: ~w',E)).

% register an internal error
add_infolog_error(T) :- format(user_error,'~n*** INTERNAL ERROR: ',[]),
   print_information(T,user_error),
   format(user_error,'~n',[]).



% utilities to construct/deconstruct terms; faster than =..
unop(X,P,A1) :- functor(X,P,1), arg(1,X,A1).
binop(X,P,A1,A2) :- functor(X,P,2), arg(1,X,A1), arg(2,X,A2).
ternop(X,P,A1,A2,A3) :- functor(X,P,3), arg(1,X,A1), arg(2,X,A2), arg(3,X,A3).

pairs_to_list((X,Y), [X|R]) :- pairs_to_list(Y,R).
pairs_to_list(X, [X]).