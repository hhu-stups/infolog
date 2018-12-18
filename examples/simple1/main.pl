:- module(main,[run/0]).


run :- print('TEST'),nl, run2.

:- use_module(tools,[myprint/1]).

run2 :- myprint(ok).

run3 :- not_used.
