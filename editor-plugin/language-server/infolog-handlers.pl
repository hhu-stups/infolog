:- module('infolog-handlers',
          [message_handler/5,
           handle_testmethod/4]).

:- use_module(json).
:- use_module('../../prolog-analyzer/analyzer').

% Dispatch
message_handler('analyzeFile', Params, ID, Result, Error) :- handle_analyzeFile(Params, ID, Result, Error).


% Handlers
handle_analyzeFile(Params, _, Result, null) :-
    json_object_get(Params, 'path', Path),
    format("In handler!~n", []),
    flush_output(user_output),
    analyze(Path),
    findall(
        ['Category'-CatStr,
         'Type'-Type,
         'Message'-ErrStr,
         'Module'-Module,
         'Predicate'-Pred,
         'File'-File,
         'L1'-L1,
         'L2'-L2,
         'Hash'-Hash],
        infolog_problem_flat(CatStr,Type,ErrStr,Module,Pred,File,L1,L2,Hash),
        AllProblems),
    maplist(json_object_create, AllProblems, ProblemObjects),
    json_object_create(['problems'-ProblemObjects], Result).
