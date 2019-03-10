:- module('infolog-handlers',
          [message_handler/6]).

:- use_module(json).

% Dispatch
message_handler('analyzeFile', Params, ID, Result, Error, InfologPath) :- handle_analyzeFile(Params, ID, Result, Error, InfologPath).


% Handlers
handle_analyzeFile(Params, _, Result, null, InfologPath) :-
    atom_concat(InfologPath, '/prolog-analyzer/analyzer', AnalyzerPath),
    use_module(AnalyzerPath),
    json:json_object_get(Params, 'path', Path),
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
    maplist(json:json_object_create, AllProblems, ProblemObjects),
    exclude('infolog-handlers':server_code, ProblemObjects, FinalProblems),
    json:json_object_create(['problems'-FinalProblems], Result).

server_code(Element) :- json:json_object_get(Element, 'Module', 'infolog-server').
server_code(Element) :- json:json_object_get(Element, 'Module', 'infolog-handlers').
server_code(Element) :- json:json_object_get(Element, 'Module', 'json').
