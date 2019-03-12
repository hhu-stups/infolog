:- module('infolog-handlers',
          [message_handler/6]).

:- use_module(json_lib).

% Dispatch
message_handler('analyzeFile', Params, ID, Result, Error, InfologPath) :-
    handle_analyzeFile(Params, ID, Result, Error, InfologPath).
message_handler('analyzeFileProB', Params, ID, Result, Error, InfologPath) :-
    handle_analyzeFileProB(Params, ID, Result, Error, InfologPath).


% Handlers
handle_analyzeFile(Params, _, Result, null, InfologPath) :-
    atom_concat(InfologPath, '/prolog-analyzer/analyzer', AnalyzerPath),
    use_module(AnalyzerPath),
    json_lib:json_object_get(Params, 'path', Path),
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
    maplist(json_lib:json_object_create, AllProblems, ProblemObjects),
    exclude('infolog-handlers':server_code, ProblemObjects, FinalProblems),
    json_lib:json_object_create(['problems'-FinalProblems], Result).

handle_analyzeFileProB(Params, _, Result, null, InfologPath) :-
    atom_concat(InfologPath, '/prolog-analyzer/analyzer', AnalyzerPath),
    use_module(AnalyzerPath),
    json_lib:json_object_get(Params, 'path', Path),
    json_lib:json_object_get(Params, 'proBPath', ProBPath),
    json_lib:json_object_get(Params, 'proBInfoDir', ProBInfoDir),
    json_lib:json_object_get(Params, 'proBTargets', ProBTargets),
    atom_concat(ProBInfoDir, '/meta_user_pred_cache.pl', MetaUserPredCache),
    atom_concat(ProBInfoDir, '/tcltk_calls.pl', TclTkCalls),
    atom_concat(ProBPath, '/src/infolog_problem_db.pl', InfologProblemDB),
    analyze(ProBTargets, MetaUserPredCache, TclTkCalls),
    update_problem_db(InfologProblemDB),
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
    maplist(json_lib:json_object_create, AllProblems, ProblemObjects),
    exclude('infolog-handlers':server_code, ProblemObjects, FinalProblems),
    json_lib:json_object_create(['problems'-FinalProblems], Result).

server_code(Element) :- json_lib:json_object_get(Element, 'Module', 'infolog-server').
server_code(Element) :- json_lib:json_object_get(Element, 'Module', 'infolog-handlers').
server_code(Element) :- json_lib:json_object_get(Element, 'Module', 'json_lib').
