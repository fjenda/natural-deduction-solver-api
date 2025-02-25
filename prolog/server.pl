:- debug(http(request)).
:- debug(http(json)).
:- debug(api).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).


% Helper function for converting a string to term
string_to_term(String, Term) :-
    read_term_from_atom(String, Term, []).

% Start server function
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% Load RuleSet
:- consult('rules.pl').

% Load Substitution script
:- consult('substitution.pl').

% Endpoint to handle prove requests
:- http_handler(root(prove), prove_request, [method(post)]).

% Endpoint to check for conflicts
:- http_handler(root(conflict), conflict_request, [method(post)]).

% Endpoint to handle substitutions
:- http_handler(root(substitute), substitute_request, [method(post)]).

% Handle the prove request
prove_request(Request) :-
    % Read JSON data
    http_read_json_dict(Request, Data),

    (   is_list(Data.premises)
    ->  maplist(string_to_term, Data.premises, Premises)
    ;   string_to_term(Data.premises, Premises)
    ),

    string_to_term(Data.conclusion, Conclusion),
    atom_string(Rule, Data.rule),

    % Get all possible results
    findall(ConclusionResult, prove_handler(Premises, ConclusionResult, Rule), Results),

    % Results to string for JSON
    maplist(term_string, Results, ResultsStrings),

    % Construct the JSON reply
    (   ResultsStrings \= []
    ->  Reply = json([success = true, results = ResultsStrings])
    ;   Reply = json([success = false, premises = Data.premises, rule = Data.rule])
    ),

    reply_json(Reply).

% Handle the conflict request
conflict_request(Request) :-
    % Read JSON data
    http_read_json_dict(Request, Data),

    (   is_list(Data.proof)
    ->  maplist(string_to_term, Data.proof, Proof)
    ;   string_to_term(Data.proof, Proof)
    ),

    % Check for conflicts
    conflict_handler(Proof, C1, C2, Result),

    (   Result
    ->  maplist(term_string, [C1, C2], ResultsStrings),
        Reply = json([success = true, results = ResultsStrings])
    ;   Reply = json([success = false])
    ),

    reply_json(Reply).

substitute_request(Request) :-
    % Read JSON data
    http_read_json_dict(Request, Data),

    % Theorem to substitute
    string_to_term(Data.theorem, Theorem),

    % Variables
    (   is_list(Data.oldVars)
    ->  maplist(string_to_term, Data.oldVars, OldVars)
    ;   string_to_term(Data.oldVars, OldVars)
    ),

    (   is_list(Data.newVars)
    ->  maplist(string_to_term, Data.newVars, NewVars)
    ;   string_to_term(Data.newVars, NewVars)
    ),

    % Perform substitution
    substitute(Theorem, OldVars, NewVars, Result),

    % Construct the JSON reply
    Reply = json([success = true, result = Result]),

    reply_json(Reply).
