:- debug(http(request)).
:- debug(http(json)).
:- debug(api).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).

% Load RuleSet
:- consult('rules.pl').

% Endpoint to handle prove requests
:- http_handler(root(prove), prove_request, [method(post)]).

% Endpoint to check for conflicts
:- http_handler(root(conflict), conflict_request, [method(post)]).

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

    debug(api, 'Data: ~w', [Data]),

    (   is_list(Data.proof)
    ->  maplist(string_to_term, Data.proof, Proof)
    ;   string_to_term(Data.proof, Proof)
    ),

    % Check for conflicts
    conflict_handler(Proof, C1, C2, Result),
    maplist(term_string, [C1, C2], ResultsStrings),

    % Construct the JSON reply
    (   Result
    ->  Reply = json([success = true, results = ResultsStrings])
    ;   Reply = json([success = false])
    ),

    reply_json(Reply).

string_to_term(String, Term) :-
    read_term_from_atom(String, Term, []).

start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).