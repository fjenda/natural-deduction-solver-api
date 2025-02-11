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

string_to_term(String, Term) :-
    read_term_from_atom(String, Term, []).

start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).