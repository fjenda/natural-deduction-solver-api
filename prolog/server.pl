:- debug(http(request)).
:- debug(http(json)).

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

    maplist(string_to_term, Data.premises, Premises),
    string_to_term(Data.conclusion, Conclusion),
    atom_string(Rule, Data.rule),

    % Apply the rule
    (  prove_handler(Premises, Conclusion, Rule)
    -> Reply = json([success =  true, premises = Data.premises, conclusion = Data.conclusion, rule = Data.rule])
    ;  Reply = json([success = false, premises = Data.premises, conclusion = Data.conclusion, rule = Data.rule])
    ),

    reply_json(Reply).

string_to_term(String, Term) :-
    read_term_from_atom(String, Term, []).

start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).
