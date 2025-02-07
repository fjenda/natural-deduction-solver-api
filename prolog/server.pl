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

    % Apply the rule
    (  prove_handler(Data.premises, Data.conclusion, Data.rule)
    -> Reply = json([success =  true])
    ;  Reply = json([success = false])
    ),

    reply_json(Reply).

start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).
