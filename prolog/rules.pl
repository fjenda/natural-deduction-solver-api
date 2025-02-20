% Rule format: rule(Name, PremiseCount, ApplyPredicate)
rule('IC', 2, and_intro).
rule('EC', 1, and_elim).
%rule('ID', 1, or_intro).
rule('ID', 2, or_intro).
rule('ED', 2, or_elim).
rule('II', 2, imp_intro).
rule('MP', 2, imp_elim).
rule('IE', 2, eq_intro).
rule('EE', 1, eq_elim).
rule('NI', 1, is_negation).

% Predicate
predicate(P, X).

% Function


% Actual rule logic

% Conjunction
and_intro([A, B], and(A, B)).
and_elim([and(A, _)], A).
and_elim([and(_, B)], B).

% Disjunction
or_intro([A, B], or(A, B)).
%or_intro([B], or(_, B)).
%or_intro([A], or(A, _)).
or_elim([or(A, B), not(A)], B).
or_elim([or(A, B), not(B)], A).

% Implication
imp_intro([A, B], imp(A, B)).
imp_elim([imp(A, B), A], B).
imp_elim([A, imp(A, B)], B).

% Equivalence
eq_intro([imp(A, B), imp(B, A)], eq(A, B)).
eq_elim([eq(A, B)], imp(A, B)).
eq_elim([eq(A, B)], imp(B, A)).

% Check if a formula is a negation of another
is_negation([not(A)], A).
is_negation([A], not(A)).



% Generic rule checker
prove_handler(Premises, Conclusion, Rule) :-
    rule(Rule, PremiseCount, Predicate),
    length(Premises, PremiseCount),
    call(Predicate, Premises, Conclusion).

%has_conflict([A | Rest]) :- member(not(A), Rest), !.
%has_conflict([not(A) | Rest]) :- member(A, Rest), !.
%has_conflict([_ | Rest]) :- has_conflict(Rest).
find_conflict([A | Rest], A, not(A)) :- member(not(A), Rest), !.
find_conflict([not(A) | Rest], not(A), A) :- member(A, Rest), !.
find_conflict([_ | Rest], X, Y) :- find_conflict(Rest, X, Y).

has_conflict_with_rows(Proof, Conflict1, Conflict2) :-
    find_conflict(Proof, Conflict1, Conflict2).

% conflict checker
%conflict_handler(Proof, Conflict1, Conflict2, Result) :-
%    has_conflict_with_rows(Proof, Conflict1, Conflict2),
%    Result = true.

conflict_handler(Proof, Conflict1, Conflict2, Result) :-
    (   has_conflict_with_rows(Proof, Conflict1, Conflict2)
    ->  Result = true
    ;   Conflict1 = none,
        Conflict2 = none,
        Result = false
    ).
