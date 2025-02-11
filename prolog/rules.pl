% Rule format: rule(Name, PremiseCount, ApplyPredicate)
rule('IC', 2, and_intro).
rule('EC', 1, and_elim).
rule('ID', 1, or_intro).
rule('ED', 2, or_elim).
rule('II', 2, imp_intro).
rule('MP', 2, imp_elim).
rule('IE', 2, eq_intro).
rule('EE', 1, eq_elim).

% Predicate
predicate(P, X).

% Function


% Actual rule logic

% Conjunction
and_intro([A, B], and(A, B)).
and_elim([and(A, _)], A).
and_elim([and(_, B)], B).

% Disjunction
or_intro([A], or(A, _)).
or_intro([B], or(_, B)).
or_elim([or(A, B), not(A)], B).
or_elim([or(A, B), not(B)], A).

% Implication
imp_intro([A, B], imp(A, B)).
imp_elim([imp(A, B), A], B).

% Equivalence
eq_intro([imp(A, B), imp(B, A)], eq(A, B)).
eq_elim([eq(A, B)], imp(A, B)).
eq_elim([eq(A, B)], imp(B, A)).

% Generic rule checker
prove_handler(Premises, Conclusion, Rule) :-
    rule(Rule, PremiseCount, Predicate),
    length(Premises, PremiseCount),
    call(Predicate, Premises, Conclusion).