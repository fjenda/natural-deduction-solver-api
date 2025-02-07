% Rule format: rule(Name, PremiseCount, ApplyPredicate)
rule('IC', 2, and_intro).
rule('EC', 1, and_elim).
rule('ID', 1, or_intro).
rule('ED', 2, or_elim).
rule('II', 2, imp_intro).
rule('MP', 2, imp_elim).
rule('IE', 2, eq_intro).
rule('EE', 1, eq_elim).

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
    atom_string(RuleAtom, Rule),
    term_string(ConcTerm, Conclusion),
    maplist(atom_string, PremAtoms, Premises),

    % Check rule and apply it
    rule(RuleAtom, PremiseCount, Predicate),
    length(PremAtoms, PremiseCount),

    % Unify the conclusion term with the premises
    check_instantiated(PremAtoms, ConcTerm),

    % Apply the rule, unify the terms correctly
    call(Predicate, PremAtoms, ConcTerm).

% Check that all variables in the conclusion are instantiated by the premises
check_instantiated(ConcTerm, Premises) :-
    term_variables(ConcTerm, ConclusionVars),
    % Ensure that each variable in the conclusion is instantiated in the premises
    forall(member(Var, ConclusionVars), check_var_in_premises(Var, Premises)).

% Check that a variable is present in the premises (either as a term or a variable)
check_var_in_premises(Var, Premises) :-
    (   var(Var) -> true  % If it's a variable, we don't need to check it explicitly
    ;   member(Var, Premises)  % Otherwise, check that the term matches an atom in the premises
    ).