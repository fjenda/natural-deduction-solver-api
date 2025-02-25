% Base case: Replace a variable if it appears in Vars.
substitute(Var, Vars, NewVars, NewVar) :-
    atom(Var), % Symbolic variable
    nth0(Index, Vars, Var), % Find index of Var in Vars
    nth0(Index, NewVars, NewVar), !. % Replace with corresponding NewVar

% Recursive case
substitute(Formula, Vars, NewVars, Result) :-
    compound(Formula),
    Formula =.. [Functor | Args],
    substitute_list(Args, Vars, NewVars, NewArgs),
    Result =.. [Functor | NewArgs].

% If the formula is not a variable or compound term, return it as is.
substitute(Formula, _, _, Formula) :- atomic(Formula).

% Base case for lists.
substitute_list([], _, _, []).

% Recursive case for lists.
substitute_list([Head | Tail], Vars, NewVars, [NewHead | NewTail]) :-
    substitute(Head, Vars, NewVars, NewHead),
    substitute_list(Tail, Vars, NewVars, NewTail).
