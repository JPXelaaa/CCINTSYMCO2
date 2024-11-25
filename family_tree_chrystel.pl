:- use_module(library(dcg/basics)).

:- dynamic male/1.
:- dynamic female/1.
:- dynamic parent/2.

% First generation males
male(eric).
male(john).
male(daniel).
male(rafael).
% Second generation males
male(robert).
male(miguel).
male(gabriel).
% Third generation males
male(josh).
male(joseph).

% First generation females
female(sofia).
female(mary).
female(amber).
female(jane).
% Second generation females
female(linda).
female(bianca).
female(emily).
% Third generation females
female(mia).
female(bea).
female(camille).
female(lia).

% Parental relationships
parent(eric, linda).
parent(sofia, linda).

parent(john, robert).
parent(mary, robert).

parent(john, miguel).
parent(mary, miguel).

parent(jane, bianca).
parent(rafael, bianca).

parent(john, emily).
parent(mary, emily).

parent(daniel, gabriel).
parent(amber, gabriel).

parent(linda, josh).
parent(robert, josh).

parent(linda, mia).
parent(robert, mia).

parent(linda, bea).
parent(robert, bea).

parent(miguel, joseph).
parent(bianca, joseph).

parent(emily, camille).
parent(gabriel, camille).

parent(emily, lia).
parent(gabriel, lia).

mother(Mother, Child) :-
    parent(Mother, Child),
    female(Mother).

father(Father, Child) :-
    parent(Father, Child),
    male(Father).


% Check if two individuals are siblings
sibling_check(X, Y) :-
    parent(A, X),
    parent(A, Y),
    X \= Y.

    are_siblings(X, Y) :-
        (   sibling_check(X, Y)     % Check if X and Y are siblings
        ->  write('Yes.'), nl
        ;   write('No.'), nl
        ).

% Find all siblings of X
find_sibling(X, Sibling) :-
    setof(Y, sibling_check(X, Y), Siblings), % Get unique siblings as a list
    member(Sibling, Siblings), 
    write(Sibling), nl,
    fail;
    true.

% Check if X is a sister of Y
is_sister(X, Y) :-
    (   sibling_check(X, Y),
        female(X)
    ->  write('Yes.'), nl
    ;   write('No.'), nl
    ).

% Find all sisters of  X
find_sister(X, Sister) :-
    setof(S, (sibling_check(X, S), female(S)), Sisters),
    member(Sister, Sisters),
    write(Sister), nl,
    fail;
    true.


% Check if X is a brother of Y
is_brother(X, Y) :-
    (   sibling_check(X, Y),
        male(X)
    ->  write('Yes.'), nl
    ;   write('No.'), nl
    ).

% Find all brothers of X
find_brother(X, Brother) :-
    setof(B, (sibling_check(X, B), male(B)), Brothers),
    member(Brother, Brothers),
    write(Brother), nl,
    fail;
    true.

% Check if X is the mother of Y
is_mother(X, Y) :-
    (   parent(X, Y),
        female(X)
    ->  write('Yes.'), nl
    ;   write('No.'), nl
    ).

% Find the mother of X
find_mother(X, Mother) :-
    parent(Mother, X),
    female(Mother),
    write(Mother), nl.

% Check if X is the father of Y
is_father(X, Y) :-
    (   parent(X, Y),
        male(X)  
    ->  write('Yes.'), nl
    ;   write('No.'), nl
    ).

% Find the father of X
find_father(X, Father) :-
    parent(Father, X),
    male(Father),
    write(Father), nl.

% Check if X and Y are the parents of X
are_parents(X, Y, Z) :-
    (   parent(X, Z),
        parent(Y, Z)            % Check if X and Y are the parents of Z
    ->  write('Yes.'), nl
    ;   write('No.'), nl
    ).

% Find the parents of X
find_parent(X, Parent) :-
    parent(Parent, X),
    write(Parent), nl,
    fail;
    true.

% Test function: Adds a male sibling and links to parents

sibling_function(X, Y) :-
    findall(A, parent(A, Y), Parents), % Collect all parents of Y
    Parents \= [] ->
        (
            assertz(male(X)), % Declare X as male
            forall(
                member(A, Parents),
                assertz(parent(A, X)) % Add each parent of Y as a parent of X
            ),
            format('~w has been added as a sibling of ~w. Parents are ~w.~n', [X,Y,Parents])
        );
        write('Not feasible'), nl.





% Question: Are X and Y siblings?
question(are_siblings(X, Y)) -->
    "Are ", input(X), " and ", input(Y), " siblings?".

% Question: Who are the siblings of X?
question(find_sibling(X, _Sibling)) -->
    "Who are the siblings of ", input(X), "?".

% Question: Is X a sister of Y?
question(is_sister(X, Y)) -->
    "Is ", input(X), " a sister of ", input(Y), "?".

% Question: Who are the sisters of X?
question(find_sister(X, _Sister)) -->
    "Who are the sisters of ", input(X), "?".

% Question: Is X a brother of Y?
question(is_brother(X, Y)) -->
    "Is ", input(X), " a brother of ", input(Y), "?".

% Question: Who are the brothers of X?
question(find_brother(X, _Brother)) -->
    "Who are the brothers of ", input(X), "?".

% Question: Is X the mother of Y?
question(is_mother(X, Y)) -->
    "Is ", input(X), " the mother of ", input(Y), "?".

% Question: Who is the mother of X?
question(find_mother(X, _Mother)) -->
    "Who is the mother of ", input(X), "?".

% Question: Is X the father of Y?
question(is_father(X, Y)) -->
    "Is ", input(X), " the father of ", input(Y), "?".

% Question: Who is the father of X?
question(find_father(X, _Father)) -->
    "Who is the father of ", input(X), "?".

% Question: Are X and Y the parents of Z?
question(are_parents(X, Y, Z)) -->
    "Are ", input(X), " and ", input(Y), " the parents of ", input(Z), "?".

% Question: Who are the parents of X?
question(find_parent(X, _Parent)) -->
    "Who are the parents of ", input(X), "?".






% Sentence for "X and Y are siblings"
sentence(sibling_function(X, Y)) -->
    input(X), " and ", input(Y), " are siblings.".

% Sentence for "X is a brother of Y"
sentence(brother(X, Y)) -->
    input(X), " is a brother of ", input(Y).

% Sentence for "X is a sister of Y"
sentence(sister(X, Y)) -->
    input(X), " is a sister of ", input(Y).

% Sentence for "X is the father of Y"
sentence(father(X, Y)) -->
    input(X), " is the father of ", input(Y).

% Sentence for "X is the mother of Y"
sentence(mother(X, Y)) -->
    input(X), " is the mother of ", input(Y).

% Helper: Parse a name from input
input(Name) -->
    string(NameCodes), 
    { atom_codes(NameAtom, NameCodes), downcase_atom(NameAtom, Name) }.

start :-
    write("Enter a relationship statement or question: "),
    read_line_to_string(user_input, Input),
    string_codes(Input, Codes), 
    (   phrase(question(Query), Codes)
    ->  (   call(Query)
        ->  true
        ;   write("No result found for the given query."), nl
        )
    ;   write("Invalid input. Please enter a valid relationship statement or question."), nl
    ).