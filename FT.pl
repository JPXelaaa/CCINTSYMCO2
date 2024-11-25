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

% check if two individuals are a spouse
spouse_check(X, Y) :-
    parent(X, A),
    parent(Y, A),
    X \== Y.

% Check if two individuals are siblings
sibling_check(X, Y) :-
    parent(A, X),
    parent(A, Y),
    parent(B, X),
    parent(B, Y),
    A \== B,
    X \== Y.

% Check if two individuals are cousins
cousin_check(X, Y) :-
    parent(A, X),
    parent(B, Y),
    sibling_check(A, B).
    
% "Is X a grandmother of Y" function
is_grandmother(X,Y) :-
    (   parent(Z, Y),
        parent(X, Z),
        female(X)
    -> write('Yes granny.'), nl
    ;   write('No granny.'), nl
    ).

% "Is X a grandfather of Y" function
is_grandfather(X, Y) :-
    (   parent(Z, Y),
        parent(X, Z),
        male(X)
    -> write('Yes gramp.'), nl
    ;   write('No gramp.'), nl
    ).

% "Is X a daughter of Y" function
is_daughter(X, Y) :-
    (   parent(Y, X),
        female(X)
    -> write('Yes daughter.'), nl
    ;   write('No daughter.'), nl
    ).

% "Who is/are the daughter/s of X" function
get_daughter(X) :-
    findall(D, (parent(X, D), female(D)), Daughters),
    (   Daughters \= []
    ->  write("Daughters: "), write(Daughters), nl
    ;   write("No daughters found."), nl
    ).


% "Is X a son of Y" function
is_son(X, Y) :-
    (   parent(Y, X),
        male(X)
    -> write('Yes son.'), nl
    ;   write('No son.'), nl
    ).

% "Who is/are the son/s of X" function
get_son(X) :-
    findall(S, (parent(X, S), male(S)), Sons),
    (   Sons \= []
    ->  write("Sons: "), write(Sons), nl
    ;   write("No sons found."), nl
    ).

% "Is X a child of Y" function
is_child(X, Y) :-
    (   parent(Y, X)
    -> write('Yes child.'), nl
    ;   write('No child.'), nl
    ).

% "Who is/are the child/ren of X" function
get_children(X) :-
    findall(C, parent(X, C), Children),
    (   Children \= []
    ->  write("Children: "), write(Children), nl
    ;   write("No children found."), nl
    ).

% "Are _, _, and __ children of _" function
are_children(A, B, C, Y) :-
    (   parent(Y, A),
        parent(Y, B),
        parent(Y, C)
    -> write('Yes.'), nl
    ;   write('No.'), nl
    ).

% "Is X an aunt of Y" function 
is_aunt(X, Y) :-
    (   (
        parent(Z, Y),
        sibling_check(X, Z),
        female(X))
    -> write('Yes aunt1.'), nl
    ;   (parent(P, Y),
        sibling_check(P, Z),
        spouse_check(X, Z),
        female(X))
    -> write('Yes aunt2.'), nl
    ;   write('No aunt2.'), nl
    ).

% "Is X an uncle of Y" function
is_uncle(X, Y) :-
    (   (
        parent(Z, Y),
        sibling_check(X, Z),
        male(X))
    -> write('Yes uncle1.'), nl
    ;   (parent(P, Y),
        sibling_check(P, Z),
        spouse_check(X, Z),
        male(X))
    -> write('Yes uncle2.'), nl
    ;   write('No uncle2.'), nl
    ).

% Check if an individual is in the family tree
is_in_family_tree(Person) :-
    (   female(Person)
        ; male(Person)
    ->  write('Yes person.'), nl
    ;   write('No person.'), nl
    ).

% Check if two individuals are relatives
are_relatives(X, Y) :-
    is_in_family_tree(X),
    is_in_family_tree(Y), % Ensure both are in the family tree
    (   spouse_check(X, Y)
    ;   sibling_check(X, Y)
    ;   cousin_check(X, Y)
    ;   parent(X, Y)
    ;   parent(Y, X)
    ;   is_grandmother(X, Y)
    ;   is_grandfather(X, Y)
    ;   is_aunt(X, Y)
    ;   is_uncle(X, Y)
    ),
    !, % Stop after finding the first match
    write('Yes, they are relatives.'), nl.

are_relatives(_, _) :-
    write('No, they are not relatives.'), nl.


question(is_in_family_tree(X)) -->
    "Is ", input(X), "in the family tree?".

question(spouse_check(X, Y)) -->
    "Is ", input(X), " married to ", input(Y), "?".

question(cousin_check(X, Y)) -->
    "Is ", input(X), " a cousin of ", input(Y), "?".

question(sibling_check(X, Y)) -->
    "Is ", input(X), " a sibling of ", input(Y), "?".    

% Question: Is X a grandmother of Y?
question(is_grandmother(X, Y)) -->
    "Is ", input(X), " a grandmother of ", input(Y), "?".

% Question: Is X a grandmother of Y?
question(is_grandfather(X, Y)) -->
    "Is ", input(X), " a grandfather of ", input(Y), "?".

% Question: Is X a daughter of Y?
question(is_daughter(X, Y)) -->
    "Is ", input(X), " a daughter of ", input(Y), "?".

% Question: Who is/are the daughter/s of X?
question(get_daughter(X, _D)) -->
    "Who are the daughters of ", input(X),"?".

% Question: Is X a son of Y?
question(is_son(X, Y)) -->
    "Is ", input(X), " a son of ", input(Y), "?".

% Question: Who is/are the son/s of X?
question(get_son(X, _S)) -->
    "Who are the sons of ", input(X), "?".

% Question: Is X a child of Y?
question(is_child(X, Y)) -->
    "Is ", input(X), " a child of ", input(Y), "?".

% Question: Who is/are the child/ren of X?
question(get_children(X, _C)) -->
    "Who are the children of ", input(X), "?".

% Question: Are A, B, and C children of Y?
question(are_children(A, B, C, Y)) -->
    "Are ", input(A), input(B), "and ", input(C), "children of ", input(Y), "?".

% Question: Is X an aunt of Y?
question(is_aunt(X, Y)) -->
    "Is ", input(X), " an aunt of ", input(Y), "?".

% Question: Is X an uncle of Y?
question(is_uncle(X, Y)) -->
    "Is ", input(X), " an uncle of ", input(Y), "?".

% Question: Are X and Y relatives?
question(are_relatives(X, Y)) -->
    "Are ", input(X), "and ", input(Y), " relatives?".




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
    string(NameCodes), { atom_codes(Name, NameCodes) }.

% Main predicate: Parse and execute relationship statements
start :-
    write("Enter a relationship statement or question: "),
    read_line_to_string(user_input, Input),   % Read input from user
    string_codes(Input, Codes),
    (   phrase(sentence(Action), Codes)      % Parse input as a statement
    ->  call(Action),
        write("Action executed: "), write(Action), nl
    ;   phrase(question(Query), Codes)       % Parse input as a question
    ->  (   call(Query)                      % Evaluate the query
         ->  true                            
         ;   write("No more results."), nl
         )
    ;   write("Error: Invalid input."), nl
    ).
