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

% --------------------------------------------------------------------------------------------

% check if two individuals are a spouse
spouse_check(X, Y) :- parent(X, A), parent(Y, A),
    X \== Y.

% Check if two individuals are siblings
sibling_check(X, Y) :- parent(A, X), parent(A, Y),
                       parent(B, X), parent(B, Y),
                       A \== B, X \== Y.

% Check if two individuals are cousins
cousin_check(X, Y) :- parent(A, X), parent(B, Y),
                      sibling_check(A, B).
                
% --------------------------------------------------------------------------------------------

mother(Mother, Child) :-
                        parent(Mother, Child),
                        female(Mother).

father(Father, Child) :-
                        parent(Father, Child),
                        male(Father).

% --------------------------------------------------------------------------------------------

% "Is X a grandmother of Y" function
is_grandmother(X, Y) :-
    parent(Z, Y),
    parent(X, Z),
    female(X).

% "Is X a grandfather of Y" function
is_grandfather(X, Y) :-
    parent(Z, Y),
    parent(X, Z),
    male(X).

% "Is X a daughter of Y" function
is_daughter(X, Y) :-
    parent(Y, X),
    female(X).

% "Is X a son of Y" function
is_son(X, Y) :- 
    parent(Y, X),
    male(X).


% "Is X a child of Y" function
is_child(X, Y) :- 
               (  parent(Y, X)
               ).
            
% Check if two individuals are relatives
are_relatives(X, Y) :-
    X \== Y,
    (male(X); female(X)),
    (male(Y); female(Y)).

% "Are _, _, and __ children of _" function
are_children(A, B, C, Y) :-
                        (   parent(Y, A),
                            parent(Y, B),
                            parent(Y, C)
                        ).

% --------------------------------------------------------------------------------------------   

find_sibling(X, Sibling) :-
                            setof(Y, sibling_check(X, Y), Siblings),
                            member(Sibling, Siblings), write(Sibling), nl, fail; true.
find_sister(X, Sister) :-
                            setof(S, (sibling_check(X, S), female(S)), Sisters),
                            member(Sister, Sisters), write(Sister), nl, fail; true.
find_brother(X, Brother) :-
                            setof(B, (sibling_check(X, B), male(B)), Brothers),
                            member(Brother, Brothers), write(Brother), nl, fail; true.
find_mother(X, Mother) :- 
                            mother(Mother, X), write(Mother), nl.
find_father(X, Father) :- 
                            father(Father, X), write(Father), nl.
find_parent(X, Parent) :- 
                            parent(Parent, X), write(Parent), nl, fail; true.

% --------------------------------------------------------------------------------------------  

% "Is X an aunt of Y" function 
is_aunt(X, Y) :-
                (   
                    (
                    parent(Z, Y),
                    sibling_check(X, Z),
                    female(X)
                    )
                ;   (
                    parent(P, Y),
                    sibling_check(P, Z),
                    spouse_check(X, Z),
                    female(X)
                    )
                ).
% Check if X is a sister of Y
is_sister(X, Y) :-
    (   sibling_check(X, Y),
        female(X)
    ).

% Check if X is a brother of Y
is_brother(X, Y) :-
    (   sibling_check(X, Y),
        male(X)
    ).

% "Is X an uncle of Y" function
is_uncle(X, Y) :-
                (  
                    (
                    parent(Z, Y),
                    sibling_check(X, Z),
                    male(X)
                    )
                ;   (
                    parent(P, Y),
                    sibling_check(P, Z),
                    spouse_check(X, Z),
                    male(X)
                    )
               ).

% Check if X is the mother of Y
is_mother(X, Y) :-
    (   parent(X, Y),
        female(X)
    ).

% Check if X is the father of Y
is_father(X, Y) :-
    (   parent(X, Y),
        male(X)  
    ).
% -------------------------------------------------------------------------------------------- 

% "Who is/are the child/ren of X" function
get_children(X, C) :-
                    setof(C, (parent(X, C)), Children),
                    member(C, Children),
                    write(C), nl,
                    fail;
                    true.

% "Who is/are the daughter/s of X" function
get_daughter(X, D) :-
                    setof(D, (parent(X, D), female(D)), Daughters),
                    member(D, Daughters),
                    write(D), nl,
                    fail;
                    true.

% "Who is/are the son/s of X" function
get_son(X, S) :-
                setof(S, (parent(X, S), male(S)), Sons),
                member(S, Sons),
                write(S), nl,
                fail;
                true.

% -------------------------------------------------------------------------------------------- 

% "Adds siblings if person being connected too has parents" function
sibling_function(X, Y) :-
        findall(A, parent(A, Y), Parents),
        Parents \= [],not(male(X)),not(female(Y)) ->
                        (
                            assertz(male(X)),
                            forall(
                                member(A, Parents),
                                assertz(parent(A, X))
                            ),
                            format('~w has been added as a sibling of ~w. Parents are ~w.~n', [X,Y,Parents])
                        );
                        write('Not feasible'), nl.

% "Adds a sister if person being connected too has parents" function
sister_function(X,Y):-
        findall(A, parent(A, Y), Parents),
        Parents \= [],not(female(Y)) ->
            (
                assertz(female(X)),
                forall(
                    member(A, Parents),
                    assertz(parent(A, X))
                ),
                format('~w has been added as a sister of ~w. Parents are ~w.~n', [X,Y,Parents])
            );
            write('Not feasible'), nl.

% "Adds a brother if person being connected too has parents" function
brother_function(X,Y):-
        findall(A, parent(A, Y), Parents),
        Parents \= [],not(male(X)) ->
            (
                assertz(male(X)),
                forall(
                    member(A, Parents),
                    assertz(parent(A, X))
                ),
                format('~w has been added as a brother of ~w. Parents are ~w.~n', [X,Y,Parents])
            );
            write('Not feasible'), nl.

% "Adds a mother if the person being connected too doesnt have one" function
mother_function(X,Y):-
        \+ (parent(A, Y), female(A)) ->
            (
                assertz(female(X)),
                assertz(parent(X,Y)),
                format('~w has been added as a mother of ~w. ~n', [X,Y])
            );
            write('Not feasible'), nl.

% "Adds a father if the person being connected too doesnt have one." function
father_function(X,Y):-
       \+ (parent(A, Y), male(A)) ->
        (
            assertz(male(X)),
            assertz(parent(X,Y)),
            format('~w has been added as a father of ~w. ~n', [X,Y])
        );
        write('Not feasible'), nl.

% "Adds a mother and father if the person doesnt have both (mother
%  first, then father, then child)." function
parents_function(X,Y,Z):-
    (\+ parent(_, Z)) ->
        (
            assertz(female(X)),
            assertz(parent(X,Z)),
            assertz(male(Y)),
            assertz(parent(Y,Z)),
            format('~w and ~w has been added as parents of ~w. ~n', [X,Y,Z])
        );
        write('Not feasible'), nl.

% "Adds a grandfather on the father's side if the person doesn't have one." function
grandfather_function(X,Y):-
    parent(A,Y),male(A),\+ (parent(B,A),male(B)) ->
        (
            assertz(male(X)),
            assertz(parent(X,A)),
            format('~w has been added as the grandfather of ~w. ~n', [X,Y])
        );
        write('Not feasible'), nl.

% "Adds a grandmother on the fathers side if the person doesnt have one." function
grandmother_function(X,Y):-
    parent(A,Y),male(A),\+ (parent(B,A),female(B)) ->
        (
            assertz(female(X)),
            assertz(parent(X,A)),
            format('~w has been added as the grandmother of ~w. ~n', [X,Y])
        );
        write('Not feasible'), nl.

children_function(X, Y, Z, C) :-
    assertz(male(X)),
    assertz(male(Y)),
    assertz(male(Z)),
    assertz(parent(C,X)),
    assertz(parent(C,Y)),
    assertz(parent(C,Z)),
    format('~w ~w ~w has been added as children of ~w. ~n', [X,Y,Z,C]).

%Adds a child to a person.
child_function(X,Y):-
    assertz(male(X)),
    assertz(parent(Y,X)).

%Adds a son to a person.
son_function(X,Y):-
    assertz(male(X)),
    assertz(parent(Y,X)).

%Adds a daughter to a person.
daughter_function(X,Y):-
    assertz(female(X)),
    assertz(parent(Y,X)).

%Adds an uncle to a person if that person has grandparents.
uncle_function(X,Y):-
    parent(Parent, Y),
    findall(GP, parent(GP, Parent), Grandparents),
    Grandparents \= []->
    (
    forall(
        member(GP, Grandparents),
        assertz(parent(GP, X))
    ),
    assertz(male(X)),
    format('~w has been added as the uncle of ~w. ~n', [X,Y])
    );
    write('Not feasible'), nl.


%Adds an aunt to a person if that person has grandparents.
aunt_function(X,Y):-
    parent(Parent, Y),
    findall(GP, parent(GP, Parent), Grandparents),
    Grandparents \= []->
    (
    forall(
        member(GP, Grandparents),
        assertz(parent(GP, X))
    ),
    assertz(female(X)),
    format('~w has been added as the aunt of ~w. ~n', [X,Y])
    );
    write('Not feasible'), nl.

% ------------------------------------ "Sentence and question DCG rules" ------------------------------------ 

sentence(
        sibling_function(X, Y)
        ) --> input(X), " and ", input(Y), " are siblings.".
sentence(
        brother_function(X, Y)
        ) --> input(X), " is a brother of ", input(Y), ".".
sentence(
        sister_function(X, Y)
        ) --> input(X), " is a sister of ", input(Y), ".".
sentence(
        father_function(X, Y)
        ) --> input(X), " is the father of ", input(Y), ".".
sentence(
        mother_function(X, Y)
        ) --> input(X), " is the mother of ", input(Y), ".".
sentence(
        parents_function(X, Y, Z)
        ) --> input(X), " and ", input(Y), " are the parents of ", input(Z), ".".
sentence(
        child_function(X, Y)
        ) --> input(X), " is a child of ", input(Y), ".".
sentence(
        son_function(X, Y)
        ) --> input(X), " is a son of ", input(Y), ".".
sentence(
        daughter_function(X, Y)
        ) --> input(X), " is a daughter of ", input(Y), ".".
question(
        is_in_family_tree(X)
        ) --> "Is ", input(X), "in the family tree?".
question(   
        spouse_check(X, Y)
        ) --> "Is ", input(X), " married to ", input(Y), "?".
question(   
        cousin_check(X, Y)
        ) --> "Is ", input(X), " a cousin of ", input(Y), "?".
question(   
        sibling_check(X, Y)
        ) --> "Is ", input(X), " a sibling of ", input(Y), "?".    
question( 
        is_grandmother(X, Y)
        ) --> "Is ", input(X), " a grandmother of ", input(Y), "?".
question(   
        is_grandfather(X, Y)
        ) --> "Is ", input(X), " a grandfather of ", input(Y), "?".
question(   
        is_daughter(X, Y)
        ) --> "Is ", input(X), " a daughter of ", input(Y), "?".
question(
        get_daughter(X, _)
        ) --> "Who are the daughters of ", input(X), "?".
question(
        is_son(X, Y)
        ) --> "Is ", input(X), " a son of ", input(Y), "?".
question(
        get_son(X, _)
        ) --> "Who are the sons of ", input(X), "?".
question(
        is_child(X, Y)
        ) --> "Is ", input(X), " a child of ", input(Y), "?".
question(
        get_children(X, _)
        ) --> "Who are the children of ", input(X), "?".
question(
        are_children(A, B, C, Y)
        ) --> "Are ", input(A), ", ", input(B), ", and ", input(C), " children of ", input(Y), "?".
question(
        are_relatives(X, Y)
        ) --> "Are ", input(X), " and ", input(Y), " relatives?".
question(
        is_aunt(X, Y)
        ) --> "Is ", input(X), " an aunt of ", input(Y), "?".
question(
        is_uncle(X, Y)
        ) --> "Is ", input(X), " an uncle of ", input(Y), "?".
question(
        are_siblings(X, Y)
        ) --> "Are ", input(X), " and ", input(Y), " siblings?".
question(
        find_sibling(X, _Sibling)
        ) --> "Who are the siblings of ", input(X), "?".
question(
        is_sister(X, Y)
        ) --> "Is ", input(X), " a sister of ", input(Y), "?".
question(
        find_sister(X, _Sister)) --> "Who are the sisters of ", input(X), "?".
question(
        is_brother(X, Y)
        ) --> "Is ", input(X), " a brother of ", input(Y), "?".
question(
        find_brother(X, _Brother)
        ) --> "Who are the brothers of ", input(X), "?".
question(
        is_mother(X, Y)
        ) --> "Is ", input(X), " the mother of ", input(Y), "?".
question(
        find_mother(X, _Mother)
        ) --> "Who is the mother of ", input(X), "?".
question(
        is_father(X, Y)
        ) --> "Is ", input(X), " the father of ", input(Y), "?".
question(
        find_father(X, _Father)
        ) --> "Who is the father of ", input(X), "?".
question(   
        are_parents(X, Y, Z)
        ) --> "Are ", input(X), " and ", input(Y), " the parents of ", input(Z), "?".
question(
        find_parent(X, _Parent)
        ) --> "Who are the parents of ", input(X), "?".

% -------------------------------"Yes/No Question Checker" -------------------------------
% Helper predicate to identify yes/no questions
is_yes_no_question(is_grandmother(_, _)).
is_yes_no_question(is_grandfather(_, _)).
is_yes_no_question(is_daughter(_, _)).
is_yes_no_question(is_son(_, _)).
is_yes_no_question(is_child(_, _)).
is_yes_no_question(spouse_check(_, _)).
is_yes_no_question(cousin_check(_, _)).
is_yes_no_question(sibling_check(_, _)).
is_yes_no_question(is_aunt(_, _)).
is_yes_no_question(is_uncle(_, _)).
is_yes_no_question(is_sister(_, _)).
is_yes_no_question(is_brother(_, _)).
is_yes_no_question(is_mother(_, _)).
is_yes_no_question(is_father(_, _)).
is_yes_no_question(are_siblings(_, _)).
is_yes_no_question(are_relatives(_, _)).
is_yes_no_question(are_children(_, _, _, _)).
is_yes_no_question(are_parents(_, _, _)).
is_yes_no_question(is_in_family_tree(_)).

% ------------------------------------ "Input Section" ------------------------------------

% "Helper for parsing names"
input(Name) --> string(NameCodes), { atom_codes(NameAtom, NameCodes), downcase_atom(NameAtom, Name) }.

% "Start predicate for user interaction"
start :- 
    write("Enter a relationship statement or question: "),
    read_line_to_string(user_input, Input),
    string_codes(Input, Codes),
    (   phrase(sentence(Action), Codes) 
    ->  write("Executing action: "), write(Action), nl,
        call(Action)
    ;   phrase(question(Query), Codes) 
    ->  write("Executing query: "), write(Query), nl,
        (   is_yes_no_question(Query)
        ->  (   call(Query) -> write("Yes."), nl ; write("No."), nl )
        ;   call(Query)
        )
    ;   write("Invalid input. Please enter a valid statement or question."), nl
    ).
