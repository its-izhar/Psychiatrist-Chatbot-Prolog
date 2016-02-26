:- consult('READATOM.pl').
:-protocol('Output.txt').

% Begins the program
start_session :-
    write('Hello, I am a psychiatrist by profession.'), nl,
    write('You seem to be in a problem. What is it?'), nl,
    psychiatrist.


% Reads a query sentence from user and forms a reply (Main function)
psychiatrist :-
    nl, write('--> '), read_atomics(Input), psychiatrist(Input), !.

psychiatrist([bye]) :-
    write('Goodbye. I hope I have helped you'), nl, halt.
psychiatrist([goodbye]) :-
    write('Goodbye. Take care!'), nl, halt.
psychiatrist(Input) :-
    pattern(Stimulus, Response),
    match(Stimulus, Dictionary, Input),
    match(Response, Dictionary, Output),
    write('Psychiatrist: '),
    reply(Output),
    !, psychiatrist.


% Matching
match([N|Pattern], Dictionary, Target) :-
    integer(N), lookup(N, Dictionary, LeftTarget),
    append(LeftTarget, RightTarget, Target),
    match(Pattern, Dictionary, RightTarget).
match([Word | Pattern], Dictionary, [Word | Target]) :-
    atom(Word), match(Pattern, Dictionary, Target).
match([], _Dictionary, []).



% Gives a template reply to the query sentence
% --- Greetings
pattern([hello,my,name,is,1],['Nice',to,meet,you,1,'!','How',are,you,doing,'?']).

% Abilities
pattern([i,can,1],['How',often,can,you,1,'?']).

% --- Self
pattern([i,am,not,1],['Why',aren,'''',t,you,1,'?']).
pattern([i,am,1],['How',long,have,you,been,1,'?']).
pattern([i,like,1],['Does',anyone,else,in,your,family,like,1,'?']).
pattern([i,feel,1],['Do',you,often,feel,like,1,'?']).

% --- Third person
pattern([he,is,not,1],['What',makes,you,think,he,is,not,1,'?']).
pattern([she,is,not,1],['What',makes,you,think,she,is,not,1,'?']).
pattern([they,are,not,1],['What',makes,you,think,they,are,not,1,'?']).
pattern([he,is,1],['Has',he,always,been,like,that,'?']).
pattern([she,is,1],['Has',she,always,been,like,that,'?']).
pattern([they,are,1],['Have',they,always,been,like,that,'?']).

% --- Family and important people in life
pattern([1,X,2],['Can',you,tell,me,more,about,your,X,'?']) :- important_people(X).

% --- About other people
pattern([1,you,2,me],['What',makes,you,think,i,2,you,'?']).

% --- Catch all
pattern([1],['I',see,'.','Please',continue,'.']).



% important_people people
important_people(father).
important_people(mother).
important_people(son).
important_people(sister).
important_people(brother).
important_people(daughter).


% print the reply
reply([Head | Tail]) :-
    write(Head), write(' '), reply(Tail).
reply([]) :- nl.


% uses the integer position as index for Dictionary and returns the word at that index
lookup(Key, [(Key, Value) | _Dict], Value).
lookup(Key, [(Key1, _Val1) | Dictionary], Value) :-
    Key \= Key1, lookup(Key, Dictionary, Value).