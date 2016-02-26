:- consult('READATOM.pl').
:- protocol('Output-simple.txt').

% The main loop reads the input from the user, calls reply & stops when the user input is the word bye. 
psychiatrist :-
	write('Hello, I am a psychiatrist by profession.'), nl,
	write('You seem to be in a problem. What is it?'), nl,
	repeat,
	write('--> '),
	read_atomics(In),
	reply(In).


% Match user's sentence to template and form a reply
reply([bye | _]) :-
	write('Psychiatrist: bye'), nl, !, halt.
reply(In) :-
	form_reply(Out, In, []),
	!,
	write('Psychiatrist: '),
	print_reply(Out),
	fail.


% Templates Rules
form_reply(['Why', aren, '''', t, you | Y]) --> [i, am, not], end(Y).
form_reply(['How', long, have, you, been | Y]) --> [i, am], end(Y).
form_reply(['Why', do, you, like | Y]) --> [i, like], end(Y).
form_reply(['Do', you, often, think, of | Y]) --> beginning(_), [i, remember], end(Y).
form_reply(['Please', tell, me, more, about, your, X, .]) --> beginning(_), [X], end(_), {important_people(X)}.
form_reply(['Why', are, so, negative, '?']) --> [no].
form_reply(['Tell', me, more, .]) --> [_].
form_reply(['Please', go, on, .]) --> beginning(_).

form_reply(['What', makes, you, think, i, X | you '?']) --> beginning(_), [you], [X], [me]. 


% Link the pieces of the form_reply
beginning(X, Y, Z) :- append(X, Z, Y).
end(X, Y, Z) :- append(X, Z, Y).


% The Freudian facts
important_people(father).
important_people(mother).
important_people(brother).
important_people(sister).


% print_reply prints the content of the list containing Psychiatrist's answer.
print_reply([Head | Tail]) :-
	write(Head), write(' '),
	print_reply(Tail).

print_reply([]) :- nl.