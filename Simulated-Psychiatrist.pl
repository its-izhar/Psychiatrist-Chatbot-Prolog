:- consult('READATOM.pl').
:- protocol('Output-simple.txt').

% The main loop reads the input from the user, calls reply & stops when the user input is the word bye. 
psychiatrist :-
	write('Hello, I am a psychiatrist by profession. How can I help you?'),
	nl,
	repeat,
	write('--> '),
	read_atomics(In),
	reply(In).


% Match user's sentence to template and form a reply
reply([bye | _]) :-
	write('Psychiatrist: bye'), nl, !.
reply(In) :-
	utterance(Out, In, []),
	!,
	write('Psychiatrist: '),
	print_reply(Out),
	fail.


% Templates Rules
utterance(['Why', aren, '''', t, you | Y]) --> ['I', am, not], end(Y).
utterance(['How', long, have, you, been | Y]) --> ['I', am], end(Y).
utterance(['Why', do, you, like | Y]) --> ['I', like], end(Y).
utterance(['Do', you, often, think, of | Y]) --> beginning(_), ['I', remember], end(Y).
utterance(['Please', tell, me, more, about, your,X, .]) --> beginning(_), [X], end(_), {important_people(X)}.
utterance(['Why', are, so, negative, '?']) --> [no].
utterance(['Tell', me, more, .]) --> [_].
utterance(['Please', go, on, .]) --> beginning(_).


% Link the pieces of the utterance
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