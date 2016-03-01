%%%%%%%%%%%%%%%%%%%%%%%%%%
% Simulated Psychiatrist %
%						 %
% Author: Izhar Shaikh	 %
% License: MIT License   %
% Year: 2016					 %
%						 %
%%%%%%%%%%%%%%%%%%%%%%%%%%

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
pattern([hi,i,am,1],['Hey,',1,'!','How',are,you,doing,'?']).
pattern([thank,you],Ans) :- choose([['I',am,glad,'I',could,help,'!'],
                                    ['My','Pleasure','!']],Reply), Ans=Reply.

% Abilities
pattern([i,cannot,1],Ans) :- choose([['Why',do,you,think,you,cannot,1,'?'],
                                     ['What',would,happen,if,you,could,1,'?']],Reply), Ans=Reply.
pattern([i,can,1],Ans) :- choose([['What',else,can,you,do,'?'],
                                  ['How',often,can,you,1,'?'],
                                  ['Oh',you,can,'!','Can',you,'?']],Reply), Ans=Reply.

% Yes-No
pattern([yes,1],Ans) :- choose([['Okay','...','How','?'],
                                ['Good','!','Tell',me,more]],Reply), Ans=Reply.
pattern([yes],Ans) :- choose([['Okay','...','How','?'],
                                ['Good','!','Tell',me,more]],Reply), Ans=Reply.
pattern([no,1],Ans) :- choose([['Why','not','?'],
                                ['It','is','Okay','!']],Reply), Ans=Reply.
pattern([no],Ans) :- choose([['Why','not','?'],
                                ['It','is','Okay','!']],Reply), Ans=Reply.

% Thinking about past and future
pattern([i,should,have,1],Ans) :- choose([['What',would,have,happened,if,you,would,have,1,'?'],
                                          ['Why',do,you,think,so,'?'],
                                          ['How',do,you,feel,about,it,'?']],Reply), Ans=Reply.
pattern([i,should,not,have,1],Ans) :- choose([['What',would,happen,if,you,1,'?'],
                                              ['Why',do,you,think,you,should,not,have,1,'?'],
                                              ['Do',you,feel,regretful,'?']],Reply), Ans=Reply.
pattern([i,should,not,1],Ans) :- choose([['What',else,do,you,think,you,should,not,do,'?'],
                                     ['Why',do,you,think,you,should,not,1,'?']],Reply), Ans=Reply.
pattern([i,should,1],Ans) :- choose([['What',else,do,you,think,you,should,do,'?'],
                                     ['Is',there,something,special,about,it,'?'],
                                     ['Why',do,you,think,you,should,'?']],Reply), Ans=Reply.


% Future Possibility
pattern([i,will,not,1],Ans) :- choose([['What',will,happen,if,you,1,'?'],
                                       ['Why',do,you,think,so,'?'],
                                       ['Why',not,'?'],
                                       ['How',do,you,feel,about,it,'?']],Reply), Ans=Reply.
pattern([i,will,1],Ans) :- choose([['Will',you,feel,happy,after,doing,that,'?'],
                                       ['Why',do,you,think,you,should,1,'?'],
                                       ['Do',you,feel,it,is,necessary,'?']],Reply), Ans=Reply.


% --- Self
pattern([i,am,not,1],['Why',aren,'''',t,you,1,'?']).
pattern([i,am,1],['How',long,have,you,been,1,'?']).
pattern([i,like,1],Ans) :- choose([['Does',anyone,else,in,your,family,like,1,'?'],
                                    ['What',else,do,you,like,other,than,1,'?'],
                                    ['Nice','!','Tell',me,more,about,1]],Reply), Ans=Reply.
pattern([i,feel,good,1],Ans) :- choose([['I',feel,good,too,'!'],
                                    ['Happy',for,you,'!']],Reply), Ans=Reply.
pattern([i,feel,good],Ans) :- choose([['I',am,glad,'!'],
                                    ['Happy',for,you,'!']],Reply), Ans=Reply.
pattern([i,feel,1],Ans) :- choose([['Do',you,often,feel,like,that,'?'],
                                    ['What',makes,you,feel,1,'?'],
                                    ['Can',you,elaborate,this,more,'?']],Reply), Ans=Reply.



% --- Third person
pattern([he,is,not,1],['What',makes,you,think,he,is,not,1,'?']).
pattern([she,is,not,1],['What',makes,you,think,she,is,not,1,'?']).
pattern([they,are,not,1],['What',makes,you,think,they,are,not,1,'?']).
pattern([he,is,1],['Has',he,always,been,like,that,'?']).
pattern([she,is,1],['Has',she,always,been,like,that,'?']).
pattern([they,are,1],['Have',they,always,been,like,that,'?']).

pattern([he,has,been,1],['What',do,you,think,might,be,the,reason,of,that,'?']).
pattern([she,has,been,1],['What',do,you,think,might,be,the,reason,of,that,'?']).
pattern([they,have,been,1],['What',do,you,think,might,be,the,reason,of,that,'?']).


% --- Family and important people in life
pattern([1,X,2],['Can',you,tell,me,more,about,your,X,'?']) :- important_people(X).

% --- About other people
pattern([1,you,2,me],['What',makes,you,think,i,2,you,'?']).

% --- Catch all
pattern([1],Ans) :- choose([['I',see,'.','Please',continue,'.'],
                            ['I',understand,'.','Please',continue,'.']],Reply), Ans=Reply.



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


%% choose(List, Elt) - chooses a random element
%% in List and unifies it with Elt.
choose([], []).
choose(List, Elt) :-
        length(List, Length),
        random(0, Length, Index),
        nth0(Index, List, Elt).




% File READATOM.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Appendix B

% Version of read_atomics/1 for most Prologs.  See text.


% read_atomics(-Atomics)
%  Reads a line of text, breaking it into a
%  list of atomic terms: [this,is,an,example].

read_atomics(Atomics) :-
   read_char(FirstC,FirstT),
   complete_line(FirstC,FirstT,Atomics).


% read_char(-Char,-Type)
%  Reads a character and runs it through char_type/3.

read_char(Char,Type) :-
   get0(C),
   char_type(C,Type,Char).


% complete_line(+FirstC,+FirstT,-Atomics)
%  Given FirstC (the first character) and FirstT (its type), reads
%  and tokenizes the rest of the line into atoms and numbers.

complete_line(_,end,[]) :- !.                  % stop at end

complete_line(_,blank,Atomics) :-              % skip blanks
   !,
   read_atomics(Atomics).

complete_line(FirstC,special,[A|Atomics]) :-   % special char
   !,
   name(A,[FirstC]),
   read_atomics(Atomics).

complete_line(FirstC,alpha,[A|Atomics]) :-     % begin word
   complete_word(FirstC,alpha,Word,NextC,NextT),
   name(A,Word),  % may not handle numbers correctly - see text
   complete_line(NextC,NextT,Atomics).


% complete_word(+FirstC,+FirstT,-List,-FollC,-FollT)
%  Given FirstC (the first character) and FirstT (its type),
%  reads the rest of a word, putting its characters into List.

complete_word(FirstC,alpha,[FirstC|List],FollC,FollT) :-
   !,
   read_char(NextC,NextT),
   complete_word(NextC,NextT,List,FollC,FollT).

complete_word(FirstC,FirstT,[],FirstC,FirstT).
   % where FirstT is not alpha


% char_type(+Code,?Type,-NewCode)
%  Given an ASCII code, classifies the character as
%  'end' (of line-file), 'blank', 'alpha'(numeric), or 'special',
%  and changes it to a potentially different character (NewCode).

char_type(10,end,10) :- !.         % UNIX end of line mark
char_type(13,end,13) :- !.         % DOS end of line mark
char_type(-1,end,-1) :- !.         % get0 end of file code

char_type(Code,blank,32) :-        % blanks, other ctrl codes
  Code =< 32,
  !.

char_type(Code,alpha,Code) :-      % digits
  48 =< Code, Code =< 57,
  !.

char_type(Code,alpha,Code) :-      % lower-case letters

  97 =< Code, Code =< 122,
  !.

char_type(Code,alpha,NewCode) :-   % upper-case letters
  65 =< Code, Code =< 90,
  !,
  NewCode is Code + 32.            %  (translate to lower case)

char_type(95,alpha,95):-!.		   % '_'
char_type(46,alpha,46):-!.		   % '.'
char_type(Code,special,Code).      % all others