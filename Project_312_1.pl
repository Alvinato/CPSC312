


% 1.) write a predicate definition(Word, Meaning) that is true if meaning is a definition
% that is true when we find the definition of gloss within wordnet... 


% i think we need to load wordnet and then find we dont need the actual definition on our own file here...  just do it without it for now... 


% definition --> ('hello', G).   this should return 


%definition --> ().


%Meaning(hello) --> [an expression of greeting; 'every morning they exchanged   polite hellos'].


%Meaning(hypernym) --> [a word that is more generic than a given word].


:- consult('wn_fr.pl').

% use synset to get the definition of the word...

% lets try to import fr... 

testing(W, D) :- fr(W, D).

hello(L):- write("this is working right now").

