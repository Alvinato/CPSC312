


% 1.) write a predicate definition(Word, Meaning) that is true if meaning is a definition
% that is true when we find the definition of gloss within wordnet... 


% i think we need to load wordnet and then find we dont need the actual definition on our own file here...  just do it without it for now... 


% definition --> ('hello', G).   this should return 


%definition --> ().


%Meaning(hello) --> [an expression of greeting; 'every morning they exchanged   polite hellos'].


%Meaning(hypernym) --> [a word that is more generic than a given word].


:- consult('wn_fr.pl').


:- consult('wn_s.pl').

:- consult('wn_g.pl').
:- consult('wn_der.pl').	


% use synset to get the definition of the word...

% lets try to import fr... 




% we need to get the word id's first using the other operators!!!...
%testing(W, D):-fr(W,D).


definition(W,DEF):- s(A,B,W, C, D, E), g(A, DEF).





% ------ > second question here!! 

%2.) write a parser for a sentence and turn it into an atom
% try to look up the morph thing using the predicate... 
der(A,B,C,D).
%morph_bag_lookup(+Morph).




