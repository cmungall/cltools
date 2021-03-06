/* -*- Mode: Prolog -*- */

:- module(prolog_writer,
          [
           export_prover9/2
           ]).

export_prover9(Text,Opts) :-
        select(goals(Goal),Opts,Opts2),
        !,
        export_prover9(Text,sos,Opts2),
        export_prover9(Goal,goals,Opts2).

export_prover9(Text,Opts) :-
        export_prover9(Text,sos,Opts).


export_prover9(Text,Stanza,Opts) :-
        write_comment('Automatically generated from CL/KIF'),
	format('formulas(~w).~n',[Stanza]),
        write_axioms(Text,_,Opts),
	writeln('end_of_list.').



write_comment(A):-
        format('% ~w~n',[A]).

write_axioms(A,Toks,_) :-
        cltext(A,Toks,[]),
        concat_atom(Toks,S),
        write(S),
        nl.

jop(and).
jop(or).

nl --> ['\n'].
dot --> ['.\n'].

clcomment(Fmt,X) --> ['% '],{sformat(A,Fmt,X)},[A],nl.  % TODO - newlines
clcomment(X) --> ['% '],{sformat(A,'~q',[X])},[A],nl.  % TODO - newlines

cltext([]) --> !.
cltext([H|L]) --> !,cltext(H),cltext(L).

cltext(module(X,_Y,Text)) --> !,clcomment('Module ~w',[X]),cltext(Text). % TODO
cltext(cltext(Text)) --> !,cltext(Text).
cltext(namedtext(X,Text)) --> !,clcomment(X),cltext(Text).
cltext(comment(X,Text)) --> !,clcomment(X),cltext(Text).
cltext(X) --> axiom([],X).

% quantified sentences
axiom(BoundList,forall(X,Y)) --> !,varx(all,X),[' '],{append(X,BoundList,NewBoundList)},brac(NewBoundList,Y),dot.
axiom(BoundList,exists(X,Y)) --> !,varx(exists,X),[' '],{append(X,BoundList,NewBoundList)},brac(NewBoundList,Y),dot.
axiom(BoundList,X) --> exprs(BoundList,X),dot.

brac(BoundList,X) --> !,['('],exprs(BoundList,X),[')'].

exprs(_BoundList,[]) --> !.
exprs(BoundList,[H|L]) --> !,exprs(BoundList,H),exprs(BoundList,L).

exprs(BoundList,iff(X,Y)) --> !,['('],exprs(BoundList,X),[' <-> '],exprs(BoundList,Y),[')'].
exprs(BoundList,if(X,Y)) --> !,['('],exprs(BoundList,X),[' -> '],exprs(BoundList,Y),[')'].
exprs(BoundList,'='(X,Y)) --> !,['('],exprs(BoundList,X),[' = '],exprs(BoundList,Y),[')'].
exprs(BoundList,not(X)) --> !,['-'],brac(BoundList,X).
exprs(BoundList,X) --> {X=..[Op|L],jop(Op)},!,exprj(BoundList,Op,L).
exprs(BoundList,X) --> {compound(X),X=..[P|L]},!,predsym(P,L),['('],exprj(BoundList,',',L),[')'].
exprs(BoundList,X) --> {member(X,BoundList),var_prolog(X,V)},!,[V].
exprs(_BoundList,X) --> {safe_atom(X,A)},!,[A].

exprs(BoundList,[H]) --> !,exprs(BoundList,H).
exprs(BoundList,[H|T]) --> exprs(BoundList,H),[','],!,exprs(BoundList,T).

varx(Q,L) --> {is_list(L)},!,vars(Q,L).
varx(Q,X) --> {X=..L},!,vars(Q,L). % vars get parsed as terms..
vars(_,[]) --> [].
vars(Q,[H|T]) --> [' ',Q,' '],{var_prolog(H,V)},[V],vars(Q,T).

% always use variable arity
%predsym(P,L) --> {length(L,N)},!,[P],[N].
%predsym(P,L) --> {variable_arity(P),length(L,N)},!,[P],[N].
predsym(P,_) --> [P].

exprj(BoundList,_Op,[H]) --> !,exprs(BoundList,H).
exprj(BoundList,Op,[H|T]) --> !,exprs(BoundList,H),opx(Op),exprj(BoundList,Op,T).

opx(and) --> [' & '].
opx(or) --> [' | '].
opx(',') --> [', '].

var_prolog(A,V):-
        atom_concat('?',A1,A),
        !,
        var_prolog(A1,V).

var_prolog(A,V):-
        sub_atom(A,0,1,_,Ch),
        (   Ch @>= 'a',
            Ch @=< 'z'
        ->  V=A
        ;   downcase_atom(A,V)).

safe_atom(A,A):-
        atom_chars(A,[C1|L]),
        C1 @>= 'A',
        C1 @=< 'Z',
        forall(member(C,L),
               safe_char(C)),
        !.

safe_atom(A,Safe):-
        concat_atom(Toks,'"',A),
        concat_atom(Toks,'\\"',A2),
        sformat(Safe,'"~w"',[A2]).
        
safe_char(C):- is_alpha(C).
safe_char('_').
safe_char(C):- C @>= '0', C @=< '9'.


