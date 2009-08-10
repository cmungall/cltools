:- module(cl,
          [cltext/1,
           sentence/1,
           text_sentence/2,
           macro_expand/3]).

:- multifile(cltext/1).
:- dynamic(cltext/1).

sentence(S) :-
        cltext(SL),
        text_sentence(SL,S).
text_sentence(cltext(SL),S) :-
        text_sentence(SL,S).
text_sentence(SL,S) :-
        member(S,SL).
text_sentence(SL,S) :-
        cltext(SL),
        member(comment(_,S),SL).

% 1 level only
qsent_prolog(forall(VarSyms,S),forall(Vars,PlTerm)) :-
        findall(Sym-Var,member(Sym,VarSyms),SymVarMap),
        symvarmap_vars(SymVarMap,Vars),
        mapsyms(SymVarMap,S,PlTerm).

symvarmap_vars([],[]).
symvarmap_vars([_-V|L],[V|L2]) :- symvarmap_vars(L,L2).



mapsyms(_,S,S) :-
        var(S),
        !.
mapsyms(Map,S,S2) :-
        atom(S),
        member(S-S2,Map),
        !.
mapsyms(_,S,S) :-
        atom(S),
        !.
mapsyms(Map,S,S2) :-
        S=..L,
        maplist(mapsyms(Map),L,L2),
        L2=[V1|_],
        (   var(V1)
        ->  S2=L2
        ;   S2=..L2).

macro_expand(Text,MacroText,Text2) :-
        findall(S2,
                (   text_sentence(MacroText,S),
                    qsent_prolog(S,S2)),
                Macros),
        findall(S2,
                (   text_sentence(Text,S),
                    tr(S,Macros,S2)),
                Text2).


tr(comment(X,S),Macros,comment(X,S2)) :-
        !,
        tr(S,Macros,S2).
tr(S,Macros,S2) :-
        member(Macro,Macros),
        Macro=forall(_Vars,if(S,S2)),
        !.
tr(S,Macros,S2) :-
        member(Macro,Macros),
        Macro=forall(_Vars,iff(S,S2)),
        !.
tr(S,_,S).



        
