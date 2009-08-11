
/* -*- Mode: Prolog -*- */

:- module(owl_writer,
          [
           export_owl/3
          ]).

:- use_module(cl_io).
:- use_module(cl_transform).
:- use_module(library('thea2/owl2_model')).
:- use_module(library('thea2/owl2_metamodel')).
:- use_module(library('thea2/owl2_io')).
:- use_module(library('thea2/owl2_util')).
:- use_module(library('thea2/owl2_from_rdf'),[expand_ns/2]).


:- multifile cl_io:serialize_cltext_hook/4.
cl_io:serialize_cltext_hook(File,owl,Text,Opts) :-
        export_owl(File,Text,Opts).

export_owl(File,Text,_Opts) :-
        Prefix='http://example.org#',
        forall(text_sentence(Text,S),
               s2owl(Prefix,S)),
        expand_namespaces,
        save_axioms(File,owl).

s2owl(Prefix,S) :-
        (   s2owl(Prefix,S,S2)
        ->  assert_axiom(S2)
        ;   true).

s2owl(Prefix,S,S2) :-
        S=..[P|Args],
        cvt_pred(P,P2),
        t2owl(Prefix,Args,Args2),
        S2=..[P2|Args2].



t2owl(_Prefix,[],[]) :- !.
t2owl(Prefix,[H|L],[H2|L2]) :-
        !,
        t2owl(Prefix,H,H2),
        t2owl(Prefix,L,L2).

t2owl(_,literal(S),literal(S)) :- !.
t2owl(Prefix,S,S2) :-
        atom(S),
        !,
        (   expand_ns(S,S2),       % e.g. rdfs:label
            sub_atom(S2,_,_,_,':') % bit of a hack...
        ->  true
        ;   atom_concat(Prefix,S,S2)).

t2owl(Prefix,S,S2) :-
        S=..[P|Args],
        cvt_pred(P,P2),
        !,
        t2owl(Prefix,Args,Args2),
        (   owlpredicate_arguments(P2,[list(_)])
        ->  S2=..[P2,Args2]
        ;   S2=..[P2|Args2]).

t2owl(Prefix,S,S2) :-
        S=..[P|Args],
        !,
        t2owl(Prefix,Args,Args2),
        S2=..[P|Args2].

% by convention, terms in the owl vocabulary are preceded by
% the prefix 'owl:' (rather than the full URI).
% the CL spec is a little unclear on 'network identifiers'.
% in future we can also translate the full URI.
cvt_pred(P,P2) :-
        atom_chars(P,[o,w,l,':',C|Rest]),
        downcase_atom(C,C2),
        atom_chars(P2,[C2|Rest]).


        
        
        

