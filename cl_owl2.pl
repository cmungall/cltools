/* -*- Mode: Prolog -*- */

:- module(cl_owl2,
          [
          ]).

:- use_module(cl_io).
:- use_module(cl_transform).
:- use_module(library('thea2/owl2_model')).
:- use_module(library('thea2/owl2_metamodel')).
:- use_module(library('thea2/owl2_io')).
:- use_module(library('thea2/owl2_util')).
:- use_module(library('thea2/owl2_from_rdf'),[expand_ns/2]).

:- multifile cl_io:parse_cltext_hook/4.
cl_io:parse_cltext_hook(File,owl,Text,Opts) :-
        import_owl(File,Text,Opts).

import_owl(File,SL,_Opts) :-
        owl2_io:load_axioms(File),
        findall(S,
                (   axiom(A),
                    owl_axiom_to_cl_sentence(A,S)),
                SL).

owl_axiom_to_cl_sentence(A,S) :-
        A=..[P|Args],
        thea_pred_to_cl_pred(P,P2),
        maplist(thea_term_to_cl_term,Args,Args2),
        flatten(Args2,Args2flat),
        S=..[P2|Args2flat].

thea_term_to_cl_term(T,T) :-
        atom(T),
        !.
thea_term_to_cl_term(T,T2) :-
        is_list(T),
        !,
        maplist(thea_term_to_cl_term,T,T2).
thea_term_to_cl_term(T,T2) :-
        !,
        T=..[P|Args],
        thea_pred_to_cl_pred(P,P2),
        maplist(thea_term_to_cl_term,Args,Args2),
        flatten(Args2,Args2flat),
        T2=..[P2|Args2flat].

thea_pred_to_cl_pred(P,P).
                


        
        
        

