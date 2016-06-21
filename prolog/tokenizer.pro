:- include('opdefs.pro').

:- module(tokenizer1).
:- import(list).
:- export tokenize/2.
:- export tokenize/3.

%-------------------------------------------------
% tokenize preserving white space
%

tokenize_wh( ?str, ?tokens, ?vars ) :-
   nonvar(?str),
   string_list( ?str, ?chars ),
   to_tokens_wh( ?toks, ?chars),
   make_vars(?toks, ?tokens, [], ?vars),
   !.
   
/* nah
to_tokens_wh( [], [0'*|_] ) :-   % '
   !.
*/
to_tokens_wh( [], [0'#|_] ) :-   % '
   !.
to_tokens_wh( [], [] ) :-
   !.
to_tokens_wh( ?toks, ?chars ) :-
   tokens_wh(?toks, ?chars, []).

tokens_wh([], [], []).
tokens_wh( [?w, ?token | ?tokens] ) -->
   wh(?w),
   token_wh( ?token ),
   !,
   tokens_wh( ?tokens ).
tokens_wh( [?token | ?tokens] ) -->
   token_wh( ?token ),
   !,
   tokens_wh( ?tokens ).
tokens_wh( [?w] ) --> wh(?w).
tokens_wh( [] ) --> [].

% note, need to do white space for each, so that we can save
% special case of leading . for redisplay.

token_wh( ?token ) -->
   { var(?token) },   % get vars first when forwards
   aVariable( ?token ).
token_wh( ?token ) -->
   anAtom( ?token ).
token_wh( [] ) -->
   [ 0'[, 0'] ].
token_wh( ?token ) -->
   aString( ?token ).
token_wh( ?token ) -->
   anOperator( ?token ).
token_wh( ?token ) -->   % put after operator so -15, +15 get the signs parsed
   aNumber( ?token ).
token_wh( ?token ) -->   % put after operator so -15, +15 get the signs parsed
   aPlace( ?token ).
token_wh( ?token ) -->
   aPunctuationMark( ?token ).
token_wh( ?token ) -->
   { structure(?token) },
   aVariable( ?token ).
% A special case of white space after the last token,
% fail so that the third clause of tokens can finish up.
token_wh( _ ) -->
   !,
   fail.
% A real error.
token_wh( _ ) -->
   token_error(`Unrecognized token`).

%-------------------------------------------------
% real tokenize
%

tokenize( ?str, ?tokens ) :-
   tokenize( ?str, ?tokens, _).

tokenize( ?str, ?tokens, ?vars ) :-
   nonvar(?str),
   string_list( ?str, ?chars ),
   to_tokens( ?toks, ?chars),
   make_vars(?toks, ?tokens, [], ?vars),
   !.
tokenize( ?str, ?tokens, ?vars ) :-
   nonvar(?tokens),
   unify_vars(?vars),
   unmake_vars(?tokens, ?toks),
   to_tokens( ?toks, ?chars),
   squish_punt(?chars, ?schars),
   string_list( ?str, ?schars ),
   !.

% takes spaces out around punctuation
squish_punt([], []) :-
   !.
squish_punt([0' ], []) :-
   !.
squish_punt([0' , ?p, 0'  |?x], [?p | ?z]) :-
   punctuation(?p),
   !,
   squish_punt(?x, ?z).
squish_punt([?p, 0'  |?x], [?p | ?z]) :-
   punctuation(?p),
   !,
   squish_punt(?x, ?z).
squish_punt([?c | ?x], [?c | ?z] ) :-
   squish_punt(?x, ?z).

finished([], []).

token_error(?msg, ?chars, _) :-
   string_list(?str, ?chars),
   string_length(?str, ?len),
   ( ?len > 30 ->
      sub_string( ?str, 1, 30, ?sstr ),
      strcat( ?sstr, `...`, ?here )
      ;
      ?here = ?str ),
   stringlist_concat([ ?msg, ` near here: `, ?here], ?errmsg),
   throw( parse_error(?errmsg) ).

to_tokens( [], [0'*|_] ) :-   % '
   !.
to_tokens( [], [0'#|_] ) :-   % '
   !.
to_tokens( [], [] ) :-
   !.
to_tokens( ?toks, ?chars ) :-
   tokens(?toks, ?chars, []).

tokens([], [], []).
tokens( [?token | ?tokens] ) -->
   token( ?token ),
   !,
   tokens( ?tokens ).
tokens( [] ) --> wh.

% note, need to do white space for each, so that we can save
% special case of leading . for redisplay.

token( ?token ) -->
   { var(?token) },   % get vars first when forwards
   wh,
   aVariable( ?token ).
token( ?token ) -->
   wh,
   anAtom( ?token ).
token( [] ) -->
   wh,
   [ 0'[, 0'] ].
token( ?token ) -->
   wh,
   aString( ?token ).
token( ?token ) -->
   wh,
   anOperator( ?token ).
token( ?token ) -->   % put after operator so -15, +15 get the signs parsed
   wh,
   aNumber( ?token ).
token( ?token ) -->   % put after operator so -15, +15 get the signs parsed
   wh,
   aPlace( ?token ).
token( ?token ) -->
   wh,
   aPunctuationMark( ?token ).
token( ?token ) -->
   { structure(?token) },
   wh,
   aVariable( ?token ).
% A special case of white space after the last token,
% fail so that the third clause of tokens can finish up.
token( _ ) -->
   wh,
   !,
   fail.
% A real error.
token( _ ) -->
   token_error(`Unrecognized token`).

% variables are tokenized as v("?x", ?) - this unifies
% all the same named vars to the same internal variable.
make_vars([], [], ?vars, ?vars).
make_vars([v(?vstr, ?v)|?vss], [v(?v)|?vs], ?vars, ?allvars) :-
   member( v(?vstr, ?v), ?vars ),
   !,
   make_vars(?vss, ?vs, ?vars, ?allvars).
make_vars([v(?vstr, ?v)|?vss], [v(?v)|?vs], ?vars, ?allvars) :-
   string_term(?vstr, ?v),
   !,
   make_vars(?vss, ?vs, [v(?vstr, ?v)|?vars], ?allvars).
% make_vars([v(?v)|?vss], [v(?v)|?vs], ?vars, ?allvars) :-
%    !,
%    make_vars(?vss, ?vs, ?vars, ?allvars).
make_vars([?t|?tss], [?t|?ts], ?vars, ?allvars) :-
   !,
   make_vars(?tss, ?ts, ?vars, ?allvars).

% going backwards we unify first and then add dummy variables
unmake_vars([], []).
unmake_vars([v(?vstr)|?vs], [v(?vstr, _)|?vss]) :-
   !, unmake_vars(?vss, ?vs).
unmake_vars([?x|?vs], [?x|?vss]) :-
   !, unmake_vars(?vss, ?vs).

unify_vars([]).
unify_vars([v(?vstr, ?v) |?vs]) :-
   ?v = ?vstr,
   !, unify_vars(?vs).
   
% so we can handle cases going either way
v_member( v(?vs1, ?v), [v(?vs2, ?v) | _]) :-
   nonvar(?vs1),
   ?vs1 = ?vs2.
v_member( v(?vs, ?v), [v(?vs, ?v) | _]) :-
   var(?vs),
   ?v == ?v.
v_member(?a, [_| ?z]) :-
   v_member(?a, ?z).
   
%-------------------------------------------------
% White space
%

wh( wh(?wstr) ) -->
   { var(?wstr) },
   !,
   wh_chars(?wchars),
   { ( ?wchars \= [], flatten(?wchars, ?fwchars), string_list(?wstr, ?fwchars) ) }.
wh( wh(?wstr) ) -->
   { nonvar(?wstr) },
   !,
   { string_list(?wstr, ?wchars) },
   put_wchars(?wchars).

wh_chars([?w|?wchars]) -->
   [?w],
   { is_white(?w) },
   !, wh_chars(?wchars).
wh_chars( [ [0'~ | ?cchars] | ?wchars] ) -->  % '
   [0'~], 
   comment_chars( ?cchars ),
   !,
   wh_chars(?wchars).
wh_chars([]) --> [].

put_wchars([?c | ?cs]) --> [?c], !, put_wchars(?cs).
put_wchars([]) --> [].

is_wh --> [0' ], wh.

wh --> [?C], { nonvar(?C), is_white(?C) }, !, wh.
wh --> [?C], { var(?C), ?C = 0'  }, !.
wh --> is_comment, !, wh.
wh --> [].

is_white(?W) :-
   ?W =< 0w0020,
   !.
is_white(?W) :-
   ?W =< 0w007f,
   !, fail.
is_white(?W) :-
   ?W < 0w00a0,
   !.
is_white(?W) :-
   (?W >= 0w2000, ?W =< 0w200f
    ;
    ?W >= 0wfff0, ?W =< 0wfffe
    ;
    ?W == 0wfeff).

is_comment -->
   [0'~],
   comment.

comment --> [0'~], !.
comment --> [_], !, comment.
comment --> [].

comment_chars([0'~]) --> [0'~], !.
comment_chars([?cchar|?cchars]) -->
   [?cchar],
   !, comment_chars(?cchars).
comment_chars([]) --> [].

%--------------------------------------------
% a query place holder
%

aPlace(place(?p)) -->
   { var(?p) },
   [0'_],
   anumber_chars(?Cs),
   { atom_codes(?p, [0'_ | ?Cs]) }.   % '
aPlace(place(?p)) -->
   { nonvar(?p) },
   { atom_codes(?p, [0'_ | ?Cs]) },   % '
   [0'_],
   anumber_chars(?Cs).

%--------------------------------------------
% Numbers
%

decimal_point -->
   [ ?x ],
   { decimal_point( ?x ) }.

aNumber(?N) -->
   { var(?N) },
   asign(?C),
   !,
   anumber_chars(?Cs),
   { string_list(?S,[?C|?Cs]), string_term(?S,?N) },
   next_non_number,
   !.
aNumber(?N) -->
   { var(?N) },
   anumber_chars(?Cs),
   { string_list(?S,?Cs), string_term(?S,?N) },
   next_non_number,
   !.
aNumber(?N) -->
   { number(?N) },
   { string_term(?S,?N), string_list(?S,[?C|?Cs]) },
   asign(?C),
   !,
   anumber_chars(?Cs),
   next_non_number.
aNumber(?N) -->
   { number(?N) },
   { string_term(?S,?N), string_list(?S,?Cs) },
   anumber_chars(?Cs),
   next_non_number.

next_non_number -->
   [?C],
   { nonvar(?C), (is_digit(?C); is_letter(?C)) },
   token_error(`Invalid number`).
next_non_number --> [].

asign(0'-) --> [0'-], !.
asign(0'+) --> [0'+].

anumber_chars([0'0, ?C|?Cs]) -->
   { var(?C) },   % reading input strings
   [0'0, ?C],
   { is_base(?C) },
   !,
   based_chars(?C,?Cs).
anumber_chars([?C|?Cs]) -->
   { var(?C) },  % building output string
   [?C],
   { is_digit(?C) },
   dec_digits(?Ds),
   more_number_chars(?MDs),
   { append(?Ds, ?MDs, ?Cs) }.
/* this didn't fix the problem ?- tokeinze(?s, [15]).  no
anumber_chars([?C|?Cs]) -->
   [?C],
   { nonvar(?C), is_digit(?C) },
   { append(?Ds, ?MDs, ?Cs) },   % split the Cs which are bound, we're going backwards
   dec_digits(?Ds),
   more_number_chars(?MDs).
*/
anumber_chars([?C|?Cs]) -->
   { nonvar(?C) },  % building output string
   [?C],
   { is_digit(?C) },
   { append(?Ds, ?MDs, ?Cs) },
   dec_digits(?Ds),
   more_number_chars(?MDs).

based_chars(0'', [?C]) --> [?C], !.
based_chars(0'x, ?Cs) --> !, hex_digits(?Cs).
based_chars(0'X, ?Cs) --> !, hex_digits(?Cs).
based_chars(0'w, ?Cs) --> !, hex_digits(?Cs).
based_chars(0'W, ?Cs) --> !, hex_digits(?Cs).
based_chars(0'b, ?Cs) --> !, bin_digits(?Cs).
based_chars(0'B, ?Cs) --> !, bin_digits(?Cs).
based_chars(0'o, ?Cs) --> !, oct_digits(?Cs).
based_chars(0'O, ?Cs) --> !, oct_digits(?Cs).

hex_digits([?C|?Cs]) --> [?C], { nonvar(?C), is_hex(?C) }, !, hex_digits(?Cs).
hex_digits([]) --> [].

bin_digits([?C|?Cs]) --> [?C], { nonvar(?C), is_bin(?C) }, !, bin_digits(?Cs).
bin_digits([]) --> [].

oct_digits([?C|?Cs]) --> [?C], { nonvar(?C), is_oct(?C) }, !, oct_digits(?Cs).
oct_digits([]) --> [].
   
dec_digits([?C|?Cs]) --> [?C], { nonvar(?C), is_digit(?C) }, !, dec_digits(?Cs).
dec_digits([]) --> [].

more_number_chars([0'., ?C|?Cs]) -->
   { var(?C) },
%   [0'.],
   decimal_point,
   [?C],
   { is_digit(?C) },
   !,
   dec_digits(?Ds),
   exponent_chars(?MDs),
   { append(?Ds, ?MDs, ?Cs) }.
more_number_chars([0'., ?C|?Cs]) -->
   { nonvar(?C) },
%   [0'.],
   decimal_point,
   { is_digit(?C) },
   [?C],
   !,
   { append(?Ds, ?MDs, ?Cs) },
   dec_digits(?Ds),
   exponent_chars(?MDs).

more_number_chars(?Cs) -->
   exponent_chars(?Cs).

exponent_chars([?E,?S,?D|?Cs]) -->
   [?E], { (?E == 0'e; ?E == 0'E) },
   asign(?S),
   [?D], { nonvar(?D), is_digit(?D) },
   !,
   dec_digits(?Cs).
exponent_chars([?E,?S,?D|?Cs]) -->
   [?E], { (?E == 0'd; ?E == 0's) },
   asign(?S),
   [?D], { nonvar(?D), is_digit(?D) },
   !,
   dec_digits(?Cs).
exponent_chars([?E|?Cs]) -->
   [?E], { (?E == 0'e; ?E == 0'E) },
   !,
   dec_digits(?Cs).
exponent_chars([?C]) -->
   [?C], { (?C == 0'r; ?C == 0'R) },
   !.
exponent_chars([C]) -->
   [?C], { (?C == 0'g; ?C == 0'G; ?C == 0'f) },
   !.
exponent_chars([]) --> [].

is_digit(?C) :- ?C >= 0'0, ?C =< 0'9.

is_base(?C) :-
   is_member(?C, [0'x, 0'X, 0'w, 0'W, 0'o, 0'O, 0'b, 0'B, 0'']).

is_hex(?C) :-
   ( ?C >= 0'0, ?C =< 0'9
     ;
     ?C >= 0'a, ?C =< 0'f
     ;
     ?C >= 0'A, ?C =< 0'F ).

is_oct(?C) :- ?C >= 0'0, ?C =< 0'7.

is_bin(?C) :- (?C == 0'0; ?C == 0'1).


%--------------------------------------------
% Atoms
%

anAtom(?A) -->
   { var(?A) },
   aatom_chars(?L),
   { atom_codes(?A,?L) },
   !.
%anAtom(?S) --> { var(?S) }, quoted_chars(0'", ?Cs), { atom_codes(?S, ?Cs) }, !.
anAtom(?S) --> { var(?S) }, quoted_chars(0'', ?Cs), { atom_codes(?S, ?Cs) }, !.
%anAtom(?A) -->
%   { atom(?A),
%     atom_codes(?A,?L) },
%   aatom_chars(?L).
anAtom(?a) -->
   { atom(?a),
     string_termq(?s, ?a),
     string_list(?s, ?l) },
   any_chars(?l).

any_chars([?c|?cs]) -->
   [?c],
   !,
   any_chars(?cs).
any_chars([]) -->
   [].

aatom_chars([?C|?Cs]) --> aatom_init(?C), aatom_rest(?Cs), !.

aatom_init(?C) --> [?C], { is_atom_init(?C) }.

aatom_rest([?C|?Cs]) --> [?C], { is_atom_char(?C) }, !, aatom_rest(?Cs).
aatom_rest([]) --> [].

is_atom_init(?C) :- is_lower(?C), !.
is_atom_init(?C) :-
   current_prolog_flag(upper_case_atoms, on),
   is_upper(?C).

is_atom_char(?C) :- ( is_lower(?C); is_upper(?C); is_digit(?C); is_ok_text(?C) ).

is_letter(?C) :- (is_lower(?C); is_upper(?C)).

is_lower(?C) :-
   ?C >= 0w0061, ?C =< 0w007a,
   !.
is_lower(?C) :-
   ?C < 0w00c0,
   !, fail.
is_lower(?C) :-
   (?C >= 0w00c0, ?C =< 0w1fff
    ;
    ?C >  0w3040, ?C =< 0wd7ff
    ;
    ?C >= 0we000, ?C =< 0wffef).
   
is_upper(?C) :- ?C >= 0w0041, ?C =< 0w005a.

is_ok_text(?C) :- ?C == 0'_.

%--------------------------------------------
% Strings
%

aString(?S) --> { var(?S) }, quoted_chars(0'`, ?Cs), { string_list(?S, ?Cs) }, !.
aString(?S) --> { var(?S) }, quoted_chars(0'", ?Cs), { string_list(?S, ?Cs) }, !.
aString(?S) --> { string(?S) }, { string_list(?S, ?Cs) }, quoted_chars(0'", ?Cs).

quoted_chars(?Q, ?Cs) --> [?Q], non_quotes(?Q, ?Cs).

non_quotes(?Q, [?Q|?Cs]) --> [?Q,?Q], !, non_quotes(?Q, ?Cs).
non_quotes(?Q, []) --> [?Q], !.
non_quotes(?Q, [?C|?Cs]) --> is_escape(?Q, ?C), non_quotes(?Q, ?Cs).
non_quotes(?Q, [?C|?Cs]) --> [?C], non_quotes(?Q, ?Cs).
non_quotes(?Q, []) --> finished, token_error(`missing quote`).

is_escape(_, _) -->
   { current_prolog_flag(string_esc, off), !, fail }.
is_escape(?Q, ?C) -->
   [0'\],
   escape_code(?Q, ?C),
   !.

escape_code(_, ?C) --> [0'n], { atom_codes('\n', [?C]) }.
escape_code(_, ?C) --> [0'a], { atom_codes('\a', [?C]) }.
escape_code(_, ?C) --> [0'b], { atom_codes('\b', [?C]) }.
escape_code(_, ?C) --> [0'f], { atom_codes('\f', [?C]) }.
escape_code(_, ?C) --> [0'r], { atom_codes('\r', [?C]) }.
escape_code(_, ?C) --> [0't], { atom_codes('\t', [?C]) }.
escape_code(_, ?C) --> [0'v], { atom_codes('\v', [?C]) }.
escape_code(_, ?C) --> [0'\], { atom_codes('\\', [?C]) }.
escape_code(?Q, ?Q) --> [?Q].

escape_code(_, ?C) -->
   oct_digits(?Cs),
   [0'\],
   { string_list(?S, [0'0, 0'o|?Cs]),
     string_term(?S, ?C),
     ?C =< 0xffff }.
escape_code(_, ?C) -->
   [0'x],
   hex_digits(?Cs),
   [0'\],
   { string_list(?S, [0'0, 0'x|?Cs]),
     string_term(?S, ?C),
     ?C =< 0xffff }.

escape_code(?Q, ?C) --> [0'n], { atom_codes('\n', [?C]) }.

%--------------------------------------------
% Operators
%

% Note period is not in general a valid op char, but we accept
% the built-in =..

anOperator(?o) -->
   { var(?o) },
   [ 0'=, 0'., 0'. ],
   { ?o = '=..' },
   !.
anOperator(?O) -->
   { var(?O) },
   first_op_char(?C),
   op_chars(?Cs),
   { atom_codes(?O, [?C|?Cs]) },
   !.
anOperator(?O) -->
   { atom(?O) },
   { atom_codes(?O, [?C|?Cs]) },
   first_op_char(?C),
   op_chars(?Cs),
   !.

first_op_char(?C) -->
   [?C],
   { is_graphic(?C), ?C \= 0'. }.

op_chars([?C|?Cs]) --> [?C], { is_graphic(?C) }, !, op_chars(?Cs).
op_chars([]) --> [].

non_graphic --> [?C], { is_graphic(?C) }, !, fail.
non_graphic --> [].

is_graphic(0'#).
is_graphic(0'&).
is_graphic(0'*).
is_graphic(0'+).
is_graphic(0'-).
%is_graphic(0'.).
is_graphic(0'/).
is_graphic(0'\).
is_graphic(0':).
is_graphic(0'<).
is_graphic(0'>).
is_graphic(0'=).
is_graphic(0'@).
is_graphic(0'^).
is_graphic(0'~).
is_graphic(0'?) :-
   current_prolog_flag(vba, off),
   !.
is_graphic(?C) :-
   ?C < 0w00a1,
   !, fail.
is_graphic(?C) :-
   (?C >= 0w00a1, ?C =< 0w00bf
    ;
    ?C >= 0w2010, ?C =< 0x303f).

%--------------------------------------------
% Variables
%

aVariable(v(?VStr, _)) -->
   { var(?VStr) },
   [0'?],
   aatom_chars(?Cs),
   { string_list(?VStr, [0'?|?Cs]) },
   !.
aVariable(v(?VStr, _)) -->
   { string(?VStr) },
   { string_list(?VStr, [0'?|?Cs]) },
   [0'?],
   aatom_chars(?Cs),
   !.
aVariable(v(_)) -->
   [0'?].
     
%--------------------------------------------
% Punctuation
%

aPunctuationMark( pm(' .') ) -->
   is_wh,
   [0'.],
   !.
aPunctuationMark( pm(?PM) ) -->
   { var(?PM) },
   wh,
   [?C],
   { punctuation(?C),
     atom_codes( ?PM, [?C] ) },
   !.
aPunctuationMark( pm(?PM) ) -->
   { nonvar(?PM) ,
     atom_codes( ?PM, [?C] ) },
   wh,
   [?C],
   { punctuation(?C) },
   !.


punctuation(0'.).
punctuation(0',).
punctuation(0';).
punctuation(0')).
punctuation(0'().
punctuation(0']).
punctuation(0'[).
punctuation(0'|).
punctuation(0'}).
punctuation(0'{).
punctuation(0'!).


:- end_module(tokenizer1).