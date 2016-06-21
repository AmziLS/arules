:- include('opdefs.pro').

:- module(parser1).
:- import(list).
%:- import(tokenizer1).  % used to tokenize possible era strings
%:- export parse_rule/5.
%:- export parse_query/5.
%:- export get_just_value/2.
%:- export get_vba_value/2.
%:- export keyword/3.

%-------------------------------------
% debugging tools
%

cf_dcg(?g, ?x, ?x) :-
   ( showx( call: ?g - ?x ), nl ; showx( fail: ?g - ?x ), nl, fail ).
er_dcg(?g, ?x, ?x) :-
   ( showx( exit: ?g - ?x ), nl ; showx( redo: ?g - ?x ), nl, fail ).

finished([], []).

parse_error(?msgid, ?tokens, _) :-
   get_text( ?msgid, ?msg ),
   to_string(?tokens, ?str),
   string_length(?str, ?len),
   ( ?len > 30 ->
      sub_string( ?str, 1, 30, ?sstr ),
      strcat( ?sstr, `...`, ?here )
      ;
      ( ?len > 2 ->
          ?here = ?str
          ;
          ?here = END) ),
   get_text(parse_error(?msg, ?here), ?errmsg),
%   stringlist_concat([ ?msg, ` near here: "...`, ?here, `"`], ?errmsg),
   length(?tokens, ?left),
   maybe_warning(?left, ?errmsg, ?werrmsg),
   throw( parse_error(?werrmsg) ).
   
parse_warning(?msgid, ?tokens, _) :-
   get_text( ?msgid, ?msg ),
   to_string(?tokens, ?str),
   string_length(?str, ?len),
   ( ?len > 30 ->
      sub_string( ?str, 1, 30, ?sstr ),
      strcat( ?sstr, `...`, ?here )
      ;
      ( ?len > 2 ->
          ?here = ?str
          ;
          ?here = END) ),
   get_text(parse_error(?msg, ?here), ?errmsg),
%   stringlist_concat([ ?msg, ` near here: "...`, ?here, `"`], ?errmsg),
	length(?tokens, ?left),
   assert( warning_message(?left, ?errmsg) ),
   !,
   fail.

maybe_warning(_, ?errmsg, ?errmsg) :-
   not warning_message(_, _),
   !.
maybe_warning(?left, ?errmsg, ?werrmsg) :-
   findall( ?l - ?m, warning_message(?l, ?m), ?wks ),
   keysort([?left - ?errmsg | ?wks], ?swks),
   ?swks = [ _ - ?werrmsg | _ ],
   !.

to_string(?tokens, ?str) :-
   stringize(?tokens, ?tstrs),
   stringlist_concat(?tstrs, ?str).

stringize([], []).
stringize([v(?vn,_)|?xs], [` `, ?vn|?ys]) :-
   !,
   stringize(?xs, ?ys).
stringize([v(_)|?xs], [` ?`|?ys]) :-
   !,
   stringize(?xs, ?ys).
stringize([pm('.'), ?x|?xs], [`.`, ?y|?ys]) :-
   string_term(?y, ?x),
   !,
   stringize(?xs, ?ys).
stringize([pm(' .'), ?x|?xs], [` .`, ?y|?ys]) :-
   string_term(?y, ?x),
   !,
   stringize(?xs, ?ys).
stringize([pm(?x)|?xs], [?x|?ys]) :-
   !,
   stringize(?xs, ?ys).
stringize([?x|?xs], [` `, ?y|?ys]) :-
   string_term(?y, ?x),
   !,
   stringize(?xs, ?ys).

%------------------------------------------------
% International predicates
%

%keyphrase(?phrase, ?x1, ?x2) :-
%   language_module(?lm),
%   call( ?lm : keyphrase(?phrase, ?x1, ?x2) ),
%   !.

%keyword(?word) --> keyphrase(?word), !.
keyword(?word) --> 
   [?x],
   { lookup(?x, ?word) }.
%keyword(?word) --> [?x], { lookup([?x|?z], ?word) }, rest_of_phrase(?z).


rest_of_phrase( ?x, ?y, ?z ) :- append(?x, ?z, ?y).

delimiter -->
   [pm( ?x )],
   { argument_delimiter( ?x ) }.

%----------------------------------------------
% Parse Queries
%

parse_query(?default_m, ?goal, ?data) -->
   parse_using(?default_m, ?using_m),
   parse_find(?using_m, ?m, ?goal),
   parse_when(?m, ?data).

parse_using(_, ?using_m) -->
   keyword(USING),
   [?using_m],
   !.
parse_using(?default_m, ?default_m) -->
   [].

parse_find(_, ?m, ?Goal) -->
	keyword(FIND),
	keyword(IN),
	[?m],
%   [Find, in, ?m],
   parse_query_goal(?m, ?Goal).
parse_find(?m1, ?m, ?Goal) -->
   keyword(FIND),
% cf_dcg(   parse_query_goal(?m, ?Goal) ),
   parse_query_goal(?m, ?Goal),
% er_dcg(   parse_query_goal(?m, ?Goal) ),
   in_clause(?m1, ?m).
parse_find(_, _, _) -->
   parse_error(invalid_query).

in_clause(_, ?m) -->
   keyword(IN),
   [?m],
   !.
in_clause(?m, ?m) --> [].

parse_when(?m, ?data) -->
   keyword(WHEN),
   !,
   parse_data(?m, ?data).
parse_when(_, []) --> [].

parse_query_goal(?m, findgs( ?gs ) ) -->
   parse_goal_objects(?m, ?gs).

parse_query_goal(_, _) -->
   parse_error(invalid_query_goal).

parse_goal_objects(?m, [?mo : ?a = _ | ?gs] ) -->
% cf_dcg( parse_object(?m, om(?mo, ?a) ) ),
   parse_object(?m, om(?mo, ?a) ),
% er_dcg( parse_object(?m, om(?mo, ?a) ) ),
   !,
   more_goal_objects(?m, ?gs).

more_goal_objects(?m, [?mo : ?a = _ | ?gs] ) -->
   keyword(AND),
   parse_object(?m, om(?mo, ?a) ),
   !,
   more_goal_objects(?m, ?gs).
more_goal_objects(_, []) -->
   [].
   

parse_data(?m, [?data| ?avs]) -->
   parse_datum(?m, ?data),
   keyword(AND),
   !,
   parse_data(?m, ?avs).
parse_data(?m, [?data]) -->
   parse_datum(?m, ?data),
   !.
parse_data(_, _) -->
   parse_error(invalid_input_data).

parse_datum(?m, data(?mo,?a,?V)) -->
   parse_object(?m, om(?mo, ?a)), 
   [=],
   !,
   parse_value(?V).
parse_datum(?m, data(?mo,?a,true)) -->
   parse_object(?m, om(?mo, ?a)).

%-----------------------------------------------------
% Parse Rules
%
% Vars are handled by the tokenizer, and appear as logic vars
% in the input token list.
%
% Objects are handled at parse time, objects are all converted
% to their values by adding the appropriate 'Find' calls to the
% goals.
%

% ?Head ask ?Prompt

/* finish this sometime, a more efficient, cleaner parser at top */
/*
parse_rule(?m, ?cell, ?head, (explain(?head, ?m, ?cell), ?body)) -->
   parse_object_rule(?m, ?cell, ?head, ?body),
   !.
parse_rule(?m, ?cell, ?head, (explain(?head, ?m, ?cell), ?body)) -->
   parse_logic_rule(?m, ?cell, ?head, ?body),
   !.
parse_rule(_, _, _, _) -->
   parse_error(`Unrecognized rule`).

parse_object_rule(?m, ?cell, ?head, (explain(know(?o, ?x), ?mo, ?cell), ?body)) -->
   parse_object_head(?m, ?h),
   !,
   parse_body(?m, ?b),
   { make_rule(?cell, ?h, ?b, ?head, ?body) }.

parse_object_head(?m, know(?o, ?x)) -->
   parse_object(?m, om(?mo, ?o)),
   object_head_rhs(?m, ?x).

object_head_rhs(?m, ?x) -->
   ['='],
   parse_object(.....
*/

parse_rule(_, _, _, _, _) :-
   retractall( warning_message(_, _) ),
   fail.
parse_rule(?m, ?cell, super(?mss) ) -->
   keyword(INHERIT),
   keyword(FROM),
   parse_supersets(?mss),
   !.
parse_rule(?m, ?cell, ( ?Head :- ?Body ) ) -->
   keyword(WHEN),
   parse_when_body(?m, ?B),
   keyword(THEN),
   parse_head(?m, ?H),
   { make_rule(?cell, ?H, ?B, ?Head, ?Body) },
   !.
parse_rule(?m, ?cell,
      ( ?Head :- ?Body ) ) -->
%cf_dcg(parse_head(?m, ?H)),
   parse_head(?m, ?H),
%er_dcg(parse_head(?m, ?H)),
%cf_dcg(parse_body(?m, ?B)),
   parse_body(?m, ?B),
%er_dcg(parse_body(?m, ?B)),
%cf_dcg(make_rule),
   { make_rule(?cell, ?H, ?B, ?Head, ?Body) },
   !.
parse_rule(_,_,_) -->
   parse_error(rule_syntax_error).

make_rule(?c, rule(?o, ?v), ?b, rule(?c,[?o = ?v]), ?b) :-
   var(?v),
   !.
make_rule(?c, rule( ?ovs ), ?b, rule(?c, ?oxs), (?b, evallist(?exs) ) :-
   make_bxs( ?ovs, ?oxs, ?exs ).
make_rule(?c, ?h, ?b, ?h, ?b).

% A rule can set multiple values, this walks through those values
% and builds an ?e = ?x list that is used by evallist to get all the ?x s.
make_bxs( [], [], [] ).
make_bxs( [?o = ?e | ?ovs], [?o = ?x| ?oxs], [?e = ?x  |?exs] ) :-
   structure(?e),
   !,
   make_bxs(?ovs, ?oxs, ?exs).
make_bxs( [?o = ?v | ?ovs], [?o = ?v| ?oxs], ?exs ) :-
   !,
   make_bxs(?ovs, ?oxs, ?exs).
   
parse_supersets([?ms|?mss]) -->
   [?ms],
   parse_supersets(?mss).
parse_supersets([]) -->
   [].

% head list

parse_head(?m, rule( ?ovs )) -->
   parse_ovs(?m, ?ovs).
% pure Prolog, cannot have an object as an argument
parse_head(?m, ?Head) -->
   logic_head(?Head).
parse_head(_, _) -->
   anatom(?a),
   [pm('(')],
   parse_error(invalid_logic_head(?a)).
parse_head(?m, _) -->
   anatom(?a),
   parse_error(invalid_object_reference(?a)).
parse_head(_, _) -->
   parse_error(invalid_rule_action).

parse_ovs(?m, [?o = ?v | ?ovs]) -->
   parse_ov(?m, ?o, ?v),
   more_ovs(?m, ?ovs).

more_ovs(?m, [?o = ?v | ?ovs]) -->
   keyword(AND),
   parse_ov(?m, ?o, ?v),
   !,
   more_ovs(?m, ?ovs).
more_ovs(?m, []) -->
   [].
   
% .obj = val when ... is a 'set' or 'know' predicate
parse_ov(?m, ?o, ?Val ) -->
%cf_dcg(parse_object),
   parse_object(?m, om(?mo, ?o)),
%er_dcg(parse_object(om(?mo, ?o))),
   ['='],
%cf_dcg(parse_expression),
   parse_expression(?m, ?Val),
%er_dcg(parse_expression(?val)),
   !.
% .obj when implies .obj = true
parse_ov(?m, ?o, true ) -->
   parse_object(?m, om(?mo, ?o)),
   !.
   

% really a structure
logic_head( ?pred ) -->
	anatom(?f),
   [pm('(')],
   !,
   logic_head_arg(?a),
   logic_head_args(?as),
   { ?pred =.. [?f, ?a | ?as] },
   !.
logic_head(?f) -->
   anatom(?f),
   [pm('('), pm(')')].
logic_head(_) -->
   anatom(?f),
   [pm('[')],
   parse_error(square_logic_bracket(?f)).

% Got first arg already, rest begin with ','.
% Can be used to finish list or structure.
% Different logic goals that can include
% object references.
logic_head_args([]) -->
   ( [pm(')')] ;
     [pm(']')] ),
   !.
logic_head_args([?a|?as]) -->
%   [pm(',')],
   delimiter,
   logic_head_arg(?a),
   !,
   logic_head_args(?as).

logic_head_arg(?v) -->
   [v(?v)],
   !.
logic_head_arg(?a) -->
   logic_head_list(?a),
   !.
logic_head_arg(?a) -->
   logic_head(?a),
   !.
logic_head_arg(?a) -->
   [?a],
   { string(?a); number(?a); atom(?a) },
   !.

logic_head_list([?a | ?as]) -->
   [pm('[')],
   logic_head_arg(?a),
   logic_head_list_args(?as).

logic_head_list_args([]) -->
   [pm(']')],
   !.
logic_head_list_args(?t) -->
   [pm('|')],
   logic_head_arg(?t),
   [pm(']')],
   !.
logic_head_list_args([?a| ?as]) -->
%   [pm(',')],
   delimiter,
   logic_head_arg(?a),
   logic_head_list_args(?as).

anatom(?a) -->
   [?a],
   { atom(?a) }.

%------------------------------------------------
% Objects
%
% objects can be:
%    .obj.prop.prop...
%    .prop
%    .obj[arg,arg].prop.prop...
%
% They are represented internally as:
%    .part[widget].color -> obj( [part(widget), color] )

parse_object( ?m, ?o ) -->
% cf_dcg(parse_object(?m, ?o)),
   parse_object( ?m, ?o, [], [] ).

% .obj
parse_object( _, om(?m, ?o), ?fos1, ?fos2 ) -->
% cf_dcg(anatom(?m)),
   anatom(?m),
% er_dcg(anatom(?m)),
% cf_dcg(colonbang),
   ( [':'] ; [pm('!')] ),  % : is a graphic, not a punctuation
% er_dcg(colonbang),
   !,
% cf_dcg(object_list(?o)),
   object_list( ?m, ?o, ?fos1, ?fos2 ).
% er_dcg(object_list(?o)).
parse_object( _, om(?vm, ?o), ?fos1, ?fos2 ) -->
% cf_dcg([v(?vm)]),
   [v(?vm)],
   ( [':'] ; [pm('!')] ),  % : is a graphic, not a punctuation
   !,
   object_list( ?vm, ?o , ?fos1, ?fos2).
parse_object( ?m, om(?m, ?o), ?fos1, ?fos2 ) -->
%cf_dcg(object_list(?m, ?o)),
   object_list( ?m, ?o, ?fos1, ?fos2).

% object_list is called from arule.pro as well for building
% objects refered to in tables.  It avoids the fos arguments
% because there shouldn't be any.
object_list( ?m, obj([?o|?ps]) ) --> object_list( ?m, obj([?o|?ps]), [], [] ).

% The real object_list
object_list( ?m, obj([?o|?ps]), ?fos1, ?fos3 ) -->
   ( [pm(' .')] ; [pm('.')] ),  % use ' .' first for reconstruction
   !,
%cf_dcg(object_spec),
   object_spec(?m, ?o, ?fos1, ?fos2),
%cf_dcg(more_object_props),
   more_object_props(?m, ?ps, ?fos2, ?fos3),
   !.
/* don't allow non preperiod objects anymore
object_list( obj([?o|?ps]) ) -->
   object_spec(?o),
   object_props(?ps),
   !.
*/

% obj(arg, arg) args are primitives
object_spec(?m, ?ObjArgs, ?fos1, ?fos2) -->
   { var(?ObjArgs) },
   anatom(?Obj),
   [pm('[')],
   object_args(?m, ?Args, ?fos1, ?fos2),
   { ?ObjArgs =.. [?Obj|?Args] },
   !.
object_spec(?m, ?ObjArgs, ?fos1, ?fos2) -->
   { structure(?ObjArgs) },
   { ?ObjArgs =.. [?Obj|?Args] },
   anatom(?Obj),
   [pm('[')],
   object_args(?m, ?Args, ?fos1, ?fos2),
   !.
object_spec(_, ?ObjArgs, ?fos, ?fos) -->
   { var(?ObjArgs) },
   anatom(?Obj),
   [pm('(')],
   parse_error(object_parens(?Obj)),
   !.
object_spec(_, ?Obj, ?fos, ?fos) -->
   anatom(?Obj).

object_args(?m, [?Arg], ?fos1, ?fos2) -->
   object_arg(?m, ?Arg, ?fos1, ?fos2),
   [pm(']')],
   !.
object_args(?m, [?Arg|?Args], ?fos1, ?fos3) -->
   object_arg(?m, ?Arg, ?fos1, ?fos2),
%   [pm(',')],
%cf_dcg(delimiter), { cntr_get(3, ?lang), nl, write(lang=?lang), nl },
   delimiter,
%er_dcg(delimiter),
   !,
   object_args(?m, ?Args, ?fos2, ?fos3).
object_args(_, _, ?fos, ?fos) -->
   parse_error(invalid_object_argument).

% The ?fos arguments are used to build find calls to replace
% a variable in the argument for the particular case of an
% argument being an object.

object_arg(_, ?v, ?fos, ?fos) -->
   { var(?v) },  % that is, we're forward parsing
   [v(?v)],
   !.
object_arg(_, ?a, ?fos, ?fos) -->
   parse_value(?a),
   !.
object_arg(?m, ?v, ?fos, [find(?mo,?o,?v) | ?fos] ) -->
   parse_object(?m, om(?mo,?o)).

/*
object_arg(?a) -->
   [?a],
   { atom(?a); string(?a); number(?a) },
   !.
object_arg(?v) -->
   [v(?v)],
   !.
*/

object_props(?m, [?Prop|?Props]) -->
   ( [pm('.')] ; [pm(' .')] ),
   object_spec(?m, ?Prop),
   !,
   more_object_props(?m, ?Props).

more_object_props(?m, [?p|?ps], ?fos1, ?fos3) -->
   ( [pm('.')] ; [pm(' .')] ),
   object_spec(?m, ?p, ?fos1, ?fos2),
   !,
   more_object_props(?m, ?ps, ?fos2, ?fos3).
more_object_props(_, ?ps, ?fos, ?fos) -->
   { var(?ps) },   % not an error when going backwards
   ( [pm('.')] ; [pm(' .')] ),
   parse_error(invalid_object_property).
more_object_props(_, [], ?fos, ?fos) -->
   [].
   
%---------------------------------------------------------
% Body
%

% this called for a WHEN ... THEN ... rule.
parse_when_body(?m, ?b) -->
   parse_goals(?m, ?b).
parse_when_body(_, _) -->
   parse_error(bad_rule_body).

% this parse_body is for a HEAD WHEN rule,
% in which the WHEN might be missing.
parse_body(_, true, [], []).
parse_body(?m, ?Body) -->
   keyword(WHEN),
%cf_dcg(parse_goals(?m, ?body)),
   parse_goals(?m, ?Body).
%er_dcg(parse_goals(?m, ?body)).
parse_body(_, _) -->
   parse_error(bad_rule_body).

/*
parse_goals(?m, ( (?c1) ; ?c2) ) -->
%cf_dcg( parse_and_goals1(?c1) ),
   parse_and_goals(?m, ?c1),
%er_dcg( parse_and_goals2(?c1) ),
   keyword(OR),
%cf_dcg( parse_goals3(?c2) ),
   parse_goals(?m, ?c2).
%er_dcg( parse_goals4(?c2) ).

parse_goals(?m, (?c) ) -->
%cf_dcg( parse_and_goals5(?c) ),
   parse_and_goals(?m, ?c).
%er_dcg( parse_and_goals6(?c) ).
*/

parse_goals(?m, ( ?c ) -->
   parse_and_goals(?m, ?c1),
   or_goals(?m, ?c1, ?c).

or_goals(?m, ?c1, ( ?c1 ; ?c2 ) ) -->
   keyword(OR),
   parse_goals(?m, ?c2),
   !.
or_goals(?m, ?c, ?c) --> [].

/*
parse_and_goals(?m, (?c1 , ?c2) ) -->
%cf_dcg( parse_goal7(?c1) ),
   parse_goal(?m, ?c1),
%er_dcg( parse_goal8(?c1) ),
   keyword(AND),
%cf_dcg( parse_and_goals9(?c2) ),
   parse_and_goals(?m, ?c2).
%er_dcg( parse_and_goals10(?c2) ).

parse_and_goals(?m, ?c) -->
cf_dcg( parse_goal11(?c) ),
   parse_goal(?m, ?c).
er_dcg( parse_goal12(?c) ).
*/

parse_and_goals(?m, ?c ) -->
   parse_goal(?m, ?c1),
   and_goals(?m, ?c1, ?c).

and_goals(?m, ?c1, (?c1, ?c2)) -->
   keyword(AND),
   parse_and_goals(?m, ?c2),
   !.
and_goals(?m, ?c, ?c) --> [].



parse_goal(?m,  (?goals) ) -->
   [pm('(')],
%cf_dcg(parse_goals(?m, ?goals)),
   parse_goals(?m, ?goals),
%er_dcg(parse_goals(?m, ?goals)),
   [pm(')')],
%   ( [pm(')')] ; parse_error(missing_paren) ),
   !.
parse_goal(?m,  ! ) -->
   [pm(!)],
   !.
parse_goal(?m,  ! ) -->
   keyword(CUT),
   !.
parse_goal(?m,  not( ?Cond ) ) -->
   keyword(NOT),
   parse_goal(?m, ?Cond),
   !.
/*
%parse_goal(?m,  (find(?mo1, ?o1, ?v1) , find(?mo2, ?o2, ?v2) , ?test) ) -->
parse_goal(?m,  (?f1, ?f2, ?test) ) -->
   parse_object(?m, om(?mo1, ?o1)),
   { build_find( om(?mo1, ?o1), ?v1, ?f1 ) },
   compare_operator(?op),
   parse_object(?m, om(?mo2, ?o2)),
   { build_find( om(?mo2, ?o2), ?v2, ?f2 ) },
   { ?test = test(?op, ?v1, ?v2) }.
%   !.
%parse_goal(?m,  (find(?mo, ?o1,?v1) , ?ev, ?test) ) -->
parse_goal(?m,  (?f1, ?ev, ?test) ) -->
   parse_object(?m, om(?mo,?o1)),
   compare_operator(?op),
   parse_expression(?m, ?e2),
   { build_find( om(?mo, ?o1), ?v1, ?f1 ) },
   { (structure(?e2) -> ?ev = eval(?e2,?v2); ?ev = true, ?v2 = ?e2) },
   { ?test = test(?op, ?v1, ?v2) }.
%   !.
*/

% Will this work?  with expressions on either side of an operator?
% I have a nagging doubt that it causes some sorts of problems.
parse_goal(?m,  (?ev1 , ?ev2, ?test) ) -->
   parse_expression(?m, ?e1),
   compare_operator(?op),
   parse_expression(?m, ?e2),
   { (structure(?e1) -> ?ev1 = eval(?e1,?v1); ?ev1 = true, ?v1 = ?e1) },
   { (structure(?e2) -> ?ev2 = eval(?e2,?v2); ?ev2 = true, ?v2 = ?e2) },
   { ?test = test(?op, ?v1, ?v2) },
   !.
parse_goal(?m,  eval(?bf, true) ) -->
   parse_boolean_function(?m, ?bf),
   !.
parse_goal(?m,  find(?mo, ?o, true) ) -->
   parse_object(?m, om(?mo,?o)),
   !.
parse_goal(?m, (?ev, ?v = ?v2) ) -->
   [v(?v), '='],
   parse_expression(?m, ?e2),
   { (structure(?e2) -> ?ev = eval(?e2,?v2); ?ev = true, ?v2 = ?e2) },
   !.
parse_goal(?m,  ?goal ) -->
   ( logic_goal(?m,  ?a1 ) ; [v(?a1)] ),
   infix_operator( ?op ),
   ( logic_goal(?m,  ?a2 ) ; [v(?a2)] ),
   { ?goal =.. [?op, ?a1, ?a2] },
   !.
parse_goal(?m,  ?goal ) -->
   logic_goal(?m,  ?goal ),
   !.
% /.* because of ambiguity between ( goals ) and ( expression ) can't assume this is error
parse_goal(_, _) -->
   parse_warning(invalid_rule_condition).


% someone found an object, but wants the function call instead
build_find( om(?mo, ?ostar), ?v, vector(?x, om(?mo, ?o), ?v) ) :-
   fix_star(?ostar, ?x, ?o),
   !.
build_find( om(?mo, ?o), ?v, find(?mo, ?o, ?v) ).

% and the object might have had an object, that requires earlier finds
/*
build_find( om(?mo, ?ostar), ?fos, ?v, eval([?fosc, vector(?x, om(?mo, ?o), ?v)] ) :-
   fix_star(?ostar, ?x, ?o),
   to_commas(?fos, ?fosc),
   !.
*/

build_find( om(?mo, ?ostar), ?fos, ?v, ?ff ) :-
   fix_star(?ostar, ?x, ?o),
   (?fos = [] ->
      ?ff = vector(?x, om(?mo, ?o), ?v)
      ;
      append(?fos, [vector(?x, om(?mo, ?o), ?v)], ?f),
      ?ff = findlist(?f) ),
   !.
build_find( om(?mo, ?o), [], ?v, find(?mo, ?o, ?v) ) :- !.
build_find( om(?mo, ?o), ?fos, ?v, findlist(?f) ) :-
   append(?fos, [find(?mo, ?o, ?v)], ?f),
   !.

% Replace the star with a variable that is pulled out
% so it can be used in bagof to make vector/3 work.
fix_star(obj(?olstar), ?x, obj(?ol)) :-
   fix_star_ol(?olstar, ?x, ?ol).

fix_star_ol([ ?starprop | ?props ], ?x, [ ?varprop | ?props ]) :-
   ?starprop =.. [?prop | ?starargs ],
   fix_star_arg( ?starargs, ?x, ?varargs ),
   ?varprop =.. [?prop | ?varargs],
   !.
fix_star_ol([ ?prop | ?starprops], ?x, [?prop | ?varprops]) :-
   !,
   fix_star_ol( ?starprops, ?x, ?varprops ).

fix_star_arg( [?stararg | ?args], ?x, [?x | ?args] ) :-
   ?stararg == *,
   !.
fix_star_arg( [?arg | ?starargs], ?x, [?arg | ?varargs]) :-
   fix_star_arg( ?starargs, ?x, ?varargs ).

logic_goal(?m, (?fos, call_logic(?mo: ?pred) )) -->
%logic_goal(?m, (?fos, (?mo: ?pred) )) -->
   anatom(?mo),
   ( [':'] ; [pm('!')] ),
   sub_logic_goal(?m, ?pred, [], ?fosl),
   { to_commas(?fosl, ?fos) },
   !.
logic_goal(?m,  (?fos, call_logic(?m: ?pred) ) ) -->
%logic_goal(?m,  (?fos, (?m: ?pred) ) ) -->
   sub_logic_goal(?m, ?pred, [], ?fosl),
   { to_commas(?fosl, ?fos) },
   !.
logic_goal(?m, call_logic(?m: ?f) ) -->
%logic_goal(?m, (?m: ?f) ) -->
   anatom(?f),
   [pm('('), pm(')')],
   !.
logic_goal(?m, call_logic(?mo: ?f) ) -->
%logic_goal(?m, (?mo: ?f) ) -->
   anatom(?mo),
   ( [':'] ; [pm('!')] ),
   anatom(?f),
   [pm('('), pm(')')],
   !.
   
sub_logic_goal(?m,  ?pred, ?fos, ?fos2 ) -->
	anatom(?f),
   [pm('(')],
   !,
   logic_goal_arg(?m, ?a, ?fos, ?fos1),
   logic_goal_args(?m, ?as, ?fos1, ?fos2),
   { ?pred =.. [?f, ?a | ?as] },
   !.

to_commas([], true) :-
   !.
to_commas([?ol], ?o) :-
   list(?ol),
   to_commas(?ol, ?o),
   !.
to_commas([?o], ?o) :-
   !.
to_commas([?ol|?os], (?fos, ?o) ) :-
   list(?ol),
   to_commas(?ol, ?o),
   !,
   to_commas(?os, ?fos).
to_commas([?o|?os], (?fos, ?o) ) :-
   !,
   to_commas(?os, ?fos).


% Got first arg already, rest begin with ','.
% Can be used to finish list or structure.
% Different logic goals that can include
% object references.

% Each logic goal can have as an argument an object
% or expression.  The ?fos argument is used to build
% the finds etc. necessary for evaluating the argument
% before the call.

logic_goal_args(_, _, _, _) -->
   [pm(')'), pm('.')],
   parse_error(need_square_brackets).
logic_goal_args(?m, [], ?fos, ?fos) -->
   ( [pm(')')] ;
     [pm(']')] ),
   !.
logic_goal_args(?m, [?a|?as], ?fos, ?fos2) -->
%   [pm(',')],
   delimiter,
   logic_goal_arg(?m, ?a, ?fos, ?fos1),
   !,
   logic_goal_args(?m, ?as, ?fos1, ?fos2).

logic_goal_arg(?m, ?v, ?fos, ?fos) -->
   [v(?v)],
   !.
logic_goal_arg(?m, ?v, ?fos, [find(?mo,?o,?v) | ?fos] ) -->
   parse_object(?m, om(?mo,?o)),
   !.
logic_goal_arg(?m, ?a, ?fos, ?fos2) -->
   sub_logic_goal(?m, ?a, ?fos, ?fos2),
   !.
logic_goal_arg(?m, ?a, ?fos, ?fos2) -->
   logic_goal_list(?m, ?a, ?fos, ?fos2),
   !.
logic_goal_arg(?m, ?a, ?fos, ?fos) -->
   [?a],
   { number(?a); atom(?a); var(?a); string(?a) },
   !.

logic_goal_list(?m,  [?a | ?as], ?fos1, ?fos3 ) -->
   [pm('[')],
   logic_goal_arg(?m, ?a, ?fos1, ?fos2),
   logic_goal_list_args(?m, ?as, ?fos2, ?fos3).

logic_goal_list_args(?m, [], ?fos, ?fos) -->
   [pm(']')],
   !.
logic_goal_list_args(?m, ?t, ?fos1, ?fos2) -->
   [pm('|')],
   logic_goal_arg(?m, ?t, ?fos1, ?fos2),
   [pm(']')],
   !.
logic_goal_list_args(?m, [?a| ?as], ?fos1, ?fos3) -->
%   [pm(',')],
   delimiter,
   logic_goal_arg(?m, ?a, ?fos1, ?fos2),
   logic_goal_list_args(?m, ?as, ?fos2, ?fos3).

infix_operator( =< ) -->
   [ <= ],
   !.
infix_operator(?op) -->
   [?op],
   { current_op(_, xfx, ?op) },
   !.

compare_operator( =< ) --> [ <= ], !.
compare_operator( \= ) --> [ <> ], !.
compare_operator( ?op ) -->
   [ ?op ],
   { is_compare_op(?op) },
   !.
   
is_compare_op(?op) :-
   is_member(?op, [>, <, >=, =<, =] ).

%--------------------------------------------------------------------------

% Change a simple atom to ?attr = ?val, where ?val = true
parse_condition( ?A = true ) -->
   [?A].

% Called from arxl api to get values from strings
get_just_value(?v, ?v) :-
   not string(?v),
   !.
get_just_value(?s, ?v) :-
   catch( (
      t_tokenize(?s, ?t),
      parse_value(?v, ?t, []) ), _, fail),
   !.
get_just_value(?s, ?v) :-
   string_atom(?s, ?v).

get_vba_value(?v, ?vs) :-
   not string(?v),
   string_termq(?vs, ?v),
   !.
get_vba_value(?s, ?vs) :-
% quietly fail and quote as string if it doesn't parse
   catch( (
      t_tokenize(?s, ?t),
      parse_value(?v, ?t, []),
      string_termq(?vs, ?v) ), _, fail),
   !.
get_vba_value(?s, ?vs) :-
   string_length(?s, ?l),
   ( ?l < 50 -> 
      string_atom(?s, ?v),
      string_termq(?vs, ?v)
      ;
      string_termq(?vs, ?s) ).

parse_just_value(?v) -->
   parse_value(?v),
   !.
   
parse_value(?dt) -->
   parse_datetime(?dt),
   !.
parse_value(?era) -->
   parse_era(?era),
   !.
parse_value(?v) -->
   { var(?v) },
   [v(?v)],
   !.
parse_value(?vv) -->
   [-, ?v],
   { number(?v), ?vv is - ?v },
   !.
parse_value(?v) -->
   [?v],
   { number(?v); atom(?v); string(?v)  },
   !.
parse_value(?v) -->
   parse_array(?v).
/*
parse_value(?v) -->
   parse_error(`Invalid property value`).
*/

parse_array([?x | ?z]) -->
   [pm('[')],
   parse_value(?x),
   parse_more_array(?z).

parse_more_array( [] ) -->
   [pm(']')],
   !.
parse_more_array( [?x | ?z] ) -->
%   [pm(',')],
   delimiter,
   parse_value(?x),
   parse_more_array(?z).

% datetime

% only numbers and vars allowed in datetimes
parse_dtvalue(?v) -->
   { var(?v) },
   [v(?v)],
   !.
parse_dtvalue(?v) -->
   [?v],
   { number(?v) },
   !.
   
parse_datetime(datetime(?y,?m,?d, 0,0,0)) -->
   keyword(date),
   [pm('(')],
   parse_dtvalue(?y),
%   [pm(',')],
   delimiter,
   parse_dtvalue(?m),
%   [pm(',')],
   delimiter,
   parse_dtvalue(?d),
   [pm(')')],
   !.
parse_datetime(datetime(?y,?m,?d, ?h,?n,?s)) -->
   keyword(datetime),
   [pm('(')],
   parse_dtvalue(?y),
   delimiter,
   parse_dtvalue(?m),
   delimiter,
   parse_dtvalue(?d),
   delimiter,
   parse_dtvalue(?h),
   delimiter,
   parse_dtvalue(?n),
   delimiter,
   parse_dtvalue(?s),
   [pm(')')],
   !.

% a cell might have contained era information and wound up
% putting it as text in an atom.  Those possibilities are
% flagged on the vba side as possible eras.  We need to catch
% and fail, because if the string doesn't parse, then it wasn't
% an era.
parse_era(?e) -->
   [?s],
   { string(?s),
     catch( t_tokenize(?s, ?ts), _, fail ),
     parse_era(?e, ?ts, [])  },
   !.
/* is this still used?
% it wasn't really an era, just some text with year month
% or day in it.
parse_era(?a) -->
   [pera(?a)],
   !.
*/
parse_era(era(?yy,?mm,?dd,?hh,?nn,?ss)) -->
   parse_years(?y),
   parse_months(?m),
   parse_days(?d),
   parse_hours(?h),
   parse_mins(?n),
   parse_secs(?s),
   { ?l = [?y, ?m, ?d, ?h, ?n, ?s],
     ?l \= [na, na, na, na, na, na],
     natz( ?l, [?yy, ?mm, ?dd, ?hh, ?nn, ?ss] ) },
%   { era(?y,?m,?d,?h,?n,?s) \= era(0,0,0,0,0,0) },
   !.


% We need to distinguish between no time units specified,
% and a specification of say just "0 days".
natz( [], [] ) :- !.
natz( [na | ?nas], [0 | ?zs] ) :- !, natz( ?nas, ?zs ).
natz( [?n | ?ns], [?n | ?nns] ) :- !, natz( ?ns, ?nns ).

parse_years(?y) -->
   (parse_dtvalue(?y); ['+'], parse_dt_value(?y)),
   keyword(years),
   !.
parse_years(- ?y) -->
   ['-'], parse_dtvalue(?y),
   keyword(years),
   !.
parse_years(na) --> [].
   
parse_months(?m) -->
   (parse_dtvalue(?m); ['+'], parse_dtvalue(?m)),
   keyword(months),
   !.
parse_months(- ?m) -->
   ['-'], parse_dtvalue(?m),
   keyword(months),
   !.
parse_months(na) --> [].
   
parse_days(?d) -->
   (parse_dtvalue(?d); ['+'], parse_dtvalue(?d)),
   keyword(days),
   !.
parse_days(- ?d) -->
   ['-'], parse_dtvalue(?d),
   keyword(days),
   !.
parse_days(?w) -->
   (parse_dtvalue(?d); ['+'], parse_dtvalue(?d)),
   keyword(weeks),
   { ?w is 7 * ?d },
   !.
parse_days(- ?w) -->
   ['-'], parse_dtvalue(?d),
   keyword(weeks),
   { ?w is 7 * ?d },
   !.
parse_days(na) --> [].
   
parse_hours(?h) -->
   (parse_dtvalue(?h); ['+'], parse_dtvalue(?h)),
   keyword(hours),
   !.
parse_hours(- ?h) -->
   ['-'], parse_dtvalue(?h),
   keyword(hours),
   !.
parse_hours(na) --> [].
   
parse_mins(?n) -->
   (parse_dtvalue(?n); ['+'], parse_dtvalue(?n)),
   keyword(minutes),
   !.
parse_mins(- ?n) -->
   ['-'], parse_dtvalue(?n),
   keyword(minutes),
   !.
parse_mins(na) --> [].
   
parse_secs(?s) -->
   (parse_dtvalue(?s); ['+'], parse_dtvalue(?s)),
   keyword(seconds),
   !.
parse_secs(- ?s) -->
   ['-'], parse_dtvalue(?s),
   keyword(seconds),
   !.
parse_secs(na) --> [].
   
% functions

parse_function( ?m, ?function ) -->
   [?x],
   { lookup(?x, ?w) },
   !,
   parse_function(?w, ?m, ?function).
   
% findall is different because the second argument is a goal
parse_function(FINDALL, ?m, FINDALL(?x, ?g) ) -->
%   keyword(FINDALL),
   [pm('(')],
   function_arg(?m, ?x),
   delimiter,
   parse_goal(?m, ?g),
%   { to_findas(?g, ?ga) },
%   ( [pm(')')] ; parse_error(missing_paren(FINDALL)) ),
   [pm(')')],
   !.

parse_function(COLLECT, ?m, FINDALL(?x, (?g1, ?g2)) ) -->
%   keyword(COLLECT),
   [pm('(')],
   function_arg(?m, ?x),
   delimiter,
   parse_goal(?m, ?g1),
   delimiter,
   parse_goal(?m, ?g2),
%   { to_findas(?g, ?ga) },
%   ( [pm(')')] ; parse_error(missing_paren(FINDALL)) ),
   [pm(')')],
   !.

parse_function(COLLECT, ?m, FINDALL(?x, (?g1, ?g2)) -->
%   keyword(COLLECT),
   function_arg(?m, ?x),
   keyword(FOREACH),
   parse_goal(?m, ?g1),
   keyword(WHERE),
   parse_goal(?m, ?g2).

/*
% EXISTS and IS_KNOWN require just a pure object as args.
parse_function(?m, EXISTS(?o)) -->
   keyword(EXISTS),
   [pm('(')],
   parse_object(?m, ?o),
   [pm(')')],
   !.
parse_function(?m, IS_KNOWN(?o)) -->
   keyword(IS_KNOWN),
   [pm('(')],
   parse_object(?m, ?o),
   [pm(')')],
   !.
*/
   
/*
parse_function( ?m, ?f ) -->
   function_name(?n, ?a, _),
   [pm('(')],
   function_args(?a, ?m, ?args),
%   ( [pm(')')] ; parse_error(missing_paren(?n)) ),
   [pm(')')],
   { ?f =.. [?n|?args] },
   !.
*/
parse_function(?NAME, ?m, ?f ) -->
   { function_argn_ret(?NAME, ?a, value) },
   [pm('(')],
   function_args(?a, ?m, ?args),
%   ( [pm(')')] ; parse_error(missing_paren(?n)) ),
   [pm(')')],
   { ?f =.. [?NAME |?args] },
   !.

% parse_boolean_function is called directly by parse_goal for those
% functions that return true or false.

parse_boolean_function( ?m, ?function ) -->
   [?x],
   { lookup(?x, ?w) },
   !,
   parse_boolean_function(?w, ?m, ?function).

parse_boolean_function(ONETIME, ?m, ONETIME( ?g ) ) -->
%   keyword(ONETIME),
   [pm('(')],
   parse_goal(?m, ?g),
%   { to_findas(?g, ?ga) },
   [pm(')')],
   !.
   
parse_boolean_function(?NAME, ?m, ?f ) -->
   { function_argn_ret(?NAME, ?a, boolean) },
   [pm('(')],
   function_args(?a, ?m, ?args),
%   ( [pm(')')] ; parse_error(missing_paren(?n)) ),
   [pm(')')],
   { ?f =.. [?NAME|?args] },
   !.

   
%-----------------------------

function_args(0, _, []) --> [].
function_args(1, ?m, [?arg]) -->
   function_arg(?m, ?arg),
   !.
function_args(?a, ?m, [?arg | ?args]) -->
   function_arg(?m, ?arg),
   delimiter,
   { ?aa is ?a - 1 },
   !,
   function_args(?aa, ?m, ?args).

/*
function_name(AFTER, 2, value) --> keyword(AFTER).
function_name(AGE_TO_LISTSTRING, 1, value) --> keyword(AGE_TO_LISTSTRING).
function_name(APPEND, 2, value) --> keyword(APPEND).
function_name(ASK, 1, value) --> keyword(ASK).
function_name(ASK, 3, value) --> keyword(ASK).
function_name(BEFORE, 2, value) --> keyword(BEFORE).
function_name(CONCATENATE, 1, value) --> keyword(CONCATENATE).
function_name(COUNT, 1, value) --> keyword(COUNT).
function_name(DAY, 1, value) --> keyword(DAY).
function_name(DEBUGPRINT, 1, value) --> keyword(DEBUGPRINT).
function_name(DIFFERENCE, 2, value) --> keyword(DIFFERENCE).
function_name(EVALUATE, 1, value) --> keyword(EVALUATE).
function_name(EXISTS, 1, boolean) --> keyword(EXISTS).
function_name(FIRST, 1, value) --> keyword(FIRST).
function_name(FORMAT_DATE, 2, value) --> keyword(FORMAT_DATE).
function_name(INTERSECTION, 2, value) --> keyword(INTERSECTION).
function_name(INDEX, 2, value) --> keyword(INDEX).
function_name(IS_KNOWN, 1, boolean) --> keyword(IS_KNOWN).
function_name(IS_RULESET, 1, boolean) --> keyword(IS_RULESET).
function_name(IS_SUBSET, 2, boolean) --> keyword(IS_SUBSET).
function_name(ITEM_AT, 2, value) --> keyword(ITEM_AT).
function_name(LAST, 1, value) --> keyword(LAST).
function_name(LIST, 1, value) --> keyword(LIST).
function_name(LIST_TO_STRING, 2, value) --> keyword(LIST_TO_STRING).
function_name(MAKE_DATE, 3, value) --> keyword(MAKE_DATE).
function_name(MAKE_DATETIME, 6, value) --> keyword(MAKE_DATETIME).
function_name(MAXIMUM, 2, value) --> keyword(MAXIMUM).
function_name(MAXIMUM, 1, value) --> keyword(MAXIMUM).
function_name(MEMBER, 1, value) --> keyword(MEMBER).
function_name(MINIMUM, 2, value) --> keyword(MINIMUM).
function_name(MINIMUM, 1, value) --> keyword(MINIMUM).
function_name(MONTH, 1, value) --> keyword(MONTH).
function_name(MSGBOX, 1, boolean) --> keyword(MSGBOX).
function_name(NEXT, 2, value) --> keyword(NEXT).
function_name(PERMUTE, 1, value) --> keyword(PERMUTE).
function_name(PRIOR, 2, value) --> keyword(PRIOR).
function_name(RANGE, 1, value) --> keyword(RANGE).
function_name(REMOVE_DUPLICATES, 1, value) --> keyword(REMOVE_DUPLICATES).
function_name(REVERSE, 1, value) --> keyword(REVERSE).
function_name(QUIT, 1, boolean) --> keyword(QUIT).
function_name(SET, 1, value) --> keyword(SET).
function_name(SORT, 1, value) --> keyword(SORT).
function_name(SQL, 2, value) --> keyword(SQL).
function_name(SUM, 1, value) --> keyword(SUM).
function_name(UNION, 2, value) --> keyword(UNION).
function_name(WEEKDAY, 1, value) --> keyword(WEEKDAY).
function_name(YEAR, 1, value) --> keyword(YEAR).
*/

function_argn_ret(AFTER, 2, value).
function_argn_ret(AGE_TO_LISTSTRING, 1, value).
function_argn_ret(APPEND, 2, value).
function_argn_ret(ASK, 1, value).
function_argn_ret(ASK, 3, value).
function_argn_ret(BEFORE, 2, value).
function_argn_ret(CONCATENATE, 1, value).
function_argn_ret(COUNT, 1, value).
function_argn_ret(DAY, 1, value).
function_argn_ret(DEBUGPRINT, 1, value).
function_argn_ret(DIFFERENCE, 2, value).
function_argn_ret(EVALUATE, 1, value).
function_argn_ret(EXISTS, 1, boolean).
function_argn_ret(EXTRACT_DATE, 1, value).
function_argn_ret(EXTRACT_TIME, 1, value).
function_argn_ret(FIRST, 1, value).
function_argn_ret(FORMAT_DATE, 2, value).
function_argn_ret(INTERSECTION, 2, value).
function_argn_ret(INDEX, 2, value).
function_argn_ret(IS_KNOWN, 1, boolean).
function_argn_ret(IS_RULESET, 1, boolean).
function_argn_ret(IS_SUBSET, 2, boolean).
function_argn_ret(ITEM_AT, 2, value).
function_argn_ret(LAST, 1, value).
function_argn_ret(LIST, 1, value).
function_argn_ret(LIST_TO_STRING, 2, value).
function_argn_ret(MAKE_DATE, 3, value).
function_argn_ret(MAKE_DATETIME, 6, value).
function_argn_ret(MAXIMUM, 2, value).
function_argn_ret(MAXIMUM, 1, value).
function_argn_ret(MEMBER, 1, value).
function_argn_ret(MINIMUM, 2, value).
function_argn_ret(MINIMUM, 1, value).
function_argn_ret(MONTH, 1, value).
function_argn_ret(MSGBOX, 1, boolean).
function_argn_ret(NEXT, 2, value).
function_argn_ret(PERMUTE, 1, value).
function_argn_ret(PRIOR, 2, value).
function_argn_ret(RANGE, 1, value).
function_argn_ret(REMOVE_DUPLICATES, 1, value).
function_argn_ret(REVERSE, 1, value).
function_argn_ret(QUIT, 1, boolean).
function_argn_ret(SET, 1, value).
function_argn_ret(SORT, 1, value).
function_argn_ret(SQL, 2, value).
function_argn_ret(SUM, 1, value).
function_argn_ret(MINUTES_BETWEEN, 2, value).
function_argn_ret(TIMEIT, 1, value).
function_argn_ret(UNION, 2, value).
function_argn_ret(WEEKDAY, 1, value).
function_argn_ret(YEAR, 1, value).

%function_arg(?m, ?a) --> rexpterm(?m, ?a).
function_arg(?m, ?a) --> parse_expression(?m, ?a).

%function_arg(?m, ?o) --> parse_object(?m, ?o), !.
%function_arg(_, ?v) --> parse_value(?v).

%-----------------------------------------------------
% Operator Expressions
%

% First tokenize the input stream and then evaluate
% the list as an expression.

parse_expression(?m, ?E) -->
   rexpression(?m, ?E),
   !.

rexpression(?m, ?E) -->
   rexplist(?m, ?ExpList),
   { rexp(?ExpList, ?E) }.
% /.* because of ambiguity between ( goals ) and ( expression ) can't assume this is error
rexpression(_, _) -->
   parse_warning(invalid_expression).


rexplist(?m, ?ExpList) -->
   rphrase(?m, ?ExpList1),
%   !,
   rmore_expression(?m, ?ExpList2),
   { append(?ExpList1, ?ExpList2, ?ExpList) }.

% put the phrase in as a list, so it is expanded
% by itself first as its operators, if any, can't
% override the infix operator

rmore_expression(?m, []) -->
   rpeek_exp_end,
   !.
rmore_expression(?m, ?ExpList) -->
   rinfix(?Op,?P,?A),
%   { not(goal_op(Context,Op)) },
   rphrase(?m, ?ExpList1),
   { append([opx(?Op,?P,?A)|?ExpList1], ?ExpList2, ?ExpList) },
   !,
   rmore_expression(?m, ?ExpList2).
rmore_expression(?m, []) --> [].
  % parse_error(`possible operator errors parsing expression`).


rphrase(?m, [opx(?Op,?P,?A)|?L]) -->
   rprefix(?Op,?P,?A),
   rphrase(?m, ?L).
rphrase(?m, [?X|?Z]) -->
   rexpterm(?m, ?X),
   !,
   rpostfixes(?Z).
%aphrase(_,_,_,_) -->
%   parse_error(`bad phrase in expression`).

% Can't cut on the search for postfixes, the postfix op
% might actually be an infix op.
rpostfixes([opx(?Op,?P,?A)|?Z]) -->
   rpostfix(?Op,?P,?A),
   rpostfixes(?Z).   
rpostfixes([]) --> [].  

rexpterm(?m, paren(?EL)) -->
   [pm('(')],
   rexpression(?m, ?EL),
   [pm(')')],
   !.
%rexpterm(?m,  om(?mo,?o) ) -->
%rexpterm(?m,  ?f ) -->
%   parse_object(?m, om(?mo,?o), [], []),
%   { build_find(om(?mo, ?o), ?v, ?f) },
%   !.
rexpterm(?m,  ?f ) -->
   parse_object(?m, om(?mo,?o), [], ?fos),
   { build_find(om(?mo, ?o), ?fos, ?v, ?f) },
   !.
rexpterm(?m, ?f) -->
   parse_function(?m, ?f),
   !.
rexpterm(?m, ?s) -->
   parse_special(?m, ?s),
   !.
rexpterm(?m, ?T) -->
   parse_value(?T),
   !.
%rexpterm(?m, [?a | ?as]) -->
rexpterm(?m, explist([?a | ?as])) -->
   [pm('[')],
%   parse_value(?a),
   parse_expression(?m, ?a),
   parse_list(?m, ?as),
   !.
rexpterm(_, _) -->
   parse_error(bad_expression_term).

parse_list(?m, []) -->
   [ pm(']') ],
   !.
parse_list(?m, ?t) -->
   [ pm('|') ],
   parse_value(?t),  % this stays a value, probably a variable for tail
   [ pm(']') ],
   !.
parse_list(?m, [?a | ?as]) -->
   delimiter,
%   parse_value(?m, ?a),
   parse_expression(?m, ?a),
   !,
   parse_list(?m, ?as).
   
% Take certain built-in words and convert them to a simple
% structure so those who call parse_expression will see them
% as structures and call eval, which will then get the right
% value for them.
parse_special(_, special(now)) -->
   keyword(now),
   !.
parse_special(_, special(today)) -->
   keyword(today),
   !.
parse_special(_, special(call_stack)) -->
   keyword(call_stack),
   !.

rexp(?EL, ?E ) :-
   rreduce(?EL, opx(x,0,x), 0, [], ?E),
   !.
%exp(EL, E) :-
%   write('******** Bad Expression ****************'), nl,
%   write(EL), nl, nl,
%   fail.

% reduce(ExpressionList, LastOperator, MaxP, EvalStack, Expression)
% LastOperator is the last operator pushed on the 
% EvalStack.  But prefix ops get pushed on anyway, so MaxP
% keeps track of the maximum precedence pushed.

%reduce(EL, LastOp, MaxP, Out, E) :-
%   write('Input' = EL), nl,
%   write('LastOp' = LastOp), nl,
%   write('MaxP' = MaxP), nl,
%   write('Output' = Out), nl, nl,
%   fail.
rreduce([], _, _, ?EvalSt, ?E) :-
   rcompress_stack(?EvalSt, 1201, [?E]),
   !.
rreduce([?V|?Z], ?LastOp, ?MaxP, ?EvalSt, ?E) :-
   var(?V),
   !,
   rreduce(?Z, ?LastOp, ?MaxP, [?V|?EvalSt], ?E).
rreduce([opx(?Op,?P,?A)|_], opx(?Op,?P,?A), _, _, _) :-
   (?A == xfx; ?A == xf; ?A == fx),
   !,
   fail.
rreduce([opx(?Op1,?P1,?A1)|?Z], opx(?Op2,?P2,?A2), ?MaxP, ?EvalSt, ?E) :-
   ( rpop_first(opx(?Op1,?P1,?A1), opx(?Op2,?P2,?A2), ?MaxP) ->
       rcompress_stack(?EvalSt, ?P1, ?EvalSt2),
       ?MaxP2 = ?P1
       ;
       ?EvalSt2 = ?EvalSt,
      ( ?P1 > ?MaxP -> ?MaxP2 = ?P1; ?MaxP2 = ?MaxP) ),
   !,
   rreduce(?Z, opx(?Op1,?P1,?A1), ?MaxP2, [opx(?Op1,?P1,?A1)|?EvalSt2], ?E).
rreduce([paren(?EL)|?Z], ?LastOp, ?MaxP, ?EvalSt, ?E) :-
   !,
   rreduce([?EL|?Z], ?LastOp, ?MaxP, ?EvalSt, ?E).  % EL already reduced
rreduce([?A|?Z], ?LastOp, ?MaxP, ?EvalSt, ?E) :-
   !,
   rreduce(?Z, ?LastOp, ?MaxP, [?A|?EvalSt], ?E).

% pop the stack first if the new operator
% is a higher precedence infix, or if its
% a higher precedence postfix, or if its
% an equal precedence yfx.  prefix ops
% just put on no matter what precedence.

rpop_first(opx(_,?P1,?A1), opx(_,?P2,?A2), ?MaxP) :-
   ris_infix(?A1),
   ?P1 > ?P2,
   !.
rpop_first(opx(_,?P1,?A1), opx(_,?P2,?A2), ?MaxP) :-
   ris_infix(?A1),
   ?P1 < ?P2,
   !, fail.
rpop_first(opx(_,?P1,yfx), _, _) :-
   !.
rpop_first(opx(_,?P1,?A1), _, ?MaxP) :-
   ris_postfix(?A1),
   ?P1 > ?MaxP,
   !.

% compress the stack down to MaxP, that is, we've had an operator
% that will swallow up everything on the stack up until an
% equal or higher operator.
rcompress_stack([], _, []) :-  % might happen the first time around
   !.
rcompress_stack([?A], _, [?A]) :-  % normal end condition
   !.
rcompress_stack([?X,opx(?Op,?P,?A),?Y|?Z], ?MaxP, ?SS) :-
   nonvar(?P),   % op$ structure not unified with a var
   ?P =< ?MaxP,
   roparg(?X),
   roparg(?Y),
   ris_infix(?A),
   ?E =.. [?Op,?Y,?X],
   !,
   rcompress_stack([?E|?Z], ?MaxP, ?SS).
rcompress_stack([?X,opx(?Op,?P,?A)|?Z], ?MaxP, ?SS) :-
   nonvar(?P),   % op$ structure not unified with a var
   ?P < ?MaxP,
   roparg(?X),
   ris_prefix(?A),
   ?E =.. [?Op,?X],
   !,
   rcompress_stack([?E|?Z], ?MaxP, ?SS).
rcompress_stack([?OP|?Z], ?MaxP, ?SS) :-
   ris_op(?OP),
   rextract_postfixes([?OP|?Z], ?MaxP, ?E, ?Z1),
   !,
   rcompress_stack([?E|?Z1], ?MaxP, ?SS).
rcompress_stack(?S, _, ?S).

rextract_postfixes([?X|?Z], _, ?X, ?Z) :-
   roparg(?X),
   !.
rextract_postfixes([opx(?Op,?P,?A)|?Z], ?MaxP, ?E, ?Z2) :-
   ?P >= ?MaxP,
   !,
   fail.
rextract_postfixes([opx(?Op,?P,?A)|?Z], ?MaxP, ?E, ?Z2) :-
   ris_postfix(?A),
   ?E =.. [?Op,?E2],
   !,
   rextract_postfixes(?Z, ?MaxP, ?E2, ?Z2).

%is_op(X) :- nonvar(X), X = op$(_,_,_).
ris_op(?OP) :- functor(?OP, opx, 3).

roparg(?A) :- var(?A), !.
roparg(opx(_,_,_)) :- !, fail.
roparg(_).

ris_infix(xfx).
ris_infix(xfy).
ris_infix(yfx).

ris_postfix(xf).
ris_postfix(yf).

ris_prefix(fy).
ris_prefix(fx).

% Operators

% Exclude the comparison operators.
rinfix(_, _, _) -->
   compare_operator( _ ),
   !, fail.
rinfix(?Op,?P,?A) -->
   [?Op],
   { current_op(?P,?A,?Op),
     ris_infix(?A) }.

rprefix(?Op,?P,?A) --> [?Op], { current_op(?P,?A,?Op), ris_prefix(?A) }.

rpostfix(?Op,?P,?A) --> [?Op], { current_op(?P,?A,?Op), ris_postfix(?A) }.

rpeek_exp_end(?DCG, ?DCG) :-
   rexp_end(?DCG, _),
   !.

rexp_end --> keyword(WHEN).
rexp_end --> keyword(AND).
rexp_end --> keyword(OR).
rexp_end --> [pm(')')].
%rexp_end --> [pm(',')].
rexp_end --> delimiter.

:- end_module(parser).