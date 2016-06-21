%--------------------------------------------------------
% ARules Reasoning Engine
%
% Rules are always of the form:
%   rule(?attr, ?id, ?rule) where
%     ?rule:  ?attr = ?val WHEN ?conditions
%
% booleans have ?val of true/false, facts have conditions = true.
%

:- import(list).
:- import(date_time).
:- import(aodbc).
:- include('opdefs.pro').
%:- loadlsx('amysql.lsx').

% A query might have a list of things to find
findgs( [] ).
findgs( [?m : ?o = ?v | ?movs ] ) :-
   find(?m, ?o, ?v),
   !,
   findgs( ?movs ).

% If a find/3 has variables in it's ?o then we need to allow
% for backtracking; otherwise we don't.  findx and finda provide
% these two flavors of find.

find(?m, ?o, ?v) :-
	(in_flux(?m) -> throw( execution_error(in_flux(?m)) ) ; true),
   ground(?o),
   !,
%   ( trace_flag -> do_trace(find(?m, ?o, ?v)) ; true ),
   obj_parts(?o, ?obj, ?props),
   (findx(?m, ?obj, ?props, ?x) ->
%   (probe( x: ?m: ?obj, findx(?m, ?obj, ?props, ?x) ) ->
      ( (?x = ?v) ->
         true
         ;
         fail )
      ;
%      throw( execution_error(no_value(?m, ?o)) ) ),
      (findall_stack -> fail ; throw( execution_error(no_value(?m, ?o))) ) ),
   
%   ( trace_flag -> do_trace(found(?m, ?o, ?v)) ; true ),
   !.
   
find(?m, ?o, ?v) :-
%   ( trace_flag -> do_trace(find(?m, ?o, ?v)) ; true ),
   obj_parts(?o, ?obj, ?props),
   finda(?m, ?obj, ?props, ?x),
%   probe( a: ?m: ?obj, finda(?m, ?obj, ?props, ?x) ),
   ?x = ?v.
%   ( trace_flag -> do_trace(found(?m, ?o, ?v)) ; true ).
      
%xx   (get_parm(explain, on) -> trace_output(found(?m, ?o, ?v)) ; true).
   
% It's an error if we couldn't find any value for a non-ground object.
/* we don't need this anymore, do we????
find(?m, ?o, ?v) :-
   obj_parts(?o, ?obj, ?props),
   not was_knowable(?m, ?obj, ?props),
%   not probe( w: ?m: ?obj, was_knowable(?m, ?obj, ?props) ),
   throw( execution_error(no_value(?m, ?o)) ).
*/

% After a find fails, if the ?m:?o was knowable, it's value
% would be known.  Call this before deciding to throw a 
% can't-find-value error. Also, hack hack, if a flag was set
% from findall, ignore the error as well.

was_knowable(?m, ?obj, ?props) :-
   known(?m, ?obj, ?props, _, _),
   !.
was_knowable(?m, ?obj, ?props) :-
   data(?m, ?obj, ?props, _, _),
   !.
was_knowable(?m, ?obj, ?props) :-
   call(?m:super(?ms)),
   was_super_knowable(?ms, ?obj, ?props),
   !.

was_super_knowable([?ms| _], ?obj, ?props) :-
   was_knowable(?ms, ?obj, ?props).
was_super_knowable([_| ?mss], ?obj, ?props) :-
   was_super_knowable(?mss, ?obj, ?props).

exists(?m, ?o) :-
   get_known(?m, ?o, _, _),
   !.
exists(?m, ?o) :-
   get_data(?m, ?o, _, _),
   !.
exists(?m, ?o) :-
   current_module(?m),
   obj_parts(?o, ?obj, ?props),
   call(?m: know(?obj, ?props, _, _, _)),
   !.
exists(?m, ?o) :-
   current_module(?m),
   call(?m:super(?ms)),
   super_exists(?ms, ?o),
   !.

super_exists([?ms| _], ?o) :-
   exists(?ms, ?o).
super_exists([_| ?mss], ?o) :-
   super_exists(?mss, ?o).

% pick off a vector from a multidimensional array.
% was specified with a * as a parameter.
vector(?i, om(?m, ?o), ?v) :-
    bagof(?x, ?i ^ find(?m, ?o, ?x), ?v).

% for debugging, replace the find above with findb:
/*
findb(?m, ?o, ?x) :-
   ?o = obj([schedule(_,_) | _]),
   !,
%  (write(call:findb), nl; write(fail:findb), nl, fail),
 (showx(call: ?m: ?o); showx(fail: ?m: ?o), fail),
   find(?m, ?o, ?x),
 (showx(exit: ?m: ?o: ?x); showx(redo: ?m: ?o), fail).
%  (write(exit:findb), nl; write(redo:findb), nl, fail).

% to tempoarily undo findb:
findb(?m, ?o, ?x) :- find(?m, ?o, ?x).
*/

% This is used primarily by RANGE() which looks for just
% data so it can get the cell addresses.

find_data(?m, ?o, ?x, ?r) :-
   get_data(?m, ?o, ?x, ?r).
find_data(?m, ?o, ?x, ?r) :-
   call(?m:super(?mss)),
   find_super_data(?mss, ?o, ?x, ?r).

% find in the super class
find_super_data([?ms|?mss], ?o, ?x, ?r) :-
   find_data(?ms, ?o, ?x, ?r).
find_super_data([_|?mss], ?o, ?x, ?r) :-
   find_super_data(?mss, ?o, ?x, ?r).

%---------------------------------------------------------------
% findx - non backtracking version when object is ground. The
% find which called it cuts when it succeeds, so internal cuts
% aren't needed.
%

% It's in the data, and if it's a data structure, then don't
% try to find it some other way.
findx(?m, ?obj, ?props, ?x) :-
% probe(findx(data),   data(?m, ?obj, ?props, ?x, _) ).
   data(?m, ?obj, ?props, ?x, _),
   (trace_flag -> do_trace( data(?m, ?obj, ?props, ?x) ) ; true).

% It's already known, which also means if we were doing
% a finda, it would have backtracked already through all
% the rules for this case, so cut so we don't do the rules
% again.
findx(?m, ?obj, ?props, ?x) :-
% probe(findx(known),   known(?m, ?obj, ?props, ?x, _) ).
   known(?m, ?obj, ?props, ?x, _),
   (trace_flag -> do_trace( known(?m, ?obj, ?props, ?x) ) ; true).

% Look for the most usual
% case of rules with just ?o = ?v either as the only
% or first assignment of the rule.  Don't reassign
% object values if they're already known.
findx(?m, ?obj, ?props, ?x) :-
   ( trace_flag ->   % if tracing, use findxt with hooks in it
      findxt(?m, ?obj, ?props, ?x)
      ;
      call(?m : know(?obj, ?props, ?x, ?r, ?ovs)),
      ( trace_flag -> do_trace(try(?m, ?o, ?v)) ; true ),
   
      stack_call(?m : rule(?r, ?ovs)),
      
      not known_ovs(?m, ?ovs),
      assert_ovs(?m, ?r, ?ovs) ).

% probe(findx(rule), (
%  )).

% If we didn't find a value above, maybe it's because
% it is a boolean, and there isn't a rule for the false
% case (or true if the rule specified false). If so,
% then assert the negation of the boolean as known.
% Also, if the query was asking for a true/false specifically,
% then it's also a boolean, and maybe intended for input, but
% in any case, we don't put out a "can't find value for" error
% for booleans.
findx(?m, ?obj, ?props, ?x) :-
% probe(findx(boolean), (
   is_boolean(?m, ?obj, ?props, ?tf),
   not known(?m, ?obj, ?props, _, _),
   ( ?tf = true -> ?x = false ; ?x = true ),
   assert_known( ?m, ?obj, ?props, ?x, failure ).
%  )).

findx(?m, ?obj, ?props, ?x) :-
% probe(findx(super), (
   call(?m:super(?mss)),
   findsx(?mss, ?obj, ?props, ?x).
% )).


/*
thats_what_were_talking_about(compute_status(DiptheriaTetanus)).
thats_what_were_talking_about(current_status(DiptheriaTetanus)).
thats_what_were_talking_about('FuturePlan!B73').
*/

% find in the super class
findsx([?ms|?mss], ?obj, ?props, ?x) :-
   findx(?ms, ?obj, ?props, ?x).
findsx([_|?mss], ?obj, ?props, ?x) :-
   findsx(?mss, ?obj, ?props, ?x).

% same as findx for rules, but with hooks for tracing
findxt(?m, ?obj, ?props, ?x) :-
   (do_trace( find(?m, ?obj, ?props, ?x) ); fail),
%   (do_trace( find(?m, ?obj, ?props, ?x) ); undo_trace( find(?m, ?obj, ?props, ?x) ), fail),
%   do_trace( find(?m, ?obj, ?props, ?x) ),
   call(?m : know(?obj, ?props, ?x, ?r, ?ovs)),
   ( do_trace(call(rule(?r))) ; do_trace(fail(rule(?r))), fail ),
   stack_call(?m : rule(?r, ?ovs)),
   ( do_trace(exit(rule(?r))) ; do_trace(redo(rule(?r))), fail ),
   not known_ovs(?m, ?ovs),
   assert_ovs(?m, ?r, ?ovs),
   do_trace( found(?m, ?obj, ?props, ?x, rule(?r)) ).

%---------------------------------------------------------------
% finda - backtracking version
%

finda(?m, ?obj, ?props, ?x) :-
% probe( finda(data, ?m, ?obj),   data(?m, ?obj, ?props, ?x, _) ).
   data(?m, ?obj, ?props, ?x, _),
   ( (trace_flag -> do_trace( data(?m, ?obj, ?props, ?x) ) ; true)
     ;
     (trace_flag -> clear_trace( data(?m, ?obj, ?props, ?x) ) ; true),
     fail ).

finda(?m, ?obj, ?props, ?x) :-
% probe( finda(known, ?m, ?obj),   known(?m, ?obj, ?props, ?x, _) ).
   known(?m, ?obj, ?props, ?x, _),
   ( (trace_flag -> do_trace( known(?m, ?obj, ?props, ?x) ) ; true)
     ;
     (trace_flag -> clear_trace( known(?m, ?obj, ?props, ?x) ) ; true),
     fail ).

finda(?m, ?obj, ?props, ?x) :-
   ( trace_flag ->   % if tracing, use findat with hooks in it
      findat(?m, ?obj, ?props, ?x)
      ;
% probe(finda(rule, ?m, ?obj), (
      call(?m : know(?obj, ?props, ?x, ?r, ?ovs)),
      stack_call(?m : rule(?r, ?ovs)),
      not known_ovs(?m, ?ovs),   % needed to prevent a rule from duplicating a known.
      assert_ovs(?m, ?r, ?ovs) ).
% )).

% If we didn't find a value above, maybe it's because
% it is a boolean, and there isn't a rule for the false
% case (or true if the rule specified false). If so,
% then assert the negation of the boolean as known.
% Also, if the query was asking for a true/false specifically,
% then it's also a boolean, and maybe intended for input, but
% in any case, we don't put out a "can't find value for" error
% for booleans.
finda(?m, ?obj, ?props, ?x) :-
% probe(finda(boolean), (
   is_boolean(?m, ?obj, ?props, ?tf),
   not known(?m, ?obj, ?props, _, _),
   ( ?tf = true -> ?x = false ; ?x = true ),
   assert_known( ?m, ?obj, ?props, ?x, failure ).
% )).

% see if a super class has a value for ?o as long as
% we haven't found it already, or if it has variables
% implying multiple answers are possible.
finda(?m, ?obj, ?props, ?x) :-
% probe(finda(super), (
   (not known(?m, ?obj, ?props, ?x, _) ; not ground(o(?m, ?obj, ?props)) ),
   !,
   call(?m:super(?mss)),
   findsa(?mss, ?obj, ?props, ?x).
% )).

% find in the super class
findsa([?ms|?mss], ?obj, ?props, ?v) :-
   finda(?ms, ?obj, ?props, ?v).
findsa([_|?mss], ?obj, ?props, ?v) :-
   findsa(?mss, ?obj, ?props, ?v).

% same as finda for rules, but with hooks for tracing
findat(?m, ?obj, ?props, ?x) :-
   (do_trace( find(?m, ?obj, ?props, ?x) ); fail),
%   (do_trace( find(?m, ?obj, ?props, ?x) ); undo_trace( find(?m, ?obj, ?props, ?x) ), fail),
%   do_trace( find(?m, ?obj, ?props, ?x) ),
   call(?m : know(?obj, ?props, ?x, ?r, ?ovs)),
   ( do_trace(call(rule(?r))) ; do_trace(fail(rule(?r))), fail ),
   stack_call(?m : rule(?r, ?ovs)),
   ( do_trace(exit(rule(?r))) ; do_trace(redo(rule(?r))), fail ),
   not known_ovs(?m, ?ovs),
   assert_ovs(?m, ?r, ?ovs),
   do_trace( found(?m, ?obj, ?props, ?x, rule(?r)) ).

%----------------------------------------------------------------

% Needed by findx to determine if a default false or 
% true needs to be 'known' when findx failed to findx.
is_boolean(?m, ?obj, ?props, ?tf) :-
%   obj_parts(?o, ?obj, ?props),
   call( ?m : know(?obj, ?props, ?tf, _, _) ),
   ( ?tf == true ; ?tf == false ).

get_ov(?o = ?v, [?o = ?v | _]) :-
   !.
get_ov(?o = ?v, [_ | ?ovs]) :-
   !, get_ov( ?o = ?v, ?ovs ).

% We want to preserve the idea that there are unique values
% for properties, and not backtrack assigning new values,
% so this checks against that.  succeed if any known,
% fail otherwise.
known_ovs(?m, []) :- !, fail.
known_ovs(?m, [?o = ?v | ?ovs]) :-
   get_known(?m, ?o, _, _),
   !.
known_ovs(?m, [_ | ?ovs]) :-
   known_ovs(?m, ?ovs).

   
% When there were multiple assignments, assert the ones
% after the head.
assert_ovs(_, _, []) :- !.
assert_ovs(?m, ?c, [?o = ?v | ?ovs]) :-
   assert_known( ?m, ?o, ?v, rule(?c) ),
   !,
   assert_ovs(?m, ?c, ?ovs).

stack_call(?m : rule(?r, ?ovs)) :-
   ( push_stack(?r) ; pop_stack(?r), fail ),
   call( ?m : rule(?r, ?ovs) ),
   ( pop_stack(?r) ; push_stack(?r), fail ).

dumpstack :- get_call_stack(?s), showx(?s).

push_stack(?r) :- asserta( stack(?r) ).
pop_stack(?r) :- retract( stack(?r) ), !.

call_stack_clear :- abolish( stack/1 ).

get_call_stack(?stack) :-
   findall( ?r, stack(?r), ?stack ).
   
get_call_stack(?n, ?stack) :-
   cntr_set(2, ?n),
   findall( ?r, (stack(?r), cntr_dec(2, ?i), ?i > 0), ?stack ).

% need to have a special call for this so that we can apply inheritance
call_logic(?m: ?logic) :-
   functor(?logic, ?f, ?a),
   get_logic_module(?m, ?f / ?a, ?mod),
   !,
   ( trace_flag -> trcall( ?mod : ?logic) ; call(?mod : ?logic) ).

trcall( ?mod : ?logic ) :-
   ( do_trace( call(logic(?mod : ?logic)) ); do_trace( fail(logic(?mod : ?logic)) ), fail),
   call( ?mod : ?logic ),
   ( do_trace( exit(logic(?mod : ?logic)) ); do_trace( redo(logic(?mod : ?logic)) ), fail).
   
% If it's in the current module, use it.
% If it's in a superset module, use that.
% If you can't find it, just call it because it's
% probably user or a system module.
get_logic_module(?m, ?fa, ?m) :-
   current_predicate(?m : ?fa),
   !.
get_logic_module(?m, ?fa, ?mod) :-
   call(?m:super(?mss)),
   member(?ms, ?mss),
   get_logic_module(?ms, ?fa, ?mod).
get_logic_module(?m, _, ?m).

to_string( obj([?o]), ?s) :-
   stringlist_concat([`.`,?o], ?s),
   !.
to_string( obj([?o|?ps]), ?s) :-
   dot_props(?ps, ?dps),
   stringlist_concat([?o|?dps], ?s),
   !.
to_string( ?t, ?s ) :-
   string_term(?s, ?t).

dot_props([], []).
dot_props([?p|?ps], [`.`, ?p|?dps]) :-
   dot_props(?ps, ?dps).

% a rule might have an evallist when it sets multiple properties
evallist( [] ).
evallist( [?e = ?x | ?exs ] ) :-
   eval(?e, ?x),
%   !,
   evallist( ?exs ).
   
eval(?x, ?x) :- var(?x), !.
eval([], []) :- !.
eval([?x| ?xs], [?v| ?vs]) :-
   eval(?x, ?v),
%   !,
   eval(?xs, ?vs).
eval(true, true) :- !.
eval(false, false) :- !.
eval(find(?m, ?o, ?v), ?v) :-
   !,
% write(evalfind(?m, ?o, ?v)), nl,
   find(?m, ?o, ?v).
%eval(finda(?m, ?o, ?v), ?v) :-
%   !,
%   finda(?m, ?o, ?v).
eval(findlist(?finds), ?v) :-
   !,
   findlist(?finds, ?v).
eval(vector(?i, ?o, ?v), ?v) :-
   !,
   vector(?i, ?o, ?v).
eval(explist(?x), ?v) :-
   !,
   explist(?x, ?v).
eval(special(now), ?x) :-
   datetime_get(now, ?x),
   !.
eval(special(today), ?x) :-
   datetime_get(today, ?x),
   !.
eval(special(call_stack), ?cs) :-
   get_call_stack(?x),
   string_termq(?cs, ?x),
   !.
eval(?f, ?v) :-
   is_function(?f),
   !,
   function(?f, ?v).
/*
eval(om(?m,?o), ?v) :-
   !,
   find(?m, ?o, ?v).
*/
eval(?a & ?b, ?v) :-
   !,
   ands_list(?a & ?b, ?sl),
   stringlist_concat(?sl, ?v).
eval(?x, ?v) :-
   ?x =.. [?op, ?a, ?b],
   !,
   eval(?a, ?av),
   eval(?b, ?bv),
   doop(?op, ?av, ?bv, ?v).
eval(?x, ?v) :-
   ?x =.. [?op, ?a],
   !,
   eval(?a, ?av),
   doop(?op, ?av, ?v).
eval(?x, ?x).

% an expression might have included a list, which can
% have objects and other things in it, so this evaluates
% each to see what we really have.
explist([], []) :- !.
explist([?e| ?es], [?v| ?vs]) :-
   eval(?e, ?v),
   !,
   explist(?es, ?vs).

% an object might have had another object as an argument,
% leading to a linked list of required finds.  The last one
% is the one with the answer.

findlist(?finds, ?v) :- findlist(?finds, false, ?v).

findlist([], ?v, ?v).
findlist([find(?m, ?o, ?v)|?finds], _, ?vv) :-
   find(?m, ?o, ?v),
   findlist(?finds, ?v, ?vv).
   
% Needed to quickly determine if we have a function, so the
% function can backtrack, but not to other evals.

is_function(AFTER(?list, ?value)).
is_function(AGE_TO_LISTSTRING(?a)).
is_function(APPEND(?list1, ?list2)).
is_function(ASK(?prompt)).
is_function(ASK(?prompt, ?title, ?default)).
is_function(BEFORE(?list, ?value)).
is_function(CALL_STACK).
is_function(CONCATENATE(?x)).
is_function(COUNT(?x)).
is_function(DAY(?x)).
is_function(DAYS_BETWEEN(?x, ?y)).
is_function(DEBUGPRINT(?x)).
is_function(DIFFERENCE(?x,?y)).
is_function(DYNAMIC_FIND(?x)).
is_function(EVALUATE(?x)).
is_function(EXISTS(?x)).
is_function(EXTRACT_DATE(?x)).
is_function(EXTRACT_TIME(?x)).
%is_function(FAILS(?x)).
is_function(FINDALL(?item, ?test)).
is_function(FIRST(?z)).
is_function(FORMAT_DATE(?date, ?fmt)).
is_function(INDEX(?table, ?i)).
is_function(INTERSECTION(?x,?y)).
is_function(IS_KNOWN(om(?m,?o))).
is_function(IS_KNOWN(?x)).
is_function(IS_RULESET(?x)).
is_function(IS_SUBSET(?x, ?y)).
is_function(ITEM_AT(?x, ?y)).
is_function(LAST(?z)).
is_function(LIST_TO_STRING(?l, ?sep)).
is_function(MAKE_DATE(?y, ?m, ?d)).
is_function(MAKE_DATETIME(?yr, ?mo, ?da, ?hr, ?mi, ?sc)).
is_function(MAXIMUM(?x1, ?x2)).
is_function(MAXIMUM(?x1)).
is_function(MSGBOX(?m)).
is_function(MEMBER(?x)).
is_function(MINIMUM(?x1, ?x2)).
is_function(MINIMUM(?x1)).
is_function(MONTH(?x)).
is_function(NOW).
is_function(NEXT(?x, ?z)).
is_function(ONETIME(?g)).
is_function(PERMUTE(?x)).
is_function(PRIOR(?y, ?z)).
is_function(QUIT(?m)).
is_function(RANGE(?x)).
is_function(REMOVE_DUPLICATES(?x)).
is_function(REVERSE(?x)).
is_function(SET(?x)).
is_function(SORT(?x)).
is_function(SQL(?x,?y)).
is_function(SUM(?l)).
is_function(MINUTES_BETWEEN(?x,?y)).
is_function(TIMEIT(?f)).
is_function(TODAY).
is_function(UNION(?x,?y)).
%is_function(VECTOR(?i, ?o)).  % called LIST in user program
is_function(WEEKDAY(?x)).
is_function(YEAR(?x)).

function(AFTER(?x, ?a), ?v) :-
   !,
   eval(?x, ?xs),
   ( is_list(?xs) -> true ; throw( execution_error(bad_list_arg(AFTER, ?xs)))),
   eval(?a, ?as),
   after(?xs, ?as, ?v).
function(APPEND(?x, ?y), ?v) :-
   eval(?x, ?xl),
   ( is_list(?xl) -> true ; throw( execution_error(bad_list_arg(APPEND, ?xl)))),
   eval(?y, ?yl),
   ( is_list(?yl) -> true ; throw( execution_error(bad_list_arg(APPEND, ?yl)))),
   !,
   append(?xl, ?yl, ?v).
function(ASK(?p), ?v) :-
   eval(?p, ?pp),
   (trace_flag ->
      throw( execution_error(trace_nocanuse(ASK)) )
      ;
      ask( ?pp, ?v ) ),
   !.
function(ASK(?p, ?t, ?d), ?v) :-
   eval(?p, ?pp),
   eval(?t, ?tt),
   eval(?d, ?dd),
   (trace_flag ->
      throw( execution_error(trace_nocanuse(ASK)) )
      ;
      ask( ?pp, ?tt, ?dd, ?v ) ),
   !.
function(BEFORE(?x, ?a), ?v) :-
   !,
   eval(?x, ?xs),
   ( is_list(?xs) -> true ; throw( execution_error(bad_list_arg(BEFORE, ?xs)))),
   eval(?a, ?as),
   before(?xs, ?as, ?v).
% concatenates a list of lists to a list
function(CALL_STACK, ?cs) :-
   !,
   get_call_stack(?x),
   string_termq(?cs, ?x),
   !.
function(CONCATENATE(?x), ?v) :-
   !,
   eval(?x, ?y),
   ( is_list(?y) -> true ; throw( execution_error(bad_list_arg(CONCATENATE, ?y)))),
   reverse(?y, ?yr),  % so func_concatenate goes faster
   func_concatenate(?yr, [?v]),
   !.
function(COUNT(?x), ?v) :-
   !,
   eval(?x, ?y),
   ( is_list(?y) -> true ; throw( execution_error(bad_list_arg(COUNT, ?y)))),
   length(?y, ?v).
function(DAY(?x), ?v) :-
   eval(?x, ?d),
   ( is_datetime(?d) -> true ; throw( execution_error(bad_date_function_arg(DAY, ?d)) ) ),
   !,
   datetime_extract(?d, days(?v)).
function(DAYS_BETWEEN(?x, ?y), ?v) :-
   eval(?x, ?dx),
   ( is_datetime(?dx) -> true ; throw( execution_error(bad_date_function_arg(DAYS_BETWEEN, ?dx)) ) ),
   eval(?y, ?dy),
   ( is_datetime(?dy) -> true ; throw( execution_error(bad_date_function_arg(DAYS_BETWEEN, ?dy)) ) ),
   !,
   to_date(?dx, ?xd),
   to_date(?dy, ?yd),
   date_1900_days(?xd, ?xdays),
   date_1900_days(?yd, ?ydays),
   ?v is ?ydays - ?xdays.
function(DEBUGPRINT(?m), true) :-
   !,
   eval(?m, ?mm),
   (trace_flag ->
      true
      ;
      debugprint(?mm) ).
function(DIFFERENCE(?x, ?y), ?v) :-
   eval(?x, ?xl),
   ( is_list(?xl) -> true ; throw( execution_error(bad_list_arg(DIFFERENCE, ?xl)))),
   eval(?y, ?yl),
   ( is_list(?yl) -> true ; throw( execution_error(bad_list_arg(DIFFERENCE, ?yl)))),
   !,
   findall( ?i, (member(?i, ?xl), not member(?i, ?yl)), ?xy),
   to_set(?xy, ?v).
function(EVALUATE(?x), ?v) :-
   eval(?x, ?sin),
   (trace_flag ->
      throw( execution_error(trace_nocanuse(EVALUATE)) )
      ;
      xl_evaluate(?sin, ?sout),
      p_get_just_value(?sout, ?v) ),
   !.
function(EXISTS(find(?m, ?o, _)), ?v) :-
   ( exists(?m, ?o) -> ?x = true ; ?x = false ),
   !,
   ?x = ?v.
function(EXISTS(?x), _) :-
   throw( execution_error(bad_object_arg(EXISTS, ?x)) ),
   !.
function(EXTRACT_DATE(?x), ?y) :-
   eval(?x, ?dt),
   ( is_datetime(?dt) -> true ; throw( execution_error(bad_date_function_arg(EXTRACT_DATE, ?x)) ) ),   
   ?dt = datetime(?h, ?l, ?d, _, _, _),
   !,
   ?y = datetime(?h, ?l, ?d, 0, 0, 0).
function(EXTRACT_TIME(?x), ?y) :-
   eval(?x, ?dt),
   ( is_datetime(?dt) -> true ; throw( execution_error(bad_date_function_arg(EXTRACT_TIME, ?x)) ) ),   
   ?dt = datetime(_, _, _, ?h, ?m, ?s),
   !,
   ?y = datetime(0, 0, 0, ?h, ?m, ?s).

% catch doesn't allow backtracking after a failure, so we need the
% kludge of a findall stack to let findx know it shouldn't throw a
% a no value error when we're in a findall.
function(FINDALL(?item, ?test), ?list) :-
   !,
   findall_stack_in,
   ( trace_flag -> do_trace( findall ) ; true ),
%   ( set_parm(findall, true); set_parm(findall, false), fail),
% ?t1 is cputime,
   findall( (?item),
%      catch( (?test), execution_error(no_value(_,_)), fail ),
      (?test),
      ?list ),
% ?t2 is cputime - ?t1,
% debugprint( ?t2 - findall(?item, ?test) ),
   ( trace_flag -> do_trace( foundall(?list) ) ; true ),
   findall_stack_out.
   
function(DYNAMIC_FIND(?query), ?list) :-
   !,
   eval(?query, ?q),
   (string(?q) -> ?qstr = ?q; string_atom(?qstr, ?q)),
   t_tokenize(?qstr, ?toks, ?vars),
%   ?toks = [ v(?item), when | ?goaltoks ],
   append( ?argtoks, [when | ?goaltoks], ?toks ),
   current_module(?m),
   parser2:function_arg(?m, ?item, ?argtoks, []),
   p_parse_goal(?m, ?test, ?goaltoks, []),
   findall_stack_in,
   ( trace_flag -> do_trace( findall ) ; true ),
   findall( (?item),
      (?test),
      ?list ),
% ?t2 is cputime - ?t1,
% debugprint( ?t2 - findall(?item, ?test) ),
   ( trace_flag -> do_trace( foundall(?list) ) ; true ),
   findall_stack_out.
function(FIRST(?z), ?a) :-
   !,
   eval(?z, ?list),
   ( is_list(?list) -> true ; throw( execution_error(bad_list_arg(FIRST, ?list)))),
   [?a|_] = ?list.
   
%function(FORMAT_DATE(datetime(?y, ?m, ?d, _,_,_), ?fmt), ?datestr) :-
function(FORMAT_DATE(?x, ?fmt), ?datestr) :-
   eval(?x, ?date),
   ( is_datetime(?date) -> true ; throw( execution_error(bad_date_function_arg(FORMAT_DATE, ?date)) ) ),
   ?date = datetime(?y, ?m, ?d, _, _, _), 
   date_string( date(?y, ?m, ?d), ?fmt, ?datestr ).

function(AGE_TO_LISTSTRING(?x), ?agestr) :-
   eval(?x, ?era),
   ( is_era(?era) -> true ; throw( execution_error(bad_age_function_arg(AGE_TO_LISTSTRING, ?era)) ) ),
   ?era = era(?y, ?m, ?d, _, _, _),
   string_term(?agestr, [?y years, ?m months, ?d days]).

function(INDEX(find(?m, obj([?table]), _), ?pos ), ?v) :-
% obj([loans])
% data(?m, loans(1, FICO), _, _, _).
   !,
   get_indexes(?m, ?table, ?pos, ?indexes),
   member(?v, ?indexes).
   
function(INTERSECTION(?x, ?y), ?v) :-
   eval(?x, ?xl),
   ( is_list(?xl) -> true ; throw( execution_error(bad_list_arg(INTERSECTION, ?xl)))),
   eval(?y, ?yl),
   ( is_list(?yl) -> true ; throw( execution_error(bad_list_arg(INTERSECTION, ?yl)))),
   !,
   findall( ?i, (member(?i, ?xl), member(?i, ?yl)), ?xy),
   to_set(?xy, ?v).
function(IS_KNOWN(find(?m, ?o, _)), ?v) :-
   ( get_known(?m, ?o, _, _) -> ?x = true ; ?x = false ),
   !,
   ?x = ?v.
function(IS_KNOWN(?x), _) :-
   throw( execution_error(bad_object_arg(IS_KNOWN, ?x)) ),
   !.
function(IS_RULESET(?x), ?v) :-
   !,
   ( string(?x) -> atom_string(?a, ?x) ; ?a = ?x ),
   ( current_module(?a) -> ?v = true ; ?v = false ).
function(IS_SUBSET(?x, ?y), ?v) :-
   !,
   eval(?x, ?xl),
   ( is_list(?xl) -> true ; throw( execution_error(bad_list_arg(IS_SUBSET, ?xl)))),
   eval(?y, ?yl),
   ( is_list(?yl) -> true ; throw( execution_error(bad_list_arg(IS_SUBSET, ?yl)))),
   !,
   ( is_subset(?xl, ?yl) -> ?v = true ; ?v = false ).
function(ITEM_AT(?xl, ?xi), ?v) :-
   !,
   eval(?xi, ?i),
   eval(?xl, ?l),
   ( is_list(?l) -> true ; throw( execution_error(bad_list_arg(ITEM_AT, ?l)))),
   nth_elem(?l, ?v, ?i).
function(LIST_TO_STRING(?xl, ?xsep), ?str) :-
   eval(?xl, ?l),
   eval(?xsep, ?sep),
   ( is_list(?l) -> true ; throw( execution_error(bad_list_arg(LIST_TO_STRING, ?l)))),
   stringlist_concat(?l, ?sep, ?str),
   !.
function(MAKE_DATE(?x, ?y, ?z), datetime(?year, ?month, ?day, 0, 0, 0)) :-
   eval(?x, ?year),
   eval(?y, ?month),
   eval(?z, ?day),
   !.
function(MAKE_DATETIME(?x, ?y, ?z, ?a, ?b, ?c), datetime(?year, ?month, ?day, ?hour, ?min, ?sec)) :-
   eval(?x, ?year),
   eval(?y, ?month),
   eval(?z, ?day),
   eval(?a, ?hour),
   eval(?b, ?min),
   eval(?c, ?sec),
   !.
function(MAXIMUM(?x1, ?x2), ?v) :-
   eval(?x1, ?v1),
   eval(?x2, ?v2),
   func_maximum(?v1, ?v2, ?v),
   !.
function(MAXIMUM(?x), ?v) :-
%   ( is_list(?x) -> ?list = ?x ; eval(?x, ?list) ),
   eval(?x, ?list),
   ( is_list(?list) -> true ; throw( execution_error(bad_list_arg(MAXIMUM, ?list)))),
   ?list = [?a | ?z],
   func_maximum_list(?z, ?a, ?v),
   !.
function(MEMBER(?x), ?v) :-
   eval(?x, ?list),
   ( is_list(?list) -> true ; throw( execution_error(bad_list_arg(MEMBER, ?list)))),
   !,
   member(?v, ?list).
function(MINIMUM(?x1, ?x2), ?v) :-
   eval(?x1, ?v1),
   eval(?x2, ?v2),
   func_minimum(?v1, ?v2, ?v),
   !.
function(MINIMUM(?x), ?v) :-
%   ( is_list(?x) -> ?list = ?x ; eval(?x, ?list) ),
   eval(?x, ?list),
   ( is_list(?list) -> true ; throw( execution_error(bad_list_arg(MINIMUM, ?list)))),
   ?list = [?a | ?z],
   func_minimum_list(?z, ?a, ?v),
   !.
function(MINUTES_BETWEEN(?x, ?y), ?v) :-
   eval(?x, ?dx),
   eval(?y, ?dy),
   time_interval(?dx, ?dy, mins(?v)),
   !.
function(MINUTES_BETWEEN(?x, ?y), ?v) :-
   time_interval(?x, ?y, mins(?v)),
   !.
function(MINUTES_BETWEEN(?x, ?y), ?v) :-
   throw( execution_error(bad_date_function_arg(TIME_DIFFERENCE_MINUTES, [?x,?y])) ).
function(MONTH(?x), ?v) :-
   eval(?x, ?d),
   ( is_datetime(?d) -> true ; throw( execution_error(bad_date_function_arg(MONTH, ?d)) ) ),
   !,
   datetime_extract(?d, months(?v)).
function(MSGBOX(?m), true) :-
   eval(?m, ?mm),
   !,
   (trace_flag ->
      true
      ;
      showx(?mm) ).
function(LAST(?z), ?y) :-
   !,
   eval(?z, ?list),
   ( is_list(?list) -> true ; throw( execution_error(bad_list_arg(LAST, ?list)))),
   alast(?list, ?y).
function(NEXT(?z, ?x), ?y) :-
   !,
   eval(?z, ?list),
   ( is_list(?list) -> true ; throw( execution_error(bad_list_arg(NEXT, ?list)))),
   next(?x, ?list, ?y).
function(NOW, ?now) :-
   !,
   datetime_get(now, ?now).
function(ONETIME(?g), ?tf) :-
   !,
   (call( ?g ) -> ?tf = true; ?tf = false),
   !.
function(PERMUTE(?x), ?y) :-
   !,
   eval(?x, ?l),
   ( is_list(?l) -> true ; throw( execution_error(bad_list_arg(PERMUTE, ?l)))),
   permutation(?y, ?l, _).
function(PRIOR(?z, ?y), ?x) :-
   !,
   eval(?z, ?list),
   ( is_list(?list) -> true ; throw( execution_error(bad_list_arg(PRIOR, ?list)))),
   prior(?y, ?list, ?x).
function(QUIT(?m), true) :-
   !,
   eval(?m, ?msg),
   throw( execution_error(quit(?msg)) ).
function(RANGE( find(?m, ?o, _) ), ?r) :-
   not not find_data(?m, ?o, _, _),
   !,
   findall( ?c, find_data(?m, ?o, _, ?c), ?cs ),
   stringlist_concat( ?cs, `,`, ?r ).
function(RANGE( finda(?m, ?o, _) ), ?r) :-
   not not find_data(?m, ?o, _, _),
   !,
   findall( ?c, find_data(?m, ?o, _, ?c), ?cs ),
   stringlist_concat( ?cs, `,`, ?r ).
function(RANGE( vector(?x, om(?m, ?o), _) ), ?r) :-
   not not find_data(?m, ?o, _, _),
   !,
   findall( ?c, find_data(?m, ?o, _, ?c), ?cs ),
   first(?cs, ?first),
   alast(?cs, ?last),
   stringlist_concat( [?first, ?last], `:`, ?r ).
function(RANGE( ?x ), _ ) :-
   !,
   throw( execution_error(bad_range_arg(?x)) ).
function(REMOVE_DUPLICATES( ?x ), ?y ) :-
   !,
   eval(?x, ?l),
   ( is_list(?l) -> true ; throw( execution_error(bad_list_arg(REMOVE_DUPLICATES, ?l)))),
   remove_dups(?l, ?y).
function(REVERSE( ?x ), ?r) :-
   !,
   eval(?x, ?l),
   ( is_list(?l) -> true ; throw( execution_error(bad_list_arg(REVERSE, ?l)))),
   reverse(?l, ?r).
function(SET(?x), ?s) :-
   !,
   eval(?x, ?l),
   ( is_list(?l) -> true ; throw( execution_error(bad_list_arg(SET, ?l)))),
   to_set(?l, ?s).
function(SQL(?db, ?q), ?table) :-
   !,
   eval(?q, ?sql),
   sql_query(?db, ?sql, ?table ).   
function(SORT(?x), ?sortedlist) :-
   !,
   eval(?x, ?list),
   ( is_list(?list) -> true ; throw( execution_error(bad_list_arg(SORT, ?list)))),
   sort(?list, ?sortedlist).
function(SUM(?x), ?sum) :-
   !,
   eval(?x, ?list),
   ( is_list(?list) -> true ; throw( execution_error(bad_list_arg(SUM, ?list)))),
   sum(?list, ?sum).
function(TIMEIT(?f), ?x) :-
   w_msgbox( ?f ),
   ?t1 is cputime,
   eval(?f, ?x),
   ?t is cputime - ?t1,
   w_msgbox( ?t ).
   
function(TODAY, ?today) :-
   !,
   datetime_get(today, ?today).

function(UNION(?x, ?y), ?v) :-
   eval(?x, ?xl),
   ( is_list(?xl) -> true ; throw( execution_error(bad_list_arg(UNION, ?xl)))),
   eval(?y, ?yl),
   ( is_list(?yl) -> true ; throw( execution_error(bad_list_arg(UNION, ?yl)))),
   !,
   append(?xl, ?yl, ?xy),
   to_set(?xy, ?v).
function(WEEKDAY(?x), ?v) :-
   eval(?x, ?d),
   %get_arg(?x, ?d),
   ( is_datetime(?d) -> true ; throw( execution_error(bad_date_function_arg(WEEKDAY, ?d)) ) ),
   week_dayn(?d, ?vn),
   aday_name(?vn, ?v),
   !.
function(YEAR(?x), ?v) :-
   eval(?x, ?d),
   ( is_datetime(?d) -> true ; throw( execution_error(bad_date_function_arg(YEAR, ?d)) ) ),
   !,
   datetime_extract(?d, years(?v)).

% Because we can't backtrack over a thrown error, we need this hack
% to tell if we're in a findall or not so we know whether to throw
% a not found error or not.
findall_stack_in :- ( assert( findall_stack ) ; retract( findall_stack ) ).

findall_stack_out :- ( retract( findall_stack) ; assert( findall_stack ) ).

%findall_stack_clear :- retractall( findall_stack ).
findall_stack_clear :- abolish( findall_stack/0 ).

% get the indexes for a table
get_indexes(?m, ?table, ?pos, ?indexes) :-
   known(?m, obj([?table]), [index(?pos)], ?indexes, internal),
   !.
get_indexes(?m, ?table, ?pos, ?indexes) :-
   index_predicate(?tr, ?m, ?table, ?pos, ?pred),
   (?tr = table -> 
      findall(?i, (data(?m, ?pred, _, _, _), arg(?pos, ?pred, ?i), ground(?i) ), ?dupindexes )
      ;
      findall(?i, ( call(?m:know(?pred, _,_,_,_)), arg(?pos, ?pred, ?i), ground(?i) ), ?dupindexes ) ),
   remove_dups(?dupindexes, ?indexes),
   assert_known(?m, obj([?table]), [index(?pos)], ?indexes, internal),
   !.

% find the arity of a table name, make sure big enough
index_predicate(table, ?m, ?table, ?index, ?pred) :-
   data(?m, ?x, _, _, _),
   ?x =.. [?table | ?args],
   length( ?args, ?arity ),
   ?arity >= ?index,
   functor(?pred, ?table, ?arity),
   !.
index_predicate(rule, ?m, ?table, ?index, ?pred) :-
   call( ?m: know(?x, _, _, _, _)),
   ?x =.. [?table | ?args],
   length( ?args, ?arity ),
   ?arity >= ?index,
   functor(?pred, ?table, ?arity),
   !.
index_predicate(_, ?m, ?table, ?index, _) :-
   throw(execution_error(no_index(?m, ?table, ?index))).

sum(?l, ?sum) :- sum(?l, 0, ?sum).

sum([], ?sum, ?sum) :- !.
sum([?x | ?xs], ?acc, ?sum) :-
   eval(?x, ?n),
   ?acc2 is ?acc + ?n,
   !,
   sum(?xs, ?acc2, ?sum).
   
% The list of lists should have been reversed first, so this is quicker
func_concatenate([], []) :-
   !.
func_concatenate([?a], [?a]) :-
   !.
%func_concatenate([?a, ?b | ?abs], ?cs) :-
%   append(?a, ?b, ?c),
%   !,
%   func_concatenate( [?c | ?abs], ?cs ).
func_concatenate([?b, ?a | ?abs], ?cs) :-
   append(?a, ?b, ?c),
   !,
   func_concatenate( [?c | ?abs], ?cs ).

func_maximum(?v1, ?v2, ?v) :-
   number(?v1),
   number(?v2),
   (?v1 >= ?v2 -> ?v = ?v1; ?v = ?v2),
   !.
func_maximum(?v1, ?v2, ?v) :-
   is_datetime(?v1),
   is_datetime(?v2),
   ( date_compare(?v1, >=, ?v2) -> ?v = ?v1; ?v = ?v2 ),
   !.
func_maximum(?v1, ?v2, ?v) :-
   (atom(?v1); string(?v1)),
   (atom(?v2); string(?v2)),
   (?v1 @>= ?v2 -> ?v = ?v1; ?v = ?v2),
   !.
func_maximum(?v1, ?v2, ?v) :-
   structure(?v1),
   structure(?v2),
   (?v1 @>= ?v2 -> ?v = ?v1; ?v = ?v2),
   !.
func_maximum(?v1, ?v2, ?v) :-
   is_list(?v1),
   is_list(?v2),
   (?v1 @>= ?v2 -> ?v = ?v1; ?v = ?v2),
   !.
func_maximum(?v1, ?v2, ?v) :-
   throw( execution_error(incompatible_function_argument(MAXIMUM, (?v1,?v2)) ) ).

func_maximum_list([], ?v, ?v) :-
   !.
func_maximum_list([?a|?z], ?x, ?v) :-
   func_maximum(?a, ?x, ?x2),
   !,
   func_maximum_list(?z, ?x2, ?v).
   
func_minimum_list([], ?v, ?v) :-
   !.
func_minimum_list([?a|?z], ?x, ?v) :-
   func_minimum(?a, ?x, ?x2),
   !,
   func_minimum_list(?z, ?x2, ?v).
   
func_minimum(?v1, ?v2, ?v) :-
   number(?v1),
   number(?v2),
   (?v1 =< ?v2 -> ?v = ?v1; ?v = ?v2),
   !.
func_minimum(?v1, ?v2, ?v) :-
   is_datetime(?v1),
   is_datetime(?v2),
   ( date_compare(?v1, =<, ?v2) -> ?v = ?v1; ?v = ?v2 ),
   !.
func_minimum(?v1, ?v2, ?v) :-
   (atom(?v1); string(?v1)),
   (atom(?v2); string(?v2)),
   (?v1 @=< ?v2 -> ?v = ?v1; ?v = ?v2),
   !.
func_minimum(?v1, ?v2, ?v) :-
   structure(?v1),
   structure(?v2),
   (?v1 @=< ?v2 -> ?v = ?v1; ?v = ?v2),
   !.
func_minimum(?v1, ?v2, ?v) :-
   is_list(?v1),
   is_list(?v2),
   (?v1 @=< ?v2 -> ?v = ?v1; ?v = ?v2),
   !.
func_minimum(?v1, ?v2, ?v) :-
   throw( execution_error(incompatible_function_argument(MINIMUM, (?v1,?v2)) ) ).

func_set(?l, ?s) :-
   sort(?l, ?l2),
   set_dups_away(?l2, ?s).

doop(?op, ?a, ?v) :-   % just keep, probably a date quantity
   ?v =.. [?op, ?a],
   is_datetime_interval(?v),
   !.
doop(?op, ?a, ?v) :-
   number(?a),
   ?x =.. [?op, ?a],
   ?v is ?x,
   !.
doop(?op, ?a, ?b, ?v) :-
   number(?a),
   number(?b),
   !,
   ?x =.. [?op, ?a, ?b],
   ?v is ?x.
doop(+, ?a, ?b, ?v) :-
%   (is_datetime(?a); ?a = today; ?a = now),
   is_datetime(?a),
   to_interval(?b, ?bb),
   !,
   datetime_add(?a, ?bb, ?v).
doop(+, ?a, ?b, ?v) :-
%   (is_datetime(?a); ?a = today; ?a = now),
   is_datetime(?a),
   is_era(?b),
   !,
   datetime_add(?a, ?b, ?v).
doop(+, ?a, ?b, ?v) :-
   is_era(?a),
   is_era(?b),
   !,
   add_eras(?a, ?b, ?v).
doop(-, ?a, ?b, ?v) :-
   is_era(?a),
   is_era(?b),
   !,
   subtract_eras(?a, ?b, ?v).
doop(-, ?a, ?b, ?v) :-
%   (is_datetime(?a); ?a = today; ?a = now),
   is_datetime(?a),
   to_interval(- ?b, ?bb),
   !,
   datetime_add(?a, ?bb, ?v).
doop(-, ?a, ?b, ?v) :-
   is_datetime(?a),
   is_datetime(?b),
   !,
   datetime_difference(?a, ?b, ?d),
   to_era(?d, ?v).
doop(?op, ?a, ?b, _) :-
   ?e =.. [?op, ?a, ?b],
   throw( execution_error(eval_error(?e) ) ).

is_era(era(_,_,_,_,_,_)).

add_eras(era(?y1,?m1,?d1,?h1,?n1,?s1), era(?y2,?m2,?d2,?h2,?n2,?s2), era(?y,?m,?d,?h,?n,?s)) :-
   ?y is ?y1 + ?y2,
   ?m is ?m1 + ?m2,
   ?d is ?d1 + ?d2,
   ?h is ?h1 + ?h2,
   ?n is ?n1 + ?n2,
   ?s is ?s1 + ?s1.

subtract_eras(era(?y1,?m1,?d1,?h1,?n1,?s1), era(?y2,?m2,?d2,?h2,?n2,?s2), era(?y,?m,?d,?h,?n,?s)) :-
   ?y is ?y1 - ?y2,
   ?m is ?m1 - ?m2,
   ?d is ?d1 - ?d2,
   ?h is ?h1 - ?h2,
   ?n is ?n1 - ?n2,
   ?s is ?s1 - ?s1.

% A stopgap, needs to be thought out more.  We can fix months and years,
% but if comparing an age, say 1 month 4 days to 6 weeks (42 days) well
% we can't normalize that.  Should have era keep a parallel era in days
% as well.
era_normalize(era(?y,?m,?d,?h,?n,?s), era(?yn,?mn,?d,?h,?n,?s)) :-
   ?m >= 12,
   ?yn is ?y + ?m // 12,
   ?mn is ?m mod 12,
   !.
era_normalize(?e, ?e).

to_era(?l, era(?y,?m,?d, ?h,?n,?s)) :-
   list(?l),
   (member(?y years, ?l); ?y = 0),
   (member(?m months, ?l); ?m = 0),
   (member(?d days, ?l); ?d = 0),
   (member(?h hours, ?l); ?h = 0),
   (member(?n mins, ?l); ?n = 0),
   (member(?s secs, ?l); ?s = 0),
   !.
to_era(?y years,  era(?y, 0, 0, 0, 0, 0)) :- !.
to_era(?m months, era( 0,?m, 0, 0, 0, 0)) :- !.
to_era(?d days,   era( 0, 0,?d, 0, 0, 0)) :- !.
to_era(?h hours,  era( 0, 0, 0,?h, 0, 0)) :- !.
to_era(?n mins,   era( 0, 0, 0, 0,?n, 0)) :- !.
to_era(?s secs,   era( 0, 0, 0, 0, 0,?s)) :- !.

to_interval(- era(?year, ?mon, ?day, ?hr, ?min, ?sec),
      [- ?year years, - ?mon months, - ?day days, - ?hr hours, - ?min mins, - ?sec secs]).
to_interval(era(?year, ?mon, ?day, ?hr, ?min, ?sec),
      [?year years, ?mon months, ?day days, ?hr hours, ?min mins, ?sec secs]).

era_to_string(?e, ?st) :-
   to_interval(?e, ?i),
   interval_to_string(?i, ?s),
   string_trim(?s, ?st).

interval_to_string(?i, ?s) :-
   interval_to_stringlist(?i, ?sl),
   stringlist_concat(?sl, ` `, ?s).

interval_to_stringlist([], []) :- !.
interval_to_stringlist([?u| ?us], [?s| ?ss]) :-
   unit_to_string(?u, ?s),
   !,
   interval_to_stringlist(?us, ?ss).

unit_to_string(?u, ``) :-
   ?u =.. [_, 0],
   !.
unit_to_string(?u, ?s) :-
   string_term(?s, ?u).

aday_name(0, ?x) :- get_text(monday, ?x).
aday_name(1, ?x) :- get_text(tuesday, ?x).
aday_name(2, ?x) :- get_text(wednesday, ?x).
aday_name(3, ?x) :- get_text(thursday, ?x).
aday_name(4, ?x) :- get_text(friday, ?x).
aday_name(5, ?x) :- get_text(saturday, ?x).
aday_name(6, ?x) :- get_text(sunday, ?x).

test(?op, ?x, ?y) :-
   (trace_flag -> tr_test(?op, ?x, ?y) ; testxy(?op, ?x, ?y)).

tr_test(?op, ?x, ?y) :-
   ?test =.. [?op, ?x, ?y],
   (do_trace( call(test(?test)) ) ; do_trace( fail(test(?test)) ), fail),
   testxy(?op, ?x, ?y),
   (do_trace( exit(test(?test)) ) ; do_trace( redo(test(?test)) ), fail).
      
testxy(?op, ?v1, ?v2) :-
   is_member(?op, ['>', '<', '>=', '=<']),
   !,
   (are_numbers(?v1, ?v2, ?vn1, ?vn2) ->
      ?x =.. [?op, ?vn1, ?vn2],
      call(?x)
      ;
      angle_test(?op, ?v1, ?v2) ).
testxy(?op, ?v1, ?v2) :-
   ?x =.. [?op, ?v1, ?v2],
   call(?x),
   !.

% there is an odd case where an empty cell is compared to a number,
% in which case we're going to make blank 0 instead.
are_numbers(?v1, ?v2, ?v1, ?v2) :- number(?v1), number(?v2), !.
are_numbers(?v1, '', ?v1, 0) :- number(?v1), !.
are_numbers('', ?v2, 0, ?v2) :- number(?v2).

angle_op( '>', '@>' ).
angle_op( '<', '@<' ).
angle_op( '>=', '@>=' ).
angle_op( '=<', '@=<' ).

angle_test(?op, ?e1, ?e2) :-
   is_era(?e1),
   is_era(?e2),
   !,
   era_normalize(?e1, ?ne1),
   era_normalize(?e2, ?ne2),
   angle_op(?op, ?aop),
   ?x =.. [?aop, ?ne1, ?ne2],
   call(?x),
   !.   
angle_test(?op, ?v1, ?v2) :-
   same_type(?v1, ?v2),
   angle_op(?op, ?aop),
   ?x =.. [?aop, ?v1, ?v2],
   call(?x),
   !.

% can't compare dates or era with other things
same_type(?v1, ?v2) :- is_datetime(?v1), is_datetime(?v2), !.
%same_type(?v1, ?v2) :- is_era(?v1), is_era(?v2), !.
same_type(?v1, ?v2) :- (is_datetime(?v1) ; is_datetime(?v2)), !, fail.
same_type(?v1, ?v2) :- (is_era(?v1) ; is_era(?v2)), !, fail.
same_type(_, _).


ands_list(?b, [?bv]) :-
   var(?b),
   !,
   eval(?b, ?bv).
ands_list(?a & ?b, [?av|?z]) :-
%   eval(?a, ?av),
   to_stringval(?a, ?av),
   !,
   ands_list(?b, ?z).
ands_list(?a, [?av]) :-
   to_stringval(?a, ?av).
%   eval(?a, ?av).

eval_text(?list, ?val) :-
   is_list(?list),
   !,
   to_stringvals(?list, ?strlist),
   stringlist_concat(?strlist, ?val).
eval_text(?txt1, ?txt2) :-
   to_stringval(?txt1, ?txt2).

to_stringvals([], []) :-
   !.
to_stringvals([?i|?is], [?s|?ss]) :-
   to_stringval(?i, ?s),
   !,
   to_stringvals(?is, ?ss).

/*
to_stringval(om(?m,?o), ?s) :-
   !,
   find(?m,?o,?v),
   to_stringval(?v, ?s).
*/

to_stringval(?x, ?s) :-
   var(?x),
   !,
   string_term(?x, ?s).
to_stringval(?x, ?s) :-
   eval(?x, ?v),
   to_stringval2(?v, ?s).
   
to_stringval2(find(?m,?o,?v), ?s) :-
   !,
   find(?m,?o,?v),
   to_stringval(?v, ?s).
to_stringval2(line, `\n`) :-
   !.
%to_stringval(line, ?s) :-
%   string_list(?s, [10]),
%   !.
to_stringval2(space, ` `) :-
   !.
to_stringval2(?d, ?s) :-
   is_datetime(?d),
   !,
   to_datestring(?d, ?s).
to_stringval2(?e, ?s) :-
   is_era(?e),
   !,
   era_to_string(?e, ?s).
to_stringval2(?i, ?s) :-
   string_term(?s, ?i).
   
to_datestring(datetime(?y, ?m, ?d, 0, 0, 0), ?s) :-
   !,
   date_string(date(?y, ?m, ?d), 'y-m-d', ?s).
to_datestring(date(?y, ?m, ?d), ?s) :-
   !,
   date_string(date(?y, ?m, ?d), 'y-m-d', ?s).
to_datestring(?d, ?s) :-
   string_term(?d, ?s).

to_date(datetime(?y, ?m, ?d, _, _, _), date(?y, ?m, ?d)) :- !.
to_date(date(?y, ?m, ?d), date(?y, ?m, ?d)).

%-------------------------------------------------------
% Utility Predicates for ARulesXL applications
%

prior(?y, [?x, ?y | _], ?x) :-
   !.
prior(?y, [_ | ?z], ?x) :-
   prior(?y, ?z, ?x).

next(?x, [?x, ?y | _], ?y) :-
   !.
next(?x, [_ | ?z], ?y) :-
   !, next(?x, ?z, ?y).

first([?x | _], ?x).

alast([?x], ?x) :-
   !.
alast([_ | ?z], ?x) :-
   !, alast(?z, ?x).

% Find the element after a given element in a sorted list
after( [?x | ?xs], ?y, ?x ) :-
   ?x @> ?y,
   !.
after( [?x | ?xs], ?y, ?z ) :-
   after( ?xs, ?y, ?z ).

% find the element before a given element in a sorted list  
before( [?x1, ?x2 | ?xs], ?y, ?x1 ) :-
   ?x1 @< ?y,
   ?x2 @>= ?y,
   !.
before( [?x], ?y, ?x ) :-
   ?x @< ?y,
   !.
before( [?x | ?xs], ?y, ?z ) :-
   !,
   before( ?xs, ?y, ?z ).

% removing dups from a sorted list is quicker and easier

to_set(?l, ?s) :-
   sort(?l, ?l2),
   set_dups_away(?l2, ?s).
   
set_dups_away([], []) :- !.
set_dups_away([?a, ?b | ?xs], ?ys) :-
   ?a == ?b,
   !, set_dups_away( [?b | ?xs], ?ys ).
set_dups_away([?x | ?xs], [?x | ?ys]) :-
   !, set_dups_away(?xs, ?ys).

is_subset([], _) :- !.
is_subset([?x| ?xs], ?l) :-
   is_member(?x, ?l),
   !, is_subset(?xs, ?l).

% for testing translate, can't use member as predicate
mimber(?a, [?a | _]).
mimber(?a, [_ | ?z]) :- mimber(?a, ?z).

