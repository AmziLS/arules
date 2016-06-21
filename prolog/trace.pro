%--------------------------------------------------------
% ARules Trace Support
%

:- import(list).
:- import(date_time).
:- import(aodbc).
:- include('opdefs.pro').

% comment out for real runs
% test_trace.

%---------------------------------------------
% put trace info on external file
% for testing
%

set_trace_path(?path) :-
   retractall( trace_file(_) ),
   strcat(?path, `/ARulesXL Trace.txt`, ?trace_file),
   assert( trace_file( ?trace_file ) ).
   
init_trace_output :-
   retractall( trace_handle(_) ),
   trace_file( ?trace_file ),
   catch( open(?trace_file, write, ?id, [alias(trace)]),
          _,
          fail ),
   write(trace, `Starting Trace\n`),
   date(?month, ?day, ?year),
   write(trace, ?year / ?month / ?day ),
   tab(trace, 2),
   time(?hour, ?min, ?sec),
   write(trace, ?hour : ?min : ?sec),
   nl(trace),
   assert( trace_handle(?id) ),
   cntr_set(0,0),     % for indentation
   !.
init_trace_output :-
   retractall( trace_handle(_) ).
   
close_trace_output :-
   trace_handle(?id),
   !,
   close(?id).
close_trace_output.

file_trace_output(?s) :-
   trace_handle(_),
   spaces(?sp),
   write(trace, ?sp),
%   ?to = ?s,
   trace_output(?s, ?to),
   write(trace, ?to),
   nl(trace),
   !.
file_trace_output(_).
   
%-------------------------------------------------
% Tracing
%

initialize_trace(on) :-
   set_parm(trace, on),
   set_parm(step, on),
   delete_parm(skipping).
initialize_trace(off) :-
   set_parm(trace, off),
   set_parm(step, off),
   delete_parm(skipping).
   
explain(_, _, _) :-
   get_parm(explain, off),
   !.
explain(?result, ?mod, ?cell) :-
	get_parm(explain, on),
%   trace_cell(?mod, ?cell, 1),  % 1 = call
   trace_output(rule(call, ?mod, ?cell, ?head)),
   trace_step.
explain(?result, ?mod, ?cell) :-
	get_parm(explain, on),
%   trace_cell(?mod, ?cell, 2),  % 2 = fail
   trace_output(rule(fail, ?mod, ?cell, ?head)),
   trace_step,
   !,
   fail.

do_trace(find(_, RuleSetLocale(language), _, _)) :-
   !.
%do_trace(find(_, obj([RuleSetLocale(language)]), _)) :-  % ignore this one
%   !.
do_trace( query(?m, ?q) ) :-
   assert_trace( query(?m, ?q) ),
   !,
   trace_step.
do_trace(done) :-
   assert_trace( "arules_trace_done" ),
   !.
do_trace( find(?m, ?obj, ?props, ?x) ) :-
%   cntr_inc(0,_),
   assert_trace( find(?m, ?obj, ?props, ?x) ),
   !,
   trace_step.
do_trace( found(?m, ?obj, ?props, ?x, ?r) ) :-
%   retract_trace( find(?m, ?obj, ?props, _) ), % the most recent find
   clear_trace( find(?m, ?obj, ?props, _) ), % the most recent find and from sub sets
%   retract_trace( found(_, _, _, _, _) ),  % only one found at a time
   retract_trace( exit(_) ), % clear last exit
   assert_trace( found(?m, ?obj, ?props, ?x, ?r) ),
%   cntr_dec(0,_),
   clear_skip,
   !,
   trace_step.
do_trace( data(?m, ?obj, ?props, ?x) ) :-
%   retract_trace( find(?m, ?obj, ?props, _) ), % the most recent find
%   retract_trace( found(_, _, _, _, _) ),  % only one found at a time
%   retract_trace( exit(_) ), % clear last exit
   assert_trace( data(?m, ?obj, ?props, ?x) ),
   clear_skip,
   !,
   trace_step.
do_trace( known(?m, ?obj, ?props, ?x) ) :-
%   retract_trace( find(?m, ?obj, ?props, _) ), % the most recent find
%   retract_trace( found(_, _, _, _, _) ),  % only one found at a time
%   retract_trace( exit(_) ), % clear last exit
   assert_trace( known(?m, ?obj, ?props, ?x) ),
   clear_skip,
   !,
   trace_step.
do_trace( findall ) :-
   cntr_inc(0,_),
   assert_trace( findall ),
   !,
   trace_step.
do_trace( foundall(?list) ) :-
   clear_trace( findall ),
   assert_trace( foundall(?list) ),
   cntr_dec(0,_),
   clear_skip,
   !,
   trace_step.
   
do_trace( call(test(?t)) ) :-
   retract_trace( fail(_) ),
   cntr_inc(0,_),
   assert_trace( call(test(?t)) ),
   !,
   trace_step.
do_trace( exit(test(?t)) ) :-
%   retract_trace( call(test(?t)) ), 
   clear_trace( test(?t) ),
   assert_trace( exit(test(?t)) ),
   cntr_dec(0,_),
   clear_skip,
   !.
do_trace( redo(test(?t)) ) :-
%   retract_trace( exit(test(?t)) ),
   clear_trace( test(?t) ),
   cntr_inc(0,_),
   assert_trace( redo(test(?t)) ),
   !.
do_trace( fail(test(?t)) ) :-
%   retract_trace( call(rule(?r)) ),
%   retract_trace( redo(rule(?r)) ),
%   retract_trace( found(_, _, _, _, _) ),
   clear_trace( test(?t) ),
   assert_trace( fail(test(?t)) ),
   cntr_dec(0,_),
   clear_skip,
   !.

do_trace( eval(?x) ) :-
   assert_trace( eval(?x) ),
   !,
   trace_step.
do_trace( eval(?x, ?y) ) :-
   clear_trace( eval(?x) ),
   assert_trace( eval(?x, ?y) ),
   !,
   trace_step.


do_trace( call(logic(?r)) ) :-
   retract_trace( fail(logic(_)) ),
   cntr_inc(0,_),
   assert_trace( call(logic(?r)) ),
   !,
   trace_step.
do_trace( exit(logic(?r)) ) :-
%   retract_trace( call(rule(?r)) ), 
   clear_trace( logic(?r) ),
   assert_trace( exit(logic(?r)) ),
   cntr_dec(0,_),
   clear_skip,
   !,
   trace_step.
do_trace( redo(logic(?r)) ) :-
%   retract_trace( exit(rule(?r)) ),
   clear_trace( logic(?r) ),
   cntr_inc(0,_),
   assert_trace( redo(logic(?r)) ),
   !,
   trace_step.
do_trace( fail(logic(?r)) ) :-
%   retract_trace( call(rule(?r)) ),
%   retract_trace( redo(rule(?r)) ),
%   retract_trace( found(_, _, _, _, _) ),
   clear_trace( logic(?r) ),
   assert_trace( fail(logic(?r)) ),
   cntr_dec(0,_),
   clear_skip,
   !,
   trace_step.




do_trace( call(rule(?r)) ) :-
   retract_trace( fail(rule(_)) ),
   cntr_inc(0,_),
   assert_trace( call(rule(?r)) ),
   !,
   trace_step.
do_trace( exit(rule(?r)) ) :-
%   retract_trace( call(rule(?r)) ), 
   clear_trace( rule(?r) ),
   assert_trace( exit(rule(?r)) ),
   cntr_dec(0,_),
   clear_skip,
   !,
   trace_step.
do_trace( redo(rule(?r)) ) :-
%   retract_trace( exit(rule(?r)) ),
   clear_trace( rule(?r) ),
   cntr_inc(0,_),
   assert_trace( redo(rule(?r)) ),
   !,
   trace_step.
do_trace( fail(rule(?r)) ) :-
%   retract_trace( call(rule(?r)) ),
%   retract_trace( redo(rule(?r)) ),
%   retract_trace( found(_, _, _, _, _) ),
   clear_trace( rule(?r) ),
   assert_trace( fail(rule(?r)) ),
   cntr_dec(0,_),
   clear_skip,
   !,
   trace_step.
   
do_trace( error(?m) ) :-
   assert_trace( error(?m) ),
   !.
do_trace( ?x ) :-
   assert_trace( ?x ),
   !,
   trace_step.

undo_trace( ?x ) :-
   retract_trace( ?x ).
   
assert_trace( ?x ) :-
   file_trace_output( ?x ),
   asserta( trace( ?x ) ).
retract_trace( ?x ) :-
   ( retract( trace(?x) ) ; true ),
   !.

% Keep retracting until the thing we retracted was the thing we
% were looking for.
clear_trace( rule(?r) ) :-
   not ( trace( call(rule(?r)) ) ;
         trace( exit(rule(?r)) ) ;
         trace( redo(rule(?r)) ) ;
         trace( fail(rule(?r)) ) ),
   !.
clear_trace( rule(?r) ) :-
   !,
   retract( trace( ?x ) ),
   (?x = call( rule(?r) ); ?x = exit( rule(?r) ); ?x = fail( rule(?r) ); ?x = redo( rule(?r) )),
   !.
   
clear_trace( test(?t) ) :-
   not ( trace( call(test(?t)) ) ;
         trace( exit(test(?t)) ) ;
         trace( redo(test(?t)) ) ;
         trace( fail(test(?t)) ) ),
   !.
clear_trace( test(?t) ) :-
   !,
   retract( trace( ?x ) ),
   (?x = call( test(?t) ); ?x = exit( test(?t) ); ?x = fail( test(?t) ); ?x = redo( test(?t) )),
   !.
   
clear_trace( logic(?r) ) :-
   not ( trace( call(logic(?r)) ) ;
         trace( exit(logic(?r)) ) ;
         trace( redo(logic(?r)) ) ;
         trace( fail(logic(?r)) ) ),
   !.
clear_trace( logic(?r) ) :-
   !,
   retract( trace( ?x ) ),
   (?x = call( logic(?r) ); ?x = exit( logic(?r) ); ?x = fail( logic(?r) ); ?x = redo( logic(?r) )),
   !.

clear_trace( find( ?m, ?o, ?p, _) ) :-
   !,
   retract_trace( find( ?m, ?o, ?p, _)),
   % now see if there are any immediately above it which are the
   % same but from a sub set rule set, i.e., we have an inheritance chain
   ( trace( find( ?mm, ?oo, ?pp, _) ) ->
      ( (?oo = ?o, ?pp = ?p) ->
           clear_trace( find( ?mm, ?o, ?p, _) )
           ;
           true )
       ;
       true ).

clear_trace( ?x ) :-
   not trace(?x),
   !.
clear_trace( ?x ) :-
   retract( trace( ?y ) ),
   ?x = ?y,
   !.

retractall_trace( ?x ) :-
   retractall( trace(?x) ).

trace_output(query(?m, ?q), ?s) :-
   get_text(query(?m, ?q), ?s),
   !.
trace_output( find(?m, ?obj, ?props, ?x), ?s ) :-
   object_string(?obj, ?props, ?os),
   get_text(trace_find, ?action),
   (var(?x) -> ?xx = '?' ; ?xx = ?x),
   trace_string( [?action, ?m, ?os, ?xx, ''], ?s ),
   !.
trace_output( found(?m, ?obj, ?props, ?x, ?r), ?s ) :-
   object_string(?obj, ?props, ?os),
   get_text(trace_found, ?action),
   format_value_string(?x, ?vx),
   trace_string( [?action, ?m, ?os, ?vx, ?r], ?s ),
   !.
trace_output( data(?m, ?obj, ?props, ?x), ?s ) :-
   object_string(?obj, ?props, ?os),
   get_text(trace_data, ?action),
   format_value_string(?x, ?vx),
   trace_string( [?action, ?m, ?os, ?vx, ''], ?s ),
   !.
trace_output( known(?m, ?obj, ?props, ?x), ?s ) :-
   object_string(?obj, ?props, ?os),
   get_text(trace_known, ?action),
   format_value_string(?x, ?vx),
   trace_string( [?action, ?m, ?os, ?vx, ''], ?s ),
   !.
   
trace_output( call(rule(?r)), ?s ) :-
   get_text(trace_try, ?action),
   trace_string( [?action, '', '', '', rule(?r)], ?s ),
   !.
trace_output( fail(rule(?r)), ?s ) :-
   get_text(trace_fail, ?action),
   trace_string( [?action, '', '', '', rule(?r)], ?s ),
   !.
trace_output( exit(rule(?r)), ?s ) :-
   get_text(trace_exit, ?action),
   trace_string( [?action, '', '', '', rule(?r)], ?s ),
   !.
trace_output( redo(rule(?r)), ?s ) :-
   get_text(trace_redo, ?action),
   trace_string( [?action, '', '', '', rule(?r)], ?s ),
   !.
   
trace_output( call(test(?t)), ?s ) :-
   ground(?t),
   get_text(trace_test, ?action),
   string_termq(?ts, ?t),
   trace_string( [?action, '', '', ?ts, ''], ?s ),
   !.
trace_output( call(test(?t)), ?s ) :-
   not ground(?t),
   get_text(trace_match, ?action),
   string_termq(?ts, ?t),
   trace_string( [?action, '', '', ?ts, ''], ?s ),
   !.
trace_output( fail(test(?t)), ?s ) :-
   get_text(trace_false, ?action),
   string_termq(?ts, ?t),
   trace_string( [?action, '', '', ?ts, ''], ?s ),
   !.
trace_output( exit(test(?t)), ?s ) :-
   get_text(trace_true, ?action),
   string_termq(?ts, ?t),
   trace_string( [?action, '', '', ?ts, ''], ?s ),
   !.
trace_output( redo(test(?t)), ?s ) :-
   get_text(trace_retest, ?action),
   string_termq(?ts, ?t),
   trace_string( [?action, '', '', ?ts, ''], ?s ),
   !.
   
trace_output( call(logic(?r)), ?s ) :-
   get_text(trace_try, ?action),
   string_termq(?rs, ?r),
   trace_string( [?action, '', '', ?rs, ''], ?s ),
   !.
trace_output( fail(logic(?r)), ?s ) :-
   get_text(trace_fail, ?action),
   string_termq(?rs, ?r),
   trace_string( [?action, '', '', ?rs, ''], ?s ),
   !.
trace_output( exit(logic(?r)), ?s ) :-
   get_text(trace_exit, ?action),
   string_termq(?rs, ?r),
   trace_string( [?action, '', '', ?rs, ''], ?s ),
   !.
trace_output( redo(logic(?r)), ?s ) :-
   get_text(trace_redo, ?action),
   string_termq(?rs, ?r),
   trace_string( [?action, '', '', ?rs, ''], ?s ),
   !.

trace_output( error(?m), ?s ) :-
%   string_termq(?sm, ?m),
%   trace_string( [Error, ?sm, '', '', ''], ?s ),
   trace_string( [Error, '', '', '', ''], ?s ),
   !.
trace_output( findall, ?s ) :-
   get_text(findall, ?ft),
   trace_string( [?ft, '', '', '', ''], ?s),
   !.
trace_output( foundall(?l), ?s ) :-
   get_text(foundall, ?ft),
   format_value_string(?l, ?v),
%   string_term(?sl, ?l),
%   truncate_string(?sl, 80, ?v),
   trace_string( [?ft, '', '', ?v, ''], ?s),
   !.
trace_output(?s, ?s) :-
   string(?s),
   !.
trace_output(?x, ?s) :-
   string_termq(?s, ?x),
   !.

format_value_string(?l, ?vv) :-
   list(?l),
   to_value_string_list(?l, ?sl),
   stringlist_concat(?sl, `\n`, ?v),
   stringlist_concat([`[`, ?v, `]`], ?vv),
   !.
format_value_string(?x, ?v) :-
   string_termq(?v, ?x).

to_value_string_list([], []) :- !.
to_value_string_list([?x | ?xs], [?s | ?ss]) :-
   string_termq(?s, ?x),
   !, to_value_string_list(?xs, ?ss).
   
truncate_string(?s, ?n, ?s) :-
   string_length(?s, ?l),
   ?l =< ?n,
   !.
truncate_string(?s, ?n, ?std) :-
   ?nn is ?n - 3,
   sub_string(?s, 1, ?nn, ?st),
   strcat(?st, `...`, ?std).

trace_string([?a, ?m, ?o, ?x, ?r], ?s) :-
	string_term(?sx, ?x),
	string_term(?rx, ?r),
	stringlist_concat([?a, ?m, ?o, ?sx, ?rx], `;`, ?s).

spaces(?ss) :-
   cntr_get(0, ?i),
   make_spaces(?i, ``, ?ss).

make_spaces(?i, ?ss, ?ss) :-
   ?i =< 0,
   !.
make_spaces(?i, ?acc, ?ss) :-
   strcat(?acc, `  `, ?acc2),
   ?i2 is ?i - 1,
   !,
   make_spaces(?i2, ?acc2, ?ss).

% Get the last rule called, so we can skip until we see
% it getting cleared.
last_rule(?r) :-
   trace(?x),
   skip_point(?x, ?r),
   !.
last_rule(start).

skip_point( call(?r), ?r ).
skip_point( redo(?r), ?r ).
skip_point( find(?m, ?obj, ?props, _), find(?m, ?obj, ?props ) ).
skip_point( findall, findall ).

% If we're skipping, see if the latest trace information clears
% the skip.
clear_skip :-
   get_parm(skipping, ?r),
   once trace(?x), % just get the latest, i.e. first one
   skip_match(?x, ?r),
   delete_parm(skipping),
   set_parm(step, on),
   !.
clear_skip.

skip_match( exit(?r), ?r ).
skip_match( fail(?r), ?r ).
skip_match( found(?m, ?obj, ?props, _, _), find(?m, ?obj, ?props) ).
skip_match( foundall(?l), findall ).

trace_step :-
   get_parm(step, off),
   !.
trace_step :-
   test_trace,   % comment out at top of file for real runs
   !,
   get_action(?x),
   trace_action(?x),
   !.
trace_step :-
%   ( file_trace_output( callasync ); file_trace_output( failasync ), fail ),
   get_async_action(?x),
%   ( file_trace_output( exitasync(?x) ); file_trace_output( redoasync ), fail ),
	trace_action(?x),
	!.

get_action(?x) :-
   nl, trace_report,
   write('Action? '),
   read_string(?s),
   string_atom(?s, ?a),
   atom_action(?s, ?x),
   !.
   
atom_action('', step).
atom_action(k, skip).
atom_action(r, run).
atom_action(q, quit).
atom_action(?a, ?a).


trace_report :-
   arxl_trace_item(?ti),
   write(?ti),nl,
   fail.
trace_report :-
   nl,
   arxl_known_item(?ki),
   write(?ki), nl,
   fail.
trace_report :-
   nl.

trace_action(quit) :- set_parm(step, off).
trace_action(run) :- set_parm(step, off).
trace_action(skip) :- last_rule(?r), set_parm(skipping, ?r), set_parm(step, off).
trace_action(step) :- set_parm(step, on).
trace_action(error) :- assert(trace("Error awaiting user input")).
trace_action(?huh) :- string_term(?act, "Unknown action " - ?huh), assert(trace(?act)).


%---------------------------------------------------
% logging
%

% parm(logging, on).

logopen :-
   get_parm(logfile, open),
   !.
logopen :-
   open('c:/temp/aruleslog.txt', write, _, [alias(log)]),
   set_parm(logfile, open).

timeit(?c) :-
   timer(?t1),
   call(?c),
   timer(?t2),
   ?t3 is ?t2 - ?t1,
   log( timed(?t3, ?c) ).
   
log(_) :-
   not get_parm(logging, on),
   !.
log(?x) :- 
   logopen,
   time(?h,?m,?s),
   write(log, ?h: ?m: ?s),
   tab(log, 2),
   writeq(log, ?x),
   nl(log).

%--------------------------------------------------------------
% Diagnostic performance probe
%


probe(?tag, ?goals) :-
   copy_term(?tag, ?tagcopy),
%   numbervars(?tag, 1, _),
   ( probe_call(?tag); probe_fail(?tag) ),
   call(?goals),
   ( probe_exit(?tag); probe_redo(?tagcopy) ).

probe_call(?tag) :-
   ?tcall is cputime,
   asserta(probe_enter(?tcall)),
   push_probe_stack(?tag),
   !.

probe_fail(?tag) :-
   ?tfail is cputime,
   once ( retract(probe_enter(?t1)); ?t1 = ?tfail ) ,
   ?t2 is ?tfail - ?t1,
   probe_data(?tag, ?t2),
   pop_probe_stack(?tag),
   !, fail.

probe_exit(?tag) :-
   ?texit is cputime,
   once ( retract(probe_enter(?t3)); ?t3 = ?texit ) ,
   ?t4 is ?texit - ?t3,
   probe_data(?tag, ?t4),
   pop_probe_stack(?tag),
   !.

probe_redo(?tag) :-
   ?tredo is cputime,
   push_probe_stack(?tag),
   asserta(probe_enter(?tredo)),
   !, fail.

push_probe_stack(?tag) :-
   asserta( probe_stack(?tag) ),
   !.
   
pop_probe_stack(?tag) :-
   retract( probe_stack(?tag) ),
   !.
pop_probe_stack(?tag) :-
   write('pop failed ': ?tag), nl,
   findall( ?x, probe_stack(?x), ?ps ),
   write('probe_stack ': ?ps), nl,
   throw( pop_probe_error ),
   !, fail.
   
probe_data(?tagx, ?t2) :-
   findall(?tg, probe_stack(?tg), ?rtag),
   reverse(?rtag, ?tag),
   ( retract( timing_data(?tag, ?x1, ?t1) ),
     ?t3 is ?t1 + ?t2,
     ?x3 is ?x1 + 1
     ;
     ?x3 = 1,
     ?t3 = ?t2 ),
   asserta( timing_data(?tag, ?x3, ?t3) ),
   !.
%probe_data(?tag, ?t2) :-
%   asserta( timing_data(?tag, 1, ?t2) ).

probe_report1 :-
   findall( [?t, ?x, ?c, ?m, ?o], ( timing_data( find(?c, ?m, ?o), ?x, ?t) ), ?xmos ),
   sort(?xmos, ?up_xmos),
   reverse(?up_xmos, ?down_xmos),
   write_list(?down_xmos, '\n').

pr2 :- tell('c:\\temp\\probe2.txt'), probe_report2, told.

probe_report2 :-
   findall( [?mos, ?x, ?t], ( timing_data( ?mos, ?x, ?t) ), ?xmos ),
   sort(?xmos, ?sorted_xmos),
   probe_write_list(?sorted_xmos).
   
probe_write_list([]) :- nl.
probe_write_list( [ [?mos, ?x, ?t] | ?xmos ] ) :-
   (?t > 0.05 -> write(time = ?t), nl; true),
   (?x > 10 -> write(tick = ?x), nl; true), 
   write_mos(?mos), tab(2),
   write(?x), tab(1),
   write(?t), nl,
   !, probe_write_list(?xmos).

write_mos([]).
write_mos([?mo]) :- write(?mo), !.
write_mos([_ | ?mos]) :- tab(3), !, write_mos(?mos).


%------------------------------------------------------------
% Other debugging tools
%

mybuginit :-
   get_parm(buginited, true),
   !.
mybuginit :-
   set_parm(buginited, true),
   ( xl_msgbox("debugging on") ; write('*** DEBUGGING ON ***'), nl ),
   buginit('c:/temp/arulesbug.txt').

user_bugwrite(_, ?x) :- showx(?x).

showx(?x) :-
  string_termq(?s, ?x),
%   w_msgbox(?s),  % put this in to get system message box
   xl_msgbox(?s),
   !.
showx(?x) :-
   writeq(?x),
   nl.

debugprint(?x) :-
   string_termq(?s, ?x),
   xl_debugprint(?s),
   !.
debugprint(_).