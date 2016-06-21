:- import(list).
:- import(date_time).
:- import(rename).
:- include('opdefs.pro').
:- op(600, fx, test).

%-----------------------------------------------
% various mains
%

ic2 :-
   load('ic2.axl'),
%   consult('ic2.src'),
   set_parm(parser, new),
%   arxl_query(CompRules, true, "FIND OTV", ?ans),
   ?t1 is cputime,
   cntr_set(0, 0),
   prebuild_query(CompRules, "FIND TotalComm", ?goal, ?data),
   
   repeat,
   fast_query(CompRules, true, ?goal, ?data, ?ans),
%   arxl_query(CompRules, true, "FIND TotalComm", ?ans),
   cntr_inc(0, ?i),
%   write(?i), nl,
   ?i >= 100,
   
   write(?ans), nl,
   ?t2 is cputime - ?t1,
   write(?t2), nl,
   ?s is 20000000 * ?t2 / 100,
   ?h is ?s / 3600,
   write(?h), nl.

main :- rmain.

xmain :-
   repeat,
   write('> '),
   read(?x),
   write(?x),
   nl,
   ?x = z.

% regress testing
rmain :-
	assert(regression_testing(true)),
   data_available,
   test( regress ).

% VL testing
vmain :- vl2, pr2.

heinz :- test 57.
abc :- test 56.
t58 :- test 58.
t63 :- test 63.

% just to test rename
rename_test :-
   rename(`= "bbb" & rquery(   ddd, "FIND a WHEN ddd!.d = 5 and weekend(?x) and e = 3")`, ddd, eee, ?x),
   writeq(x = ?x).

data_available :-
   case( _, _ ),
   !.
data_available :-
   consult( test_data ).

%----------------------------------------------------
% test
%

timed_test( ?case ) :-
   ?t1 is cputime,
   test(?case),
   ?t is cputime - ?t1,
   write(time = ?t), nl.
   
test( regress ) :-
   data_available,
   set_flag( regress, on ),
   case( ?case, ?attrs ),
   once do_case( ?case, ?attrs ),
   fail.
test( regress ).

test( ?case ) :-
	assert(regression_testing(true)),
   (test_trace -> set_parm(trace, on), set_parm(step, on) ; true ),
   data_available,
   set_flag( regress, off ),
   case( ?case, ?attrs ),
   do_case( ?case, ?attrs ).

do_case( ?case, ?attrs ) :-
   member( rulesets = [?rs | ?rss], ?attrs ),
   ( member( ask_answers = ?aas, ?attrs ) ->
       retractall(ask_answer(_)),
       assert_aas( ?aas )
       ;
       true ),
   nl,
   write( start( ?case, [?rs | ?rss] ) ),
   nl,
   do_rulesets( [?rs | ?rss] ),
   do_test_queries( ?rs ),
   write( finish( ?case ) ),
   nl.


%-------------------------------------------------------------------
% load the rules
%

do_rulesets([]).
do_rulesets([ ?rs | ?rss ]) :-
   ruleset(?rs),
   !,
   do_rulesets(?rss).

%ruleset(r45) :-
%   current_module(r45),
%   !.
ruleset(?rs) :-
   clear_ruleset(?rs),
   set_parm(decision_table, false),
   add_test_rules(?rs),
   add_special_asserts(?rs),
   do_data(?rs).

% --- the rules ---

add_test_rules(?RuleSet) :-
   r(?RuleSet, ?Addr, ?RuleStr),
%   ( get_parm(parser,new) ->
%        once pup_text(?RuleStr, ?Str)
%        ;
%        ?Str = ?RuleStr ),
   do_add_rule(?RuleSet, ?Addr, ?RuleStr),
   fail.
add_test_rules(?RuleSet) :-
   true.

do_add_rule(_, _, []) :-
	set_parm(decision_table, false),
	!.
do_add_rule(?RuleSet, ?Addr, ?lst) :-
   get_parm(decision_table, true),
   get_parm(decision_table_headers, false),
   decision_table_headers( ?RuleSet, ?lst ),
   set_parm(decision_table_headers, true),
   !.
do_add_rule(?RuleSet, ?Addr, ?lst) :-
   get_parm(decision_table, true),
   get_parm(decision_table_headers, true),
   decision_table_row( ?RuleSet, "cell", ?lst ),
   !.
do_add_rule(?RuleSet, ?Addr, ?str) :-
   not get_parm(decision_table, true),
   sub_string(?str, ?x, _, "Table"), ?x = 1,  % can't have arg 2 bound without 3
   set_parm(decision_table, true),
   set_parm(decision_table_headers, false),
   define_decision_table( ?RuleSet, ?str),
   !.
do_add_rule(?RuleSet, ?Addr, ?str) :-
   not get_parm(decision_table, true),
   add_rule(?RuleSet, ?Addr, ?str),
   output(Added : ?RuleSet : ?Addr : ?str),
   output(nl),
%   warning_report,
   !.
do_add_rule(?RuleSet, ?Addr, ?Str) :-
   output(Failed : ?RuleSet : ?Addr : ?Str),
   output(nl),
   rule_error(?text),
   output(?text),
   output(nl),
%   warning_report,
   !,
   fail.

% --- special asserts ---

% some of the rules for testing are over here so that they
% don't usurp user's capitalization of R etc.

% we might be asserting some clauses from a listing for debugging
% purposes.
add_special_asserts(?rs) :-
   a(?rs, ?r),
   assert( ?rs : ?r ),
   fail.
add_special_asserts(_).

% --- data tables ---
%do_data(r45) :-
%   get_data(r45, _, _, _),
%   !.
do_data(?rs) :-
%   write( loading_data : ?rs ),
   retract_data( data(?rs, _, _, _) ),
   d(?rs, ?obj, ?data),
   once add_headers(?rs, ?obj, ?data),
   once add_data(?rs, ?obj, ?data),
   fail.
do_data(?rs) :-
   dd(?rs, ?o, ?p, ?v, ?c),
   assert( data(?rs, ?o, ?p, ?v, ?c) ),
   fail.
do_data(_).

add_headers(?rs, ?obj, [?rows, ?cols | ?data]) :-
   assert_data( data(?rs, obj([?obj, headers(1)]), ?rows, d2) ),
   assert_data( data(?rs, obj([?obj, headers(2)]), ?cols, d3) ).

add_data(?rs, ?obj, [?rows, ?cols | ?data]) :-
   add_data(?rs, ?obj, ?rows, ?cols, ?data).

add_data(_, _, _, _, []) :-
   !.
add_data(?rs, ?obj, [?rname | ?rnames], ?cnames, [?row | ?rows]) :-
   add_data_row(?rs, ?obj, ?rname, ?cnames, ?row),
   !,
   add_data(?rs, ?obj, ?rnames, ?cnames, ?rows).

add_data_row(_, _, _, _, []) :-
   !.
add_data_row(?rs, ?obj, ?rname, [?cname | ?cnames], [?valstr | ?vals]) :-
   ?obj =.. [?oname | ?oargs],
   append(?oargs, [?rname, ?cname], ?orcargs),
   ?objrc =.. [?oname | ?orcargs],
	(string( ?valstr) ->
	   t_tokenize(?valstr, ?valtoks),
	   p_parse_value(?val, ?valtoks, [])
	   ;
	   ?val = ?valstr),
   assert_data( data( ?rs, obj( [?objrc] ), ?val, d1 )),
   !,
   add_data_row(?rs, ?obj, ?rname, ?cnames, ?vals).

%-----------------------------------------------------------
% Do the queries
%

do_test_queries(?RuleSet) :-
   ( q(?RuleSet, ?QStr) ; q(?RuleSet, ?QStr, ?RA) ),
%   ( get_parm(parser,new) ->
%        once pup_text(?QStr, ?Q)
%        ;
%        ?Q = ?QStr ),
   ?Q = ?QStr,
   output(nl),
   output(Query = ?Q),
   output(nl),
   (once ( query_rules(?RuleSet, false, ?Q, ?A); test_cope ) ),
   output(Answer = ?A),
   output(nl),
   show_known, output(nl),
   string_termq(?s1, ?RA),
   string_termq(?s2, ?A),
%   ( ?RA \== ?A ->
%      write(?RA - '*** AAARRGGHHHH ***' - ?A), nl ; true),
   ( ?s1 == ?s2 ->
      true
      ;
%      writeq(?RA - '*** AAARRGGHHHH ***' - ?A), nl ),
      string_termq(?RAstr, ?RA),
      string_termq(?Astr, ?A),
      stringlist_concat( [ `***** Wrong: `, ?Astr, ` Should be: `, ?RAstr ], ?Note ),
      write(?Note), nl ),
   fail.
do_test_queries(_).

test_cope :-
   query_error(?err),
   write('* * *': ?err),
   nl,
   show_known, nl,
   listing(known), nl,   
   !,
   fail.
test_cope :-
   write('* * *':Huh),
   nl,
   fail.

%------------------------------------------------------
% test utilities
%

assert_aas([]) :- !.
assert_aas([?aa | ?aas]) :-
   assert( ask_answer(?aa) ),
   !, assert_aas( ?aas ).

set_flag( ?f, ?v ) :-
   retractall( flag(?f, _) ),
   assert( flag(?f, ?v) ).

output( ?x ) :-
   flag( regress, on ),
   !.
output( nl ) :-
   nl,
   !.
output( ?x ) :-
   writeq( ?x ),
   !.

warning_report :-
   p_warning_message(?l, ?x),
   write(?l - ?x), nl,
   fail.
warning_report.

show_known :-
   get_known_string(?r, ?os, ?v, ?src),
   output(?r: ?os: ?v: ?src),
   output(nl),
   fail.
show_known.


   