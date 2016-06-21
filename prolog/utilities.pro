%--------------------------------------------------------
% ARules Utilities
%

:- import(list).
:- import(date_time).
:- import(aodbc).
:- include('opdefs.pro').

%------------------------------------------------
% Maintaining language versions
%

missing_dicts :-
   arxlEnglish:dict(?english, ?keyword),
   not arxlFrench:dict(_, ?keyword),
%   write(?keyword), nl, nl,
   writeq(dict(?english, ?keyword)), nl,
   writeq(dict( French, ?keyword)), nl, nl,
   fail.
missing_dicts.

missing_texts :-
   arxlEnglish:text(?t, ?e),
   not arxlFrench:text(?t, _),
%   write(?t - ?e), nl, nl,
   writeq(text(?t, ?e)), nl,
   writeq(text(?t, French)), nl, nl,
   fail.
missing_texts.

%---------------------------------------------
% Data functions
%

assert_data( data( ?rs, ?o, ?v, ?c ) ) :-
   obj_parts(?o, ?obj, ?props),
	assert( data(?rs, ?obj, ?props, ?v, ?c) ).

retract_data( data( ?rs, ?o, _, _) ) :-
   obj_parts(?o, ?obj, ?props),
   findall( obj([?obj|?props]), data(?rs, ?obj, ?props, _, _), ?os),
   retract_objs_data(?os, ?rs).

retract_objs_data([], _) :- !.
retract_objs_data([?o| ?os], ?rs) :-
   obj_parts(?o, ?obj, ?props),
   retract( data(?rs, ?obj, ?props, _, _) ),
   !, retract_objs_data( ?os, ?rs ).

retract_rs_data(?rs) :-
   data(?rs, ?o, ?p, _, _),
   retract( data(?rs, ?o, ?p, _, _) ),
   fail.
retract_rs_data(_).

/*
retract_data( data( ?rs, ?o, _, _) ) :-
   obj_parts(?o, ?obj, ?props),
   data(?rs, ?obj, ?props, _, _),   % this is needed to bind vars so indexed retract works
   retract( data(?rs, ?obj, ?props, _, _) ),
   fail.
retract_data( _ ).
*/

get_data( ?m, ?o, ?v, ?c ) :-
   obj_parts(?o, ?obj, ?props),
   data(?m, ?obj, ?props, ?v, ?c).

%---------------------------------------------
% Known functions
%

assert_known( ?rs, ?olist, ?v, ?c ) :-
   obj_parts(?olist, ?obj, ?props),
% let's quietly do nothing instead, let's findall work when there's a
% catchall rule like .ok[?i] = no as a final rule.  it can call the rule
% again for the unbound variable cases.
%   ( not ground(?obj) ->
%      throw( execution_error(unbound_object(?rs, ?olist)) )      
%      ;
%      true ),
	(ground(?obj) -> asserta( known(?rs, ?obj, ?props, ?v, ?c) ) ; true).
	
assert_known( ?rs, ?obj, ?props, ?v, ?c ) :-
%   ( not ground(?obj) ->
%      obj_parts(?olist, ?obj, ?props),
%      throw( execution_error(unbound_object(?rs, ?olist)) )      
%      ;
%      true ),
	(ground(?obj) -> asserta( known(?rs, ?obj, ?props, ?v, ?c) ) ; true).

/*
assert_known( known( ?rs, ?o, ?v, ?c ) ) :-
   obj_parts(?o, ?obj, ?props),
	assert( known(?rs, ?obj, ?props, ?v, ?c) ).
*/

retract_known( known( ?rs, ?o, _, _) ) :-
   obj_parts(?o, ?obj, ?props),
   known(?rs, ?obj, ?props, _, _),
   retract( known(?rs, ?obj, ?props, _, _) ),
   fail.
retract_known( _ ).

abolish_known :- abolish(known/5).

get_known( ?m, ?o, ?v, ?c ) :-
   obj_parts(?o, ?obj, ?props),
   known(?m, ?obj, ?props, ?v, ?c).

% only used for displaying test results from test.pro
get_known_string(?m, ?os, ?v, ?c) :-
   get_known(?m, ?o, ?v, ?c),
   mobject_string(?o, ?os).

%--------------------------------------------
% Object functions
%

obj_parts(obj([?obj|?props]), ?obj, ?props).

% Make a nice string out of an object.

mobject_string( find(?m, ?o, _), ?os ) :- mobject_string(?m, ?o, ?os).
mobject_string( finda(?m, ?o, _), ?os ) :- mobject_string(?m, ?o, ?os).
mobject_string( vector(_, ?o, _), ?os ) :- mobject_string(_, ?o, ?os).
mobject_string( ?x, ?xs ) :- string_termq(?xs, ?x).

mobject_string(?m, obj(?ol), ?os) :-
   mobj_string( om(?m, obj(?ol)), ?os ),
   !.
mobject_string(_, om(?m, ?o), ?os) :-
   mobj_string( om(?m, ?o), ?os ),
   !.
mobject_string(?m, ?o, ?os) :-
   string_term(?os, ?o).

mobject_string(?m, ?o, ?p, ?os) :-
   obj_parts(?op, ?o, ?p),
   mobj_string( om(?m, ?op), ?os ).

% Reverse parse the object to get the string to display
mobj_string( om(?m, ?o), ?os) :-
   catch(
     ( p_parse_object(?m, om(?m, ?o), ?ol, []),
       t_tokenize(?os, ?ol) ),
     _,
     fail ).

object_string(obj(?o), ?os) :-
   catch(
     ( p_object_list(?m, obj(?o), ?ol, []),
       t_tokenize(?os, ?ol) ),
     _,
     fail ).
     
object_string(?o, ?p, ?os) :-
   obj_parts(?op, ?o, ?p),
   catch(
     ( p_object_list(?m, ?op, ?ol, []),
       t_tokenize(?os, ?ol) ),
     _,
     fail ).
   
%----mobject_string--------------------------------
% Parameter utilities
%
% Some parms get called a lot, and get special treament
% so instead of get_parm(trace, ?v) one can say
% trace_flag.
%

set_parm(trace, ?val) :-
   ( ?val = on ->
      ( trace_flag -> true ; assert(trace_flag) )
      ;
      ( trace_flag -> abolish(trace_flag) ; true ) ),
   !.
set_parm(?parm, ?val) :-
   retractall( parm(?parm, _) ),
   assert( parm(?parm, ?val) ).

get_parm(trace, ?val) :-
   ( trace_flag -> ?val = on ; ?val = off ),
   !.
get_parm(?parm, ?val) :-
   parm(?parm, ?val).

delete_parm(?parm) :-
   retractall( parm(?parm, _) ).

%parm(explain, off).  % default

%---------------------------------------------------------
% translate old parser to new
%

% pup_test(".weekend when day_type(.call.date.day, weekend)").
% pup_test("day_type(?day, weekend) when ?day = saturday or ?day = sunday").
% pup_test(".a = ?x").
% pup_test("day_type(?day, 'weekday') when ?day = member([monday, tuesday, wednesday, thursday, friday])").
% pup_test("when").
% pup_test(".age = 1 year 12 days").
% pup_test(".call.rate = 0.05
%   when
%   fourDays!.weekend").
% pup_test("inherit from five").
% pup_test("get_av(?a, ?v, ?l)  when mimber(av(?a,?v), ?l)").
% pup_test("mimber(?a, [? | ?z]) WHEN mimber(?a, ?z)").
% pup_test("#.rate = 0.05 WHEN .weekend").
% pup_test(".age = .today - .birthday + MEMBER([a,b])").
% pup_test("find in r19a .a").
% pup_test(".row[?i] = .pl[?i, *]").
% pup_test(".status[?v, ?n, ?d1, ?d2] = [X, 'Before min. interval'] WHEN
%   ?n > 1 AND
%   ?d2 < ?d1").
% pup_test(".status[?v, ?n, ?d1, ?d2] = [X, 'Before min. interval'] WHEN
%   ?v ! .table[?n, 'Minimum Interval']").
% pup_test(".status[?v, ?n, ?d1, ?d2] = [X, 'Before min. interval'] WHEN
%   ?n > 1 AND
%   ?d2 < ?d1 + ?v ! .table[?n, 'Minimum Interval']").
% pup_test(".status[?v, ?n, ?d1, ?d2] = [X, 'Before min. interval'] WHEN
%   ?n = ?d1 + ?v ! .table[?n, 'Minimum Interval']").
% pup_test(".status[?v, ?n, ?d1, ?d2] = 2 WHEN ?d1 > ?d1 + ?v ! .table[?n, 'Minimum Interval']").
% pup_test(".status[?v, ?n, ?d1, ?d2] = 2 WHEN ?d1 > ?d1 + 3").
% pup_test(".status[?v, ?n, ?d1, ?d2] = 2 WHEN ?d1 > 3 + 2").
% pup_test(".bigger[?x, ?y] = true WHEN ?x > ?y + 2").
% pup_test(".row[?i] = FINDALL(?v, mmr32 ! .table[?i, ?x] = ?v)").
% pup_test(".duh when is_ruleset(r35)").
% pup_test(["yes", "*", " 0.05"]).
% pup_test("*").
% pup_test(".ok[?i] = yes WHEN .large[?i] = yes AND .loans[?i, FICO] > 10").
% pup_test(".ok[?i] = yes WHEN .large[?i] <> yes AND .loans[?i, LTV] > 10").
% pup_test(".a = 2 WHEN .b <> 3").
% pup_test("=RTable(`.animals[poop]`, true)").
% pup_test("=RQuery(Animals, `FIND .animal[3]`, This!D3)").
% pup_test("=RQuery(Animals, `FIND .animal[_1]`, This!D3").
% pup_test(`=RCell( ".animals", C3)`).
% pup_test(`= RQuery(PriceBasic,"FIND .unitprice when .quantity = 12")`).
% pup_test(`=RQuery(ObjectRules, "FIND .license.addenda[1] when .person.age = _1 and .flying.purpose = _2 and .flying.in_weather = _3 and .person.vision = _4 and .plane.engines = _5 and .plane.landing_gear = _6 and .plane.propeller_pitch = _7", B19, B20, B21, B22, B23, B24, B25)`).
% pup_test(`= B10 * RQuery(PriceLinked,"FIND .unitprice WHEN .quantity = _1",B10 )`).
% pup_test(`=RQuery(PriceDates, "FIND .baseprice WHEN .birthday = _1 AND .expiration = _2 AND .quantity = _3", D3, D4, D5)`).
% pup_test(`=RQuery(PriceDates2, "FIND .baseprice WHEN .birthday = _1 AND .expiration = _2 AND .quantity = _3", D3, D4, D5)`).
% pup_test(`=RQuery(LoanInput2, "find .Status WHEN .Name = _1", A21)`).
% pup_test(`=RQuery(TableRules, "FIND .data[Revenues, Q1]")`).
% pup_test(`= RQuery(SmallClaimsRules," find .Advice" )`).
% pup_test(`= 3 & RQuery(aaa," find .a when .d = 5 and .e = 3" )`).
% pup_test(`= " bbb" & RQuery(aaa," find .a when .d = 5 and .e = 3" )`).
% pup_test(`.status = ?v:.status WHEN ?v = .vaccine`).
pup_test(`.age = today - .birthdate`).
% pup_test(`=RXLDependency(MMR,DTAP,OPV )`).

% pup_test(`.tag[?tag, ?string] = "<" & ?tag & ">" & ?string & "</" & ?tag & ">"`).
% pup_test(`.tag = "<"`).
% pup_test(`.indicator_abbreviation[include] = "IN"`).

/*
 pup_test(`.vaccinations = SORT( FINDALL( [?date, ?vaccine],
   ( .raw_vaccination[?i, "vaccinationdate"] = ?date AND
     .raw_vaccination[?i, "vaccination"] = ?vname AND
     ?vname <> * AND
     .real_name[?vname] = ?vaccine  ) ))`).

 pup_test(`mmr_components{[ [?v, ?s, ?d, ?c, ?n] | ?gvs], ?a, ?vstr} WHEN
   member(?v, ["Measles", "Mumps", "Rubella"]) AND
   ! AND mmr_components{ ?gvs, [ [?v, ?s, ?d, ?c, ?n] | ?a], ?vstr}`).
 pup_test(`fit{ [ ?e | ?nes ], ?max, ?x, ?len, [ ?ex | ?nes ] } WHEN
   list:length{?e, ?elen} AND
   ?elen + 1 + ?len =< ?max AND
   list:append(?e, [""| ?x], ?ex) `).
 pup_test(`.vl2_history = FINDALL( ?msg,
   ( [?antigen, ?dose, ?date, ?status, ?comment, ?age] = MEMBER( .history ) AND
     [?date, ?vaccine] = MEMBER( .vaccinations ) AND
     CommonRules:.dotcontains[ ?vaccine, ?antigen ] AND
     ?mdyDate = FORMAT_DATE(?date, 'mm/dd/yyyy') AND
     ?agelist = AGE_TO_LISTSTRING( ?age ) AND
    .original_name[?date, ?vaccine] = ?raw_name AND
     ?msg = ?antigen & ";" & ?dose & ";" & ?raw_name & ";" & ?mdyDate & ";" & ?agelist & ";" & ?status & ";" & ?comment ) )`).
 pup_test(`vaccineoops = FINDALL( ?r, (
   raw_vaccination[?i, "vaccination"] = ?v AND
   ?v <> * AND
   real_name[ ?v ] = "Unknown" AND
   ?r = RANGE( raw_vaccination[?i, "vaccination"] )
   ))`).
*/

% pup_test(``).
% pup_test(``).
% pup_test(``).


pupr :-
   pup_test(?tst),
 write('in: '), writeq(?tst), nl, nl,
   pup_text(?tst, ?txt),
 write('out: '), writeq(?txt), nl, nl,
   tokenizer2:tokenize(?txt, ?tokens),
   parser2:parse_rule(rs, a1, ?rule, ?tokens, []),
 writeq(?rule), nl.
 
pupt :-
   pup_test(?tst),
 write('in: '), writeq(?tst), nl, nl,
   pup_text(?tst, ?txt),
 write('out: '), writeq(?txt), nl, nl.
 
pupq :-
   pup_test(?tst),
 write('in: '), writeq(?tst), nl, nl,
   pupq_text(?tst, ?txt),
 write('out: '), writeq(?txt), nl, nl.
 
pupqs :-
   pup_test(?tst),
   write('in: '), writeq(?tst), nl,
   ( pupq_text(?tst, ?txt) ->
       write('out: '), writeq(?txt), nl, nl
       ;
       write('failed'), nl, nl ),
   fail.
pupqs.

% we know its an rquery
pupq_text(?oldString, ?newString) :-
   tokenizer1:tokenize_wh(?oldString, ?oldTokens, ?Vars),
% write('old: '), writeq(?oldTokens), nl, nl,
% write('var: '), writeq(?Vars), nl, nl,
   q_trans(?nestedNewTokens, ?oldTokens, []),
   flatten(?nestedNewTokens, ?newTokens),
% write('new: '), writeq(?newTokens), nl, nl,
   tokenizer2:tokenize(?newString, ?newTokens, ?Vars),
   !.

% anything but an rquery
pup_text([], []) :- !.
pup_text([?os | ?oss], [?ns | ?nss]) :-
   pup_text(?os, ?ns),
   !, pup_text(?oss, ?nss).
pup_text(?oldString, ?newString) :-
   tokenizer1:tokenize_wh(?oldString, ?oldTokens, ?Vars),
% write('old: '), writeq(?oldTokens), nl, nl,
% write('var: '), writeq(?Vars), nl, nl,
   pup_trans(?nestedNewTokens, ?oldTokens, []),
   flatten(?nestedNewTokens, ?newTokens),
% write('new: '), writeq(?newTokens), nl, nl,
   tokenizer2:tokenize(?newString, ?newTokens, ?Vars),
   !.

% we were already told this has an RQuery in it somewhere

q_trans( [?beforeRQ | ?args] ) -->
   q_beforeRQ(?beforeRQ),
   q_args(?args).

q_beforeRQ( [RQuery] ) -->
   [RQuery],
   !.
q_beforeRQ( [?x | ?xs] ) -->
   [?x],
   !, q_beforeRQ( ?xs ).

q_args( [?first | ?find] ) -->
   q_first(?first),
   q_find(?find).

q_first( [pm(',')] ) -->
   [pm(',')],
   !.
q_first( [?x | ?xs] ) -->
   [?x],
   !, q_first(?xs).
   
q_find( [wh(?w), text(?findout) | ?rest] ) -->
   [wh(?w)],
   [?findin],
   { pup_text(?findin, ?findout) },
   !,
   rest_ok(?rest).
q_find( [text(?findout) | ?rest] ) -->
   [?findin],
   { pup_text(?findin, ?findout) },
   !,
   rest_ok(?rest).


pup_trans( [=, RArray, pm('('), text(?o) | ?rest] ) -->
   [=], whig, [RTable, pm('(')], whig, [?os],
   !,
   { pup_text(?os, ?o) },
   rest_ok(?rest).
pup_trans( [=, RCell, pm('('), text(?o) | ?rest] ) -->
   [=], whig, [RCell, pm('(')], whig, [?os],
   !,
   { pup_text(?os, ?o) },
   rest_ok(?rest).
pup_trans( [=, RXLDependency | ?rest] ) -->
   [=], whig, [RXLDependency],
   !,
   rest_ok(?rest).
pup_trans( [=, RXLDependency | ?rest] ) -->
   [=], whig, [LoadRules],
   !,
   rest_ok(?rest).
pup_trans( [?y | ?ys ] ) -->
   pup_tran(?y),
   !,
   pup_trans(?ys).
pup_trans( [] ) --> [].

whig --> [wh(_)].
whig --> [].

rest_ok( [] ) -->
   [].
rest_ok( [?ok | ?oks] ) -->
   [?ok],
   !, rest_ok(?oks).

% a rule variable
pup_tran(?o ) -->
   pup_object(?o),
   !.
/* catch in tokenizer2 instead
% a special clause
pup_tran([?inherit, ?from, ?a]) -->
   [?inherit],
   { atom(?inherit), lookup(?inherit, INHERIT) },
   [?from],
   { atom(?from), lookup(?from, FROM) },
   [?a],
   { atom(?a) },
   !. */
% text which is a value: "Saturday"
pup_tran(text(?a)) -->
   [?a],
   { string(?a) },
   !.   
% a special phrase
pup_tran([?in, wh(?wh), ?rs]) -->
   [?in],
   { atom(?in), lookup(?in, IN) },
   [wh(?wh)],
   [?rs],
   { atom(?rs) },
   !.
% a special phrase
pup_tran([?using, wh(?wh), ?rs]) -->
   [?using],
   { atom(?using), lookup(?using, USING) },
   [wh(?wh)],
   [?rs],
   { atom(?rs) },
   !.
% a reserved word date operator: years, day  (see list below)
pup_tran(?a) -->
   [?a],
   { ( atom(?a), lookup(?a, ?word), dateop(?word) ) },
   !.
% an old special word now a function with no arguments: today, now, call_stack (see list below)
pup_tran([?a, pm('('), pm(')')]) -->
   [?a],
   { ( atom(?a), lookup(?a, ?word), special(?word) ) },
   !.
% start of a function: MEMBER(
pup_tran([?a, pm('('), ?args]) -->
   [?a],
   { atom(?a) },
   [pm('(')],
   { lookup(?a, _) },
   pup_args( pm(')'), ?args),
   !.
% a reserved word not a function: WHEN, OR  (see list below)
pup_tran(reserved(?word)) -->
   [?a],
   { ( atom(?a), lookup(?a, ?word), reserved(?word) ) },
   !.
% start of a predicate: history( becomes history{
pup_tran([?a, pm('{'), ?args]) -->
   [?a],
   { atom(?a) },
   [pm('(')],
   pup_args( pm('}'), ?args),
   !.
% module qualification: one : 
pup_tran([?a, ':']) -->
   [?a],
   { atom(?a) },
   ( [':']; [pm('!')] ),
   !.
% module qualification: one : 
pup_tran([?a, wh(?w), ':']) -->
   [?a],
   { atom(?a) },
   [wh(?w)],
   ( [':']; [pm('!')] ),
   !.
% an operator: =  >=
pup_tran(?o) -->
   [?o],
   { (current_op(_, _, ?o)) },
   !.
% a pseudo operator: <= <>
pup_tran(<=) -->
   [<=],
   !.
% a pseudo operator: <= <>
pup_tran(<>) -->
   [<>],
   !.
% an indirect operator:  QUAND, year (becomes years)
pup_tran(?o) -->
   [?a],
   { (atom(?a), lookup(?a, ?o), current_op(_, _, ?o)) },
   !.
% an ambiguous operator, also a function: year
pup_tran(?a) -->
   [?a],
   { ( atom(?a), member(?a, [year, month, day]) ) },
   !.
% text which is a value: "Saturday"
pup_tran(text(?a)) -->
   [?a],
   { atom(?a) },
   !.
% anything else that doesn't need translation: pm('(')  0.45
pup_tran(?y) -->
   [?y],
   !.

reserved(FIND).
reserved(WHEN).
reserved(THEN).
reserved(OR).
reserved(AND).
reserved(IF).
reserved(NOT).
reserved(INHERIT).
reserved(FROM).
reserved(TABLE).
reserved(COLLECT).
reserved(FOREACH).
reserved(WHERE).

special(today).
special(now).
special(call_stack).

dateop(years).
dateop(days).
dateop(months).
dateop(hours).
dateop(minutes).
dateop(weeks).
dateop(seconds).

pup_object([?o, ?as | ?ps]) -->
   ([pm('.')] ; [pm(' .')]),
   [?o],
   { atom(?o) },
   pup_attrs(?as),
   pup_properties(?ps),
   !.

pup_properties([pm(.), ?a | ?ps]) -->
   ([pm('.')] ; [pm(' .')]),
   [?a],
   { atom(?a) },
   !,
   pup_properties(?ps).
pup_properties([]) --> [].

pup_attrs( [ pm('['), ?args ] ) -->
   [ pm('[') ],
   pup_args( pm(']'), ?args),
   !.
pup_attrs( [] ) --> [].

pup_args(?close, ?a) -->
   pup_arg(?close, ?a, done),
   !.
pup_args(?close, [ ?a | ?as ]) -->
   pup_arg(?close, ?a, more),
   !,
   pup_args(?close, ?as).

pup_arg(?close, [?close], done) -->
   [?close],
   !.
pup_arg(pm('}'), [pm('}')], done) -->
   [pm(')')],
   !.
pup_arg(?close, [pm(',')], more) -->
   [pm(',')],
   !.
pup_arg(?close, [?y|?ys], ?more) -->
   pup_tran(?y),
   !,
   pup_arg(?close, ?ys, ?more).
   