%--------------------------------------------------------
% ARules Decision Table Support
%

:- import(list).
%:- import(tokenizer).
%:- import(parser).

:- include('opdefs.pro').

%-------------------------------------------------------
% predicates called from VBA
%

define_decision_table(?RS, ?TableDefStr) :-
	dtable_clear,
%   showx( ?RS: ?TableDefStr ),
   string_tokens(?TableDefStr, [_ | ?Headers], ""),
   catch( check_headers(?Headers), ?E, deal_with(?E) ),
   assert( dtable_outputs(?Headers) ),
   !.

decision_table_headers(?RS, ?headers) :-
%   showx( ?RS: ?Headers ),
   dtable_outputs(?outs),
   assert( dtable_headers(?headers) ),
   catch( header_inputs(?outs, ?headers, ?ins),
     ?error,
     ( assert( dtable_inputs(?ins) ), deal_with(?error) )),
   assert( dtable_inputs(?ins) ).

decision_table_row(?RS, ?Cell, ?Row) :-
%   showx( ?RS: ?Row ),
   dtable_outputs(?Outs),
   dtable_headers(?Heads),
   catch(
      ( get_decision_rule(?Heads, ?Row, ?Outs, [], [], ?RuleStr),
        add_rule(?RS, ?Cell, ?RuleStr) ),
      ?error,
      deal_with(?error) ).

%---------------------------------------------------------------

dtable_clear :-
   retractall( dtable_outputs(_) ),
   retractall( dtable_inputs(_) ),
   retractall( dtable_headers(_) ).

header_inputs([], ?ins, ?ins) :- !.
header_inputs([?out | ?outs], ?headers, ?ins) :-
   remove(?out, ?headers, ?headsleft),
   !,
   header_inputs(?outs, ?headsleft, ?ins).
header_inputs(?outs, _, _) :-
	get_text(inputs_not_in_headers(?outs), ?msg),
   throw( parse_error( ?msg ) ).

% get_rule( ?Heads, ?Row, ?Outs, ?ActionStrs, ?ConditionsStrs )

get_decision_rule( [], _, _, ?ActionStrs, ?ConditionStrs, ?RuleStr ) :-
   remove_last(?ActionStrs, ?Acts),
   remove_last(?ConditionStrs, ?Conds),
   ( ?Conds = [] ->
      ?RuleBits = ?Acts
      ;
      append(?Acts, [" WHEN " | ?Conds], ?RuleBits) ),
   stringlist_concat(?RuleBits, ?RuleStr),
   !.

get_decision_rule( [?head | ?heads], [?item | ?items], ?Outs, ?Acts, ?Conds, ?RuleStr ) :-
   member( ?head, ?Outs ),
   make_out(?head, ?item, ?action),
   !,
   get_decision_rule( ?heads, ?items, ?Outs, [?action, " AND " | ?Acts], ?Conds, ?RuleStr ).
get_decision_rule([?head | ?heads], ["*" | ?items], ?Outs, ?Acts, ?Conds, ?RuleStr ) :-
   !,
   get_decision_rule( ?heads, ?items, ?Outs, ?Acts, ?Conds, ?RuleStr ).
get_decision_rule([?head | ?heads], [?item | ?items], ?Outs, ?Acts, ?Conds, ?RuleStr ) :-
	make_in(?head, ?item, ?condition),
   !,
   get_decision_rule( ?heads, ?items, ?Outs, ?Acts, [?condition, " AND " | ?Conds], ?RuleStr ).

make_out(?head, ?item, ?action) :-
   stringlist_concat([?head, " = ", ?item], ?action).

make_in(?head, ?item, ?condition) :-
   t_tokenize(?item, ?tokens),
   make_in(?tokens, ?head, ?item, ?condition),
   !.
   
make_in([?not | ?toks], ?head, ?item, ?negative) :-
   keyword(NOT, [?not], []),
   t_tokenize(?pos_item, ?toks),
   make_in(?toks, ?head, ?pos_item, ?positive),
   stringlist_concat([?not, " (", ?positive, ") "], ?negative),
   !.
make_in(?toks, ?head, ?item, ?condition) :-
   (?conj = OR; get_text(OR, ?conj); ?conj = AND; get_text(AND, ?conj)),
   split(?toks, ?conj, ?left, ?right),
   !,
   t_tokenize(?left_item, ?left),
   make_in(?left, ?head, ?left_item, ?left_condition),
   t_tokenize(?right_item, ?right),
   make_in(?right, ?head, ?right_item, ?right_condition),
   stringlist_concat([ ?left_condition, " ", ?conj, " ", ?right_condition ], ?condition),
   !.
make_in([?op | ?toks], ?head, ?item, ?condition) :-
   p_compare_operator( _, [?op], []),
   stringlist_concat([?head, " ", ?item], ?condition),
   !.
make_in(_, ?head, ?item, ?condition) :-
   stringlist_concat([?head, " = ", ?item], ?condition),
   !.

split(?toks, ?tok, ?left, ?right) :-
   append(?left, [?tok | ?right], ?toks),
   list(?left),
   list(?right).

remove_last([], []) :- !.
remove_last([_], []) :- !.
remove_last([?x | ?xs], [?x | ?ys]) :-
   !, remove_last(?xs, ?ys).

has_op(?item) :-
   string_tokens(?item, [?op | _]),
   member( ?op, ['>', '<'] ).

%---------------------------------------
% used for testing from check rules
%

% this one called from above when defining a table
check_headers([]).
check_headers([?h | ?hs]) :-
   string_term(?hstr, ?h),
   check_dthead(?hstr),
   !, check_headers(?hs).

check_dtable_header(?h) :-
	catch(
	   check_dthead(?h),
	   ?error,
	   deal_with(?error) ).

check_dthead(?h) :-
   t_tokenize(?h, ?htoks),
   p_parse_object(m, _, ?htoks, []),
   !.
check_dthead(?h) :-
	(sub_string(?h, 1, 1, ".") ->
	   get_text(invalid_dtable_header(?h), ?txt)
	   ;
	   get_text(dtable_header_missing_dot(?h), ?txt) ),
   throw( parse_error(?txt) ).

% called from checkarule when we don't know the input/output
% property of the headers
check_dtable_entry(?v) :-
	catch(
	   check_dtinput(?v),
	   ?error,
	   deal_with(?error) ).

% called when we know the column and have the headers
check_dtable_entry(?k, ?v) :-
	is_input_entry(?k),
	!,
	catch(
	   check_dtinput(?v),
	   ?error,
	   deal_with(?error) ).
check_dtable_entry(?k, ?v) :-
	catch(
	   check_dtoutput(?v),
	   ?error,
	   deal_with(?error) ).

is_input_entry(?k) :-
   dtable_headers(?headers),
   nth_elem(?headers, ?h, ?k),
   dtable_inputs(?inputs),
   member(?h, ?inputs),
   !.

check_dtinput("*") :-
   !.
check_dtinput(?h) :-
   make_in("x", ?h, ?cond),
   t_tokenize(?cond, ?toks),
   p_parse_goals(m, _, ?toks, []),
   !.
check_dtinput(?h) :-
	get_text(invalid_dtable_input(?h), ?msg),
   throw(parse_error(?msg)).
      
check_dtoutput(?h) :-
   make_out("x", ?h, ?cond),
   t_tokenize(?cond, ?toks),
   p_parse_goal(m, _, ?toks, []),
   !.
check_dtoutput(?h) :-
	get_text(invalid_dtable_output(?h), ?msg),
   throw(parse_error(?msg)).
   

%---------------------------------------
% internal testing
%

touts( [".daytime"] ).
theads( [".day", ".daytime"] ).
trow( ["*", "yes"] ).

test_drule :-
   touts( ?outs ),
   theads( ?heads ),
   trow( ?row ),
   get_decision_rule( ?heads, ?row, ?outs, [], [], ?rulestr ),
   write( ?rulestr ),
   nl.
   
andor(?x) :- ?x > 3, ?x < 10; ?x > 20, ?x < 30.
