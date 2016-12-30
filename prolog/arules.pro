% ARulesXL
% Copyright(c) 2009 Amzi! inc.  All rights reserved
%
% Extended Predicates defined in VBA:
%
%  xl_msgbox( ?MessageText )
%  xl_inputbox( ?Prompt, ?BoxTitle, ?DefaultAnswer, ?Response )
%  xl_cell( ?Address, ?Value )   - retrieves a value from a cell by address
%  xl_cell( ?Row, ?Col, ?Value ) - retrieves a value from a cell by row,col
%  xl_cell( ?Range, ?RowOffset, ?ColOffset, ?Value) - retrieve a cell by offset
%  xl_range_size( ?Range, ?Rows, ?Cols ) - get size of a range
%
%
% Interface Predicates called from VBA
%
%  assert_clause( ?LogicClauseString ) - assert logic clause, predicate 'abolished' first
%  logic_query( ?QueryString, ?Answer ) - query the logic base
%
%  add_rule( ?RuleID, ?RuleString ) - assert a new rule by ID, replace old one
%  query_rules( ?QueryString, ?Answer ) - query the arules rule set
%
% Interface Predicates usable by rules
%  ask( ?Prompt, ?Title, ?Default, ?Response ) - uses xl_inputbox/4
%

:- import(list).
:- import(date_time).
:- import(amzi_xml).
:- include('opdefs.pro').

%:- import(tokenizer).
%:- import(parser).
:- import(rename).

:- dynamic parm/2.
% this indexing seems faster than having 1st two indexed
:- indexed data(0,1,0,0,0).
% :- sorted data/5.  % goes a little faster for VL, but might give different results
:- indexed known(0,1,0,0,0).
% :- sorted known/5.
:- sorted stored_query/4.

% arules_release( [`4`, `0`, `1`, `Beta`] ).
arules_release( [`5`, `0`, `1`] ).

/*
beta_expires( date(2009,6,15) ).

beta_expired :-
   arules_release( [_, _, _, `Beta`] ),
   date_get(today, ?today),
   beta_expires( ?expires ),
   date_compare( ?today, >, ?expires ).
*/
   
arules_release_text( ?text ) :-
   arules_release( ?list ),
   stringlist_concat( ?list, `-`, ?text ).

arules_title(`ARulesXL`).
arules_long_title(?msg) :- get_text(arules_long_title, ?msg).
arules_short_title(?msg) :- get_text(arules_short_title, ?msg).
arules_build(?msg) :- arules_release_text(?t), get_text(version( ?t ), ?msg).
arules_copyright(?msg) :- get_text(copyright, ?msg).
arules_website(`www.arulesxl.com`).
arules_version(?v) :-
   arules_long_title(?t),
   arules_build(?b),
   arules_copyright(?c),
   arules_website(?w),
   stringlist_concat([?t,?b,?c,?w], `\n`, ?v).
arules_edition(?e) :-
   get_text(edition, ?msg),
   license_type(?l),
   stringlist_concat([?l,?msg], ` `, ?e).
   
%-----------------------------------------------
% Security checks
%

% GEMA86 only
%check_maintenance :- !.

check_maintenance :- !.    % nah, let's not bother the people.
check_maintenance :-
   get_parm(maintenance_message, done),
   !.
check_maintenance :-
	(license_type("Trial") ; license_type("Runtime") ),
	set_parm(maintenance_message, done),
	!.
check_maintenance :-
   amzi_system:license$info(maint_days_left, ?MDL),
   maintenance_action(?MDL),
   set_parm(maintenance_message, done).

maintenance_action(?MDL) :-
  ?MDL > 30,
  !.
maintenance_action(?MDL) :-
   get_text(maintenance_expires(?MDL), ?msg),
   message(?msg),
   !.

check_license :- !.  % Not doing that anymore.  dcm
check_license :-
   regression_testing(true),  % asserted by test.pro
   !.
check_license :-
%  Uncomment this and ARULESXL_CUSTOM_GEMA86 in keys.cpp
%  Fix the list of rule set names in keys.cpp
%  Also fix version number below
%  And maintenance warning above
%   check_custom_gema86,
   license_type(?t),
   % Runtime license does not need renewal
   ( amzi_system:license$info(version_gt_renew_date, false) ;
     ?t == "Runtime" ),
   %amzi_system:license$info(arulesxl, true),
   check_license_type(?t),
   !.
%check_license :-
%   license_type("Internal Developer").

% Custom xpl file checks
% For GEMA86 make sure we have the arules.dll that checks the rule set names
check_custom_gema86 :-
   version_build(76, 7, _).
   
% eliminate the need for runtime license checks.
check_license_type("Runtime") :- !.
% Have we either entered a key from the web, or validated the author key
% in the axl file?
check_license_type("Runtime") :-
   % Older axl files may not have a key in them, yet may be able to run
   % for example, modl
   amzi_system:license$info(is_runtime_registered, _),
%   tell('c:/UDFs/arules.log'), write('registered'), told,
   !.
check_license_type("Runtime") :-
   arulesxl_user_license(?authorkey),
   amzi_system:license$info(is_runtime_registered, ?authorkey),
%   tell('c:/UDFs/arules.log'), write('registered'), told,
   !.
% Have we run out of attempts and days to get a key from the web?
% If so, put up Armadillo's key box
check_license_type("Runtime") :-
%   tell('c:/UDFs/arules.log'), write('not registered enter key'), told,
   arulesxl_user_license(?authorkey),
   amzi_system:license$info(is_runtime_expired, ?authorkey),
   !,
   amzi_system:enter$licensekey.
% Otherwise, try again to get a key from the web
check_license_type("Runtime") :-
%   tell('c:/UDFs/arules.log'), write('not registered register web'), told,
   !,
   arulesxl_user_license(?authorkey),
%?authorkey = `000014-JMFYK6-F31G5N-GGKCFM-VDHK60-P6YCGQ-TBG206-PTC30M-3Z6DWK-FHD8KF`,
   amzi_system:license$info(runtime_limit, ?count),
   get_text(license_message(?count), ?message),
   get_text(license_success, ?success),
   get_text(license_failure_cannot_connect, ?failure_connect),
   get_text(license_failure_author, ?failure_author),
   (amzi_system:register$runtime(?authorkey, ?message, ?success, ?failure_connect, ?failure_author)
   ; true).
check_license_type(_) :-
%   tell('c:/UDFs/arules.log'), write('not registered last clause'), told,
   true.

% Returns string: Trial, Runtime, Developer or Professional
license_type("Professional") :- !.    % FIX THIS!
license_type(?t) :-
   amzi_system:license$info(product_name_str, ?t).
license_type("Internal Developer") :-
   product_name("Prof Internal").
   
user_info(?un, '', '') :-
   user_name(?un),
   !.
% this was the old way..
user_info(?un, ?hf, ?md) :-
   user_name(?un),
   hardware_fingerprint(?hf),
   maintenance_days(?md),
   !.
   
user_name(?un) :-
	amzi_system:license$info(user_name_str, ?un),
	!.
user_name(?msg) :-
   get_next(no_user_name, ?msg).

product_name(?pt) :-
	amzi_system:license$info(product_name_str, ?pt).

hardware_fingerprint(?hf) :-
	amzi_system:license$info(pc_fingerprint_str, ?hf),
	!.
hardware_fingerprint(?msg) :-
   get_text(no_hardware_fingerprint, ?msg).

maintenance_days(``) :-
	( license_type(`Trial`) ; license_type(`Runtime`) ),
	!.
maintenance_days(?md) :-
   amzi_system:license$info(maint_days_left, ?mdn),
   string_term(?md, ?mdn),
   !.
maintenance_days(?msg) :-
   get_text(no_maintenance_days, ?msg).


%-----------------------------------------------
% User interface
%

% country code is set in VB ARulesXL startup

primary_language(?l) :- get_parm(UserLanguage, ?l), !.
primary_language(?l) :- get_parm(CountryCode, ?cc), country_code(?cc, ?l).

default_language(English) :- primary_language(?l), ?l == English, !, fail.
default_language(English).

language(?l) :- primary_language(?l).
language(?l) :- default_language(?l).

% used by tokenizer which is looking at characters
decimal(English, 0'.).
decimal(French, 0',).

/*
decimal_point( ?x ) :-
   language(?lang),
   decimal(?lang, ?x).
*/

% decimals have to be one or the other.
decimal_point( 0'. ) :- cntr_get(3, 1), !.
decimal_point( 0', ) :- cntr_get(3, 2), !.
decimal_point( ?x ) :-
   cntr_get(3, 0),
   determine_language,
   !, decimal_point( ?x ).

argument_delimiter(English, ',').
argument_delimiter(French, ';').

/*
argument_delimiter( ?x ) :-
   language(?lang),
   argument_delimiter(?lang, ?x).
*/

% delimiters can be either english or french
argument_delimiter( ',' ) :- cntr_get(3, 1), !.
argument_delimiter( ';' ) :- cntr_get(3, 2), !.
argument_delimiter( ',' ) :- cntr_get(3, 2), !.
argument_delimiter( ?x ) :-
   cntr_get(3, 0),
   determine_language,
   !, argument_delimiter( ?x ).

country_code(1, English).
country_code(33, French).

language_module(English, arxlEnglish).
language_module(French, arxlFrench).

language_module(?m) :-
   language(?lang),
   language_module(?lang, ?m).

% See if the user put in an explicit language statement
set_language(English) :- cntr_set(3, 1), set_parm(UserLanguage, English), !.
set_language(French) :- cntr_set(3, 2), set_parm(UserLanguage, French), !.
set_language(?x) :- cntr_set(3, 1), set_parm(UserLanguage, English).

/*
set_language(?rs) :-
   catch(
      find(?rs, obj([RuleSetLocale(language)]), ?lang),
      _,
      fail ),
   set_parm(UserLanguage, ?lang),
   ( ?lang = English -> cntr_set(3, 1) ; cntr_set(3, 2) ),  % assert(known_language(?l)),
   !.
set_language(_) :-
   delete_parm(UserLanguage).
*/

%lookup(?trans, ?word) :- arxlEnglish:dict(?trans, ?word), !.
% Note that language lookup has gotten a bit convuluted for performance
% reasons.  It uses the counter 3 to remember a value.  After rules are
% read in, the counter might be changed as well in the situation where
% an explicit rule sets the language.
lookup(?trans, ?word) :-
   cntr_get(3, 1), % known_language(English),
   !,
   arxlEnglish:dict(?trans, ?word),
   !.
lookup(?trans, ?word) :-
   cntr_get(3, 2), % known_language(French),
   !,
   (arxlFrench:dict(?trans, ?word) -> true ; arxlEnglish:dict(?trans, ?word)),
   !.
lookup(?trans, ?word) :-
   cntr_get(3, 0),
   determine_language,
   !, lookup(?trans, ?word).

determine_language :-
   language(?l),
   ( ?l = English -> cntr_set(3, 1) ; cntr_set(3, 2) ),  % assert(known_language(?l)),
   !.

/*
get_text(?msg, ?text) :-
   language_module(?lm),
   call( ?lm : text(?msg, ?list) ),
   eval_text(?list, ?text),
   !.
*/
get_text(?msg, ?text) :-
   cntr_get(3, 2),
   arxlFrench : text(?msg, ?list),
   eval_text(?list, ?text),
   !.
get_text(?msg, ?text) :-
   arxlEnglish : text(?msg, ?list),
   eval_text(?list, ?text),
   !.
get_text(?msg, ?msg) :-
 	string(?msg),
 	!.
get_text(?msg, ?txt) :-
   string_termq(?txt, ?msg),
   !.

%-----------------------------------------------------------------------
% Export compile
%

arxl_init_export(?tempfile) :-
    license_type(`Professional`),
    %set_prolog_flag(string_esc, off),
    tell(?tempfile),
    %set_prolog_flag(string_esc, on),
    write(`:- set_prolog_flag(vba, on).\n`),
    write(`:- indexed data(0,1,0,0,0).\n`),
    write(`:- indexed known(0,1,0,0,0).\n`),
%    write(`:- sorted data/5.\n`),
%    write(`:- sorted known/5.\n`),
    write(`:- op(1200, xfx, When).\n`),
    write(`:- op(1100, xfy, Or).\n`),
    write(`:- op(1000, xfy, And).\n`),
    write(`:- op(600, xfy, In).\n`),
    write(`:- op(500, xfy, &).\n`),
    write(`:- op(50, xf, days).\n`),
    write(`:- op(50, xf, months).\n`),
    write(`:- op(50, xf, weeks).\n`),
    write(`:- op(50, xf, years).\n`),
    write(`:- op(50, xf, hours).\n`),
    write(`:- op(50, xf, mins).\n`),
    write(`:- op(50, xf, secs).\n`),
    amzi_system:license$info(license_key_str, ?key),
    write(`arulesxl_user_license(```),write(?key),write(```).\n`),
    true.
arxl_init_export(_) :-
    license_type(`Professional`),
    get_text(temp_file_error, ?errormsg),
    throw(?errormsg).
arxl_init_export(_) :- 
    get_text(no_license, ?errormsg),
    throw(?errormsg).

arxl_export_ruleset(?ruleset) :-
%   amzi_system:license$info(is_valid_ruleset, ?ruleset),
   blocked(?ruleset, _),
   !.
arxl_export_ruleset(?ruleset) :-
   arxl_module_listing(?ruleset),
   !.
arxl_export_ruleset(_) :- !.

% Used by ARulesXL, not advertised at this point,
% NOTE - it exports all the predicates to support ARulesXL inheritance
% copied from alib.pro but with the wrinkle of adding the index on know/5
% so excel and axl run the same.
arxl_module_listing(?M) :-
  amzi_system:pp$module(?M),
  % write(`:- indexed know(1,0,0,0,0).\n`),
  write(`:- discontiguous know/5.\n`),
  amzi_system:current_predicate(?M : ?P / ?A),
  amzi_system:predicate_property(?M : ?P / ?A, dynamic),
%  write(':- export('), writeq(P/A), write(').'), nl,
  amzi_system:pp$nl,
  arxl_modlist$ing(?M : ?P / ?A),
  fail.
arxl_module_listing(_).

arxl_modlist$ing(?MOD : ?NAME / ?ARITY) :-
  amzi_system:functor(?PRED, ?NAME, ?ARITY),
  amzi_system:clause(?MOD : ?PRED, ?BODY),
  amzi_system:pp((?PRED :- ?BODY)),
  fail.
arxl_modlist$ing(_).





arxl_compile(?in, ?out, ?list) :-
   set_prolog_flag(string_esc, on),
   amzi_compiler:xcompile(?in, ?out, ?list).

arxl_compile(?in, ?out) :-
   set_prolog_flag(string_esc, on),
   amzi_compiler:xcompile(?in, ?out, null).

%---------------------------------------------------------
% dual parser support for the time being
%

set_parser(?p) :-
   set_parm(parser, ?p).
   
new_parser :- not get_parm(parser, old).

t_tokenize(?a, ?b) :-
   (new_parser ->
      tokenizer2:tokenize(?a, ?b);
      tokenizer1:tokenize(?a, ?b) ).
t_tokenize(?a, ?b, ?c) :-
   (new_parser ->
      tokenizer2:tokenize(?a, ?b, ?c);
      tokenizer1:tokenize(?a, ?b, ?c) ).

p_parse_query(?a, ?b, ?c, ?d, ?e) :-
   (new_parser ->
      parser2:parse_query(?a, ?b, ?c, ?d, ?e);
      parser1:parse_query(?a, ?b, ?c, ?d, ?e) ).
p_object_list(?a, ?b, ?c, ?d) :-
   (new_parser ->
      parser2:object_list(?a, ?b, ?c, ?d);
      parser1:object_list(?a, ?b, ?c, ?d) ).
p_parse_rule(?a, ?b, ?c, ?d, ?e) :- 
   (new_parser ->
      parser2:parse_rule(?a, ?b, ?c, ?d, ?e);
      parser1:parse_rule(?a, ?b, ?c, ?d, ?e) ).
p_compare_operator(?a, ?b, ?c) :-
   (new_parser ->
      parser2:compare_operator(?a, ?b, ?c);
      parser1:compare_operator(?a, ?b, ?c) ).
p_parse_object(?a, ?b, ?c, ?d) :-
   (new_parser ->
      parser2:parse_object(?a, ?b, ?c, ?d);
      parser1:parse_object(?a, ?b, ?c, ?d) ).
p_parse_goals(?a, ?b, ?c, ?d) :-
   (new_parser ->
      parser2:parse_goals(?a, ?b, ?c, ?d);
      parser1:parse_goals(?a, ?b, ?c, ?d) ).
p_parse_goal(?a, ?b, ?c, ?d) :-
   (new_parser ->
      parser2:parse_goal(?a, ?b, ?c, ?d);
      parser1:parse_goal(?a, ?b, ?c, ?d) ).
p_warning_message(?a, ?b) :-
   (new_parser ->
      parser2:warning_message(?a, ?b);
      parser1:warning_message(?a, ?b) ).
p_parse_value(?a, ?b, ?c) :-
   (new_parser ->
      parser2:parse_value(?a, ?b, ?c);
      parser1:parse_value(?a, ?b, ?c) ).
p_get_just_value(?a, ?b) :-
   (new_parser ->
      parser2:get_just_value(?a, ?b);
      parser1:get_just_value(?a, ?b) ).

p_get_vba_value(?a, ?b) :-
   (new_parser ->
      parser2:get_vba_value(?a, ?b);
      parser1:get_vba_value(?a, ?b) ).   % called from vba

%-----------------------------------------------------------------------

% Don't ask if no rules loaded yet, it means spreadsheet is mucking about.
ask(?prompt, ?title, ?default, ?answer) :-
   xl_inputbox(?prompt, ?title, ?default, ?string),
   string_value(?string, ?answer),
   !.
ask(?p, ?t, ?d, ?a) :-  % used for regress testing
   retract(ask_answer(?a)),
   !,
   write(?p), tab(2),
   write(?a), nl.
ask(?p, ?t, ?d, ?a) :-
   write(?p), tab(1),
   read_string(?s),
   string_value(?s, ?a).

ask(?p, ?a) :-
   ask(?p, ARulesXL, '', ?a).

message(?msg) :-
   xl_msgbox(?msg),
   !.
message(?msg) :-
   nl,
   write(?msg),
   nl.
   

% an answer can only be a string, number or yes/no boolean
% string_value(`yes`, true) :- !.
% string_value(`no`, false) :- !.
string_value(?t, true) :- lookup(?t, true), !.
string_value(?f, false) :- lookup(?f, false), !.
string_value(?s, ?v) :-
   string_number(?s, ?v),
   !.
string_value(?s, ?t) :-
   catch( string_term(?s, ?t), _, fail ),
   !.
string_value(?s, ?a) :-
   string_atom(?s, ?a).

%-------------------------------------------
% Add a new ARule, replacing old one
%
% Booleans are expanded to ?a = true form;
% Facts are expanded to ?a = ?v WHEN true rules;
% <= operators are translated to =<
%
 
/*
add_rule(?ID, ?Str) :-
   string_tokens(?Str, ?Tokens, `=<>+()'/-*,`),
   parse_rule(?Attr, ?Rule, ?Tokens, []),
   retractall( rule(?Attr, ?ID, _) ),
   assert( rule(?Attr, ?ID, ?Rule) ).
*/

%ok_license("Developer").
%ok_license("Trial").
%ok_license("Internal Developer").
%ok_license("Professional").
ok_license("Professional").
ok_license("Standard").
ok_license(_).



clear_blocked(?rs) :-
   ( retract( blocked(?rs, _) ) -> true ; true ).
   
clear_ruleset(?M) :-
%log(   loading_rules(?M) ),
   blocked(?M, _),
   !.

clear_ruleset(?M) :-
   current_module(?M),
   once license_type(?L),
   not ok_license(?L),
   assert( blocked(?M, ?L) ),
   !,
   throw( license_error("Cannot update rules with runtime only license") ),
   true.

clear_ruleset(?M) :-
%mybuginit,
   ensure_module(?M),
   current_predicate(?M: ?F/ ?A),
   abolish(?M: ?F/ ?A),
   fail.
clear_ruleset(?M) :-
%   retractall( data(?M, _, _, _) ),
   retractall( rule_error(_) ),
   fail.
clear_ruleset(?M) :-
   retractall( known_language(_) ),
   retractall( language(_) ),
   cntr_set(3, 0),
   delete_parm(UserLanguage),
   fail.
clear_ruleset(_) :-
%   delete_parm(parser),
   set_parm(parser, new),
   fail.
clear_ruleset(?M) :-
   retract_rs_data(?M).

delete_ruleset(?M) :-
   current_predicate(?M: ?F/ ?A),
   abolish(?M: ?F/ ?A),
   fail.
delete_ruleset(?M) :-
   retract_rs_data(?M).

ensure_module(?M) :-
   current_module(?M),
   !.
ensure_module(?M) :-
   module$(?M),
   amzi_system:import$mod(?M, list),
   amzi_system:import$mod(?M, date_time),
   set$indexed(know(1,0,0,0,0)),
   true.

% if we don't close the module, then current module in the dll loader
% is left not at user, but the last module, causing problems for export
% and who knows who else.  This, like clear_ruleset, is called from VBA
% as a bracket to loading rules.
close_ruleset(?M) :-
   end_module$(?M).

flux_ruleset(?rs) :-
   assert(in_flux(?rs)),
   !.
unflux_ruleset(?rs) :-
   retractall(in_flux(?rs)),
   !.

/*
add_rule(?RuleSet, _, _) :-
   blocked(?RuleSet, _),
   !.
   */
/*
add_rule(?RuleSet, _, _) :-
   blocked(?RuleSet, ?L),
   	get_text(cannot_update_rules(?L), ?blocked_msg),
      assert( rule_error( ?blocked_msg ) ),
    !,  fail.
*/
add_rule(?RuleSet, ?Cell, ?Str) :-
   (blocked(?RuleSet, ?L) ->
   	get_text(cannot_update_rules(?L), ?blocked_msg),
      assert( rule_error( ?blocked_msg ) ),
      fail
      ;
      true ),
   catch(
      make_rule(?RuleSet, ?Cell, ?Str),
      ?Error,
      deal_with(?Error) ),
   !.

% just parse the rule, see if it's OK
check_rule(?RuleSet, ?Cell, ?Str) :-
   (blocked(?RuleSet, ?L) ->
   	get_text(cannot_update_rules(?L), ?blocked_msg),
      assert( rule_error( ?blocked_msg ) ),
      fail
      ;
      true ),
   catch(
      almost_make_rule(?RuleSet, ?Cell, ?Str),
      ?Error,
      deal_with(?Error) ),
   !.

to_atom(?a, ?a) :- atom(?a), !.
to_atom(?s, ?a) :- string(?s), string_atom(?s, ?a), !.
to_atom(?x, ?a) :- string_term(?s, ?x), string_atom(?s, ?a).

% just for checking
almost_make_rule(?RuleSetStr, ?CellStr, ?Str) :-
	to_atom(?RuleSetStr, ?RuleSet),
	to_atom(?CellStr, ?Cell),
%   set_language(?RuleSet),
   t_tokenize(?Str, ?Tokens, ?Vars),
   (?Tokens = [] ->
      true       % was a comment
      ;
      p_parse_rule(?RuleSet, ?Cell, ?Rule, ?Tokens, []) ).

make_rule(?RuleSetStr, ?CellStr, ?Str) :-
	to_atom(?RuleSetStr, ?RuleSet),
	to_atom(?CellStr, ?Cell),
%   set_language(?RuleSet),
   t_tokenize(?Str, ?Tokens, ?Vars),
   (?Tokens = [] ->
      true       % was a comment
      ;
      p_parse_rule(?RuleSet, ?Cell, ?Rule, ?Tokens, []),
      assert_rule( ?RuleSet, ?Rule ) ).

assert_rule( ?m, (rule(?c, ?ovs) :- ?b) ) :-
   assert_knows(?ovs, ?m, ?c, ?ovs),
   assert( ?m : ( rule(?c, ?ovs) :- ?b ) ).
assert_rule( ?m, ?r ) :-
   assert( ?m : ?r ).

% We need to keep the full argument as well to make a
% good calling match when we call the rule.
assert_knows( [], _, _, _ ).
assert_knows( [?o = ?v | ?ovs], ?m, ?c, ?oxs) :-
   obj_parts(?o, ?obj, ?props),
   assert( ?m : know(?obj, ?props, ?v, ?c, ?oxs) ),
   (?obj = RuleSetLocale(language) -> set_language(?v) ; true),
   (?obj = SystemParser -> set_parser(?v) ; true),
   !,
   assert_knows(?ovs, ?m, ?c, ?oxs).

deal_with(parse_error(?Error)) :-
   asserta( rule_error( ?Error ) ),
   !,
   fail.
deal_with(?err) :-
   string_term(?str, ?err),
   asserta( rule_error(?str) ),
   fail.

get_rule_error(?estr) :-
   retract( rule_error(?e) ),
   ( string(?e) -> ?estr = ?e ; string_termq(?estr, ?e) ),
   !.
get_rule_error(?estr) :-
   get_text(unknown_error, ?e),
   ( string(?e) -> ?estr = ?e ; string_termq(?estr, ?e) ).

%------------------------------------------
% API versions
%
% arxl_query(?ruleset, ?requery, ?qstr, ?fans)
% arxl_initialize_table(?ruleset, ?obj_str)
% arxl_add_to_table(?ruleset, ?obj_str, ?rowx, ?colx, ?valx)
% arxl_add_to_table(?rowx, ?colx, ?valx)
% arxl_add_to_vector(?ruleset, ?obj_str, ?rowx, ?valx)
% arxl_add_to_vector(?rowx, ?valx)
% arxl_add_data_cell(?ruleset, ?obj_str, ?valx)
%
% ?ruleset - name of the ruleset
% ?requery - true if inputs haven't changed, otherwise false
% ?qstr - the query string
% ?fans - the formatted answers
% ?rowx, ?colx - row and column headings
% ?valx - the value of a cell
% ?rowx, ?colx, ?valx - a primitive number, atom, string or
%    a string with date(y,m,d) or era like 4 months 2 years.
% ?obj_str - a string representing an object minus the row
%    and col information when building a table.
%    ex. ".data[2005]" might become ".data[2005,?,?]"
%
% arxl_intialize_table - clears a table, subsequent
%    arxl_add_to_table/3 or arxl_add_to_vector/2 calls add to
%    that initialized table.
% arxl_add_to_table/5, arxl_add_to_vector/4 - add to a table
%    or vector without having to immediately follow an
%    arxl_initialize_table.
%

arxl_query(?m, ?requery, ?qstr, ?fans) :-
   query_rules(?m, ?requery, ?qstr, ?fans),
   !.

arxl_query(?m, ?requery, ?qstr, ?fans, ?rowcnt, ?colcnt) :-
   query_rules(?m, ?requery, ?qstr, ?fans, ?rowcnt, ?colcnt),
   !.

arxl_query_xml(?m, ?requery, ?qstr, ?xml) :-
   query_rules(?m, ?requery, ?qstr, ?fans),
   xml_term(?xml, ?fans),
   !.

% arxl_prebuild_query(+RuleSet, +QueryString, -GoalTerm, -DataTerm)
arxl_prebuild_query(?m, ?qstr, ?goal, ?data) :-
   build_query(?m, ?qstr, ?goal, ?data).

% arxl_fast_query(+RuleSet, +RedoQueryBoolean, +GoalTerm, +DataTerm, -Outputs)
arxl_fast_query(?m, ?requery, ?goal, ?data, ?fans) :-
   (?requery = true -> true ; abolish_known ),
   abolish(query_error/1),
   findall_stack_clear,
   call_stack_clear,
   execute_query(?m, ?requery, ?goal, ?data),
   format_answers(?goal, ?fans),
   !.

arxl_initialize_table(?ruleset, ?obj_str) :-
   t_tokenize(?obj_str, ?obj_toks),
   p_object_list(?ruleset, obj(?obj), ?obj_toks, []),
   clear_data_table(?ruleset, ?obj),
   retractall( arxl_making_table(_, _) ),
   assert( arxl_making_table(?ruleset, ?obj) ),
   !.

arxl_add_to_table(?ruleset, ?obj_str, ?rowx, ?colx, ?valx) :-
   t_tokenize(?obj_str, ?obj_toks),
   p_object_list(?ruleset, obj(?obj), ?obj_toks, []),
   p_get_just_value(?rowx, ?row),
   p_get_just_value(?colx, ?col),
   p_get_just_value(?valx, ?val),
%   arxl_making_table( ?ruleset, ?obj ),
   add_obj_args(?obj, [?row, ?col], ?objargs),
   assert_data( data( ?ruleset, obj( ?objargs ), ?val, api )),
   !.
   
arxl_add_to_table(?rowx, ?colx, ?valx) :-
   p_get_just_value(?rowx, ?row),
   p_get_just_value(?colx, ?col),
   p_get_just_value(?valx, ?val),
   arxl_making_table( ?ruleset, ?obj ),
   add_obj_args(?obj, [?row, ?col], ?objargs),
   assert_data( data( ?ruleset, obj( ?objargs ), ?val, api )),
   !.

arxl_add_to_vector(?ruleset, ?obj_str, ?rowx, ?valx) :-
   t_tokenize(?obj_str, ?obj_toks),
   p_object_list(?ruleset, obj(?obj), ?obj_toks, []),
   p_get_just_value(?rowx, ?row),
   p_get_just_value(?valx, ?val),
%   arxl_making_table( ?ruleset, ?obj ),
   add_obj_args(?obj, [?row], ?objargs),
   assert_data( data( ?ruleset, obj( ?objargs ), ?val, api )),
   !.
   
arxl_add_to_vector(?rowx, ?valx) :-
   p_get_just_value(?rowx, ?row),
   p_get_just_value(?valx, ?val),
   arxl_making_table( ?ruleset, ?obj ),
   add_obj_args(?obj, [?row], ?objargs),
   assert_data( data( ?ruleset, obj( ?objargs ), ?val, api )),
   !.

arxl_add_data_cell(?ruleset, ?obj_str, ?valx) :-
   t_tokenize(?obj_str, ?obj_toks),
   p_object_list(?ruleset, obj(?obj), ?obj_toks, []),
   p_get_just_value(?valx, ?val),
%   retractall( data( ?ruleset, obj(?obj), _, _ )),
   retract_data( data( ?ruleset, obj(?obj), _, _ )),
%   assert( data( ?ruleset, obj(?obj), ?val, api ) ).
   assert_data( data( ?ruleset, obj(?obj), ?val, api ) ),
   !.

% For use in VBA tracking code.  It can either get the whole
% stack or each individual item.

/*
arxl_call_stack(?css) :-
   findall(?fs, call_stack_item(?fs), ?cs),
   string_term(?css, ?cs).
*/

arxl_call_stack_item(?fsi) :-
   call_stack_item(?fsi).
%arxl_call_stack_item(_) :- !, fail.

call_stack_item(?fsi) :-
   findall( ?fsi, ( stack(?csi), format_stack_item(?csi, ?fsi) ), ?fsis ),
   reverse( ?fsis, ?sisf ),
   !,
   member( ?fsi, ?sisf ).

format_stack_item(?st, ?fst) :-
   string_term(?fst, ?st).

/*
arxl_knowns(?kss) :-
   findall(?k, arxl_known_item(?k), ?ks),
   string_term(?kss, ?ks).
*/
   
arxl_known_item(?fk) :-
   known_item(?fk).
%arxl_known_item(_) :- !, fail.
   
known_item(?fk) :-
   known(?rs, ?obj, ?props, ?v, ?c),
   format_known(?rs, ?obj, ?props, ?v, ?c, ?fk).

format_known(?rs, ?obj, ?props, ?v, ?c, ?fk) :-
   object_string( obj([ ?obj| ?props ]), ?o),
%   string_term(?sv, ?v),
   format_value_string(?v, ?sv),
   string_term(?sc, ?c),
   stringlist_concat( [?rs, ?o, ?sv, ?sc], `;`, ?fk).

arxl_trace_item(?sti) :-
   findall( ?t, trace(?t), ?tl ),
   reverse( ?tl, ?rtl ),
   member( ?ti, ?rtl ),
   trace_output(?ti, ?sti).

%------------------------------------------
% ARulesXL versions
%

initialize_table(?ruleset, ?obj_str) :-
   clear_blocked(?ruleset),
%log(   loading_table(?ruleset, ?obj_str) ),
   t_tokenize(?obj_str, ?obj_toks),
   p_object_list(?ruleset, obj(?obj), ?obj_toks, []),
   clear_data_table(?ruleset, ?obj),
   retractall( making_table(_) ),
   assert( making_table(?obj) ).

add_to_table(?ruleset, ?row, ?col, ?val, ?cell) :-
   making_table( ?obj ),
   add_obj_args(?obj, [?row, ?col], ?objargs),
   assert_data( data( ?ruleset, obj( ?objargs ), ?val, ?cell )).
   
add_to_vector(?ruleset, ?row, ?val, ?cell) :-
   making_table( ?obj ),
   add_obj_args(?obj, [?row], ?objargs),
   assert_data( data( ?ruleset, obj( ?objargs ), ?val, ?cell )).

add_data_cell(?ruleset, ?obj_str, ?val, ?cell) :-
   clear_blocked(?ruleset),
   t_tokenize(?obj_str, ?obj_toks),
   p_object_list(?ruleset, obj(?obj), ?obj_toks, []),
%   retractall( data( ?ruleset, obj(?obj), _, _ )),
   retract_data( data( ?ruleset, obj(?obj), _, _ )),
   assert_data( data( ?ruleset, obj(?obj), ?val, ?cell ) ).

clear_data_table(?ruleset, ?obj) :-
   add_obj_args(?obj, [_, _], ?objrc2),
%   retractall( data( ?ruleset, obj(?objrc2), _, _ )),
   retract_data( data( ?ruleset, obj(?objrc2), _, _ )),
   add_obj_args(?obj, [_], ?objrc1),
%   retractall( data( ?ruleset, obj(?objrc1), _, _ )).
   retract_data( data( ?ruleset, obj(?objrc1), _, _ )).

add_obj_args([ ?o ], ?rc, [ ?orc ]) :-
   ?o =.. [ ?f | ?as ],
   append(?as, ?rc, ?asrc),
   ?orc =.. [ ?f | ?asrc ].
add_obj_args([ ?o | ?os ], ?rc, [?o | ?orcs]) :-
   !,
   add_obj_args(?os, ?rc, ?orcs).
      
%-------------------------------------------
% Query the ARules rule set
%

% Might need the dimensions of an array answer for some callers.
query_rules(?m, ?requery, ?qstr, ?fans, ?imax, ?jmax) :-
   query_rules(?m, ?requery, ?qstr, ?fans),
   get_dimensions(?fans, ?imax, ?jmax).

query_rules(?m, ?requery, _, ?msg) :-
   (?requery = true -> true ; abolish_known ),
%   retractall(query_error(_)),
   abolish(query_error/1),
   findall_stack_clear,
   call_stack_clear,
%   set_language(?m),
   (beta_expired ->
      get_text(beta_expired, ?msg)
      ;
      ( check_license ->
         fail   % it's ok so we fail into the next real query_rules clause
         ;
         get_text(bad_license, ?msg) ) ),
   !.
query_rules(?m, ?requery, ?qstr, ?fans) :-
% timer(?t1), log(query1(?m, ?qstr)),
   (trace_flag ->
      init_trace_output,
      abolish(trace/1),
      do_trace(query(?m, ?qstr))
      ;
      true),
   catch(
      ( build_query(?m, ?qstr, ?goal, ?data),
        execute_query(?m, ?requery, ?goal, ?data) ),
%      execution_error(?error),
%      query_error_cope(execution_error(?error)) ),
      ?BigE,
      query_error_cope(?BigE) ),
   !,
   (trace_flag -> do_trace(done), close_trace_output ; true),
   ( query_error( ?msg ) -> ?fans = ?msg, fail ; format_answers( ?goal, ?fans ) ).
%   format_answers(?goal, ?fans),
%   (get_parm(explain, on) -> trace_output("query finished"), trace_output(?fans); true),
%   close_trace_output.

% timer(?t2), ?t3 is ?t2 - ?t1, log(query2(?m, ?qstr, ?t3)).

/*

get_dimensions(?array, ?imax, ?jmax) :-
   list(?array),
   length(?array, ?imax),
   ( (?array = [ ?col | _], list(?col)) ->
         length(?col, ?jmax)
         ;
         ?jmax = 0 ).
*/
get_dimensions(?array, ?imax, ?jmax) :-
   list(?array),
   length(?array, ?imax),
   get_jmax(?array, 0, ?jmax),
   !.
get_dimensions(_, 0, 0).

get_jmax([], ?jmax, ?jmax).
get_jmax([?a | ?as], ?tmax, ?jmax) :-
   ( list(?a) -> length(?a, ?lmax) ; ?lmax = 0 ),
   ( ?lmax > ?tmax -> ?nmax = ?lmax ; ?nmax = ?tmax ),
   !, get_jmax( ?as, ?nmax, ?jmax ).

format_answers( ?v, ?v ) :-
   var(?v),
   !.
format_answers(findgs(?movs), ?fas) :-
   format_answers(?movs, ?as),
   (?as = [?fas] -> true ; ?fas = ?as).
format_answers([], []).
format_answers([_ : _ = ?v | ?movs], [?a | ?as]) :-
   format_answer(?v, ?a),
   !,
   format_answers(?movs, ?as).

format_answer( ?v, ?v ) :-
   var(?v),
   !.
format_answer( [?a1 | ?z1], [?a2 | ?z2] ) :-
   format_answer(?a1, ?a2),
   !,
   format_answer(?z1, ?z2).
format_answer(datetime(?y,?m,?d,0,0,0), ?s) :-
   eval_text([?y, '-', ?m, '-', ?d], ?s),
   !.
format_answer(datetime(0,0,0,?h,?m,?s), ?str) :-
   eval_text([?h, ':', ?m, ':', ?s], ?str),
   !.
format_answer(datetime(?y,?m,?d,?h,?n,?s), ?t) :-
   eval_text([?y, '-', ?m, '-', ?d, ' ', ?h, ':', ?n, ':', ?s], ?t),
   !.
format_answer(era(?y,?m,?d,?h,?n,?s), ?t) :-
   format_era(years, ?y, ?fy),
   format_era(months, ?m, ?fm),
   format_era(days, ?d, ?fd),
   format_era(hours, ?h, ?fh),
   format_era(mins, ?n, ?fn),
   format_era(secs, ?s, ?fs),
   stringlist_concat([?fy, ?fm, ?fd, ?fh, ?fn, ?fs], ?ts),
   string_trim(?ts, ?t),
   !.
format_answer(?a, ?a).

format_era(_, 0, ``).
format_era(years, ?n, ?txt) :- get_text(years(?n), ?txt), !.
format_era(months, ?n, ?txt) :- get_text(months(?n), ?txt), !.
format_era(days, ?n, ?txt) :-
   (?n mod 7 =:= 0 ->
      ?nn is ?n / 7,
      get_text(weeks(?nn), ?txt)
      ;
      get_text(days(?n), ?txt) ),
   !.
format_era(hours, ?n, ?txt) :- get_text(hours(?n), ?txt), !.
format_era(mins, ?n, ?txt) :- get_text(minutes(?n), ?txt), !.
format_era(secs, ?n, ?s) :- get_text(seconds(?n), ?txt), !.

add_call_stack(?msg, ?msg_stack) :-
   get_call_stack(5, [?last|?stack]),
   get_call_stack(6, ?lentest),
   ( length(?lentest, 6) ->
      stringlist_concat(?stack, " ", ?stack_items),
      strcat(?stack_items, " ...", ?stackstring)
      ;
      stringlist_concat(?stack, ?stackstring)
   ),
   string_term(?laststring, ?last),
   get_text(callstack(?laststring, ?msg, ?stackstring), ?msg_stack),
%   stringlist_concat(["At ", ?laststring, " ", ?msg, " Call stack = ", ?stackstring], ?msg_stack),
   !.
add_call_stack(?msg, ?msg).

/*
last_n_stack_string(?n, ?stack, ?stackstring) :-
   last_n_stack_items(?n, ?stack, ?stacklist),
   string_termq(?stacklist, ?stackstring).

last_n_stack_items(?n, ?stack, []) :-
   ?n =< 0,
   !.
last_n_stack_items(_, [], []) :- !.
last_n_stack_items(?n, [?item|?rest_in], [?item|?rest_out] ) :-
   ?nn is ?n - 1,
   !,
   last_n_stack_items(?nn, ?rest_in, ?rest_out).
*/

build_query(?m, ?qstr, ?goal, ?data) :-
   stored_query(?m, ?qstr, ?goal, ?data),
   !.
build_query(?m, ?qstr, ?goal, ?data) :-
   t_tokenize(?qstr, ?qtoks),
   p_parse_query(?m, ?goal, ?data, ?qtoks, []),
   assert( stored_query(?m, ?qstr, ?goal, ?data) ),
   !.

% Only retract the knowns when it a new query, not a requery
execute_query(_, ?requery, ?Goal, ?Inputs) :-
%   (?requery = true -> true ; retractall( known(_,_,_,_) ) ),
   assert_inputs(?Inputs),
%   ?Goal =.. [?pred | ?args],
%   append(?args, [ [], ?callstack ], ?sargs),
%   ?SGoal =.. [?pred | ?sargs],
   call(?Goal),
   !.
execute_query(_, _, ?goal, _) :-
   string_termq(?str, ?goal),
   throw( execution_error(failed_goal(?goal)) ).

assert_inputs([]) :-
   !.
assert_inputs([data(?m, ?a, ?v) | ?avs]) :-
   assert_known( ?m, ?a, ?v, input ),
   !,
   assert_inputs(?avs).


%-------------------------------------------------
% Error messages
%

query_error_cope(?e) :-
   query_error_msg(?e, ?msg),
   add_call_stack(?msg, ?msg_stack),
   assert(query_error(?msg_stack)),
   (trace_flag -> do_trace(error(?msg)), close_trace_output ; true),
   !,
%   fail.  % put this back, no we fail if there was an error now
   true.


query_error_msg(error(?etype, ?eattrs), ?errmsg) :-
   member(rc = 1021, ?eattrs),
   get_text(stack_overflow, ?errmsg),
%   stringlist_concat([`Runtime error: `, ?etype, ` `, ?msg], ?errmsg),
   !.
query_error_msg(error(?etype, ?eattrs), ?errmsg) :-
   member(rc = 217, ?eattrs),
   member(message = ?msg, ?eattrs),
   string_length(?msg, ?len),
   ?ll is ?len - 25,
   sub_string(?msg, 26, ?ll, ?extmsg),
   get_text(extended_predicate_error(?extmsg), ?errmsg),
%   stringlist_concat([`Runtime error: `, ?etype, ` `, ?msg], ?errmsg),
   !.
query_error_msg(error(?etype, ?eattrs), ?errmsg) :-
   member(rc = 1029, ?eattrs),
   get_text(break_hit, ?errmsg),
   !.
query_error_msg(error(?etype, ?eattrs), ?errmsg) :-
   member(message = ?msg, ?eattrs),
   get_text(runtime_error(?etype, ?msg), ?errmsg),
%   stringlist_concat([`Runtime error: `, ?etype, ` `, ?msg], ?errmsg),
   !.

query_error_msg(parse_error(?msg), ?errmsg) :-
   get_text(syntax_error(?msg), ?errmsg),
%   strcat(`Syntax error: `, ?msg, ?errmsg),
   !.
   
query_error_msg(execution_error(no_value(?m, ?o)), ?msg) :-
   mobject_string(?m, om(?m,?o), ?os),
   get_text(no_value(?os), ?msg),
%   stringlist_concat([`Reasoning error: Unable to find value for: `, ?os], ?msg),
   !.
query_error_msg(execution_error(failed_goal(find(?m, ?o, _))), ?msg) :-
   mobject_string(?m, ?o, ?os),
   get_text(failed_goal(?m, ?os), ?msg),
%   stringlist_concat([`Reasoning error: Unable to satisfy query for: `, ?m, `!`, ?os], ?msg),
   !.
query_error_msg(execution_error(failed_goal(?g)), ?msg) :-
   string_termq(?gs, ?g),
   get_text(failed_goal(?gs), ?msg),
%   stringlist_concat([`Reasoning error: Unable to satisfy query goal: `, ?gs], ?msg),
   !.
query_error_msg(execution_error(bad_is_known_arg(?g)), ?msg) :-
   string_termq(?gs, ?g),
   lookup(?ik, IS_KNOWN),
   get_text(bad_isknown_arg(?ik, ?gs), ?msg),
%   stringlist_concat([`Reasoning error: IS_KNOWN argument must be an object/property: `, ?gs], ?msg),
   !.
query_error_msg(execution_error(bad_date_function_arg(?f, ?g)), ?msg) :-
   string_termq(?gs, ?g),
   lookup(?ff, ?f),
   get_text(bad_date_arg(?ff, ?gs), ?msg),
%   stringlist_concat([`Reasoning error: Date function `, ?f, ` given argument not a date: `, ?gs], ?msg),
   !.
query_error_msg(execution_error(bad_list_arg(?f, ?g)), ?msg) :-
   string_termq(?gs, ?g),
   lookup(?ff, ?f),
   get_text(bad_list_arg(?ff, ?gs), ?msg),
%   stringlist_concat([`Reasoning error: Array function `, ?f, ` given argument not an array: `, ?gs], ?msg),
   !.
query_error_msg(execution_error(bad_numeric_arg(?f, ?g)), ?msg) :-
   string_termq(?gs, ?g),
   lookup(?ff, ?f),
   get_text(bad_numeric_arg(?f, ?gs), ?msg),
%   stringlist_concat([`Reasoning error: Numeric function `, ?f, ` given non numeric argument: `, ?gs], ?msg),
   !.
query_error_msg(execution_error(bad_range_arg), ?msg) :-
    get_text(bad_range_arg, ?msg),
%   stringlist_concat([`Reasoning error: RANGE function given argument not a table`], ?msg),
   !.
query_error_msg(execution_error(bad_range_arg( ?x )), ?msg) :-
   mobject_string(?x, ?os),
   get_text(bad_range_arg(?os), ?msg),
%   stringlist_concat([`Reasoning error: RANGE function given argument not a table: `, ?os], ?msg),
   !.
query_error_msg(execution_error(no_index( ?m, ?x, ?i )), ?msg) :-
   mobject_string(?x, ?os),
   get_text(no_index(?m, ?os, ?i), ?msg),
%   stringlist_concat([`Reasoning error: RANGE function given argument not a table: `, ?os], ?msg),
   !.
query_error_msg(execution_error(incompatible_function_argument(?f, ?a)), ?msg) :-
   string_termq(?gs, ?a),
   lookup(?ff, ?f),
   get_text(bad_function_argument(?ff, ?gs), ?msg),
%   stringlist_concat([`Reasoning error: Function `, ?f, ` given incompatible argument(s): `, ?gs], ?msg),
   !.
query_error_msg(execution_error(in_flux( ?x )), ?msg) :-
   get_text(in_flux(?x), ?msg),
%   stringlist_concat([`Reasoning error: RANGE function given argument not a table: `, ?os], ?msg),
   !.
query_error_msg(execution_error(trace_nocanuse(?f)), ?msg) :-
   get_text(trace_nocanuse(?f), ?msg),
%   stringlist_concat([`Reasoning error: Unable to find value for: `, ?os], ?msg),
   !.
query_error_msg(execution_error(unbound_object(?m, ?o)), ?msg) :-
   mobject_string(?m, om(?m,?o), ?os),
   get_text(unbound_object(?os), ?msg),
%   stringlist_concat([`Reasoning error: Unable to find value for: `, ?os], ?msg),
   !.
query_error_msg(execution_error(quit(?msg)), ?msg) :-
   !.
query_error_msg(execution_error(eval_error(?e)), ?msg) :-
   string_termq(?es, ?e),
   get_text(eval_error(?es), ?msg),
%   stringlist_concat([`Reasoning error: Unevaluable expression, possible bad types: `, ?es], ?msg),
   !.
query_error_msg(?err, ?msg) :-
   string_termq(?str, ?err),
   get_text(runtime_error(?str), ?msg),
%   stringlist_concat([`Runtime error: `, ?str], ?msg),
   !.
