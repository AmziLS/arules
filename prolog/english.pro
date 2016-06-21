:- include('opdefs.pro').

:- module(arxlEnglish).

% Words used in rules
% dict(NationalLanguageWord, KeyWord).
% alphabetical by KeyWord - note it's second so
% that first argument indexing will find word we're
% looking for faster.

dict(AFTER, AFTER).
dict(AGE_TO_LISTSTRING, AGE_TO_LISTSTRING).
dict(AND, AND).
dict(APPEND, APPEND).
dict(ASK, ASK).
dict(BEFORE, BEFORE).
dict(call_stack, call_stack).
dict(COLLECT, COLLECT).
dict(CONCATENATE, CONCATENATE).
dict(COUNT, COUNT).
dict(CUT, CUT).
dict(date, date).
dict(datetime, datetime).
dict(DAY, DAY).
dict(days, days).
dict(day, days).
dict(DAYS_BETWEEN, DAYS_BETWEEN).
dict(DEBUGPRINT, DEBUGPRINT).
dict(DIFFERENCE, DIFFERENCE).
dict(DYNAMIC_FIND, DYNAMIC_FIND).
dict(EVALUATE, EVALUATE).
dict(EXISTS, EXISTS).
dict(EXTRACT_DATE, EXTRACT_DATE).
dict(EXTRACT_TIME, EXTRACT_TIME).
dict(false, false).
dict(FIND, FIND).
%dict([FIND,IN], FIND_IN).
dict(FINDALL, FINDALL).
dict(FOREACH, FOREACH).
dict(FORMAT_DATE, FORMAT_DATE).
dict(FORMAT_AGE, FORMAT_AGE).
dict(FOUNDALL, FOUNDALL).
dict(FIRST, FIRST).
dict(hours, hours).
dict(hour, hours).
dict(IN, IN).
dict(INDEX, INDEX).
%dict([INHERIT, FROM], INHERIT_FROM).
dict(INHERIT, INHERIT).
dict(FROM, FROM).
dict(INTERSECTION, INTERSECTION).
dict(IS_KNOWN, IS_KNOWN).
dict(IS_RULESET, IS_RULESET).
dict(IS_SUBSET, IS_SUBSET).
dict(ITEM_AT, ITEM_AT).
dict(LAST, LAST).
dict(LIST, LIST).
dict(LIST_TO_STRING, LIST_TO_STRING).
dict(MAKE_DATE, MAKE_DATE).
dict(MAKE_DATETIME, MAKE_DATETIME).
dict(MAXIMUM, MAXIMUM).
dict(MEMBER, MEMBER).
dict(MINIMUM, MINIMUM).
dict(MINUTES_BETWEEN, MINUTES_BETWEEN).
dict(minutes, minutes).
dict(minute, minutes).
dict(mins, minutes).
dict(min, minutes).
dict(MONTH, MONTH).
dict(months, months).
dict(month, months).
dict(MSGBOX, MSGBOX).
dict(NEXT, NEXT).
dict(NOT, NOT).
dict(now, now).
dict(ONETIME, ONETIME).
dict(OR, OR).
dict(PERMUTE, PERMUTE).
dict(PRIOR, PRIOR).
dict(QUIT, QUIT).
dict(RANGE, RANGE).  % Excel meaning of range
dict(REMOVE_DUPLICATES, REMOVE_DUPLICATES).
dict(REVERSE, REVERSE).
dict(seconds, seconds).
dict(second, seconds).
dict(secs, seconds).
dict(sec, seconds).
dict(SET, SET).
dict(SORT, SORT).
dict(SQL, SQL).
dict(SUM, SUM).
dict(TABLE, TABLE).
dict(TIME, TIME).
dict(TIMEIT, TIMEIT).
dict(THEN, THEN).
dict(today, today).
dict(true, true).
dict(UNION, UNION).
dict(USING, USING).
dict(WEEK, WEEK).
dict(weeks, weeks).
dict(week, weeks).
dict(WEEKDAY, WEEKDAY).
dict(WHEN, WHEN).
dict(WHERE, WHERE).
dict(YEAR, YEAR).

dict(years, years).
dict(year, years).

% Messages

text(about_licensed_to, `Licensed to: `).
text(about_maintenance_days_left,
   `Maintenance Days Left: `).      
text(about_user_name, `User Name: `).


text(arules_long_title,
   `ARulesXL -\nIntegrating Decision Support with Computational Analysis` ).
   
text(arules_short_title,
   `ARulesXL - Integrating Inference and Calculation` ).

text(bad_date_arg(?f, ?gs),
   [`Reasoning error: Date function `, ?f, ` given argument not a date: `, ?gs] ).

text(bad_expression_term,
   `Bad term in expression` ).

text(bad_goal_list,
   `A goal expression (ANDs, ORs) has a syntax error` ).

text(bad_function_arg(?f, ?gs),
   [`Reasoning error: Function `, ?f, ` given incompatible argument(s): `, ?gs] ).
   
text(bad_license,
   `Invalid ARulesXL license` ).

text(bad_list_arg(?f, ?gs),
   [`Reasoning error: Array function `, ?f, ` given argument not an array: `, ?gs] ).
   
text(bad_numeric_arg(?f, ?g),
   [`Reasoning error: Numeric function `, ?f, ` given non numeric argument: `, ?gs] ).

text(bad_object_arg(?f, ?gs),
   [`Reasoning error: `, ?f, ` argument must be a property: `, ?gs] ).

text(bad_range_arg,
   `Reasoning error: RANGE function given argument not a table` ).
   
text(bad_range_arg(?os),
   [`Reasoning error: RANGE function given argument not a table: `, ?os] ).
   
text(bad_rule_body,
   [`Ill-formed body of rule, possible missing `, ?when] ) :- lookup(?when, WHEN).
   
text(bad_rule_conditions,
   `Ill-formed conditions in body of rule` ).

text(beta_expired,
   `This beta test version has gone stale, please get a newer version.` ).

text(binary_rules,
   `ARulesXL Rulesets in Binary Format. You may hide this worksheet. You may also delete the rules from your spreadsheet; but do NOT delete the rule sets themselves, nor the RTable() and RCell() calls.` ).

text(blank_row_ends_table, `The row following a table must be blank.` ).

text(break_hit,
   `User interrupt stopped execution.` ).
   
text(button_cancel, `Cancel`).
text(button_ok, `OK`).

text(callstack(?laststring, ?msg, ?stackstring),
   [?laststring, " ", ?msg, ": Call stack = ", ?stackstring] ).

text(cannot_find_axl(?F),
   [`Cannot find matching exported rule set file, `, ?F, ` in the same directory as the current spreadsheet`] ).
   
text(cannot_update_rules(?L),
   [`Cannot update rule sets with license type: `, ?L] ).

text(control_step, `Step`).
text(control_title, `ARulesXL Debugger`).
text(control_run, `Run`).
text(control_quit, `Quit`).

text(copyright,
   `Copyright(c) 2005-2009 Amzi! inc. All rights reserved` ).
   
text(days(1),
   `1 day ` ).

text(days(?n),
   [ ?n, ` days `] ).

text(delete_ok(?rs),
   [`OK to remove rule set `, ?rs, `?`] ).
   
text(do_not_display_message_again,
   `Do not display this message again` ).

text(dtable_header_missing_dot(?h),
   [`Decision table header not a legal property, first character must be a dot: `, ?h] ).

text(edition,
   `Edition` ).
   
text(error(?msg),
   [`***ERROR*** `, ?msg] ).
   
text(eval_error(?es),
   [`Reasoning error: Unevaluable expression, possible bad types: `, ?es] ).

text(excel_name_conflict(?n),
   [?n, ` conflicts with Excel allowable range names`] ).
   
text(export_error(?rs),
   [`Error exporting rule set `, ?rs] ).

text(extended_predicate_error(?msg),
   [`Error in external function: `, ?msg] ).
   
text(fail_rule(?ss, ?m, ?c),
   [?ss, `FAIL RULE `, ?m, `:`, ?c] ).
      
text(failed_goal(?gs),
   [`Reasoning error: Unable to satisfy goal: `, ?gs] ).
   
text(failed_goal(?m, ?os),
   [`Reasoning error: Unable to satisfy goal: `, ?m, `:`, ?os] ).

text(failed_object(?ss, ?m, ?os),
   [?ss, `FAILED `, ?m, `:`, ?os] ).
   
text(find_object(?ss, ?m, ?os),
   [?ss, `FIND `, ?m, `:`, ?os] ).
   
text(found_object(?ss, ?m, ?os, ?vs),
   [?ss, `FOUND `, ?m, `:`, ?os, ` = `, ?vs] ).
   
text(friday, `Friday`).

text(fx_rcell_description,
	`Sets a property to the value in a cell.` ).
	
text(fx_rquery_description,
	`Queries the specified rule set with a query of the form 'FIND .object [WHEN .property1 = _1 AND ... OR ...] where the values for _1, _2, etc. come from the specified cells.` ).
	
text(fx_rarray_description,
	`Sets an array from a range with row and column headers. (These can optionally be turned off.)` ).
text(fx_rrowtable_description,
	`Sets a two-dimensional array from a range with column headers.` ).
text(fx_rcolumntable_description,
	`Sets a two-dimensional array from a range with row headers.` ).
text(fx_rinputrow_description,
	`Sets a one-dimensional input array from a range with column headers.` ).
text(fx_rinputcolumn_description,
	`Sets a one-dimensional input array from a range with row headers.` ).
	
text(fx_rxldependency_description,
	`Tells Excel about rule sets that are dependent on the rule set this function is located in.` ).
	
text(hours(1),
   `1 hour ` ).

text(hours(?n),
   [ ?n, ` hours `] ).

text(in_flux(?rs),
   [?rs, `  changed. Select 'Load Modified Rules' from the ARulesXL menu` ] ).
   
text(initializing_rule_sets,
   `Loading Rule Sets` ).

text(inputing_data,
   `Inputting Rule Set Data` ).

text(inputs_not_in_headers(?outs),
      [ `Decision table output property not included as column head: `, ?outsstr ] ) :-
   (list(?outs) -> stringlist_concat( ?outs, " ", ?outsstr ) ; ?outsstr = ?outs).
      
text(invalid_dtable_header(?h) ,
   [ `Decision table header not a legal property name: `, ?h ] ).

text(invalid_dtable_input(?h) ,
   [ `Decision table entry is not a comparison or evaluable expression: `, ?h ] ).

text(invalid_dtable_output(?h) ,
   [ `Decision table output does not evaluate to a value: `, ?h ] ).

text(invalid_expression,
   `Invalid expression` ).
   
text(invalid_input_data,
   `Invalid input data` ).
   
text(invalid_logic_head(?a),
   [`Invalid logic rule head "`, ?a, `"(...`] ).
   
text(invalid_object_argument,
   `Invalid property argument` ).
   
text(invalid_object_property,
   `Invalid property` ).
   
text(invalid_object_reference(?a),
   [`Invalid property reference "`, ?a, `", possible missing dot`] ).
   
text(invalid_query,
   `Invalid query` ).
   
text(invalid_query_goal,
   `Invalid query goal` ).
   
text(invalid_rule_action,
   `Invalid rule action` ).
   
text(invalid_rule_condition,
   `Invalid rule condition` ).
   
text(invalid_rule_set_name(?i),
   [`Argument `, ?i, ` does not refer to a valid ARulesXL Rule Set`] ).

text(known_source(?srcs, ?m, ?os, ?vs),
   [`FROM `, ?srcs, ` `, ?m, `:`, ?os, ` = `, ?vs] ).

text(license_expired,
   [`Attempt to run an ARulesXL version installed after maintenance expired. `,
    `Either renew maintenance at www.arulesxl.com or revert to a previous release.`] ).
    
text(license_message(?limit),
   [`This software needs to connect to the Internet to obtain a license key. `,
   `It will stop running after `, ?limit, ` uses or days. No personal information will `,
   `be communicated; only the license key for this PC. Please connect to the `,
   `Internet now and click 'Yes' to try again, or click 'No' to try later.`] ).
   
text(license_success,
   `Software license successfully installed.` ).

text(license_failure_cannot_connect,
   `Unable to communicate with the Internet. Please connect and try again.` ).

text(license_failure_author,
   `A license key cannot be issued for this PC. Please contact the author of this software.` ).

text(maintenance_expires(?MDL),
   [`Maintenance expires in `, ?MDL, ` days.\nPlease renew at www.arulesxl.com`] ).

text(menu_delete, `Remove Rule Set`).
text(menu_new, `&New Rule Set...` ).
text(menu_options, `&Options...`).
text(menu_export, `&Export Rule Sets...` ).
text(menu_exportexcel, `Export Rule Sets into Workbook` ).
text(menu_sharepoint, `Convert for Sharepoint` ).
text(menu_sharepoint_revert, `Convert back to Excel`).
text(menu_trace, `&Trace Query` ).
text(menu_open_trace_log, `Open Trace Log File`).
text(menu_help, `Documentation` ).
%text(menu_load, `Load Modified Rules` ).
text(menu_load_actives, `Load Modified Rules`).
text(menu_reload_all, `Reload All Rules`).
text(menu_tutorial, `Tutorial` ).
text(menu_samples, `Samples...` ).
text(menu_support, `Support Forum` ).
text(menu_about, `&About` ).

text(minutes(1),
   `1 min ` ).

text(minutes(?n),
   [ ?n, ` mins `] ).

text(missing_paren,
   `Possible missing parenthesis` ).
   
text(missing_paren(?function),
   [`Possible missing parenthesis in function `, ?f] ) :- lookup(?f, ?function).

text(monday, `Monday`).

text(months(1),
   `1 month ` ).

text(months(?n),
   [ ?n, ` months `] ).

text(narrow_dtable, `Decision table needs multiple columns`).

text(need_curly_brackets,
   `Logic predicates require curly brackets` ).
   
text(need_square_brackets,
   `Property arguments require square brackets` ).
   
text(new_ruleset_help,
   `To add rules, first highlight a cell range and create a rule set in them by selecting ARulesXL | New Ruleset...\n\nType rules into the cells in the box.\n\nClick outside the rule set to rerun your RQuery() functions.` ).
   
text(no_hardware_fingerprint,
   `Unable to get hardware fingerprint` ).

text(no_index(?m, ?o, ?i),
   [`No table or rules found `, ?m, `:`, ?o, ` with index `, ?i]).
   
text(no_license,
	 `This function requires the Professional Edition-- visit www.arulesxl.com to upgrade.` ).
	 
text(no_maintenance_days,
   `Unable to get remaining maintenance days` ).
   
text(no_rule_sets,
   `No rule sets defined yet` ).

text(no_runtime_edit,
	 [`You cannot modify the rules using just the free runtime.`,
	  ` Purchase a license to modify and/or create rules.`] ).

text(no_spaces_in_name,
   `A Rule set name cannot have spaces` ).
   
text(no_user_name,
   `Unable to get user name` ).

text(no_value(?os),
   [`Reasoning error: Unable to find value for: `, ?os] ).

text(no_vba_value(?x),
   [`Can't get a value for item: `, ?x] ).
   
text(object_parens(?o),
   [`Property `, ?o, ` with parenthesis instead of square brackets`] ).

text(ok,
   `OK` ).

text(one_cell_ruleset,
	`A rule set must have at least two cells (so you can insert new cells)` ).
	
text(options_refresh_rules_always,
   `Refresh Rules on Every Change` ).
   
text(options_refresh_rules_exit,
   `Refresh Rules After Clicking Outside Rule Set` ).

text(options_ruleset_style,
   `Rule Set Style Name` ).

text(options_selected_ruleset_style,
   `Selected Rule Set Style Name` ).

text(parse_error(?msg, ?here),
   [?msg, ` near here: `, ?here] ).

text(query(?m, ?q),
   [`QUERY `, ?m, `:  `, ?q] ).

text(range_overlaps,
   `Rule sets cannot overlap other rule sets.` ).
   
text(rcell_ruleset, 
   `RCell() must be used inside a rule set` ).

text(rename_ok(?old, ?new),
	[`Rename rule set `, ?old, ` to `, ?new, `?`] ).

text(rtable_too_many_rows,
   `RTable: Too many rows or columns for a one dimensional table`  ).
   
text(rtable_ruleset, 
   `RTable() must be used inside a rule set`).
   
text(rule_set_errors(?rs),
   [?rs, ` has errors, see comments in rule set for details`] ).

text(rule_set_exists(?rs),
   [`Rule set `, ?rs, ` already is defined in this workbook`] ).
   
text(rule_set_name_bad,
   `Bad rule set name. Must be a valid Excel name (no spaces).` ).
   
text(rule_set_not_found,
   `Rule set not found: `).
   
text(rule_set_exists(?n),
   [`Rule set `, ?n, ` already exists, pick a different name.`] ).
   
text(rule_set_name,
   `Rule Set Name:\n(no spaces)` ).

text(rule_set_range,
   `Rule Set Cell Range:` ).
   
text(rule_set_new_help, 
   `After creating the rule set, you can type a rule into each cell in the rule set range box.` ).

text(rule_syntax_error,
   `Syntax error in rule` ).
   
text(rules_not_initialized,
   `Rule sets not yet initialized.` ).

text(runtime_error(?str),
   [`Runtime error: `, ?str] ).

text(runtime_error(?etype, ?msg),
   [`Runtime error: `, ?etype, ` `, ?msg] ).

text(runtime_about_message,
	[`Runtime only, allows use of rules but not modification. `,
	 ` To modify and/or create your own rules, use the link below to purchase a license.`] ).

text(saturday, `Saturday`).

text(seconds(1),
   `1 sec ` ).

text(seconds(?n),
   [ ?n, ` secs `] ).

text(select_delete_cell,
   `Select a cell within the rule set to remove` ).
   
text(select_rule_set_label, `Rule Set Name: `).
text(select_rule_set_title, `Select Rule Set`).

text(square_logic_bracket(?f),
   [`If `, ?f, ` is a property, it needs a leading period-- if a logic relation it needs parenthesis, not square brackets`] ).

text(stack_overflow,
   `Stack overflow, probably due to circular rule definitions`).

/*
text(stack_overflow,
   [`Stack overflow, probably due to circular rule definitions. `,
    `Use debugger to trace execution of query causing the fault. `,
    `See documentation for details.`]  ).
*/
   
text(sunday, `Sunday`).

text(syntax_error(?msg),
   [ `Syntax error: `, ?msg ] ).

text(tables_first_column, `Tables must start in the first column of the rule set` ).

text(temp_file_error,
   `Error creating and/or writing temporary file.` ).
   
text(thursday, `Thursday`).

text(trace_cannot_end,
   `Unable to end current trace, exit and restart Excel` ).
text(trace_error_select_query_cell, 
   `Before starting trace, first select a cell that has a rule query.` ).
text(trace_log_not_found,
   `Unable to find trace log file in current workbook directory.` ).
text(trace_start_msg,
   `Click 'step' to start trace` ).
text(trace_stop_msg,
   `Trace stopped` ).
text(trace_continue_msg,
   `Click 'step' to continue stepping; 'run' to run to completion; 'end' to stop trace` ).
text(trace_run_msg,
   `Run not completed yet, click 'run' to continue or 'end' to end` ).
   
text(trace_data, `Data`).
text(trace_exit, `Succeed`).
text(trace_end, `End`).
text(trace_fail, `Fail`).
text(trace_false, `No`).
text(trace_find, `Find`).
text(trace_found, `Found`).
text(trace_help, `Help`).
text(trace_known, `Known`).
text(trace_match, `Match`).
text(trace_nocanuse(?f), [?f, ` cannot be used when tracing.`] ).
text(trace_redo, `Retry`).
text(trace_retest, `Retest`).
text(trace_run, `Run`).
text(trace_skip, `Skip`).
text(trace_step, `Step`).
text(trace_test, `Test`).
text(trace_true, `Yes`).
text(trace_try, `Try`).

text(try_rule(?ss, ?m, ?c),
   [?ss, `TRY RULE `, ?m, `:`, ?c] ).
      
text(tuesday, `Tuesday`).

text(unacceptable_cell_content,
   `Cell content is not acceptable in a rule set` ).

text(unknown_error, `Unknown Error`).

text(unbound_object(?o),
   [`Attempt to set a property with unbound variables: `, ?o] ).

text(visit_website,
   `Visit Website` ).
   
text(years(1),
   `1 year `).

text(years(?n),
   [ ?n, ` years `] ).
   
text(version(?v),
   [`Version `, ?v] ).

text(wednesday, `Wednesday`).

text(weeks(1),
   `1 week ` ).

text(weeks(?n),
   [ ?n, ` weeks `] ).

:- end_module(arxlEnglish).
