% rename.pro
%
% Used when renaming rule sets

:- include('opdefs.pro').

:- module(rename).
:- import(list).
:- import(tokenizer).  % used to tokenize possible era strings
:- export rename/4.

rename(?oldtext, ?oldname, ?newname, ?newtext) :-
   t_tokenize(?oldtext, ?oldtokens),
   replace_all_elem(?oldname, ?newname, ?oldtokens, ?midtokens),
   check_strings(?midtokens, ?oldname, ?newname, ?newtokens),
   t_tokenize(?newtext, ?newtokens).
  
% the arguments to RQuery() for example are a string that has a
% query that might also need changes.

check_strings([], _, _, []) :-
   !.
check_strings([?midtok|?midtoks], ?oldname, ?newname, [?newtok|?newtoks]) :-
   string(?midtok),
   rename(?midtok, ?oldname, ?newname, ?newtok),
   !,
   check_strings(?midtoks, ?oldname, ?newname, ?newtoks).
check_strings([?tok|?midtoks], ?oldname, ?newname, [?tok|?newtoks]) :-
   !,
   check_strings(?midtoks, ?oldname, ?newname, ?newtoks).
   
   
:- end_module(rename).
