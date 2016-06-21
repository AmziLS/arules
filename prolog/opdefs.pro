% Operator definitions

/* System Operators for reference
:- (op(1200, xfx, [:-, -->])).
:- (op(1200, fx, [?-, :-])).
:- (op(1100, fx, [import, export, dynamic, multifile, discontiguous, sorted,
                 indexed])).
:- (op(1100, xfy, ';')).
:- (op(1050, xfy, ->)).
:- (op(1000, xfy, ',')).
:- (op(900, fy, [\+, not, once])).
:- (op(900, fy, [?, bug])).
:- (op(700, xfx, [=, \=, is, =.., ==, \==, =:=, ~=, =\=, <, >, =<, >=, @<,
                 @>, @=<, @>=])).
:- (op(600, xfy, :)).
:- (op(500, yfx, [+, -, /\, \/, xor])).       % moved \ here from 900 fy
:- (op(400, yfx, [rem, mod, divs, mods, divu, modu])). % new ones and mod
:- (op(400, yfx, [/, //, *, >>, <<])).
:- (op(200, xfx, **)).
:- (op(200, xfy, ^)).
:- (op(200, fy, [+, -, \])).
*/

:- set_prolog_flag(vba, on).
:- set_prolog_flag(decimals, float).
:- set_prolog_flag(floats, double).

:- op(1200, xfx, When).  % same as :-
:- op(1100, xfy, Or).   % same as ;
:- op(1000, xfy, And).  % same as ,
:- op(600, xfy, In).
:- op(500, xfy, &).      % same as +, but opposit associativity

%:- op(500, yfx, [land, lor]).

:- op(50, xf, days).
:- op(50, xf, months).
:- op(50, xf, weeks).
:- op(50, xf, years).
:- op(50, xf, hours).
:- op(50, xf, mins).
:- op(50, xf, secs).
