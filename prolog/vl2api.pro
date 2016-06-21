/*

RECOMMENDED NOW: 
Varicella #2....;
HEPA #2.........;
tdap #booster...;

BEHIND SCHEDULE: 
Varicella.......;

FORECAST: 
Varicella #2 ... recommended now;
HEPA #2 ........ recommended between now and 08/26/2092;

EXPLANATIONS: 
Varicella ...... : On standard schedule.;
HEPA ........... : On standard schedule.;
tdap ........... : Tdap booster due 5 years after last DTaP when DTaP complete before 7 years.;
dt ............. : DT complete when DTaP complete;
DTaP ........... : Complete five dose DTaP schedule.;
HepB ........... : Three valid doses is complete.;
hib_nonomp ..... : Hib is considered complete for anyone over 5 years old;
hib_omp ........ : Hib is considered complete for anyone over 5 years old;
IPV ............ : Four or five valid doses of polio with the last after 4 years of age is complete.;
MCV4 ........... : One valid dose of MCV4 is complete.;
MMR ............ : Full complement of MMR given.;
dtap_hepb_ipv .. : Past vaccinations with individual components suggest DTaP+HepB+IPV not be used in this case.;
hepb_2dose ..... : Continue HepB schedule.;
hib_hepb ....... : Continue using individual Hib and/or HepB instead of combination.;
hpv ............ : HPV is only for females over 9 years old..;
Influenza ...... : Influenza not indicated;
mmrv ........... : MMRV not applicable when Varicella complete or MMR complete or doctor deferred;
PCV ............ : PCV not recommended after 5 years of age;
rotavirus ...... : Rotavirus cannot be used after 32 weeks;
Td ............. : Use Tdap as booster when not used before.;
synagis ........ : Synagis not specifically indicated.;

HISTORY
DOB:..........................08/26/1992
diptheriatetanus  1  DTP....  10/15/1992  [0y, 1m, 19d]    ok  Minimum interval  
diptheriatetanus  2  DTP....  01/13/1993  [0y, 4m, 18d]    ok  Recommended age range  
diptheriatetanus  3  DTP....  03/24/1993  [0y, 6m, 26d]    ok  Recommended age range  
diptheriatetanus  4  DTP....  03/14/1994  [1y, 6m, 16d]    ok  Recommended age range  
diptheriatetanus  5  DTaP...  09/19/1996  [4y, 0m, 24d]    ok  Recommended age range  
dt..........  1  DTP........  10/15/1992  [0y, 1m, 19d]    ok  Minimum interval  
dt..........  2  DTP........  01/13/1993  [0y, 4m, 18d]    ok  Recommended age range  
dt..........  3  DTP........  03/24/1993  [0y, 6m, 26d]    ok  Recommended age range  
dt..........  4  DTP........  03/14/1994  [1y, 6m, 16d]    ok  Recommended age range  
dt..........  5  DTaP.......  09/19/1996  [4y, 0m, 24d]    ok  Recommended age range  
HEPA........  1  HEPA.......  04/07/1997  [4y, 7m, 12d]    ok  Recommended age range  
HEPA........  X2  HEPA......  06/02/1997  [4y, 9m, 7d]     x  Before min. interval  
hepatitisb..  1  HepB.......  04/27/1993  [0y, 8m, 1d]     ok  After recommended range  
hepatitisb..  2  HepB.......  06/09/1993  [0y, 9m, 14d]    ok  After recommended range  
hepatitisb..  3  HepB.......  12/20/1993  [1y, 3m, 24d]    ok  Recommended age range  
HIB.........  1  HIB-PRP-T..  10/15/1992  [0y, 1m, 19d]    ok  Minimum interval  
HIB.........  2  HIB-PRP-T..  01/13/1993  [0y, 4m, 18d]    ok  Recommended age range  
HIB.........  3  HIB-PRP-T..  03/24/1993  [0y, 6m, 26d]    ok  Recommended age range  
HIB.........  4  HIB-PRP-T..  12/20/1993  [1y, 3m, 24d]    ok  Recommended age range  
MCV4........  X1  MENINGOCOCCAL  04/07/1997  [4y, 7m, 12d]  x  Before min. age  
MCV4........  1  MCV4.......  06/03/2005  [12y, 9m, 8d]    ok  After recommended range  
Measles.....  1  MMR........  08/28/1993  [1y, 0m, 2d]     ok  Recommended age range  
Measles.....  2  MMR........  06/02/1997  [4y, 9m, 7d]     ok  Recommended age range  
MMR.........  1  MMR........  08/28/1993  [1y, 0m, 2d]     ok  Recommended age range  
MMR.........  2  MMR........  06/02/1997  [4y, 9m, 7d]     ok  Recommended age range  
Mumps.......  1  MMR........  08/28/1993  [1y, 0m, 2d]     ok  Recommended age range  
Mumps.......  2  MMR........  06/02/1997  [4y, 9m, 7d]     ok  Additional dose  
Polio.......  1  OPV........  10/15/1992  [0y, 1m, 19d]    ok  Minimum interval  
Polio.......  2  OPV........  01/13/1993  [0y, 4m, 18d]    ok  Recommended age range  
Polio.......  3  OPV........  03/24/1993  [0y, 6m, 26d]    ok  Recommended age range  
Polio.......  4  OPV........  09/19/1996  [4y, 0m, 24d]    ok  Recommended age range  
Rubella.....  1  MMR........  08/28/1993  [1y, 0m, 2d]     ok  Recommended age range  
Rubella.....  2  MMR........  06/02/1997  [4y, 9m, 7d]     ok  Additional dose  
Varicella...  1  Var........  08/14/1995  [2y, 11m, 19d]   ok  After recommended range  

Always check the manufacturer's Prescribing Information sheet for current indications and usage guidelines. VacLogic(tm) ver.VacLogic 3-58 .. © 2006 Visual Data LLC and Amzi! inc, all rights reserved

*/
% vl2api.pro
%
% An interface supporting VacLogic II calling conventions
%


:- import(list).
:- import(date_time).
:- import(amzi_xml).
:- include('opdefs.pro').

%:- import(tokenizer).
%:- import(parser).
:- import(rename).

:- dynamic parm/2.
:- indexed data(1,1,0,0,0).
:- indexed known(1,1,0,0,0).

:- discontiguous vl2_test/3.
:- discontiguous vl2_test/4.

%-------------------------------------------------------
% main.pro copy from VL2 for testing
%

vlamziod :-
   load('AmZiOd.axl'),
   arxl_query(AmZiOd, false, "FIND name1", ?x),
   write(?x), nl.
   
vl2 :-
   load('vaclogic.axl'),
   do_case(5555),
%   do_case(7554),
   vvh.
   
vvh :-
   write("---history_report-----"), nl,
   vl2api_history_report('D-T', ?msg),
   write(?msg), nl,
   fail.
vvh :-
   write("---history_report2-----"), nl,
   vl2api_history_report2('D-T', ?x),
   write(?x), nl,
   fail.
vvh :-
   write("---unknowns-----"), nl,
   vl2api_unknowns(?x),
   write(?x), nl,
   fail.
vvh :-
   write("---vaccine_history('D-T')-----"), nl,
   vl2api_vaccine_history('D-T', ?x, ?y),
   write(?x: ?y), nl,
   fail.
vvh.

do_case(?CaseID) :-
   vl2api_clear,
   get_vdata(?CaseID),
   indicators,
   ?t1 is cputime,
   vl2api_advise,
   ?t2 is cputime - ?t1,
   write(time = ?t2), nl,
   report_advice.

indicators :-
   vl2api_know(flu_indicated, true),
   vl2api_know(pneumo_indicated, true).
%    vl2api_know(pneumo_indicated, true))

get_vdata(?CaseID) :-
   vl2_output(''),
   vl2_output('---- Test Case Input Data ----'),
   vl2_output(?CaseID),
   establish_dates(?CaseID),
   move_facts(?CaseID),
   !.
get_vdata(?CaseID) :-
   vl2_output('Uh oh, get_vdata failed for case ' : ?CaseID),
   fail.

establish_dates(?CaseID) :-
   vl2_test(?CaseID, birthday, ?BDay),
   vl2api_know(birthday, ?BDay),
   vl2_test(?CaseID, today, ?Today),
   vl2api_know(today, ?Today),
   date_difference(?Today, ?BDay, ?Age),
   vl2_output(birthday = ?BDay),
   vl2_output(today = ?Today),
   vl2_output(age = ?Age),
   !.

move_facts(?CaseID) :-
   vl2_test(?CaseID, ?Attr, ?Val),
   not( member(?Attr, [age, birthday, today, vaccines]) ),
   ?Val2 = ?Val,
   vl2_output(?Attr= ?Val),
   vl2api_know(?Attr, ?Val2),
   fail.
move_facts(?CaseID) :-
   vl2_test(?CaseID, ?Obj, ?Attr, ?Val),
   ?Val2 = ?Val,
   vl2_output(?Obj= ?Attr= ?Val),
   vl2api_know(?Obj, ?Attr, ?Val2),
   fail.
move_facts(?CaseID) :-
   vl2_test(?CaseID, ?Obj, indicator, ?I, ?DateVal),
   ?DateVal2 = ?DateVal,
   vl2_output(?Obj=indicator= ?I= ?DateVal2),
   vl2api_know_indicator(?Obj, ?I, ?DateVal2),
   fail.
move_facts(_).

report_advice :-
   vl2_output('---- program vl2_output ----'),
   vl2_output('ERRORS'),
   report_advice(errors),
   vl2_output('STATUS'),
   report_advice(status),
   vl2_output('PLANNING'),
   report_advice(planning),
   vl2_output('VERBOSE'),
   report_advice(verbose),
   vl2_output('BEHIND SCHEDULE'),
   report_advice(behind),
   vl2_output('HISTORY'),
   report_advice(history),
   vl2_output('ODDS'),
   report_advice(odds),
   vl2_output('BASIC HISTORY'),
   report_advice(basic_history),
   vl2_output('BASIC PLANS'),
   report_advice(basic_plans),
   vl2_output('PACKED SUMMARIES'),
   report_advice(packed_summaries),
   vl2_output('XML SUMMARIES'),
   report_advice(xml_summaries).

report_advice(?Report) :-
   vl2api_report(?Report,?String),
   vl2_output([`  `,?String]),
   fail.
report_advice(_).


%--------------------------------------------------

vl2_output(?X) :-
   to_output_string(?X, ?S, ` `, no_quote),
   do_output(?S),
   !.
   
output2(?X, ?Sep) :-
   to_output_string(?X, ?S, ?Sep, no_quote),
   do_output(S),
   !.

outputq(?X) :-
   to_output_string(?X, ?S, ` `, quote),
   do_output(?S),
   !.
output2q(?X, ?Sep) :-
   to_output_string(?X, ?S, ?Sep, quote),
   do_output(?S),
   !.

do_output(?S) :-
   ext_output(?S),
   !.
do_output(?S) :-
   write(?S), nl.

to_output_string('', ``, _, _) :-
   !.
to_output_string(?S, ?S, _, _) :-
   string(?S),
   !.
to_output_string(?L, ?S, ?Sep, quote) :-
   list(?L),
   to_stringlistq(?L, ?SL),
   stringlist_concat(?SL, ?Sep, ?S),
   !.
to_output_string(?X, ?S, _, quote) :-
   string_termq(?S, ?X).
to_output_string(?L,?S, ?Sep, no_quote) :-
   list(?L),
   to_stringlist(?L, ?SL),
   stringlist_concat(?SL, ?Sep, ?S),
   !.
to_output_string(?X, ?S, _, no_quote) :-
   string_term(?S, ?X).

to_stringlist([], []).
to_stringlist([`  `| ?Xs], [`  `| ?Ss]) :-
   !, to_stringlist(?Xs, ?Ss).
to_stringlist([?X| ?Xs], [?S| ?Ss]) :-
   string_term(?S, ?X),
   !, to_stringlist(?Xs, ?Ss).

to_stringlistq([], []).
to_stringlistq([`  `| ?Xs], [`  `| ?Ss]) :-
   !, to_stringlistq(?Xs,?Ss).
to_stringlistq([?X| ?Xs], [?S| ?Ss]) :-
   string_termq(?S, ?X),
   !, to_stringlistq(?Xs, ?Ss).

process_error(?E) :-
   vl2_output('***** Error *****'),
   vl2_output(?E).


%-------------------------------------------------------
% Test data
%

vl2_test(5555, birthday, date(1998,1,1)).
vl2_test(5555, today, date(2003,3,15)).
vl2_test(5555, 'HepB/HIB', vaccination, date(1998,2,17)).
vl2_test(5555, 'HepB/HIB', vaccination, date(1998,4,21)).
vl2_test(5555, 'HepB/HIB', vaccination, date(1999,4,9)).
vl2_test(5555, 'DTaP', vaccination, date(1998,3,3)).
vl2_test(5555, 'DTaP', vaccination, date(1998,5,14)).
vl2_test(5555, 'DTaP', vaccination, date(1998,7,22)).
vl2_test(5555, 'DTaP', vaccination, date(2000,3,24)).
vl2_test(5555, 'DTaP', vaccination, date(2003,1,6)).
vl2_test(5555, 'IPV', vaccination, date(1998,2,17)).
vl2_test(5555, 'IPV', vaccination, date(1998,4,21)).
vl2_test(5555, 'IPV', vaccination, date(1999,4,9)).
vl2_test(5555, 'IPV', vaccination, date(2003,1,6)).
vl2_test(5555, 'Corn on the Cob', vaccination, date(2002,2,2)).
vl2_test(5555, 'Flank Steak', vaccination, date(2002,2,2)).

vl2_test(7554, birthday, date(2002,4,21)).
vl2_test(7554, today, date(2003,3,15)). 
vl2_test(7554, 'D-T', vaccination, date(2002,6,21)).
vl2_test(7554, 'D-T', vaccination, date(2002,8,30)).
vl2_test(7554, 'D-T', vaccination, date(2002,10,28)).
vl2_test(7554, 'HepB/HIB', vaccination, date(2002,6,14)).
vl2_test(7554, 'HepB/HIB', vaccination, date(2002,8,23)).
vl2_test(7554, 'IPV', vaccination, date(2002,6,14)).
vl2_test(7554, 'IPV', vaccination, date(2002,8,23)).
vl2_test(7554, 'IPV', vaccination, date(2002,10,28)).
vl2_test(7554, 'Pneumococcal.Conj', vaccination, date(2002,6,21)).
vl2_test(7554, 'Pneumococcal.Conj', vaccination, date(2002,8,30)).
vl2_test(7554, 'Pneumococcal.Conj', vaccination, date(2002,10,28)).

/* VL2 output for case 5555
---- program output ----
ERRORS
STATUS
   M-M-R #1.
   Varicella (if no documented immunity)  #1.
PLANNING
   Tdap #1 after 01/01/2009
   M-M-R #1 recommended now
   Varicella #1 recommended now
VERBOSE
   D-T : Complete. Five D-T-P dose schedule.
   Tdap : Td booster is due 5 years after last D-T-P or 11 years of age when last D-T-P was under 7 years of age.
   Hib : SC/NR. Hib not recommended after 5 years of age.
   Polio : Complete: Four dose schedule with last after 4 years of age.
   HepB : Complete: Three dose HepB schedule.
   PCV : SC/NR. PCV not recommended after 5 years of age.
   M-M-R #1.
   M-M-R : First dose of MMR on or after first birthday.
   Varicella (if no documented immunity)  #1.
   Varicella : One dose schedule: after first birthday before 13 years. Two dose schedule: if started after 13 years old.
   Hepatitis A : Not needed in this practice.
   Influenza : Influenza vaccination not indicated.6
BEHIND SCHEDULE
   M-M-R
   Varicella
HISTORY
   Hepatitis B;1;HepB/HIB;02/17/1998;[0 years, 1 months, 16 days];OK: Recommended age range
   Hepatitis B;2;HepB/HIB;04/21/1998;[0 years, 3 months, 20 days];OK: Recommended age range
   Hepatitis B;3;HepB/HIB;04/09/1999;[1 years, 3 months, 8 days];OK: Recommended age range
   D-T;1;DTaP;03/03/1998;[0 years, 2 months, 2 days];OK: Recommended age range
   D-T;2;DTaP;05/14/1998;[0 years, 4 months, 13 days];OK: Recommended age range
   D-T;3;DTaP;07/22/1998;[0 years, 6 months, 21 days];OK: Recommended age range
   D-T;4;DTaP;03/24/2000;[2 years, 2 months, 23 days];OK: After recommended range
   D-T;5;DTaP;01/06/2003;[5 years, 0 months, 5 days];OK: Recommended age range
   Hib;1;HepB/HIB;02/17/1998;[0 years, 1 months, 16 days];OK: Minimum interval
   Hib;2;HepB/HIB;04/21/1998;[0 years, 3 months, 20 days];OK: Minimum interval
   Hib;3;HepB/HIB;04/09/1999;[1 years, 3 months, 8 days];OK: Recommended age range
   Polio;1;IPV;02/17/1998;[0 years, 1 months, 16 days];OK: Minimum interval
   Polio;2;IPV;04/21/1998;[0 years, 3 months, 20 days];OK: Minimum interval
   Polio;3;IPV;04/09/1999;[1 years, 3 months, 8 days];OK: Recommended age range
   Polio;4;IPV;01/06/2003;[5 years, 0 months, 5 days];OK: Recommended age range
ODDS
*/


%-------------------------------------------------------
% VL2 API
%

/*
  { Clear out the reasoner }
  tf := dmVaccines.ls.ExecPStr(term, 'vl2api_clear');
*/

% Not really necessary as RQuery does the clear now,
% but for compatibility, we'll make all the vl2api queries
% requeries, and do the clear here.  We'll also
% initialize the data tables used for input.
vl2api_clear :-
   abolish_known,
   retractall( advice(_,_,_) ),
   arxl_initialize_table(CommonRules, `data`),
   arxl_initialize_table(CommonRules, `raw_vaccination`),
   arxl_initialize_table(CommonRules, `indicators`),
   arxl_add_to_vector(CommonRules, `data`, Vaccines, all),
   reset_indicators_index,
   add_indicator('*', '', ''),   % required so there's at least one
   reset_vaccinations_index,
   add_vaccination('*', ''),    % required so there's at least one
   arules_release_text(?ar_version),
   vl2api_know(arules_release, ?ar_version),
   arxl_query(VacLogic, true, "FIND version", ?vl_version),
   vl2api_know(vaclogic_release, ?vl_version).
%   hack_name_fix.

/*
hack_name_fix :-
   vnames( ?vns ),
   atomizeem( ?vns ).

atomizeem([]).
atomizeem([?s| ?ss]) :-
   string_term(?s, _),
   !, atomizeem(?ss).
   
vnames([`DiptheriaTetanus`, `DT`, `DTaP`, `DTaP_HepB_IPV`, `DTaP_Hib_IPV`, `Gardasil`, `HepA`, `HepA_HepB`,
  `HepatitisB`, `HepB`, `HepB_2Dose`, `Hib`, `Hib_HepB`, `Hib_nonomp`, `Hib_omp`, `Influenza`, `IPV`, `MCV4`,
  `Measles`, `MMR`, `MMRV`, `Mumps`, `OPV`, `PCV`, `Polio`, `PPV`, `Rota_Virus`, `Rubella`, `Synagis`, `td`, `tdap`, `Varicella`] ).
*/

/*
  { Tell the reasoner the birthday }
  tf := dmVaccines.ls.ExecPStr(term,
    'vl2api_know_date(birthday, `' + FormatDateTime('mm/dd/YYYY',
    Birthday) + '`)');

  { Tell the reasoner what day it is }
  tf := dmVaccines.ls.ExecPStr(term,
    'vl2api_know_date(today, `' + FormatDateTime('mm/dd/YYYY',
    ForTheDay) + '`)');
*/

% vl2api_know_date/2
vl2api_know_date(birthday, ?DateString) :-
   date_string(?Date, 'm/d/y', ?DateString),
   date_datetime(?Date, ?DateTime),
   arxl_add_to_vector(CommonRules, `data`, BirthDate, ?DateTime).
vl2api_know_date(today, ?DateString) :-
   date_string(?Date, 'm/d/y', ?DateString),
   date_datetime(?Date, ?DateTime),
   arxl_add_to_vector(CommonRules, `data`, TestDate, ?DateTime).

/*
        'vl2api_know_date(`' + vacname + '`,vaccination, `' + strVal + '`)');
*/

% vl2api_know_date/3
vl2api_know_date(?Vaccine, vaccination, ?DateString) :-
   date_string(?Date, 'm/d/y', ?DateString),
   date_datetime(?Date, ?DateTime),
   add_vaccination(?Vaccine, ?DateTime),
   !.

add_vaccination(?Vaccine, ?DateTime) :-
   next_vaccination_index(?i),
   arxl_add_to_table(CommonRules, `raw_vaccination`, ?i, Vaccination, ?Vaccine),
   arxl_add_to_table(CommonRules, `raw_vaccination`, ?i, VaccinationDate, ?DateTime),
   !.

/*
        strVal :=
          'vl2api_know_indicator(`' + vacname + '`,' + qIndicators.FieldByName('INDICATORID').AsString +
          ',`' + DateToStr(qIndicators.FieldByName('DATE_INDICATED').AsDateTime) + '`)';
*/

vl2api_know_indicator(?VacName, ?IndicatorID, ?date ) :-
   date_datetime(?date, ?datetime),
   !,
   add_indicator(?VacName, ?IndicatorID, ?datetime ).
vl2api_know_indicator(?VacName, ?IndicatorID, ?DateString) :-
   date_string(?Date, 'm/d/y', ?DateString),
   date_datetime(?Date, ?DateTime),
   add_indicator(?VacName, ?IndicatorID, ?DateTime),
   !.

add_indicator(?VacName, ?IndicatorID, ?Date) :-
   ( date_datetime(?Date, ?DateTime); ?DateTime = ?Date ),  % 3rd parm not a real date
   next_indicator_index(?i),
   arxl_add_to_table(CommonRules, `indicators`, ?i, Vaccine, ?VacName),
   arxl_add_to_table(CommonRules, `indicators`, ?i, Indicator, ?IndicatorID),
   arxl_add_to_table(CommonRules, `indicators`, ?i, Date, ?DateTime),
   !.

reset_indicators_index :-
   cntr_set(1, 1).
   
next_indicator_index(?i) :-
   cntr_inc(1, ?i).
      
/*
    tf := dmVaccines.ls.ExecPStr(term,
      'vl2api_know(`HepA`, include, true)');
*/

% know/3
vl2api_know(HepA, include, ?x) :-
   !, add_indicator(HepA, include, '').
vl2api_know(?Vaccine, vaccination, ?Date) :-
   date_datetime(?Date, ?DateTime),
   add_vaccination(?Vaccine, ?DateTime),
%   next_vaccination_index(?i),
%   arxl_add_to_table(CommonRules, `.raw_vaccination`, ?i, Vaccination, ?Vaccine),
%   arxl_add_to_table(CommonRules, `.raw_vaccination`, ?i, VaccinationDate, ?DateTime),
   !.

/*
    tf := dmVaccines.ls.ExecPStr(term, 'vl2api_know(flu_indicated, true)');
    tf := dmVaccines.ls.ExecPStr(term, 'vl2api_know(flu_indicated, false)');
    tf := dmVaccines.ls.ExecPStr(term, 'vl2api_know(synagis_indicated, true)');
*/

% know/2
vl2api_know(flu_indicated, true) :-
   !, add_indicator(Influenza, indicated, '').
vl2api_know(flu_indicated, false) :-
   !, add_indicator(Influenza, contraindicated, '').
vl2api_know(synagis_indicated, true) :-
   !, add_indicator(Synagis, indicated, '').
vl2api_know(synagis_indicated, false) :-
   !, add_indicator(Synagis, contraindicated, '').
vl2api_know(pneumo_indicated, true) :-
   !, add_indicator(PPV, indicated, '').
vl2api_know(pneumo_indicated, false) :-
   !, add_indicator(PPV, contraindicated, '').
vl2api_know(birthday, ?date) :-
   date_datetime(?date, ?datetime),
   !, arxl_add_to_vector(CommonRules, `data`, BirthDate, ?datetime).
vl2api_know(today, ?date) :-
   date_datetime(?date, ?datetime),
   !, arxl_add_to_vector(CommonRules, `data`, TestDate, ?datetime).
vl2api_know(arules_release, ?ar) :-
   !, arxl_add_to_vector(CommonRules, `data`, ARulesRelease, ?ar).
vl2api_know(gender, ?g) :-
   !, arxl_add_to_vector(CommonRules, `data`, Gender, ?g).
vl2api_know(vaclogic_release, ?vr) :-
   !, arxl_add_to_vector(CommonRules, `data`, VacLogicRelease, ?vr).

reset_vaccinations_index :-
   cntr_set(2, 1).
   
next_vaccination_index(?i) :-
   cntr_inc(2, ?i).

date_datetime( date(?y, ?m, ?d), datetime(?y, ?m, ?d, 0, 0, 0) ).

/*
  tf := dmVaccines.ls.ExecPStr(term, 'vl2api_advise');
*/

vl2api_advise :-
%   vl_listing('c:\\vldump1.txt'),
   arxl_query(VacLogic, false, "FIND initialize", _),
   arxl_query(VacLogic, true, "FIND history", _),
%   vl_listing('c:\\vldump2.txt'),
   arxl_query(VacLogic, true, "FIND plans", _),
%   vl_listing('c:\\vldump3.txt'),
   !.
%vl2api_advise :-
%   query_error(?msg),
%   write( ?msg ), nl.

vl2api_error(?msg) :-
   query_error(?msg).
   
vl_listing(?f) :-
   tell(?f),
   listing,
   told.

/*
  tf := dmVaccines.ls.CallPStr(term, 'vl2api_report(status, TEXT)');
  tf := dmVaccines.ls.CallPStr(term, 'vl2api_report(planning, TEXT)');
  tf := dmVaccines.ls.CallPStr(term, 'vl2api_report(verbose, TEXT)');
  tf := dmVaccines.ls.CallPStr(term, 'vl2api_report(history, TEXT)');
  tf := dmVaccines.ls.CallPStr(term, 'vl2api_report(odds, TEXT)');
  tf := dmVaccines.ls.CallPStr(term, 'vl2api_report(behind,' + strVaccinesGroups + ')');
*/

vl2api_report(status, ?line) :-
   arxl_query(VacLogic, true, "FIND vl2_status", ?report),
   !, member( ?line, ?report ).
vl2api_report(planning, ?line) :-
   arxl_query(VacLogic, true, "FIND vl2_planning", ?report),
   !, member( ?line, ?report ).
vl2api_report(verbose, ?line) :-
   arxl_query(VacLogic, true, "FIND vl2_verbose", ?report),
   !, member( ?line, ?report ).
vl2api_report(behind, ?line) :-
   arxl_query(VacLogic, true, "FIND vl2_behind", ?report),
   !, member( ?line, ?report ).
vl2api_report(history, ?line) :-
   arxl_query(VacLogic, true, "FIND vl2_history", ?report),
   !, member( ?line, ?report ).
vl2api_report(odds, ?line) :-
   arxl_query(VacLogic, true, "FIND vl2_odds", ?report),
   !, member( ?line, ?report ).
vl2api_report(basic_history, ?line) :-
   arxl_query(VacLogic, true, "FIND basic_history", ?report),
   !, member( ?line, ?report ).
vl2api_report(basic_plans, ?line) :-
   arxl_query(VacLogic, true, "FIND basic_plans", ?report),
   !, member( ?line, ?report ).
vl2api_report(packed_summaries, ?line) :-
   arxl_query(VacLogic, true, "FIND packed_summaries", ?report),
   !, member( ?line, ?report ).
vl2api_report(xml_summaries, ?report) :-
   arxl_query(VacLogic, true, "FIND xml_summaries", ?report),
   !.


/*
        tf := dmVaccines.ls.CallPStr(term, 'vl2api_vaccine_version(X)');
        tf := dmVaccines.ls.CallPStr(term, 'vl2api_copyright_notice(X)');
*/

vl2api_vaccine_version(?v) :-
   arxl_query(VacLogic, true, "FIND version", ?v).
   
vl2api_copyright_notice(?c) :-
   arxl_query(VacLogic, true, "FIND copyright", ?c).


/*
  tf := dmVaccines.ls.CallPStr(term,
    'vl2api_is_ok(''' + strVacCategory + ''', Status)');
*/

vl2api_is_ok(?v, ?msg) :-
   string_term(?vs, ?v),
   stringlist_concat(["FIND vl2_is_ok['", ?vs, "']"], ?query),
   arxl_query(VacLogic, true, ?query, ?msg).

vl2api_ok_break_down2(?v, ?msg) :-
   string_term(?vs, ?v),
   stringlist_concat(["FIND vl2_ok_break_down2['", ?vs, "']"], ?query),
   arxl_query(VacLogic, true, ?query, ?msg).

/*
  tf := dmVaccines.ls.CallPStr(term,
    'vl2api_history_report(''' + strVacCategory + ''', Status)');
*/

vl2api_unknowns(?v) :-
   arxl_query(VacLogic, true, "FIND vl2_unknowns", ?unks),
   !,
   member(?v, ?unks).

vl2api_history_report(?v, ?msg) :-
   string_term(?vs, ?v),
   stringlist_concat(["FIND vl2_history['", ?vs, "']"], ?query),
   arxl_query(VacLogic, true, ?query, ?msgs),
   !,
   member(?msg, ?msgs).

/*
    tf := dmVaccines.ls.CallPStr(term,
      'vl2api_history_report2(''' + strVacCategoryName + ''', TEXT)');
*/

vl2api_history_report2(?v, ?msg) :-
   string_term(?vs, ?v),
   stringlist_concat(["FIND vl2_history2['", ?vs, "']"], ?query),
   arxl_query(VacLogic, true, ?query, ?msg),
   !,
   not sub_string(?msg, _, _, "no indicators").

/*
  tf := dmVaccines.ls.CallPStr(term,
    'vl2api_vaccine_history(''D-T'', Vaccine, Date)');
  tf := dmVaccines.ls.CallPStr(term,
    'vl2api_vaccine_history(''Hib'', Vaccine, Date)');
  tf := dmVaccines.ls.CallPStr(term,
    'vl2api_vaccine_history(''Hepatitis A'', Vaccine, Date)');
  tf := dmVaccines.ls.CallPStr(term,
    'vl2api_vaccine_history(''Hepatitis B'', Vaccine, Date)');
  tf := dmVaccines.ls.CallPStr(term,
    'vl2api_vaccine_history(''live_virus2'', Vaccine, Date)');
  tf := dmVaccines.ls.CallPStr(term,
    'vl2api_vaccine_history(''Polio'', Vaccine, Date)');
  tf := dmVaccines.ls.CallPStr(term,
    'vl2api_vaccine_history(''Varicella'', Vaccine, Date)');
  tf := dmVaccines.ls.CallPStr(term,
    'vl2api_vaccine_history(''MCV4'', Vaccine, Date)');
  tf := dmVaccines.ls.CallPStr(term,
    'vl2api_vaccine_history(''PCV'', Vaccine, Date)');
  tf := dmVaccines.ls.CallPStr(term,
    'vl2api_vaccine_history(''Influenza'', Vaccine, Date)');
*/

vl2api_vaccine_history(?cat, ?vaccine, ?datestr) :-
   string_term(?catstr, ?cat),
   stringlist_concat(["FIND vl2_vaccine_history['", ?catstr, "']"], ?query),
   arxl_query(VacLogic, true, ?query, ?dvs),
   !,
   member( [?dates1, ?vaccine], ?dvs ),
   date_string( date(?y, ?m, ?d), 'y-m-d', ?dates1 ),
   date_string( date(?y, ?m, ?d), 'mm/dd/yyyy', ?datestr ).
   
/*
  tf := dmVaccines.ls.CallPStr(term,
    'vl2api_category(''' + strVacName + ''', CategoryOut)');
*/

vl2api_category(?v, ?catstr) :-
   string_term(?vstr, ?v),
   stringlist_concat(["FIND vl2_category['", ?vstr, "']"], ?query),
   arxl_query(VacLogic, true, ?query, ?cat),
   string_term(?catstr, ?cat).

/*
  tf := dmVaccines.ls.CallPStr(term,
    'general:category ->> X');
*/

vl2api_categories( ?cat ) :-
   arxl_query(VacLogic, true, "FIND vl2_categories", ?cats),
   member(?cat, ?cats).

vl2api_ok_to_give( ?v ) :-
   string_term(?vstr, ?v),
   stringlist_concat(["FIND vl2_ok_to_give['", ?vstr, "']"], ?query),
   arxl_query(VacLogic, true, ?query, ?yn),
   !,
   ?yn = yes.
   
vl2api_is_complete( ?v ) :-
   string_term(?vstr, ?v),
   stringlist_concat(["FIND vl2_is_complete['", ?vstr, "']"], ?query),
   arxl_query(VacLogic, true, ?query, ?yn),
   !,
   ?yn = yes.

vl2_format_date( datetime(?y, ?m, ?d, _, _, _), ?datestr ) :-
   date_string( date(?y, ?m, ?d), 'mm/dd/yyyy', ?datestr ).

vl2_format_era( era(?y, ?m, ?d, _, _, _), ?agestr ) :-
   string_term(?agestr, [?y years, ?m months, ?d days]).

% A listing that excludes the regress data for VacLogic
% lists rest of data/5 --- NOT USED ANYMORE
vl_data_listing :-
%  functor(PRED, NAME, ARITY),
%  clause(user : PRED, BODY),
%  pp((PRED :- BODY)),
%  fail.
   clause(user: data(?mod, ?obj, ?prop, ?val, ?src), ?body),
   ?mod \= regress,
   pp(( data(?mod, ?obj, ?prop, ?val, ?src) :- ?body )),
   fail.
vl_data_listing.

all_vnames :-
   findall(?vname, data(CommonRules, Vaccines(_, name), _, ?vname, _), ?vnames),
   writeq(?vnames), nl.
   
/*
RECOMMENDED NOW: 
HepA #2.........;
tdap #booster...;

FORECAST: 
HepA #2 ........ recommended between now and 12/19/2087;

EXPLANATIONS: 
HepA ........... : On standard schedule.;
tdap ........... : Tdap First booster due 5 years later when last DT family given after 7 years.;
dt ............. : DT complete when DTaP complete;
dtap ........... : Complete five dose DTaP schedule.;
hepa_hepb ...... : Complete three doses of HepB or HepA+HepB makes HepA+HepB complete;
HepB ........... : Three valid doses is complete.;
hib_nonomp ..... : Hib is considered complete for anyone over 5 years old;
hib_omp ........ : Hib is considered complete for anyone over 5 years old;
ipv ............ : Four or five valid doses of polio with the last after 4 years of age is complete.;
MCV4 ........... : One valid dose of MCV4 is complete.;
MMR ............ : Full complement of MMR given.;
dtap_hepb_ipv .. : Past vaccinations with individual components suggest DTaP+HepB+IPV not be used in this case.;
hepb_2dose ..... : HepB two dose cannot be used after 16 years;
hib_hepb ....... : Continue using individual Hib and/or HepB instead of combination.;
Influenza ...... : Influenza not indicated;
mmrv ........... : MMRV not applicable when Varicella complete or MMR complete or doctor deferred;
PCV ............ : PCV not recommended after 5 years of age;
rota_virus ..... : Rota virus cannot be used after 32 weeks;
Td ............. : Use Tdap as booster when not used before.;
synagis ........ : Synagis not specifically indicated.;
Varicella ...... : Documented disease;

HISTORY
DOB:..........................12/19/1987
diptheriatetanus  1  DTP....  03/02/1988  [0y, 2m, 12d]    ok  Recommended age range  
diptheriatetanus  2  DTP....  04/28/1988  [0y, 4m, 9d]     ok  Recommended age range  
diptheriatetanus  3  DTP....  07/07/1988  [0y, 6m, 18d]    ok  Recommended age range  
diptheriatetanus  4  DTP....  10/01/1989  [1y, 9m, 12d]    ok  After recommended range  
diptheriatetanus  5  DTP....  12/31/1991  [4y, 0m, 12d]    ok  Recommended age range  
diptheriatetanus  X6  Td....  06/24/1999  [11y, 6m, 5d]    x  Extra dose  
dt..........  1  DTP........  03/02/1988  [0y, 2m, 12d]    ok  Recommended age range  
dt..........  2  DTP........  04/28/1988  [0y, 4m, 9d]     ok  Recommended age range  
dt..........  3  DTP........  07/07/1988  [0y, 6m, 18d]    ok  Recommended age range  
dt..........  4  DTP........  10/01/1989  [1y, 9m, 12d]    ok  After recommended range  
dt..........  5  DTP........  12/31/1991  [4y, 0m, 12d]    ok  Recommended age range  
dt..........  X6  Td........  06/24/1999  [11y, 6m, 5d]    x  Extra dose  
HepA........  1  HepA 2dose.  05/16/2005  [17y, 4m, 27d]   ok  Recommended age range  
hepatitisb..  1  HepB.......  07/16/1997  [9y, 6m, 27d]    ok  After recommended range  
hepatitisb..  2  HepB.......  07/17/1998  [10y, 6m, 28d]   ok  After recommended range  
hepatitisb..  3  HepB.......  06/24/1999  [11y, 6m, 5d]    ok  After recommended range  
HIB.........  1  HIB-PRP-T..  06/07/1989  [1y, 5m, 19d]    ok  Recommended age range  
MCV4........  1  MENINGOCOCCAL  05/16/2005  [17y, 4m, 27d]  ok  After recommended range  
Measles.....  1  MMR........  04/12/1989  [1y, 3m, 24d]    ok  Recommended age range  
Measles.....  2  MMR........  08/05/1992  [4y, 7m, 17d]    ok  Recommended age range  
MMR.........  1  MMR........  04/12/1989  [1y, 3m, 24d]    ok  Recommended age range  
MMR.........  2  MMR........  08/05/1992  [4y, 7m, 17d]    ok  Recommended age range  
Mumps.......  1  MMR........  04/12/1989  [1y, 3m, 24d]    ok  Recommended age range  
Mumps.......  X2  MMR.......  08/05/1992  [4y, 7m, 17d]    x  Extra dose  
Polio.......  1  OPV........  03/02/1988  [0y, 2m, 12d]    ok  Recommended age range  
Polio.......  2  OPV........  04/28/1988  [0y, 4m, 9d]     ok  Recommended age range  
Polio.......  3  OPV........  07/07/1988  [0y, 6m, 18d]    ok  Recommended age range  
Polio.......  4  OPV........  06/07/1989  [1y, 5m, 19d]    ok  Minimum interval  
Polio.......  X5  OPV.......  12/31/1991  [4y, 0m, 12d]    x  Extra dose  
Rubella.....  1  MMR........  04/12/1989  [1y, 3m, 24d]    ok  Recommended age range  
Rubella.....  X2  MMR.......  08/05/1992  [4y, 7m, 17d]    x  Extra dose  

OTHER VACCINES:
TYPHOID-PARENTERAL  05/16/2005  

Always check the manufacturer's Prescribing Information sheet for current indications and usage guidelines. VacLogic(tm) ver.VacLogic 3-54 .. © 2006 Visual Data LLC and Amzi! inc, all rights reserved

*/