:- include('opdefs.pro').

:- module(arxlFrench).

% Words used in rules
% dict(NationalLanguageWord, KeyWord).
% alphabetical by KeyWord - note it's second so
% that first argument indexing will find word we're
% looking for faster.

% NOTE - phrase are entered as a list, see
% dict( INHERIT_FROM ) for example.

dict(APRES, AFTER).
dict(apr�s, AFTER).
dict(ET, AND).
dict(suspends, append).
dict(demander, ask).
dict(AVANT, BEFORE).
dict(pile_appels, call_stack).
dict(CONCATENER, CONCATENATE).
dict(concat�ner, CONCATENATE).
dict(COMPTER, COUNT).
dict(COUPER, CUT).
dict(date, date).
dict(datetime, datetime).
dict(JOUR, DAY).
dict(jours, days).
dict(jour, days).
dict(IMPRESSIONDEBUG, DEBUGPRINT).
dict(diff�rence, DIFFERENCE).
dict(�valuer, EVALUATE).
dict(EXISTE, EXISTS).
dict(faux, false).
dict(CHERCHE, FIND).
%dict([CHERCHE, DANS], FIND_IN).
dict(TROUVETOUT, FINDALL).
dict(PREMIER, FIRST).
dict(TousTrouv�s, foundall).
dict(heures, hours).
dict(heure, hours).
dict(DANS, IN).
%dict([HERITER, DE], INHERIT_FROM).
dict(HERITER, INHERIT).
dict(DE, FROM).
dict(INTERSECTION, INTERSECTION).
dict(EST_CONNU, IS_KNOWN).
dict(EST_REGLES, IS_RULESET).
dict(est_r�gles, IS_RULESET).
dict(EST_SOUSENSEMBLE, IS_SUBSET).
dict(DONNEE_A, ITEM_AT).
dict(donn�e_�, ITEM_AT).
dict(DERNIER, LAST).
dict(LISTE, LIST).
dict(ENTRER_DATE , MAKE_DATE).
dict(ENTRER_HEURE , MAKE_DATETIME).
dict(MAXIMUM, MAXIMUM).
dict(MEMBRE, MEMBER).
dict(MINIMUM, MINIMUM).
dict(minutes, minutes).
dict(minute, minutes).
dict(min, minutes).
dict(min, minutes).
dict(MOIS, MONTH).
dict(mois, months).
dict(BOITEMESSAGE, MSGBOX).
dict(SUVANT, NEXT).
dict(PAS, NOT).
dict(maintenant, now).
dict(UNEFOIS, ONETIME).
dict(OU, OR).
dict(PERMUTE, PERMUTE).
dict(PRECEDENT, PRIOR).
dict(pr�c�dent, PRIOR).
dict(quitter , quit).
dict(INVERSE, REVERSE).
dict(secondes, seconds).
dict(seconde, seconds).
dict(sec, seconds).
dict(sec, seconds).
dict(CREE_ENSEMBLE, SET).
dict(TRIER, SORT).
dict(SQL, SQL).
dict(TABLEAU , TABLE).
dict(aujourdhui, today).
dict(vrai, true).
dict(PLAGE, RANGE).
dict(ALORS, THEN).
dict(UNION, UNION).
dict(UTILISANT, USING).
dict(SEMAINE, WEEK).
dict(semaines, weeks).
dict(semaine, weeks).
dict(JOUR_SEMAINE, WEEKDAY).
dict(QUAND, WHEN).
dict(ann�e, year).
dict(ann�es, years).
dict(ann�e, years).

% Messages

text(about_licensed_to, `Enregistr� pour: `).
text(about_maintenance_days_left, `Jours de maintenance restant: `).
text(about_user_name, `Nom utilisateur: `).

text(arules_long_title,
   `ARulesXL -\Aide � la d�cision int�gr�e avec analyse par ordinateur` ).
   
text(arules_short_title,
   `ARulesXL - Inference et Calcul Int�gr�s` ).

text(bad_date_arg(?f, ?gs),
   [`Erreur de logique: Fonction Date `, ?f, ` l'argument n'est pas une date: `, ?gs] ).

text(bad_expression_term,
   `Erreur de saisie dans l'expression` ).

text(bad_function_arg(?f, ?gs),
   [`Erreur de logique: Fonction `, ?f, ` avec argument(s) incompatible(s): `, ?gs] ).

text(bad_goal_list, `Erreur de logique: Mauvaise liste d'objectifs`).

text(bad_isknown_arg(?isknown, ?gs),
   [`Erreur de logique: `, ?isknown, ` l'argument doit �tre un objet/propri�t�: `, ?gs] ).

text(bad_license,
   `Licence ARulesXL invalide` ).

text(bad_list_arg(?f, ?gs),
   [`Erreur de logique: fonction tableau`, ?f, ` l'argument donn� n'est pas un tableau: `, ?gs] ).
   
text(bad_numeric_arg(?f, ?g),
   [`Erreur de logique: fonction num�rique`, ?f, ` l'argument donn� n'est pas num�rique: `, ?gs] ).

text(bad_object_arg(?f, ?gs),
   [`Erreur de logique:�, ?f, l'argument doit �tre un objet/une propri�t�: `, ?gs]).

text(bad_range_arg,
   `Erreur de logique: l'argument donn� pour la fonction n'est pas une table` ).
   
text(bad_range_arg(?os),
   [`Erreur de logique: l'argument donn� pour la fonction n'est pas une table: `, ?os] ).
   
text(bad_rule_body,
   [`Corps de r�gle en erreur, il manque peut-�tre `, ?when] ) :- lookup(?when, WHEN).
   
text(bad_rule_conditions,
   `Erreurs dans les conditions du corps de r�gle` ).

text(beta_expired,
   `Cette version d��valuation a expir�, veuillez installer une nouvelle version.` ).

text(blank_row_ends_table,  
   `La ligne suivant un tableau doit �tre vide`).

text(break_hit,  
   `L'interruption par l'utilisateur a stopp� l'ex�cution`).
   
text(button_cancel, `Annuler`).
text(button_ok, `OK`).

text(callstack(?laststring, ?msg, ?stackstring),
   [?laststring, " ", ?msg, ": pile d'appels= ", ?stackstring] ).

text(cannot_update_rules(?L),
   [`Le jeu de r�gles ne peut pas �tre actualis� avec cette licence: `, ?L] ).

text(control_step, `Etape`).
text(control_title, `Mise au point d�ARulesXL`).
text(control_run, `Marche`).
text(control_quit, `Quitter`).

text(copyright,
   `Copyright(c) 2005-2009 Amzi! inc. tous droits reserv�s` ).
   
text(days(1),
   `1 jour ` ).

text(days(?n),
   [ ?n, ` jours `] ).

text(delete_ok(?x),  
   [`OK pour supprimer le jeu de r�gles`, ?x] ).

text(do_not_display_message_again, `Ne plus afficher ce message `).

text(dtable_header_missing_dot(?t),  
   [`L'en-t�te du tableau de d�cision n'est pas une propri�t� l�gale, le premi�re caract�re doit �tre un point: `, ?t] ).
   
text(edition, `Edition`).

text(error(?msg),
   [`***ERREUR*** `, ?msg] ).
   
text(eval_error(?es),
   [`Erreur de logique: expression non evaluable, peut �tre erreur de types: `, ?es] ).

text(excel_name_conflict(?n),  
   [?n, ` conflit avec les noms de cellules Excel autoris�s`] ).

text(export_error(?e),  
   [`Erreur lors de l'exportation du jeu de r�gles: `, ?e] ).

text(extended_predicate_error(?e),  
   [`Erreur dans la fonction externe: `, ?e ] ).
   
text(fail_rule(?ss, ?m, ?c),
   [?ss, `REGLE A ECHOUE `, ?m, `:`, ?c] ).
text(fail_rule(?ss, ?m, ?c),
   [?ss, `R�gle � �chou� `, ?m, `:`, ?c] ).
      
text(failed_goal(?gs),
   [`Erreur de logique: impossible d'atteindre l'objectif: `, ?gs] ).
   
text(failed_goal(?m, ?os),
   [`Erreur de logique: mpossible d'atteindre l'objectif: `, ?m, `:`, ?os] ).

text(failed_object(?ss, ?m, ?os),
   [?ss, `ECHOUE `, ?m, `:`, ?os] ).
text(failed_object(?ss, ?m, ?os),
   [?ss, `�chou� `, ?m, `:`, ?os] ).
   
text(find_object(?ss, ?m, ?os),
   [?ss, `TROUVE `, ?m, `:`, ?os] ).
text(find_object(?ss, ?m, ?os),
   [?ss, `trouve `, ?m, `:`, ?os] ).
   
text(found_object(?ss, ?m, ?os, ?vs),
   [?ss, `TROUVE `, ?m, `!`, ?os, ` = `, ?vs] ).
text(found_object(?ss, ?m, ?os, ?vs),
   [?ss, `trouv� `, ?m, `!`, ?os, ` = `, ?vs] ).   
   
text(friday, `Vendredi`).

text(fx_rcell_description,  
   `D�finit une propri�t� � la valeur d'une cellule`).

text(fx_rquery_description, 
   `Recherche le jeu de r�gles sp�cifi� � l�aide d�une recherche de forme �FIND.object [QUAND .property1 = _1 ET ... OU ...] o� les valeurs de _1, _2, etc proviennent des cellules indiqu�es`).
   
text(fx_rtable_description, 
   `D�finit un groupe ou une liste pour un jeu de valeurs d�une plage de cellules utilisant �ventuellement des en-t�tes des lignes et colonnes ou des index de colonnes num�riques`).

text(fx_rxldependency_description, 
   `Indique � Excel les jeu de r�gles d�pendant du jeu o� se situe la fonction`).
   
text(hours(1),
   `1 heure ` ).

text(hours(?n),
   [ ?n, ` heures `] ).

text(in_flux(?r), 
   [`Le jeu de r�gles `, ?r, ` est en cours de modification`] ).

text(initializing_rule_sets, `Chargement d�un Jeu de R�gles`).

text(inputing_data, `Saisie des donn�es du jeu de r�gles`).

text(inputs_not_in_headers(?p), 
   [`La propri�t� du r�sultat du tableau de d�cision n�est pas une en-t�te de colonne: `, ?p] ).
   
text(invalid_dtable_header(?h), 
   [`L�en-t�te du tableau de d�cision n�est pas un nom valable: `, ?h] ).

text(invalid_dtable_input(?e), 
   [`L�entr�e de la table de d�cision n�est pas une comparaison ni une expression valable: `, ?e] ).

text(invalid_dtable_output(?e), 
   [`Le r�sultat du tableau de d�cision ne correspond pas � une valeur: `, ?e] ).
   
text(invalid_expression,
   `Expression invalide` ).
   
text(invalid_input_data,
   `Mauvaises donn�es d'entr�e` ).
   
text(invalid_logic_head(?a),
   [`En-t�te de r�gle invalide "`, ?a, `"(...`] ).
   
text(invalid_object_argument,
   `Argument d'objet invalide` ).
   
text(invalid_object_property,
   `Propri�t� d'objet invalide` ).
   
text(invalid_object_reference(?a),
   [`R�f�rence d'objet invalide "`, ?a, `", point manquant possible`] ).
   
text(invalid_query,
   `Requ�te invalide` ).
   
text(invalid_query_goal,
   `Objectif de requ�te invalide` ).
   
text(invalid_rule_action,
   `Action de r�gle invalide` ).
   
text(invalid_rule_condition,
   `Condition de r�gle invalide` ).

text(invalid_rule_set_name(?x),
   [`L�argument `, ?x, ` ne correspond pas � un jeu de r�gles ARulesXL valable`] ).

text(known_source(?srcs, ?m, ?os, ?vs),
   [`DEPUIS `, ?srcs, ` `, ?m, `:`, ?os, ` = `, ?vs] ).
text(known_source(?srcs, ?m, ?os, ?vs),
   [`depuis `, ?srcs, ` `, ?m, `:`, ?os, ` = `, ?vs] ).

text(license_expired, 
   [`La tentative d�ex�cution d�une version d�ArulesXL install�e apr�s la mise � jour a expir�. `,
    `Renouveler la mise � jour sur le site www.arulesxl.com ou utilis�e une version pr�c�dente.`] ).
    
text(license_message(?limit),
   [`Connectez-vous � Internet pour obtenir une cl�. Ce logiciel s'arr�tera apr�s `,
   ?limit, ` utilisation ou jours. Aucune information personnelle ne sera communiqu�e`,
   ` � l'exception de la cl� de licence pour ce PC. Veuillez vous connecter � Internet`,
   ` et cliquez sur Oui pour essayer maintenant ou Non pour r�essayer plus tard.`] ).

text(license_success, `Licence du logiciel install�e correctement.`).

text(license_failure_cannot_connect,
   `Impossible de communiquer avec Internet. Veuillez vous connecter et r�essayer.`).
   
text(license_failure_author,
   `Une cl� de licence ne peut pas �tre obtenue pour ce PC. Veuillez contacter l'auteur de ce logiciel.`).

text(maintenance_expires(?MDL),
   [`La maintenance expire dans `, ?MDL, ` jours.\nMerci de renouveller sur www.arulesxl.com`] ).

text(menu_new,
   `&Nouveau...` ).

text(menu_delete,
   `&Supprimer...` ).
   
text(menu_export, `&Export...` ).

text(menu_load, `Charger les r�gles modifi�es`).

text(menu_open_trace_log, `Ouvir le journal de trace`).

text(menu_options, `&Options...`).

text(menu_rename,
   `&Renommer...` ).
   
text(menu_range_to_rules,
   `S�lection du Jeu de R�gles` ).

text(menu_rules_to_range,
   `Plage du Jeu de R�gles` ).

text(menu_support, `Forum Technique`).

text(menu_check,
   `&V�rifier...` ).

text(menu_debug,
   `&Tester` ).

text(menu_help,
   `&Documentation` ).

text(menu_tutorial,
   `Didacticiel` ).

text(menu_trace, `&Trace`).
   
text(menu_samples,
   `Exemples...` ).
   
text(menu_about,
   `&A Propos` ).

text(minutes(1),
   `1 min ` ).

text(minutes(?n),
   [ ?n, ` mins `] ).

text(missing_paren,
   `Il manque peut-�tre une parenth�se` ).
   
text(missing_paren(?function),
   [`Il manque peut-�tre une parenth�se dans la fonction`, ?f] ) :- lookup(?f, ?function).

text(monday, `Lundi`).

text(months(1),
   `1 mois` ).

text(months(?n),
   [ ?n, ` mois `] ).

text(narrow_dtable, 
   `Le tableau de d�cision requiert plusieurs colonnes`).
   
text(need_square_brackets,
   `L'argument de l'objet n�cessite des crochets` ).

text(new_ruleset_help,
   `Pour ajouter des r�gles, cr�er d�abord un jeu en s�lectionnant ARulesXL | Nouveau jeu de r�gles.` ).

text(no_hardware_fingerprint,
   `Impossible d'obtenir l'empreinte mat�rielle` ).

text(no_license,
   `Cette fonction n�cessite l'Edition Professionnelle - Visitez www.arulesxl.com la mise � jour.`).

text(no_maintenance_days,
   `Impossible d'obtenir l'�ch�ance de la maintenance` ).
   
text(no_user_name,
   `Impossible d'obtenir le nom d'utilisateur` ).

text(no_rule_sets, `Aucun jeu de r�gles d�fini` ).

text(no_spaces_in_name, 
   `Un nom de jeu de r�gles ne peut pas contenir d�espaces`).

text(no_value(?os),
   [`Erreur de logique: impossible de trouver la valeur pour: `, ?os] ).

text(no_vba_value(?v), 
   [`Impossible d�obtenir une valeur pour l��l�ment: `, ?v] ).

text(object_parens(?o),
   [`Objet `, ?o, ` avec des parenth�ses au lieu de crochets`] ).

text(ok, "OK").

text(one_cell_ruleset, 
   `Un jeu de r�gles doit contenir au moins deux cellules (afin de pouvoir en ins�rer de nouvelles`).

text(options_refresh_rules_always, `Rafra�chir les r�gles � chaque modification`).

text(options_refresh_rules_exit,
    `Rafra�chir les r�gles en cliquant � l�ext�rieur du champ Jeu de r�gles`).

text(options_ruleset_style, `Nom du style de jeu de r�gles`).

text(options_selected_ruleset_style, `Nom du style de jeu de r�gles s�lectionn�` ).

text(parse_error(?msg, ?here),
   [?msg, ` pr�s d'ici: "...`, ?ici, `"`] ).

text(query(?m, ?q),
   [`REQUETE `, ?m, `:  `, ?q] ).
text(query(?m, ?q),
   [`requ�te `, ?m, `:  `, ?q] ).

text(range_overlaps, `Des jeux de r�gles ne peuvent pas French`).
   
text(rcell_ruleset, `RCell() doit �tre utilis� dans un jeu de r�gles`).

text(rename_ok(?old, ?new), [`Renommer jeu de r�gles` , ?old, ` en ` , ?new]).

text(rtable_ruleset, `RTable() doit �tre utilis� dans un jeu de r�gles` ).

text(rtable_too_many_rows, 
   `Rtable : Trop de lignes ou colonnes pour un tableau en une dimension`).
   
text(rule_set_errors(?rs),
   [?rs, ` contient des erreurs, se reporter aux commentaires du jeu de r�gles pour plus de d�tails`] ).

text(rule_set_exists(?r),  
   [`Le jeu de r�gles `, ?r, ` est d�j� d�fini dans ce classeur`] ).
   
text(rule_set_name, `Nom du jeu de r�gles (sans espaces).`).

text(rule_set_name_bad,  
   `Nom de jeu de r�gles erron�. Il doit s'agir d'un nom Excel valide (sans d'espaces).`).

text(rule_set_exists(?r),  
   [`Jeu de r�gles `, ?r, ` existe d�j�, choisir un autre nom`] ).
   
text(rule_set_new_help,
   `Apr�s avoir cr�� un jeu de r�gles, vous pouvez saisir une r�gle dans chaque cellule de la bo�te de plage du jeu de r�gles` ).

text(rule_set_not_found, `Jeu de r�gles introuvable`).

text(rule_set_range, `Plage de cellules du jeu de r�gles`).

text(rule_syntax_error,
   `Syntax error in rule` ).

text(rules_not_initialized, `Jeux de r�gles non initialis�s`).

text(runtime_error(?str),
   [`Erreur d'ex�cution: `, ?str] ).

text(runtime_error(?etype, ?msg),
   [`Erreur d'ex�cution: `, ?etype, ` `, ?msg] ).

text(saturday, `Samedi`).

text(seconds(1),
   `1 sec ` ).

text(seconds(?n),
   [ ?n, ` secs `] ).

text(select_delete_cell,  `S�lectionner une cellule du jeu de r�gles pour supprimer`).
   
text(select_rule_set_label, `Nom du jeu de r�geles: `).

text(select_rule_set_title, `Selectionner Jeu de R�gles`).

text(square_logic_bracket(?f),
   [`Si `, ?f, ` est un objet, utiliser une p�riode principale -- si c�est une relation utiliser des parenth�ses, au lieu des crochets `]  ).

text(stack_overflow,
   `D�bordement m�moire, certainement du � une r�f�rence circulaire dans les r�gles`).

/*
text(stack_overflow,
   [`D�bordement m�moire, certainement du � une r�f�rence circulaire dans les r�gles. `,
    `Utiliser le v�rificateur pour rechercher la requ�te fautive. `,
    `Regarder la documentation pour plus d�information.`]  ).
*/

text(sunday, `Dimanche`).

text(syntax_error(?msg),
   [ `Erreur de syntaxe: `, ?msg ] ).

text(tables_first_column,  
   `Les tableaux doivent commencer dans la premi�re colonne du jeu de r�gles`).
   
text(temp_file_error,
   `Erreur lors de la cr�ation et/ou de l'�criture du fichier temporaire`).

text(thursday, `Jeudi`).

text(trace_cannot_end, `Impossible d�interrompre la trace en cours, sortir et red�marrer Excel`).

text(trace_continue_msg,
   `Cliquer sur '�tape' pour continuer le pas � pas, sur 'ex�cuter' pour achever et sur 'fin' pour interrompre la trace` ).

text(trace_data, `Donn�es`).

text(trace_end,  `Fin`).

text(trace_error_select_query_cell,
   `Avant de lancer  la trace, s�lectionnez d'abord une cellule avec requ�te de r�gle`).
   
text(trace_exit, `Succ�s`).

text(trace_fail, `Echec`).

text(trace_false, `Non`).

text(trace_find, `Rechercher`).

text(trace_found, `Trouv�`).

text(trace_help,  `Aide`).

text(trace_known, `Connu`).

text(trace_log_not_found,  
   `Impossible de localiser le journal dans le r�pertoire du classeur en cours`).

text(trace_match, `Correspondance`).

text(trace_nocanuse(?x),
   [?x, `, ne peut pas �tre utilis� lors du  trac�`] ).

text(trace_redo, `R�essayer `).

text(trace_retest, `Recommencer le test`).

text(trace_run, `Ex�cuter`).

text(trace_run_msg,
   `Ex�cution en cours, cliquer sur 'ex�cuter' pour continuer ou sur 'fin' pour interrompre`).

text(trace_skip, `Ignorer`).

text(trace_start_msg, `Cliquer sur '�tape' pour d�marrer la trace`).

text(trace_step, `Etape`).

text(trace_stop_msg, `Trace interrrompue`).

text(trace_test, ` Test`).

text(trace_true, `Oui`).

text(trace_try, `Essayer`).

text(try_rule(?ss, ?m, ?c),
   [?ss, `ESSAYE LA REGLE `, ?m, `:`, ?c] ).
      
text(tuesday, `Mardi`).

text(unacceptable_cell_content, `Le contenu de la cellule n�est pas conforme au jeu de r�gles`).

text(unbound_object(?o),
   [`Tentative d'attribution de propri�t�s � un objet avec variables non li�es, `, ?o] ).

text(unknown_error, `Erreur inconnue`).

text(years(1),
   `1 ann�e` ).

text(years(?n),
   [ ?n, ` ann�es `] ).
   
text(version(?v),
   [`Version `, ?v] ).

text(visit_website, `Visiter le site web`).

text(wednesday, `Mercredi`).

text(weeks(1),
   `1 semaine ` ).

text(weeks(?n),
   [ ?n, ` semaines `] ).

:- end_module(arxlFrench).
