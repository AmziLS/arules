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
dict(après, AFTER).
dict(ET, AND).
dict(suspends, append).
dict(demander, ask).
dict(AVANT, BEFORE).
dict(pile_appels, call_stack).
dict(CONCATENER, CONCATENATE).
dict(concaténer, CONCATENATE).
dict(COMPTER, COUNT).
dict(COUPER, CUT).
dict(date, date).
dict(datetime, datetime).
dict(JOUR, DAY).
dict(jours, days).
dict(jour, days).
dict(IMPRESSIONDEBUG, DEBUGPRINT).
dict(différence, DIFFERENCE).
dict(évaluer, EVALUATE).
dict(EXISTE, EXISTS).
dict(faux, false).
dict(CHERCHE, FIND).
%dict([CHERCHE, DANS], FIND_IN).
dict(TROUVETOUT, FINDALL).
dict(PREMIER, FIRST).
dict(TousTrouvés, foundall).
dict(heures, hours).
dict(heure, hours).
dict(DANS, IN).
%dict([HERITER, DE], INHERIT_FROM).
dict(HERITER, INHERIT).
dict(DE, FROM).
dict(INTERSECTION, INTERSECTION).
dict(EST_CONNU, IS_KNOWN).
dict(EST_REGLES, IS_RULESET).
dict(est_règles, IS_RULESET).
dict(EST_SOUSENSEMBLE, IS_SUBSET).
dict(DONNEE_A, ITEM_AT).
dict(donnée_à, ITEM_AT).
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
dict(précédent, PRIOR).
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
dict(année, year).
dict(années, years).
dict(année, years).

% Messages

text(about_licensed_to, `Enregistré pour: `).
text(about_maintenance_days_left, `Jours de maintenance restant: `).
text(about_user_name, `Nom utilisateur: `).

text(arules_long_title,
   `ARulesXL -\Aide à la décision intégrée avec analyse par ordinateur` ).
   
text(arules_short_title,
   `ARulesXL - Inference et Calcul Intégrés` ).

text(bad_date_arg(?f, ?gs),
   [`Erreur de logique: Fonction Date `, ?f, ` l'argument n'est pas une date: `, ?gs] ).

text(bad_expression_term,
   `Erreur de saisie dans l'expression` ).

text(bad_function_arg(?f, ?gs),
   [`Erreur de logique: Fonction `, ?f, ` avec argument(s) incompatible(s): `, ?gs] ).

text(bad_goal_list, `Erreur de logique: Mauvaise liste d'objectifs`).

text(bad_isknown_arg(?isknown, ?gs),
   [`Erreur de logique: `, ?isknown, ` l'argument doit être un objet/propriété: `, ?gs] ).

text(bad_license,
   `Licence ARulesXL invalide` ).

text(bad_list_arg(?f, ?gs),
   [`Erreur de logique: fonction tableau`, ?f, ` l'argument donné n'est pas un tableau: `, ?gs] ).
   
text(bad_numeric_arg(?f, ?g),
   [`Erreur de logique: fonction numérique`, ?f, ` l'argument donné n'est pas numérique: `, ?gs] ).

text(bad_object_arg(?f, ?gs),
   [`Erreur de logique:’, ?f, l'argument doit être un objet/une propriété: `, ?gs]).

text(bad_range_arg,
   `Erreur de logique: l'argument donné pour la fonction n'est pas une table` ).
   
text(bad_range_arg(?os),
   [`Erreur de logique: l'argument donné pour la fonction n'est pas une table: `, ?os] ).
   
text(bad_rule_body,
   [`Corps de règle en erreur, il manque peut-être `, ?when] ) :- lookup(?when, WHEN).
   
text(bad_rule_conditions,
   `Erreurs dans les conditions du corps de règle` ).

text(beta_expired,
   `Cette version d’évaluation a expiré, veuillez installer une nouvelle version.` ).

text(blank_row_ends_table,  
   `La ligne suivant un tableau doit être vide`).

text(break_hit,  
   `L'interruption par l'utilisateur a stoppé l'exécution`).
   
text(button_cancel, `Annuler`).
text(button_ok, `OK`).

text(callstack(?laststring, ?msg, ?stackstring),
   [?laststring, " ", ?msg, ": pile d'appels= ", ?stackstring] ).

text(cannot_update_rules(?L),
   [`Le jeu de règles ne peut pas être actualisé avec cette licence: `, ?L] ).

text(control_step, `Etape`).
text(control_title, `Mise au point d’ARulesXL`).
text(control_run, `Marche`).
text(control_quit, `Quitter`).

text(copyright,
   `Copyright(c) 2005-2009 Amzi! inc. tous droits reservés` ).
   
text(days(1),
   `1 jour ` ).

text(days(?n),
   [ ?n, ` jours `] ).

text(delete_ok(?x),  
   [`OK pour supprimer le jeu de règles`, ?x] ).

text(do_not_display_message_again, `Ne plus afficher ce message `).

text(dtable_header_missing_dot(?t),  
   [`L'en-tête du tableau de décision n'est pas une propriété légale, le première caractère doit être un point: `, ?t] ).
   
text(edition, `Edition`).

text(error(?msg),
   [`***ERREUR*** `, ?msg] ).
   
text(eval_error(?es),
   [`Erreur de logique: expression non evaluable, peut être erreur de types: `, ?es] ).

text(excel_name_conflict(?n),  
   [?n, ` conflit avec les noms de cellules Excel autorisés`] ).

text(export_error(?e),  
   [`Erreur lors de l'exportation du jeu de règles: `, ?e] ).

text(extended_predicate_error(?e),  
   [`Erreur dans la fonction externe: `, ?e ] ).
   
text(fail_rule(?ss, ?m, ?c),
   [?ss, `REGLE A ECHOUE `, ?m, `:`, ?c] ).
text(fail_rule(?ss, ?m, ?c),
   [?ss, `Règle à échoué `, ?m, `:`, ?c] ).
      
text(failed_goal(?gs),
   [`Erreur de logique: impossible d'atteindre l'objectif: `, ?gs] ).
   
text(failed_goal(?m, ?os),
   [`Erreur de logique: mpossible d'atteindre l'objectif: `, ?m, `:`, ?os] ).

text(failed_object(?ss, ?m, ?os),
   [?ss, `ECHOUE `, ?m, `:`, ?os] ).
text(failed_object(?ss, ?m, ?os),
   [?ss, `échoué `, ?m, `:`, ?os] ).
   
text(find_object(?ss, ?m, ?os),
   [?ss, `TROUVE `, ?m, `:`, ?os] ).
text(find_object(?ss, ?m, ?os),
   [?ss, `trouve `, ?m, `:`, ?os] ).
   
text(found_object(?ss, ?m, ?os, ?vs),
   [?ss, `TROUVE `, ?m, `!`, ?os, ` = `, ?vs] ).
text(found_object(?ss, ?m, ?os, ?vs),
   [?ss, `trouvé `, ?m, `!`, ?os, ` = `, ?vs] ).   
   
text(friday, `Vendredi`).

text(fx_rcell_description,  
   `Définit une propriété à la valeur d'une cellule`).

text(fx_rquery_description, 
   `Recherche le jeu de règles spécifié à l’aide d’une recherche de forme ‘FIND.object [QUAND .property1 = _1 ET ... OU ...] où les valeurs de _1, _2, etc proviennent des cellules indiquées`).
   
text(fx_rtable_description, 
   `Définit un groupe ou une liste pour un jeu de valeurs d’une plage de cellules utilisant éventuellement des en-têtes des lignes et colonnes ou des index de colonnes numériques`).

text(fx_rxldependency_description, 
   `Indique à Excel les jeu de règles dépendant du jeu où se situe la fonction`).
   
text(hours(1),
   `1 heure ` ).

text(hours(?n),
   [ ?n, ` heures `] ).

text(in_flux(?r), 
   [`Le jeu de règles `, ?r, ` est en cours de modification`] ).

text(initializing_rule_sets, `Chargement d’un Jeu de Règles`).

text(inputing_data, `Saisie des données du jeu de règles`).

text(inputs_not_in_headers(?p), 
   [`La propriété du résultat du tableau de décision n’est pas une en-tête de colonne: `, ?p] ).
   
text(invalid_dtable_header(?h), 
   [`L’en-tête du tableau de décision n’est pas un nom valable: `, ?h] ).

text(invalid_dtable_input(?e), 
   [`L’entrée de la table de décision n’est pas une comparaison ni une expression valable: `, ?e] ).

text(invalid_dtable_output(?e), 
   [`Le résultat du tableau de décision ne correspond pas à une valeur: `, ?e] ).
   
text(invalid_expression,
   `Expression invalide` ).
   
text(invalid_input_data,
   `Mauvaises données d'entrée` ).
   
text(invalid_logic_head(?a),
   [`En-tête de règle invalide "`, ?a, `"(...`] ).
   
text(invalid_object_argument,
   `Argument d'objet invalide` ).
   
text(invalid_object_property,
   `Propriété d'objet invalide` ).
   
text(invalid_object_reference(?a),
   [`Référence d'objet invalide "`, ?a, `", point manquant possible`] ).
   
text(invalid_query,
   `Requête invalide` ).
   
text(invalid_query_goal,
   `Objectif de requête invalide` ).
   
text(invalid_rule_action,
   `Action de règle invalide` ).
   
text(invalid_rule_condition,
   `Condition de règle invalide` ).

text(invalid_rule_set_name(?x),
   [`L’argument `, ?x, ` ne correspond pas à un jeu de règles ARulesXL valable`] ).

text(known_source(?srcs, ?m, ?os, ?vs),
   [`DEPUIS `, ?srcs, ` `, ?m, `:`, ?os, ` = `, ?vs] ).
text(known_source(?srcs, ?m, ?os, ?vs),
   [`depuis `, ?srcs, ` `, ?m, `:`, ?os, ` = `, ?vs] ).

text(license_expired, 
   [`La tentative d’exécution d’une version d’ArulesXL installée après la mise à jour a expiré. `,
    `Renouveler la mise à jour sur le site www.arulesxl.com ou utilisée une version précédente.`] ).
    
text(license_message(?limit),
   [`Connectez-vous à Internet pour obtenir une clé. Ce logiciel s'arrêtera après `,
   ?limit, ` utilisation ou jours. Aucune information personnelle ne sera communiquée`,
   ` à l'exception de la clé de licence pour ce PC. Veuillez vous connecter à Internet`,
   ` et cliquez sur Oui pour essayer maintenant ou Non pour réessayer plus tard.`] ).

text(license_success, `Licence du logiciel installée correctement.`).

text(license_failure_cannot_connect,
   `Impossible de communiquer avec Internet. Veuillez vous connecter et réessayer.`).
   
text(license_failure_author,
   `Une clé de licence ne peut pas être obtenue pour ce PC. Veuillez contacter l'auteur de ce logiciel.`).

text(maintenance_expires(?MDL),
   [`La maintenance expire dans `, ?MDL, ` jours.\nMerci de renouveller sur www.arulesxl.com`] ).

text(menu_new,
   `&Nouveau...` ).

text(menu_delete,
   `&Supprimer...` ).
   
text(menu_export, `&Export...` ).

text(menu_load, `Charger les règles modifiées`).

text(menu_open_trace_log, `Ouvir le journal de trace`).

text(menu_options, `&Options...`).

text(menu_rename,
   `&Renommer...` ).
   
text(menu_range_to_rules,
   `Sélection du Jeu de Règles` ).

text(menu_rules_to_range,
   `Plage du Jeu de Règles` ).

text(menu_support, `Forum Technique`).

text(menu_check,
   `&Vérifier...` ).

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
   `Il manque peut-être une parenthèse` ).
   
text(missing_paren(?function),
   [`Il manque peut-être une parenthèse dans la fonction`, ?f] ) :- lookup(?f, ?function).

text(monday, `Lundi`).

text(months(1),
   `1 mois` ).

text(months(?n),
   [ ?n, ` mois `] ).

text(narrow_dtable, 
   `Le tableau de décision requiert plusieurs colonnes`).
   
text(need_square_brackets,
   `L'argument de l'objet nécessite des crochets` ).

text(new_ruleset_help,
   `Pour ajouter des règles, créer d’abord un jeu en sélectionnant ARulesXL | Nouveau jeu de règles.` ).

text(no_hardware_fingerprint,
   `Impossible d'obtenir l'empreinte matérielle` ).

text(no_license,
   `Cette fonction nécessite l'Edition Professionnelle - Visitez www.arulesxl.com la mise à jour.`).

text(no_maintenance_days,
   `Impossible d'obtenir l'échéance de la maintenance` ).
   
text(no_user_name,
   `Impossible d'obtenir le nom d'utilisateur` ).

text(no_rule_sets, `Aucun jeu de règles défini` ).

text(no_spaces_in_name, 
   `Un nom de jeu de règles ne peut pas contenir d’espaces`).

text(no_value(?os),
   [`Erreur de logique: impossible de trouver la valeur pour: `, ?os] ).

text(no_vba_value(?v), 
   [`Impossible d’obtenir une valeur pour l’élément: `, ?v] ).

text(object_parens(?o),
   [`Objet `, ?o, ` avec des parenthèses au lieu de crochets`] ).

text(ok, "OK").

text(one_cell_ruleset, 
   `Un jeu de règles doit contenir au moins deux cellules (afin de pouvoir en insérer de nouvelles`).

text(options_refresh_rules_always, `Rafraîchir les règles à chaque modification`).

text(options_refresh_rules_exit,
    `Rafraîchir les règles en cliquant à l’extérieur du champ Jeu de règles`).

text(options_ruleset_style, `Nom du style de jeu de règles`).

text(options_selected_ruleset_style, `Nom du style de jeu de règles sélectionné` ).

text(parse_error(?msg, ?here),
   [?msg, ` près d'ici: "...`, ?ici, `"`] ).

text(query(?m, ?q),
   [`REQUETE `, ?m, `:  `, ?q] ).
text(query(?m, ?q),
   [`requête `, ?m, `:  `, ?q] ).

text(range_overlaps, `Des jeux de règles ne peuvent pas French`).
   
text(rcell_ruleset, `RCell() doit être utilisé dans un jeu de règles`).

text(rename_ok(?old, ?new), [`Renommer jeu de règles` , ?old, ` en ` , ?new]).

text(rtable_ruleset, `RTable() doit être utilisé dans un jeu de règles` ).

text(rtable_too_many_rows, 
   `Rtable : Trop de lignes ou colonnes pour un tableau en une dimension`).
   
text(rule_set_errors(?rs),
   [?rs, ` contient des erreurs, se reporter aux commentaires du jeu de règles pour plus de détails`] ).

text(rule_set_exists(?r),  
   [`Le jeu de règles `, ?r, ` est déjà défini dans ce classeur`] ).
   
text(rule_set_name, `Nom du jeu de règles (sans espaces).`).

text(rule_set_name_bad,  
   `Nom de jeu de règles erroné. Il doit s'agir d'un nom Excel valide (sans d'espaces).`).

text(rule_set_exists(?r),  
   [`Jeu de règles `, ?r, ` existe déjà, choisir un autre nom`] ).
   
text(rule_set_new_help,
   `Après avoir créé un jeu de règles, vous pouvez saisir une règle dans chaque cellule de la boîte de plage du jeu de règles` ).

text(rule_set_not_found, `Jeu de règles introuvable`).

text(rule_set_range, `Plage de cellules du jeu de règles`).

text(rule_syntax_error,
   `Syntax error in rule` ).

text(rules_not_initialized, `Jeux de règles non initialisés`).

text(runtime_error(?str),
   [`Erreur d'exécution: `, ?str] ).

text(runtime_error(?etype, ?msg),
   [`Erreur d'exécution: `, ?etype, ` `, ?msg] ).

text(saturday, `Samedi`).

text(seconds(1),
   `1 sec ` ).

text(seconds(?n),
   [ ?n, ` secs `] ).

text(select_delete_cell,  `Sélectionner une cellule du jeu de règles pour supprimer`).
   
text(select_rule_set_label, `Nom du jeu de règeles: `).

text(select_rule_set_title, `Selectionner Jeu de Règles`).

text(square_logic_bracket(?f),
   [`Si `, ?f, ` est un objet, utiliser une période principale -- si c’est une relation utiliser des parenthèses, au lieu des crochets `]  ).

text(stack_overflow,
   `Débordement mémoire, certainement du à une référence circulaire dans les règles`).

/*
text(stack_overflow,
   [`Débordement mémoire, certainement du à une référence circulaire dans les règles. `,
    `Utiliser le vérificateur pour rechercher la requête fautive. `,
    `Regarder la documentation pour plus d’information.`]  ).
*/

text(sunday, `Dimanche`).

text(syntax_error(?msg),
   [ `Erreur de syntaxe: `, ?msg ] ).

text(tables_first_column,  
   `Les tableaux doivent commencer dans la première colonne du jeu de règles`).
   
text(temp_file_error,
   `Erreur lors de la création et/ou de l'écriture du fichier temporaire`).

text(thursday, `Jeudi`).

text(trace_cannot_end, `Impossible d’interrompre la trace en cours, sortir et redémarrer Excel`).

text(trace_continue_msg,
   `Cliquer sur 'étape' pour continuer le pas à pas, sur 'exécuter' pour achever et sur 'fin' pour interrompre la trace` ).

text(trace_data, `Données`).

text(trace_end,  `Fin`).

text(trace_error_select_query_cell,
   `Avant de lancer  la trace, sélectionnez d'abord une cellule avec requête de règle`).
   
text(trace_exit, `Succès`).

text(trace_fail, `Echec`).

text(trace_false, `Non`).

text(trace_find, `Rechercher`).

text(trace_found, `Trouvé`).

text(trace_help,  `Aide`).

text(trace_known, `Connu`).

text(trace_log_not_found,  
   `Impossible de localiser le journal dans le répertoire du classeur en cours`).

text(trace_match, `Correspondance`).

text(trace_nocanuse(?x),
   [?x, `, ne peut pas être utilisé lors du  tracé`] ).

text(trace_redo, `Réessayer `).

text(trace_retest, `Recommencer le test`).

text(trace_run, `Exécuter`).

text(trace_run_msg,
   `Exécution en cours, cliquer sur 'exécuter' pour continuer ou sur 'fin' pour interrompre`).

text(trace_skip, `Ignorer`).

text(trace_start_msg, `Cliquer sur 'étape' pour démarrer la trace`).

text(trace_step, `Etape`).

text(trace_stop_msg, `Trace interrrompue`).

text(trace_test, ` Test`).

text(trace_true, `Oui`).

text(trace_try, `Essayer`).

text(try_rule(?ss, ?m, ?c),
   [?ss, `ESSAYE LA REGLE `, ?m, `:`, ?c] ).
      
text(tuesday, `Mardi`).

text(unacceptable_cell_content, `Le contenu de la cellule n’est pas conforme au jeu de règles`).

text(unbound_object(?o),
   [`Tentative d'attribution de propriétés à un objet avec variables non liées, `, ?o] ).

text(unknown_error, `Erreur inconnue`).

text(years(1),
   `1 année` ).

text(years(?n),
   [ ?n, ` années `] ).
   
text(version(?v),
   [`Version `, ?v] ).

text(visit_website, `Visiter le site web`).

text(wednesday, `Mercredi`).

text(weeks(1),
   `1 semaine ` ).

text(weeks(?n),
   [ ?n, ` semaines `] ).

:- end_module(arxlFrench).
