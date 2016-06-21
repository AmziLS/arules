# ARulesXL

Copyright (c) Amzi! inc. 2016
Licensed under the MIT Open Source License

This is the source code for ARulesXL. The system has two major components.

Prolog - the rule language and reasoning engine used in ARulesXL are written in Prolog, using DCG for the language.  Note that it would be relatively easy to create a new language with the same integration points as the existing one.

Visual Basic for Applications (VBA) - The glue that connects the rule engine and language to spreadsheet cells is written using VBA.  The application is called ARulesXL.xla.

ARulesXL is a perfect example of the sorts of things that can be built using Amzi! Prolog + Logic Server.  The Amzi! Prolog portion is used as a meta-language for creating a new rule language and reasoning engine.  The Logic Server is used to allow easy integration with VBA and thus an Excel spreadsheet.

A separate portion of the source is used to implement various interfaces between an ARulesXL application and other software tools.  So, for example, the Vaccination Logic application is developed and tested in the Excel environment, but is exported and integrated into the development language used for it's host pediatric medical records application.
