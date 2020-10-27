# Release notes of Ampersand

## Unreleased changes
* [Issue #1070](https://github.com/AmpersandTarski/Ampersand/issues/1070) Remove CLI options dbHost, dbName, dbUser and dbPass. These are part of the prototype framework and can be specified as config or environment variable as described here: https://github.com/AmpersandTarski/prototype/tree/development/config 
* [Issue #1093](https://github.com/AmpersandTarski/Ampersand/issues/1093) Don't download prototype framework from github anymore. The framework must be deployed first via another method (manual, docker or composer+packagist)
  * The following CLI options are removed --force-reinstall-framework, --prototype-framework-version, --customizations

## v4.1.2 (9 october 2020)

* Add devcontainer functionality for developers of Ampersand
* Generate better error when graphviz isn't installed and is required

## v4.1.1 (11 september 2020)

* [Issue #1107](https://github.com/AmpersandTarski/Ampersand/issues/1107) Daemon doesn't correctly ignore invariant violations

## v4.1.0 (14 august 2020)

* [Issue #1067](https://github.com/AmpersandTarski/Ampersand/issues/1067) Docker build push to Docker hub instead of Github package repo
* [Issue #1084](https://github.com/AmpersandTarski/Ampersand/issues/1084) Add template attributes to BOX syntax
* **Breaking change** Because of the implementation of feature of #1084 we could greatly reduce the number of BOX templates (e.g. ROWS, ROWSNL, HROWS and HROWSNL are merged into a single template). Documentation of new templates can be found [here](https://github.com/AmpersandTarski/prototype/tree/master/templates). 
This breaking change presents the opportunity to rename the built-in templates to more self explaining template names:
  * ROWS -> FORM
  * COLS -> TABLE
  * TABS -> TABS (unchanged)
* Update default prototype framework version to v1.6.0, which includes new templates as described above

## v4.0.2 (17 july 2020)

* Small bugfixes and refactorings

## v4.0.1 (19 june 2020)

* [Issue #1026](https://github.com/AmpersandTarski/Ampersand/issues/1026) Allow PATTERNs with the same name. Meaning: all declarations from patterns with the same name are merged into one.
* [Issue #1081](https://github.com/AmpersandTarski/Ampersand/issues/1081) Disable invariant checking for documentation.
* [Issue #988](https://github.com/AmpersandTarski/Ampersand/issues/988) Add switch to disable warnings with `ampersand daemon` command

## v4.0.0 (23 may 2020)

* Refactor Docker image for Ampersand compiler
* Add continuous integration via Github Actions. With cache.
* [Issue #991](https://github.com/AmpersandTarski/Ampersand/issues/991) CLI interface has changed. Changes are documented in [Commands.md](https://github.com/AmpersandTarski/Ampersand/blob/development/commands.md)
* [Issue #1029](https://github.com/AmpersandTarski/Ampersand/issues/1029) Fixed detection of rules with same name.
* [Issue #1047](https://github.com/AmpersandTarski/Ampersand/issues/1047) Non-existing directory is generated automagically when required for output.
* [Issue #999](https://github.com/AmpersandTarski/Ampersand/issues/999) Treat all concepts in a cycle in CLASSIFY statements as aliases of a single concept.
* [Issue #1056](https://github.com/AmpersandTarski/Ampersand/issues/1056) Bugfix in .xlsx parser
* [Issue #1054](https://github.com/AmpersandTarski/Ampersand/issues/1054) Ampersand daemon now also reports type errors
* [Issue #1063](https://github.com/AmpersandTarski/Ampersand/issues/1063) Return violations of invariants with standard check
* [Issue #735](https://github.com/AmpersandTarski/Ampersand/issues/735) Upgrade to pandoc 2.9. 

* Remove option --skip-composer. Relates to topic of [Archicture of Ampersand compiler](https://github.com/AmpersandTarski/Ampersand/issues/903)
* Introduce option --[no-]frontend to do/don't generate frontend (i.e. javascript and html files for Angular app)
* Introduce option --[no-]backend to do/don't generate backend (i.e. json model for php framework)
* Remove default value 'customizations' for option --customizations. Copying folders must be explicitly specified.
* Change default output folder to .proto instead of '<scriptname.adl>.proto' for prototype generation.

## v3.17.5 (3 january 2020)

* Update to prototype framework v1.4.0

## v3.17.4 (13 september 2019)

* Upgrade to [LTS Haskell 14.5 (ghc-8.6.5)](https://www.stackage.org/lts-14.5)

## v3.17.3 (2 august 2019)

* Allow interface roles to be editable. Accessible interfaces for a given role are now queries from database instead of generated json files (requires update of prototype framework)
* Update to prototype framework v1.3.0
* [Issue #921](https://github.com/AmpersandTarski/Ampersand/issues/921) Third round of enhancements for RIO.
* Removed depricated stuff: keywords "EDITS", "PROCESS" and "ENDPROCESS". EDITS did not have any effect at all since about 2015, (END)PROCESS has been the same as (END)PATTERN for a long time.

## v3.17.2 (5 july 2019)

* Again some major refactoring has been done to leverage the [RIO library](https://haskell.fpcomplete.com/library/rio).
* [Issue #976](https://github.com/AmpersandTarski/Ampersand/issues/976) Lines in --deamon configuration file can be commented out by prepending such line with "#" or "--" (MUST be at beginning of line)
* [Issue #971](https://github.com/AmpersandTarski/Ampersand/issues/971) The functionality of meatgrinding has been refactored. It is now far more composable. It is expected that in the near future this will lead to a set of different switches.

## v3.17.1 (7 june 2019)

* A lot of refactoring took place this release, embracing best practices from the RIO library. First phase. In the next couple of releases, we plan to gradually do more of this refactoring.

## v3.17.0 (10 may 2019)

* [Issue #923](https://github.com/AmpersandTarski/Ampersand/issues/923) Lexer error message had disappeared. Now they show again.
* [Issue #925](https://github.com/AmpersandTarski/Ampersand/issues/925) Add warning when a script contains `BOX <ROWSNH>`, for this is deprecated and could result in an error when a prototype is being generated.
* [Issue #578](https://github.com/AmpersandTarski/Ampersand/issues/578) Implement meta model for navigation menu
* [Issue #932](https://github.com/AmpersandTarski/Ampersand/issues/932) `--daemon` now has an optional parameter to tell what config file is used. defaults to `.ampersand`.
* [Issue #903](https://github.com/AmpersandTarski/Ampersand/issues/903) Add switch to skip installing php dependencies (using Composer package manager).
* [Issue #952](https://github.com/AmpersandTarski/Ampersand/issues/952) Fixed bug causing the ampersand daemon to crash when an .xlsx file had an error.
* Upgrade to [prototype framework v1.2.0](https://github.com/AmpersandTarski/Prototype/releases/tag/v1.2.0). Needed for [Issue #578](https://github.com/AmpersandTarski/Ampersand/issues/578)
* [Issue #578](https://github.com/AmpersandTarski/Ampersand/issues/578) Improvements on the way the navigation bar of a prototype works.

## v3.16.0 (8 april 2019) (few days before scedule)

* New switch: --daemon. This enables automatic checking of your scripts as you type. Very nice in combination with the vscode extension [Ampersand (ADL) language support](https://marketplace.visualstudio.com/items?itemName=AmpersandTarski.language-ampersand).
* [Issue #920](https://github.com/AmpersandTarski/Ampersand/issues/920) Warnings are generated for concepts/relations with names that are equal except for the caseing.

## v3.15.0 (15 march 2019)

* Minor bugfixes and enhanced automatic build/release

## v3.14.0 (15 february 2019)

* Hotfix: update to prototype framework v1.1.1. See [releases](https://github.com/AmpersandTarski/Prototype/releases) for more information
* [Issue #890](https://github.com/AmpersandTarski/Ampersand/issues/890) Add static check for subinterfaces. The target concept of it must be Object, not scalar.
* [Issue #894](https://github.com/AmpersandTarski/Ampersand/issues/894) Bugfix issue in concepts.json output for prototype
* [Issue #901](https://github.com/AmpersandTarski/Ampersand/issues/901) Bugfix issue in displaying isa edges in graphical output.

## v3.13.0 (18 january 2019)

* Update prototype framework to v1.1.0. See [releases](https://github.com/AmpersandTarski/Prototype/releases) for more information
* Add 'public' folder in prototype directory to better distinguish between public and non-public scripts. If used, requires change in 'customizations' folder
* [Issue #616](https://github.com/AmpersandTarski/Ampersand/issues/616) Add missing TType 'Object' to parser
* [Issue #792](https://github.com/AmpersandTarski/Ampersand/issues/792) Add possibility to extend TXT in interfaces
* [Issue #862](https://github.com/AmpersandTarski/Ampersand/issues/862) Bugfix in generated SQL in several cases where CLASSIFY statements were involved in combination with relations with the INJ property.
* [Issue #865](https://github.com/AmpersandTarski/Ampersand/issues/865) Another bugfix in the generated SQL
* [Issue #873](https://github.com/AmpersandTarski/Ampersand/issues/873) It is now possible to provide warnings in the output while generating a script. Several warnings are now generated as well when appropriate.
* Only run composer install when clean install of prototype framework is downloaded
* Removed --dev switch as alias of self-explanatory --ignore-invariant-violations
* Improved output of any invariant violations or signals for initial population
* Improved use of compiler when no prototype is requested (reporting violations, testing specific rules and rap population output are possible)
* Improved use of compiler when no script is provided (e.g. for --sampleConfigFile)
* [Issue #879](https://github.com/AmpersandTarski/Ampersand/issues/879) Bugfix in the meatgrinder. Also a big performance win in generating meta-stuff.

## v3.12.0 (21 december 2018)

* [Issue #855](https://github.com/AmpersandTarski/Ampersand/issues/855) Minor enhancement in CLASSIFY statement
* Add semantics to difference in API or INTERFACE keywords.

## v3.11.5 (23 november 2018)

* [Issue #628](https://github.com/AmpersandTarski/Ampersand/issues/628) Fixed a performance issue for specific queries.

## v3.11.4 (27 october 2018)

* Minor changes in the releaseing of ampersand

## v3.11.3 (28 september 2018)

* Bugfix: Figures are available again in the generated specification document.
* [Issue #836](https://github.com/AmpersandTarski/Ampersand/issues/836) Bugfix switch --force-reinstall-framework

## v3.11.2 (31 august 2018)

* [Issue #821](https://github.com/AmpersandTarski/Ampersand/issues/821) Fix error messages.
* Exit codes: code 20 was erroneously called 10. On 8 august 2018 this was discovered and fixed in `Exit.hs` when documenting error codes.

## v3.11.1 (3 august 2018)

* [Issue #814](https://github.com/AmpersandTarski/Ampersand/issues/814) More clear error message.
* [Issue #798](https://github.com/AmpersandTarski/Ampersand/issues/798) Refactor code, to remove some double code.

## v3.11.0 (6 july 2018)

* Upgrade to LTS-11.13
* PHP Backend: implement [PSR6 caching interface](https://www.php-fig.org/psr/psr-6/) for conjunct violation cache
* [Issue #789](https://github.com/AmpersandTarski/Ampersand/issues/789) Introduction `API` keyword as synonym for `INTERFACE`
* [Issue #411](https://github.com/AmpersandTarski/Ampersand/issues/411) Fix issue with conjunct violation cache for src/tgt atom identifiers longer than 255 chars
* Upgrade to pandoc vs 2.0
* [Issue #482](https://github.com/AmpersandTarski/Ampersand/issues/482) Using checksum from generator to check if Ampersand model files are changed
* [Issue #801](https://github.com/AmpersandTarski/Ampersand/issues/801) Fix a problem with empty values in .xlsx files during compilation of an .adl file

## v3.10.1 (8 june 2018)

* [Issue #779](https://github.com/AmpersandTarski/Ampersand/issues/779) Removed GenBericht module (project specific extension which is not used anymore)
* [Issue #482](https://github.com/AmpersandTarski/Ampersand/issues/482) Add checksum for generated Ampersand model files to detect changes

## v3.10.0 (11 may 2018)

* PHP Backend: integrated and refactored ExecEngine (not an extension anymore)
* PHP Backend: integrated and refactored ExcelImporter (not an extension anymore)
* PHP Backend: added functionality to import json population files via UI of ExcelImporter
* PHP Backend: now requires PHP 7.x (5.6 is not supported anymore)
* PHP Backend: added [Pimple](https://pimple.symfony.com/) as Dependency Injection Container library
* PHP Backend: refactored creation of AngularApp, AmpersandApp and MysqlDB using Pimple container
* PHP Backend: mysql database class placed under Plug namespace
* PHP Backend: added function/method parameter and return type specifications
* PHP Backend: update from Slim v2 to v3 api framework
* [Issue #770](https://github.com/AmpersandTarski/Ampersand/issues/770) Add configuration option to specify login page for prototype

## v3.9.5 (15 april 2018)

* [Issue #760](https://github.com/AmpersandTarski/Ampersand/issues/760) Fixed a bug causing wrong results in the prototype.

## v3.9.4 (16 march 2018)

* Better performance of the generator, due to using Haskell's sets rather than lists.
* [Issue #758](https://github.com/AmpersandTarski/Ampersand/issues/758) Fixed a bug that could cause a runtime error in edgecases

## v3.9.3 (16 february 2018)

* [Issue #752](https://github.com/AmpersandTarski/Ampersand/issues/752) Fixed a bug that could cause failure without proper error message
* Better (more uniform) diagnostic comments in SQL output

## v3.9.2 (19 january 2018)

* [Issue #739](https://github.com/AmpersandTarski/Ampersand/issues/739) Better parser error message for mismatch of values
* [Issue #744](https://github.com/AmpersandTarski/Ampersand/issues/744) Better lexer error message for unterminated string
* [Issue #741](https://github.com/AmpersandTarski/Ampersand/issues/741) Solved a bug that could cause invalid database state during runtime of a prototype

## v3.9.1 (22 december 2017)

* Security fix: use PHP session strict mode. This prevents a user defined session ID that is never generated
* [Issue #707](https://github.com/AmpersandTarski/Ampersand/issues/707) Bugfix issue where user input '_SESSION' was also replaced by the session id
* [Issue #723](https://github.com/AmpersandTarski/Ampersand/issues/723) THEMES have been removed from the syntax.
* [Issue #729](https://github.com/AmpersandTarski/Ampersand/issues/729) Bugfix issue where REPRESENT statements with DATE, DATETIME etc. is involved
* [Issue #732](https://github.com/AmpersandTarski/Ampersand/issues/732) Create own prelude, to guarantee correct use of UTF8 everywhere in the Haskell code

## v3.9.0 (24 november 2017)

It has taken some time since the last release. This release has some major work in it, and also a change of syntax of scripts. Unfortunately this is not downward compatible.

* Introduction of PrototypeContext.adl, which will be required for new interface generation, which will become available in some future release
* Upgraded to LTS-9.2
* [Issue #713](https://github.com/AmpersandTarski/Ampersand/issues/713) New syntax for singleton expressions

## v3.8.8 (1 september 2017)

* Somewhat more verbose test output
* [Issue #692](https://github.com/AmpersandTarski/Ampersand/issues/692) Better allignment between Formal Ampersand and meatgrinder
* [Issue #693](https://github.com/AmpersandTarski/Ampersand/issues/693) Fixed a bug about cyclic defined CLASSIFY statements

## v3.8.7 (4 august 2017)

* Upgraded to LTS-8.22
* Made the meatgrinder do better static analisys, inspired by issue #684

## v3.8.6 (7 july 2017)

* [Issue #674](https://github.com/AmpersandTarski/Ampersand/issues/674) New feature for ExecEngine to navigate user to other interface
* [Issue #675](https://github.com/AmpersandTarski/Ampersand/issues/675) Extend scope of _NEW in ExecEngine from NewStruct to complete violation statement

## v3.8.5 (12 may 2017)

* [Issue #666](https://github.com/AmpersandTarski/Ampersand/issues/666) A hint message is supplied when a case-incorrect url is used.
* Upgrade LTS Haskell 8.5, which includes GHC 8.0.2

## v3.8.4 (not released, there was nothing new in april 2017)

## v3.8.3 (17 march 2017)

* Minor modifications on frontend

## v3.8.2 (17 february 2017)

* [Issue #621](https://github.com/AmpersandTarski/Ampersand/issues/621) Disabled --fpa-excel switch, because it resulted in a fatal error.
* [Issue #617](https://github.com/AmpersandTarski/Ampersand/issues/617) Minor fix in functional design document.
* [Issue #624](https://github.com/AmpersandTarski/Ampersand/issues/624) New feature: ExecEngine can merge atoms to fix violations of univalence and other identity violations.
* [Issue #625](https://github.com/AmpersandTarski/Ampersand/issues/625) Comparison of origins now based on canonicalized paths
* [Issue #627](https://github.com/AmpersandTarski/Ampersand/issues/627) Fixed a bug in generation of queries for frontend

## v3.8.1 (20 january 2017)

* [Issue #605](https://github.com/AmpersandTarski/Ampersand/issues/605) Added modules "Modules.adl" and "Patterns.adl" in FormalAmpersand as preparatory work for issue #605.
* [Issue #603](https://github.com/AmpersandTarski/Ampersand/issues/603) Removed obsolete switches about what normalization is used.
* [Issue #600](https://github.com/AmpersandTarski/Ampersand/issues/600) When a configuration file exists (.yaml) with the same name as the file being compiled, it will be used if no config file is explicitly mentioned in the command.
* Renamed "functional specification" to "functional design" for more realistic expectations with users.
* Restructured internal structure of modules of Ampersand.

## v3.8.0 (20 december 2016)

* [Issue #587](https://github.com/AmpersandTarski/Ampersand/issues/587) There is no need any longer to explicitly specify the language in a script. If omitted, IN ENGLISH will be used as default.
* [Issue #588](https://github.com/AmpersandTarski/Ampersand/issues/588) PURPOSE sytax changed: Now matching brackets are: `{+` and `+}`   (no backwards compatibility, to enable nested comment blocks)
* Added some additional diagnosis info in settings.json
* Added support for project/application and extension specific composer dependencies.
* Added 'customization' folder to prototype generation process. This folder can be used to e.g. overwrite generated views.
* Includes for frontend app must be placed in 'app' folder now. The include folder thereby directly matches the destination directory structure.
* Ampersand version info is printed in verbose mode
* New switch, to add all relations, concepts and generalisation relations of formal ampersand into your script: --add-semantic-metamodel

## v3.7.3 (25 november 2016)

* Alternative definition of univalence and injectivity to get better violations on runtime.

## v3.7.2 (28 october 2016)

* Some updates in the meatgrinder. (still experimental)
* Several changes in the generation of the functional design document. (Less LaTex specific)

## v3.7.1 (30 september 2016)

* Upgrade version of Haskell compiler to 8.0.1 (automatically used via Stack)
* Progress on meatgrinder. Basic meatgrinder functionality is available now.
* Several bugfixes

## v3.7.0 (2 september 2016)

* [Issue #506](https://github.com/AmpersandTarski/Ampersand/issues/506) Bugfix in chapter diagnosis of Func. spec.
* [Issue #483](https://github.com/AmpersandTarski/Ampersand/issues/483) Introduction of proper way to produce exit codes of Ampersand
* [Issue #502](https://github.com/AmpersandTarski/Ampersand/issues/502) More stable way to generate .pdf file (LaTeX output)
* [Issue  #72](https://github.com/AmpersandTarski/Ampersand/issues/72) Symmetry of relations will be checked by a generated application
* [Issue #256](https://github.com/AmpersandTarski/Ampersand/issues/256) Surjectivity of relations will be checked by a generated application
* [Issue #345](https://github.com/AmpersandTarski/Ampersand/issues/345) Surjectivity of relations will be checked by a generated application

## v3.6.1 (5 august 2016)

* [Issue #488](https://github.com/AmpersandTarski/Ampersand/issues/488) Performance enhancement: Added indexes on table columns when possible
* [Issue #486](https://github.com/AmpersandTarski/Ampersand/issues/486) Performance enhancement: Removed DISTINCT in subqueries
* [Issue #459](https://github.com/AmpersandTarski/Ampersand/issues/459) fix for underscores in Concept name
* [Issue #489](https://github.com/AmpersandTarski/Ampersand/issues/489) Implemented markdown for rule violation messages in frontend
* [Issue #412](https://github.com/AmpersandTarski/Ampersand/issues/412) Partial fix for removing rows from COLS template
* [Issue #373](https://github.com/AmpersandTarski/Ampersand/issues/373) Fix breadcrumb
* ExecEngine extension: improved logging for debugging
* Added new reporting functionality for backend framework
* DB performance: less queries because 'I[Concept]'-expression is not queried anymore
* Minor backend fixes
* Fix issues regarding CRUD specifications: missing interfaces in navbar +menu, missing crudR check in templates, return content after create

## v3.6.0 (8 july 2016)

* [Issue #406](https://github.com/AmpersandTarski/Ampersand/issues/406) Minor changes on syntax of INTERFACE statement.
* [Issue #438](https://github.com/AmpersandTarski/Ampersand/issues/438) New switch: `--include`
* [Issue #438](https://github.com/AmpersandTarski/Ampersand/issues/438) Enhancement: Introduction of a configuration file. A sample configuration file is generated when you use the switch `--sampleConfigFile`. To use a config file, use the switch `--config=MyConfig.yaml`
* [Issue #468](https://github.com/AmpersandTarski/Ampersand/issues/468) Enhancement: default configuration file (when it exists)
* [Issue #414](https://github.com/AmpersandTarski/Ampersand/issues/414) Enhancement: From now on, all text values read from .xlsx files are trimmed (leading and trailing spaces are removed), unless the switch --do-not-trim-cellvalues is given.
* More automation on releasing Ampersand.
* Frontend: Options in frontend navbar now can be defined for certain roles only (e.g. the installer and excelimporter).
* [Issue #103](https://github.com/AmpersandTarski/Ampersand/issues/103) Fix overlap by multiple rows in navbar
* [Issue #423](https://github.com/AmpersandTarski/Ampersand/issues/423) Database is automatically installed when it does not exists yet (first time use)

## v3.5.2 (10 juni 2016)

* Work on meatgrinder (still experimental!)
* Bug fix: Issue with SQL query [Issue #152](https://github.com/AmpersandTarski/Ampersand/issues/152)
* Bug fix: minor issue with SQL query [Issue #436](https://github.com/AmpersandTarski/Ampersand/issues/436)
* Bug fix: Nontermination of functional document generator. [Issue #231](https://github.com/AmpersandTarski/Ampersand/issues/231)
* SQL query performance improvements: [Issue #426](https://github.com/AmpersandTarski/Ampersand/issues/426) and [Issue #217](https://github.com/AmpersandTarski/Ampersand/issues/217)
* Back end performance: Postpone calculation of view and label for Atoms untill really needed (e.g. in interfaces)
* Added frontend switch to turn on/off auto saving changes

## v3.5.1 (17 may 2016)

* Minor enhancement of generation of Logical Data Model
* More consisten use of views in interface definitions: [Issue #416](https://github.com/AmpersandTarski/Ampersand/issues/416)
* Re-enabled output format for `--fSpec=asciidoc`
* Added depth parameter in API resources call (?depth=\<int\>). This provides functionality to specify the depth of subinterfaces for which the content must be returned and is especially usefull for recursive (sub)interfaces using the LINKTO statement.
* Added this release notes file.
* Bug fixes: [Issue #413](https://github.com/AmpersandTarski/Ampersand/issues/413)

## v3.5.0 (28 apr 2016)

This is the first release of 2016. We are happy announce that we have much more possibilities on creating nice interfaces for the prototype application. Apart from that, lots of bugfixes have been made, and some other features have been introduced.

## releases prior to v3.5.0

We didnt make proper release notes. If you are interested, have a look at the Git history.
