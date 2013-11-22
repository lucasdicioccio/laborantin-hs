Changelog
=========

# 0.1.2.0 (2013-11-22)
* change timestamp format (data-incompatible) from custom to UTCTime
* add TExpr and UExpr module
    - add possibility to filter results using a query

# 0.1.1.0 (2013-10-20)
* add mini-command line in defaultMain with support for:
    - help: print basic help
    - describe: prints info on available scenarios
    - find: prints a list (possibly filtered) of already run scenarios
    - run: runs some scenario (possibly filtered by scenario/param)
    - analyze: plays the 'analyze' hook for each scenario
    - rm: deletes scenarios using a query

# 0.1.0.0
* initial release
