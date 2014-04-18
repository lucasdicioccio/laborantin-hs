Changelog
=========

# 0.1.5.1 (2014-04-18)
* widen 'text' dependency to <1.2
* widen 'aeson' dependency to <0.8

# 0.1.5.0 (2014-03-14)
* bump 'base' dependency to support GHC 7.8
* add -c <int> to support concurrent runs
    - race conditions may happen (e.g., a dependency rans twice), recommended
      use: pure statistics on already ran experiments

# 0.1.4.0 (2013-12-14)
* no longer need to explicitly implement all hooks, they default to `return ()`
* add ancestors helper to load and filter existing ancestors in experiment Step
* add UExpr parsing preferences (e.g., for time locale)
    - not used in practice for now in CLI/DSL

# 0.1.3.0 (2013-12-8)
* add TExpr Bool parameter expansions
* add ancestors querying and loading
* add require helper to load/generate ancestors

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
