laborantin-hs
=============

Initially, Laborantin is a Ruby framework for controlling and managing
experiments. Laborantin-Hs is the Haskell port of Laborantin.

# Introduction

Designing scientific experiments is hard. Scientific experiments often must
test an hypothesis such as *does parameter X* influences the outcome of *process A* ?
Experimenters must write code to run the *process A*, a task that may be daunting
when it comes to setting up machines remotely or calling half-a-dozen of shell
scripts to edit configurations such as firewall settings for computer networks.
As a result of spending time to prepare and debug the code for *process A*,
little time is left for writing the code around *parameter X*.

In general, running scientific experiments requires a number of actions such as:
  - writing code for the experiments themselves
  - preparing the system for conducting experiments
  - actually conducting the experiments
  - organizing results for the experiments
  - documenting the experiments
  - analyzing the results of experiments

Analyzing results itself is an experiment because a sound analyses also requires
steps such as:
  - writing code for the analyses themselves
  - actually conducting the analyses
  - organizing results for the analyses
  - [...]

After each analysis step, the scientist may want to run extra experiments if
some questions are not fully answered, or if new questions arise. These extra
experiments again ask for the same care in preparing/conducting/analyzing
experiments.  Laborantin is a framework to help you along this iterative
process.

Laborantin is moving away from Ruby and adopted Haskell for two distinct
reasons.  First, Haskell is a functional programming language and it is easier
to reuse chunk of codes in declarative DSLs (Domain Specific Languages) with
functional programming languages than scripting languages (although Ruby is
great for DSLs too). Second, Haskell has a very powerful type system and it
allows to catch a whole set of bugs at compile time. While I may consider the
first point on DSLs open for debate, this second point is the nail in the
coffin: real-world experiments generally involve a time-consuming phase where
we act on the physical world (e.g., sending hundreds of network packets spaced
in time).  You do not want to lose minutes because a typo crashed your
experiment: it is infuriating and stressful.  You typically cannot write tests
for this type of "effects on the real-world-only" code. Nor it is possible to
mock and write unit tests for the whole world when you are under pressure for
getting results for your research.  Thus, Haskell's opinionated choices to
segregate effectful code from pure code and Haskell's obnoxious type system are
a time saver in code for running experiments.  One drawback of using Haskell is
that Laborantin-Hs needs a compilation phase now (i.e., Laborantin-Hs is more a
library than a command-line utility).  Somehow, I think that the pros far
outweigh the cons.  Plus, it seems possible to write a `labor`-like script
for Laborantin-Hs that will compile the project or call `runhaskell`
underneath.

Laborantin-Hs brings the following to the experimenter:
  - a clean DSL to express scenarios, parameters, as well as raw data an
    product analysis
  - an execution engine that runs scenarios, exhausting the parameter space for you
  - a default backend to store executed scenarios data and metadata in the
    filesystem
  - auto-documentation features to later generate simple HTML reports
  - the full power of Haskell type-system to catch runtime errors at compile time

With releasing Laborantin for free and under an open-source license, our goal
is to make sure that your precious expert time is used in productive efforts. This
is not a purely altruistic goal because I want to benefit from your good
science asap.  Another goal of Laborantin is to empower scientists who want to
open their code and datasets more easily than possible nowadays.

# Historical anecdote

A pattern I started with, and that I have seen often while observing my peers
is to encode parameters in filename such as process_A_parameterX1.dat to store
a data result and `process_A_parameterX2.dat`. For instance
`ping_grenouille.com_1500.dat`.  Such a scheme is okay for a first shot but
quickly becomes opaque past a few parameters, or when you have
special characters in string parameters.  Plus, with evolving version of *process
A* into finer and finer refinements a whole genealogy of experiments unrolls
and the number of required result files explodes. A similar effect happens as
the number of result files needed grows or as the number of parameters
increases.  

After failing to manage sound experiments with mere bash scripts encoding
parameters in filenames, an obvious next step is to use a build system such as
`make` to run experiments.  Makefiles help, but they only work for so long.
Make is a build system for managing dependencies in a build process.  I think
you can use `make` to explore a set of parameters, but it looks totally
unnatural to write rules for encoding and decoding parameters in filenames.  In
the end, your Makefile is barely decipherable and you need the Rosetta stone
to remember what `%<` means because your web search engine will simply ignore
the glyph. Thus, documenting the experiments while evolving the set of
experiments quickly becomes painful.  You can try to work around by combining
`make` with shell scripts and environment variables but the coupling between
different files becomes so tight that your experiment project is impossible to
maintain and it will fail in absurd and totally obscure manner. Similarly, I
let you imagine how annoying it is, when you spent a full day writing and
debugging your Makefile and it turns out you need to spend another day to make
sure `make clean` does not wipe all your results because you had an experiment
crash.

The only way around managing results for an exploding parameter space is to use
a sort of database, and to let the computer manage the database. If you ever
start encoding parameters values or experiment names in filenames, you are
doing it wrong. It is exactly the same as that using a spoon to drive a
screw: it works but it is a lot of effort. Laborantin is out, free, and
open-source: use it, fork it, or clone it.

# Roadmap

For version 0.1.5.x

* [improvement] Use system locale rather than default locale for time parsing
    - actually might not be such a good idea, let everyone input %d/%d/%y
* [feature] exports to propose exported files using "show-exports" command
* [feature] labor-script binary for easy-integration of other scripts
  - will need to expand parameter spaces by extending undeclared parameters
* [code] cleanup/share/unshare parts of implementation/laborantin/cli script
