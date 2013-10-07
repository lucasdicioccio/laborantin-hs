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

After each step of analysis, the scientist may want to run extra experiments if
some questions are not fully answered, or if new questions arise. These extra
experiments again ask for the same care in preparing/conducting/analyzing
experiments.

Laborantin is a framework to help you manage experiments, which means reducing
the pain of the iterative process of experimental science.

Laborantin brings the following to the experimenter:
  - a clean DSL to express scenarios, parameters, as well as raw data an
    product analysis
  - an execution engine that runs scenarios, exhausting the parameter space for you
  - a default backend to store executed scenarios data and metadata in the
    filesystem
  - auto-documentation features to later generate simple HTML reports

With releasing Laborantin for free and under an open-source license, our goal
is to make sure your precious expert time is used in productive efforts. This
is not a purely altruistic goal because I want to benefit from your good
science asap.  Another goal of Laborantin is to empower scientists who want to
open their code and datasets more easily than possible nowadays.

# Historical anecdote

A pattern I started with, and that I have seen often while observing my peers
is to encode parameters in filename such as process_A_parameterX1.dat to store
a data result and `process_A_parameterX2.dat`. For instance
`ping_grenouille.com_1500.dat`.  Such a scheme is okay for a first shot but
quickly become impossible to maintain past one parameter, or when you have
special characters in string params.  Indeed, with evolving version of *process
A* into finer and finer refinements a whole genealogy of experiments unrolls
and the number of required result files explodes.  The same effect happens as
the number of result files needed grows or as the number of parameters
increases.  

After failing to manage sound experiments with mere bash scripts encoding
parameters in filenames, an obvious next step is to use a build system such as
`make` to run experiments.  Makefiles help, but they only work for so long.
Make is a build system for managing dependencies in a build process.  I think
you can use `make` to explore a set of parameters, but it looks totally
unnatural to write rules for encoding and decoding parameters in filenames.  In
the end, your make script is barely introspectable. Thus, documenting the
experiments while evolving the set of experiments quickly becomes painful.  You
can try to work around by combining `make` with shell scripts and environment
variables but the coupling between different files becomes so tight that your
experiment project is impossible to maintain and it will fail in absurd and
totally obscure manner.

The only way around managing results for an exploding parameter space is to use
a sort of database, and to let the computer manage the database. If you ever
start encoding parameters values or experiment names in filenames, you are
doing it wrong. It is exactly the same thing that using a spoon to drive a
screw: it works but it is a lot of effort. Laborantin is out, free, and
open-source: use it, fork it, or clone it.

# Roadmap


# TODO

* more doc
* use Text rather than String where appropriate
* defaultMain with run/describe/find/analyze/rm modes
* in-memory (pure?) backend
* dependency annotations
* configuration reader
