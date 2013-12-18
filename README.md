laborantin-hs
=============

Laborantin is a Haskell framework for running controlled experiments.
It is already quite stable and only few things should change in the near
future. Comments and pull requests are warmly welcome.

# Install

The easiest way to install Laborantin is to use the package published on
hackage.

```sh
  cabal update
  cabal install laborantin-hs
```

Alternatively you can clone this repository with git to get the latest
development version.

```sh
  git clone https://github.com/lucasdicioccio/laborantin-hs	
  cd laborantin-hs
  cabal update
  cabal sandbox init # only if you want a sandboxed install
  cabal install # you can also use cabal configure && cabal build to just build the repo
```

# Two-minutes tutorial

When using Laborantin the typical workflow is as follows:

i) write one or multiple scenarios using the DSL <my-experiment.hs>
ii) compile your application with gch --make -O2 <my-experiment.hs>
iii) run experiments with `./my-experiment run -m "@sc.param 'some-param' in [42, 'toto'] and @sc.param 'other-param' == 1234"`

Example, annotated, code is as follows. Inline comments start with "--". Please
note that the actual implementation of the `executePingCommand` is left as an
exercise.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where
-- import the world
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Laborantin.DSL
import Laborantin.Types
import Laborantin.CLI
import Laborantin.Implementation

-- declare one scenario
ping :: ScenarioDescription EnvIO
ping = scenario "ping" $ do -- start defining a scenario called "ping"
  -- enters the description for this scenario
  describe "ping to a remote server"
  -- declares a first parameter, called "destination".
  -- this parameter has some description for documentation purposes
  -- we should explore two values (strings) by default for this parameter
  parameter "destination" $ do
    describe "a destination server (host or ip)"
    values [str "example.com", str "probecraft.net"]
  -- declares a second parameter, called "packet-size".
  -- we should also explore two values (rational numbers) by default for this
  -- parameter
  parameter "packet-size" $ do
    describe "packet size in bytes"
    values [num 50, num 1500] 

  -- now implement the "run hook", which is the actual code to run
  run $ do
    (StringParam srv) <- param "destination" -- lookup "destination" parameter, it should be a string
    (NumberParam ps) <- param "packet-size" -- lookup "packet-size" parameter, it should be a rational number
    liftIO (executePingCommand srv ps) >>= writeResult "raw-result" -- executes the ping action defined below, and dumps the result into a file called "raw-result"
    where executePingCommand :: Text -> Rational -> IO (Text)
          executePingCommand host packetSize = ...

-- list your scenarios in the defaultMain to get a command-line app
main :: IO ()
main = defaultMain [ping]
```

At first, it looks like Laborantin requires a lot of boilerplate if your
experiment is a one-liner. Nothing is free and this is the small cost you have
to pay.  However you get a handy command-line tool for this cost.

For instance, you get some documentation command: `./my-experiment describe` will output:

```
# Scenario: ping
    ping to a remote server
    4 parameter combinations by default
## Parameters:
### destination
(destination)
    a destination server (host or ip)
    2 values:
    - StringParam "example.com"
    - StringParam "probecraft.net"

### packet-size
(packet-size)
    packet size in bytes
    2 values:
    - NumberParam (50 % 1)
    - NumberParam (1500 % 1)
```

You can run experiments with: `./my-experiment run` (stripped output).

```
backend> execution finished

backend> "preparing ping"
backend> "resolving dependencies"
backend> scenario: "ping"
         rundir: results/ping/81e44c78-4fd8-4ab9-8f97-5494dac646a2
         json-params: {"packet-size":{"val":1500.0,"type":"num"},"destination":{"val":"probecraft.net","type":"string"}}

backend> execution finished
```

Then find where experiments results are located with: `./my-experiment find`.

```
results/ping/2ead949a-ed36-4523-9a9c-7c7e2c22a1b2 ping (Success) {"packet-size":{"val":1500.0,"type":"num"},"destination":{"val":"example.com","type":"string"}}
results/ping/81e44c78-4fd8-4ab9-8f97-5494dac646a2 ping (Success) {"packet-size":{"val":1500.0,"type":"num"},"destination":{"val":"probecraft.net","type":"string"}}
results/ping/866b63e8-d407-442c-bc33-f1fb4e96c2a8 ping (Success) {"packet-size":{"val":50.0,"type":"num"},"destination":{"val":"example.com","type":"string"}}
results/ping/a34826bc-5160-4e12-95cc-12fb5c02fc7b ping (Success) {"packet-size":{"val":50.0,"type":"num"},"destination":{"val":"probecraft.net","type":"string"}}
```

# Discussion

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
    - actually might not be such a good idea, let everyone input %m/%d/%y
* [feature] exports to propose exported files using "show-exports" command
* [feature] labor-script binary for easy-integration of other scripts
  - will need to expand parameter spaces by extending undeclared parameters
* [code] cleanup/share/unshare parts of implementation/laborantin/cli script
