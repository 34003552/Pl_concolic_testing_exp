# A Concolic Testing Tool for Logic Programming

The tool uses SWI Prolog to performs concolic testing on logic programs via an interface with Microsoft's SMT solver Z3 to solve constraints.

This tool is just an improvement of the Sophie Fortz's Concolic Testing Tool available here: https://github.com/sfortz/Pl_Concolic_Testing.
To get details about the way it works, you'd better check her repository first then take a look at the "docs" directory into mine.

It has been tested with SWI Prolog 8.0.3, Z3 4.8.8 and gcc/g++ 9.3.0 on Ubuntu 16.04 (32 bits).
It also has been tested with SWI Prolog 8.0.3, Z3 4.8.7 and gcc/g++ 8.2.0 on Windows 10 (32 bits).

### Compiling the SWIPrologZ3 interface

First, you should install SWI Prolog and the SMT solver Z3 (https://github.com/Z3Prover/z3).

Then, you can download or clone the repository, e.g.,

````$ git clone https://github.com/34003552/Pl_concolic_testing_exp.git````

and run one of the following scripts to compile the needed C/C++ source files and build the shared library 'swiplz3':
   - ```build_swiplz3.sh``` on Unix systems to build  ```swiplz3.so```
   - ```build_swiplz3.bat``` on Windows systems to build ```swiplz3.dll```

If the scripts won't work on your system, you may have to adapt them by following these steps:

1) scan each ```swiplz3``` subdirectories in order to:
   - compile all '.c' files using your favorite C compiler (gcc recommended)
   - compile all '.cpp' files using your favorite C++ compiler (g++ recommended)

2) link the resulting '.o' files together with the C++ standard and the Z3 libraries using the SWI Prolog utility program as follows:

````$ swipl-ld -shared -o swiplz3 *.o -lstdc++ -lz3````

Once you have the shared library, you can use its functions by loading the file ```swiplz3.pl``` into your Prolog file using:

````:- use_module(swiplz3).````

The list of its primitives is available in the "docs" directory.
See the section about 'swiplz3_tester' to discover a minimalist use case of that library.

### Installing the Concolic Testing Tool (requires swiplz3)

** interactive mode **

1) run SWI Prolog within the 'concolic_tool' directory

   > cd concolic_tool
   > swipl

2) load in the 'main' program, e.g.,

   > ?- ['main'].

3) try some of the predefined tests from the file 'samples.txt', e.g.,

   > ?- sample(cex1). % load only the sample named 'cex1'
   > ...
   > ?- samples. % load consecutively all known samples

NB: The script 'test.sh' can help you load the 'main' program.

** command mode **

1) run SWI Prolog to build the 'concolic_tool' executable within the 'concolic_tool' directory

   > cd concolic_tool
   > swipl -q --goal=main --toplevel=halt --stand_alone=true -o concolic_tool -c main.pl

2) exit from SWI Prolog and use it from the command line, e.g,

   > ./concolic_tool -vv -cg "p(s(a))" -ground "[1]" -depth "2" -timeout "10" -file "examples/ex01.pl" -trace

   Note that only atomic initial goals are allowed (w.l.o.g)

NB: The script 'test_exe.sh' can help you build and run the 'concolic_tool' executable.

### Experimental tools

##### Swiplz3_tester (requires swiplz3)

-- Description --

The swiplz3 tester can load SMTLIB2 scripts via the swiplz3 module.
It can be used to test individually each of the module primitives.

-- Usage --

1) run SWI Prolog to load the 'swiplz3_tester' program within the 'swiplz3_tester' directory while specifying the script you want to run with, e.g,

   > cd ./extras/swiplz3_tester
   > swipl -l "./swiplz3_tester.pl" -g main -- "./scripts/assert_model.txt"

NB: The script 'test_swiplz3.sh' can help you load the 'swiplz3_tester' program.

Alternatively, if the script you want to load is missing, the tester will let you select another one from its script directory, e.g,

   > |: assert_model.txt

##### Constr2smt_translator

-- Description --

The constr2smt translator can translate constraints into SMT instructions.
It can be used to know how constraints are seen by a SMT solver before solving.

-- Usage --

1) run SWI Prolog to load the 'constr2smt_tester' program within the 'constr2smt_translator' directory, e.g.,

   > cd ./extras/constr2smt_translator
   > swipl -l "./constr2smt_tester.pl" -g main

NB: The script 'test_constr2smt.sh' can help you load the 'constr2smt_tester' program.

-- Example --

If you write a constraint like the following:

   > |: {X}/exists([Y], (X = Y, Y = 1)).

the translator will generate these corresponding SMT instructions:

   > (declare-const B Int)
   > (assert (exists ((A Int)) (and (= B A) (= A 1))))

##### Autotyper

-- Description --

The autotyper can infer type annotations from Prolog programs.
Its goal is to strenghten Prolog typing to make it as strong as in Typed-Prolog.

Warning: This tool is highly experimental so very verbose and potentially unstable!

-- Usage --

1) run SWI Prolog to load the 'autotyper' program within the 'autotyper' directory while specifying the file you want to type with, e.g,

   > cd ./extras/autotyper
   > swipl -l "./autotyper.pl" -g main -- "../../concolic_tool/examples/bench/chat_parser.pl"

NB: The script 'test_autotyper.sh' can help you load the 'autotyper' program.

##### Mercury_loader

-- Description --

The mercury loader can load and run Mercury programs through a Prolog environment.
Its goal is to extend the compatibility of the concolic tool to Mercury programs.
It can be seen as a first step before a wider extension to other non-Prolog based logic programming languages.

Warning: This tool is highly experimental so very verbose and potentially unstable!

-- Usage --

1) run SWI Prolog to load the 'mloader' program within the 'mercury_loader' directory while specifying the file you want to run with, e.g.,

   > cd ./extras/mercury_loader
   > swipl -l "./mloader.pl" -g main -- "../../concolic_tool/examples/mercury/samples/eliza.m"

NB: The script 'test_mloader.sh' can help you load the 'mloader' program.
