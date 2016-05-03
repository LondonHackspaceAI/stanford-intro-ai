This requires the Gambit-C Scheme system, version 4.5.3 (perhaps other
versions will work, too, but then quite likely not.)

This code has only been tested on Debian Linux.

To run:

    cd stanford-intro-ai
    git submodule init
    git submodule update

    cd scheme
    gsc -:tE,dar,t8,f8,-8

To run a search:

    > (treesearch 'Arad 'Bucharest)

With nicer view of the resulting path:

    > (.show (treesearch 'Arad 'Bucharest))

To only run the tests in ai.scm:

    > (run-tests "ai")

To run all the tests:

    > (run-tests)

