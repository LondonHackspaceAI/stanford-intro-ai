This requires the Gambit-C Scheme system with some patches as
available [here](https://github.com/pflanze/gambc.git); on a Debian
(perhaps Ubuntu) system it can easily be installed via
[chjize](https://github.com/pflanze/chjize) by following the
instructions there then running `make gambit`.

This code has only been tested on Debian Linux.

To run:

    cd stanford-intro-ai
    git submodule init
    git submodule update

    cd scheme
    gsc -:tE,dar,t8,f8,-8

To run a search:

    > (.show (treesearch links 'Arad 'Bucharest))

With nicer view of the resulting path:

    > (treesearch* links 'Arad 'Bucharest)

To run the tests:

    > (run-tests ".")
