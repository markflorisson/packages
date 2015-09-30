# Package Language

Package language based on interfaces. The language simplifies dependency
resolution and ensures unsatisfiability errors don't happen (e.g. due to
conflicting requirements on package dependencies). The system also ensures
type-compatibility between dependencies.

# Get the code

    git clone git@github.com:markflorisson/packages.git

# Install dependencies

Running the code requires GHC (The Glasgow Haskell Compiler) and cabal to be installed:

    cd packages
    cabal sandbox init
    cabal install

# Give it a try!

    runghc Main.hs W-v1.0 test.pkg

# Run the tests

    runghc RunTests.hs
