# hsreduce-test-cases

This is a repo containing test cases for evaluating a yet unreleased reduction tool for Haskell programs I wrote as part of my master thesis (https://github.com/dnlkrgr/hsreduce).

Each folder has the same structure:
* a haskell file we want to reduce
* a shell script that returns exit code 0 if the file contains interesting behavior (reproduces a bug) and returns 1 otherwise


## Adding single file test cases:

1. fork and clone the repo
1. create a new folder
1. in that folder, add a haskell file, call it "Bug.hs" (makes it easier for me to automate)
1. add a shell script, call it "interesting.sh"
1. check that running the shell script really returns exit code 0
1. add a shell.nix (preferred) or cabal file for dependencies (specific libraries or specific GHC version); else you can write it in the PR message 
1. create a pull request


## How do I write interestingness tests?
Let's say you have a large Haskell file that prints "hello world" when you run it, but you're only interested in which part of the program makes it do that.
I think a good interestingness would look like this:

```bash
#!/usr/bin/env bash
ghc Bug.s && ./Bug |& grep 'hello world'
```


## Adding cabal project test cases:
I haven't come up with a good way yet on how to uniformly organize project test cases, so here is a rough first version:

1. fork and clone the repo
1. create a new folder
1. in that folder, add your project as a git submodule and checkout the specific commit that triggers the interesting behavior
1. add an init script or other instructions on how to trigger the behavior; ideally the init script prepares the project to easily reproduce the behavior, stuff for the init script could be: changing the cabal file, adding additional Haskell files, copying specific nix files into the project directory
1. (optional) add additional files that are needed
1. create a pull request


## Questions
Don't hesitate to message me on twitter (twitter.com/dnlkrgr) or on reddit (reddit.com/u/danielkruger)
