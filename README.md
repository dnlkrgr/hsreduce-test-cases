# hsreduce-test-cases

This is a repo containing test cases for evaluating a reduction tool for Haskell programs (https://github.com/dnlkrgr/hsreduce).

Each folder has the same structure:
* a haskell file we want to reduce
* a shell script that returns exit code 0 if the file contains interesting behavior (reproduces a bug) and returns 1 otherwise


To add test-cases:

1. fork the repo
1. create a new folder
1. in that folder, add a haskell file, call it "Bug.hs" 
1. add a shell script, call it "interesting.sh"
1. add a shell.nix file for dependencies (example: specific GHC version) or include it in the PR message 
1. check that running the shell script really returns exit code 0
1. create a pull request
