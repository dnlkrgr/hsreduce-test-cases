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

## FAQ
**Q: What sort of criteria are you looking for?**

A: For the haskell file, I don't have any special criteria as the goal is to be able to handle a wide variety of Haskell programs.
Can be anything from a file that just prints "hello world" with a lot of unnecessary code (though not very interesting), to files that trigger GHC bugs.
The main goal is to make it easier for people to submit minimal reproducing bug examples when opening issues on the GHC repository.
The best test cases will probably be mostly those that exhibit some kind of bug (for example GHC crashing during the simplifier or some performance regression) which is reproducible via a shell script which returns exit code zero if the bug is still appearing in the file and returns exit code one if not.
With the shell script we can be very general, the tool doesn't know anything about the shell script, it just receives the exit code.
So in summary: a helpful test case would be an added folder containing a haskell file which has some interesting behavior, a shell script that returns zero (interesting) or one (uninteresting) and optionally a nix or cabal file for needed dependencies.
You can also find a little bit information about that in the repository's readme.
My tool also supports some merging of cabal projects (to then later reduce the merge result) but I haven't come up with a uniform way on how to structure those test cases.
If you have an interesting cabal project, you can also add it with maybe like a text file with instructions on how to reproduce the behavior.
I hope this explained it a bit, if you still have questions, fire away!


**Q: I find this to be strange behaviour: "a shell script that returns exit code 0 if the file contains interesting behavior (reproduces a bug) and returns 1 otherwise ". Surely it makes more sense that something that checks for buggy behaviour returns a non-zero exitcode if it finds a bug?**

A: You're correct.
But here reproducing the bug is the "normal behavior" that we seek and anything else not.
If a reduction is done that leads to a test case that crashes for another reason or leads to a timeout, then it's not "normal behavior".


**Q: what do you mean by reduction?**

A: By reduction I mean "making the Haskell file smaller to make it more easily readable by humans".
This is done by trying to delete various elements from the Haskell program or by simplifying elements.


## Other Questions
Don't hesitate to message me on twitter (twitter.com/dnlkrgr) or on reddit (reddit.com/u/danielkruger)
