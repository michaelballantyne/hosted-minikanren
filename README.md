# README 


## Test 

`raco test ++arg <some-module-file> ++ <some-output-file>  -c minikanren-ee` 

E.g., from within base directory, 

`raco setup minikanren-ee && raco test ++arg mk/private-unstable.rkt ++arg out.txt -c minikanren-ee`

## Setup 

`raco setup -j 3` for some number of cores you want to use in install 

## faster-minikanren subtree

The directory `mk/` is a `git subtree` of the `faster-minikanren` repo. To use it, first add `faster-minikanren` as a remote:
```
git remote add faster-minikanren git@github.com:michaelballantyne/faster-minikanren.git
```

Then, to pull in new commits from upstream, pull with:
```
git subtree pull --prefix mk faster-minikanren master --squash
```
The syntax properties provided to be used for code-gen are listed in `/private/compile/prop-vars.rkt`

