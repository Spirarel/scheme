Scheme
============


### Running

You'll need ghc to run the code. The app depends on Parsec, so using Stack will eliminate some pain. It's best to run a compiled form since layered interpreters will interpret special characters literally (e.g. backspace).

Compile with
`ghc -o scheme app/Main.hs -O2`

and run with
`./scheme`

Or if using stack,
```{.sh}
$ stack init
$ stack build
$ stack exec main
```

Creating a Scheme interpreter was the final MP of my languages course at uni.
