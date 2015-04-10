idris-erlang
============

Idris to Erlang Compiler and Libraries

- Setup a cabal sandbox
- Install [Idris HEAD](https://github.com/idris-lang/Idris-dev) into sandbox
- Install idris-erlang using `cabal install`
- Install the erlang package using `(cd libs/erlang; idris --install erlang.ipkg)`

You're up and running. To invoke the compiler, use

```
$ idris --codegen=erlang --package erlang Main.idr -o main.erl
```

Then run the program using

```
$ escript main.erl
```
