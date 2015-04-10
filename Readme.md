idris-erlang
============

Idris to Erlang Compiler and Libraries

- Install Erlang
- Setup a cabal sandbox, but its `bin` dir on your path.
- Install [Idris HEAD](https://github.com/idris-lang/Idris-dev) into sandbox
- Install idris-erlang using `cabal install`
- Install the erlang package using `(cd libs/erlang; idris --install erlang.ipkg)`
- Compile the runtime support using `(cd irts; erlc *.erl)`

You're up and running. To invoke the compiler, use

```
$ idris --codegen=erlang --package erlang Main.idr -o main.erl
```

Then run the program using

```
$ escript main.erl
```
