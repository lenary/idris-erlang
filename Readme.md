idris-erlang
============

Idris to Erlang Compiler and Libraries

- Install Erlang
- Setup a cabal sandbox, but its `bin` dir on your path.
- Install [Idris HEAD](https://github.com/idris-lang/Idris-dev) into sandbox. I'm bad about keeping this constant, but right now you want at least commit idris-lang/Idris-dev@d2f892b502248429822df5bd51d8db9776e29e7c , but probably with lenary/Idris-dev@b257e4bfab04f0c38efd493cd40ee78677a40081 (which solves some gotchas with erasure and constructors).
- Checkout this repository and cd into its toplevel directory
- Install idris-erlang using `cabal install`
- Install the erlang package using `(cd libs/erlang; idris --install erlang.ipkg)`
- Compile the runtime support using `(cd irts; erlc *.erl)`

You're up and running. To invoke the compiler, use

```
$ idris --codegen=erlang --package=erlang Main.idr -o main.erl
```

Then run the program using

```
$ escript main.erl
```

If everything has worked, then you should be able to compile and run all the examples. If not, you may need to fiddle about with your cabal sandbox quite a lot more, which is effort.
