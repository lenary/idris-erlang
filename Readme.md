idris-erlang
============

Idris to Erlang Compiler and Libraries.

I started using Stackage, so these instructions now use stackage.

- Install Stack
- Install Erlang
- Checkout this repository and cd into its toplevel directory
- Compile the runtime support using `(cd irts; erlc *.erl)`
- Build this package with `stack build`
- Install the erlang package using `(cd libs/erlang; stack exec idris -- --install erlang.ipkg)`

You're up and running. To invoke the compiler, use

```
$ stack exec idris -- --codegen=erlang --package=erlang Main.idr -o main.erl
```

Then run the program using

```
$ escript main.erl
```

If everything has worked, then you should be able to compile and run
all the examples. If not, `¯\_(ツ)_/¯`
