
### Examples - Running the new Interpreter

The file `RunXY.hs` contains a top-level implementation which is compatible
with the monadic interpreter.

```
$ runhaskell RunXY.hs factorial.xy 5
120
```

```
$ runhaskell RunXY.hs fibonacci.xy 6
8
```

To Run the optimized interpreter, use 

```
runhaskell --ghc-arg="-package mtl" RunOptimize.hs <filename>

```

then

```
$ ./optimize.sh op-ex0.xyio
```


