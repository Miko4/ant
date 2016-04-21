![Travis-CI Status](https://api.travis-ci.org/Miko4/ant.svg)

#                ANT
### “ant is not TeX.”

   https://github.com/Miko4/ant

by  Achim Blumensath

ANT is a typesetting system written by Achim Blumensath. It resembles TeX, but improves on the language. There are no catcodes and programming happens in a "real", Haskell-alike programming language that has domain-specific features for typesetting. Other features include:
 * Unicode support
 * support for various font formats including Type1, TrueType, and OpenType
 * partial support for advanced OpenType features;
 * support for colour and graphics;
 * simple page layout specifications;
 * river detection. 

For more information please take a look at the [Manual](https://github.com/Miko4/ant/blob/master/manual.pdf)


# Building

Unfortunatly Mr. Blumensath has not updated this project since the rease of version 0.8 in late 2007, so building it is requires right now a somewhat old environmnet. But thanks to @gasche this shouldn't be a problem anymore.

First of all, install OPAM, the ocaml package-manager. It's included in most distributions or can follow the instructions on the offical [Homepage](https://opam.ocaml.org/).

The next thing is to install OCaml 3.10.2 and libraries needed:

```sh
opam switch install 3.10.2
opam install ocamlfind
opam install omake
opam install camlimages
```

After that run ./build.sh.

# Errata

Here are some informations from the old Readme, mostly redudant. 

In order to compile ant you need:

 * OCaml version 3.10,
 * OMake version 0.9.8.1,
 * kpathsea version 3.2,
 * FreeType version 2,
 * CamlImages version 2.2,
 * mlgmp version 0.13 (optional).
  

(1) You might want to edit the file “OMakefile” to set some paths. In
particular, the variables KPATHSEA_LDFLAGS and KPATHSEA_CFLAGS probably
need adjustment.

(2) You can choose between three number libraries:

 * Float: (default) All computations use floating point numbers. This is
          the fastest option but it might result in rounding errors.
 * Gmp:   uses the mlgmp library. This gives exact results but it
          slower.
 * Num:   Is similar to Gmp but does not depend on an external library.
          This is the slowest option.

You can specify the number library by setting the NUM_LIB variable in
the file “OMakefile”. Alternatively you can pass the corresponding
option directly to omake, like:

  $ omake NUM_LIB=Gmp

(3) You can specify which version of the compiler to use by setting the
following variables:

 * NATIVE_ENABLED: If set to “true” (default) ant is compiled to native code.
 * BYTE_ENABLED: If set to “true” ant is compiled to byte code.
 * NATIVE_CAMLP4: If set to “true” (default) a native version of the preprocessor
   camlp4 is created. This speeds up the compilation process.

(4) Finally, to compile ant type

  $ omake

You might get errors about a stack overflow. In this case you can increase
your stack size either by

  $ ulimit -s 16384              (if you use the native compilers)

or by

  $ make OCAMLRUNPARAM="l=16M"   (if you use the bytecode version).

(5) Further documentation can be found in the file “manual.pdf”. The
directory “Examples” contains some example ant sources including the
source of the manual. Note that some of these examples use fonts not normally
installed in a TeX system. To compile these documents you have to replace the
corresponding \include{...} command by \include{fonts.ant} (which loads
Computer Modern).

# License

The sourcecode is in the public domain. If this is not possbile in your jurisdiction is licensed under the terms of the WTFPL
