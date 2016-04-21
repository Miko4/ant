eval `opam config env`

mkdir lib
omake Runtime/kpathsea-stubs.o Runtime/freetype-stubs.o Unicode/Tables.o
omake

