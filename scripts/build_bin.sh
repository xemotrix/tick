llc -filetype=obj output.ll -o out.o &&
clang out.o -o output &&
rm out.o;
