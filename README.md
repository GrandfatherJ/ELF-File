ELF::File - Read, edit and write ELF files

ELF::File reads ELF files and provides access to the file contents. It is a pure
Perl module, but is designed to deal with 32 and 64 bit files that may be big or
little endian. ELF::File is OS agnostic and 32/64 bit Perl build agnostic so
long as the ivsize is sufficiently large to handle the ELF file contents.

ELF::File may also be used to create ELF files and manipulate the contents of
existing files.


