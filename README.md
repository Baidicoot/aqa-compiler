This is a compiler for [AQA pseudocode](https://filestore.aqa.org.uk/resources/computing/AQA-8520-TG-PC.PDF) that targets both [AQA assembly](https://filestore.aqa.org.uk/resources/computing/AQA-75162-75172-ALI.PDF) and an ARM subset called [ARMLite](https://peterhigginson.co.uk/ARMlite/Programming%20reference%20manual_v1_2.pdf). The compiler can currently compile the most of the AQA pseudocode specification (pretty much everything minus IO and array/string literals). However, AQA assembly has no features for indirect jumps, rendering subroutines impractical, and so features such as `SUBROUTINE`s and `RETURN` statements are reserved for ARMLite only.

The compiler is written, and tested, in Python 3.10, in a functional-esque style (an OCaml port is inevitable). Python 3.10 is still in alpha at the moment, but can be built from source. Moreover, `parsita`, the parser library the frontend uses, is not available via `python3.10 -m pip`; it can be downloaded from its [repository](https://github.com/drhagen/parsita). To run the compiler, execute `python3.10 compile.py [FILEPATH] [TARGET]`, where `[FILEPATH]` is the pseudocode program to compile, and `[TARGET]` is the target backend (currently `aqa` or `arm`).

Programs are structured like in C, being a sequence of functions in which one acts as the 'entry point' of the program. By default the entry point is the function named `main`, but it can be specified with `python3.10 compile.py [FILEPATH] [TARGET] [ENTRY]`. IO is possible in the [ARMLite simulator](https://www.peterhigginson.co.uk/ARMlite/) via direct memory access, as demonstrated in the `IO.pseudo` example:

```
SUBROUTINE putc(char)
    4294967068[0] <- char
ENDSUBROUTINE
```