      ******************************************************************
      * Some ANSI stuff for terminal.
      *
       01  ANSI-CODES.
           02  ANSI-ESC                PIC X VALUE X"1B".               ESC
           02  ANSI-RED-BOLD           PIC X(6) VALUE '[1;31m'.
           02  ANSI-YELLOW-BOLD        PIC X(6) VALUE '[1;33m'.
           02  ANSI-GREEN              PIC X(4) VALUE '[32m'.
           02  ANSI-RESET              PIC X(3) VALUE '[0m'.
       