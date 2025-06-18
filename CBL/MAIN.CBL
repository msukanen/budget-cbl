       IDENTIFICATION DIVISION.
       PROGRAM-ID.   BUDGET-MAIN.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 17, 2025.
      ******************************************************************
      *
      * A miniature budget calculations thing.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BUDGET-FILE          ASSIGN TO "DATA/BUDGET.CSV"
                                       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  BUDGET-FILE.
       01  BUDGET-RECORD.
           05  CSV-LINE                PIC X(50).
       WORKING-STORAGE SECTION.
       01  WS-CURRENCY                 PIC X(3) VALUE '€'.
       01  WS-CSV-DATA.
           05  CHANGE                  PIC X(10).
           05  COMMENT                 PIC X(50).
       01  WS-BUDGET.
           05  INITIAL-BUDGET          PIC S9(5)V9(2) USAGE COMP-3.
           05  FINAL-BUDGET            PIC S9(5)V9(2) USAGE COMP-3.
           05  BUDGET-ENTRY-COUNT      PIC 9(5) USAGE COMP-3 VALUE 0.
           05  BUDGET-CHANGE           OCCURS 0 TO 1000 TIMES
                                       DEPENDING ON BUDGET-ENTRY-COUNT
                                       INDEXED BY BUDGET-IDX.
               10  CHANGE              PIC S9(5)V9(2) USAGE COMP-3.
               10  COMMENT             PIC X(50) VALUE '-'.
       01  WS-MONEY-STR                PIC +(4)9.99.
       01  LOOP                        USAGE COMP-1.
       01  WS-TMP-STR10                PIC X(10).
       01  WS-OUTPUT-LINE              PIC X(79).
       01  WS-CONSOLE-CODES.
           02  EOL                     PIC X VALUE X"0a".
           02  CONS-RST              PIC X(4).
           02  BOLDFACE                PIC X(4).
           02  BOLD-YELLOW                PIC X(7).
           02  C-GREEN                  PIC X(7).
           02  CONS-G-B                PIC X(7).
           02  BOLD-RED                PIC X(7).

       PROCEDURE DIVISION.
      *    Some string initializations:
           STRING X"1b"    "[0m" DELIMITED BY SIZE INTO CONS-RST
           STRING X"1b"    "[1m" DELIMITED BY SIZE INTO BOLDFACE
           STRING X"1b" "[1;33m" DELIMITED BY SIZE INTO BOLD-YELLOW
           STRING X"1b" "[0;32m" DELIMITED BY SIZE INTO C-GREEN
           STRING X"1b" "[1;32m" DELIMITED BY SIZE INTO CONS-G-B
           STRING X"1b" "[1;31m" DELIMITED BY SIZE INTO BOLD-RED
      *    Some value/index initializations:
           SET BUDGET-IDX TO 1.
           MOVE 0 TO BUDGET-ENTRY-COUNT.

      *    Read the BUDGET.CSV:
           OPEN INPUT BUDGET-FILE.
           PERFORM UNTIL EXIT
               READ BUDGET-FILE INTO CSV-LINE
                   AT END EXIT PERFORM
                   NOT AT END PERFORM PROCESS-CSV-LINE
               END-READ
           END-PERFORM.
           CLOSE BUDGET-FILE.

           MOVE INITIAL-BUDGET TO FINAL-BUDGET
           MOVE INITIAL-BUDGET TO WS-MONEY-STR
           DISPLAY CONS-RST 'With your initial budget of '
                   BOLD-YELLOW FUNCTION TRIM(WS-MONEY-STR)
                   CONS-RST WS-CURRENCY' and'
                   NO ADVANCING
           IF BUDGET-ENTRY-COUNT < 1 THEN
               DISPLAY ' with no changes detected! Awesome?' CONS-RST
               GOBACK
           END-IF
           DISPLAY '…' EOL
                   CONS-RST' .----'
                   BOLDFACE 'AMNT'
                   CONS-RST'  ∵  '
                   BOLDFACE 'FROM/TO'
                   CONS-RST' …'.
           SET BUDGET-IDX TO 1.
           PERFORM VARYING LOOP
                   FROM 1 BY 1
                   UNTIL LOOP > BUDGET-ENTRY-COUNT
               IF CHANGE OF BUDGET-CHANGE(BUDGET-IDX) = 0 THEN
                   SET BUDGET-IDX UP BY 1
                   CONTINUE
               END-IF
               
               MOVE CHANGE OF BUDGET-CHANGE(BUDGET-IDX) TO WS-MONEY-STR
               EVALUATE TRUE
                   WHEN CHANGE OF BUDGET-CHANGE(BUDGET-IDX) > 0
                       DISPLAY C-GREEN NO ADVANCING
                   WHEN CHANGE OF BUDGET-CHANGE(BUDGET-IDX) < 0
                       DISPLAY BOLD-RED NO ADVANCING
               END-EVALUATE
               INITIALIZE WS-OUTPUT-LINE
               DISPLAY '  'WS-MONEY-STR
                       CONS-RST FUNCTION TRIM(WS-CURRENCY)
                       NO ADVANCING
               IF COMMENT OF BUDGET-CHANGE(BUDGET-IDX) NOT = SPACES THEN
                   IF CHANGE OF BUDGET-CHANGE(BUDGET-IDX) > 0 THEN
                        DISPLAY '   from ' NO ADVANCING
                   ELSE DISPLAY '   to ' NO ADVANCING
                   END-IF
                   DISPLAY COMMENT OF BUDGET-CHANGE(BUDGET-IDX)
               ELSE DISPLAY EOL
               END-IF
               COMPUTE FINAL-BUDGET
                     = FINAL-BUDGET
                     + CHANGE OF BUDGET-CHANGE(BUDGET-IDX)
               SET BUDGET-IDX UP BY 1
           END-PERFORM.

           MOVE FINAL-BUDGET TO WS-MONEY-STR.
           DISPLAY ' `---------.' EOL
                   BOLD-YELLOW'  'WS-MONEY-STR
                   CONS-RST FUNCTION TRIM(WS-CURRENCY)
                   NO ADVANCING.
           IF FINAL-BUDGET < 0 THEN
                DISPLAY ' YOU WILL BE/ARE BANKRUPT!'
           ELSE DISPLAY EOL.
           GOBACK.

       PROCESS-CSV-LINE.
           INITIALIZE CHANGE OF WS-CSV-DATA
                      COMMENT OF WS-CSV-DATA.
           UNSTRING CSV-LINE
               DELIMITED BY ';' INTO
                   CHANGE OF WS-CSV-DATA
                   COMMENT OF WS-CSV-DATA
               ON OVERFLOW
                   MOVE 112 TO RETURN-CODE
                   STOP RUN
               NOT ON OVERFLOW
                   EVALUATE TRUE
      *                The line beginning with '#' is used to mark the
      *                initial budget.
                       WHEN CSV-LINE(1:1) = '#'
                           MOVE FUNCTION TRIM (CSV-LINE(2:))
                                TO WS-MONEY-STR
                           MOVE WS-MONEY-STR TO INITIAL-BUDGET
                           EXIT PARAGRAPH
      *                A line beginning with '*' is a comment and thus
      *                to be ignored.
                       WHEN CSV-LINE(1:1) = '*' EXIT PARAGRAPH
                   END-EVALUATE

                   IF CHANGE OF WS-CSV-DATA NOT = SPACES THEN
                       ADD 1 TO BUDGET-ENTRY-COUNT
                       MOVE CORR WS-CSV-DATA
                            TO BUDGET-CHANGE(BUDGET-IDX)
                       SET BUDGET-IDX UP BY 1
                   END-IF
           END-UNSTRING.
           EXIT PARAGRAPH.

       DISPLAY-OUTPUT-NO-ADVANCING.
           DISPLAY WS-OUTPUT-LINE NO ADVANCING.
           EXIT PARAGRAPH.

       DISPLAY-OUTPUT.
           PERFORM DISPLAY-OUTPUT-NO-ADVANCING.
           DISPLAY EOL NO ADVANCING.
           EXIT PARAGRAPH.
