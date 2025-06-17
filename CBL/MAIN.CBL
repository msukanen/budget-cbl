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
       COPY CONSOLE.
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

       PROCEDURE DIVISION.
           SET BUDGET-IDX TO 1.
           MOVE 0 TO BUDGET-ENTRY-COUNT.
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
           DISPLAY ANSI-ESC ANSI-RESET
                   'With your initial budget of '
                   ANSI-ESC '[1;33m'
                   FUNCTION TRIM(WS-MONEY-STR)
                   ANSI-ESC ANSI-RESET WS-CURRENCY' and'
                   NO ADVANCING
           IF BUDGET-ENTRY-COUNT < 1 THEN
               DISPLAY ' with no changes detected! Awesome?' ANSI-RESET
               GOBACK
           END-IF
           DISPLAY '…' ANSI-ESC ANSI-RESET
           DISPLAY ' .----'X'1B''[1mAMNT'X'1B''[0m ∵ '
                   X'1B''[1mFROM/TO'X'1B''[0m …'

           SET BUDGET-IDX TO 1
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
                       DISPLAY ANSI-ESC ANSI-GREEN NO ADVANCING
                   WHEN CHANGE OF BUDGET-CHANGE(BUDGET-IDX) < 0
                       DISPLAY ANSI-ESC ANSI-RED-BOLD NO ADVANCING
               END-EVALUATE
               DISPLAY '  '
                   WS-MONEY-STR
                   ANSI-ESC ANSI-RESET
                   FUNCTION TRIM(WS-CURRENCY)
                   NO ADVANCING
               IF COMMENT OF BUDGET-CHANGE(BUDGET-IDX) NOT = SPACES THEN
                   IF CHANGE OF BUDGET-CHANGE(BUDGET-IDX) > 0 THEN
                        DISPLAY ' from ' NO ADVANCING
                   ELSE DISPLAY ' to ' NO ADVANCING
                   END-IF
                    DISPLAY COMMENT OF BUDGET-CHANGE(BUDGET-IDX)
               ELSE DISPLAY SPACE
               END-IF
               COMPUTE FINAL-BUDGET
                     = FINAL-BUDGET
                     + CHANGE OF BUDGET-CHANGE(BUDGET-IDX)
               SET BUDGET-IDX UP BY 1
           END-PERFORM.

           MOVE FINAL-BUDGET TO WS-MONEY-STR
           DISPLAY ' `---------.'
           DISPLAY ANSI-ESC ANSI-YELLOW-BOLD
                   '  'WS-MONEY-STR
                   ANSI-ESC ANSI-RESET
                   FUNCTION TRIM(WS-CURRENCY)' left.'
                   NO ADVANCING
           IF FINAL-BUDGET < 0 THEN
                DISPLAY ' YOU WILL BE/ARE BANKRUPT!'
           ELSE DISPLAY SPACE.
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
