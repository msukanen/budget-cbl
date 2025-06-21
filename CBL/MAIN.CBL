       IDENTIFICATION DIVISION.
       PROGRAM-ID.   BUDGET-MAIN.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 17—June 21, 2025.
      ******************************************************************
      *
      * A miniature BudGet calculations thing.
      *
      * Note that we have to resort to using 'e' instead of '€' until
      * NATIONAL PIC gets properly implemented (for GnuCOBOL at least).
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BUDGET-FILE          ASSIGN TO "DATA/BUDGET.DTA"
                                       ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  BUDGET-FILE
           RECORD IS VARYING IN SIZE
      *  bytes
      *     9  - monetary value
      *     1  - [opt] mode, see DATAPOTN
      *    30  - [opt] comment
           FROM 9 TO 40 CHARACTERS
           DEPENDING ON RECORD-LENGTH.
       01  BUDGET-RECORD.
           05  CHANGE                  PIC -(5)9.99.                    9
           05  RECORD-MODE             PIC X.                           1
               COPY DATAPOTN.
      *                                Is 30 characters enough for a
      *                                proper comment?  Maybe is, maybe
      *                                isn't, but it'll do for now.
           05  COMMENT                 PIC X(30).                       30
       WORKING-STORAGE SECTION.
       01  FILE-STATUS                 PIC X VALUE '-'.
           88  EOF                     VALUE 'Y'.
       01  CURSOR-POSITION.
           02  CURSOR-LINE             PIC 99.
      *    02  CURSOR-COL              PIC 99.
       01  WS-CURRENCY                 PIC X VALUE 'e'.                  ← N'€'
       01  INITIAL-BUDGET              PIC S9(5)V99 USAGE COMP-3
                                       VALUE 0.
       01  FINAL-BUDGET                PIC S9(5)V99 USAGE COMP-3
                                       VALUE 0.
       01  WORST-CASE-BUDGET            PIC S9(5)V99 USAGE COMP-3
                                       VALUE 0.
      * Tens of thousands entries is probably overkill to prepare for,
      * but what the heck, why not!
       01  ENTRY-COUNT                 PIC 9(5) USAGE COMP-3 VALUE 0.
       01  WS-BUDGET.
           05  RECORD-LENGTH           PIC 9(4) COMP.
      *                                1,000 entries? Pretty busy …
           05  BUDGET-CHANGE           OCCURS 0 TO 1000 TIMES
                                       DEPENDING ON ENTRY-COUNT
                                       INDEXED BY BUDGET-IDX.
               10  CHANGE              PIC S9(5)V99 USAGE COMP-3.
               10  COMMENT             PIC X(30).
               10  RECORD-MODE         PIC X VALUE SPACE.
                   COPY DATAPOTN.
      * Screen stuff:
       77  BUDGET-CHANGE-COL           PIC 99 VALUE 30.
       01  BUDGET-STATE                PIC 9.
           88  BUDGET-DEBIT            VALUE 4.                         4-red
           88  BUDGET-CREDIT           VALUE 2.                         2-green
       SCREEN SECTION.
       01  MAIN-SCREEN                 BLANK SCREEN
                                       FOREGROUND-COLOR 7
                                       BACKGROUND-COLOR 0.
         02 HDR                        FOREGROUND-COLOR 7
                                       BACKGROUND-COLOR 1.
           03  LINE 1 ERASE EOL.
           03  LINE 1 COLUMN 11
               VALUE 'BudGet-CBL - Copyright (c) 2025 Markku '.
           03  VALUE 'Sukanen' ERASE EOL.
         02  MONEYS                    FOREGROUND-COLOR 7
                                       BACKGROUND-COLOR 0.
           03  SCR-INITIAL-BUDGET.
      *        Initial budget:
             04  LINE 3 COLUMN 6 VALUE 'Initial: '.
             04  PIC -(5)9.99 FROM INITIAL-BUDGET.
             04  PIC X FROM WS-CURRENCY.
           03  SCR-ESTIMATED-BUDGET.
      *        Estimated budget:
             04  LINE 4 COLUMN 4 VALUE 'Estimated: '.
             04  PIC -(5)9.99 FROM FINAL-BUDGET.
             04  PIC X FROM WS-CURRENCY.
           03  SCR-WORST-CASE-BUDGET.
      *        Worst-case budget:
             04  LINE 5 COLUMN 3 VALUE 'Worst-case: '.
             04  PIC -(5)9.99 FROM WORST-CASE-BUDGET.
             04  PIC X FROM WS-CURRENCY.
       01  ONE-BUDGET-ENTRY.
         02  LINE CURSOR-LINE COLUMN BUDGET-CHANGE-COL
             PIC -(5)9.99 FROM CHANGE OF BUDGET-CHANGE(BUDGET-IDX)
             FOREGROUND-COLOR BUDGET-STATE.
         02  PIC X FROM WS-CURRENCY.
         02  PIC XXX VALUE ' - '.
         02  PIC X(26) FROM COMMENT OF BUDGET-CHANGE(BUDGET-IDX).

       PROCEDURE DIVISION.
           OPEN INPUT BUDGET-FILE.
           PERFORM VARYING BUDGET-IDX FROM 1 BY 1 UNTIL EOF
               READ BUDGET-FILE INTO BUDGET-RECORD
                   AT END SET EOF TO TRUE
                   NOT AT END PERFORM PROCESS-RECORD
               END-READ
           END-PERFORM.
           CLOSE BUDGET-FILE.

           DISPLAY MAIN-SCREEN.
      *    DISPLAY all the (relevant) budget change lines:
           MOVE 3 TO CURSOR-LINE.
           PERFORM VARYING BUDGET-IDX FROM 1 BY 1
                   UNTIL BUDGET-IDX > ENTRY-COUNT
               IF NOT REC-INITIAL OF BUDGET-CHANGE(BUDGET-IDX) THEN
                   IF CHANGE OF BUDGET-CHANGE(BUDGET-IDX) < 0 THEN
                        SET BUDGET-DEBIT TO TRUE
                   ELSE SET BUDGET-CREDIT TO TRUE
                   END-IF
                   DISPLAY ONE-BUDGET-ENTRY
                   ADD 1 TO CURSOR-LINE
               END-IF
           END-PERFORM.
           GOBACK.

       PROCESS-RECORD.
           MOVE CORR BUDGET-RECORD TO BUDGET-CHANGE(BUDGET-IDX).
           EVALUATE TRUE
               WHEN REC-INITIAL OF BUDGET-RECORD
                   MOVE CHANGE OF BUDGET-RECORD TO INITIAL-BUDGET
                   MOVE INITIAL-BUDGET TO FINAL-BUDGET
                   MOVE INITIAL-BUDGET TO WORST-CASE-BUDGET
               WHEN REC-POTENTIAL OF BUDGET-RECORD
                   PERFORM COMPUTE-POTENTIAL-BUDGET
               WHEN OTHER
                   PERFORM COMPUTE-POTENTIAL-BUDGET
                   COMPUTE FINAL-BUDGET
                         = FINAL-BUDGET
                         + CHANGE OF BUDGET-CHANGE(BUDGET-IDX)
           END-EVALUATE.
           ADD 1 TO ENTRY-COUNT.
           EXIT PARAGRAPH.

       COMPUTE-POTENTIAL-BUDGET.
           COMPUTE WORST-CASE-BUDGET
                 = WORST-CASE-BUDGET
                 + CHANGE OF BUDGET-CHANGE(BUDGET-IDX).
           EXIT PARAGRAPH.
