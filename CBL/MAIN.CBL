       IDENTIFICATION DIVISION.
       PROGRAM-ID.   BUDGET-MAIN.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 17—June 18, 2025.
      ******************************************************************
      *
      * A miniature budget calculations thing.
      *
      * Note that we have to resort to using 'e' instead of '€' until
      * NATIONAL PIC gets properly implemented (for GnuCOBOL at least).
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     CURSOR IS CURSOR-POSITION
                     CRT STATUS IS CRT-STATUS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BUDGET-FILE          ASSIGN TO "DATA/BUDGET.DTA"
                                       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  BUDGET-FILE
           RECORD IS VARYING IN SIZE
           FROM 9 TO 40 CHARACTERS
           DEPENDING ON RECORD-LENGTH.
       01  BUDGET-RECORD.
           05  CHANGE                  PIC -(5)9.99.                    9
           05  RECORD-TYPE             PIC X.                           1
               COPY DATAPOTN.
           05  COMMENT                 PIC X(30).                       30
       WORKING-STORAGE SECTION.
       01  BUDGET-FILE-STATUS          PIC X VALUE '-'.
           88  EOF                     VALUE 'Y'.
       01  CURSOR-POSITION.
           02  CURSOR-LINE             PIC 99.
           02  CURSOR-COL              PIC 99.
       01  CRT-STATUS.
           05  KEY1                    PIC X.
           05  KEY2                    PIC X.
               88  FKEY-10             VALUE 11.
           05  FILLER                  PIC XX.
      *01  WS-CURRENCY                 PIC N(4) VALUE N'€'.
       01  WS-CURRENCY                 PIC X VALUE 'e'.                 ↑ nation
       01  INITIAL-BUDGET              PIC S9(5)V99 USAGE COMP-3
                                       VALUE 0.
       01  FINAL-BUDGET                PIC S9(5)V99 USAGE COMP-3
                                       VALUE 0.
       01  POTENTIAL-BUDGET            PIC S9(5)V99 USAGE COMP-3
                                       VALUE 0.
       01  ENTRY-COUNT                 PIC 9(5) USAGE COMP-3 VALUE 0.
       01  WS-BUDGET.
           05  RECORD-LENGTH           PIC 9(4) COMP.
           05  BUDGET-CHANGE           OCCURS 0 TO 1000 TIMES
                                       DEPENDING ON ENTRY-COUNT
                                       INDEXED BY BUDGET-IDX.
               10  CHANGE              PIC S9(5)V99 USAGE COMP-3.
               10  COMMENT             PIC X(30).
               10  RECORD-TYPE         PIC X VALUE SPACE.
                   COPY DATAPOTN.
       01  LOOP                        USAGE COMP-1.
       01  WS-TMP-STR10                PIC X(10).
       01  ACCEPT-ITEM1                PIC X.
      * Screen stuff:
       01  BUDGET-LINE                 PIC 99.
       77  AMNT-COL                    PIC 99 VALUE 38.
       77  AMNT-CHANGE-COL             PIC 99 VALUE 40.
       77  AMNT-CURRENCY-COL           PIC 99 VALUE 49.
       77  AMNT-COMMENT-COL            PIC 99 VALUE 52.
       01  BUDGET-CHANGE-COLOR         PIC 9.
           88  BUDGET-DEBIT            VALUE 4.                         4-red
           88  BUDGET-CREDIT           VALUE 2.                         2-green
       SCREEN SECTION.
       01  MAIN-SCREEN                 BLANK SCREEN
                                       FOREGROUND-COLOR 7
                                       BACKGROUND-COLOR 0.
           02  HDR                     FOREGROUND-COLOR 7
                                       BACKGROUND-COLOR 1.
               03  LINE 1 ERASE EOL.
               03  LINE 1 COLUMN 11    VALUE 'BudGet-CBL'.
               03  LINE 1 COLUMN 21 ERASE EOL.
           02  MONEYS                  FOREGROUND-COLOR 7
                                       BACKGROUND-COLOR 0.
      *            Initial budget:
               03  LINE 3 COLUMN 6     VALUE 'Initial: '.
               03  PIC -(5)9.99 FROM INITIAL-BUDGET.
               03  PIC X FROM WS-CURRENCY.
      *            Estimated budget:
               03  LINE 4 COLUMN 4     VALUE 'Estimated: '.
               03  PIC -(5)9.99 FROM FINAL-BUDGET.
               03  PIC X FROM WS-CURRENCY.
      *            Worst-case budget:
               03  LINE 5 COLUMN 3     VALUE 'Worst-case: '.
               03  PIC -(5)9.99 FROM POTENTIAL-BUDGET.
               03  PIC X FROM WS-CURRENCY.
       01  BUDGET-CHANGE-TEXT.
           03  LINE BUDGET-LINE COLUMN AMNT-CHANGE-COL
               PIC -(5)9.99
               FROM CHANGE OF BUDGET-CHANGE(BUDGET-IDX)
               FOREGROUND-COLOR BUDGET-CHANGE-COLOR.
           03  PIC X FROM WS-CURRENCY.
           03  PIC XXX VALUE ' - '.
           03  PIC X(26)
               FROM COMMENT OF BUDGET-CHANGE(BUDGET-IDX).

       PROCEDURE DIVISION.
           SET BUDGET-IDX TO 1.
           OPEN INPUT BUDGET-FILE.
           PERFORM VARYING BUDGET-IDX FROM 1 BY 1 UNTIL EOF
               READ BUDGET-FILE INTO BUDGET-RECORD
                   AT END SET EOF TO TRUE
                   NOT AT END PERFORM PROCESS-RECORD
               END-READ
           END-PERFORM.
           CLOSE BUDGET-FILE.

           DISPLAY MAIN-SCREEN.
           MOVE 3 TO BUDGET-LINE.
           PERFORM VARYING BUDGET-IDX FROM 1 BY 1
                   UNTIL BUDGET-IDX > ENTRY-COUNT
               IF NOT REC-INITIAL OF BUDGET-CHANGE(BUDGET-IDX) THEN
                   IF CHANGE OF BUDGET-CHANGE(BUDGET-IDX) < 0 THEN
                        SET BUDGET-DEBIT TO TRUE
                   ELSE SET BUDGET-CREDIT TO TRUE
                   END-IF
                   DISPLAY BUDGET-CHANGE-TEXT
                   ADD 1 TO BUDGET-LINE
               END-IF
           END-PERFORM.
           GOBACK.

       PROCESS-RECORD.
           MOVE CORR BUDGET-RECORD TO BUDGET-CHANGE(BUDGET-IDX).
           EVALUATE TRUE
               WHEN REC-INITIAL OF BUDGET-RECORD
                   MOVE CHANGE OF BUDGET-RECORD TO INITIAL-BUDGET
                   MOVE INITIAL-BUDGET TO FINAL-BUDGET
                   MOVE INITIAL-BUDGET TO POTENTIAL-BUDGET
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
           COMPUTE POTENTIAL-BUDGET
                 = POTENTIAL-BUDGET
                 + CHANGE OF BUDGET-CHANGE(BUDGET-IDX).
           EXIT PARAGRAPH.
