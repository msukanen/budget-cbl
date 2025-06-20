       IDENTIFICATION DIVISION.
       PROGRAM-ID.   BUDGET-MAIN.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 17—June 18, 2025.
      ******************************************************************
      *
      * A miniature budget calculations thing.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     CURSOR IS CURSOR-POSITION
                     CRT STATUS IS CRT-STATUS.
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
       01  CURSOR-POSITION.
           02  CURSOR-LINE             PIC 99.
           02  CURSOR-COL              PIC 99.
       01  CRT-STATUS.
           05  KEY1                    PIC X.
           05  KEY2                    PIC X.
               88  FKEY-10             VALUE 11.
           05  FILLER                  PIC XX.
       01  WS-CURRENCY                 PIC X VALUE 'e'.                 national
       01  WS-CSV-DATA.
           05  CHANGE                  PIC X(10).
           05  COMMENT                 PIC X(50).
           05  POTENTIAL               PIC X VALUE '-'.
               COPY DATAPOTN.
           05  TESTING                 PIC X VALUE '-'.
               COPY DATATEST.
       01  WS-BUDGET.
           05  INITIAL-BUDGET          PIC S9(5)V99 USAGE COMP-3.
           05  FINAL-BUDGET            PIC S9(5)V99 USAGE COMP-3.
           05  POTENTIAL-BUDGET        PIC S9(5)V99 USAGE COMP-3.
           05  BUDGET-ENTRY-COUNT      PIC 9(5) USAGE COMP-3 VALUE 0.
           05  BUDGET-CHANGE           OCCURS 0 TO 1000 TIMES
                                       DEPENDING ON BUDGET-ENTRY-COUNT
                                       INDEXED BY BUDGET-IDX.
               10  CHANGE              PIC S9(5)V99 USAGE COMP-3.
               10  COMMENT             PIC X(50) VALUE '-'.
               10  POTENTIAL           PIC X VALUE '-'.
                   COPY DATAPOTN.
               10  TESTING             PIC X VALUE '-'.
                   COPY DATATEST.
       01  WS-MONEY-STR                PIC +(4)9.99.
       01  LOOP                        USAGE COMP-1.
       01  WS-TMP-STR10                PIC X(10).
       01  ACCEPT-ITEM1                PIC X.
       01  BUDGET-LINE                 PIC 99.
       77  AMNT-COL                    PIC 99 VALUE 38.
       77  AMNT-CHANGE-COL             PIC 99 VALUE 40.
       77  AMNT-CURRENCY-COL           PIC 99 VALUE 49.
       77  AMNT-COMMENT-COL            PIC 99 VALUE 52.
       SCREEN SECTION.
       01  MENU-TEXT.
           02 PIC X(10) VALUE '[MENU]' COLUMN 5.
           02 PIC X(2) VALUE '~'.
      **************************************************************************
      *__________BudGet-CBL____________________________________________________
      *-~[MENU]~-  |                          .----[ AMNT ∵ FROM/TO ]-----------
      * (S)tatus   |   C.Budget: 1,234.56€    |
      * (M)odify   |   E.Budget:   321.09€    |   132.54€   from XyzFooBarBank
      *            |                             -543.21€   to Mafia
      * (Q)uit /F10|                           -9,876.54€   to CIA
      *                                             1.21€   from Mom
      *                                         .........
      *                                       -10,286.00€
      *
       01  MAIN-SCREEN                 BLANK SCREEN
                                       FOREGROUND-COLOR 7
                                       BACKGROUND-COLOR 0.
           02  HDR                     FOREGROUND-COLOR 7
                                       BACKGROUND-COLOR 1.
               03  LINE 1 ERASE EOL.
               03  LINE 1 COLUMN 11    VALUE 'BudGet-CBL'.
               03  LINE 1 COLUMN 21 ERASE EOL.
           02  SIDE-MENU               FOREGROUND-COLOR 0
                                       BACKGROUND-COLOR 7.
               03  LINE 2 PIC X(3) VALUE '-~['.
               03  LINE 2 COLUMN 4 PIC X(4) VALUE 'MENU' UNDERLINE.
               03  LINE 2 COLUMN 8 PIC X(6) VALUE ']~-'.
               03  LINE 3              PIC X TO ACCEPT-ITEM1.
               03                      PIC X(10) VALUE '(S)tatus'.
               03  LINE 4              PIC X TO ACCEPT-ITEM1.
               03                      PIC X(10) VALUE '(M)odify'.
               03  LINE 5              PIC X(11).
               03  LINE 6              PIC X TO ACCEPT-ITEM1.
               03  LINE 6              VALUE '(Q)uit /F10'.
           02  PRIMARY-VIEW            FOREGROUND-COLOR 7
                                       BACKGROUND-COLOR 0.
               03  LINE 3 COLUMN 17 VALUE 'C.Budget:'.
               03  LINE 3 COLUMN 27 PIC -(4)9.99
                                    FROM INITIAL-BUDGET AUTO.
      * GnoCOBOL: handling of USAGE NATIONAL is unfinished;
      *           implementation is likely to be changed [-Wunfinished]
      *        03  LINE 3 COLUMN 35 PIC N(4) VALUE N'€'.
               03  LINE 3 COLUMN 35 VALUE 'e'.                          placehld
               03  LINE 4 COLUMN 17 VALUE 'E.Budget:'.
               03  LINE 5 COLUMN 16 VALUE '(P.Budget:'.
           02  AMNT-FT-VIEW FOREGROUND-COLOR 7
                            BACKGROUND-COLOR 0.
               03  LINE 2 COLUMN AMNT-COL
                   VALUE '.----[ AMNT - FROM/TO ]-----------'.
               03  LINE 3 COLUMN AMNT-COL VALUE '|'.
               03  LINE 4 COLUMN AMNT-COL VALUE '|'.
               03  LINE 5 COLUMN AMNT-COL VALUE '|'.
       01  BUDGET-CHANGE-TEXT-N.
           03  LINE BUDGET-LINE COLUMN AMNT-CHANGE-COL
               PIC -(5)9.99
               FROM CHANGE OF BUDGET-CHANGE(BUDGET-IDX)
               FOREGROUND-COLOR 4.
           03  LINE BUDGET-LINE COLUMN AMNT-CURRENCY-COL
               FROM WS-CURRENCY.
           03  LINE BUDGET-LINE COLUMN AMNT-COMMENT-COL
               PIC X(26)
               FROM COMMENT OF BUDGET-CHANGE(BUDGET-IDX).
       01  BUDGET-CHANGE-TEXT-P.
           03  LINE BUDGET-LINE COLUMN AMNT-CHANGE-COL
               PIC -(5)9.99
               FROM CHANGE OF BUDGET-CHANGE(BUDGET-IDX)
               FOREGROUND-COLOR 2.
           03  LINE BUDGET-LINE COLUMN AMNT-CURRENCY-COL
               FROM WS-CURRENCY.
           03  LINE BUDGET-LINE COLUMN AMNT-COMMENT-COL
               PIC X(26)
               FROM COMMENT OF BUDGET-CHANGE(BUDGET-IDX).

       PROCEDURE DIVISION.
           PERFORM READ-BUDGET-CSV.
           MOVE INITIAL-BUDGET TO FINAL-BUDGET
           MOVE INITIAL-BUDGET TO POTENTIAL-BUDGET

           DISPLAY MAIN-SCREEN.
           MOVE 0 TO CURSOR-LINE, CURSOR-COL.

           SET BUDGET-IDX TO 1.
           SET BUDGET-LINE TO 4.
           PERFORM VARYING LOOP
                   FROM 1 BY 1
                   UNTIL LOOP > BUDGET-ENTRY-COUNT
               EVALUATE TRUE
      *            Skip if no changes or if data is only for testing
      *            purposes.
                   WHEN (CHANGE OF BUDGET-CHANGE(BUDGET-IDX) = 0)
                     OR TEST-ONLY OF BUDGET-CHANGE(BUDGET-IDX)
                       SET BUDGET-IDX UP BY 1
                       CONTINUE
               END-EVALUATE
               
               IF CHANGE OF BUDGET-CHANGE(BUDGET-IDX) < 0 THEN
                    DISPLAY BUDGET-CHANGE-TEXT-N
               ELSE DISPLAY BUDGET-CHANGE-TEXT-P END-IF

               COMPUTE POTENTIAL-BUDGET
                       = POTENTIAL-BUDGET
                       + CHANGE OF BUDGET-CHANGE(BUDGET-IDX)
               IF NOT POTENTIAL-ONLY OF BUDGET-CHANGE(BUDGET-IDX) THEN
                   COMPUTE FINAL-BUDGET
                         = FINAL-BUDGET
                         + CHANGE OF BUDGET-CHANGE(BUDGET-IDX)
               END-IF
               SET BUDGET-IDX UP BY 1
               ADD 1 TO BUDGET-LINE
           END-PERFORM.
           GOBACK.

       READ-BUDGET-CSV.
           SET BUDGET-IDX TO 1.
           OPEN INPUT BUDGET-FILE.
           PERFORM UNTIL EXIT
               READ BUDGET-FILE INTO CSV-LINE
                   AT END EXIT PERFORM
                   NOT AT END PERFORM PROCESS-CSV-LINE
               END-READ
           END-PERFORM.
           CLOSE BUDGET-FILE.
           EXIT PARAGRAPH.

       PROCESS-CSV-LINE.
           INITIALIZE CHANGE OF WS-CSV-DATA
                      COMMENT OF WS-CSV-DATA.
           UNSTRING CSV-LINE
               DELIMITED BY ';' INTO
                   CHANGE OF WS-CSV-DATA
                   COMMENT OF WS-CSV-DATA
               ON OVERFLOW
                   MOVE 112 TO RETURN-CODE
                   DISPLAY 'CSV ERROR ON: 'CSV-LINE FOREGROUND-COLOR 4
                   STOP RUN
               NOT ON OVERFLOW
      *            Skip test data altogether from being included in the
      *            actual calculations.
                   IF "TEST" = FUNCTION TRIM(COMMENT OF WS-CSV-DATA)
                        SET TEST-ONLY OF WS-CSV-DATA TO TRUE
                   ELSE SET NOT-TEST-ONLY OF WS-CSV-DATA TO TRUE
                   END-IF
                   
                   SET NOT-POTENTIAL-ONLY OF WS-CSV-DATA TO TRUE
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
                       WHEN CSV-LINE(1:1) = '*'
                           EXIT PARAGRAPH
      *                Mark a potential, but not necessarily happening
      *                budget change with '~' in front of the value.
                       WHEN CSV-LINE(1:1) = '~'
                           MOVE CHANGE OF WS-CSV-DATA TO WS-TMP-STR10
                           MOVE FUNCTION TRIM (WS-TMP-STR10(2:))
                                TO CHANGE OF WS-CSV-DATA
                           SET POTENTIAL-ONLY OF WS-CSV-DATA TO TRUE
                   END-EVALUATE

                   IF CHANGE OF WS-CSV-DATA NOT = SPACES THEN
                       ADD 1 TO BUDGET-ENTRY-COUNT
                       MOVE CORR WS-CSV-DATA
                            TO BUDGET-CHANGE(BUDGET-IDX)
                       SET BUDGET-IDX UP BY 1
                   END-IF
           END-UNSTRING.
           EXIT PARAGRAPH.
