       IDENTIFICATION DIVISION.
       PROGRAM-ID.   BUDGET.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. 17 June, 2025.
      ******************************************************************
      *
      * Budget app wrapper.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PARM.
           05  PARM-LENGTH      PIC 999.
           05  PARM-DATA        PIC X(100).

       PROCEDURE DIVISION.
           ACCEPT PARM-DATA FROM COMMAND-LINE.
           IF PARM-DATA EQUAL SPACES THEN
               ACCEPT PARM-DATA FROM ENVIRONMENT "PARM"
           END-IF.
           MOVE FUNCTION STORED-CHAR-LENGTH(PARM-DATA) TO PARM-LENGTH.
           CALL 'BUDGET-MAIN' USING PARM.
           GOBACK.
