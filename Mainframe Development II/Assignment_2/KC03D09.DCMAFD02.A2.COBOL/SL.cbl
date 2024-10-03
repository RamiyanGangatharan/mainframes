       IDENTIFICATION DIVISION.
       PROGRAM-ID. SL.
       DATE-WRITTEN. March 26, 2024.
       AUTHOR. Christian Weersink.
      *Description: Does some tax math for s and l data
      *             Used by Ramiyan Gangatharan
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
      * input-file declaration
           SELECT INPUT-FILE
               ASSIGN TO INFILE
               ORGANIZATION IS SEQUENTIAL.
      *
      * output-file declaration
           SELECT OUTPUT-FILE
               ASSIGN TO OUTFILE
               ORGANIZATION IS SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD INPUT-FILE
           RECORDING MODE IS F
           DATA RECORD IS INPUT-LINE
           RECORD CONTAINS 36 CHARACTERS.
      *
       01 INPUT-LINE.
          05 IL-CODE               PIC X.
          05 IL-AMT                PIC 9(5)V99.
          05 IL-PAY-TYPE           PIC XX.
          05 IL-STORE              PIC 99.
          05 IL-INVOICE            PIC X(9).
          05 IL-SKU                PIC X(15).
      *
       FD OUTPUT-FILE
           RECORDING MODE IS F
           DATA RECORD IS OUTPUT-LINE
           RECORD CONTAINS 76 CHARACTERS.
      *
       01 OUTPUT-LINE              PIC X(76).
      *
       WORKING-STORAGE SECTION.
      *
       01 WS-HEADER1.
          05 FILLER                PIC X(18)
                                                  VALUE
                                                   "Christian Weersink".

       01 WS-HEADER2.
          05 FILLER                PIC X(30)      VALUE SPACES.
          05 FILLER                PIC X(15)
                                                  VALUE
                                                      "Assignment 8 SL".

       01 WS-HEADER3.
          05 FILLER                PIC X(69)      VALUE SPACES.
          05 FILLER                PIC X(6)       VALUE "PAGE #".
          05 WS-PAGE-NUMBER        PIC 9.

       01 WS-HEADER-COLUMNS.
          05 FILLER                PIC X(4)       VALUE "TYPE".
          05 FILLER                PIC X(3)       VALUE SPACES.
          05 FILLER                PIC X(6)       VALUE "AMOUNT".
          05 FILLER                PIC X(3)       VALUE SPACES.
          05 FILLER                PIC X(8)       VALUE "PAY TYPE".
          05 FILLER                PIC X(3)       VALUE SPACES.
          05 FILLER                PIC X(5)       VALUE "STORE".
          05 FILLER                PIC X(3)       VALUE SPACES.
          05 FILLER                PIC X(11)      VALUE "INVOICE NUM".
          05 FILLER                PIC X(8)       VALUE SPACES.
          05 FILLER                PIC X(3)       VALUE "SKU".
          05 FILLER                PIC X(10)      VALUE SPACES.
          05 FILLER                PIC X(9)       VALUE "TAX OWING".

       01 WS-LINES.
          05 FILLER                PIC X(4)       VALUE "----".
          05 FILLER                PIC X(3)       VALUE SPACES.
          05 FILLER                PIC X(6)       VALUE "------".
          05 FILLER                PIC X(3)       VALUE SPACES.
          05 FILLER                PIC X(8)       VALUE "--------".
          05 FILLER                PIC X(3)       VALUE SPACES.
          05 FILLER                PIC X(5)       VALUE "-----".
          05 FILLER                PIC X(3)       VALUE SPACES.
          05 FILLER                PIC X(11)      VALUE "-----------".
          05 FILLER                PIC X(8)       VALUE SPACES.
          05 FILLER                PIC X(3)       VALUE "---".
          05 FILLER                PIC X(10)      VALUE SPACES.
          05 FILLER                PIC X(9)       VALUE "---------".



      * FOOTER DETAILS
       01 WS-FOOTER-HEADER.
          05 FILLER                PIC X(20)      VALUE SPACES.
          05 FILLER                PIC X(7)       VALUE "RECORDS".
          05 FILLER                PIC X(7)       VALUE SPACES.
          05 FILLER                PIC X(6)       VALUE "AMOUNT".

       01 SL-FOOTER.
          05 FILLER                PIC X(19)
                                                  VALUE
                                                  "SALES AND LAYAWAYS:".
          05 FILLER                PIC X(5)       VALUE SPACES.
          05 WS-SL-RECORDS         PIC ZZ9.
          05 FILLER                PIC X(3)       VALUE SPACES.
          05 WS-SL-AMT             PIC $$$,$$9.99.

       01 S-FOOTER.
          05 FILLER                PIC X(6)
                                                  VALUE "SALES:".
          05 FILLER                PIC X(3)       VALUE SPACES.
          05 FILLER                PIC X(15)      VALUE SPACES.
          05 WS-S-RECORDS          PIC ZZ9.
          05 FILLER                PIC X(3)       VALUE SPACES.
          05 WS-S-AMT              PIC $$$,$$9.99.

       01 L-FOOTER.
          05 FILLER                PIC X(9)
                                                  VALUE "LAYAWAYS:".
          05 FILLER                PIC X(1)       VALUE SPACES.
          05 FILLER                PIC X(14)      VALUE SPACES.
          05 WS-L-RECORDS          PIC ZZ9.
          05 FILLER                PIC X(3)       VALUE SPACES.
          05 WS-L-AMT              PIC $$$,$$9.99.

       01 CA-FOOTER.
          05 FILLER                PIC X(5)       VALUE "CASH:".
          05 FILLER                PIC X(4)       VALUE SPACES.
          05 FILLER                PIC X(15)      VALUE SPACES.
          05 WS-CA-RECORDS         PIC ZZ9.
          05 FILLER                PIC X(7)       VALUE SPACES.
          05 WS-CA-PERCENT         PIC 99.99.
          05 FILLER                PIC X(1)       VALUE "%".

       01 CR-FOOTER.
          05 FILLER                PIC X(7)       VALUE "CREDIT:".
          05 FILLER                PIC X(2)       VALUE SPACES.
          05 FILLER                PIC X(15)      VALUE SPACES.
          05 WS-CR-RECORDS         PIC ZZ9.
          05 FILLER                PIC X(7)       VALUE SPACES.
          05 WS-CR-PERCENT         PIC 99.99.
          05 FILLER                PIC X(1)       VALUE "%".

       01 DB-FOOTER.
          05 FILLER                PIC X(6)       VALUE "DEBIT:".
          05 FILLER                PIC X(3)       VALUE SPACES.
          05 FILLER                PIC X(15)      VALUE SPACES.
          05 WS-DB-RECORDS         PIC ZZ9.
          05 FILLER                PIC X(7)       VALUE SPACES.
          05 WS-DB-PERCENT         PIC 99.99.
          05 FILLER                PIC X(1)       VALUE "%".


       01 TAX-FOOTER.
          05 FILLER                PIC X(10)      VALUE "TAX TOTAL:".
          05 FILLER                PIC X(20)      VALUE SPACES.
          05 WS-TAX-TOTAL          PIC $$$$$$9.99.


       01 HIGH-STORE.
          05 FILLER                PIC X(20)
                                                  VALUE
                                                 "HIGHEST TOTAL STORE:".
          05 FILLER                PIC X(4)       VALUE SPACES.
          05 HIGHEST-STORE         PIC ZZ9.
          05 FILLER                PIC X(1)       VALUE SPACES.
          05 FILLER                PIC X(3)       VALUE "AT ".
          05 HIGH-AMT              PIC $$,$$9.99.

       01 LOW-STORE.
          05 FILLER                PIC X(20)
                                                  VALUE
                                                 "LOWEST TOTAL STORE: ".
          05 FILLER                PIC X(4)       VALUE SPACES.
          05 LOWEST-STORE          PIC ZZ9.
          05 FILLER                PIC X(1)       VALUE SPACES.
          05 FILLER                PIC X(3)       VALUE "AT ".
          05 LOW-AMT               PIC $$,$$9.99.


      * Detail line


       01 WS-DETAIL.
          05 FILLER                PIC X(2)       VALUE SPACES.
          05 WS-CODE               PIC X.
          05 FILLER                PIC X(1)       VALUE SPACES.
          05 WS-AMT                PIC $$$$$9.99.
          05 FILLER                PIC X(6)       VALUE SPACES.
          05 WS-PAY-TYPE           PIC X(2).
          05 FILLER                PIC X(7)       VALUE SPACES.
          05 WS-STORE              PIC Z9.
          05 FILLER                PIC X(6)       VALUE SPACES.
          05 WS-INVOICE            PIC X(9).
          05 FILLER                PIC X(3)       VALUE SPACES.
          05 WS-SKU                PIC X(15).
          05 FILLER                PIC X(3)       VALUE SPACES.
          05 WS-TAX                PIC $$$$9.99.





      * Math stuff

       01 WS-LINE-COUNT            PIC 999        VALUE ZERO.

       01 WS-MATH.
          05 MATH-TAX              PIC 99999V99   VALUE ZERO.
          05 TAX_MULTIPLIER        PIC 9V99       VALUE 0.13.
          05 TOTAL-TAX             PIC 9(9)V99    VALUE ZERO.
          05 STORE-AMT             PIC 9(9)V99 OCCURS 6 TIMES.
          05 S-AMT                 PIC 9(9)V99    VALUE ZERO.
          05 SL-AMT                PIC 9(9)V99    VALUE ZERO.
          05 L-AMT                 PIC 9(9)V99    VALUE ZERO.

          05 HIGH-WINNER           PIC 9(9)V99    VALUE ZERO.
          05 LOW-WINNER            PIC 9(9)V99    VALUE 500000.
          05 LOWEST-STORE-WINNER   PIC 99.
          05 HIGHEST-STORE-WINNER  PIC 99.

          05 DECIMAL-MATH          PIC 999V9999   VALUE ZERO.
          05 CR-PERCENT            PIC 99V99.
          05 CA-PERCENT            PIC 99V99.
          05 DB-PERCENT            PIC 99V99.

       01 WS-COUNTERS.
          05 PAGE-NUM              PIC 99         VALUE ZERO.
          05 S-COUNT               PIC 999        VALUE ZERO.
          05 L-COUNT               PIC 999        VALUE ZERO.
          05 SL-COUNT              PIC 999        VALUE ZERO.
          05 CA-COUNT              PIC 999        VALUE ZERO.
          05 CR-COUNT              PIC 999        VALUE ZERO.
          05 DB-COUNT              PIC 999        VALUE ZERO.

       01 WS-EOF-FLAG              PIC X          VALUE "N".
       01 WS-SPACES                PIC X(76)      VALUE SPACES.

       01 WS-INDEX                 PIC 99.


       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.

           READ INPUT-FILE
           AT END
              MOVE "Y" TO WS-EOF-FLAG.

           PERFORM 100-PROCESS UNTIL WS-EOF-FLAG = "Y".

           PERFORM 300-FOOTERS.

           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.
      *
           GOBACK.
      *


       100-PROCESS.
           ADD 1 TO PAGE-NUM.
           MOVE PAGE-NUM TO WS-PAGE-NUMBER.

           IF (PAGE-NUM > 1) THEN
              WRITE OUTPUT-LINE AFTER ADVANCING PAGE
              MOVE WS-HEADER3 TO OUTPUT-LINE
              WRITE OUTPUT-LINE
              MOVE WS-SPACES TO OUTPUT-LINE
              WRITE OUTPUT-LINE
              MOVE WS-HEADER-COLUMNS TO OUTPUT-LINE
              WRITE OUTPUT-LINE
              MOVE WS-LINES TO OUTPUT-LINE
              WRITE OUTPUT-LINE
           ELSE
              MOVE WS-HEADER1 TO OUTPUT-LINE
              WRITE OUTPUT-LINE
              MOVE WS-HEADER2 TO OUTPUT-LINE
              WRITE OUTPUT-LINE
              MOVE WS-HEADER3 TO OUTPUT-LINE
              WRITE OUTPUT-LINE
              MOVE WS-SPACES TO OUTPUT-LINE
              WRITE OUTPUT-LINE
              MOVE WS-HEADER-COLUMNS TO OUTPUT-LINE
              WRITE OUTPUT-LINE
              MOVE WS-LINES TO OUTPUT-LINE
              WRITE OUTPUT-LINE
           END-IF.

           PERFORM 200-PROCESS-DATA
              VARYING WS-LINE-COUNT FROM 1 BY 1
              UNTIL(WS-LINE-COUNT > 20 OR WS-EOF-FLAG = "Y").

           MOVE WS-SPACES TO OUTPUT-LINE
           WRITE OUTPUT-LINE.


       200-PROCESS-DATA.
           MOVE IL-CODE TO WS-CODE.
           MOVE IL-AMT TO WS-AMT.
           MOVE IL-PAY-TYPE TO WS-PAY-TYPE.
           MOVE IL-STORE TO WS-STORE.
           MOVE IL-INVOICE TO WS-INVOICE.
           MOVE IL-SKU TO WS-SKU.

           ADD 1 TO SL-COUNT.
           ADD IL-AMT TO SL-AMT.


           IF (IL-CODE = "S") THEN
              ADD 1 TO S-COUNT
              ADD IL-AMT TO S-AMT
           ELSE
              ADD 1 TO L-COUNT
              ADD IL-AMT TO L-AMT
           END-IF.

           IF (IL-PAY-TYPE = "CA") THEN
              ADD 1 TO CA-COUNT
           ELSE
              IF (IL-PAY-TYPE = "CR") THEN
                 ADD 1 TO CR-COUNT
              ELSE
                 IF (IL-PAY-TYPE = "DB") THEN
                    ADD 1 TO DB-COUNT
                 END-IF
              END-IF
           END-IF.

           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 6
                   IF (IL-STORE = WS-INDEX) THEN
                      ADD IL-AMT TO STORE-AMT(WS-INDEX)
                   ELSE
                      IF (IL-STORE = 12 AND WS-INDEX = 6) THEN
                         ADD IL-AMT TO STORE-AMT(6)
                      END-IF
                   END-IF
           END-PERFORM.


           MULTIPLY IL-AMT BY TAX_MULTIPLIER GIVING MATH-TAX ROUNDED.

           ADD MATH-TAX TO TOTAL-TAX.

           MOVE MATH-TAX TO WS-TAX.

           WRITE OUTPUT-LINE FROM WS-DETAIL.

           MOVE ZEROS TO MATH-TAX.


           READ INPUT-FILE
           AT END
              MOVE "Y" TO WS-EOF-FLAG.


       300-FOOTERS.


           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 6
                   IF (STORE-AMT(WS-INDEX) > HIGH-WINNER) THEN
                      MOVE STORE-AMT(WS-INDEX) TO HIGH-WINNER
                      MOVE WS-INDEX TO HIGHEST-STORE-WINNER
                   END-IF

                   IF (STORE-AMT(WS-INDEX) < LOW-WINNER) THEN
                      MOVE STORE-AMT(WS-INDEX) TO LOW-WINNER
                      MOVE WS-INDEX TO LOWEST-STORE-WINNER
                   END-IF

           END-PERFORM.

           IF (LOWEST-STORE-WINNER = 6) THEN
              MOVE 12 TO LOWEST-STORE
           ELSE
              MOVE LOWEST-STORE-WINNER TO LOWEST-STORE
           END-IF.

           IF (HIGHEST-STORE-WINNER = 6) THEN
              MOVE 12 TO HIGHEST-STORE
           ELSE
              MOVE HIGHEST-STORE-WINNER TO HIGHEST-STORE
           END-IF.

           MOVE HIGH-WINNER TO HIGH-AMT.
           MOVE LOW-WINNER TO LOW-AMT.

           DIVIDE CR-COUNT BY SL-COUNT GIVING DECIMAL-MATH ROUNDED.
           MULTIPLY DECIMAL-MATH BY 100 GIVING CR-PERCENT.
           MOVE CR-PERCENT TO WS-CR-PERCENT.
           MOVE CR-COUNT TO WS-CR-RECORDS.
           MOVE ZERO TO DECIMAL-MATH.

           DIVIDE CA-COUNT BY SL-COUNT GIVING DECIMAL-MATH ROUNDED.
           MULTIPLY DECIMAL-MATH BY 100 GIVING CA-PERCENT.
           MOVE CA-PERCENT TO WS-CA-PERCENT.
           MOVE CA-COUNT TO WS-CA-RECORDS.
           MOVE ZERO TO DECIMAL-MATH.

           DIVIDE DB-COUNT BY SL-COUNT GIVING DECIMAL-MATH ROUNDED.
           MULTIPLY DECIMAL-MATH BY 100 GIVING DB-PERCENT.
           MOVE DB-PERCENT TO WS-DB-PERCENT.
           MOVE DB-COUNT TO WS-DB-RECORDS.

           MOVE SL-COUNT TO WS-SL-RECORDS.
           MOVE SL-AMT TO WS-SL-AMT.

           MOVE S-COUNT TO WS-S-RECORDS.
           MOVE S-AMT TO WS-S-AMT.

           MOVE L-COUNT TO WS-L-RECORDS.
           MOVE L-AMT TO WS-L-AMT.


           MOVE TOTAL-TAX TO WS-TAX-TOTAL.

           WRITE OUTPUT-LINE FROM WS-SPACES.

           WRITE OUTPUT-LINE FROM WS-FOOTER-HEADER.
           WRITE OUTPUT-LINE FROM SL-FOOTER.
           WRITE OUTPUT-LINE FROM L-FOOTER.
           WRITE OUTPUT-LINE FROM S-FOOTER.

           WRITE OUTPUT-LINE FROM WS-SPACES.

           WRITE OUTPUT-LINE FROM TAX-FOOTER.
           WRITE OUTPUT-LINE FROM WS-SPACES.

           WRITE OUTPUT-LINE FROM CA-FOOTER.
           WRITE OUTPUT-LINE FROM CR-FOOTER.
           WRITE OUTPUT-LINE FROM DB-FOOTER.

           WRITE OUTPUT-LINE FROM WS-SPACES.

           WRITE OUTPUT-LINE FROM HIGH-STORE.
           WRITE OUTPUT-LINE FROM LOW-STORE.



       END PROGRAM SL.
