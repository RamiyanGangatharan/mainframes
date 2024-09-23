        IDENTIFICATION DIVISION.
        PROGRAM-ID. A9RET.
        DATE-WRITTEN. APRIL 11 2024.
        AUTHOR. RAMIYAN GANGATHARAN.

        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.

        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT INPUT-FILE
                ASSIGN TO "INFILE"
                ORGANIZATION IS SEQUENTIAL.

            SELECT OUTPUT-FILE
                ASSIGN TO "OUTFILE"
                ORGANIZATION IS SEQUENTIAL.

        DATA DIVISION.
        FILE SECTION.
        FD INPUT-FILE
            RECORDING MODE IS F
            RECORD CONTAINS 108 CHARACTERS.
       01 INPUT-LINE.
          05 IL-TRANSACTION-CODE        PIC X(1).
          05 IL-TRANSACTION-AMOUNT      PIC 9(5)V99.
          05 IL-PAYMENT-TYPE            PIC X(2).
          05 IL-STORE-NUMBER            PIC 9(2).
          05 IL-INVOICE-NUMBER          PIC X(9).
          05 SPLIT-INVOICE REDEFINES IL-INVOICE-NUMBER.
             10 INVOICE-PREFIX-1        PIC X(1).
             10 INVOICE-PREFIX-2        PIC X(1).
             10 DASH                    PIC X(1).
             10 INVOICE-NUMBER          PIC X(6).
          05 IL-SKU                     PIC X(15).

        FD OUTPUT-FILE
            RECORDING MODE IS F
            RECORD CONTAINS 85 CHARACTERS.
       01 OUTPUT-LINE                   PIC X(85).

        WORKING-STORAGE SECTION.
      * END OF FILE FLAG
       01 EOF-FLAG                      PIC X(01)      VALUE 'N'.
          88 EOF                                       VALUE 'Y'.
          88 NOT-EOF                                   VALUE 'N'.

      * END OF PAGE FLAG
       01 EOP-FLAG                      PIC X(1).
          88 EOPG                                      VALUE 'Y'.
          88 NOT-EOPG                                  VALUE 'N'.


       01 WS-R-TOTALS.
          05 WS-R-RECORD-COUNT OCCURS 12 TIMES
                                        PIC 9(05)      VALUE ZERO.
          05 WS-R-RECORD-AMOUNT OCCURS 12 TIMES
                                        PIC 9(08)V99   VALUE ZERO.

       01 WS-TOTAL-R-RECORD-COUNT       PIC 9(05)      VALUE ZERO.
       01 WS-TOTAL-R-RECORD-AMOUNT      PIC 9(08)V99   VALUE ZERO.
       01 WS-TOTAL-TAX-OWED             PIC 9(08)V99   VALUE ZERO.

       01 WS-TOTAL-R-RECORD-AMOUNT-STR  PIC $Z(4).99.


       01 WS-STORE-TALLY.
          05 WS-STORE-SALES-TOTALS OCCURS 12 TIMES.
             10 WS-STORE-AMOUNT         PIC 9(08)V99   VALUE ZERO.

       01 WS-NUMERIC-FIELD              PIC 9(07)V99.
       01 IDX                           PIC 9(02).

       01 WS-MATH.
          05 WSM-PRODUCT-AMT            PIC 9(7)V99.
          05 WSM-TAX-RATE               PIC V9(4)      VALUE 0.13.
          05 WSM-TAX-OWING              PIC 9(4)V99.
          05 WSM-TOTAL-TAX-OWING        PIC 9(4)V99.

       01 WS-REPORT-HEADER.
          05 FILLER                     PIC X(10)      VALUE SPACES.
          05 WS-TITLE                   PIC X(10)      VALUE
                "ASSIGNMENT".
          05 WS-TITLE                   PIC X(2)       VALUE " 9".
          05 FILLER                     PIC X(10)      VALUE SPACES.
          05 WS-TITLE                   PIC X(8)       VALUE "RAMIYAN ".
          05 WS-TITLE                   PIC X(11)      VALUE
                "GANGATHARAN".
          05 FILLER                     PIC X(10)      VALUE SPACES.
          05 WS-TITLE                   PIC X(10)      VALUE
                "4/12/2024".

       01 WS-REPORT-HEADER2.
          05 FILLER                     PIC X(30)      VALUE SPACES.
          05 WS-TITLE                   PIC X(25)      VALUE
                "RETURNS REPORT".

       01 WS-PAGE-HEADER1.
          05 FILLER                     PIC X(4)       VALUE SPACES.
          05 WSH-TRANSACTION-CODE1      PIC X(6)       VALUE "TRANS".
          05 FILLER                     PIC X(4)       VALUE SPACES.
          05 WSH-TRANSACTION-AMOUNT1    PIC X(5)       VALUE "TRANS".
          05 FILLER                     PIC X(4)       VALUE SPACES.
          05 WSH-PAYMENT-TYPE1          PIC X(3)       VALUE "PAY".
          05 FILLER                     PIC X(4)       VALUE SPACES.
          05 WSH-STORE-NUMBER           PIC X(6)       VALUE "STORE".
          05 FILLER                     PIC X(2)       VALUE SPACES.
          05 WSH-INVOICE-NUMBER         PIC X(9)       VALUE "INVOICE".
          05 FILLER                     PIC X(8)       VALUE SPACES.
          05 WSH-SKU                    PIC X(6)       VALUE "SKU".
          05 FILLER                     PIC X(12)      VALUE SPACES.
          05 WSH-TAXES                  PIC X(11)      VALUE "TAXES".

       01 WS-PAGE-HEADER2.
          05 FILLER                     PIC X(4)       VALUE SPACES.
          05 WSH-TRANSACTION-CODE2      PIC X(6)       VALUE "CODE  ".
          05 FILLER                     PIC X(5)       VALUE SPACES.
          05 WSH-TRANSACTION-AMOUNT2    PIC X(5)       VALUE "AMT  ".
          05 FILLER                     PIC X(3)       VALUE SPACES.
          05 WSH-PAYMENT-TYPE2          PIC X(4)       VALUE "TYPE".
          05 FILLER                     PIC X(46)      VALUE SPACES.
          05 WSH-TAXES2                 PIC X(5)       VALUE "OWING".

       01 WS-FOOTER.
          05 WS-FOOTER-PAGE-NUMBER      PIC X(7)       VALUE "Page: ".

       01 WS-PAGE-NUMBER                PIC Z.


       01 WS-DETAIL-LINE.
          05 FILLER                     PIC X(6)       VALUE SPACES.
          05 WSD-TRANSACTION-CODE       PIC X(1)       VALUE SPACES.
          05 FILLER                     PIC X(4)       VALUE SPACES.
          05 WSD-TRANSACTION-AMOUNT     PIC $Z(5).99.
          05 FILLER                     PIC X(3)       VALUE SPACES.
          05 WSD-PAYMENT-TYPE           PIC X(2)       VALUE SPACES.
          05 FILLER                     PIC X(3)       VALUE SPACES.
          05 WSD-STORE-NUMBER           PIC Z(3)       VALUE ZEROS.
          05 FILLER                     PIC X(4)       VALUE SPACES.
          05 WSD-INVOICE-NUMBER         PIC X(9)       VALUE SPACES.
          05 FILLER                     PIC X(5)       VALUE SPACES.
          05 WSD-SKU                    PIC X(15)      VALUE SPACES.
          05 FILLER                     PIC X(8)       VALUE SPACES.
          05 WSD-TAXES-OWING            PIC $Z(3).99.

       01 WS-REPORT-FOOTER.
          05 FILLER                     PIC X(30)      VALUE SPACES.
          05 WS-RPT-FOOTER-LINE1        PIC X(30)      VALUE
                "RETURNS SUMMARY".

       01 WS-COUNTERS.
          05 WS-ROW-COUNTER             PIC 9(2)       VALUE ZERO.
          05 WS-PAGE-COUNTER            PIC 9(1)       VALUE ZERO.
          05 WS-SALES-COUNTER           PIC 9(2)       VALUE ZERO.
          05 WS-LAYAWAY-COUNTER         PIC 9(2)       VALUE ZERO.
          05 WS-SL-COUNTER              PIC 9(2)       VALUE ZERO.

       01 WS-IDX-STRING                 PIC Z(2).
       01 WS-NUM-FIELD-STRING           PIC ZZZ,ZZZ,ZZ9.99.


        PROCEDURE DIVISION.
       000-MAIN.
           PERFORM 100-OPEN-FILES.
           PERFORM 700-DISPLAY-REPORT-HEADER.
           PERFORM 750-DISPLAY-PAGE-HEADER.
           MOVE 0 TO WS-PAGE-COUNTER.
           PERFORM UNTIL EOF
                   INITIALIZE WS-DETAIL-LINE
                   PERFORM 150-READ-FILES
                   PERFORM 200-PROCESS-RECORDS
                   PERFORM 210-PAGING
                   PERFORM 250-UPDATE-TOTALS
           END-PERFORM.
           PERFORM 300-PRINT-TOTALS.
           PERFORM 900-CLOSE-FILES.
           GOBACK.

       100-OPEN-FILES.
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.

       150-READ-FILES.
           READ INPUT-FILE
           AT END
              SET EOF TO TRUE
           NOT AT END
               PERFORM 250-UPDATE-TOTALS
           END-READ.


       200-PROCESS-RECORDS.
           MOVE IL-TRANSACTION-CODE TO WSD-TRANSACTION-CODE.
           MOVE IL-TRANSACTION-AMOUNT TO WSD-TRANSACTION-AMOUNT.
           MOVE IL-PAYMENT-TYPE TO WSD-PAYMENT-TYPE.
           MOVE IL-STORE-NUMBER TO WSD-STORE-NUMBER.
           MOVE IL-INVOICE-NUMBER TO WSD-INVOICE-NUMBER.
           MOVE IL-SKU TO WSD-SKU.

           PERFORM 220-TAX-PROCESSOR.

           MOVE SPACES TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

       210-PAGING.
           ADD 1 TO WS-ROW-COUNTER.
           IF WS-ROW-COUNTER > 20
              PERFORM 800-DISPLAY-PAGE-FOOTER
              MOVE 1 TO WS-ROW-COUNTER
              ADD 1 TO WS-PAGE-COUNTER
              PERFORM 750-DISPLAY-PAGE-HEADER
           END-IF.

       220-TAX-PROCESSOR.
           MOVE IL-TRANSACTION-AMOUNT TO WSM-PRODUCT-AMT.
           COMPUTE WSM-TAX-OWING = WSM-PRODUCT-AMT * WSM-TAX-RATE.
           ADD WSM-TAX-OWING TO WS-TOTAL-TAX-OWED.
           MOVE WSM-TAX-OWING TO WSD-TAXES-OWING.

       250-UPDATE-TOTALS.
           COMPUTE IDX = FUNCTION NUMVAL-C(IL-STORE-NUMBER(1:2)).
           IF IDX >= 1 AND IDX <= 12 THEN
              IF IL-TRANSACTION-CODE = 'R' THEN
                 ADD 1 TO WS-R-RECORD-COUNT(IDX)
                 ADD IL-TRANSACTION-AMOUNT TO WS-R-RECORD-AMOUNT(IDX)
                 ADD 1 TO WS-TOTAL-R-RECORD-COUNT
                 ADD IL-TRANSACTION-AMOUNT TO WS-TOTAL-R-RECORD-AMOUNT
              END-IF
              ADD IL-TRANSACTION-AMOUNT TO WS-STORE-AMOUNT(IDX)
           END-IF.




       300-PRINT-TOTALS.
           MOVE WS-TOTAL-R-RECORD-AMOUNT TO
              WS-TOTAL-R-RECORD-AMOUNT-STR.
           MOVE ALL "-" TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 12
                   IF WS-STORE-SALES-TOTALS(IDX) > ZERO THEN
                      MOVE IDX TO WS-IDX-STRING
                      MOVE WS-STORE-SALES-TOTALS(IDX) TO
                         WS-NUMERIC-FIELD
                      MOVE WS-NUMERIC-FIELD TO WS-NUM-FIELD-STRING
                      STRING "Store " DELIMITED BY SIZE
                             WS-IDX-STRING DELIMITED BY SIZE
                             " 'R' Record Count: " DELIMITED BY SIZE
                             WS-R-RECORD-COUNT(IDX) DELIMITED BY SIZE
                             " Total: $" DELIMITED BY SIZE
                         INTO OUTPUT-LINE
                      WRITE OUTPUT-LINE AFTER ADVANCING 1 LINE
                   END-IF
           END-PERFORM.

           STRING "Total 'R' Record Count: " DELIMITED BY SIZE
                  WS-TOTAL-R-RECORD-COUNT DELIMITED BY SIZE
                  " Total 'R' Record Amount: $" DELIMITED BY SIZE
                  WS-TOTAL-R-RECORD-AMOUNT-STR DELIMITED BY SIZE
                  " Total Tax Owed: $" DELIMITED BY SIZE
              INTO OUTPUT-LINE.
           WRITE OUTPUT-LINE AFTER ADVANCING 2 LINES.

      * TAKES THE VARIABLES FROM WS-REPORT-HEADER THEN DISPLAYS THEM
       700-DISPLAY-REPORT-HEADER.
           MOVE SPACES TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

           MOVE WS-REPORT-HEADER TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

           MOVE SPACES TO OUTPUT-LINE.
           MOVE WS-REPORT-HEADER2 TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

      * TAKES THE VARIABLES FROM WS-PAGE-HEADER THEN DISPLAYS THEM
       750-DISPLAY-PAGE-HEADER.

           MOVE SPACES TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

           MOVE ALL "_" TO OUTPUT-LINE(3:77).
           WRITE OUTPUT-LINE.

           MOVE WS-PAGE-HEADER1 TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

           MOVE WS-PAGE-HEADER2 TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

           MOVE ALL "-" TO OUTPUT-LINE(3:77).
           WRITE OUTPUT-LINE.

       800-DISPLAY-PAGE-FOOTER.
           MOVE ALL "-" TO OUTPUT-LINE.
           WRITE OUTPUT-LINE AFTER ADVANCING 1 LINE.
           MOVE "Page: " TO WS-FOOTER-PAGE-NUMBER.
           MOVE WS-PAGE-COUNTER TO WS-PAGE-NUMBER.
           STRING WS-FOOTER-PAGE-NUMBER WS-PAGE-NUMBER
              DELIMITED BY SIZE INTO OUTPUT-LINE.
           WRITE OUTPUT-LINE AFTER ADVANCING 1 LINE.

       900-CLOSE-FILES.
           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.

        END PROGRAM A9RET.
