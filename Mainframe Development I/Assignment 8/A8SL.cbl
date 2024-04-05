        IDENTIFICATION DIVISION.
        PROGRAM-ID. A8SL.
        DATE-WRITTEN. MARCH 26, 2024.
        AUTHOR. RAMIYAN GANGATHARAN.
      * DESCRIPTION: COBOL FILE FOR ASSIGNMENT 8.

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

      * THIS SPLITS UP THE DATA INTO USABLE CHUNKS FROM THE RAW DATA.
       01 INPUT-LINE.
          05 IL-TRANSACTION-CODE      PIC X(1).
          05 IL-TRANSACTION-AMOUNT    PIC 9(5)V99.
          05 IL-PAYMENT-TYPE          PIC X(2).
          05 IL-STORE-NUMBER          PIC 9(2).
          05 IL-INVOICE-NUMBER        PIC X(9).
          05 SPLIT-INVOICE REDEFINES IL-INVOICE-NUMBER.
             10 INVOICE-PREFIX-1      PIC X(1).
             10 INVOICE-PREFIX-2      PIC X(1).
             10 DASH                  PIC X(1).
             10 INVOICE-NUMBER        PIC X(6).
          05 IL-SKU                   PIC X(15).


       FD OUTPUT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 100 CHARACTERS.

       01 OUTPUT-LINE                 PIC X(100).


        WORKING-STORAGE SECTION.

       01 EOF-FLAG                    PIC X(1).
          88 EOF                                   VALUE 'Y'.
          88 NOT-EOF                               VALUE 'N'.

       01 EOP-FLAG                    PIC X(1).
          88 EOPG                                  VALUE 'Y'.
          88 NOT-EOPG                              VALUE 'N'.

       01 WS-TRANSACTION-CODE-OPTIONS PIC X(1).
          88 TCO-SALES                             VALUE 'S'.
          88 TCO-LAYAWAYS                          VALUE 'L'.

       01 WS-PAYMENT-TYPE-OPTIONS     PIC X(2).
          88 PTO-DEBIT                             VALUE 'DB'.
          88 PTO-CREDIT                            VALUE 'CR'.
          88 PTO-CASH                              VALUE 'CA'.

       01 WS-STORE-CODES              PIC 9(2).
          88 STORE-ONE                             VALUE 01.
          88 STORE-TWO                             VALUE 02.
          88 STORE-THREE                           VALUE 03.
          88 STORE-FOUR                            VALUE 04.
          88 STORE-FIVE                            VALUE 05.
          88 STORE-TWELVE                          VALUE 12.


       01 WS-REPORT-HEADER.
          05 FILLER                   PIC X(10)    VALUE SPACES.
          05 WS-TITLE                 PIC X(10)    VALUE "ASSIGNMENT".
          05 WS-TITLE                 PIC X(2)     VALUE " 8".
          05 FILLER                   PIC X(10)    VALUE SPACES.
          05 WS-TITLE                 PIC X(8)     VALUE "RAMIYAN ".
          05 WS-TITLE                 PIC X(11)    VALUE "GANGATHARAN".
          05 FILLER                   PIC X(10)    VALUE SPACES.
          05 WS-TITLE                 PIC X(10)    VALUE "24/03/2024".

       01 WS-REPORT-HEADER2.
          05 FILLER                   PIC X(30)    VALUE SPACES.
          05 WS-TITLE                 PIC X(25)    VALUE
                "SALES AND LAYAWAYS REPORT".

       01 WS-PAGE-HEADER1.
          05 FILLER                   PIC X(4)     VALUE SPACES.
          05 WSH-TRANSACTION-CODE1    PIC X(6)     VALUE "TRANS".
          05 FILLER                   PIC X(4)     VALUE SPACES.
          05 WSH-TRANSACTION-AMOUNT1  PIC X(5)     VALUE "TRANS".
          05 FILLER                   PIC X(4)     VALUE SPACES.
          05 WSH-PAYMENT-TYPE1        PIC X(3)     VALUE "PAY".
          05 FILLER                   PIC X(4)     VALUE SPACES.
          05 WSH-STORE-NUMBER         PIC X(6)     VALUE "STORE".
          05 FILLER                   PIC X(2)     VALUE SPACES.
          05 WSH-INVOICE-NUMBER       PIC X(9)     VALUE "INVOICE".
          05 FILLER                   PIC X(8)     VALUE SPACES.
          05 WSH-SKU                  PIC X(6)     VALUE "SKU".
          05 FILLER                   PIC X(12)    VALUE SPACES.
          05 WSH-TAXES                PIC X(11)    VALUE "TAXES".

       01 WS-PAGE-HEADER2.
          05 FILLER                   PIC X(4)     VALUE SPACES.
          05 WSH-TRANSACTION-CODE2    PIC X(6)     VALUE "CODE  ".
          05 FILLER                   PIC X(5)     VALUE SPACES.
          05 WSH-TRANSACTION-AMOUNT2  PIC X(5)     VALUE "AMT  ".
          05 FILLER                   PIC X(3)     VALUE SPACES.
          05 WSH-PAYMENT-TYPE2        PIC X(4)     VALUE "TYPE".
          05 FILLER                   PIC X(46)    VALUE SPACES.
          05 WSH-TAXES2               PIC X(5)     VALUE "OWING".

       01 WS-FOOTER.
          05 WS-FOOTER-PAGE-NUMBER    PIC X(7)     VALUE "Page: ".

       01 WS-PAGE-NUMBER              PIC Z.


      *    TODO:
      *    TOTAL S&L RECORDS
      *    TOTAL AMOUNT FOR S&L
      *    TOTAL S RECORDS
      *    TOTAL AMOUNT FOR S
      *    TOTAL L RECORDS
      *    TOTAL AMOUNT FOR L
      *    THE NUMBER OF EACH PAYMENT TYPE
      *    THE PERCENTAGE OF EACH PAYMENT TYPE
      *    TOTAL TAX OWING
      *    THE STORE NUMBER WITH THE HIGHEST AND LOWEST S&L AMOUNT

       01 WS-REPORT-FOOTER.
          05 FILLER                   PIC X(30)    VALUE SPACES.
          05 WS-RPT-FOOTER-LINE1      PIC X(30)    VALUE
                "SALES AND LAYAWAYS SUMMARY".
          05 WS-TOTAL-SL-RECORDS      PIC 9(3).
          05 WS-TOTAL-S-RECORDS       PIC 9(2).
          05 WS-TOTAL-L-RECORDS       PIC 9(2).
          05 WS-TOTAL-DEBIT-TRANS     PIC 9(2).
          05 WS-TOTAL-CREDIT-TRANS    PIC 9(2).
          05 WS-TOTAL-CASH-TRANS      PIC 9(2).

          05 WS-TOTAL-SL-AMT          PIC 9(6)V99.
          05 WS-TOTAL-S-AMT           PIC 9(6)V99.
          05 WS-TOTAL-L-AMT           PIC 9(6)V99.

          05 WS-TOTAL-SL-AMT-TALLY    PIC 9(6)V99.
          05 WS-TOTAL-S-AMT-TALLY     PIC 9(6)V99.
          05 WS-TOTAL-L-AMT-TALLY     PIC 9(6)V99.

          05 WS-TOTAL-TAX-OWING       PIC 9(6)V99.
          05 WS-HIGHEST-SL-AMT        PIC 9(6)V99.
          05 WS-LOWEST-SL-AMT         PIC 9(6)V99.


       01 WS-DETAIL.
          05 FILLER                   PIC X(6)     VALUE SPACES.
          05 WSD-TRANSACTION-CODE     PIC X(1)     VALUE SPACES.
          05 FILLER                   PIC X(4)     VALUE SPACES.
          05 WSD-TRANSACTION-AMOUNT   PIC $Z(5).99.
          05 FILLER                   PIC X(3)     VALUE SPACES.
          05 WSD-PAYMENT-TYPE         PIC X(2)     VALUE SPACES.
          05 FILLER                   PIC X(3)     VALUE SPACES.
          05 WSD-STORE-NUMBER         PIC Z(3)     VALUE ZEROS.
          05 FILLER                   PIC X(4)     VALUE SPACES.
          05 WSD-INVOICE-NUMBER       PIC X(9)     VALUE SPACES.
          05 FILLER                   PIC X(5)     VALUE SPACES.
          05 WSD-SKU                  PIC X(15)    VALUE SPACES.
          05 FILLER                   PIC X(5)     VALUE SPACES.
          05 WSD-TAXES-OWING          PIC $Z(3).99.

          05 WSD-SALES-COUNT          PIC Z(2)     VALUE ZEROS.
          05 WSD-LAYAWAYS-COUNT       PIC Z(2)     VALUE ZEROS.
          05 WSD-SL-COUNT             PIC Z(3)     VALUE ZEROS.

          05 WSD-SALES-AMOUNT         PIC $Z(6).99.
          05 WSD-LAYAWAYS-AMOUNT      PIC $Z(6).99.
          05 WSD-SL-AMOUNT            PIC $Z(6).99.

          05 WSD-TOTAL-DEBIT-TRANS    PIC Z(2).
          05 WSD-TOTAL-CREDIT-TRANS   PIC Z(2).
          05 WSD-TOTAL-CASH-TRANS     PIC Z(2).

          05 WSD-TOTAL-TAX-OWING      PIC $Z(6).99.
          05 WSD-HIGHEST-SL-AMT       PIC $Z(6).99.
          05 WSD-LOWEST-SL-AMT        PIC $Z(6).99.

       01 WS-MATH.
          05 WSM-PRODUCT-AMT          PIC 9(7)V99.
          05 WSM-TAX-RATE             PIC V9(4)    VALUE 0.13.
          05 WSM-TAX-OWING            PIC 9(6)V99.

       01 WS-COUNTERS.
          05 WS-ROW-COUNTER           PIC 9(2)     VALUE ZERO.
          05 WS-PAGE-COUNTER          PIC 9(1)     VALUE ZERO.
          05 WS-SALES-COUNTER         PIC 9(2)     VALUE ZERO.
          05 WS-LAYAWAY-COUNTER       PIC 9(2)     VALUE ZERO.
          05 WS-SL-COUNTER            PIC 9(2)     VALUE ZERO.

       01 WS-SUMMARY.
          05 WS-SALES-AMOUNT          PIC 9(6)V99.
          05 WS-LAYAWAYS-AMOUNT       PIC 9(6)V99.
          05 WS-SL-AMOUNT             PIC 9(6)V99.

        PROCEDURE DIVISION.
       000-MAIN.
           PERFORM 100-OPEN-FILES.
           PERFORM 700-DISPLAY-REPORT-HEADER.
           PERFORM 750-DISPLAY-PAGE-HEADER.
           MOVE SPACES TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.
           PERFORM UNTIL EOF
               INITIALIZE WS-DETAIL
               PERFORM 150-READ-FILES
               PERFORM 200-PROCESS-RECORDS
               MOVE SPACES TO OUTPUT-LINE
               WRITE OUTPUT-LINE
           END-PERFORM.

           PERFORM 850-DISPLAY-REPORT-FOOTER.
           PERFORM 900-CLOSE-FILES.
           GOBACK.

       100-OPEN-FILES.
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.

       150-READ-FILES.
           READ INPUT-FILE
           AT END
              SET EOF TO TRUE
           END-READ.

       200-PROCESS-RECORDS.

           PERFORM 210-PAGER.
           PERFORM 220-TAX-PROCESSOR.
           PERFORM 230-COUNT-PROCESSOR.
           PERFORM 240-AMOUNT-PROCESSOR.


       210-PAGER.
           ADD 1 TO WS-ROW-COUNTER.

           IF WS-ROW-COUNTER IS GREATER THAN 20
              MOVE 0 TO WS-ROW-COUNTER
              ADD 1 TO WS-PAGE-COUNTER
              PERFORM 800-DISPLAY-PAGE-FOOTER
              MOVE SPACES TO OUTPUT-LINE
              WRITE OUTPUT-LINE
              PERFORM 750-DISPLAY-PAGE-HEADER
           END-IF.

       220-TAX-PROCESSOR.
           MOVE IL-TRANSACTION-CODE TO WSD-TRANSACTION-CODE.
           MOVE IL-TRANSACTION-AMOUNT TO WSD-TRANSACTION-AMOUNT.
           MOVE IL-PAYMENT-TYPE TO WSD-PAYMENT-TYPE.
           MOVE IL-STORE-NUMBER TO WSD-STORE-NUMBER.
           MOVE IL-INVOICE-NUMBER TO WSD-INVOICE-NUMBER.
           MOVE IL-SKU TO WSD-SKU.

           MOVE IL-TRANSACTION-AMOUNT TO WSM-PRODUCT-AMT.
           COMPUTE WSM-TAX-OWING = WSM-PRODUCT-AMT * WSM-TAX-RATE.
           MOVE WSM-TAX-OWING TO WSD-TAXES-OWING.

           MOVE WS-DETAIL TO OUTPUT-LINE.
           WRITE OUTPUT-LINE AFTER ADVANCING 1 LINE.

       230-COUNT-PROCESSOR.

           MOVE WSD-TRANSACTION-CODE TO WS-TRANSACTION-CODE-OPTIONS.
           MOVE WSD-PAYMENT-TYPE TO WS-PAYMENT-TYPE-OPTIONS.

           IF TCO-SALES THEN ADD 1 TO WS-TOTAL-S-RECORDS.
           IF TCO-LAYAWAYS THEN ADD 1 TO WS-TOTAL-L-RECORDS.

           IF PTO-DEBIT THEN ADD 1 TO WS-TOTAL-DEBIT-TRANS.
           IF PTO-CREDIT THEN ADD 1 TO WS-TOTAL-CREDIT-TRANS.
           IF PTO-CASH THEN ADD 1 TO WS-TOTAL-CASH-TRANS.

       240-AMOUNT-PROCESSOR.
           EVALUATE TRUE

                WHEN TCO-SALES
                    ADD IL-TRANSACTION-AMOUNT TO WS-SALES-AMOUNT
                    MOVE WS-SALES-AMOUNT TO WS-TOTAL-S-AMT-TALLY

                WHEN TCO-LAYAWAYS
                    ADD IL-TRANSACTION-AMOUNT TO WS-LAYAWAYS-AMOUNT
                    MOVE WS-LAYAWAYS-AMOUNT TO WS-TOTAL-L-AMT-TALLY

           END-EVALUATE.

       700-DISPLAY-REPORT-HEADER.
           MOVE SPACES TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

           MOVE WS-REPORT-HEADER TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

           MOVE SPACES TO OUTPUT-LINE.
           MOVE WS-REPORT-HEADER2 TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

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
           MOVE SPACES TO OUTPUT-LINE.
           WRITE OUTPUT-LINE AFTER ADVANCING 1 LINE.

           MOVE WS-PAGE-COUNTER TO WS-PAGE-NUMBER.
           STRING "Page: " DELIMITED BY SIZE
                  WS-PAGE-NUMBER DELIMITED BY SIZE
              INTO OUTPUT-LINE.
           WRITE OUTPUT-LINE AFTER ADVANCING 1 LINE.


       850-DISPLAY-REPORT-FOOTER.

           MOVE SPACES TO OUTPUT-LINE.

           WRITE OUTPUT-LINE.

           MOVE ALL "_" TO OUTPUT-LINE(3:77).
           WRITE OUTPUT-LINE.

           MOVE WS-RPT-FOOTER-LINE1 TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

           MOVE SPACES TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

           MOVE WS-TOTAL-S-RECORDS TO WSD-SALES-COUNT.
           MOVE WS-TOTAL-L-RECORDS TO WSD-LAYAWAYS-COUNT.
           MOVE WS-TOTAL-S-AMT-TALLY TO WSD-SALES-AMOUNT.
           MOVE WS-TOTAL-L-AMT-TALLY TO WSD-LAYAWAYS-AMOUNT.
           MOVE WS-TOTAL-CREDIT-TRANS TO WSD-TOTAL-CREDIT-TRANS.
           MOVE WS-TOTAL-DEBIT-TRANS TO WSD-TOTAL-DEBIT-TRANS.
           MOVE WS-TOTAL-CASH-TRANS TO WSD-TOTAL-CASH-TRANS.


           ADD WS-TOTAL-S-RECORDS TO WS-TOTAL-L-RECORDS GIVING
           WS-TOTAL-SL-RECORDS.

           MOVE WS-TOTAL-SL-RECORDS TO WSD-SL-COUNT.

           ADD  WS-TOTAL-S-AMT-TALLY
           TO WS-TOTAL-L-AMT-TALLY GIVING WS-TOTAL-SL-AMT.
           MOVE WS-TOTAL-SL-AMT TO WSD-SL-AMOUNT.

           STRING "TOTAL SALES RECORDS: ", WSD-SALES-COUNT
           DELIMITED BY SIZE INTO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

           STRING "TOTAL LAYAWAY RECORDS: ", WSD-LAYAWAYS-COUNT
           DELIMITED BY SIZE INTO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

           STRING "TOTAL SALES AND LAYAWAY RECORDS: ", WSD-SL-COUNT
           DELIMITED BY SIZE INTO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

           MOVE SPACES TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

           STRING "TOTAL SALES AMOUNT: ", WSD-SALES-AMOUNT
           DELIMITED BY SIZE INTO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

           STRING "TOTAL LAYAWAY AMOUNT: ", WSD-LAYAWAYS-AMOUNT
           DELIMITED BY SIZE INTO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

           STRING "TOTAL SALES AND LAYAWAY AMOUNT: ", WSD-SL-AMOUNT
           DELIMITED BY SIZE INTO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

           MOVE SPACES TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

           STRING "TOTAL CREDIT TRANSACTIONS: ", WSD-TOTAL-CREDIT-TRANS
           DELIMITED BY SIZE INTO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

           STRING "TOTAL DEBIT TRANSACTIONS: ", WSD-TOTAL-DEBIT-TRANS
           DELIMITED BY SIZE INTO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

           STRING "TOTAL CASH TRANSACTIONS: ", WSD-TOTAL-CASH-TRANS
           DELIMITED BY SIZE INTO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

       900-CLOSE-FILES.
           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.

        END PROGRAM A8SL.
