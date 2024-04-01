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
           RECORD CONTAINS 165 CHARACTERS.

       01 OUTPUT-LINE                 PIC X(165).


        WORKING-STORAGE SECTION.

       01 EOF-FLAG                    PIC X(1).
          88 EOF                                     VALUE 'Y'.
          88 NOT-EOF                                 VALUE 'N'.

       01 EOP-FLAG                    PIC X(1).
          88 EOPG                                    VALUE 'Y'.
          88 NOT-EOPG                                VALUE 'N'.

       01 PAGE-LIMITS                 PIC 9(2).
          88 ROW-MINIMUM                             VALUE 0.
          88 ROW-MAXIMUM                             VALUE 20.

       01 WS-REPORT-HEADER.

       01 WS-PAGE-HEADER1.
          05 FILLER                   PIC X(4)       VALUE SPACES.
          05 WSH-TRANSACTION-CODE1    PIC X(6)       VALUE "TRANS".
          05 FILLER                   PIC X(4)       VALUE SPACES.
          05 WSH-TRANSACTION-AMOUNT1  PIC X(5)       VALUE "TRANS".
          05 FILLER                   PIC X(4)       VALUE SPACES.
          05 WSH-PAYMENT-TYPE1        PIC X(3)       VALUE "PAY".
          05 FILLER                   PIC X(4)       VALUE SPACES.
          05 WSH-STORE-NUMBER         PIC X(6)       VALUE "STORE".
          05 FILLER                   PIC X(2)       VALUE SPACES.
          05 WSH-INVOICE-NUMBER       PIC X(9)       VALUE "INVOICE".
          05 FILLER                   PIC X(8)       VALUE SPACES.
          05 WSH-SKU                  PIC X(6)       VALUE "SKU".
          05 FILLER                   PIC X(12)      VALUE SPACES.
          05 WSH-TAXES                PIC X(11)      VALUE "TAXES".

       01 WS-PAGE-HEADER2.
          05 FILLER                   PIC X(4)       VALUE SPACES.
          05 WSH-TRANSACTION-CODE2    PIC X(6)       VALUE "CODE  ".
          05 FILLER                   PIC X(5)       VALUE SPACES.
          05 WSH-TRANSACTION-AMOUNT2  PIC X(5)       VALUE "AMT  ".
          05 FILLER                   PIC X(3)       VALUE SPACES.
          05 WSH-PAYMENT-TYPE2        PIC X(4)       VALUE "TYPE".
          05 FILLER                   PIC X(46)       VALUE SPACES.
          05 WSH-TAXES2               PIC X(5)      VALUE "OWING".

       01 WS-FOOTER.

       01 WS-REPORT-FOOTER.


       01 WS-DETAIL.
          05 FILLER                  PIC X(6)    VALUE SPACES.
          05 WSD-TRANSACTION-CODE    PIC X(1)    VALUE SPACES.
          05 FILLER                  PIC X(4)    VALUE SPACES.
          05 WSD-TRANSACTION-AMOUNT  PIC $Z(5).99.
          05 FILLER                  PIC X(3)    VALUE SPACES.
          05 WSD-PAYMENT-TYPE        PIC X(4)    VALUE SPACES.
          05 FILLER                  PIC X(3)    VALUE SPACES.
          05 WSD-STORE-NUMBER        PIC Z(3)    VALUE ZEROS.
          05 FILLER                  PIC X(4)    VALUE SPACES.
          05 WSD-INVOICE-NUMBER      PIC X(9)    VALUE SPACES.
          05 FILLER                  PIC X(5)    VALUE SPACES.
          05 WSD-SKU                 PIC X(15)   VALUE SPACES.
          05 FILLER                  PIC X(5)    VALUE SPACES.
          05 WSD-TAXES-OWING         PIC $Z(3).99.

       01 WS-MATH.
          05 WSM-PRODUCT-AMT          PIC 9(7)V99.
          05 WSM-TAX-RATE             PIC V9(4)      VALUE 0.13.
          05 WSM-TAX-OWING            PIC 9(6)V99.

       01 WS-COUNTERS.
           05 ROW-COUNTER            PIC 9(2)        VALUE ZERO.

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
           PERFORM 800-DISPLAY-PAGE-FOOTER.
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
           ADD 1 TO ROW-COUNTER.
           IF ROW-COUNTER > ROW-MAXIMUM
               PERFORM 800-DISPLAY-PAGE-FOOTER
               MOVE 0 TO ROW-COUNTER
               PERFORM 750-DISPLAY-PAGE-HEADER
           END-IF.

           MOVE IL-TRANSACTION-CODE TO WSD-TRANSACTION-CODE.
           MOVE IL-TRANSACTION-AMOUNT TO WSD-TRANSACTION-AMOUNT.
           MOVE IL-PAYMENT-TYPE TO WSD-PAYMENT-TYPE.
           MOVE IL-STORE-NUMBER TO WSD-STORE-NUMBER.
           MOVE IL-INVOICE-NUMBER TO WSD-INVOICE-NUMBER.
           MOVE IL-SKU TO WSD-SKU.

           PERFORM 250-TAX-CALCULATIONS.

           MOVE WS-DETAIL TO OUTPUT-LINE.
           WRITE OUTPUT-LINE AFTER ADVANCING 1 LINE.

       250-TAX-CALCULATIONS.
           MOVE IL-TRANSACTION-AMOUNT TO WSM-PRODUCT-AMT.
           COMPUTE WSM-TAX-OWING = WSM-PRODUCT-AMT * WSM-TAX-RATE.
           MOVE WSM-TAX-OWING TO WSD-TAXES-OWING.

       700-DISPLAY-REPORT-HEADER.

       750-DISPLAY-PAGE-HEADER.

           MOVE SPACES TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

           MOVE WS-PAGE-HEADER1 TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

           MOVE WS-PAGE-HEADER2 TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

       800-DISPLAY-PAGE-FOOTER.

       850-DISPLAY-REPORT-FOOTER.

       900-CLOSE-FILES.
           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.

        END PROGRAM A8SL.
