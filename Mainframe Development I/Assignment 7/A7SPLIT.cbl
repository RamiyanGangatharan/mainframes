        IDENTIFICATION DIVISION.
        PROGRAM-ID. A7SPLIT.
        DATE-WRITTEN. MARCH 20, 2023.
        AUTHOR. RAMIYAN GANGATHARAN.
      * DESCRIPTION:

        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.

      * TO DO LIST:
      *
      *'L' = LAYAWAY
      *'S' = SALE
      *'R' = RETURN
      *
      *    INITIALIZE INPUTS AND OUTPUTS [DONE]
      *    SPLIT UP DATA INTO CHUNKS OF INFORMATION [DONE]
      *LOOP THRU THE RECORDS IN THE FILE [DONE]
      *DURING LOOP: [DONE]
      *    WRITE TO S&L RECORDS FILE IF TRANS-CODE = 'S' OR 'L' [DONE]
      *    WRITE TO RETURNS FILE IF TRANS-CODE = 'R' [DONE]
      *AFTER LOOP:
      *    ANALYZE DATA ['S' OR 'L']:
      *         TRANS-CODE = 'S' OR 'L'
      *             TOTAL NUMBER OF 'S' AND 'L' RECORDS [DONE]
      *             TOTAL AMOUNT OF 'S' AND 'L' RECORDS
      *
      *             TOTAL NUMBER OF 'S' RECORDS [DONE]
      *             TOTAL AMOUNT OF 'S' RECORDS
      *
      *             TOTAL NUMBER OF 'L' RECORDS [DONE]
      *             TOTAL AMOUNT OF 'L' RECORDS
      *
      *             TOTAL RECORD COUNT [DONE]
      *
      *             TOTAL AMOUNT FOR EACH STORE
      *
      *             PERCENTAGE OF TRANSACTIONS IN EACH TYPE OF PAYMENT
      *
      *             NUMBER OF TRANSACTIONS IN EACH TYPE OF PAYMENT [DONE]
      *                 NOTE: ONLY FOR 'S' AND 'L', NOT 'R'
      *
      *   ANALYZE DATA ['R']:
      *         TOTAL NUMBER OF 'R' RECORDS [DONE]
      *         TOTAL AMOUNT OF 'R' RECORDS FOR EACH STORE
      *         TOTAL NUMBER OF 'R' RECORDS
      *         TOTAL AMOUNT OF 'R' RECORDS
      *
      *   ANALYZE DATA ['S' OR 'L' OR 'R']:
      *         GRAND TOTAL = (('S' TOTAL + 'L' TOTAL) - 'R' TOTAL)


        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT INPUT-FILE
                ASSIGN TO "INFILE"
                ORGANIZATION IS SEQUENTIAL.

            SELECT OUTPUT-FILE
                ASSIGN TO "OUTFILE"
                ORGANIZATION IS SEQUENTIAL.

           SELECT SALE_LAY_OUTFILE
                ASSIGN TO "SOUTFILE"
                ORGANIZATION IS SEQUENTIAL.

           SELECT RETURNED_OUTFILE
                ASSIGN TO "ROUTFILE"
                ORGANIZATION IS SEQUENTIAL.

        DATA DIVISION.
        FILE SECTION.

        FD INPUT-FILE
            RECORDING MODE IS F
            RECORD CONTAINS 108 CHARACTERS.

      * THIS SPLITS UP THE DATA INTO USABLE CHUNKS FROM THE RAW DATA.
       01 INPUT-LINE.
          05 IL-TRANSACTION-CODE       PIC X(1).
          05 IL-TRANSACTION-AMOUNT     PIC 9(5)V99.
          05 IL-PAYMENT-TYPE           PIC X(2).
          05 IL-STORE-NUMBER           PIC X(2).
          05 IL-INVOICE-NUMBER         PIC X(9).
          05 SPLIT-INVOICE REDEFINES IL-INVOICE-NUMBER.
             10 INVOICE-PREFIX-1       PIC X(1).
             10 INVOICE-PREFIX-2       PIC X(1).
             10 DASH                   PIC X(1).
             10 INVOICE-NUMBER         PIC X(6).
          05 IL-SKU                    PIC X(15).

        FD OUTPUT-FILE
            RECORDING MODE IS F
            RECORD CONTAINS 108 CHARACTERS.
       01 OUTPUT-LINE                  PIC X(108).

        FD SALE_LAY_OUTFILE
           RECORDING MODE IS F
           RECORD CONTAINS 108 CHARACTERS.
       01 SALE_LAY_OUTLINE             PIC X(108).

       FD RETURNED_OUTFILE
           RECORDING MODE IS F
           RECORD CONTAINS 108 CHARACTERS.
       01 RETURNED_OUTLINE             PIC X(108).

        WORKING-STORAGE SECTION.

       01 END-OF-FILE                  PIC X(1).
          88 EOF                                   VALUE 'Y'.
          88 NOT-EOF                               VALUE 'N'.

       01 WS-TRANS-CODE-SALES-OPTIONS  PIC X(1).
          88 TCSO-SALES                            VALUE 'S'.
          88 TCSO-LAYAWAYS                         VALUE 'L'.
          88 TCSO-RETURNED                         VALUE 'R'.

       01 WS-PAYMENT-TYPE-OPTIONS      PIC X(2).
          88 PTO-DEBIT                             VALUE 'DB'.
          88 PTO-CREDIT                            VALUE 'CR'.
          88 PTO-CASH                              VALUE 'CA'.

       01 WS-SALES-COUNT               PIC 9(3).
       01 WS-LAYAWAY-COUNT             PIC 9(3).
       01 WS-RETURN-COUNT              PIC 9(3).
       01 WS-SL-TOTAL-COUNT            PIC 9(3).
       01 WS-TOTAL-ROW-COUNTER         PIC 9(3).

       01 WS-TOTAL-DEBIT-COUNTER       PIC 9(3).
       01 WS-TOTAL-CREDIT-COUNTER      PIC 9(3).
       01 WS-TOTAL-CASH-COUNTER        PIC 9(3).

       01 WS-SALES-COUNT-STR           PIC Z(3).
       01 WS-LAYAWAY-COUNT-STR         PIC Z(3).
       01 WS-RETURN-COUNT-STR          PIC Z(3).
       01 WS-SL-TOTAL-COUNT-STR        PIC Z(3).
       01 WS-TOTAL-ROW-COUNTER-STR     PIC Z(3).

       01 WS-TOTAL-DEBIT-COUNTER-STR   PIC Z(3).
       01 WS-TOTAL-CREDIT-COUNTER-STR  PIC Z(3).
       01 WS-TOTAL-CASH-COUNTER-STR    PIC Z(3).

        PROCEDURE DIVISION.
       000-MAIN.
           PERFORM 100-OPEN-FILES.
           PERFORM UNTIL EOF
               PERFORM 150-READ-FILES
               PERFORM 200-PROCESS-RECORDS
               ADD 1 TO WS-TOTAL-ROW-COUNTER
           END-PERFORM.
           PERFORM 890-SALES-SUMMARY.
           PERFORM 900-CLOSE-FILES.
           GOBACK.

       100-OPEN-FILES.
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.
           OPEN OUTPUT SALE_LAY_OUTFILE.
           OPEN OUTPUT RETURNED_OUTFILE.

       150-READ-FILES.
           READ INPUT-FILE
           AT END
              SET EOF TO TRUE
           END-READ.

       200-PROCESS-RECORDS.
           MOVE IL-TRANSACTION-CODE TO WS-TRANS-CODE-SALES-OPTIONS.
           MOVE IL-PAYMENT-TYPE     TO WS-PAYMENT-TYPE-OPTIONS.
           PERFORM 210-SPLITTER.
           PERFORM 220-SPLIT-BY-PAYMENT.

       210-SPLITTER.
           IF TCSO-SALES THEN
              PERFORM 300-SPLIT-SALES
              ADD 1 TO WS-SALES-COUNT
           END-IF.

           IF TCSO-LAYAWAYS THEN
              PERFORM 300-SPLIT-SALES
              ADD 1 TO WS-LAYAWAY-COUNT
           END-IF.

           IF TCSO-RETURNED THEN
              PERFORM 350-SPLIT-RETURNS
              ADD 1 TO WS-RETURN-COUNT
           END-IF.

       220-SPLIT-BY-PAYMENT.
           IF PTO-DEBIT THEN
              ADD 1 TO WS-TOTAL-DEBIT-COUNTER
           END-IF.
           IF PTO-CREDIT THEN
              ADD 1 TO WS-TOTAL-CREDIT-COUNTER
           END-IF.
           IF PTO-CASH THEN
              ADD 1 TO WS-TOTAL-CASH-COUNTER
           END-IF.

       300-SPLIT-SALES.
           MOVE INPUT-LINE          TO SALE_LAY_OUTLINE.
           WRITE SALE_LAY_OUTLINE.

       350-SPLIT-RETURNS.
           MOVE INPUT-LINE          TO RETURNED_OUTLINE.
           WRITE RETURNED_OUTLINE.


       890-SALES-SUMMARY.
           MOVE WS-SALES-COUNT      TO WS-SALES-COUNT-STR.
           MOVE WS-LAYAWAY-COUNT    TO WS-LAYAWAY-COUNT-STR.
           MOVE WS-RETURN-COUNT     TO WS-RETURN-COUNT-STR.

      *    SALES
           MOVE "             SALES COUNT: " TO OUTPUT-LINE(1:25).
           MOVE WS-SALES-COUNT-STR           TO OUTPUT-LINE(27:5).
           WRITE OUTPUT-LINE.

      *    LAYAWAYS
           MOVE "           LAYAWAY COUNT: " TO OUTPUT-LINE(1:25).
           MOVE WS-LAYAWAY-COUNT-STR         TO OUTPUT-LINE(27:5).
           WRITE OUTPUT-LINE.

      *    SALES AND LAYAWAYS
           ADD WS-SALES-COUNT
              TO WS-LAYAWAY-COUNT
              GIVING WS-SL-TOTAL-COUNT.
           MOVE WS-SL-TOTAL-COUNT            TO WS-SL-TOTAL-COUNT-STR.

           MOVE "TOTAL SALES AND LAYAWAYS: " TO OUTPUT-LINE(1:25).
           MOVE WS-SL-TOTAL-COUNT-STR TO OUTPUT-LINE(27:5).
           WRITE OUTPUT-LINE.

      *    RETURNS
           MOVE "            RETURN COUNT: " TO OUTPUT-LINE(1:25).
           MOVE WS-RETURN-COUNT-STR TO OUTPUT-LINE(27:5).
           WRITE OUTPUT-LINE.

      *    TOTAL ROWS
           MOVE WS-TOTAL-ROW-COUNTER        TO WS-TOTAL-ROW-COUNTER-STR.
           MOVE "      TOTAL ROWS COUNTED: " TO OUTPUT-LINE(1:25).
           MOVE WS-TOTAL-ROW-COUNTER-STR    TO OUTPUT-LINE(27:5).
           WRITE OUTPUT-LINE.

      *    CASH CREDIT DEBIT COUNTERS
           MOVE SPACES TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

           MOVE WS-TOTAL-DEBIT-COUNTER TO WS-TOTAL-DEBIT-COUNTER-STR.
           MOVE "   TOTAL DEBIT PURCHASES: " TO OUTPUT-LINE(1:25).
           MOVE WS-TOTAL-DEBIT-COUNTER-STR   TO OUTPUT-LINE(27:5).
           WRITE OUTPUT-LINE.

           MOVE WS-TOTAL-CREDIT-COUNTER TO WS-TOTAL-CREDIT-COUNTER-STR.
           MOVE "  TOTAL CREDIT PURCHASES: " TO OUTPUT-LINE(1:25).
           MOVE WS-TOTAL-CREDIT-COUNTER-STR  TO OUTPUT-LINE(27:5).
           WRITE OUTPUT-LINE.

           MOVE WS-TOTAL-CASH-COUNTER TO WS-TOTAL-CASH-COUNTER-STR.
           MOVE "    TOTAL CASH PURCHASES: " TO OUTPUT-LINE(1:25).
           MOVE WS-TOTAL-CASH-COUNTER-STR    TO OUTPUT-LINE(27:5).
           WRITE OUTPUT-LINE.

       900-CLOSE-FILES.
           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.
           CLOSE SALE_LAY_OUTFILE.
           CLOSE RETURNED_OUTFILE.

        END PROGRAM A7SPLIT.
