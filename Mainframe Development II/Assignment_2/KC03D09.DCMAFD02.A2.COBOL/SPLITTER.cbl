       IDENTIFICATION DIVISION.
       PROGRAM-ID. SPLITTER.
       DATE-WRITTEN.
       AUTHOR. John France.
      *Description: this is being modified by Ramiyan Gangatharan
      *             for assignment II in mainframes II
      *
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
               ASSIGN TO RPT
               ORGANIZATION IS SEQUENTIAL.
           SELECT OUTPUT-SL
                ASSIGN TO SL
                ORGANIZATION IS SEQUENTIAL.
           SELECT OUTPUT-R
                ASSIGN TO RET
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
          05 IN-TRANS-CODE       PIC X.
             88 TRANS-CODE-RET                VALUE 'R'.
             88 TRANS-CODE-SALE               VALUE 'S'.
             88 TRANS-CODE-LAY                VALUE 'L'.
          05 IN-TRANS-AMOUNT     PIC 9(5)V99.
          05 IN-PAY-TYPE         PIC XX.
          05 IN-STORE-NUM        PIC XX.
             88 STORE-01                      VALUE '01'.
             88 STORE-02                      VALUE '02'.
             88 STORE-03                      VALUE '03'.
             88 STORE-04                      VALUE '04'.
             88 STORE-05                      VALUE '05'.
             88 STORE-12                      VALUE '12'.
          05 IN-INVOICE-NUM      PIC X(9).
          05 IN-SKU              PIC X(15).
      *
       FD OUTPUT-FILE
           RECORDING MODE IS F
           DATA RECORD IS OUTPUT-LINE
           RECORD CONTAINS 101 CHARACTERS.
      *
       01 OUTPUT-LINE            PIC X(101).
      *
       FD OUTPUT-SL
           RECORDING MODE IS F
           DATA RECORD IS OUTPUT-SL-LINE
           RECORD CONTAINS 36 CHARACTERS.
       01 OUTPUT-SL-LINE         PIC X(36).

       FD OUTPUT-R
           RECORDING MODE IS F
           DATA RECORD IS OUTPUT-R-LINE
           RECORD CONTAINS 36 CHARACTERS.
       01 OUTPUT-R-LINE          PIC X(36).

       WORKING-STORAGE SECTION.
      *
      * Collects running counts.
       01 WS-ACCUMULATORS.
          05 WS-STORE-S-CNT      PIC 999 OCCURS 6 TIMES.
          05 WS-STORE-S-AMT      PIC 9(9)V99 OCCURS 6 TIMES.
          05 WS-STORE-L-CNT      PIC 999 OCCURS 6 TIMES.
          05 WS-STORE-L-AMT      PIC 9(9)V99 OCCURS 6 TIMES.
          05 WS-STORE-SL-CNT     PIC 999 OCCURS 6 TIMES.
          05 WS-STORE-SL-AMT     PIC 9(9)V99 OCCURS 6 TIMES.
          05 WS-STORE-R-CNT      PIC 999 OCCURS 6 TIMES.
          05 WS-STORE-R-AMT      PIC 9(9)V99 OCCURS 6 TIMES.

       01 WS-TOTALS.
          05 WS-S-CNT-TOT-C      PIC 999.
          05 WS-S-CNT-PER-C      PIC 999V9.
          05 WS-S-AMT-TOT-C      PIC 9(9)V99.
          05 WS-S-AMT-PER-C      PIC 999V9.
          05 WS-L-CNT-TOT-C      PIC 999.
          05 WS-L-CNT-PER-C      PIC 999V9.
          05 WS-L-AMT-TOT-C      PIC 9(9)V99.
          05 WS-L-AMT-PER-C      PIC 999V9.
          05 WS-SL-CNT-TOT-C     PIC 999.
          05 WS-SL-AMT-TOT-C     PIC 9(9)V99.
          05 WS-R-CNT-TOT-C      PIC 999.
          05 WS-R-AMT-TOT-C      PIC 9(9)V99.
          05 WS-TOT-REC-CNT      PIC 999.
          05 WS-GRAND-TOT-C      PIC 9(9)V99.


      * For the sake of clearer code, these constants will be used for
      * indexing into the accumulator tables.
       77 WS-STORE-01            PIC 9        VALUE 1.
       77 WS-STORE-02            PIC 9        VALUE 2.
       77 WS-STORE-03            PIC 9        VALUE 3.
       77 WS-STORE-04            PIC 9        VALUE 4.
       77 WS-STORE-05            PIC 9        VALUE 5.
       77 WS-STORE-12            PIC 9        VALUE 6.

       01 WS-REPORT-LINES.
          05 WS-REPORT-HEADER.
             10 FILLER           PIC X(30)    VALUE SPACES.
             10 FILLER           PIC X(32)
                                              VALUE
                                     "COUNTS AND CONTROL TOTALS REPORT".
          05 WS-REPORT-COLUMNS.
             10 FILLER           PIC X(7)     VALUE "Store #".
             10 FILLER           PIC X(15)    VALUE SPACES.
             10 FILLER           PIC X(2)     VALUE "01".
             10 FILLER           PIC X(9)     VALUE SPACES.
             10 FILLER           PIC X(2)     VALUE "02".
             10 FILLER           PIC X(9)     VALUE SPACES.
             10 FILLER           PIC X(2)     VALUE "03".
             10 FILLER           PIC X(9)     VALUE SPACES.
             10 FILLER           PIC X(2)     VALUE "04".
             10 FILLER           PIC X(9)     VALUE SPACES.
             10 FILLER           PIC X(2)     VALUE "05".
             10 FILLER           PIC X(9)     VALUE SPACES.
             10 FILLER           PIC X(2)     VALUE "12".
             10 FILLER           PIC X(9)     VALUE SPACES.
             10 FILLER           PIC X(5)     VALUE "Total".
             10 FILLER           PIC X(4)     VALUE SPACES.
             10 FILLER           PIC X        VALUE "%".
          05 WS-DASH-DIV         PIC X(100)   VALUE ALL "-".
          05 WS-RPT-S-CNT.
             10 FILLER           PIC X(14)
                                              VALUE "Sale Count  | ".
             10 WS-S-C-TBL OCCURS 6 TIMES.
                15 FILLER        PIC X(8)     VALUE SPACES.
                15 WS-S-CNT      PIC ZZ9.
             10 FILLER           PIC X(10)    VALUE SPACES.
             10 WS-S-CNT-TOT     PIC ZZ9.
             10 WS-S-CNT-PER     PIC ZZ9.9.
          05 WS-RPT-S-AMT.
             10 FILLER           PIC X(14)
                                              VALUE "Sale Amount | ".
             10 WS-S-A-TBL OCCURS 6 TIMES.
                15 FILLER        PIC X        VALUE SPACES.
                15 WS-S-AMT      PIC Z(6)9.99.
             10 FILLER           PIC X        VALUE SPACES.
             10 WS-S-AMT-TOT     PIC Z(8)9.99.
             10 WS-S-AMT-PER     PIC ZZ9.9.
          05 WS-LINE-BREAK.
             10 FILLER           PIC X(12)    VALUE SPACES.
             10 FILLER           PIC X        VALUE "|".
          05 WS-RPT-L-CNT.
             10 FILLER           PIC X(14)
                                              VALUE "Layaway Cnt | ".
             10 WS-L-C-TBL OCCURS 6 TIMES.
                15 FILLER        PIC X(8)     VALUE SPACES.
                15 WS-L-CNT      PIC ZZ9.
             10 FILLER           PIC X(10)    VALUE SPACES.
             10 WS-L-CNT-TOT     PIC ZZ9.
             10 WS-L-CNT-PER     PIC ZZ9.9.
          05 WS-RPT-L-AMT.
             10 FILLER           PIC X(14)
                                              VALUE "Layaway Amt | ".
             10 WS-L-A-TBL OCCURS 6 TIMES.
                15 FILLER        PIC X        VALUE SPACES.
                15 WS-L-AMT      PIC Z(6)9.99.
             10 FILLER           PIC X        VALUE SPACES.
             10 WS-L-AMT-TOT     PIC Z(8)9.99.
             10 WS-L-AMT-PER     PIC ZZ9.9.
          05 WS-RPT-T-CNT.
             10 FILLER           PIC X(14)
                                              VALUE "Tot(S+L) Cnt| ".
             10 WS-T-C-TBL OCCURS 6 TIMES.
                15 FILLER        PIC X(8)     VALUE SPACES.
                15 WS-T-CNT      PIC ZZ9.
             10 FILLER           PIC X(10)    VALUE SPACES.
             10 WS-T-CNT-TOT     PIC ZZ9.
          05 WS-RPT-T-AMT.
             10 FILLER           PIC X(14)
                                              VALUE "Tot(S+L) Amt| ".
             10 WS-T-A-TBL OCCURS 6 TIMES.
                15 FILLER        PIC X        VALUE SPACES.
                15 WS-T-AMT      PIC Z(6)9.99.
             10 FILLER           PIC X        VALUE SPACES.
             10 WS-T-AMT-TOT     PIC Z(8)9.99.
          05 WS-RPT-R-CNT.
             10 FILLER           PIC X(14)
                                              VALUE "Return cnt  | ".
             10 WS-R-C-TBL OCCURS 6 TIMES.
                15 FILLER        PIC X(8)     VALUE SPACES.
                15 WS-R-CNT      PIC ZZ9.
             10 FILLER           PIC X(10)    VALUE SPACES.
             10 WS-R-CNT-TOT     PIC ZZ9.
          05 WS-RPT-R-AMT.
             10 FILLER           PIC X(14)
                                              VALUE "Return Amt  | ".
             10 WS-R-A-TBL OCCURS 6 TIMES.
                15 FILLER        PIC X        VALUE SPACES.
                15 WS-R-AMT      PIC Z(6)9.99.
             10 FILLER           PIC X        VALUE SPACES.
             10 WS-R-AMT-TOT     PIC Z(8)9.99.
          05 WS-RPT-GRAND-TOT.
             10 FILLER           PIC X(13)
                                              VALUE "Grand Total |".
             10 FILLER           PIC X(69)    VALUE SPACES.
             10 WS-GRAND-TOTAL   PIC Z(8)9.99.
      *
       01 WS-EOF-FLAG            PIC X.
          88 WS-EOF                           VALUE "Y"
                                                     , "y".
       77 WS-EOF-N               PIC X        VALUE "N".
       77 WS-EOF-Y               PIC X        VALUE "Y".
       01 WS-SUB                 PIC 999      VALUE 1.

       PROCEDURE DIVISION.
       000-MAIN.
      *
           PERFORM 100-OPEN-FILES.
           PERFORM 200-READ-FILE.
           PERFORM 400-PROCESS-RECS
              UNTIL WS-EOF.
           PERFORM 700-WRITE-SUMMARY-REPORT.
           PERFORM 900-CLOSE-FILES.
           GOBACK.
      *
      * Initializes the in and out files for writing.
       100-OPEN-FILES.
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.
           OPEN OUTPUT OUTPUT-SL.
           OPEN OUTPUT OUTPUT-R.

      * "Primes the pump" with the first record line.
       200-READ-FILE.
           READ INPUT-FILE
           AT END
              MOVE WS-EOF-Y TO WS-EOF-FLAG.

      * Calls sub-processes for processing record.
       400-PROCESS-RECS.
           PERFORM 410-SORT-AND-WRITE.
           PERFORM 420-ACCUMULATE.


           READ INPUT-FILE
           AT END
              MOVE WS-EOF-Y TO WS-EOF-FLAG.

      * Write the rec to the correct output file based on trans type.
       410-SORT-AND-WRITE.

           IF TRANS-CODE-SALE OR TRANS-CODE-LAY THEN
              WRITE OUTPUT-SL-LINE FROM INPUT-LINE
           ELSE
              WRITE OUTPUT-R-LINE FROM INPUT-LINE
           END-IF.

      * Sorts the rec by store and trans type, accumulates and tallies.
       420-ACCUMULATE.
           EVALUATE TRUE ALSO TRUE
      * STORE 01
           WHEN STORE-01 ALSO TRANS-CODE-SALE
                ADD IN-TRANS-AMOUNT TO WS-STORE-S-AMT(WS-STORE-01)
                ADD 1 TO WS-STORE-S-CNT(WS-STORE-01)

           WHEN STORE-01 ALSO TRANS-CODE-LAY
                ADD IN-TRANS-AMOUNT TO WS-STORE-L-AMT(WS-STORE-01)
                ADD 1 TO WS-STORE-L-CNT(WS-STORE-01)

           WHEN STORE-01 ALSO TRANS-CODE-RET
                ADD IN-TRANS-AMOUNT TO WS-STORE-R-AMT(WS-STORE-01)
                ADD 1 TO WS-STORE-R-CNT(WS-STORE-01)
      * STORE 02
           WHEN STORE-02 ALSO TRANS-CODE-SALE
                ADD IN-TRANS-AMOUNT TO WS-STORE-S-AMT(WS-STORE-02)
                ADD 1 TO WS-STORE-S-CNT(WS-STORE-02)

           WHEN STORE-02 ALSO TRANS-CODE-LAY
                ADD IN-TRANS-AMOUNT TO WS-STORE-L-AMT(WS-STORE-02)
                ADD 1 TO WS-STORE-L-CNT(WS-STORE-02)

           WHEN STORE-02 ALSO TRANS-CODE-RET
                ADD IN-TRANS-AMOUNT TO WS-STORE-R-AMT(WS-STORE-02)
                ADD 1 TO WS-STORE-R-CNT(WS-STORE-02)
      * STORE 03
           WHEN STORE-03 ALSO TRANS-CODE-SALE
                ADD IN-TRANS-AMOUNT TO WS-STORE-S-AMT(WS-STORE-03)
                ADD 1 TO WS-STORE-S-CNT(WS-STORE-03)

           WHEN STORE-03 ALSO TRANS-CODE-LAY
                ADD IN-TRANS-AMOUNT TO WS-STORE-L-AMT(WS-STORE-03)
                ADD 1 TO WS-STORE-L-CNT(WS-STORE-03)

           WHEN STORE-03 ALSO TRANS-CODE-RET
                ADD IN-TRANS-AMOUNT TO WS-STORE-R-AMT(WS-STORE-03)
                ADD 1 TO WS-STORE-R-CNT(WS-STORE-03)
      * STORE 04
           WHEN STORE-04 ALSO TRANS-CODE-SALE
                ADD IN-TRANS-AMOUNT TO WS-STORE-S-AMT(WS-STORE-04)
                ADD 1 TO WS-STORE-S-CNT(WS-STORE-04)

           WHEN STORE-04 ALSO TRANS-CODE-LAY
                ADD IN-TRANS-AMOUNT TO WS-STORE-L-AMT(WS-STORE-04)
                ADD 1 TO WS-STORE-L-CNT(WS-STORE-04)

           WHEN STORE-04 ALSO TRANS-CODE-RET
                ADD IN-TRANS-AMOUNT TO WS-STORE-R-AMT(WS-STORE-04)
                ADD 1 TO WS-STORE-R-CNT(WS-STORE-04)
      * STORE 05
           WHEN STORE-05 ALSO TRANS-CODE-SALE
                ADD IN-TRANS-AMOUNT TO WS-STORE-S-AMT(WS-STORE-05)
                ADD 1 TO WS-STORE-S-CNT(WS-STORE-05)

           WHEN STORE-05 ALSO TRANS-CODE-LAY
                ADD IN-TRANS-AMOUNT TO WS-STORE-L-AMT(WS-STORE-05)
                ADD 1 TO WS-STORE-L-CNT(WS-STORE-05)

           WHEN STORE-05 ALSO TRANS-CODE-RET
                ADD IN-TRANS-AMOUNT TO WS-STORE-R-AMT(WS-STORE-05)
                ADD 1 TO WS-STORE-R-CNT(WS-STORE-05)
      * STORE 12
           WHEN STORE-12 ALSO TRANS-CODE-SALE
                ADD IN-TRANS-AMOUNT TO WS-STORE-S-AMT(WS-STORE-12)
                ADD 1 TO WS-STORE-S-CNT(WS-STORE-12)

           WHEN STORE-12 ALSO TRANS-CODE-LAY
                ADD IN-TRANS-AMOUNT TO WS-STORE-L-AMT(WS-STORE-12)
                ADD 1 TO WS-STORE-L-CNT(WS-STORE-12)

           WHEN STORE-12 ALSO TRANS-CODE-RET
                ADD IN-TRANS-AMOUNT TO WS-STORE-R-AMT(WS-STORE-12)
                ADD 1 TO WS-STORE-R-CNT(WS-STORE-12)

           END-EVALUATE.


      * Writes the report summarizing the processed data.
       700-WRITE-SUMMARY-REPORT.
           PERFORM 710-TALLY-DATA.
           PERFORM 720-FIND-SUMS.
           PERFORM 730-FIND-PERCENTS.
           PERFORM 740-REPORT-PREPING.
           PERFORM 750-WRITE-REPORT.

      * Sums the store by store gross total sales.
       710-TALLY-DATA.
           PERFORM
              VARYING WS-SUB
              FROM WS-STORE-01
              BY 1
              UNTIL WS-SUB > WS-STORE-12
                   ADD WS-STORE-S-CNT(WS-SUB)
                      TO WS-STORE-SL-CNT(WS-SUB)
                   ADD WS-STORE-S-AMT(WS-SUB)
                      TO WS-STORE-SL-AMT(WS-SUB)
                   ADD WS-STORE-L-CNT(WS-SUB)
                      TO WS-STORE-SL-CNT(WS-SUB)
                   ADD WS-STORE-L-AMT(WS-SUB)
                      TO WS-STORE-SL-AMT(WS-SUB)
           END-PERFORM.

      * Finds the sums for the counts and amounts for sale, layaway,
      * returns, total (gross) sales, and finally the grand (net) total.
       720-FIND-SUMS.
           PERFORM
              VARYING WS-SUB
              FROM WS-STORE-01
              BY 1
              UNTIL WS-SUB > WS-STORE-12
                   ADD WS-STORE-S-CNT(WS-SUB) TO WS-S-CNT-TOT-C
                   ADD WS-STORE-S-AMT(WS-SUB) TO WS-S-AMT-TOT-C
                   ADD WS-STORE-L-CNT(WS-SUB) TO WS-L-CNT-TOT-C
                   ADD WS-STORE-L-AMT(WS-SUB) TO WS-L-AMT-TOT-C
                   ADD WS-STORE-R-CNT(WS-SUB) TO WS-R-CNT-TOT-C
                   ADD WS-STORE-R-AMT(WS-SUB) TO WS-R-AMT-TOT-C
                   ADD WS-STORE-SL-CNT(WS-SUB) TO WS-SL-CNT-TOT-C
                   ADD WS-STORE-SL-AMT(WS-SUB) TO WS-SL-AMT-TOT-C
           END-PERFORM.

           ADD WS-SL-CNT-TOT-C TO WS-R-CNT-TOT-C
              GIVING WS-TOT-REC-CNT.
           SUBTRACT WS-R-AMT-TOT-C FROM WS-SL-AMT-TOT-C
              GIVING WS-GRAND-TOT-C.

      * Find the what percent of sales were regular vs layaway. Both by
      * sale count, and by sale dollar value.
       730-FIND-PERCENTS.
           COMPUTE WS-S-CNT-PER-C ROUNDED
              =(WS-S-CNT-TOT-C / WS-SL-CNT-TOT-C) * 100.
           COMPUTE WS-S-AMT-PER-C ROUNDED
              =(WS-S-AMT-TOT-C / WS-SL-AMT-TOT-C) * 100.
           COMPUTE WS-L-CNT-PER-C ROUNDED
              =(WS-L-CNT-TOT-C / WS-SL-CNT-TOT-C) * 100.
           COMPUTE WS-L-AMT-PER-C ROUNDED
              =(WS-L-AMT-TOT-C / WS-SL-AMT-TOT-C) * 100.

      * Moves all the data into the report lines to prep for writing.
       740-REPORT-PREPING.
           PERFORM
              VARYING WS-SUB
              FROM WS-STORE-01
              BY 1
              UNTIL WS-SUB > WS-STORE-12
                   MOVE WS-STORE-S-CNT(WS-SUB) TO WS-S-CNT(WS-SUB)
                   MOVE WS-STORE-L-CNT(WS-SUB) TO WS-L-CNT(WS-SUB)
                   MOVE WS-STORE-SL-CNT(WS-SUB) TO WS-T-CNT(WS-SUB)
                   MOVE WS-STORE-R-CNT(WS-SUB) TO WS-R-CNT(WS-SUB)
                   MOVE WS-STORE-S-AMT(WS-SUB) TO WS-S-AMT(WS-SUB)
                   MOVE WS-STORE-L-AMT(WS-SUB) TO WS-L-AMT(WS-SUB)
                   MOVE WS-STORE-SL-AMT(WS-SUB) TO WS-T-AMT(WS-SUB)
                   MOVE WS-STORE-R-AMT(WS-SUB) TO WS-R-AMT(WS-SUB)
           END-PERFORM.

           MOVE WS-S-CNT-TOT-C TO WS-S-CNT-TOT.
           MOVE WS-S-CNT-PER-C TO WS-S-CNT-PER.
           MOVE WS-S-AMT-TOT-C TO WS-S-AMT-TOT.
           MOVE WS-S-AMT-PER-C TO WS-S-AMT-PER.
           MOVE WS-L-CNT-TOT-C TO WS-L-CNT-TOT.
           MOVE WS-L-CNT-PER-C TO WS-L-CNT-PER.
           MOVE WS-L-AMT-TOT-C TO WS-L-AMT-TOT.
           MOVE WS-L-AMT-PER-C TO WS-L-AMT-PER.
           MOVE WS-SL-CNT-TOT-C TO WS-T-CNT-TOT.
           MOVE WS-SL-AMT-TOT-C TO WS-T-AMT-TOT.
           MOVE WS-R-CNT-TOT-C TO WS-R-CNT-TOT.
           MOVE WS-R-AMT-TOT-C TO WS-R-AMT-TOT.
           MOVE WS-GRAND-TOT-C TO WS-GRAND-TOTAL.

      * Writes all the lines to the report file.
       750-WRITE-REPORT.
           WRITE OUTPUT-LINE FROM WS-REPORT-HEADER.
           WRITE OUTPUT-LINE FROM WS-REPORT-COLUMNS.
           WRITE OUTPUT-LINE FROM WS-DASH-DIV.
           WRITE OUTPUT-LINE FROM WS-RPT-S-CNT.
           WRITE OUTPUT-LINE FROM WS-RPT-S-AMT.
           WRITE OUTPUT-LINE FROM WS-LINE-BREAK.
           WRITE OUTPUT-LINE FROM WS-RPT-L-CNT.
           WRITE OUTPUT-LINE FROM WS-RPT-L-AMT.
           WRITE OUTPUT-LINE FROM WS-LINE-BREAK.
           WRITE OUTPUT-LINE FROM WS-RPT-T-CNT.
           WRITE OUTPUT-LINE FROM WS-RPT-T-AMT.
           WRITE OUTPUT-LINE FROM WS-LINE-BREAK.
           WRITE OUTPUT-LINE FROM WS-RPT-R-CNT.
           WRITE OUTPUT-LINE FROM WS-RPT-R-AMT.
           WRITE OUTPUT-LINE FROM WS-RPT-GRAND-TOT.

      * Closes all the input and output files.
       900-CLOSE-FILES.
           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.
           CLOSE OUTPUT-SL.
           CLOSE OUTPUT-R.

       END PROGRAM SPLITTER.
