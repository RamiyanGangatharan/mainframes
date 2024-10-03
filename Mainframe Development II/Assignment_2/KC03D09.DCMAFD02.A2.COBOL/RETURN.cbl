       IDENTIFICATION DIVISION.
       PROGRAM-ID. RET.
       DATE-WRITTEN. 01/04/2024.
       AUTHOR. Christina Jackson.
      *Description: Analyzes returns data for each store and displays
      * tax owed and totals. Used by Ramiyan Gangatharan.

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

      * Input line variables
       01 INPUT-LINE.
          05 IL-TCODE            PIC X.
          05 IL-TAMT             PIC 9(5)V99.
          05 IL-PTYPE            PIC XX.
          05 IL-STNUM            PIC 99.
          05 IL-INVNUM           PIC X(9).
          05 IL-SKU              PIC X(15).
      *
       FD OUTPUT-FILE
           RECORDING MODE IS F
           DATA RECORD IS OUTPUT-LINE
           RECORD CONTAINS 83 CHARACTERS.
      *
       01 OUTPUT-LINE            PIC X(83).
      *
       WORKING-STORAGE SECTION.

      * Title with name and assignment
       01 WS-HEADING-NAME.
          05 FILLER              PIC X(52)
                                               VALUE SPACES.
          05 FILLER              PIC X(31)
                                               VALUE
                                      "Assignment 9: Christina Jackson".

      * Title of assignment
       01 WS-HEADING-TITLE.
          05 FILLER              PIC X(30)
                                               VALUE SPACES.
          05 FILLER              PIC X(14)
                                               VALUE "Returns Report".
          05 FILLER              PIC X(38)
                                               VALUE
                               "                                 Page ".
          05 WS-PAGE-NO          PIC 9.

      * First line of columns
       01 WS-HEADING-COLUMN1.
          05 FILLER              PIC X(30)
                                               VALUE
                                       "Store   Trans     Trans       ".
          05 FILLER              PIC X(26)
                                               VALUE
                                           "Payment     Invoice       ".

      * Second line of columns
       01 WS-HEADING-COLUMN2.
          05 FILLER              PIC X(31)
                                               VALUE
                                      "  #      Code     Amount       ".
          05 FILLER              PIC X(28)
                                               VALUE
                                         "Type        Number          ".
          05 FILLER              PIC X(18)
                                               VALUE
                                                   "SKU            Tax".

      * Third line of columns
       01 WS-HEADING-LINES.
          05 FILLER              PIC X(29)
                                               VALUE
                                        "-----   -----    --------    ".
          05 FILLER              PIC X(27)
                                               VALUE
                                          "---------   ---------      ".
          05 FILLER              PIC X(23)
                                               VALUE
                                              "---------       -------".

      * Blank line for spacing
       01 WS-BLANK-LINE.
          05 FILLER              PIC X(77)
                                               VALUE SPACES.

      * End of file flag
       01 WS-EOF                 PIC X
                                               VALUE "N".
          88 WS-END-OF-FILE                    VALUE "Y".

      * Constants
       77 WS-LINES-PER-PAGE      PIC 99
                                               VALUE 20.
       77 WS-PAGE-COUNT          PIC 99
                                               VALUE 0.
       77 WS-LINE-COUNT          PIC 99
                                               VALUE 0.
       77 WS-NUM-STORES          PIC 9
                                               VALUE 6.
       77 WS-TAX                 PIC 9V99
                                               VALUE 0.13.

      * Calculation variables
       01 WS-CALCULATIONS.
          05 WS-INDEX            PIC 9
                                               VALUE 0.
          05 CALC-EACH-STORE     PIC 99999V99 OCCURS 6 TIMES.
          05 CALC-TOTAL-RETURNS  PIC 99999V99.
          05 CALC-TOT-TAX        PIC 9999V99.
          05 CALC-TOTAL-TAX      PIC 99999V99.
          05 CALC-AMOUNT         PIC 99999V99.

      * Counters
       01 WS-COUNTERS.
          05 WS-RET-COUNT        PIC 99 OCCURS 6 TIMES.
          05 WS-TOT-COUNT        PIC 99.


      * Output line
       01 WS-DETAIL-LINE.
          05 FILLER              PIC X
                                               VALUE SPACES.
          05 WS-DL-SN            PIC XX.
          05 FILLER              PIC X(7)
                                               VALUE SPACES.
          05 WS-DL-TC            PIC X.
          05 FILLER              PIC X(5)
                                               VALUE SPACES.
          05 WS-DL-TA            PIC $$$$$9.99.
          05 FILLER              PIC X(7)
                                               VALUE SPACES.
          05 WS-DL-PT            PIC XX.
          05 FILLER              PIC X(7)
                                               VALUE SPACES.
          05 WS-DL-IN            PIC X(9).
          05 FILLER              PIC X(3)
                                               VALUE SPACES.
          05 WS-DL-SKU           PIC X(15).
          05 FILLER              PIC XX
                                               VALUE SPACES.
          05 WS-DL-TAX           PIC $$$$$9.99.

      * Footers
       01 WS-FOOTER-1.
          05 FILLER              PIC X(26)
                                               VALUE SPACES.
          05 FILLER              PIC X(23)
                                               VALUE
                                              "Total Returns per Store".

       01 WS-FOOTER-2.
          05 FILLER              PIC X(25)
                                               VALUE SPACES.
          05 FILLER              PIC X(25)
                                               VALUE
                                            "-------------------------".

      * Store 1 calculations
       01 WS-FOOTER-3.
          05 FILLER              PIC X(25)
                                               VALUE SPACES.
          05 FILLER              PIC X(11)
                                               VALUE "Store 1    ".
          05 WS-RET-ST1          PIC Z9.
          05 FILLER              PIC X(4)
                                               VALUE " at ".
          05 WS-RET-ST-AMT1      PIC $$$9.99.

      * Store 2 calculations
       01 WS-FOOTER-4.
          05 FILLER              PIC X(25)
                                               VALUE SPACES.
          05 FILLER              PIC X(11)
                                               VALUE "Store 2    ".
          05 WS-RET-ST2          PIC Z9.
          05 FILLER              PIC X(4)
                                               VALUE " at ".
          05 WS-RET-ST-AMT2      PIC $$$9.99.

      * Store 3 calculations
       01 WS-FOOTER-5.
          05 FILLER              PIC X(25)
                                               VALUE SPACES.
          05 FILLER              PIC X(11)
                                               VALUE "Store 3    ".
          05 WS-RET-ST3          PIC Z9.
          05 FILLER              PIC X(4)
                                               VALUE " at ".
          05 WS-RET-ST-AMT3      PIC $$$9.99.

      * Store 4 calculations
       01 WS-FOOTER-6.
          05 FILLER              PIC X(25)
                                               VALUE SPACES.
          05 FILLER              PIC X(11)
                                               VALUE "Store 4    ".
          05 WS-RET-ST4          PIC Z9.
          05 FILLER              PIC X(4)
                                               VALUE " at ".
          05 WS-RET-ST-AMT4      PIC $$$9.99.

      * Store 5 calculations
       01 WS-FOOTER-7.
          05 FILLER              PIC X(25)
                                               VALUE SPACES.
          05 FILLER              PIC X(11)
                                               VALUE "Store 5    ".
          05 WS-RET-ST5          PIC Z9.
          05 FILLER              PIC X(4)
                                               VALUE " at ".
          05 WS-RET-ST-AMT5      PIC $$$9.99.

      * Store 12 calculations
       01 WS-FOOTER-8.
          05 FILLER              PIC X(25)
                                               VALUE SPACES.
          05 FILLER              PIC X(11)
                                               VALUE "Store 12   ".
          05 WS-RET-ST12         PIC Z9.
          05 FILLER              PIC X(4)
                                               VALUE " at ".
          05 WS-RET-ST-AMT12     PIC $$$9.99.

      * Total returns line
       01 WS-FOOTER-9.
          05 FILLER              PIC X(25)
                                               VALUE
                                            "Total Returns:           ".
          05 WS-TOT-RET          PIC 99.
          05 FILLER              PIC X(4)
                                               VALUE " at ".
          05 WS-TOT-RET-AMT      PIC $$$9.99.

      * Total tax line
       01 WS-FOOTER-10.
          05 FILLER              PIC X(25)
                                               VALUE
                                            "Total Tax Owed:          ".
          05 WS-TOT-TAX          PIC $$$.99.



       PROCEDURE DIVISION.
       000-MAIN.
      *

      * Open files
           PERFORM 100-OPEN-FILES.

      * Read files and perform calculations
           PERFORM 200-PRINT-INFO
              UNTIL WS-END-OF-FILE.

      * Perform final calculations and write footers
           PERFORM 400-FOOTER-CALCS.

      * Close files
           CLOSE INPUT-FILE
                 OUTPUT-FILE.


           GOBACK.
      *

      * Opens files
       100-OPEN-FILES.
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.

           READ INPUT-FILE
           AT END
              MOVE "Y" TO WS-EOF.

      * Prints headings and process stores
       200-PRINT-INFO.
           PERFORM 210-PRINT-HEADINGS.

           PERFORM 300-PROCESS-STORES
              VARYING WS-LINE-COUNT FROM 1 BY 1
              UNTIL(WS-LINE-COUNT > WS-LINES-PER-PAGE
              OR WS-END-OF-FILE).

      * Prints headings depending on what page it is currently on
      * There's only one page so it will only need one set of headings
       210-PRINT-HEADINGS.
           ADD 1 TO WS-PAGE-COUNT.
           MOVE SPACES TO OUTPUT-LINE.
           MOVE WS-PAGE-COUNT TO WS-PAGE-NO.

           IF (WS-PAGE-COUNT > 1) THEN
              WRITE OUTPUT-LINE
                 AFTER ADVANCING PAGE

              WRITE OUTPUT-LINE
              WRITE OUTPUT-LINE FROM WS-HEADING-TITLE
              WRITE OUTPUT-LINE FROM WS-BLANK-LINE
              WRITE OUTPUT-LINE FROM WS-HEADING-COLUMN1
              WRITE OUTPUT-LINE FROM WS-HEADING-COLUMN2
              WRITE OUTPUT-LINE FROM WS-HEADING-LINES
              WRITE OUTPUT-LINE FROM WS-BLANK-LINE
              MOVE SPACES TO OUTPUT-LINE

           ELSE
              WRITE OUTPUT-LINE
              WRITE OUTPUT-LINE FROM WS-HEADING-NAME
              WRITE OUTPUT-LINE FROM WS-BLANK-LINE
              WRITE OUTPUT-LINE FROM WS-HEADING-TITLE
              WRITE OUTPUT-LINE FROM WS-BLANK-LINE
              WRITE OUTPUT-LINE FROM WS-HEADING-COLUMN1
              WRITE OUTPUT-LINE FROM WS-HEADING-COLUMN2
              WRITE OUTPUT-LINE FROM WS-HEADING-LINES
              WRITE OUTPUT-LINE FROM WS-BLANK-LINE

           END-IF.


      * Store information processing
       300-PROCESS-STORES.

      * Sort Return amounts per store
           PERFORM
              VARYING WS-INDEX
              FROM 1 BY 1

              UNTIL WS-INDEX > WS-NUM-STORES

                   IF (IL-STNUM = 1) AND (WS-INDEX = 1) THEN
                      ADD IL-TAMT TO CALC-EACH-STORE(WS-INDEX)
                      ADD 1 TO WS-RET-COUNT(WS-INDEX)
                      ADD IL-TAMT TO CALC-TOTAL-RETURNS

                   ELSE
                      IF (IL-STNUM = 2) AND (WS-INDEX = 2) THEN
                         ADD IL-TAMT TO CALC-EACH-STORE(WS-INDEX)
                         ADD 1 TO WS-RET-COUNT(WS-INDEX)
                         ADD IL-TAMT TO CALC-TOTAL-RETURNS

                      ELSE
                         IF (IL-STNUM = 3) AND (WS-INDEX = 3) THEN
                            ADD IL-TAMT TO CALC-EACH-STORE(WS-INDEX)
                            ADD 1 TO WS-RET-COUNT(WS-INDEX)
                            ADD IL-TAMT TO CALC-TOTAL-RETURNS

                         ELSE
                            IF (IL-STNUM = 4) AND (WS-INDEX = 4) THEN
                               ADD IL-TAMT TO CALC-EACH-STORE(WS-INDEX)
                               ADD 1 TO WS-RET-COUNT(WS-INDEX)
                               ADD IL-TAMT TO CALC-TOTAL-RETURNS

                            ELSE
                               IF (IL-STNUM = 5) AND (WS-INDEX = 5) THEN
                                  ADD IL-TAMT TO CALC-EACH-STORE
                                     (WS-INDEX)
                                  ADD 1 TO WS-RET-COUNT(WS-INDEX)
                                  ADD IL-TAMT TO CALC-TOTAL-RETURNS

                               ELSE
                                  IF (IL-STNUM = 12) AND (WS-INDEX = 6)
                                     THEN
                                     ADD IL-TAMT TO CALC-EACH-STORE
                                        (WS-INDEX)
                                     ADD 1 TO WS-RET-COUNT(WS-INDEX)
                                     ADD IL-TAMT TO CALC-TOTAL-RETURNS

                                  END-IF
                               END-IF
                            END-IF
                         END-IF
                      END-IF
                   END-IF


           END-PERFORM.

      * Move inline information to detail line variables for display
           MOVE IL-STNUM TO WS-DL-SN.
           MOVE IL-TCODE TO WS-DL-TC.
           MOVE IL-TAMT TO WS-DL-TA.
           MOVE IL-PTYPE TO WS-DL-PT.
           MOVE IL-INVNUM TO WS-DL-IN.
           MOVE IL-SKU TO WS-DL-SKU.


           ADD 1 TO WS-TOT-COUNT.

      * Tax calculation
           MOVE IL-TAMT TO CALC-AMOUNT.
           MULTIPLY CALC-AMOUNT BY WS-TAX GIVING CALC-TOT-TAX ROUNDED.

           ADD CALC-TOT-TAX TO CALC-TOTAL-TAX.
           MOVE CALC-TOT-TAX TO WS-DL-TAX.

           WRITE OUTPUT-LINE FROM WS-DETAIL-LINE.

           READ INPUT-FILE
           AT END
              MOVE "Y" TO WS-EOF.




      * Footer

       400-FOOTER-CALCS.

      * Each store's return counts
           PERFORM
              VARYING WS-INDEX
              FROM 1 BY 1

              UNTIL WS-INDEX > WS-NUM-STORES

                   IF (WS-INDEX = 1) THEN
                      MOVE CALC-EACH-STORE(WS-INDEX) TO
                         WS-RET-ST-AMT1
                      MOVE WS-RET-COUNT(WS-INDEX) TO
                         WS-RET-ST1

                   ELSE
                      IF (WS-INDEX = 2) THEN
                         MOVE CALC-EACH-STORE(WS-INDEX) TO
                            WS-RET-ST-AMT2
                         MOVE WS-RET-COUNT(WS-INDEX) TO
                            WS-RET-ST2

                      ELSE
                         IF (WS-INDEX = 3) THEN
                            MOVE CALC-EACH-STORE(WS-INDEX) TO
                               WS-RET-ST-AMT3
                            MOVE WS-RET-COUNT(WS-INDEX) TO
                               WS-RET-ST3

                         ELSE
                            IF (WS-INDEX = 4) THEN
                               MOVE CALC-EACH-STORE(WS-INDEX) TO
                                  WS-RET-ST-AMT4
                               MOVE WS-RET-COUNT(WS-INDEX) TO
                                  WS-RET-ST4

                            ELSE
                               IF (WS-INDEX = 5) THEN
                                  MOVE CALC-EACH-STORE(WS-INDEX) TO
                                     WS-RET-ST-AMT5
                                  MOVE WS-RET-COUNT(WS-INDEX) TO
                                     WS-RET-ST5

                               ELSE
                                  IF (WS-INDEX = 6) THEN
                                     MOVE CALC-EACH-STORE(WS-INDEX) TO
                                        WS-RET-ST-AMT12
                                     MOVE WS-RET-COUNT(WS-INDEX) TO
                                        WS-RET-ST12

                                  END-IF
                               END-IF
                            END-IF
                         END-IF
                      END-IF
                   END-IF


           END-PERFORM.


      * Totals for returns and tax owed
           MOVE WS-TOT-COUNT TO WS-TOT-RET.
           MOVE CALC-TOTAL-RETURNS TO WS-TOT-RET-AMT.
           MOVE CALC-TOTAL-TAX TO WS-TOT-TAX.



      * Final footer writes
           WRITE OUTPUT-LINE FROM WS-BLANK-LINE.
           WRITE OUTPUT-LINE FROM WS-FOOTER-1.
           WRITE OUTPUT-LINE FROM WS-FOOTER-2.
           WRITE OUTPUT-LINE FROM WS-FOOTER-3.
           WRITE OUTPUT-LINE FROM WS-FOOTER-4.
           WRITE OUTPUT-LINE FROM WS-FOOTER-5.
           WRITE OUTPUT-LINE FROM WS-FOOTER-6.
           WRITE OUTPUT-LINE FROM WS-FOOTER-7.
           WRITE OUTPUT-LINE FROM WS-FOOTER-8.
           WRITE OUTPUT-LINE FROM WS-BLANK-LINE.
           WRITE OUTPUT-LINE FROM WS-FOOTER-9.
           WRITE OUTPUT-LINE FROM WS-FOOTER-10.




       END PROGRAM RET.
