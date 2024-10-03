       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDIT.
       DATE-WRITTEN. 03-06-2024.
       AUTHOR. Meng Cai.
      *Description:input data, output invalid, valid data and report
      *            Currently being used by Ramiyan Gangatharan
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
      *
           SELECT REPORT-FILE
               ASSIGN TO RPTFILE
               ORGANIZATION IS SEQUENTIAL.
      *
           SELECT VALID-FILE
               ASSIGN TO VALFILE
               ORGANIZATION IS SEQUENTIAL.
      *
           SELECT INVALID-FILE
               ASSIGN TO INVFILE
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
          05 IL-TRANS-CODE            PIC X.
          05 IL-TRANS-AMOUNT          PIC 9(5)V99.
          05 IL-PAY-TYPE              PIC XX.
          05 IL-STORE-NUM             PIC 99.
          05 IL-INVOICE-NUM           PIC X(9).
          05 IL-SKU-CODE              PIC X(15).
      *
       FD REPORT-FILE
           RECORDING MODE IS F
           DATA RECORD IS REPORT-LINE
           RECORD CONTAINS 125 CHARACTERS.
      *
       01 REPORT-LINE                 PIC X(125).

       FD VALID-FILE
           RECORDING MODE IS F
           DATA RECORD IS VALID-LINE
           RECORD CONTAINS 36 CHARACTERS.
      *
       01 VALID-LINE                  PIC X(36).
      *
       FD INVALID-FILE
           RECORDING MODE IS F
           DATA RECORD IS INVALID-LINE
           RECORD CONTAINS 36 CHARACTERS.
      *
       01 INVALID-LINE                PIC X(36).
      *
      *

      *
       WORKING-STORAGE SECTION.
      *
       01 WS-NAME-LINE.
          05 FILLER                   PIC X(5)
                                                  VALUE SPACES.
          05 FILLER                   PIC X(25)
                                                  VALUE
                                                     '    Meng Cai    '.
      *               ----+----1----+----2----+
          05 FILLER                   PIC X(29)
                                                  VALUE
                                        '                 Assignment 6'.
      *               ----+----1----+----2----+----
          05 FILLER                   PIC X(5)
                                                  VALUE SPACES.
          05 WS-NAME-LINE-DATE        PIC 9(6).
          05 FILLER                   PIC X(4)
                                                  VALUE SPACES.
          05 WS-NAME-LINE-TIME        PIC 9(8).
          05 FILLER                   PIC X(50)
                                                  VALUE SPACES.
      *
      *
       01 WS-HEADING-1.
          05 FILLER                   PIC X(40)
                                                  VALUE
                             "                           S U M M A R Y".
      *               ----+----1----+----2----+----3----+----4
          05 FILLER                   PIC X(40)
                                                  VALUE
                             "  &  E R R O R   R E P O R T            ".
      *               ----+----5----+----6----+----7----+----8
          05 FILLER                   PIC X(29)
                                                  VALUE
                                        "                        Page ".
      *               ----+----9----+----0----+----
          05 WS-HEADING-1-PAGE-NUM    PIC Z9.
          05 FILLER                   PIC X(9)
                                                  VALUE SPACES.
      *
      *
       01 WS-HEADING-2.
          05 FILLER                   PIC X(40)
                                                  VALUE
                                         "    Raw Input Data          ".
      *               ----+----1----+----2----+----3----+----4
          05 FILLER                   PIC X(40)
                                                  VALUE
                                  "           E r r or   M e s s a g e".
      *               ----+----5----+----6----+----7----+----8

      *
       01 WS-HEADING-3.
          05 FILLER                   PIC X(40)
                                                  VALUE
                                 "------------------------------------".
      *               ----+----1----+----2----+----3----+----4
          05 FILLER                   PIC X(40)
                                                  VALUE
                                   "   -------------------------------".
      *               ----+----5----+----6----+----7----+----8

      *
       01 WS-BLANK-LINE               PIC X(120)
                                                  VALUE SPACES.
       01 WS-SUMMARY.
          05 FILLER                   PIC X(24)
                                                  VALUE
                                             "Number of Records   =   ".
      *               ----+----1----+----2----
          05 WS-SUM-RECORD-COUNT      PIC ZZ9.
          05 FILLER                   PIC X(10)
                                                  VALUE SPACES.
          05 FILLER                   PIC X(20)
                                                  VALUE
                                                 "Valid Records   =   ".
      *               ----+----1----+----2
          05 WS-SUM-VALID-RECORDS     PIC ZZ9.
          05 FILLER                   PIC X(10)
                                                  VALUE SPACES.
          05 FILLER                   PIC X(20)
                                                  VALUE
                                                 "Invalid Records =   ".
      *               ----+----1----+----2
          05 WS-SUM-INVALID-RECORDS   PIC ZZ9.
          05 FILLER                   PIC X(27)
                                                  VALUE SPACES.
      *
       01 WS-ERROR-TEXT-CNST.
          05 WS-ERROR-TEXT-1-CNST     PIC X(40)
                                                  VALUE
                                       "Transaction Code not S, R or L".
          05 WS-ERROR-TEXT-2-CNST     PIC X(40)
                                                  VALUE
                                       "Transaction Amount not numeric".
          05 WS-ERROR-TEXT-3-CNST     PIC X(40)
                                                  VALUE
                                        "Payment Type not CA, CR or DB".
          05 WS-ERROR-TEXT-4-CNST     PIC X(40)
                                                  VALUE
                                "Store Number not 01,02,03,04,05,or 12".
          05 WS-ERROR-TEXT-5-CNST     PIC X(40)
                                                  VALUE
                                     "Invoice Number XX not alphabetic".
          05 WS-ERROR-TEXT-6-CNST     PIC X(40)
                                                  VALUE
                                   "Invoice Number last 6 not numeric ".
          05 WS-ERROR-TEXT-7-CNST     PIC X(40)
                                                  VALUE
                                      "Invoice Num XX not A,B,C,D or E".
          05 WS-ERROR-TEXT-8-CNST     PIC X(40)
                                                  VALUE
                                     "Invoice Number XX same 2 letters".
          05 WS-ERROR-TEXT-9-CNST     PIC X(40)
                                                  VALUE
                             "Invoice Number last 6 >900000 or <100000".
          05 WS-ERROR-TEXT-10-CNST    PIC X(40)
                                                  VALUE
                                                   "SKU Code has space".
          05 WS-ERROR-TEXT-11-CNST    PIC X(40)
                                                  VALUE
                                             "Invoice Number 3 miss - ".
      *
      *
       01 WS-DETAIL-LINE.
          05 WS-DL-INPUT-LINE         PIC X(40)
                                                  VALUE SPACES.
          05 FILLER                   PIC X(3)
                                                  VALUE SPACES.
          05 WS-DL-ERROR-TEXT         PIC X(40)
                                                  VALUE SPACES.
          05 FILLER                   PIC X(11)
                                                  VALUE SPACES.
      *
      *
       77 WS-EOF-FLAG                 PIC X
                                                  VALUE "n".

       01 WS-COUNTERS.
          05 WS-RECORD-COUNT          PIC 999
                                                  VALUE 0.
          05 WS-VALID-RECORD-COUNT    PIC 999
                                                  VALUE 0.
          05 WS-INVALID-RECORD-COUNT  PIC 999
                                                  VALUE 0.

      *
       PROCEDURE DIVISION.
       000-MAIN.
      * open files
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT REPORT-FILE.
           OPEN OUTPUT VALID-FILE,
           OPEN OUTPUT INVALID-FILE.
      *
      * read initial record from input-file
           READ INPUT-FILE
           AT END
              MOVE "y" TO WS-EOF-FLAG.
      *
           PERFORM 30-OUTPUT-REPORT-RECORD.
      * iterate through all input lines
           PERFORM 20-PROCESS-LINES
              UNTIL WS-EOF-FLAG = "y".
      *


           PERFORM 60-OUTPUT-RECORD.
      * close files
           CLOSE INPUT-FILE,
                 REPORT-FILE,
                 VALID-FILE,
                 INVALID-FILE.
      *
           GOBACK.
      *
      *
       20-PROCESS-LINES.
           ADD 1 TO WS-RECORD-COUNT.

      *
      *    add 1 to ws-record-count.
      *    if ws-eof-flag = "y"
      *       perform 30-output-report-record


      * write to file based on TRANS TYPE
           IF IL-TRANS-CODE NOT = "S" AND
              IL-TRANS-CODE NOT = "R" AND
              IL-TRANS-CODE NOT = "L" THEN
              MOVE WS-ERROR-TEXT-1-CNST TO WS-DL-ERROR-TEXT


           ELSE

              IF IL-TRANS-AMOUNT IS NOT NUMERIC THEN
                 MOVE WS-ERROR-TEXT-2-CNST TO WS-DL-ERROR-TEXT

              ELSE

                 IF IL-PAY-TYPE NOT = "CA" AND
                    IL-PAY-TYPE NOT = "CR" AND
                    IL-PAY-TYPE NOT = "DB" THEN
                    MOVE WS-ERROR-TEXT-3-CNST TO WS-DL-ERROR-TEXT

                 ELSE

                    IF IL-STORE-NUM NOT = 01 AND
                       IL-STORE-NUM NOT = 02 AND
                       IL-STORE-NUM NOT = 03 AND
                       IL-STORE-NUM NOT = 04 AND
                       IL-STORE-NUM NOT = 05 AND
                       IL-STORE-NUM NOT = 12 THEN
                       MOVE WS-ERROR-TEXT-4-CNST TO WS-DL-ERROR-TEXT

                    ELSE

                       IF IL-INVOICE-NUM(1:2) IS NOT ALPHABETIC THEN
                          MOVE WS-ERROR-TEXT-5-CNST TO WS-DL-ERROR-TEXT

                       ELSE

                          IF IL-INVOICE-NUM(4:6) IS NOT NUMERIC THEN
                             MOVE WS-ERROR-TEXT-6-CNST TO
                                WS-DL-ERROR-TEXT

                          ELSE

                             IF IL-INVOICE-NUM(1:1) NOT = "A" AND
                                IL-INVOICE-NUM(1:1) NOT = "B" AND
                                IL-INVOICE-NUM(1:1) NOT = "C" AND
                                IL-INVOICE-NUM(1:1) NOT = "D" AND
                                IL-INVOICE-NUM(1:1) NOT = "E" THEN
                                MOVE WS-ERROR-TEXT-7-CNST TO
                                   WS-DL-ERROR-TEXT

                             ELSE

                                IF IL-INVOICE-NUM(2:1) NOT = "A" AND
                                   IL-INVOICE-NUM(2:1) NOT = "B" AND
                                   IL-INVOICE-NUM(2:1) NOT = "C" AND
                                   IL-INVOICE-NUM(2:1) NOT = "D" AND
                                   IL-INVOICE-NUM(2:1) NOT = "E" THEN
                                   MOVE WS-ERROR-TEXT-7-CNST TO
                                      WS-DL-ERROR-TEXT

                                ELSE

                                   IF IL-INVOICE-NUM(1:1) =
                                      IL-INVOICE-NUM(2:1) THEN
                                      MOVE WS-ERROR-TEXT-8-CNST TO
                                         WS-DL-ERROR-TEXT

                                   ELSE

                                      IF IL-INVOICE-NUM(4:6) > 900000 OR
                                         IL-INVOICE-NUM(4:6) < 100000
                                         THEN
                                         MOVE WS-ERROR-TEXT-9-CNST TO
                                            WS-DL-ERROR-TEXT

                                      ELSE

                                         IF IL-INVOICE-NUM(3:1) NOT =
                                            "-" THEN
                                            MOVE WS-ERROR-TEXT-11-CNST
                                               TO WS-DL-ERROR-TEXT

                                         ELSE

                                            IF LENGTH OF IL-SKU-CODE NOT
                                               = 15 THEN
                                               MOVE
                                                 WS-ERROR-TEXT-10-CNST
                                                  TO WS-DL-ERROR-TEXT

                                            END-IF
                                         END-IF
                                      END-IF
                                   END-IF
                                END-IF
                             END-IF
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.


      *

      * write to file based on TRANS TYPE
           IF (IL-TRANS-CODE = "S" OR
              IL-TRANS-CODE = "R" OR
              IL-TRANS-CODE = "L") AND
              IL-TRANS-AMOUNT IS NUMERIC AND
              (IL-PAY-TYPE = "CA" OR
              IL-PAY-TYPE = "CR" OR
              IL-PAY-TYPE = "DB") AND
              (IL-STORE-NUM = 01 OR
              IL-STORE-NUM = 02 OR
              IL-STORE-NUM = 03 OR
              IL-STORE-NUM = 04 OR
              IL-STORE-NUM = 05 OR
              IL-STORE-NUM = 12) AND
              IL-INVOICE-NUM(1:2) IS ALPHABETIC AND
              IL-INVOICE-NUM(4:6) IS NUMERIC AND
              (IL-INVOICE-NUM(1:1) = "A" OR
              IL-INVOICE-NUM(1:1) = "B" OR
              IL-INVOICE-NUM(1:1) = "C" OR
              IL-INVOICE-NUM(1:1) = "D" OR
              IL-INVOICE-NUM(1:1) = "E") AND
              (IL-INVOICE-NUM(2:1) = "A" OR
              IL-INVOICE-NUM(2:1) = "B" OR
              IL-INVOICE-NUM(2:1) = "C" OR
              IL-INVOICE-NUM(2:1) = "D" OR
              IL-INVOICE-NUM(2:1) = "E") AND
              IL-INVOICE-NUM(1:1) NOT = IL-INVOICE-NUM(2:1) AND
              IL-INVOICE-NUM(4:6) <= 900000 AND
              IL-INVOICE-NUM(4:6) >= 100000 AND
              IL-INVOICE-NUM(3:1) = "-" AND
              IL-SKU-CODE(15:1) NOT = SPACE THEN
              PERFORM 40-OUTPUT-VALID-RECORD
              ADD 1 TO WS-VALID-RECORD-COUNT
           ELSE
              PERFORM 50-OUTPUT-INVALID-RECORD
              ADD 1 TO WS-INVALID-RECORD-COUNT
              MOVE INPUT-LINE TO WS-DL-INPUT-LINE

              WRITE REPORT-LINE FROM WS-DETAIL-LINE
              WRITE REPORT-LINE FROM WS-BLANK-LINE
           END-IF.


      *




      * read next input-file record
           READ INPUT-FILE
           AT END
              MOVE "y" TO WS-EOF-FLAG.
      *
       30-OUTPUT-REPORT-RECORD.

           WRITE REPORT-LINE FROM WS-NAME-LINE.

           WRITE REPORT-LINE FROM WS-BLANK-LINE.

           WRITE REPORT-LINE FROM WS-HEADING-1.

           WRITE REPORT-LINE FROM WS-BLANK-LINE.

           WRITE REPORT-LINE FROM WS-HEADING-2.

           WRITE REPORT-LINE FROM WS-HEADING-3.

      *    write report-line from ws-blank-line.



       40-OUTPUT-VALID-RECORD.

           WRITE VALID-LINE FROM INPUT-LINE.
      *    add 1 to ws-valid-record-count.


       50-OUTPUT-INVALID-RECORD.
      *
           WRITE INVALID-LINE FROM INPUT-LINE.

           MOVE INPUT-LINE TO WS-DL-INPUT-LINE.



      *
       60-OUTPUT-RECORD.

           MOVE WS-VALID-RECORD-COUNT TO WS-SUM-VALID-RECORDS.

           MOVE WS-INVALID-RECORD-COUNT TO WS-SUM-INVALID-RECORDS.

           MOVE WS-RECORD-COUNT TO WS-SUM-RECORD-COUNT.
      *
           WRITE REPORT-LINE FROM WS-BLANK-LINE.
      *
      *    write report-line from ws-detail-line.
      *
           WRITE REPORT-LINE FROM WS-SUMMARY.

       END PROGRAM EDIT.
