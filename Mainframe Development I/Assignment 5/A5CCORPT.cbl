       IDENTIFICATION DIVISION.
       PROGRAM-ID. A5CCORPT.
       AUTHOR. RAMIYAN GANGATHARAN.
       DATE-WRITTEN.  FEBRUARY 12 2024.
      *PROGRAM DESCRIPTION: ASSIGNMENT 5 COBOL
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
           SELECT EMP-FILE
               ASSIGN TO INFILE
               ORGANIZATION IS SEQUENTIAL.
      *
           SELECT REPORT-FILE
               ASSIGN TO OUTFILE
               ORGANIZATION IS SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD EMP-FILE
           DATA RECORD IS EMP-REC
           RECORDING MODE IS F
           RECORD CONTAINS 51 CHARACTERS.
      *
       01 EMP-REC.
          05 EMP-REC-NUM              PIC X(3).
          05 EMP-REC-NAME             PIC X(12).
          05 EMP-REC-CALLS            PIC 999 OCCURS 12 TIMES.
      *
       FD REPORT-FILE
           RECORDING MODE IS F
           DATA RECORD IS REPORT-LINE
           RECORD CONTAINS 132 CHARACTERS.
      *
       01 REPORT-LINE                 PIC X(132).
      *
       WORKING-STORAGE SECTION.

       01 WS-DL-CALL-TOTAL            PIC 9(4)   VALUE 0.

       01 WS-CONSTANTS.
          05 WS-NUMBER-OF-MONTHS      PIC 99     VALUE 12.

       01 WS-SUB                      PIC 99.

       01 WS-CALCULATED-FIELDS.
          05 WS-NON-ZERO-MONTH-COUNT  PIC 9(2)   VALUE 0.

       01 WS-EOF-FLAG                 PIC X      VALUE 'n'.
          88 WS-END-OF-FILE                      VALUE "y".

       01 WS-TOTALS.
          05 WS-GRAND-TOTAL           PIC 9(5)   VALUE 0.
          05 WS-EMP-TOTAL             PIC 9(5)   VALUE 0.
          05 WS-TOTAL-NO-CALLS        PIC 9(5)   VALUE 0.

      *
       01 WS-NAME-LINE.
          05 FILLER                   PIC X(5)   VALUE SPACES.
          05 FILLER                   PIC X(9)   VALUE ' RAMIYAN '.
          05 FILLER                   PIC X(13)  VALUE ' GANGATHARAN '.
          05 FILLER                   PIC X(6)   VALUE ' LAB 5'.
          05 FILLER                   PIC X(5)   VALUE SPACES.
          05 WS-NAME-LINE-DATE        PIC 9(6).
          05 FILLER                   PIC X(4)   VALUE SPACES.
          05 WS-NAME-LINE-TIME        PIC 9(8).
          05 FILLER                   PIC X(50)  VALUE SPACES.
      *
       01 WS-REPORT-HEADING.
          05 FILLER                   PIC X(40)  VALUE SPACES.
          05 FILLER                   PIC X(12)  VALUE 'CALL CENTRE '.
          05 FILLER                   PIC X(12)  VALUE 'VOLUMES FOR '.
          05 FILLER                   PIC X(11)  VALUE 'JULY - JUNE'.

          05 FILLER                   PIC X(40)  VALUE SPACES.
          05 FILLER                   PIC X(12)  VALUE SPACES.
      *
       01 WS-HEADING-LINE1.
          05 FILLER                   PIC X(2)   VALUE SPACES.
          05 FILLER                   PIC X(8)   VALUE 'OPERATOR'.
          05 FILLER                   PIC X(2)   VALUE SPACES.
          05 FILLER                   PIC X(8)   VALUE 'OPERATOR'.
          05 FILLER                   PIC X(6)   VALUE SPACES.
          05 FILLER                   PIC X(3)   VALUE 'JUL'.
          05 FILLER                   PIC X(4)   VALUE SPACES.
          05 FILLER                   PIC X(3)   VALUE 'AUG'.
          05 FILLER                   PIC X(4)   VALUE SPACES.
          05 FILLER                   PIC X(3)   VALUE 'SEP'.
          05 FILLER                   PIC X(4)   VALUE SPACES.
          05 FILLER                   PIC X(3)   VALUE 'OCT'.
          05 FILLER                   PIC X(4)   VALUE SPACES.
          05 FILLER                   PIC X(3)   VALUE 'NOV'.
          05 FILLER                   PIC X(4)   VALUE SPACES.
          05 FILLER                   PIC X(3)   VALUE 'DEC'.
          05 FILLER                   PIC X(4)   VALUE SPACES.
          05 FILLER                   PIC X(3)   VALUE 'JAN'.
          05 FILLER                   PIC X(4)   VALUE SPACES.
          05 FILLER                   PIC X(3)   VALUE 'FEB'.
          05 FILLER                   PIC X(4)   VALUE SPACES.
          05 FILLER                   PIC X(3)   VALUE 'MAR'.
          05 FILLER                   PIC X(4)   VALUE SPACES.
          05 FILLER                   PIC X(3)   VALUE 'APR'.
          05 FILLER                   PIC X(4)   VALUE SPACES.
          05 FILLER                   PIC X(3)   VALUE 'MAY'.
          05 FILLER                   PIC X(4)   VALUE SPACES.
          05 FILLER                   PIC X(3)   VALUE 'JUN'.
          05 FILLER                   PIC X(4)   VALUE SPACES.
          05 FILLER                   PIC X(5)   VALUE 'TOTAL'.
          05 FILLER                   PIC X(4)   VALUE SPACES.
          05 FILLER                   PIC X(3)   VALUE 'AVG'.
          05 FILLER                   PIC X(4)   VALUE SPACES.
          05 FILLER                   PIC X(3)   VALUE 'REM'.
          05 FILLER                   PIC X(3)   VALUE SPACES.
      *
       01 WS-HEADING-LINE2.
          05 FILLER                   PIC X(5)   VALUE SPACES.
          05 FILLER                   PIC X(1)   VALUE '#'.
          05 FILLER                   PIC X(8)   VALUE SPACES.
          05 FILLER                   PIC X(4)   VALUE 'NAME'.
          05 FILLER                   PIC X(114) VALUE SPACES.
      *
       01 WS-DETAIL-LINE.
          05 FILLER                   PIC X(4)   VALUE SPACES.
          05 WS-DETAIL-LINE-NUM       PIC X(3).
          05 FILLER                   PIC X(6)   VALUE SPACES.
          05 WS-DETAIL-LINE-NAME      PIC X(12).
          05 FILLER                   PIC X(1)   VALUE SPACES.

          05 WS-DETAIL-LINE-MONTHS OCCURS 12 TIMES.
             10 WS-D1-MONTHS          PIC ZZ9.
             10 FILLER                PIC X(4)   VALUE SPACES.

          05 WS-DL-TOTAL              PIC ZZZZ9.
          05 FILLER                   PIC X(5)   VALUE SPACES.
          05 WS-DL-AVG                PIC 9(2).
          05 FILLER                   PIC X(4)   VALUE SPACES.
          05 WS-DL-REM                PIC 99.
          05 FILLER                   PIC X(84)  VALUE SPACES.
      *
       01 WS-TOTAL-LINE1.
          05 FILLER                   PIC X(6)   VALUE SPACES.
          05 FILLER                   PIC X(10)  VALUE "NUMBER OF ".
          05 FILLER                   PIC X(10)  VALUE "OPERATORS ".
          05 FILLER                   PIC X(8)   VALUE "WITH NO ".
          05 FILLER                   PIC X(7)   VALUE "CALLS: ".
      *
          05 WS-TOTAL-LINE-NO-CALLS   PIC ZZZZ9.
          05 FILLER                   PIC X(86)  VALUE SPACES.
      *
       01 WS-TOTAL-LINE2.
          05 FILLER                   PIC X(6)   VALUE SPACES.
          05 FILLER                   PIC X(10)  VALUE "NUMBER OF ".
          05 FILLER                   PIC X(12)  VALUE "MONTHS WITH ".
          05 FILLER                   PIC X(12)  VALUE "NO CALLS:   ".
      *
          05 WS-TOTAL-LINE-ZERO-MTHS  PIC ZZZZ9.
          05 FILLER                   PIC X(86)  VALUE SPACES.
      *
       01 WS-TOTAL-LINE3.
          05 FILLER                   PIC X(6)   VALUE SPACES.
          05 FILLER                   PIC X(14)  VALUE "OVERALL TOTAL ".
          05 FILLER                   PIC X(14)  VALUE "CALLS:        ".
      *
          05 WS-TOTAL-LINE-CALLS      PIC ZZZZ9.
          05 FILLER                   PIC X(86)  VALUE SPACES.
      *
       PROCEDURE DIVISION.
      *
       000-MAIN.
      *
      *open files
           OPEN INPUT EMP-FILE,
                OUTPUT REPORT-FILE.
      *
      *get the current date & time
           ACCEPT WS-NAME-LINE-DATE FROM DATE.
           ACCEPT WS-NAME-LINE-TIME FROM TIME.

           PERFORM 100-PRINT-HEADINGS.
           PERFORM 200-READ-INPUT-FILE.
           PERFORM 300-PROCESS-RECORDS
              UNTIL WS-END-OF-FILE.

           PERFORM 400-PRINT-TOTALS.

           CLOSE EMP-FILE
                 REPORT-FILE.

           STOP RUN.
      *
       100-PRINT-HEADINGS.
      *
           WRITE REPORT-LINE
              FROM WS-NAME-LINE
              AFTER ADVANCING 1 LINE.
      *
           WRITE REPORT-LINE
              FROM WS-REPORT-HEADING
              AFTER ADVANCING 1 LINE.
      *
           WRITE REPORT-LINE
              FROM WS-HEADING-LINE1
              AFTER ADVANCING 2 LINES.
      *
           WRITE REPORT-LINE
              FROM WS-HEADING-LINE2
              AFTER ADVANCING 1 LINE.
      *
       200-READ-INPUT-FILE.
           READ EMP-FILE
           AT END
              MOVE 'y' TO WS-EOF-FLAG.

       300-PROCESS-RECORDS.
           MOVE ZERO TO WS-DL-CALL-TOTAL.

           PERFORM 310-CALCULATIONS
              VARYING WS-SUB FROM 1 BY 1
              UNTIL WS-SUB IS GREATER THAN WS-NUMBER-OF-MONTHS.

           MOVE EMP-REC-NUM
              TO WS-DETAIL-LINE-NUM.

           MOVE EMP-REC-NAME
              TO WS-DETAIL-LINE-NAME.

           MOVE WS-DL-CALL-TOTAL
              TO WS-DL-TOTAL.

           WRITE REPORT-LINE
              FROM WS-DETAIL-LINE
              AFTER ADVANCING 1 LINE.

           MOVE 0
              TO WS-EMP-TOTAL.

           MOVE 0
              TO WS-NON-ZERO-MONTH-COUNT.

           PERFORM 200-READ-INPUT-FILE.
      *
       310-CALCULATIONS.
           MOVE EMP-REC-CALLS(WS-SUB)
              TO WS-D1-MONTHS(WS-SUB).

      *    AVERAGE CALCULATIONS
           IF (EMP-REC-CALLS(WS-SUB) IS GREATER THAN ZERO)
              THEN
              ADD EMP-REC-CALLS(WS-SUB)
                 TO WS-DL-CALL-TOTAL

              ADD 1 TO WS-NON-ZERO-MONTH-COUNT

              IF (WS-NON-ZERO-MONTH-COUNT IS GREATER THAN ZERO)
                 THEN
                 DIVIDE WS-DL-CALL-TOTAL
                    BY WS-NON-ZERO-MONTH-COUNT
                    GIVING WS-DL-AVG REMAINDER WS-DL-REM
              ELSE
                 MOVE ZERO
                    TO WS-DL-AVG
              END-IF

           ELSE
              MOVE ZERO
                 TO WS-DL-AVG

           END-IF.

      *    MANUAL REMAINDER CALCULATIONS
      *    (before discovering the prebuilt function)

      *    IF (WS-NON-ZERO-MONTH-COUNT IS GREATER THAN ZERO)
      *       THEN
      *       SUBTRACT WS-DL-AVG
      *          FROM WS-NUMBER-OF-MONTHS
      *          GIVING WS-DL-REM
      *    ELSE
      *       MOVE ZERO TO WS-DL-REM
      *    END-IF.


       400-PRINT-TOTALS.
           MOVE WS-TOTAL-NO-CALLS TO WS-TOTAL-LINE-NO-CALLS.
           MOVE WS-GRAND-TOTAL TO WS-TOTAL-LINE-CALLS.

           IF WS-DL-AVG IS NUMERIC THEN
              MOVE WS-DL-AVG TO WS-TOTAL-LINE-CALLS
           ELSE
              MOVE ZERO TO WS-TOTAL-LINE-CALLS
           END-IF.

           WRITE REPORT-LINE FROM WS-TOTAL-LINE1 AFTER ADVANCING 2
              LINES.
           WRITE REPORT-LINE FROM WS-TOTAL-LINE2 AFTER ADVANCING 1 LINE.
           WRITE REPORT-LINE FROM WS-TOTAL-LINE3 AFTER ADVANCING 1 LINE.

       END PROGRAM A5CCORPT.
