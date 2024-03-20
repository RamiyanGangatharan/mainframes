       IDENTIFICATION DIVISION.
       PROGRAM-ID. A4SALRPT.
       DATE-WRITTEN. FEBRUARY 5, 2024.
       AUTHOR. RAMIYAN GANGATHARAN.
      *DESCRIPTION: THE COBOL FILE FOR ASSIGNMENT 4.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT INPUT-FILE
           ASSIGN TO INFILE
           ORGANIZATION IS SEQUENTIAL.

           SELECT OUTPUT-FILE
           ASSIGN TO OUTFILE
           ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD INPUT-FILE
           RECORDING MODE IS F
           DATA RECORD IS INPUT-LINE
           RECORD CONTAINS 28 CHARACTERS.

       01 INPUT-LINE.
          05 IL-EMPLOYEE-NUMBER         PIC 9(3).
          05 IL-NAME                    PIC X(15).
          05 IL-EDUCATION-CODE          PIC X(1).
          05 IL-YEARS-OF-SERVICE        PIC X(2).
          05 IL-PRESENT-SALARY          PIC 9(5)V99.

       FD OUTPUT-FILE
           RECORDING MODE IS F
           DATA RECORD IS OUTPUT-LINE
           RECORD CONTAINS 145 CHARACTERS.

       01 OUTPUT-LINE                   PIC X(145).

       WORKING-STORAGE SECTION.

       01 WS-REPORT-HEADER.
          05 FILLER                     PIC X(4)       VALUE SPACES.
          05 WS-AUTHOR1                 PIC X(8)       VALUE "RAMIYAN ".
          05 WS-AUTHOR2                 PIC X(1)       VALUE "G".
          05 WS-COMMA                   PIC X(2)       VALUE ", ".
          05 WS-ASSIGNMENT              PIC X(3)       VALUE "A4 ".
          05 FILLER-REPORT1             PIC X(15)      VALUE SPACES.
          05 WS-DUE-DATE                PIC X(10)      VALUE "9/2/2024".
          05 FILLER-REPORT2             PIC X(44)      VALUE SPACES.
          05 WS-STUDENT-NUMBER          PIC X(9)       VALUE "100835223"
                                                                      .


       01 WS-GAP.
          05 WS-FILLER                  PIC X(145)     VALUE " ".

       01 WS-REPORT-TITLE.
          05 TITLE_FILLER               PIC X(40)      VALUE SPACES.
          05 TITLE1                     PIC X(8)       VALUE "EMPLOYEE".
          05 TITLE_FILLER               PIC X(1)       VALUE SPACES.
          05 TITLE2                     PIC X(7)       VALUE "SALARY ".
          05 TITLE3                     PIC X(6)       VALUE "REPORT".

       01 WS-COLUMN-HEADER.
          05 WS-INIT-FILLER             PIC X(4)       VALUE SPACES.
          05 WS-EMP-NUMBER              PIC X(3)       VALUE "NUM".
          05 WS-GAP-FILL                PIC X(7)       VALUE SPACES.
          05 WS-EMP-NAME                PIC X(4)       VALUE "NAME".
          05 WS-GAP-FILL                PIC X(7)       VALUE SPACES.
          05 WS-EDU-CODE                PIC X(4)       VALUE "CODE".
          05 WS-GAP-FILL                PIC X(2)       VALUE SPACES.
          05 WS-YEARS                   PIC X(5)       VALUE "YEARS".
          05 WS-GAP-FILL                PIC X(2)       VALUE SPACES.
          05 WS-POSITION                PIC X(8)       VALUE "POSITION".
          05 WS-GAP-FILL                PIC X(8)       VALUE SPACES.
          05 WS-PRESENT-SALARY          PIC X(6)       VALUE "SALARY".
          05 WS-GAP-FILL                PIC X(2)       VALUE SPACES.
          05 WS-PERCENT-RAISE           PIC X(7)       VALUE "% RAISE".
          05 WS-GAP-FILL                PIC X(3)       VALUE SPACES.
          05 WS-PAY-INCREASE            PIC X(10)      VALUE
                                                           "$ INCREASE".
          05 WS-GAP-FILL                PIC X(3)       VALUE SPACES.
          05 WS-NEW-SALARY              PIC X(10)      VALUE
                                                           "NEW SALARY".

       01 WS-DETAIL.
          05 WS-FILLER                  PIC X(4)       VALUE SPACES.
          05 WSD-EMPLOYEE-NUMBER        PIC 9(3).
          05 WS-FILLER                  PIC X(2)       VALUE SPACES.
          05 WSD-EMPLOYEE-NAME          PIC X(15).
          05 WS-FILLER                  PIC X(2)       VALUE SPACES.
          05 WSD-EDUCATION-CODE         PIC X(1).
          05 WS-FILLER                  PIC X(6)       VALUE SPACES.
          05 WSD-YEARS-SERVICE          PIC ZZ.
          05 WS-FILLER                  PIC X(2)       VALUE SPACES.
          05 WSD-POSITION               PIC X(12).
          05 WS-FILLER                  PIC X(2)       VALUE SPACES.
          05 WSD-PRESENT-SALARY         PIC ZZ,ZZ9.99.
          05 WS-FILLER                  PIC X(2)       VALUE SPACES.
          05 WSD-INCREASE-PERCENT       PIC 9.9999.
          05 WS-FILLER                  PIC X(2)       VALUE SPACES.
          05 WSD-PAY-INCREASE           PIC ZZZ,ZZ9.99.
          05 WS-FILLER                  PIC X(2)       VALUE SPACES.
          05 WSD-NEW-SALARY             PIC ZZZ,ZZ9.99.


       01 WS-MATH.
          05 MATH-YEARS-SERVICE         PIC 9(2).
          05 MATH-PRESENT-SALARY        PIC 9(6)V99.
          05 MATH-PAY-INCREASE          PIC 9(6)V99.
          05 MATH-NEW-SALARY            PIC 9(6)V99.

       01 WSD-POSITIONER.
          05 ANALYST                    PIC X(12)      VALUE
                                                         '   ANALYST  '.
          05 SENIOR-PROG                PIC X(12)      VALUE
                                                         'SENIOR PROGR'.
          05 PROGRAMMER                 PIC X(12)      VALUE
                                                         ' PROGRAMMER '.
          05 JUNIOR-PROG                PIC X(12)      VALUE
                                                         'JUNIOR PROGR'.
          05 UNCLASSIFIED               PIC X(12)      VALUE
                                                         'UNCLASSIFIED'.

       01 WS-POSITION-RAISE-PERCENTAGES.
          05 PERCENT-ANALYST            PIC V9(4)      VALUE 0.1280.
          05 PERCENT-SENIOR-PROG        PIC V9(4)      VALUE 0.0930.
          05 PERCENT-PROGRAMMER         PIC V9(4)      VALUE 0.0670.
          05 PERCENT-JUNIOR-PROG        PIC V9(4)      VALUE 0.0320.
          05 PERCENT-UNCLASSIFIED       PIC V9(4)      VALUE 0.0000.


       01 WS-LINE-COUNT                 PIC 99         VALUE 0.
       01 WS-PAGE-COUNTER               PIC 9(2)       VALUE 1.

       01 WS-EOF-FLAG                   PIC X(1).
          88 WS-EOF                                    VALUE "Y"
                                                          , "y".
       77 WS-EOF-Y                      PIC X          VALUE "Y".
       77 WS-EOF-N                      PIC X          VALUE "N".
       77 WS-PAGE-CTR                   PIC 9(2)       VALUE 0.
       77 WS-LINE-CTR                   PIC 9(2)       VALUE 0.
       88 WS-EOP                                       VALUE 10 THRU 99.

       PROCEDURE DIVISION.
       000-MAIN.
           PERFORM 150-OPEN-FILES.
           PERFORM 200-REPORT-HEADER
           PERFORM 300-PAGE-HEADER.
           PERFORM 350-COLUMN-HEADER.
           PERFORM 125-GAP.
           PERFORM 400-PROCESS-INPUT-RECORDS.
           PERFORM 800-CLOSE-FILES.
           PERFORM 900-CLEANUP.
           GOBACK.

       125-GAP.
           MOVE WS-GAP
              TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

       150-OPEN-FILES.
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.

       200-REPORT-HEADER.
           PERFORM 125-GAP.
           WRITE OUTPUT-LINE
              FROM WS-REPORT-HEADER.

       300-PAGE-HEADER.
           PERFORM 125-GAP.
           WRITE OUTPUT-LINE
              FROM WS-REPORT-TITLE.

       350-COLUMN-HEADER.
           PERFORM 125-GAP.
           MOVE WS-COLUMN-HEADER
              TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

       400-PROCESS-INPUT-RECORDS.
           PERFORM 125-GAP.
           PERFORM UNTIL WS-EOF
                   READ INPUT-FILE
                   AT END
                      SET WS-EOF TO TRUE
                   NOT AT END
                       PERFORM 450-CALCULATIONS
                       PERFORM 500-PREPARE-OUTPUT
                   END-READ
                   IF WS-LINE-COUNT IS GREATER THAN OR EQUAL TO 10
                      PERFORM 125-GAP
                      PERFORM 350-COLUMN-HEADER
                      MOVE 0
                         TO WS-LINE-COUNT
                   END-IF
           END-PERFORM.

       450-CALCULATIONS.
      * ALGORITHM FOR POSITION NAMES
           MOVE WSD-YEARS-SERVICE
              TO MATH-YEARS-SERVICE.
           IF (IL-EDUCATION-CODE = "G")
              THEN
              IF (MATH-YEARS-SERVICE > 15)
                 THEN
                 MOVE ANALYST
                    TO WSD-POSITION
              ELSE
                 IF (MATH-YEARS-SERVICE > 7)
                    THEN
                    MOVE SENIOR-PROG
                       TO WSD-POSITION
                 ELSE
                    IF (MATH-YEARS-SERVICE > 2)
                       THEN
                       MOVE PROGRAMMER
                          TO WSD-POSITION
                    ELSE
                       MOVE UNCLASSIFIED
                          TO WSD-POSITION
                    END-IF
                 END-IF
              END-IF
           ELSE
              IF (MATH-YEARS-SERVICE > 10)
                 THEN
                 MOVE PROGRAMMER
                    TO WSD-POSITION
              ELSE
                 IF (MATH-YEARS-SERVICE > 4)
                    THEN
                    MOVE JUNIOR-PROG
                       TO WSD-POSITION
                 ELSE
                    MOVE UNCLASSIFIED
                       TO WSD-POSITION
                 END-IF
              END-IF
           END-IF

           MOVE WSD-PRESENT-SALARY
              TO MATH-PRESENT-SALARY

           EVALUATE WSD-POSITION

           WHEN ANALYST
                MULTIPLY PERCENT-ANALYST
                   BY 100
                   GIVING WSD-INCREASE-PERCENT

                MULTIPLY MATH-PRESENT-SALARY
                   BY PERCENT-ANALYST
                   GIVING MATH-PAY-INCREASE

           WHEN SENIOR-PROG
                MULTIPLY PERCENT-SENIOR-PROG
                   BY 100
                   GIVING WSD-INCREASE-PERCENT

                MULTIPLY MATH-PRESENT-SALARY
                   BY PERCENT-SENIOR-PROG
                   GIVING MATH-PAY-INCREASE

           WHEN PROGRAMMER
                MULTIPLY PERCENT-PROGRAMMER
                   BY 100
                   GIVING WSD-INCREASE-PERCENT

                MULTIPLY MATH-PRESENT-SALARY
                   BY PERCENT-PROGRAMMER
                   GIVING MATH-PAY-INCREASE

           WHEN JUNIOR-PROG
                MULTIPLY PERCENT-JUNIOR-PROG
                   BY 100
                   GIVING WSD-INCREASE-PERCENT

                MULTIPLY MATH-PRESENT-SALARY
                   BY PERCENT-JUNIOR-PROG
                   GIVING MATH-PAY-INCREASE

           WHEN OTHER
                MOVE ZERO
                   TO WSD-INCREASE-PERCENT
                MOVE ZERO
                   TO MATH-PAY-INCREASE

           END-EVALUATE

           IF MATH-PRESENT-SALARY NOT = ZERO
              THEN
              ADD MATH-PAY-INCREASE
                 TO MATH-PRESENT-SALARY
                 GIVING MATH-NEW-SALARY
           ELSE
              MOVE ZERO
                 TO MATH-NEW-SALARY
           END-IF

           MOVE MATH-PAY-INCREASE
              TO WSD-PAY-INCREASE.

           MOVE MATH-NEW-SALARY
              TO WSD-NEW-SALARY.

           MOVE WS-DETAIL
              TO OUTPUT-LINE.


       500-PREPARE-OUTPUT.
           PERFORM 450-CALCULATIONS.

           MOVE IL-EMPLOYEE-NUMBER
              TO WSD-EMPLOYEE-NUMBER.

           MOVE IL-NAME
              TO WSD-EMPLOYEE-NAME.

           MOVE IL-EDUCATION-CODE
              TO WSD-EDUCATION-CODE.

           MOVE IL-YEARS-OF-SERVICE
              TO WSD-YEARS-SERVICE.

           MOVE IL-PRESENT-SALARY
              TO WSD-PRESENT-SALARY.

           ADD 1
              TO WS-LINE-COUNT.

           WRITE OUTPUT-LINE
              FROM WS-DETAIL.
           PERFORM 125-GAP.

       800-CLOSE-FILES.
           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.

       900-CLEANUP.
           DISPLAY "Cleanup complete.".

       END PROGRAM A4SALRPT.
