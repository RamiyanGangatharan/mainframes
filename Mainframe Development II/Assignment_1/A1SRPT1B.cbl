       IDENTIFICATION DIVISION.
       PROGRAM-ID. A1SRPT1B.
       DATE-WRITTEN. SEPTEMBER 20, 2024.
       AUTHOR. RAMIYAN GANGATHARAN.
      * DESCRIPTION: COBOL FILE FOR ASSIGNMENT 1 (MAINFRAME II) RESTRUCTURED

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

      *    KC03D09.DCMAFD02.A1.NG1B.DATA - INPUT FILE FOR THE PROGRAM
           SELECT INPUT-FILE
           ASSIGN TO INFILE
           ORGANIZATION IS SEQUENTIAL.

      *    KC03D09.DCMAFD02.A1.RPT1B.OUT - OUTPUT FILE #1 (.OUT)
           SELECT OUTPUT-FILE
           ASSIGN TO OUTFILE
           ORGANIZATION IS SEQUENTIAL.


       DATA DIVISION.

       FILE SECTION.

      * CONTACTING THE JCL TO WRITE & STORE THE COLUMNS INTO INPUT-LINE.
       FD INPUT-FILE
           RECORDING MODE IS F
           DATA RECORD IS INPUT-LINE
           RECORD CONTAINS 36 CHARACTERS.

       01 INPUT-LINE.
          05 IL-EMPLOYEE-NUMBER         PIC 9(3).
          05 IL-NAME                    PIC X(15).
          05 IL-YEARS-OF-SERVICE        PIC X(2).
          05 IL-EDUCATION-CODE          PIC X(1).
          05 IL-PRESENT-SALARY          PIC 9(5)V99.
          05 IL-BUDGET-ESTIMATE         PIC 9(6)V99.

      * CONTACTING THE JCL TO WRITE THE NEW COLUMNS INTO OUTPUT-LINE.
       FD OUTPUT-FILE
           RECORDING MODE IS F
           DATA RECORD IS OUTPUT-LINE
           RECORD CONTAINS 145 CHARACTERS.

       01 OUTPUT-LINE                   PIC X(145).

       WORKING-STORAGE SECTION.

      * THE HEADER FOR THE TOP OF THE PAGE, ONLY USE ONCE.
       01 WS-REPORT-HEADER.
          05 WS-GAP-FILL                PIC X(4)       VALUE SPACES.
          05 WS-AUTHOR1                 PIC X(8)       VALUE "RAMIYAN ".

          05 WS-AUTHOR2                 PIC X(11)      VALUE
                "GANGATHARAN".

          05 WS-GAP-FILL                PIC X(23)      VALUE SPACES.

          05 WS-ASSIGNMENT              PIC X(36)      VALUE
                "MAINFRAMES II - ASSIGNMENT I".

          05 WS-GAP-FILL                PIC X(8)       VALUE SPACES.

          05 WS-DUE-DATE                PIC X(10)      VALUE
                "09/20/2024".

          05 WS-GAP-FILL                PIC X(17)      VALUE SPACES.
          05 WS-STUDENT-NUMBER          PIC X(9)       VALUE "100835223"
           .

      * VARIABLE FOR A LINE BREAK.
       01 WS-GAP.
          05 WS-FILLER                  PIC X(145)     VALUE " ".

       01 WS-REPORT-TITLE.
          05 TITLE_FILLER               PIC X(46)      VALUE SPACES.
          05 TITLE0                     PIC X(8)       VALUE "GRADUATE".
          05 TITLE_FILLER               PIC X(1)       VALUE SPACES.
          05 TITLE1                     PIC X(8)       VALUE "EMPLOYEE".
          05 TITLE_FILLER               PIC X(1)       VALUE SPACES.
          05 TITLE2                     PIC X(7)       VALUE "SALARY ".
          05 TITLE3                     PIC X(6)       VALUE "REPORT".

       01 WS-COLUMN-HEADER.
          05 WS-INIT-FILLER             PIC X(4)       VALUE SPACES.
          05 WS-EMP-NUMBER              PIC X(3)       VALUE "NUM".
          05 WS-GAP-FILL                PIC X(7)       VALUE SPACES.
          05 WS-EMP-NAME                PIC X(4)       VALUE "NAME".
          05 WS-GAP-FILL                PIC X(11)      VALUE SPACES.
          05 WS-YEARS                   PIC X(5)       VALUE "YEARS".
          05 WS-GAP-FILL                PIC X(3)       VALUE SPACES.
          05 WS-EDU-CODE                PIC X(4)       VALUE "CODE".
          05 WS-GAP-FILL                PIC X(3)       VALUE SPACES.
          05 WS-POSITION                PIC X(8)       VALUE "POSITION".
          05 WS-GAP-FILL                PIC X(5)       VALUE SPACES.
          05 WS-PRESENT-SALARY          PIC X(6)       VALUE "SALARY".
          05 WS-GAP-FILL                PIC X(4)       VALUE SPACES.
          05 WS-PERCENT-RAISE           PIC X(7)       VALUE "% RAISE".
          05 WS-GAP-FILL                PIC X(3)       VALUE SPACES.
          05 WS-PAY-INCREASE            PIC X(10)      VALUE
                "$ INCREASE".

          05 WS-GAP-FILL                PIC X(3)       VALUE SPACES.

          05 WS-NEW-SALARY              PIC X(10)      VALUE
                "NEW_SALARY".

          05 WS-GAP-FILL                PIC X(3)       VALUE SPACES.

          05 WS-BUDGET-ESTIMATE         PIC X(13)      VALUE
                "BUD_ESTIMATE".

          05 WS-GAP-FILL                PIC X(3)       VALUE SPACES.

          05 WS-BUDGET-DIFFERENCE       PIC X(8)       VALUE
                "BUD_DIFF".

       01 WS-INCREASE-PERCENT-NUMERIC   PIC 9(5)V99.

      * VARIABLE GROUPING FOR ALL THE DISPLAY VARIABLES CORRESPONDING
      * TO THE INPUT FILE AND GENERATED DATA FROM THE CALCULATIONS.
       01 WS-DETAIL.
          05 WS-FILLER                  PIC X(4)       VALUE SPACES.
          05 WSD-EMPLOYEE-NUMBER        PIC 9(3).
          05 WS-FILLER                  PIC X(4)       VALUE SPACES.
          05 WSD-EMPLOYEE-NAME          PIC X(15).
          05 WS-FILLER                  PIC X(4)       VALUE SPACES.
          05 WSD-YEARS-SERVICE          PIC ZZ.
          05 WS-FILLER                  PIC X(6)       VALUE SPACES.
          05 WSD-EDUCATION-CODE         PIC X(1).
          05 WS-FILLER                  PIC X(2)       VALUE SPACES.
          05 WSD-POSITION               PIC X(12).
          05 WS-FILLER                  PIC X(2)       VALUE SPACES.
          05 WSD-PRESENT-SALARY         PIC ZZ,ZZ9.99.
          05 WS-FILLER                  PIC X(4)       VALUE SPACES.
          05 WSD-INCREASE-PERCENT       PIC 9.9999.
          05 WS-FILLER                  PIC X(2)       VALUE SPACES.
          05 WSD-PAY-INCREASE           PIC ZZZ,ZZ9.99.
          05 WS-FILLER                  PIC X(3)       VALUE SPACES.
          05 WSD-NEW-SALARY             PIC ZZZ,ZZ9.99.
          05 WS-FILLER                  PIC X(4)       VALUE SPACES.
          05 WSD-BUDGET-ESTIMATE        PIC ZZZ,ZZ9.99.
          05 WS-FILLER                  PIC X(5)       VALUE SPACES.
          05 WSD-BUDGET-DIFFERENCE      PIC -ZZ,ZZ9.99.

      * A TEMPORARY VARIABLE GROUPING TO MOVE VALUES INTO TO PERFORM
      * ARITHMETIC OPERATIONS INTO THEN TO MOVE TO DISPLAY VARIABLES.
       01 WS-MATH.
          05 MATH-YEARS-SERVICE         PIC 9(2)       VALUE 0.
          05 MATH-PRESENT-SALARY        PIC 9(6)V99    VALUE 0.
          05 MATH-PAY-INCREASE          PIC 9(6)V99    VALUE 0.
          05 MATH-NEW-SALARY            PIC 9(6)V99    VALUE 0.
          05 MATH-CURRENT-BUDGET        PIC 9(6)V99    VALUE 0.
          05 MATH-BUD-DIFF              PIC S9(7)V99   VALUE 0.

      * DISPLAY VARIABLES FOR POSITION CODES.
       01 WSD-POSITIONER.
          05 ANALYST                    PIC X(12)      VALUE
                '   ANALYST  '.

          05 SENIOR-PROG                PIC X(12)      VALUE
                ' SENIOR PROG'.

          05 PROGRAMMER                 PIC X(12)      VALUE
                ' PROGRAMMER '.

          05 JUNIOR-PROG                PIC X(12)      VALUE
                ' JUNIOR PROG'.

          05 UNCLASSIFIED               PIC X(12)      VALUE
                'UNCLASSIFIED'.

      * NUMERIC DECIMAL VARIABLES TO HOLD PERCENTAGE CALCULATION NUMBERS.
       01 WS-POSITION-RAISE-PERCENTAGES.
          05 PERCENT-ANALYST            PIC V9(4)      VALUE 0.1480.
          05 PERCENT-SENIOR-PROG        PIC V9(4)      VALUE 0.1130.
          05 PERCENT-PROGRAMMER         PIC V9(4)      VALUE 0.0870.
          05 PERCENT-JUNIOR-PROG        PIC V9(4)      VALUE 0.0520.
          05 PERCENT-UNCLASSIFIED       PIC V9(4)      VALUE 0.0000.

      * COUNTERS FOR PAGINATION
       01 WS-LINE-COUNT                 PIC 99         VALUE 0.
       01 WS-PAGE-COUNTER               PIC 9(2)       VALUE 1.
       01 WS-LINE-LIMIT                 PIC 9(2)       VALUE 20.

      * FLAG VARIABLES FOR USE AS PAGE AND LINE CONTROLLERS.
      * EOF = END OF FILE; EOP = END OF PAGE; CTR = SHORT FOR COUNTER;
       01 WS-EOF-FLAG                   PIC X(1).
          88 WS-EOF                                    VALUE "Y", "y".
       77 WS-EOF-Y                      PIC X          VALUE "Y".
       77 WS-EOF-N                      PIC X          VALUE "N".
       77 WS-PAGE-CTR                   PIC 9(2)       VALUE 0.
       77 WS-LINE-CTR                   PIC 9(2)       VALUE 0.
       88 WS-EOP                                       VALUE 10 THRU 99.

       PROCEDURE DIVISION.

      * USE THIS PARAGRAPH (000) TO RUN EVERYTHING LIKE IN JAVA OR C#/C++
       000-MAIN.
           PERFORM 100-OPEN-FILES.
           PERFORM 200-REPORT-HEADER
           PERFORM 225-PAGE-HEADER.
           PERFORM 250-COLUMN-HEADER.
           PERFORM 400-PROCESS-RECORDS.
           PERFORM 700-DISPLAY-FOOTER.
           PERFORM 990-CLOSE-FILES.
           GOBACK.

      * OPEN ALL INPUT AND OUTPUT FILES HERE
       100-OPEN-FILES.
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.

      * PARAGRAPH TO USE THE LINE BREAK VARIABLE
       125-GAP.
           MOVE WS-GAP TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

      * PARAGRAPH TO PRINT OUT THE REPORT HEADER
       200-REPORT-HEADER.
           PERFORM 125-GAP.
           WRITE OUTPUT-LINE FROM WS-REPORT-HEADER.

      * PARAGRAPH TO PRINT OUT THE PAGE HEADER
       225-PAGE-HEADER.
           PERFORM 125-GAP.
           WRITE OUTPUT-LINE FROM WS-REPORT-TITLE.

      * PARAGRAPH TO PRINT OUT THE COLUMN HEADER
       250-COLUMN-HEADER.
           PERFORM 125-GAP.
           MOVE WS-COLUMN-HEADER TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.

      * PARAGRAPH TO READ THE ROWS IN A LOOP THEN PERFORM CALCULATIONS
      * WHILE IN THAT LOOP (SALARY RAISES, BUDGET DIFFERENCES ETC.)
       400-PROCESS-RECORDS.
           PERFORM UNTIL WS-EOF
                   READ INPUT-FILE
                   AT END
                      SET WS-EOF TO TRUE
                   NOT AT END
                       IF (IL-EDUCATION-CODE = "N")
                          THEN
                          PERFORM 425-NON-GRADUATES
                          PERFORM 600-DISPLAY-OUTPUT
                       END-IF
                   END-READ

                   IF (WS-LINE-COUNT >= WS-LINE-LIMIT)
                      THEN
                      PERFORM 125-GAP
                      PERFORM 250-COLUMN-HEADER
                      MOVE 0 TO WS-LINE-COUNT
                   END-IF
           END-PERFORM.

       425-NON-GRADUATES.
           PERFORM 475-N-POSITION-DETERMINATION.
           PERFORM 500-NEW-SALARY-CALCULATION.

      * POSITION DESIGNATIONS FOR GRADUATES ONLY
      * NOTE: WRITE CODE IN A1SRPT1B FOR NON-GRADUATES.
       475-N-POSITION-DETERMINATION.
           MOVE IL-YEARS-OF-SERVICE TO MATH-YEARS-SERVICE

           IF (MATH-YEARS-SERVICE > 10)
              THEN
              MOVE PROGRAMMER TO WSD-POSITION
              MOVE PERCENT-PROGRAMMER TO WSD-INCREASE-PERCENT
           ELSE
              IF (MATH-YEARS-SERVICE > 4 AND MATH-YEARS-SERVICE <= 10)
                 THEN
                 MOVE JUNIOR-PROG TO WSD-POSITION
                 MOVE PERCENT-JUNIOR-PROG TO WSD-INCREASE-PERCENT
              ELSE
                 IF (MATH-YEARS-SERVICE <= 4)
                    THEN
                    MOVE UNCLASSIFIED TO WSD-POSITION
                    MOVE PERCENT-UNCLASSIFIED
                       TO WSD-INCREASE-PERCENT
                 END-IF.

       500-NEW-SALARY-CALCULATION.
           MOVE IL-PRESENT-SALARY    TO MATH-PRESENT-SALARY.
           MOVE WSD-INCREASE-PERCENT TO WS-INCREASE-PERCENT-NUMERIC.

           COMPUTE MATH-PAY-INCREASE
              = MATH-PRESENT-SALARY * WS-INCREASE-PERCENT-NUMERIC.

           COMPUTE MATH-NEW-SALARY
              = MATH-PRESENT-SALARY + MATH-PAY-INCREASE.

           MOVE MATH-NEW-SALARY TO WSD-NEW-SALARY.
           MOVE MATH-PAY-INCREASE TO WSD-PAY-INCREASE.

           MOVE IL-BUDGET-ESTIMATE TO MATH-CURRENT-BUDGET.
           COMPUTE MATH-BUD-DIFF
              = MATH-CURRENT-BUDGET - MATH-NEW-SALARY.
           MOVE MATH-BUD-DIFF TO WSD-BUDGET-DIFFERENCE.

       600-DISPLAY-OUTPUT.
           MOVE IL-EDUCATION-CODE   TO WSD-EDUCATION-CODE.
           MOVE MATH-PRESENT-SALARY TO WSD-PRESENT-SALARY
           MOVE MATH-BUD-DIFF       TO WSD-BUDGET-DIFFERENCE.
           MOVE IL-BUDGET-ESTIMATE  TO WSD-BUDGET-ESTIMATE.
           MOVE IL-EMPLOYEE-NUMBER  TO WSD-EMPLOYEE-NUMBER.
           MOVE IL-NAME             TO WSD-EMPLOYEE-NAME.
           MOVE IL-YEARS-OF-SERVICE TO WSD-YEARS-SERVICE.
           MOVE WS-DETAIL           TO OUTPUT-LINE.

           WRITE OUTPUT-LINE.

           ADD 1 TO WS-LINE-COUNT.

      * GENERATE SUMMARIES HERE
       700-DISPLAY-FOOTER.


      * CLOSE ALL INPUT AND OUTPUT FILES HERE
       990-CLOSE-FILES.
           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.

       END PROGRAM A1SRPT1B.
