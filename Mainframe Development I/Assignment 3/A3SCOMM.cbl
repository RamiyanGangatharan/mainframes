        IDENTIFICATION DIVISION.
        PROGRAM-ID. A3SCOMM.
        DATE-WRITTEN. JANUARY 30, 2024.
        AUTHOR. RAMIYAN GANGATHARAN.
      * DESCRIPTION: MAINFRAME I, ASSIGNMENT III.

        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.

        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
      * INPUT-FILE DECLARATION
            SELECT INPUT-FILE
                ASSIGN TO INFILE
                ORGANIZATION IS SEQUENTIAL.

      * OUTPUT-FILE DECLARATION
            SELECT OUTPUT-FILE
                ASSIGN TO OUTFILE
                ORGANIZATION IS SEQUENTIAL.

        DATA DIVISION.
        FILE SECTION.

        FD INPUT-FILE
            RECORDING MODE IS F
            DATA RECORD IS INPUT-LINE
            RECORD CONTAINS 32 CHARACTERS.

        01 INPUT-LINE.
            05 il-number     pic x(3).
            05 il-name       pic x(8).
            05 il-sales      pic x(6).
            05 il-mincomm    pic x(6).
            05 il-maxcomm    pic x(6).
            05 il-commperc   pic 99v9.

        FD OUTPUT-FILE
            RECORDING MODE IS F
            DATA RECORD IS OUTPUT-LINE
            RECORD CONTAINS 145 CHARACTERS.

        01 OUTPUT-LINE       pic x(145).

        WORKING-STORAGE SECTION.

        01 ws-eof-flag       pic x value "N".
        77 ws-eof-Y          pic x value "y".
        77 ws-eof-N          pic x value "N".
        77 ws-EARNED         pic 9(10)v99 value zero.
        77 ws-BONUS          pic 9(10)v99 value zero.

        01 ws-title.
            05 ws-filler1    pic x(25)  value spaces.
            05 ws-name       pic x(22) value "RAMIYAN GANGATHARAN -".
            05 ws-text       pic x(14) value "MAINFRAME I - ".
            05 ws-text2      pic x(14) value "ASSIGNMENT III".

        01 ws-title2.
            05 ws-filler2    pic x(35) value spaces.
            05 ws-commission pic x(23) value "SALES COMMISSION REPORT".

        01 ws-col-header.
            05 ws-filler3      pic x(5)   value spaces.
            05 ws-ColNumber    pic x(3)   value "NO.".
            05 ws-filler4      pic x(6)   value spaces.
            05 ws-name-out     pic x(7)   value "NAME".
            05 ws-filler5      pic x(6)   value spaces.
            05 ws-ColSales     pic x(9)   value "SALES".
            05 ws-filler6      pic x(1)   value spaces.
            05 ws-ColMin       pic x(3)   value "MIN".
            05 ws-filler7      pic x(8)   value spaces.
            05 ws-ColMax       pic x(3)   value "MAX".
            05 ws-filler8      pic x(6)   value spaces.
            05 ws-ColPerc      pic x(6)   value "COMM %".
            05 ws-filler9      pic x(8)   value spaces.
            05 ws-ColEarn      pic x(12)  value "EARNED".
            05 ws-filler10     pic x(3)   value spaces.
            05 ws-ColPaid      pic x(9)   value "PAID".
            05 ws-filler11     pic x(3)   value spaces.
            05 ws-ColBonus     pic x(11)  value "BONUS".
            05 ws-filler12     pic x(5)   value spaces.

        01 ws-detail.
            05 wsd_filler11   pic x(5)   value spaces.
            05 wsd_number     pic zzz.
            05 wsd_filler12   pic x(6)   value spaces.
            05 wsd_name       pic x(8).
            05 wsd_filler13   pic x(3)   value spaces.
            05 wsd_sales      pic zzz,zz9.
            05 wsd_filler14   pic x(3)   value spaces.
            05 wsd_mincomm    pic zzz,zz9.
            05 wsd_filler15   pic x(3)   value spaces.
            05 wsd_maxcomm    pic zzz,zz9.
            05 wsd_filler16   pic x(3)   value spaces.
            05 wsd_commperc   pic zz9.99.
            05 wsd_filler17   pic x(3)   value spaces.
            05 wsd_earned     pic zzz,zzz,zz9.99.
            05 wsd_filler18   pic x(3)   value spaces.
            05 wsd_paid       pic z,zzz,zz9.99.
            05 wsd_filler19   pic x(3)   value spaces.
            05 wsd_bonus      pic zzz,zz9.99.
            05 wsd_filler20   pic x(5)   value spaces.

      * MATHEMATICAL WORKING STORAGE
        01 ws-math.
            05 MATH-sales pic 9(6).
            05 MATH-mincomm pic 9(8).
            05 MATH-maxcomm pic 9(8).
            05 MATH-commperc pic 9(9).
            05 MATH-bonus pic 9(9)v99.
            05 MATH-bonusEarned pic x(16).
            05 MATH-sales-calculated pic 9(6).
            05 MATH-rate pic 999.
            05 MATH-earned pic 9(9).

        PROCEDURE DIVISION.
        000-MAIN.
      *
      * 100 = INITIALIZATION.
      * 200 = READING FILES.
      * 300 = REPORT HEADER GENERATION.
      * 400 = PROCESS RECORDS.
      * 500 = PAGE PROCESSING.
      * 600 = PAGE FOOTERS.
      * 700 = REPORT FOOTERS.
      * 800 = CLOSE FILES.

            PERFORM 800-PROCESSOR.
            GOBACK.

        100-INITIALIZATION.
            OPEN INPUT INPUT-FILE.
            OPEN OUTPUT OUTPUT-FILE.
            MOVE ws-eof-N TO ws-eof-flag.

        200-READ.
            READ INPUT-FILE
                 AT END MOVE ws-eof-Y TO ws-eof-flag.

        300-HEADER.
            WRITE OUTPUT-LINE.
            WRITE OUTPUT-LINE FROM ws-title.
            WRITE OUTPUT-LINE FROM ws-title2.
            WRITE OUTPUT-LINE.
            WRITE OUTPUT-LINE FROM ws-col-header.
            WRITE OUTPUT-LINE.

        400-DISPLAYER.
            MOVE il-number   TO wsd_number.
            MOVE il-name     TO wsd_name.
            MOVE il-sales    TO wsd_sales.
            MOVE il-mincomm  TO wsd_mincomm.
            MOVE il-maxcomm  TO wsd_maxcomm.
            MOVE il-commperc TO wsd_commperc.

            PERFORM 500-CALCULATION.

            MOVE ws-Earned TO wsd_earned.
            MOVE ws-BONUS TO wsd_bonus.

            MOVE ws-detail TO OUTPUT-LINE.
            WRITE OUTPUT-LINE.

            PERFORM 200-READ.

        500-CALCULATION.
            MOVE il-sales TO MATH-sales.
            MOVE il-commperc TO MATH-commperc.

            COMPUTE MATH-earned ROUNDED =
            (MATH-sales * MATH-commperc) / 100.

            IF (MATH-sales > 300000) THEN
                COMPUTE ws-BONUS ROUNDED =
                (MATH-sales - 300000) * 0.1525
                ADD MATH-earned TO ws-BONUS GIVING ws-Earned
            ELSE
                MOVE MATH-earned TO ws-Earned
                MOVE 0 TO ws-BONUS
            END-IF.

        700-DISPLAY-EARNED.
            MOVE ws-EARNED TO wsd_earned.
            MOVE ws-BONUS TO wsd_bonus.
            WRITE OUTPUT-LINE FROM wsd_earned.
            WRITE OUTPUT-LINE.
            WRITE OUTPUT-LINE FROM wsd_bonus.

        800-PROCESSOR.
            PERFORM 100-INITIALIZATION.
            PERFORM 200-READ.
            PERFORM 300-HEADER.
            PERFORM 400-DISPLAYER
                 UNTIL ws-eof-flag = ws-eof-Y.
            PERFORM 700-DISPLAY-EARNED.
      *   PERFORM 700-RPT-FTR.
      *   PERFORM 800-CLOSE.

        END PROGRAM A3SCOMM.
