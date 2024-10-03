//RD09A2      JOB
//* ----------------------------------------------------------------
//* DELETE OUTPUT FILES FROM PREVIOUS RUNS
//* ----------------------------------------------------------------
//STEP001 EXEC PGM=IEFBR14

//DELERRPT DD DSN=KC03D09.DCMAFD02.A2.EDIT.REPORT.ERROR,
//         DISP=(MOD,DELETE,DELETE),
//         SPACE=(TRK,(5,2))

//DELVALDT DD DSN=KC03D09.DCMAFD02.A2.EDIT.VALID.DATA,
//         DISP=(MOD,DELETE,DELETE),
//         SPACE=(TRK,(5,2))

//DELINVDT DD DSN=KC03D09.DCMAFD02.A2.EDIT.INVALID.DATA,
//         DISP=(MOD,DELETE,DELETE),
//         SPACE=(TRK,(5,2))

//* ----------------------------------------------------------------
//* RUN EDIT PROGRAM TO PROCESS DATA AND CREATE REPORTS
//* ----------------------------------------------------------------
//STEP002 EXEC PGM=EDIT

//STEPLIB  DD DSN=KC03D09.DCMAFD01.COBOL.LOADLIB,
//         DISP=SHR

//INFILE   DD DSN=KC03D09.DCMAFD02.A2.POSDAT1.DATA,
//         DISP=SHR

//RPTFILE  DD DSN=KC03D09.DCMAFD02.A2.EDIT.REPORT.ERROR,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5,2)),
//         RECFM=F,
//         LRECL=125

//VALFILE  DD DSN=KC03D09.DCMAFD02.A2.EDIT.VALID.DATA,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5,2)),
//         RECFM=F,
//         LRECL=36

//INVFILE  DD DSN=KC03D09.DCMAFD02.A2.EDIT.INVALID.DATA,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5,2)),
//         RECFM=F,
//         LRECL=36

//* ----------------------------------------------------------------
//* DELETE SPLIT FILES FROM PREVIOUS RUNS
//* ----------------------------------------------------------------
//STEP003 EXEC PGM=IEFBR14

//DELSPRC  DD DSN=KC03D09.DCMAFD02.A2.SPLIT.REPORT.CONTROL,
//         DISP=(MOD,DELETE,DELETE),
//         SPACE=(TRK,(5,2))

//DELSPRT  DD DSN=KC03D09.DCMAFD02.A2.SPLIT.RETURNS.DATA,
//         DISP=(MOD,DELETE,DELETE),
//         SPACE=(TRK,(5,2))

//DELSSL   DD DSN=KC03D09.DCMAFD02.A2.SPLIT.SL.DATA,
//         DISP=(MOD,DELETE,DELETE),
//         SPACE=(TRK,(5,2))

//* ----------------------------------------------------------------
//* RUN SPLITTER PROGRAM TO SPLIT THE EDIT OUTPUT DATA
//* ----------------------------------------------------------------
//STEP004 EXEC PGM=SPLITTER

//STEPLIB  DD DSN=KC03D09.DCMAFD01.COBOL.LOADLIB,
//         DISP=SHR

//INFILE   DD DSN=KC03D09.DCMAFD02.A2.EDIT.VALID.DATA,
//         DISP=SHR

//RPT      DD DSN=KC03D09.DCMAFD02.A2.SPLIT.REPORT.CONTROL,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5,2)),
//         RECFM=F,
//         LRECL=101

//SL       DD DSN=KC03D09.DCMAFD02.A2.SPLIT.SL.DATA,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5,2)),
//         RECFM=F,
//         LRECL=36

//RET      DD DSN=KC03D09.DCMAFD02.A2.SPLIT.RETURNS.DATA,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5,2)),
//         RECFM=F,
//         LRECL=36

//* ----------------------------------------------------------------
//* DELETE SALES AND LAYAWAYS FILES FROM PREVIOUS RUNS
//* ----------------------------------------------------------------
//STEP005 EXEC PGM=IEFBR14

//DELSLRPT DD  DSN=KC03D09.DCMAFD02.A2.SL.REPORT,
//             DISP=(MOD,DELETE,DELETE),
//             SPACE=(TRK,(5,2))

//* ----------------------------------------------------------------
//* RUN SALES AND LAYAWAYS PROGRAM
//* ----------------------------------------------------------------
//STEP006 EXEC PGM=SL

//STEPLIB  DD DSN=KC03D09.DCMAFD01.COBOL.LOADLIB,
//         DISP=SHR

//INFILE   DD DSN=KC03D09.DCMAFD02.A2.SPLIT.SL.DATA,
//         DISP=SHR

//OUTFILE  DD DSN=KC03D09.DCMAFD02.A2.SL.REPORT,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5,2)),
//         RECFM=F,
//         LRECL=77

//* ----------------------------------------------------------------
//* DELETE RETURNS FILES FROM PREVIOUS RUNS
//* ----------------------------------------------------------------
//STEP007 EXEC PGM=IEFBR14

//DELSPRT DD DSN=KC03D09.DCMAFD02.A2.RETURNS.REPORT,
//        DISP=(MOD,DELETE,DELETE),
//        SPACE=(TRK,(5,2))
//* ----------------------------------------------------------------
//* RUN RETURNS PROGRAM
//* ----------------------------------------------------------------
//STEP008 EXEC PGM=RETURN

//STEPLIB  DD DSN=KC03D09.DCMAFD01.COBOL.LOADLIB,
//         DISP=SHR

//INFILE   DD DSN=KC03D09.DCMAFD02.A2.SPLIT.RETURNS.DATA,
//         DISP=SHR

//OUTFILE  DD DSN=KC03D09.DCMAFD02.A2.RETURNS.REPORT,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5,2)),
//         RECFM=F,
//         LRECL=84

//* ----------------------------------------------------------------
/*
