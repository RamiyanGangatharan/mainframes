//RD09A6      JOB
/*
/* SET OUTFILE DD NAME TO MATCH YOUR ASSIGN NAME FOR FILE
/* SET LRECL 108 FOR OUTFILE TO MATCH YOUR OUTPUT RECORD SIZE
/*
//A6R        EXEC PGM=A6EDIT

//STEPLIB      DD DSN=KC03D09.DCMAFD01.COBOL.LOADLIB,
//    DISP=SHR

//INFILE       DD DSN=KC03D09.DCMAFD01.A6.POS1.DATA,
//    DISP=SHR

//OUTFILE      DD DSN=KC03D09.DCMAFD01.A6.ERPT.OUT,
//    DISP=(NEW,CATLG,DELETE),
//    SPACE=(TRK,(3,1)),
//    RECFM=F,
//    LRECL=108

//VALID        DD DSN=KC03D09.DCMAFD01.A6.VAL.DATA,
//    DISP=(NEW,CATLG,DELETE),
//    SPACE=(TRK,(3,1)),
//    RECFM=F,
//    LRECL=108

//INVALID      DD DSN=KC03D09.DCMAFD01.A6.INV.DATA,
//    DISP=(NEW,CATLG,DELETE),
//    SPACE=(TRK,(3,1)),
//    RECFM=F,
//    LRECL=108
/*
