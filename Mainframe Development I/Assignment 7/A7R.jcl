//RD09A7      JOB
/*
/* SET OUTFILE DD NAME TO MATCH YOUR ASSIGN NAME FOR FILE
/* SET LRECL 108 FOR OUTFILE TO MATCH YOUR OUTPUT RECORD SIZE
/*
//A7R        EXEC PGM=A7SPLIT
//STEPLIB      DD DSN=KC03D09.DCMAFD01.COBOL.LOADLIB,
//    DISP=SHR

//INFILE       DD DSN=KC03D09.DCMAFD01.A6.VAL.DATA,
//    DISP=SHR

//OUTFILE      DD DSN=KC03D09.DCMAFD01.A7.RPT.OUT,
//    DISP=(NEW,CATLG,DELETE),
//    SPACE=(TRK,(2,1)),
//    RECFM=F,
//    LRECL=108

//SOUTFILE DD DSN=KC03D09.DCMAFD01.A7.SLOUT.DATA,
//    DISP=(NEW,CATLG,DELETE),
//    SPACE=(TRK,(2,1)),
//    RECFM=F,
//    LRECL=108

//ROUTFILE  DD DSN=KC03D09.DCMAFD01.A7.ROUT.DATA,
//    DISP=(NEW,CATLG,DELETE),
//    SPACE=(TRK,(2,1)),
//    RECFM=F,
//    LRECL=108

/*
