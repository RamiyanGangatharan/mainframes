//RD09A2      JOB
/*
/* SET OUTFILE DD NAME TO MATCH YOUR ASSIGN NAME FOR FILE
/* SET LRECL 145 FOR OUTFILE TO MATCH YOUR OUTPUT RECORD SIZE
/*
//A4R        EXEC PGM=A4SALRPT
//STEPLIB      DD DSN=KC03D09.DCMAFD01.COBOL.LOADLIB,
//    DISP=SHR
//INFILE       DD DSN=KC03D09.DCMAFD01.A4.SALRPT.DATA,
//    DISP=SHR
//OUTFILE       DD DSN=KC03D09.DCMAFD01.A4.SALRPT.OUT,
//    DISP=(NEW,CATLG,DELETE),
//    SPACE=(TRK,(2,1)),
//    RECFM=F,
//    LRECL=145
/*
