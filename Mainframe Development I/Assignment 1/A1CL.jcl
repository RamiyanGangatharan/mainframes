//CLD09A1       JOB
//COBOL         EXEC PROC=IGYWCL,
//    PARM.COBOL='TEST,RENT,APOST,OBJECT,NODYNAM,LIB,SIZE(2097152)'
//COBOL.STEPLIB DD DSN=IGY630.SIGYCOMP,
//    DISP=SHR
/* DECLARE DATASET THAT CONTAINS SOURCE CODE
//COBOL.SYSIN  DD DSN=KC03D09.DCMAFD01.A1.COBOL(A1CLIST),
//    DISP=SHR
/*
/* DECLARE PDS MEMBER TO STORE LOAD MODULE
//LKED.SYSLMOD  DD DSN=KC03D09.DCMAFD01.COBOL.LOADLIB(A1CLIST),
//    DISP=OLD
/*
