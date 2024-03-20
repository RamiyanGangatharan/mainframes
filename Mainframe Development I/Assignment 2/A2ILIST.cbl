j       IDENTIFICATION DIVISION.
       PROGRAM-ID. A2ILIST.
       DATE-WRITTEN. January 24, 2024.
       AUTHOR. Ramiyan Gangatharan.
      *Description: Assignment 2.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       input-output section.
      *
       file-control.
      * input-file declaration
           select input-file
               assign to INFILE
               organization is sequential.
      *
      * output-file declaration
           select output-file
               assign to OUTFILE
               organization is sequential.
      *
       DATA DIVISION.
       FILE SECTION.
      *

       fd input-file
           recording mode is F
           data record is input-line
           record contains 27 characters.
      *
       01 input-line.
           05 il-item  pic x(4).
           05 il-class pic x.
           05 li-desc  pic x(13).
           05 li-qty   pic 999.
           05 li-price pic 9(4)v99.

      *
       fd output-file
           recording mode is F
           data record is output-line
           record contains 108 characters.
      *
       01 output-line  pic X(108).
      *
       WORKING-STORAGE SECTION.
      *
       01 ws-eof-flag   pic x value "N".
       77 ws-eof-Y      pic x value "y".
       77 ws-eof-N      pic x value "N".

       01 ws-title.
           05 ws-filler10 pic x(25) value spaces.
           05 ws-name     pic x(22) value "RAMIYAN GANGATHARAN -".
           05 ws-text     pic x(27) value "MAINFRAME I - ASSIGNMENT II".

       01 ws-columnHeader.
           05 ws-ColItem    pic x(6)    value "ITEM #".
           05 ws-filler1    pic x(2)    value spaces.
           05 ws-ColClass   pic x(5)    value "CLASS".
           05 ws-filler2    pic x(5)    value spaces.
           05 ws-ColDesc    pic x(13)   value "DESCRIPTION".
           05 ws-filler3    pic x(2)    value spaces.
           05 ws-ColQty     pic x(3)    value "QTY".
           05 ws-filler4    pic x(6)    value spaces.
           05 ws-ColUnit    pic x(10)   value "UNIT_PRICE".
           05 ws-filler5    pic x(2)    value spaces.
           05 ws-ColExt     pic x(13)   value "EXT PRICE".
      *    05 ws-filler6    pic x(4)    value spaces.
           05 ws-ColDisc    pic x(12)   value "DISC PRICE".
           05 ws-filler7    pic x(2)    value spaces.
           05 ws-ColNET     pic x(9)    value "NET PRICE".
           05 ws-filler8    pic x(4)    value spaces.
           05 ws-ColTRANS   pic x(5)    value "TRANS".
           05 ws-filler9    pic x(4)    value spaces.
           05 ws-TSPT       pic x(17)   value "TRANSPORT".

       01 ws-detailer.
           05 ws-UItem      pic x(4).
           05 ws-filler1    pic x(2) value spaces.
           05 ws-UClass     pic x(5).
           05 ws-filler2    pic x(2) value spaces.
           05 ws-UDesc      pic x(13).
           05 ws-filler3    pic x(3) value spaces.
           05 ws-UQTY       pic zzz.
           05 ws-filler4    pic x(6) value spaces.
           05 ws-UPrice     pic zzzz.99.
           05 ws-filler5    pic x(4) value spaces.
           05 ws-UEXT       pic zzz,zzz.99.
           05 ws-filler6    pic x(4) value spaces.
           05 ws-UDISC      pic zzz,zzz.99.
           05 ws-filler7    pic x(4) value spaces.
           05 ws-NET        pic zzz,zzz.99.
           05 ws-filler8    pic x(4) value spaces.
           05 ws-TRANSPORT  pic zz9.9.
           05 ws-filler9    pic x(4) value spaces.
           05 ws-TRANSCHG   pic 9(5)v99.

       01 ws-gap.
           05 ws-spacer     pic x(5) value spaces.

       PROCEDURE DIVISION.
       000-main.
      *
           perform 100-open-files.
           perform 200-read-file.
           display SPACES.
           display ws-title.
           display SPACES.
           display ws-columnHeader.
           perform  400-process-recs
                until ws-eof-flag = ws-eof-Y.
           perform 900-close-files.
           goback.
      *

       100-open-files.
           open input input-file.
           open output output-file.
           move ws-eof-N to ws-eof-flag.

       200-read-file.
           read input-file
                at end move ws-eof-Y to ws-eof-flag.

       400-process-recs.
           MOVE il-class    to ws-UClass.
           MOVE li-desc     to ws-UDesc.
           MOVE li-qty      to ws-UQTY.
           MOVE li-price    to ws-UPrice.

           display il-item
           ws-detailer.
           perform 200-read-file.

       900-close-files.
           close input-file, output-file.

       end program A2ILIST.
