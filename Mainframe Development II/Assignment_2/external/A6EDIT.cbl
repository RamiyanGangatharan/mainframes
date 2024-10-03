       identification division.
       program-id. A6EDIT.
       date-written. 03-06-2024.
       author. Meng Cai.
      *Description:input data, output invalid, valid data and report
      *
       environment division.
       configuration section.
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
      *
           select report-file
               assign to RPTFILE
               organization is sequential.
      *
           select valid-file
               assign to VALFILE
               organization is sequential.
      *
           select invalid-file
               assign to INVFILE
               organization is sequential.
      *
       data division.
       file section.
      *
       fd input-file
           recording mode is F
           data record is input-line
           record contains 36 characters.
      *
       01 input-line.
           05 il-trans-code            pic x.
           05 il-trans-amount          pic 9(5)v99.
           05 il-pay-type              pic xx.
           05 il-store-num             pic 99.
           05 il-invoice-num           pic x(9).
           05 il-sku-code              pic x(15).
      *
       fd report-file
           recording mode is F
           data record is report-line
           record contains 125 characters.
      *
       01 report-line                  pic x(125).

       fd valid-file
           recording mode is F
           data record is valid-line
           record contains 36 characters.
      *
       01 valid-line                   pic x(36).
      *
       fd invalid-file
           recording mode is F
           data record is invalid-line
           record contains 36 characters.
      *
       01 invalid-line                  pic x(36).
      *
      *

      *
       working-storage section.
      *
       01 ws-name-line.
           05 filler                   pic x(5)
               value spaces.
           05 filler                   pic x(25)
               value '    Meng Cai    '.
      *               ----+----1----+----2----+
           05 filler                   pic x(29)
               value '                 Assignment 6'.
      *               ----+----1----+----2----+----
           05 filler                   pic x(5)
               value spaces.
           05 ws-name-line-date        pic 9(6).
           05 filler                   pic x(4)
               value spaces.
           05 ws-name-line-time        pic 9(8).
           05 filler                   pic x(50)
               value spaces.
      *
      *
       01 ws-heading-1.
           05 filler                        pic x(40)
               value "                           S U M M A R Y".
      *               ----+----1----+----2----+----3----+----4
           05 filler                        pic x(40)
               value "  &  E R R O R   R E P O R T            ".
      *               ----+----5----+----6----+----7----+----8
           05 filler                        pic x(29)
               value "                        Page ".
      *               ----+----9----+----0----+----
           05 ws-heading-1-page-num         pic z9.
           05 filler                        pic x(9)
               value spaces.
      *
      *
       01 ws-heading-2.
           05 filler                        pic x(40)
               value "    Raw Input Data          ".
      *               ----+----1----+----2----+----3----+----4
           05 filler                        pic x(40)
               value "           E r r or   M e s s a g e".
      *               ----+----5----+----6----+----7----+----8

      *
       01 ws-heading-3.
           05 filler                        pic x(40)
               value "------------------------------------".
      *               ----+----1----+----2----+----3----+----4
           05 filler                        pic x(40)
               value "   -------------------------------".
      *               ----+----5----+----6----+----7----+----8

      *
       01 ws-blank-line                     pic x(120)
           value spaces.
       01 ws-summary.
           05 filler                        pic x(24)
               value "Number of Records   =   ".
      *               ----+----1----+----2----
           05 ws-sum-record-count           pic zz9.
           05 filler                        pic x(10)
               value spaces.
           05 filler                        pic x(20)
               value "Valid Records   =   ".
      *               ----+----1----+----2
           05 ws-sum-valid-records          pic zz9.
           05 filler                        pic x(10)
               value spaces.
           05 filler                        pic x(20)
               value "Invalid Records =   ".
      *               ----+----1----+----2
           05 ws-sum-invalid-records        pic zz9.
           05 filler                        pic x(27)
               value spaces.
      *
       01 ws-error-text-cnst.
           05 ws-error-text-1-cnst          pic x(40)
               value "Transaction Code not S, R or L".
           05 ws-error-text-2-cnst          pic x(40)
               value "Transaction Amount not numeric".
           05 ws-error-text-3-cnst          pic x(40)
               value "Payment Type not CA, CR or DB".
           05 ws-error-text-4-cnst          pic x(40)
               value "Store Number not 01,02,03,04,05,or 12".
           05 ws-error-text-5-cnst          pic x(40)
               value "Invoice Number XX not alphabetic".
           05 ws-error-text-6-cnst          pic x(40)
               value "Invoice Number last 6 not numeric ".
           05 ws-error-text-7-cnst          pic x(40)
               value "Invoice Num XX not A,B,C,D or E".
           05 ws-error-text-8-cnst          pic x(40)
               value "Invoice Number XX same 2 letters".
           05 ws-error-text-9-cnst          pic x(40)
               value "Invoice Number last 6 >900000 or <100000".
           05 ws-error-text-10-cnst         pic x(40)
               value "SKU Code has space".
           05 ws-error-text-11-cnst         pic x(40)
               value "Invoice Number 3 miss - ".
      *
      *
       01 ws-detail-line.
           05 ws-dl-input-line              pic x(40)
               value spaces.
           05 filler                        pic x(3)
               value spaces.
           05 ws-dl-error-text              pic x(40)
               value spaces.
           05 filler                        pic x(11)
               value spaces.
      *
      *
       77 ws-eof-flag        pic x
           value "n".

       01 ws-counters.
           05 ws-record-count               pic 999
               value 0.
           05 ws-valid-record-count         pic 999
               value 0.
           05 ws-invalid-record-count       pic 999
               value 0.

      *
       procedure division.
       000-main.
      * open files
           open input  input-file.
           open output report-file,
                       valid-file,
                       invalid-file.
      *
      * read initial record from input-file
           read input-file
               at end
                   move "y" to ws-eof-flag.
      *
           perform 30-output-report-record.
      * iterate through all input lines
           perform 20-process-lines
               until ws-eof-flag = "y".
      *


           perform 60-output-record.
      * close files
           close input-file,
                 report-file,
                 valid-file,
                 invalid-file.
      *
           goback.
      *
      *
       20-process-lines.
           add 1 to ws-record-count.

      *
      *    add 1 to ws-record-count.
      *    if ws-eof-flag = "y"
      *       perform 30-output-report-record


      * write to file based on TRANS TYPE
           if il-trans-code not = "S" and
              il-trans-code not = "R" and
              il-trans-code not = "L" then
                move ws-error-text-1-cnst to ws-dl-error-text


           else

           if il-trans-amount is not NUMERIC then
                move ws-error-text-2-cnst to ws-dl-error-text

           else

           if il-pay-type not = "CA" and
              il-pay-type not = "CR" and
              il-pay-type not = "DB" then
                move ws-error-text-3-cnst to ws-dl-error-text

           else

           if il-store-num not = 01 and
              il-store-num not = 02 and
              il-store-num not = 03 and
              il-store-num not = 04 and
              il-store-num not = 05 and
              il-store-num not = 12 then
                move ws-error-text-4-cnst to ws-dl-error-text

           else

           if il-invoice-num(1:2) is not ALPHABETIC then
                move ws-error-text-5-cnst to ws-dl-error-text

           else

           if il-invoice-num(4:6) is not NUMERIC then
                move ws-error-text-6-cnst to ws-dl-error-text

           else

           if il-invoice-num(1:1) not = "A" and
              il-invoice-num(1:1) not = "B" and
              il-invoice-num(1:1) not = "C" and
              il-invoice-num(1:1) not = "D" and
              il-invoice-num(1:1) not = "E" then
                move ws-error-text-7-cnst to ws-dl-error-text

           else

           if il-invoice-num(2:1) not = "A" and
              il-invoice-num(2:1) not = "B" and
              il-invoice-num(2:1) not = "C" and
              il-invoice-num(2:1) not = "D" and
              il-invoice-num(2:1) not = "E" then
                move ws-error-text-7-cnst to ws-dl-error-text

           else

           if il-invoice-num(1:1) = il-invoice-num(2:1) then
                move ws-error-text-8-cnst to ws-dl-error-text

           else

           if il-invoice-num(4:6)> 900000 or
              il-invoice-num(4:6)< 100000 then
                move ws-error-text-9-cnst to ws-dl-error-text

           else

           if il-invoice-num(3:1) not = "-" then
                move ws-error-text-11-cnst to ws-dl-error-text

           else

           if length of il-sku-code not = 15 then
                move ws-error-text-10-cnst to ws-dl-error-text

           end-if
           end-if
           end-if
           end-if
           end-if
           end-if
           end-if
           end-if
           end-if
           end-if
           end-if
           end-if.


      *

      * write to file based on TRANS TYPE
           if (il-trans-code = "S" or
              il-trans-code = "R" or
              il-trans-code = "L" )and
              il-trans-amount is NUMERIC and
              (il-pay-type = "CA" or
              il-pay-type = "CR" or
              il-pay-type = "DB") and
              (il-store-num = 01 or
              il-store-num = 02 or
              il-store-num = 03 or
              il-store-num = 04 or
              il-store-num = 05 or
              il-store-num = 12) and
              il-invoice-num(1:2) is ALPHABETIC and
              il-invoice-num(4:6) is NUMERIC and
              (il-invoice-num(1:1) = "A" or
              il-invoice-num(1:1) = "B" or
              il-invoice-num(1:1) = "C" or
              il-invoice-num(1:1) = "D" or
              il-invoice-num(1:1) = "E") and
              (il-invoice-num(2:1) = "A" or
              il-invoice-num(2:1) = "B" or
              il-invoice-num(2:1) = "C" or
              il-invoice-num(2:1) = "D" or
              il-invoice-num(2:1) = "E") and
              il-invoice-num(1:1) not = il-invoice-num(2:1) and
              il-invoice-num(4:6) <= 900000 and
              il-invoice-num(4:6) >= 100000 and
              il-invoice-num(3:1) = "-" and
              il-sku-code(15:1) not = space then
                perform 40-output-valid-record
                add 1 to ws-valid-record-count
           else
                perform 50-output-invalid-record
                add 1 to ws-invalid-record-count
                move input-line       to ws-dl-input-line

                write report-line from ws-detail-line
                write report-line from ws-blank-line
           end-if.


      *




      * read next input-file record
           read input-file
               at end
                   move "y" to ws-eof-flag.
      *
       30-output-report-record.

           write report-line from ws-name-line.

           write report-line from ws-blank-line.

           write report-line from ws-heading-1.

           write report-line from ws-blank-line.

           write report-line from ws-heading-2.

           write report-line from ws-heading-3.

      *    write report-line from ws-blank-line.



       40-output-valid-record.

           write valid-line from input-line.
      *    add 1 to ws-valid-record-count.


       50-output-invalid-record.
      *
           write invalid-line from input-line.

           move input-line       to ws-dl-input-line.



      *
       60-output-record.

           move ws-valid-record-count to ws-sum-valid-records.

           move ws-invalid-record-count to ws-sum-invalid-records.

           move ws-record-count to ws-sum-record-count.
      *
           write report-line from ws-blank-line.
      *
      *    write report-line from ws-detail-line.
      *
           write report-line from ws-summary.

       end program A6EDIT.