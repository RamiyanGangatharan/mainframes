       identification division.
       program-id. A8SL.
       date-written. March 26, 2024.
       author. Christian Weersink.
      *Description: Does some tax math for s and l data
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
           select output-file
               assign to OUTFILE
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
           05 il-code                  pic x.
           05 il-amt                   pic 9(5)v99.
           05 il-pay-type              pic xx.
           05 il-store                 pic 99.
           05 il-invoice               pic x(9).
           05 il-sku                   pic x(15).
      *
       fd output-file
           recording mode is F
           data record is output-line
           record contains 76 characters.
      *
       01 output-line                  pic x(76).
      *
       working-storage section.
      *
       01 ws-header1.
           05 filler                   pic x(18)
                value "Christian Weersink".

       01 ws-header2.
           05 filler                   pic x(30) value spaces.
           05 filler                   pic x(15)
                value "Assignment 8 SL".

       01 ws-header3.
           05 filler                   pic x(69) value spaces.
           05 filler                   pic x(6) value "PAGE #".
           05 ws-page-number           pic 9.

       01 ws-header-columns.
           05 filler                   pic x(4) value "TYPE".
           05 filler                   pic x(3) value spaces.
           05 filler                   pic x(6) value "AMOUNT".
           05 filler                   pic x(3) value spaces.
           05 filler                   pic x(8) value "PAY TYPE".
           05 filler                   pic x(3) value spaces.
           05 filler                   pic x(5) value "STORE".
           05 filler                   pic x(3) value spaces.
           05 filler                   pic x(11) value "INVOICE NUM".
           05 filler                   pic x(8) value spaces.
           05 filler                   pic x(3) value "SKU".
           05 filler                   pic x(10) value spaces.
           05 filler                   pic x(9) value "TAX OWING".

       01 ws-lines.
           05 filler                   pic x(4) value "----".
           05 filler                   pic x(3) value spaces.
           05 filler                   pic x(6) value "------".
           05 filler                   pic x(3) value spaces.
           05 filler                   pic x(8) value "--------".
           05 filler                   pic x(3) value spaces.
           05 filler                   pic x(5) value "-----".
           05 filler                   pic x(3) value spaces.
           05 filler                   pic x(11) value "-----------".
           05 filler                   pic x(8) value spaces.
           05 filler                   pic x(3) value "---".
           05 filler                   pic x(10) value spaces.
           05 filler                   pic x(9) value "---------".



      * FOOTER DETAILS
       01 ws-footer-header.
           05 filler                   pic x(20) value spaces.
           05 filler                   pic x(7) value "RECORDS".
           05 filler                   pic x(7) value spaces.
           05 filler                   pic x(6) value "AMOUNT".

       01 sl-footer.
           05 filler                   pic x(19)
                value "SALES AND LAYAWAYS:".
           05 filler                   pic x(5) value spaces.
           05 ws-sl-records            pic zz9.
           05 filler                   pic x(3) value spaces.
           05 ws-sl-amt                pic $$$,$$9.99.

       01 s-footer.
           05 filler                   pic x(6)
                value "SALES:".
           05 filler                   pic x(3) value spaces.
           05 filler                   pic x(15) value spaces.
           05 ws-s-records             pic zz9.
           05 filler                   pic x(3) value spaces.
           05 ws-s-amt                 pic $$$,$$9.99.

       01 l-footer.
           05 filler                   pic x(9)
                value "LAYAWAYS:".
           05 filler                   pic x(1) value spaces.
           05 filler                   pic x(14) value spaces.
           05 ws-l-records             pic zz9.
           05 filler                   pic x(3) value spaces.
           05 ws-l-amt                 pic $$$,$$9.99.

       01 ca-footer.
           05 filler                   pic x(5) value "CASH:".
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(15) value spaces.
           05 ws-ca-records            pic zz9.
           05 filler                   pic x(7) value spaces.
           05 ws-ca-percent            pic 99.99.
           05 filler                   pic x(1) value "%".

       01 cr-footer.
           05 filler                   pic x(7) value "CREDIT:".
           05 filler                   pic x(2) value spaces.
           05 filler                   pic x(15) value spaces.
           05 ws-cr-records            pic zz9.
           05 filler                   pic x(7) value spaces.
           05 ws-cr-percent            pic 99.99.
           05 filler                   pic x(1) value "%".

       01 db-footer.
           05 filler                   pic x(6) value "DEBIT:".
           05 filler                   pic x(3) value spaces.
           05 filler                   pic x(15) value spaces.
           05 ws-db-records            pic zz9.
           05 filler                   pic x(7) value spaces.
           05 ws-db-percent            pic 99.99.
           05 filler                   pic x(1) value "%".


       01 tax-footer.
           05 filler                   pic x(10) value "TAX TOTAL:".
           05 filler                   pic x(20) value spaces.
           05 ws-tax-total             pic $$$$$$9.99.


       01 high-store.
           05 filler                   pic x(20)
                value "HIGHEST TOTAL STORE:".
           05 filler                   pic x(4) value spaces.
           05 highest-store            pic zz9.
           05 filler                   pic x(1) value spaces.
           05 filler                   pic x(3) value "AT ".
           05 high-amt                 pic $$,$$9.99.

       01 low-store.
           05 filler                   pic x(20)
                value "LOWEST TOTAL STORE: ".
           05 filler                   pic x(4) value spaces.
           05 lowest-store             pic zz9.
           05 filler                   pic x(1) value spaces.
           05 filler                   pic x(3) value "AT ".
           05 low-amt                  pic $$,$$9.99.


      * Detail line


       01 ws-detail.
           05 filler                   pic x(2) value spaces.
           05 ws-code                  pic x.
           05 filler                   pic x(1) value spaces.
           05 ws-amt                   pic $$$$$9.99.
           05 filler                   pic x(6) value spaces.
           05 ws-pay-type              pic x(2).
           05 filler                   pic x(7) value spaces.
           05 ws-store                 pic z9.
           05 filler                   pic x(6) value spaces.
           05 ws-invoice               pic x(9).
           05 filler                   pic x(3) value spaces.
           05 ws-sku                   pic x(15).
           05 filler                   pic x(3) value spaces.
           05 ws-tax                   pic $$$$9.99.





      * Math stuff

       01 ws-line-count                pic 999 value zero.

       01 ws-math.
           05 math-tax                 pic 99999v99 value zero.
           05 TAX_MULTIPLIER           pic 9v99 value 0.13.
           05 total-tax                pic 9(9)v99 value zero.
           05 store-amt                pic 9(9)v99 occurs 6 times.
           05 s-amt                    pic 9(9)v99 value zero.
           05 sl-amt                   pic 9(9)v99 value zero.
           05 l-amt                    pic 9(9)v99 value zero.

           05 high-winner              pic 9(9)v99 value zero.
           05 low-winner               pic 9(9)v99 value 500000.
           05 lowest-store-winner      pic 99.
           05 highest-store-winner     pic 99.

           05 decimal-math             pic 999v9999 value zero.
           05 cr-percent               pic 99v99.
           05 ca-percent               pic 99v99.
           05 db-percent               pic 99v99.

       01 ws-counters.
           05 page-num                 pic 99 value zero.
           05 s-count                  pic 999 value zero.
           05 l-count                  pic 999 value zero.
           05 sl-count                 pic 999 value zero.
           05 ca-count                 pic 999 value zero.
           05 cr-count                 pic 999 value zero.
           05 db-count                 pic 999 value zero.

       01 ws-eof-flag                  pic x value "N".
       01 ws-spaces                    pic x(76) value spaces.

       01 ws-index                     pic 99.


       procedure division.
       000-main.
           open input input-file.
           open output output-file.

           read input-file
                at end move "Y"        to ws-eof-flag.

           perform 100-process until ws-eof-flag = "Y".

           perform 300-footers.

           close input-file.
           close output-file.
      *
           goback.
      *


       100-process.

           add 1 to page-num.
           move page-num to ws-page-number.
           if (page-num > 1) then
                write output-line        after advancing page
                write output-line from ws-header3
                write output-line from ws-spaces
                write output-line from ws-header-columns
                write output-line from ws-lines

           else
                write output-line from ws-header1
                write output-line from ws-header2
                write output-line from ws-header3
                write output-line from ws-spaces
                write output-line from ws-header-columns
                write output-line from ws-lines

           end-if.
           perform 200-process-data
                varying ws-line-count from 1 by 1
                    until (ws-line-count > 20
                        OR ws-eof-flag = "Y").

           write output-line from ws-spaces.



       200-process-data.
           move il-code to ws-code.
           move il-amt to ws-amt.
           move il-pay-type to ws-pay-type.
           move il-store to ws-store.
           move il-invoice to ws-invoice.
           move il-sku to ws-sku.

           add 1 to sl-count.
           add il-amt to sl-amt.


           if(il-code = "S") then
                add 1 to s-count
                add il-amt to s-amt
           else
                add 1 to l-count
                add il-amt to l-amt
           end-if.

           if(il-pay-type = "CA") then
                add 1 to ca-count
           else
           if(il-pay-type = "CR") then
                add 1 to cr-count
           else
           if(il-pay-type = "DB") then
                add 1 to db-count
           end-if
           end-if
           end-if.

           perform varying ws-index from 1 by 1 until ws-index > 6
                if(il-store = ws-index) then
                    add il-amt to store-amt(ws-index)
                else if (il-store = 12 and ws-index = 6) then
                    add il-amt to store-amt(6)
                end-if
                end-if
           end-perform.


           multiply il-amt by TAX_MULTIPLIER giving math-tax rounded.

           add math-tax to total-tax.

           move math-tax to ws-tax.

           write output-line from ws-detail.

           move zeros to math-tax.


           read input-file
                at end move "Y"    to ws-eof-flag.


       300-footers.


           perform varying ws-index from 1 by 1 until ws-index > 6
                if (store-amt(ws-index) > high-winner) then
                    move store-amt(ws-index) to high-winner
                    move ws-index to highest-store-winner
                end-if

                if (store-amt(ws-index) < low-winner) then
                    move store-amt(ws-index) to low-winner
                    move ws-index to lowest-store-winner
                end-if

           end-perform.

           if (lowest-store-winner = 6) then
                move 12 to lowest-store
           else
                move lowest-store-winner to lowest-store
           end-if.

           if (highest-store-winner = 6) then
                move 12 to highest-store
           else
                move highest-store-winner to highest-store
           end-if.

           move high-winner to high-amt.
           move low-winner to low-amt.

           divide cr-count by sl-count giving decimal-math rounded.
           multiply decimal-math by 100 giving cr-percent.
           move cr-percent to ws-cr-percent.
           move cr-count to ws-cr-records.
           move zero to decimal-math.

           divide ca-count by sl-count giving decimal-math rounded.
           multiply decimal-math by 100 giving ca-percent.
           move ca-percent to ws-ca-percent.
           move ca-count to ws-ca-records.
           move zero to decimal-math.

           divide db-count by sl-count giving decimal-math rounded.
           multiply decimal-math by 100 giving db-percent.
           move db-percent to ws-db-percent.
           move db-count to ws-db-records.

           move sl-count to ws-sl-records.
           move sl-amt to ws-sl-amt.

           move s-count to ws-s-records.
           move s-amt to ws-s-amt.

           move l-count to ws-l-records.
           move l-amt to ws-l-amt.


           move total-tax to ws-tax-total.

           write output-line from ws-spaces.

           write output-line from ws-footer-header.
           write output-line from sl-footer.
           write output-line from l-footer.
           write output-line from s-footer.

           write output-line from ws-spaces.

           write output-line from tax-footer.
           write output-line from ws-spaces.

           write output-line from ca-footer.
           write output-line from cr-footer.
           write output-line from db-footer.

           write output-line from ws-spaces.

           write output-line from high-store.
           write output-line from low-store.



       end program A8SL.