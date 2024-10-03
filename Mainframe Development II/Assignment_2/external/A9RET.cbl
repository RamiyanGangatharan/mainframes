       identification division.
       program-id. A9RET.
       date-written. 01/04/2024.
       author. Christina Jackson.
      *Description: Analyzes returns data for each store and displays
      * tax owed and totals.

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

      * Input line variables
       01 input-line.
           05 il-tcode                 pic x.
           05 il-tamt                  pic 9(5)v99.
           05 il-ptype                 pic xx.
           05 il-stnum                 pic 99.
           05 il-invnum                pic x(9).
           05 il-sku                   pic x(15).
      *
       fd output-file
           recording mode is F
           data record is output-line
           record contains 83 characters.
      *
       01 output-line                  pic x(83).
      *
       working-storage section.

      * Title with name and assignment
       01 ws-heading-name.
           05 filler                   pic x(52)
                value spaces.
           05 filler                   pic x(31)
                value "Assignment 9: Christina Jackson".

      * Title of assignment
       01 ws-heading-title.
           05 filler                   pic x(30)
                value spaces.
           05 filler                   pic x(14)
                value "Returns Report".
            05 filler                  pic x(38)
                value "                                 Page ".
            05 ws-page-no              pic 9.

      * First line of columns
       01 ws-heading-column1.
           05 filler                   pic x(30)
                value "Store   Trans     Trans       ".
           05 filler                   pic x(26)
                value "Payment     Invoice       ".

      * Second line of columns
       01 ws-heading-column2.
           05 filler                   pic x(31)
                value "  #      Code     Amount       ".
           05 filler                   pic x(28)
                value "Type        Number          ".
           05 filler                   pic x(18)
                value "SKU            Tax".

      * Third line of columns
       01 ws-heading-lines.
           05 filler                   pic x(29)
                value "-----   -----    --------    ".
           05 filler                   pic x(27)
                value "---------   ---------      ".
           05 filler                   pic x(23)
                value "---------       -------".

      * Blank line for spacing
       01 ws-blank-line.
           05 filler                   pic x(77)
                value spaces.

      * End of file flag
       01 ws-eof                       pic x
           value "N".
           88 ws-end-of-file           value "Y".

      * Constants
       77 ws-lines-per-page            pic 99
           value 20.
       77 ws-page-count                pic 99
           value 0.
       77 ws-line-count                pic 99
           value 0.
       77 ws-num-stores                pic 9
           value 6.
       77 ws-tax                       pic 9v99
           value 0.13.

      * Calculation variables
        01 ws-calculations.
           05 ws-index                 pic 9
                value 0.
           05 calc-each-store          pic 99999v99 occurs 6 times.
           05 calc-total-returns       pic 99999v99.
           05 calc-tot-tax             pic 9999v99.
           05 calc-total-tax           pic 99999v99.
           05 calc-amount              pic 99999v99.

      * Counters
       01 ws-counters.
           05 ws-ret-count             pic 99 occurs 6 times.
           05 ws-tot-count             pic 99.


      * Output line
       01 ws-detail-line.
           05 filler                   pic x
                value spaces.
           05 ws-dl-sn                 pic xx.
           05 filler                   pic x(7)
                value spaces.
           05 ws-dl-tc                 pic x.
           05 filler                   pic x(5)
                value spaces.
           05 ws-dl-ta                 pic $$$$$9.99.
           05 filler                   pic x(7)
                value spaces.
           05 ws-dl-pt                 pic xx.
           05 filler                   pic x(7)
                value spaces.
           05 ws-dl-in                 pic x(9).
           05 filler                   pic x(3)
                value spaces.
           05 ws-dl-sku                pic x(15).
           05 filler                   pic xx
                value spaces.
           05 ws-dl-tax                pic $$$$$9.99.

      * Footers
       01 ws-footer-1.
           05 filler                   pic x(26)
                value spaces.
           05 filler                   pic x(23)
                value "Total Returns per Store".

       01 ws-footer-2.
           05 filler                   pic x(25)
                value spaces.
           05 filler                   pic x(25)
                value "-------------------------".

      * Store 1 calculations
       01 ws-footer-3.
           05 filler                   pic x(25)
                value spaces.
           05 filler                   pic x(11)
                value "Store 1    ".
           05 ws-ret-st1               pic z9.
           05 filler                   pic x(4)
                value " at ".
           05 ws-ret-st-amt1           pic $$$9.99.

      * Store 2 calculations
       01 ws-footer-4.
           05 filler                   pic x(25)
                value spaces.
           05 filler                   pic x(11)
                value "Store 2    ".
           05 ws-ret-st2               pic z9.
           05 filler                   pic x(4)
                value " at ".
           05 ws-ret-st-amt2           pic $$$9.99.

      * Store 3 calculations
       01 ws-footer-5.
           05 filler                   pic x(25)
                value spaces.
           05 filler                   pic x(11)
                value "Store 3    ".
           05 ws-ret-st3               pic z9.
           05 filler                   pic x(4)
                value " at ".
           05 ws-ret-st-amt3           pic $$$9.99.

      * Store 4 calculations
       01 ws-footer-6.
           05 filler                   pic x(25)
                value spaces.
           05 filler                   pic x(11)
                value "Store 4    ".
           05 ws-ret-st4               pic z9.
           05 filler                   pic x(4)
                value " at ".
           05 ws-ret-st-amt4           pic $$$9.99.

      * Store 5 calculations
       01 ws-footer-7.
           05 filler                   pic x(25)
                value spaces.
           05 filler                   pic x(11)
                value "Store 5    ".
           05 ws-ret-st5               pic z9.
           05 filler                   pic x(4)
                value " at ".
           05 ws-ret-st-amt5           pic $$$9.99.

      * Store 12 calculations
       01 ws-footer-8.
           05 filler                   pic x(25)
                value spaces.
           05 filler                   pic x(11)
                value "Store 12   ".
           05 ws-ret-st12              pic z9.
           05 filler                   pic x(4)
                value " at ".
           05 ws-ret-st-amt12          pic $$$9.99.

      * Total returns line
       01 ws-footer-9.
           05 filler                   pic x(25)
                value "Total Returns:           ".
           05 ws-tot-ret               pic 99.
           05 filler                   pic x(4)
                value " at ".
           05 ws-tot-ret-amt           pic $$$9.99.

      * Total tax line
       01 ws-footer-10.
           05 filler                   pic x(25)
                value "Total Tax Owed:          ".
           05 ws-tot-tax               pic $$$.99.



       procedure division.
       000-main.
      *

      * Open files
           perform 100-open-files.

      * Read files and perform calculations
           perform 200-print-info
                until ws-end-of-file.

      * Perform final calculations and write footers
           perform 400-footer-calcs.

      * Close files
           close input-file
                 output-file.


           goback.
      *

      * Opens files
       100-open-files.
           open input input-file.
           open output output-file.

           read input-file
                   at end move "Y"     to ws-eof.

      * Prints headings and process stores
       200-print-info.
           perform 210-print-headings.

           perform 300-process-stores
                varying ws-line-count from 1 by 1
                until (ws-line-count > ws-lines-per-page
                    OR ws-end-of-file).

      * Prints headings depending on what page it is currently on
      * There's only one page so it will only need one set of headings
       210-print-headings.
           add 1                       to ws-page-count.
           move spaces                 to output-line.
           move ws-page-count          to ws-page-no.

           if (ws-page-count > 1) then
                write output-line
                    after advancing page

                write output-line
                write output-line      from ws-heading-title
                write output-line      from ws-blank-line
                write output-line      from ws-heading-column1
                write output-line      from ws-heading-column2
                write output-line      from ws-heading-lines
                write output-line      from ws-blank-line
                move spaces            to output-line

           else
                write output-line
                write output-line      from ws-heading-name
                write output-line      from ws-blank-line
                write output-line      from ws-heading-title
                write output-line      from ws-blank-line
                write output-line      from ws-heading-column1
                write output-line      from ws-heading-column2
                write output-line      from ws-heading-lines
                write output-line      from ws-blank-line

           end-if.


      * Store information processing
       300-process-stores.

      * Sort Return amounts per store
           perform
                varying ws-index
                    from 1 by 1

                    until ws-index > ws-num-stores

                   if (il-stnum = 1) and (ws-index = 1) then
                    add il-tamt to calc-each-store(ws-index)
                    add 1 to ws-ret-count(ws-index)
                    add il-tamt to calc-total-returns

                   else if (il-stnum = 2) and (ws-index = 2) then
                    add il-tamt to calc-each-store(ws-index)
                    add 1 to ws-ret-count(ws-index)
                    add il-tamt to calc-total-returns

                   else if (il-stnum = 3) and (ws-index = 3) then
                    add il-tamt to calc-each-store(ws-index)
                    add 1 to ws-ret-count(ws-index)
                    add il-tamt to calc-total-returns

                   else if (il-stnum = 4) and (ws-index = 4) then
                    add il-tamt to calc-each-store(ws-index)
                    add 1 to ws-ret-count(ws-index)
                    add il-tamt to calc-total-returns

                   else if (il-stnum = 5) and (ws-index = 5) then
                    add il-tamt to calc-each-store(ws-index)
                    add 1 to ws-ret-count(ws-index)
                    add il-tamt to calc-total-returns

                   else if (il-stnum = 12) and (ws-index = 6) then
                    add il-tamt to calc-each-store(ws-index)
                    add 1 to ws-ret-count(ws-index)
                    add il-tamt to calc-total-returns

              end-if
              end-if
              end-if
              end-if
              end-if
              end-if


           end-perform.

      * Move inline information to detail line variables for display
           move il-stnum               to ws-dl-sn.
           move il-tcode               to ws-dl-tc.
           move il-tamt                to ws-dl-ta.
           move il-ptype               to ws-dl-pt.
           move il-invnum              to ws-dl-in.
           move il-sku                 to ws-dl-sku.


           add 1 to ws-tot-count.

      * Tax calculation
           move il-tamt to calc-amount.
           multiply calc-amount by ws-tax giving calc-tot-tax rounded.

           add calc-tot-tax to calc-total-tax.
           move calc-tot-tax to ws-dl-tax.

           write output-line           from ws-detail-line.

           read input-file
                   at end move "Y"     to   ws-eof.




      * Footer

        400-footer-calcs.

      * Each store's return counts
           perform
                varying ws-index
                    from 1 by 1

                    until ws-index > ws-num-stores

                 if (ws-index = 1) then
                    move calc-each-store(ws-index) to
                        ws-ret-st-amt1
                    move ws-ret-count(ws-index) to
                        ws-ret-st1

                 else if (ws-index = 2) then
                    move calc-each-store(ws-index) to
                        ws-ret-st-amt2
                    move ws-ret-count(ws-index) to
                        ws-ret-st2

                 else if (ws-index = 3) then
                    move calc-each-store(ws-index) to
                        ws-ret-st-amt3
                    move ws-ret-count(ws-index) to
                        ws-ret-st3

                 else if (ws-index = 4) then
                    move calc-each-store(ws-index) to
                        ws-ret-st-amt4
                    move ws-ret-count(ws-index) to
                        ws-ret-st4

                 else if (ws-index = 5) then
                    move calc-each-store(ws-index) to
                        ws-ret-st-amt5
                    move ws-ret-count(ws-index) to
                        ws-ret-st5

                 else if (ws-index = 6) then
                    move calc-each-store(ws-index) to
                        ws-ret-st-amt12
                    move ws-ret-count(ws-index) to
                        ws-ret-st12

                 end-if
                 end-if
                 end-if
                 end-if
                 end-if
                 end-if


            end-perform.


      * Totals for returns and tax owed
           move ws-tot-count to ws-tot-ret.
           move calc-total-returns to ws-tot-ret-amt.
           move calc-total-tax to ws-tot-tax.



      * Final footer writes
           write output-line from ws-blank-line.
           write output-line from ws-footer-1.
           write output-line from ws-footer-2.
           write output-line from ws-footer-3.
           write output-line from ws-footer-4.
           write output-line from ws-footer-5.
           write output-line from ws-footer-6.
           write output-line from ws-footer-7.
           write output-line from ws-footer-8.
           write output-line from ws-blank-line.
           write output-line from ws-footer-9.
           write output-line from ws-footer-10.




       end program A9RET.