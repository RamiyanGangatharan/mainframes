       IDENTIFICATION DIVISION.
       PROGRAM-ID. A1CLIST.
       DATE-WRITTEN. January 17, 2024.
       AUTHOR. Ramiyan Gangatharan.
      *Description: Assignment 1
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ws-title.
           05 ws-filler1 pic   x(7)  value spaces.
           05 ws-text pic      x(24) value "Mainframe I Contact List".
           05 ws-filler2 pic   x(7)  value spaces.
       01 ws-columnHeader.
           05 ws-HeadName pic  x(25) value "FULL NAME".
           05 ws-HeadEmail pic x(27) value "EMAIL ADDRESS".
           05 ws-HeadPhone pic x(22) value "PHONE NUMBER".
       01 ws-attributes.
           05 ws-name pic      x(20).
           05 ws-email pic     x(32).
           05 ws-phone pic     x(30).

       PROCEDURE DIVISION.
       000-MAIN.
           display ws-title.
           display spaces.
           display ws-columnHeader.
               move "Gregory Oakes"                  to ws-Name.
               move "Gregory.Oakes@durhamcollege.ca" to ws-email.
               move "905-321-1234"                   to ws-phone.
           display ws-attributes.
               move "Mary Bell"                      to ws-Name.
               move "Mary.Bell@durhamcollege.ca"     to ws-email.
               move "416-733-2342"                   to ws-phone.
           display ws-attributes.
               move "Ed Bigalo"                      to ws-Name.
               move "Ed.Bigalo@durhamcollege.ca"     to ws-email.
               move "289-230-1231"                   to ws-phone.
           display ws-attributes.
               move "Ramiyan G"                      to ws-Name.
               move "ramiyan.gangatharan@dcmail.ca"  to ws-email.
               move "565-284-8543"                   to ws-phone.
           display ws-attributes.
               move "Kuldeep M"                      to ws-Name.
               move "kuldeep.mohanta@dcmail.ca"      to ws-email.
               move "342-321-5322"                   to ws-phone.
           display ws-attributes.
               move "George Bush"                    to ws-Name.
               move "GBush@presidential.ca"          to ws-email.
               move "887-128-9926"                   to ws-phone.
           display ws-attributes.
               move "Dominic Byrnes"                 to ws-Name.
               move "Byrnes_D@Timmies.ca"            to ws-email.
               move "281-553-2387"                   to ws-phone.
           display ws-attributes.

           GOBACK.
       END PROGRAM A1CLIST.
