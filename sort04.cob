       IDENTIFICATION DIVISION.
       PROGRAM-ID. sort04.
      * A GnuCOBOL program
      * On: 03/14/2019
      * By: Bill Blasingim      
      *
      * This program uses the COBOL SORT.
      * Sorts in STATE, CITY order.
      *
      * Print totals per state [STATE control break]
      *
      * Totalled an amount field.
      * Just a review because I'm rusty at this...
      *    You can't add [pic 999.99] to [PIC 9(7)v99 value 0 comp-3]
      *    But you can
      *    Move [pic 999.99] to [pic 999v99]
      *        and then add that to PIC 9(7)v99 value 0 comp-3]
      *
      * 03/18/2019 Modified report to use AFTER ADVANCING PAGE for
      *            Heading.
      * 03/20/2019 Add basic ON SIZE ERROR message for calculations.
      *
       ENVIRONMENT DIVISION.      
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT INFILE ASSIGN TO
      *   "/home/bill/Mystuff/COBOL/data/customer-fixed.txt"
         "/home/bill/Mystuff/COBOL/data/customer-500000.txt"             
      *  "/home/bill/Mystuff/COBOL/data/customer-million.txt" 
         LINE SEQUENTIAL.
       SELECT PRINTFILE ASSIGN TO PRINTER
           ORGANIZATION LINE SEQUENTIAL.
       SELECT SORT-FILE  ASSIGN TO "./srtwork.fil".
       DATA DIVISION.
       FILE SECTION.
       FD INFILE.
         01 INREC.
           05 ACCOUNT		PIC X(17).
           05 FILLER		PIC X(10).
           05 I-NAME.
             10 I-FIRST		PIC X(15).
             10 I-MIDDLE	PIC X(15).
             10 I-LAST		PIC X(25).
           05 FILLER		PIC X(8).                        
           05 I-BIRTHDAY.
              10 YYYY		PIC X(4).
              10 FILLER		PIC X.
              10 MM			PIC X(2).
              10 FILLER		PIC X.              
              10 DD			PIC X(2).   
           05 I-ED.
              10 E-YYYY		PIC X(4).
              10 FILLER		PIC X.
              10 E-MM		PIC X(2).
              10 FILLER		PIC X.              
              10 E-DD		PIC X(2).  
            05 I-AMOUNT     PIC 999.99.
            05 FILLER REDEFINES I-AMOUNT.
               10 I-DOLLARS PIC 999.
               10 FILLER    PIC X.
               10 I-CENTS   PIC 99.  
            05 I-ADDRESS    PIC X(20).
            05 I-CITY	    PIC X(20).
            05 I-STATE	    PIC X(2).
            05 I-ZIP	    PIC X(5). 

       FD PRINTFILE.
         01 OUTREC.
           05 O-NAME.   
             10 O-LAST         PIC X(20).                  
             10 O-FIRST        PIC X(15).
             10 O-MIDDLE       PIC X(15).
           05 O-BIRTHDAY.
              10 O-YYYY        PIC X(4).
              10 FILLER        PIC X.
              10 O-MM          PIC X(2).
              10 FILLER        PIC X.
              10 O-DD          PIC X(2). 
            05 FILLER          PIC X.
            05 O-CITY          PIC X(21).
            05 O-STATE         PIC XX.
            05 FILLER          PIC X.
            05 O-ZIP           PIC X(5).
            05 FILLER          PIC X.            
            05 O-AMOUNT        PIC $$$9.99.              
            05 FILLER          PIC X.            

       SD  SORT-FILE.
       01  SORT-RECORD.
           05  SRT-NAME.
             10 S-FIRST        PIC X(15).
             10 S-MIDDLE       PIC X(15).
             10 S-LAST         PIC X(20).           
           05 SRT-BIRTHDAY     PIC X(10).
           05 SRT-AMOUNT       PIC 999V99.
           05 SRT-CITY	       PIC X(20).
           05 SRT-STATE        PIC X(2).
           05 SRT-ZIP	       PIC X(5). 

       WORKING-STORAGE SECTION.
         01 MISC.
           88 EOF     VALUE "Y".    
      *    LINUX END OF LINE [LINE FEED]
           05 EOL    BINARY-CHAR UNSIGNED VALUE 10.  
           05  EOF-FLAG    PIC X(01) VALUE 'N'.
               88  EOF2            VALUE 'Y'. 
           05 LAST-STATE   PIC XX VALUE SPACES.               
           05 RECIN        PIC 9(7) VALUE 0 COMP-3.            
           05 LINE-CNT     PIC 9(7) VALUE 0 COMP-3.            
           05 TOT-AMT      PIC 9(7)V99 VALUE 0 COMP-3.
           05 GRAND-TOT-AMT    PIC 9(9)V99 VALUE 0 COMP-3.           
           05 WS-AMOUNT    PIC 999V99.
           05 FILLER REDEFINES WS-AMOUNT.
               10 WS-DOLLARS   PIC 999.
               10 WS-CENTS     PIC 99.                  

         01 HEADING-1.
            05 FILLER  PIC X(20) VALUE SPACES.
            05 FILLER  PIC X(35) VALUE 
            "Customer Report (Sort: STATE, CITY)".
            05 FILLER  PIC X(40) VALUE SPACES.
            05 FILLER  PIC X(03) VALUE SPACES.

         01 TOTAL-LINE.
            05 FILLER         PIC X(73) VALUE SPACES.
            05 GND             PIC X(06) VALUE SPACES.
            05 FILLER         PIC X(06) VALUE "TOTAL ".
            05 T-AMOUNT         PIC $$$,$$$,$$9.99.      

       PROCEDURE DIVISION.

           SORT SORT-FILE
                ASCENDING KEY  SRT-STATE, SRT-CITY
                INPUT PROCEDURE SRT-INPUT-PROCEDURE
                OUTPUT PROCEDURE SRT-OUTPUT-PROCEDURE.

           MOVE SPACES TO OUTREC.
           WRITE OUTREC.

           PERFORM STATE-BREAK-RTN.
           
           MOVE GRAND-TOT-AMT TO T-AMOUNT.
           MOVE "GRAND" TO GND.
           WRITE OUTREC FROM TOTAL-LINE AFTER ADVANCING 2 LINES.

         CLOSE INFILE, PRINTFILE.
         DISPLAY "Records read " RECIN.
         DISPLAY "End of program!"
         STOP RUN.                

       SRT-INPUT-PROCEDURE SECTION.
           OPEN INPUT INFILE.

           PERFORM READ-RTN THRU READ-EXIT.
           PERFORM PROCESS-RTN THRU PROCESS-EXIT
               UNTIL EOF.

       END-INPUT SECTION.

       READ-RTN.
           READ INFILE
             AT END
               SET EOF TO TRUE
           END-READ.    
      *     ADD 1 to RECIN.

      *     IF RECIN > 20000 THEN
      *         SET EOF TO TRUE.
      *    EXHIBIT NAMED IN-NAME.
       READ-EXIT.
           EXIT.       

       PROCESS-RTN.
           MOVE I-NAME TO SRT-NAME.
           MOVE I-BIRTHDAY TO SRT-BIRTHDAY.
           MOVE I-CITY TO SRT-CITY.
           MOVE I-STATE TO SRT-STATE
           MOVE I-ZIP TO SRT-ZIP.
           MOVE I-AMOUNT TO SRT-AMOUNT.
      *     MOVE I-AMOUNT TO WS-AMOUNT.
      *   Below 2 instructions were my way of converting an amount
      *   With a decimal to a numeric value that COBOL could use in
      *   calculations. Then I remembered...with Google's help, that 
      *   you could simply move it to that type of field. 
      *   If you move it COBOL converts, but COBOL won't convert it 1st
      *   if you try to use it in a calculation. I'm very rusty!
      *     Move I-dollars to ws-dollars.
      *     Move I-cents to ws-cents.
      *     ADD WS-AMOUNT TO TOT-AMT
      *         ON SIZE ERROR Display "Too big!".

           RELEASE SORT-RECORD.

           PERFORM READ-RTN THRU READ-EXIT.

       PROCESS-EXIT.
           EXIT.           

       SRT-OUTPUT-PROCEDURE SECTION.

           MOVE 'N' TO EOF-FLAG.
           RETURN SORT-FILE RECORD AT END
             MOVE 'Y' TO EOF-FLAG.

           OPEN OUTPUT PRINTFILE.

           PERFORM HEADING-RTN.
           MOVE SPACES TO OUTREC.
           WRITE OUTREC AFTER ADVANCING 1 LINE.
           MOVE ZEROES TO RECIN.
           PERFORM WRITE-RTN THRU WRITE-RTN-EXIT
               UNTIL EOF2.

       END-OUTPUT SECTION.

       WRITE-RTN.
          IF SRT-STATE <> LAST-STATE
               PERFORM STATE-BREAK-RTN THRU STATE-BREAK-EXIT.
           MOVE SPACES TO OUTREC.
           MOVE S-LAST TO O-LAST.
           MOVE S-FIRST TO O-FIRST.
           MOVE SPACES TO O-MIDDLE.
           MOVE SRT-BIRTHDAY TO O-BIRTHDAY.
           MOVE SRT-CITY TO O-CITY.
           MOVE SRT-STATE TO O-STATE.
           MOVE SRT-ZIP TO O-ZIP.
           MOVE SRT-AMOUNT TO O-AMOUNT.
           ADD SRT-AMOUNT TO TOT-AMT
               ON SIZE ERROR Display "Too big!".           

           WRITE OUTREC AFTER ADVANCING 1 LINE.
           ADD +1 TO LINE-CNT.
           if LINE-CNT > 50 THEN
               PERFORM HEADING-RTN.

           RETURN SORT-FILE RECORD AT END
             MOVE 'Y' TO EOF-FLAG.
           ADD 1 TO RECIN.             
       WRITE-RTN-EXIT.
           EXIT.

       HEADING-RTN.
           MOVE SPACES TO OUTREC.
           WRITE OUTREC AFTER ADVANCING 1 LINE.
           WRITE OUTREC FROM HEADING-1 
              AFTER ADVANCING PAGE.
           MOVE ZEROES TO LINE-CNT.
                      MOVE SPACES TO OUTREC.
           WRITE OUTREC AFTER ADVANCING 1 LINE.

       STATE-BREAK-RTN.
           IF RECIN < 1 THEN
               GO TO BYPASS-IT.
           MOVE SPACES TO OUTREC.
      *     WRITE OUTREC AFTER ADVANCING 1 LINE.
           MOVE TOT-AMT TO T-AMOUNT.
           WRITE OUTREC FROM TOTAL-LINE AFTER ADVANCING 2 LINES.

           ADD +2 TO LINE-CNT.
           if LINE-CNT > 50 THEN
               PERFORM HEADING-RTN.

           MOVE SPACES TO OUTREC.
           WRITE OUTREC AFTER ADVANCING 1 LINE. 
           ADD +1 TO LINE-CNT.
           if LINE-CNT > 50 THEN
               PERFORM HEADING-RTN.

           ADD TOT-AMT TO GRAND-TOT-AMT
               ON SIZE ERROR Display "Too big!".
           MOVE ZEROES TO TOT-AMT.
       BYPASS-IT.
           MOVE SRT-STATE TO LAST-STATE.
       STATE-BREAK-EXIT.
        EXIT.
