       IDENTIFICATION DIVISION.
       	PROGRAM-ID. MACHINE2.
       	AUTHOR. CS2-2-GROUP8.
       DATE-WRITTEN.  19SEPT2016
       
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       	 SELECT LOANFILE ASSIGN TO DISK.
       DATA DIVISION.
       FILE SECTION.
       FD  LOANFILE
       	 LABEL RECORD IS STANDARD
       	 VALUE OF FILE-ID IS "LOAN.Dat"
       	 DATA RECORD IS LOAN-REC.
       01	 LOAN-REC.
       	 05 TRANSNO		PIC 9(5).
       	 05 CLNAME 		PIC X(20).
       	 05 ADDRS 		PIC X(15).
       	 05 TELNO		PIC 9(7).
       	 05 LOA			PIC 99999V99.
       	 05 NOMON		PIC 9(2).
       	 05 INTEREST	PIC 9999V99.
       	 05 TLOAN		PIC 99999V99.
       WORKING-STORAGE SECTION.
       01	 W-LOAN-REC.	 
       	 05 W-TRANSNO	PIC 9(5).
       	 05 W-CLNAME 	PIC X(20).
       	 05 W-ADDRS 	PIC X(15).
       	 05 W-TELNO		PIC 9(7).
       	 05 W-LOA		PIC 99999V99.
       	 05 W-NOMON		PIC 9(2).
       	 05 W-INTEREST 	PIC 9999V99.
       	 05 W-TLOAN		PIC 99999V99.
       77 RCTR			PIC 9(3).
       77 RESP			PIC A. 
       77 CORE            PIC A.
       
       SCREEN SECTION.
       01  CLRSCR.
       	 05 BLANK SCREEN.
        01 SCREEN-SEC.	 
       	 	05 VALUE "RECORD NO. " LINE 4 COL 20.
       	 	05 RECORDNO LINE 4 COL 30 PIC Z(3) FROM RCTR.
       	 	05 VALUE "TRANSACTION NO." LINE 6 COL 20.
       	 	05 TRANSAC LINE 6 COL 50 PIC Z(5) FROM TRANSNO.
       		05 VALUE "CLIENT NAME" LINE 7 COL 20.
       		05 CLIENT LINE 7 COL 50 PIC X(20) FROM CLNAME.
       		05 VALUE	"ADDRESS" LINE 8 COL 20.
       		05 ADDRE LINE 8 COL 50 PIC X(15) FROM ADDRS.
       		05 VALUE	"TELEPHONE NO. " LINE 9 COL 20.
       		05 TELE LINE 9 COL 50 PIC Z(7) FROM TELNO.
       		05 VALUE "LOAN AMOUNT" LINE 10 COL 20.
       		05 VALUE "Php" LINE 10 COL 50.
       		05 LOAN LINE 10 COL 54 PIC ZZ,Z99.99 FROM LOA.
       		05 VALUE	"NO. OF MONTHS" LINE 11 COL 20.
       		05 MONTHS LINE 11 COL 50 PIC ZZ FROM NOMON.
       		05 VALUE "INTEREST AMOUNT" LINE 12 COL 20.
       		05 VALUE "Php" LINE 12 COL 50.
       		05 IAMT LINE 12 COL 55 PIC Z,ZZ9.99 FROM INTEREST.
       		05 VALUE "TOTAL LOAN" LINE 13 COL 20.
       		05 VALUE "Php" LINE 13 COL 50.
       		05 LLOAN LINE 13 COL 54 PIC ZZ,Z99.99 FROM TLOAN.
      
       PROCEDURE DIVISION.
       MAIN-RTN.
       	OPEN INPUT LOANFILE.
       	MOVE 0 TO RCTR.
       	READ LOANFILE INTO W-LOAN-REC AT END
       		DISPLAY "FILE IS EMPTY!!! " LINE 5 COL 26
       		MOVE "N" TO RESP.
      	PERFORM DISPLAY-REC-RTN THRU END-DISPLAY-REC-RTN
       			UNTIL RESP = "N" OR RESP = "n".
       	TERMINATE-RTN.
       		CLOSE LOANFILE.
       		STOP RUN.
       	COMPUTE-RTN.
       		IF (NOMON>0 AND NOMON<5)
      		COMPUTE INTEREST = (.03 *  LOA). 
       		IF (NOMON>4 AND NOMON<9)
       		COMPUTE INTEREST = (.04 *  LOA). 
       		IF (NOMON>9 AND NOMON<13)
       		COMPUTE INTEREST = (.05 *  LOA).
       		IF (NOMON>12)
       		COMPUTE INTEREST = (.01 *  LOA). 
      	END-COMPUTE-RTN.
      	COMPUTE2-RTN.
      		COMPUTE TLOAN = (INTEREST + LOA).
      	END-COMPUTE2-RTN.
       	DISPLAY-REC-RTN. 
       		ADD 1 TO RCTR.
       		DISPLAY CLRSCR.
       		DISPLAY "[<*****>]    LOAN COMPUTATION    [<*****>}"
        			LINE 2 COL 21.
        		DISPLAY "**************************************"
        		 LINE 1 COL 20.
        		DISPLAY "******" LINE 1 COL 58.
        		DISPLAY "**************************************" 
        		LINE 3 COL 20.
        		DISPLAY "******" LINE 3 COL 58.
        		DISPLAY "**************************************" 
        		LINE 14 COL 20.
        		DISPLAY "******" LINE 14 COL 58.
        			PERFORM COMPUTE-RTN THRU END-COMPUTE-RTN. 
       			PERFORM COMPUTE2-RTN THRU END-COMPUTE2-RTN.
      		DISPLAY SCREEN-SEC.
        		DISPLAY "RETRIEVE MORE FILES? [Y/N]: " 
        		LINE 16 COL 21.
       		ACCEPT RESP.
       		READ LOANFILE INTO W-LOAN-REC 
       		AT END DISPLAY "END OF FILE ENCOUNTERED!!! "
       		LINE 17 COL 21
       		MOVE "N" TO RESP.
       		
       	  END-DISPLAY-REC-RTN.
       	
       	
       