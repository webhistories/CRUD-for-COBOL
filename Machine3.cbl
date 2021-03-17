       IDENTIFICATION DIVISION.
       	PROGRAM-ID. MACHINE3.
       	AUTHOR. CS2-2-GROUP8.
       DATE-WRITTEN.  26SEPT2016
       
       
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
       	 05 TRANSNO		PIC ZZZZZ.
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
       	 05 W-LOA		PIC 9(7).
       	 05 W-DECIMAL REDEFINES W-LOA PIC 9(5)V99.
       	 05 W-NOMON		PIC Z(2).
       	 05 W-INTEREST 	PIC 9(6).
       	 05 W-DECI REDEFINES W-INTEREST PIC 9(4)V99.
       	 05 W-TLOAN		PIC 9(7).
       	 05 W-DEC REDEFINES W-TLOAN PIC 9(5)V99.
       01 W-LOA-DISPLAY	PIC ZZ,Z99.99.
       01 W-INT-DISP 		PIC Z,ZZ9.99.
       01 W-TRANSNO-DISP	PIC ZZZ99.
       01 W-NOMON-DISP	PIC Z9.
       01 W-TLOAN-DISP	PIC ZZ,Z99.99.
       77 RCTR			PIC 9(3).
       77 RESP			PIC A. 
       77 CORE            PIC A.
       
       
       SCREEN SECTION.
       01  CLRSCR.
       	 05 BLANK SCREEN.
      
       PROCEDURE DIVISION.
       MAIN-RTN.
       	OPEN INPUT LOANFILE.
       	MOVE 0 TO RCTR.
       	READ LOANFILE INTO W-LOAN-REC AT END
       		DISPLAY CLRSCR
       		DISPLAY "FILE IS EMPTY!!! " LINE 5 COL 26
       		MOVE "N" TO RESP.
      	PERFORM DISPLAY-REC-RTN THRU END-DISPLAY-REC-RTN.
      	PERFORM OUTPUT-REC-RTN THRU END-OUTPUT-REC-RTN
       			UNTIL RESP = "N" OR RESP = "n".
       	TERMINATE-RTN.
       		CLOSE LOANFILE.
       		STOP RUN.
       	COMPUTE-RTN.
       		IF (NOMON>0 AND NOMON<5)
      		COMPUTE W-INTEREST = (3 *  LOA). 
       		IF (NOMON>4 AND NOMON<9)
       		COMPUTE W-INTEREST = (4 *  LOA). 
       		IF (NOMON>9 AND NOMON<13)
       		COMPUTE W-INTEREST = (5 *  LOA).
       		IF (NOMON>12)
       		COMPUTE W-INTEREST = (1 *  LOA). 
      	END-COMPUTE-RTN.
      	COMPUTE2-RTN.
      		COMPUTE W-TLOAN = (W-INTEREST + W-LOA).
      	END-COMPUTE2-RTN.
       	DISPLAY-REC-RTN. 
       		ADD 1 TO RCTR.
       		DISPLAY CLRSCR.
       		DISPLAY "56 Matubo St., Makati City"
        			LINE 2 COL 28.
        		DISPLAY "TAMBUNTING PAWNSHOP" 
        		LINE 1 COL 30.
        		DISPLAY "Loan Receivables Report"
        		LINE 5 COL 29.
      
      	 	DISPLAY "TRANS." LINE 8 COL 2.
       	 	DISPLAY "NO." LINE 9 COL 3.
       		DISPLAY "CLIENT NAME" LINE 8 COL 15.
       		DISPLAY "LOAN" LINE 8 COL 35.
       		DISPLAY "AMOUNT" LINE 9 COL 34.
       		DISPLAY "NO. OF" LINE 8 COL 47.
       		DISPLAY "MONTHS" LINE 9 COL 47.
       		DISPLAY "INTEREST" LINE 8 COL 57.
       		DISPLAY "AMOUNT" LINE 9 COL 58.
       		DISPLAY "TOTAL" LINE 8 COL 71.
       		DISPLAY "LOAN      " LINE 9 COL 71.
       		DISPLAY " ".
      
       		
       		
       	
       	 END-DISPLAY-REC-RTN.
       	
       	OUTPUT-REC-RTN.
       		PERFORM COMPUTE-RTN THRU END-COMPUTE-RTN. 
       		PERFORM COMPUTE2-RTN THRU END-COMPUTE2-RTN.
       		MOVE W-DECIMAL TO W-LOA-DISPLAY
       		MOVE W-TRANSNO TO W-TRANSNO-DISP
       		MOVE W-DECI TO W-INT-DISP
       		MOVE W-NOMON TO W-NOMON-DISP
       		MOVE W-DEC TO W-TLOAN-DISP
       	
       		
       		DISPLAY " " W-TRANSNO-DISP "      "
       		CLNAME "P" W-LOA-DISPLAY "     "
       		W-NOMON-DISP"     " "P"W-INT-DISP"    "
       		"P"W-TLOAN-DISP.
      
        		
       		READ LOANFILE INTO W-LOAN-REC AT END
       		MOVE "N" TO RESP.
      		DISPLAY " ".
       	END-OUTPUT-REC-RTN.
       	