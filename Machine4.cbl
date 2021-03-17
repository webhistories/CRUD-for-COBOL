       IDENTIFICATION DIVISION.
       	PROGRAM-ID. MACHINE4.
       	AUTHOR. CS2-2-GROUP8.
       DATE-WRITTEN. 27SEPT2016
       
       
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
       	 05 DECIMAL REDEFINES LOA PIC 9(5)V99.
       	 05 NOMON		PIC 9(2).
       	 05 INTEREST 	PIC 9999V99.
       	 05 DECI REDEFINES INTEREST PIC 9(4)V99.
       	 05 TLOAN		PIC 99999V99.
       	 05 DEC REDEFINES TLOAN PIC 9(5)V99.
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
       77 CHOICE	PIC A.
       77 RCTR			PIC 9(3).
       77 RESP			PIC A.
       77 RESPS			PIC A.
       77 RESP2			PIC A.
       77 CORE            PIC A.
       
       SCREEN SECTION.
       01  CLRSCR.
       	 05 BLANK SCREEN.
       01 CHOICES.
       	05 VALUE "****************************************" COL 20.
       	05 VALUE "GROUP 8 - CALIXTO, CUENCA, LORZANO" LINE 2 COL 22
       	BLINK.
       	05 VALUE "****************************************" LINE 3
       	COL 20.
       	05 VALUE "[A] CREATE OR EXTEND FILES" LINE 6 COL 27.
       	05 VALUE "[B] VIEW RECORDS" LINE 7 COL 27.
       	05 VALUE "[C] DELETE RECORDS" LINE 8 COL 27.
       	05 VALUE "[D] PROCESS RECORDS" LINE 9 COL 27.
       	05 VALUE "[E] GENERATE REPORT" LINE 10 COL 27.
       	05 VALUE "[F] EXIT" LINE 11 COL 27.
       	05 VALUE "ENTER THE LETTER OF YOUR CHOICE: " LINE 13
       	COL 20.
        01 SCREEN-SEC.	 
       	 	05 VALUE "RECORD NO. " LINE 4 COL 20.
       	 	05 RECORDNO LINE 4 COL 30 PIC Z(3) FROM RCTR.
       	 	05 VALUE "TRANSACTION NO." LINE 6 COL 20.
       	 	05 TRANSAC LINE 6 COL 50 PIC Z(5) FROM W-TRANSNO.
       		05 VALUE "CLIENT NAME" LINE 7 COL 20.
       		05 CLIENT LINE 7 COL 50 PIC X(20) FROM W-CLNAME.
       		05 VALUE	"ADDRESS" LINE 8 COL 20.
       		05 ADDRE LINE 8 COL 50 PIC X(15) FROM W-ADDRS.
       		05 VALUE	"TELEPHONE NO. " LINE 9 COL 20.
       		05 TELE LINE 9 COL 50 PIC Z(7) FROM W-TELNO.
       		05 VALUE "LOAN AMOUNT" LINE 10 COL 20.
       		05 VALUE "Php" LINE 10 COL 50.
       		05 LOAN LINE 10 COL 54 PIC ZZ,Z99.99 FROM W-LOA.
       		05 VALUE	"NO. OF MONTHS" LINE 11 COL 20.
       		05 MONTHS LINE 11 COL 50 PIC ZZ FROM W-NOMON.
       		05 VALUE "INTEREST AMOUNT" LINE 12 COL 20.
       		05 VALUE "Php" LINE 12 COL 50.
       		05 IAMT LINE 12 COL 55 PIC Z,ZZ9.99 FROM INTEREST.
       		05 VALUE "TOTAL LOAN" LINE 13 COL 20.
       		05 VALUE "Php" LINE 13 COL 50.
       		05 LLOAN LINE 13 COL 54 PIC ZZ,Z99.99 FROM TLOAN.
       	
       
       PROCEDURE DIVISION.
       MAIN-RTN.
       	DISPLAY CLRSCR.
       	DISPLAY CHOICES.
       	ACCEPT CHOICE LINE 13 COL 54.
       	IF(CHOICE="A") THEN
       	PERFORM OPTION1-EXT-RTN.
       	IF (CHOICE="B")THEN
       	PERFORM OPTION2-RTN.
       	IF (CHOICE="F")THEN
       	DISPLAY CLRSCR
       	DISPLAY "EXITING...." LINE 7 COL 28
       	DISPLAY " " LINE 8 COL 23 STOP RUN.
       	IF (CHOICE="E")
       		DISPLAY CLRSCR
       		OPEN INPUT LOANFILE
       		PERFORM OPTION3-RTN.
       
       
       
       
       
      ******[B] VIEWING OF RECORDS******** 		
       OPTION2-RTN.
       	OPEN INPUT LOANFILE.
       	MOVE 0 TO RCTR.
       	READ LOANFILE INTO W-LOAN-REC AT END
       		DISPLAY "FILE IS EMPTY!!! " LINE 5 COL 26
       		MOVE "N" TO RESP.
      	PERFORM DISPLAY2-RTN THRU END-DISPLAY2-RTN
       			UNTIL RESP = "N" OR RESP = "n".
       	PERFORM TERMINATE-RTN.
       	DISPLAY2-RTN. 
       		ADD 1 TO RCTR.
       		PERFORM COMPUTE-RTN.
       		PERFORM COMPUTE2-RTN.
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
       		
       	  END-DISPLAY2-RTN.
      ***********END OF B***************************************************
       	
       	
      *****[A] CREATION AND EXTENSION***************************
       OPTION1-EXT-RTN.
       	DISPLAY CLRSCR.
       	 DISPLAY "CREATE OR EXTEND [C/E]?: ".
      	 ACCEPT CORE.
       	 IF (CORE = "C" OR CORE = "c")
       	 OPEN OUTPUT LOANFILE
       	 PERFORM CREATE-RTN.
      	 IF (CORE="E")
       	 OPEN EXTEND LOANFILE
       	 PERFORM CREATE-RTN.
       CREATE-RTN.
       	 MOVE 0 TO RCTR.
       	 MOVE "Y" TO RESP.
       	 PERFORM COMPUTE-RTN THRU END-COMPUTE-RTN. 
       	PERFORM COMPUTE2-RTN THRU END-COMPUTE2-RTN.
       	 PERFORM ACCEPT-REC-RTN THRU END-ACCEPT-REC-RTN
       		UNTIL RESP = "N" OR RESP = "n".
       PERFORM TERMINATE-RTN.
       END-CREATE-RTN.
       TERMINATE-RTN.
       	 DISPLAY "END OF PROGRAM. EXITING NOW..."
       	 LINE 18 COL 21.
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
       
       ACCEPT-REC-RTN.
       	 DISPLAY CLRSCR.
       	 COMPUTE RCTR = RCTR + 1.
       	 DISPLAY "<@@@@@ CLIENT INFORMATION @@@@@>".
       	 DISPLAY "RECORD NO. " RCTR.
       	 DISPLAY "TRANSACTION NO.: ".
       	 ACCEPT W-TRANSNO.
       	 DISPLAY "CLIENT NAME: ".
       	 ACCEPT W-CLNAME.
       	 DISPLAY "ADDRESS: ".
       	 ACCEPT W-ADDRS.
       	 DISPLAY "TELEPHONE NO.: ".
       	 ACCEPT W-TELNO.
       	 DISPLAY "LOAN AMOUNT: ".
       	 ACCEPT W-LOA.
       	 DISPLAY "NO. OF MONTHS: ".	
       	 ACCEPT W-NOMON.
       	 
       WRITE LOAN-REC FROM W-LOAN-REC.
       DISPLAY "ENTER ANOTHER RECORD [Y/N]?:".
       ACCEPT RESP.
       END-ACCEPT-REC-RTN.
      *************END OF A ************************************
      
       	OPTION3-RTN.
       	MOVE 0 TO RCTR.
       	READ LOANFILE INTO W-LOAN-REC AT END
       		DISPLAY CLRSCR
       		DISPLAY "FILE IS EMPTY!!! " LINE 5 COL 26
       		MOVE "N" TO RESP.
      	PERFORM DISPLAY-REC-RTN THRU END-DISPLAY-REC-RTN.
      	PERFORM OUTREC-RTN THRU END-OUTREC-RTN
       			UNTIL RESP = "N" OR RESP = "n".
       	PERFORM TERMINATE-RTN.
       	
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
       	
       	OUTREC-RTN.
       		PERFORM COMPUTE-RTN. 
       		PERFORM COMPUTE2-RTN.
       	 	MOVE DECIMAL TO W-LOA-DISPLAY
       		MOVE W-TRANSNO TO W-TRANSNO-DISP
       		MOVE DECI TO W-INT-DISP
       		MOVE W-NOMON TO W-NOMON-DISP
       		MOVE DEC TO W-TLOAN-DISP
       	
       		
       		DISPLAY " " W-TRANSNO-DISP "      "
       		CLNAME "P" W-LOA-DISPLAY "     "
       		W-NOMON-DISP"     " "P"W-INT-DISP"    "
       		"P"W-TLOAN-DISP.
      
        		
       		READ LOANFILE INTO W-LOAN-REC AT END
       		MOVE "N" TO RESP.
      		DISPLAY " ".
       	END-OUTREC-RTN.
      	