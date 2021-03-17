       IDENTIFICATION DIVISION.
       	PROGRAM-ID. MACHINE1.
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
       	 05 INTEREST 	PIC 9999V99.
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
       
       PROCEDURE DIVISION.
       CREATE-EXT-RTN.
       	 DISPLAY "CREATE OR EXTEND [C/E]?: ".
      	 ACCEPT CORE.
       	 IF (CORE = "C" OR CORE = "c")
       	 OPEN OUTPUT LOANFILE.
      	 IF (CORE="E" OR CORE="e")
       	 OPEN EXTEND LOANFILE.
       END-CREATE-EXT-RTN.
       MAIN-RTN.
       	 COMPUTE RCTR = RCTR + 0.
       	 MOVE "Y" TO RESP.
       	 PERFORM COMPUTE-RTN THRU END-COMPUTE-RTN. 
       	PERFORM COMPUTE2-RTN THRU END-COMPUTE2-RTN.
       	 PERFORM ACCEPT-REC-RTN THRU END-ACCEPT-REC-RTN
       		UNTIL RESP = "N" OR RESP = "n".
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
       TERMINATE-RTN.
       	 DISPLAY "END OF PROGRAM. EXITING NOW...".
       	 CLOSE LOANFILE.
       	 STOP RUN.
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