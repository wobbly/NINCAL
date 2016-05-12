PC      EQU     0

        include COMMON.INC
        include CONS.INC
.========================================================================================
         MOVE      "NFTP0003" TO PROGRAM
         MOVE      "PIDI FTP PROGRAM" TO STITLE
         MOVE      "Names in the News CA" TO COMPNME
         CALL      PAINT
RELEASE   INIT      "1.12"			28JUN2006	New file format, CSV 
;RELEASE   INIT      "1.1"			13JAN2003	Added code to email files 
;RELEASE   INIT      "1.0"			.Program will compare FTPSAV Records for PIDI with temp file for new download of records from FTP

FTPTMP   FILE
FTPSAV   FILE
FTPDWN   FILE
SAVRED   DIM    120
FILESTR  DIM    160
str120    DIM   120
.Form Pointer set to grab file name.must change if file structure changes
POINT    FORM   "56"
fstrlen  form  3
;.        call Paint
        clear n2
        Display    *P10:11,*EF,*White,"Opening Files!!"
        OPEN FTPTMP,"FTPTMP"
;        OPEN FTPTMP,"FTPTMP"
        OPEN FTPSAV,"FTPSAV3",EXCLUSIVE
        PREPARE FTPDWN,"\\nins1\E\DATA\ftpdwn"

        Display    *P10:11,*EF,*White,"Reading Save file for Files already downloaded!!"
TRYAGAIN
                READ FTPTMP,SEQ;FILESTR
                GOTO OVERNOW IF OVER
;.	        CALL TRIM USING FILESTR
;.Exceptions for the root directories
	scan ".prn" in FILESTR
	goto READSAV if equal
	reset FILESTR
	scan ".PRN" in FILESTR
	goto READSAV if equal
;patch1.12
	reset FILESTR
	scan ".CSV" in FILESTR
	goto READSAV if equal
	reset FILESTR
	scan ".csv" in FILESTR
	goto READSAV if equal
;patch1.12
.	reset FILESTR
.	bump	filestr,55
	goto TRYAGAIN
READSAV
	reset FILESTR
	bump	filestr,55
	        reposit FTPSAV,c0
        LOOP
                READ FTPSAV,SEQ;SAVRED
        UNTIL OVER
                call trim using savred
                scan SAVRED in FILESTR
                goto TRYAGAIN if equal
;.               CALL TRIM USING SAVRED
;.               If (str20 = SAVRED)
;.               ENDIF
        REPEAT
Writer
        add c1 to n2
        Display    *P10:13,*EF,*White,"Files to be downloaded   ",n2
        Pause   C2
;.       reset  FILESTR to POINT
        move   FILESTR to str120
        WRITE  FTPDWN,SEQ;str120
;patch1.1
		  add	c1 to n3
		  append	str120,Mailbody
		  append	crlf,Mailbody
;patch1.1
        GOTO   TRYAGAIN
OVERNOW
        Display    *P10:11,*EF,*White,"Job Complete!!"

   If (n2 = c0)
                Display    *P10:11,*EF,*White,"No Records to Download!!"
                Close FTPDWN
                pack  taskname,NTWKPATH1,"ftpdwn.DAT"
                erase TASKNAME
	else
;patch1.1
			move    "These Files Are Being Downloaded.",mailsubjct
			reset	Mailbody	
			Pack	MailFrom from "ComputerRequest","@nincal.com"
			Pack	MailTo from "ComputerRequest","@nincal.com"
			call	SendMail
;patch1.1
	endif
        Close   FTPTMP
        pack    taskname,NTWKPATH1,"FTPTMP.DAT"
        erase   taskname

        Pause   C5
        STOP
        include comlogic.inc
