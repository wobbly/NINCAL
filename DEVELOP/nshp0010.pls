PC       EQU       0
         INC       COMMON.inc
         INCLUDE   CONS.inc
         INC       NSHPDD.inc
         inc       hp.inc
			Include	compdd.inc
			include	CNTDD.INC
         INC       NORDDD.inc
         include   NCNTDD.inc
         include   winapi.inc
last     form      3

;
; 
release  init      "1.1.1"               DMB 07JUL2006 Changed ship cost to support new format
//release  init      "1.1"               DMB 13JAN2005 Code to reformat date if not zero filled.
;release  init      "1.00"               JD 28DEC2005  New
INPUT    FILE      
SAVE     FILE      
LOGFILE  IFILE     KEYLEN=6,var=498
;
COUNT    FORM      4
UPDATE   FORM      4
WRITE    FORM      4
UPD      FORM      4
DIFF     FORM      10
TENPER   FORM      10
KEY      DIM       6
LINES    FORM      2
FIFTY4   FORM      "54"
PAGE     FORM      2
DATE     DIM       8
SALES    DIM       2
HOLDMKEY DIM       7
BRKFLAG  FORM      1            0=LIST MANAGEMENT
;
BCNAME   DIM       25
CAREOF   INIT      "C/O"
DATE1    INIT      "99/99/99"
DELETE   DIM       100
REN      DIM       100
scost    dim       5

//Patch 1.1.2
CostMask INIT      "$99.99"
CostStr	 DIM	   6
//Patch 1.1.2
savefle  dim       35

.         PACK      DELETE,"ERASE ",NTWKPATH1,"PIDISHIP.SAV"
.         PACK      REN,"REN ",NTWKPATH1,"pidi.csv pidiship.SAV"
         PACK      savefle,NTWKPATH1,"pidiship.csv"
;END PATCH 2.52 REPLACED LOGIC
Var      DIM       1
faxflag  dim       1
newflag  init      "N"
;begin patch 2.57
;begin patch 2.53
;osflag   form   1          1=win 95,98, 2=NT
;end patch 2.53
;end patch 2.57
;
.OKSTATS  INIT      "0B"
.Patch2.81
OKSTATS  INIT      "0"
.Patch2.81
         OPEN      INPUT,savefle
         OPEN      LOGFILE,"SHIPfax"         *TURNED On 12/08/94.
;
; 
         MOVE      "NSHP0010" TO PROGRAM
         MOVE      "APPLY PIDI SHIPPING INFO" TO STITLE
         MOVE      "Names In The News Ca." TO COMPNME
         MOVE      C0 TO PAGE
         CLOCK     DATE TO DATE
         clock     timestamp to str18
         unpack    str18 to str2,str16
         move      str2 to cc
;         MOVE      DATE TO TODAY
;begin patch 2.57
; begin patch 2.53
                    call                GetWinVer
;        getinfo  system,str6
;        unpack   str6 into str1,str2
;        unpack   str2 into str1
;        move     c0 to osflag
;..0 = unknown
;..1 = Windows NT
;..2 = WIN32s Windows 3.1x (obsolete)
;..3 = Window 95
;..4 = Window 98
;..5 = Windows 2000
;..8 = Windows CE
;        if       (str1 = "3" or str1 = "4")
;        move     c1 to osflag
;        endif
;        if       (str1 = "1" or str1 = "5")
;        move     c2 to osflag
;        endif
;.end patch 2.53
;end patch 2.57
         CALL      PAINT
         MOVE      C1 TO NORDPATH
;START PATCH 2.52 REPLACED LOGIC
         PACK   STR35,NTWKPATH1,"PIDISHP.LST"
         SPLOPEN   STR35
;END PATCH 2.52 REPLACED LOGIC
         CALL      HEADER
;end patch 2.60
INPUT	   
          READ      INPUT,SEQ;*cdfon,SLRNUM:
			 		    str1:
						 str1:
						 str1:
                   SINFO:
                   str10:               .date mm,dd,cc,yy
                   STR7:
						 scost:
                   strack
         GOTO      EOJ IF OVER
;Patch2.601
			scan	 "Broker" in slrnum
			goto input if equal
			reset slrnum
			scan	 "Row" in slrnum
			goto input if equal
			reset   slrnum
;Patch2.82
			scan	 "PO" in slrnum
			goto input if equal
			reset slrnum
;Patch2.82
			goto input if (slrnum = b6 or slrnum=" ")
;Patch2.601
         MOVE      NO TO V
         CMATCH    " " TO SLRNUM
         GOTO      INPUT IF EOS
;Patch2.82
			scan      "EFT" in sinfo
			if        equal
			move      "Email" to sinfo
			endif
;Patch2.82
//Patch 1.1.1 New Code
test
//      Patch 1.1 For Pidi to account for possible dollar sign in costs.
	Move	costMask,costStr
	Edit	SCOST,costStr
        call    Trim using coststr	
   	move    "," to str1
        call    Removechar using CostStr,str1
   	move    "$" to str1
        call    Removechar using CostStr,str1   
   	move    "." to str1
        call    Removechar using CostStr,str1        
        call	trim using coststr
//      Patch 1.1 For Pidi to account for possible dollar sign in costs.        
	Count n1,CostStr
	If (n1 = c4)	
		move costStr,spost
	elseif (n1 = c3)	
		pack spost,coststr,"0"
	elseif (n1 = c2)	
		pack spost,coststr,"00"	
	elseif (n1 = c1)		
		pack spost,"0",coststr,"00"
	else
		
	Endif
	goto input
//Patch 1.1.1 New Code

//Patch 1.1.1 Comment Out	
//   	move    "," to str1
//        call    removechar using scost,str1
//        move	scost to spost
//Patch 1.1.1 Comment Out End
.>Patch 1.1 Coded Added for correct date format	
.	call debug
	move str10 to str2
	type str2
	if not equal
		scan "/" in str2
		if equal 
			reset str2		
			squeeze str2,str2,"/"
			call zfillit using str2
			move str2 to mm
		endif
	else 
		move str2 to mm
	endif
	reset str10
	scan "/" in str10
	if equal
		bump str10
		move str10 to str2
		type str2
		if not equal
			scan "/" in str2
			if equal 
				reset str2
				squeeze str2,str2,"/"
				call zfillit using str2
				move str2 to dd
			endif
		else
			move str2 to dd
		endif		
	endif
	scan "/" in str10
	if equal
		bump str10
		move str10 to str4
		type str4
		goto baddate if not equal
		unpack str4,cc,yy
		compare   "20" to CC
		goto      baddate if not equal
	endif	
.Date Check	
	move    MM,N2
        if (N2 > "12")
		goto baddate
	elseif (N2 = "00")
		goto baddate	
        else
                move    DD,N2
                if (N2 > "31")
			goto baddate                
		elseif (N2 = "00")
			goto baddate                		
                else
		        move    CC,N2
			if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
				goto baddate                
                        elseif (N2 = "19")
                                move    YY,N2
                                if (N2 < "80")
					goto baddate                
                                endif
			elseif (N2 = "00")                        
				goto baddate                			
			endif
                endif
         endif	
.>Patch 1.1 End Code Added for correct date format		
.>Patch 1.1 Code Commented Out	
..        unpack  str10 into mm,slash,dd,slash,cc,yy
.>Patch 1.1 End Code Commented Out	
        pack	sdate from cc,yy,mm,dd
.>Patch 1.1 Code Commented Out	
;end patch 2.60
;begin patch 2.2
;         MOVE      STR7 TO SQUANT
..	compare     "20" to CC
..	goto      baddate if not equal
.>Patch 1.1 End Coded Commented Out         
SQTY
   		move       "," to str1
        call       removechar using str7,str1
			 call      trim using str7
          count     n2,str7
			    
         clear     squant
			if        (n2 = 7)
         pack      squant from c0,c0,str7
			elseif     (n2 = 6)
         pack      squant from c0,c0,c0,str7
			elseif     (n2 = 5)
         pack      squant from c0,c0,c0,c0,str7
			elseif     (n2 = 4)
         pack      squant from c0,c0,c0,c0,c0,str7
			elseif     (n2 = 3)
         pack      squant from c0,c0,c0,c0,c0,c0,str7
			elseif     (n2 = 2)
         pack      squant from c0,c0,c0,c0,c0,c0,c0,str7
			elseif     (n2 = 1)
         pack      squant from c0,c0,c0,c0,c0,c0,c0,c0,str7
			endif
;end patch 2.2
         TYPE      SQUANT
         GOTO      BADQTY IF NOT EQUAL
;         CLEAR     SPOST                   *2/6/91 DO NOT APPPLY TDMC $
 
         REP       ZFILL IN SLRNUM
;         MOVE      "T" TO SCODE
         ADD       C1 TO COUNT
         DISPLAY   *P10:12,"RECORDS IN : ",COUNT
         MOVE      SLRNUM TO NORDFLD
         CALL      NORDKEY
         GOTO      NOORD IF OVER
         RESET     OKSTATS
.Patch2.81
         MATCH      OSTAT IN OKSTATS
.         SCAN      OSTAT IN OKSTATS
.Patch2.81
         GOTO      NOLIVE IF NOT EQUAL
;CHECK IF CHESHIRE OR PC LABELS.
;Start Patch #2.1 - increase var to fit increased OFOCODE
;         MOVE      C0 TO N1
;         MOVE      OFOCODE TO N1
;         BRANCH    N1 OF NOPOST,POST,POST,POST,POST,NOPOST,NOPOST,NOPOST:
;                   NOPOST
;         MOVE      C0 TO N2
;         MOVE      OFOCODE TO N2
;         BRANCH    N2 OF NOPOST,POST,POST,POST,POST,NOPOST,NOPOST,NOPOST:
;                   NOPOST
;End Patch #2.1 - increase var to fit increased OFOCODE
NOPOST
;CLEAR     SPOST
POST
          MOVE      C0 TO N10
          MOVE      OQTY TO N10
         DIV       C10 INTO N10
         MOVE      N10 TO TENPER
         MOVE      SQUANT TO N10
         MOVE      OQTY TO DIFF
         SUB       N10 FROM DIFF
         COMPARE   C0 TO DIFF
         CALL      NEG IF LESS
         COMPARE   DIFF TO TENPER
         CALL      VAR IF NOT GREATER
.START PATCH 2.58 ADDED LOGIC
	move	str18,SRDATE
	move	"IS",SINITS
.END PATCH 2.58 ADDED LOGIC
         MOVE      SLRNUM TO NSHPFLD
         CALL      NSHPTST
         GOTO      DUPE IF NOT OVER
; 
         CALL      NSHPWRT
         ADD       C1 TO WRITE
         DISPLAY   *P10:13,"RECORDS WRITTEN : ",WRITE
         CMATCH    YES TO Var
         IF        EQUAL
         MOVE      NO TO Var
         GOTO      WRITELOG
         ENDIF
         COMPARE   LINES TO FIFTY4
         CALL      HEADER IF LESS
         PRINT     *L,*10,"LR ## ",SLRNUM," RECORD ADDED"
         ADD       C2 TO LINES
         GOTO      WRITELOG
;
DUPE
         call      nshpupd
         ADD       C1 TO UPD
         DISPLAY   *P10:14,"RECORDS UPDATED : ",UPD
         COMPARE   LINES TO FIFTY4
         CALL      HEADER IF LESS
         PRINT     *L,*10,"LR ## ",SLRNUM," RECORD UPDATED"
         ADD       C2 TO LINES
;dh temp? 24jun99
         goto      writelog
         goto      input
VAR
         COMPARE   LINES TO FIFTY4
         CALL      HEADER IF LESS
         PRINT     *L,*10,"LR ## ",SLRNUM," VARIES > +/- 10%":
                   "ORDER QTY ",OQTY," TDMC QTY ",SQUANT
         ADD       C2 TO LINES
         move      yes to faxflag
         MOVE      YES TO V
         call      sendnews
         RETURN
BADDATE
         COMPARE   LINES TO FIFTY4
         CALL      HEADER IF LESS
         PRINT     *L,*10,"LR ## ",SLRNUM," ERROR IN SHIP DATE"
         ADD       C2 TO LINES
         GOTO      INPUT
NOORD
         COMPARE   LINES TO FIFTY4
         CALL      HEADER IF LESS
         PRINT     *L,*10,"LR ## ",SLRNUM," NO ORDER FOUND ",SCODE:
                   " ",SQUANT," ",SPOST,*L,*10,SINFO
         ADD       C3 TO LINES
         GOTO      INPUT
NOlive
         COMPARE   LINES TO FIFTY4
         CALL      HEADER IF LESS
.Patch2.81
			if (OSTAT = "B")
   	      PRINT     *L,*10,"LR ## ",SLRNUM," Already Billed! ",SCODE:
                   " ",SQUANT," ",SPOST,*L,*10,SINFO
	         ADD       C3 TO LINES
			else
   	      PRINT     *L,*10,"LR ## ",SLRNUM," NOT A LIVE ORDER! ",SCODE:
                   " ",SQUANT," ",SPOST,*L,*10,SINFO
	         ADD       C3 TO LINES
			endif
.Patch2.81
         GOTO      INPUT
HEADER   ADD       C1 TO PAGE
         compare    c1 to page
         if        equal
.Patch2.83
;         PRINT     *L,*L,"^[D14025376101^[NBrandi-TDMC  ^]",*n,032,hpreset:
         PRINT     *L,*L,"",*n,032,hpreset:
.Patch2.83
                   hpttray:
                   hpport:
                   hpdupl:
                   033,"&l66P":               page length
                   033,"&l65F":
                   033,"&l1E",033,"&a0c0R":
                   *10,"PIDI SHIPPING REPORT",*60,DATE:
                   *L,*60,"PAGE: ",PAGE,*L
         else          
          PRINT    *F,*L,*L,*L,*10,"PIDI SHIPPING REPORT",*60,DATE:
                   *L,*60,"PAGE: ",PAGE,*L
         endif          
         MOVE      C6 TO LINES
         RETURN
BADQTY
         COMPARE   LINES TO FIFTY4
         CALL      HEADER IF LESS
         PRINT     *L,*10,"LR ## ",SLRNUM," QUANTITY NOT NUMERIC"
         ADD       C2 TO LINES
         GOTO      INPUT
;
NEG      MULT      SEQ BY DIFF
         RETURN
;
WRITELOG
         PACK      MKEY FROM OMLRNUM,OCOBN
         MATCH     MKEY TO HOLDMKEY
         IF        NOT EQUAL
         CALL      NMLRKEY
         MOVE      MKEY TO HOLDMKEY
         ENDIF
;         MOVE      C1 TO BRKFLAG
         MOVE      C0 TO N2
         PACK      SALES FROM OSALES10,OSALES
         MOVE      SALES TO N2
         COMPARE   C6 TO N2
         goto      wrtalph2 if equal
;         IF        EQUAL
;         MOVE      C0 TO BRKFLAG
;         ENDIF
         COMPARE   "19" TO N2
         goto      wrtalph2 if equal
;         IF        EQUAL
;         MOVE      C0 TO BRKFLAG
;         ENDIF
;         MOVE      MCOMP TO BCNAME
;         BRANCH    BRKFLAG OF WRTALPH2
;         MOVE      MCOMP TO MNAME
;         PACK      BCNAME FROM MCCTO,B6,CAREOF
          GOTO      INPUT
;          
WRTALPH2 FILEPI    3;LOGFILE
         READ      LOGFILE,NORDFLD;;
         GOTO      INPUT IF NOT OVER
         WRITE     LOGFILE,NORDFLD;ORDVARS
         GOTO      INPUT
;
EOJ      CLOSE     INPUT
;         CLOSE     LOGFILE
         COMPARE   C0 TO PAGE
         CALL      HEADER IF EQUAL
         COMPARE   LINES TO FIFTY4
         CALL      HEADER IF LESS
         PRINT     *L,*10,"NUMBER OF RECORDS RECEIVED: ",COUNT
         PRINT     *FLUSH
         RELEASE
         SPLCLOSE 
;begin patch 2.57
                    cALL                getwinver
                    If                  (osflag = c1 | osflag = c5)
         PACK      TASKNAME,"c:\winnt\system32\cmd.exe /c copy ",NTWKPATH1,"PIDISHP.LST \\nts0\LASER8 "
         Execute   TASKNAME
                    ElseIf              (osflag = c3 | osflag = c4)
         PACK      TASKNAME,"c:\command.com /c copy ",NTWKPATH1,"PIDISHP.LST \\nts0\LASER8 "
         EXECUTE   TASKNAME
                    Elseif              (osflag = c6)
         PACK      TASKNAME,"c:\windows\system32\cmd.exe /c copy ",NTWKPATH1,"PIDISHP.LST \\nts0\LASER8 "
         Execute   TASKNAME
         endif
//Patch 1.1.1 Comment No need to send anything to triplex         
//         cmatch    yes to faxflag
//         if         equal
//         if        (osflag = c1 | osflag = C5)
//         PACK      TASKNAME,"c:\winnt\system32\cmd.exe /c copy /b ",NTWKPATH1,"TDMCSHP.LST \\nts2\fax "
//         Execute   TASKNAME
//         Elseif        (osflag = c3 | osflag = C4)
//         PACK      TASKNAME,"c:\command.com /c copy ",NTWKPATH1,"TDMCSHP.LST \\nts2\fax "
//         EXECUTE   TASKNAME
//         Elseif        (osflag = c6)
//         PACK      TASKNAME,"c:\windows\system32\cmd.exe /c copy /b ",NTWKPATH1,"TDMCSHP.LST \\nts2\fax "
//         Execute   TASKNAME
//         endif
//;         DISPLAY   *P1:24,*R,*EL,"Faxing copy TDMC ",*W5
//         endif
         DISPLAY   *P1:24,*R,*EL,"COPING FILE TO .SAV",*B,*W5
	call debug         
         PACK       STR35,NTWKPATH1,"pidishp.sav"
         erase      STR35
         rename     savefle,str35
DONE     STOP
;...........................
sendnews
        pack    NCNTFLD,OCOCODE
        move    "SVCREP-NCNTKEY",Location
        pack    KeyLocation,"Key: ",NCNTFLD
         clear     cntname
         clear     str35
        call    NCNTKEY
        clear   str1
        append  CNTNAME,str1         ..1st init
        reset   str1
        scan    B1,CNTNAME
        bump    CNTNAME,1
        move    CNTNAME,str7
        call    RemoveChar using str7,B1
        move    str7,str6
        call    trim using str6
        pack    str7 from str1,str6
        RESET   CNTNAME
        move    cntname to str35
 
      Move    "This is a Informational e-mail from  the PIDI shipping program",SmtpSubject Subject
;   Set the text message that is send with the attachments

       Move    "This is a Informational e-mail from  the PIDI shipping program",SmtpTextMessage(1)   Array <Text message >
        clear   str25
        append  "record## " to str25
        append  olrn to str25
        append  b1 to str25
        reset   str25
        Move    str25,SmtpTextMessage(2)   Array <Text message >
        Move    " Your above LR had a shipping qty variance:",SmtpTextMessage(3)   Array <Text message >
        clear   str55
        append  "Order qty " to str55
        append  oqty to str55
        append  ", Shipped qty ",str55
        append   squant,str55
        reset    str55
        Move    str55,SmtpTextMessage(4)   Array <Text message >
        Move    "Please review & correct as necessary.",SmtpTextMessage(5)   Array <Text message >

        Move    "5",SmtpTextIndexLast                               Index to last entry in TextMessage array

         clear     taskname
         Move    "NTS4",SmtpEmailServer                   Address of email serverc
         clear   smtpemailaddress
         append  str7 to SmtpEmailAddress
         append  "@nincal.com",SmtpEmailAddress
         reset    smtpemailaddress
        Move    str7,SmtpUserName                                User name
        Move    STR35,SmtpUserFullName              User Full Name

;   Set the destinations of the email. Max 100 (Mime spec)

        MOVE    smtpemailaddress to SmtpDestinations(1,1)           .send a copy to the originator
        MOVE     user,SmtpDestinations(1,2)                          originators UserName
        Move    "1",SmtpDestIndexLast                               Index to last entry in Dest array


        Move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
        Clear   SmtpLogFile                                         'Clear' disables the LogFile


        Call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )

        winshow
        return
;end patch 2.6 
;Patch2.8
			Include	COMPIO.inc
			include	CNTIO.INC
.         INCLUDE   NMLRIO.inc
;PATCH2.8
         INCLUDE   NSHPIO.inc
         INCLUDE   NORDIO.inc
;START PATCH 2.5 ADDED LOGIC
         include   NCNTIO.inc
;END PATCH 2.5 ADDED LOGIC
         INCLUDE   COMLOGIC.inc

