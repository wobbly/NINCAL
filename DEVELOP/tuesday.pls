PC        EQU       0
          include common.inc
          include cons.inc
.START PATCH 1.1 REPLACED LOGIC
.        include nmlrdd.inc
.        include nbrkdd.inc
          include   compdd.inc
          include   cntdd.inc
.END PATCH 1.1 REPLACED LOGIC
          include norddd.inc



RECMST    FILE      VAR=498,COMP         .was 328
FILENUM             FORM      2         TO CREATE DISKIN FILENAME
F2        DIM       2         TO CREATE DISKIN FILENAME
NEWNAME   DIM       12        USED TO SEARCH SYSTEM FOR FILE NAME IN USE
RECNAME   DIM       55        NEW FILE NAME CREATED...WRITTEN TO DRIVE 12
KeepDate1 FORM      5
KeepDate2 FORM      5
Found     form      9
BCNAME    DIM       45        BROKER NAME
HOLDMLR   DIM       7              HOLDS MLR # TO OMIT REUNDANT READS
HOLDBRK   DIM       4              HOLDS BROKER # TO OMIT REDUNDANT READS
CAREOF    INIT      "C/O"
Current	  File		
check	  FORM	    1
FULL	  DIM       50
DEST	  DIM	    50

release             init                "1.4"     Renamed to Tuesday
Reldate   Init      "16 April 2014"
.release             init                "1.3"     RVW This used to run monday.exe.  Now it does step 7 on it's own and will be it's own task
.Reldate   Init      "16 April 2014"
.release             init                "1.2"     DLH Starting LR
.Reldate   Init      "28 April 2011"
.release             init                "1.1"     26MAY2004 MAILER CONVERSION
.release             init                "1.0"    28Aug2003 Automation of Monday.
.**********.776000 FIRST NIN LR OF 2014
.Note increment occasionally to speed up job 2003-2011 it was reading entire file


                    call                paint




          clock weekday,str1
          move str1,n1    
          if (n1 = 2)                   ;it's monday
.Run it
                    clock timestamp,timestamp     
                    unpack    timestamp,cc,yy,mm,dd
                    unpack    timestamp,cc,yy,mm,dd
                    call      cvtjul
                    move      juldays to KeepDate1
                    sub       c7 from KeepDate1
                    add       c4,KeepDate1,KeepDate2
          elseif (n1 = 3)     ;it's tuesday
.Run it
                    clock timestamp,timestamp     
                    unpack    timestamp,cc,yy,mm,dd
                    unpack    timestamp,cc,yy,mm,dd
                    call      cvtjul
                    sub       c1,juldays
                    move      juldays to KeepDate1
                                        sub                 c7 from KeepDate1
                    add       c4,KeepDate1,KeepDate2

                    
          elseif (n1 =4)      ;it's wednesday
                    clock timestamp,timestamp     
                    unpack    timestamp,cc,yy,mm,dd
                    unpack    timestamp,cc,yy,mm,dd
                    call      cvtjul
                    sub       c2,juldays
                    move      juldays to KeepDate1
                                        sub                 c7 from KeepDate1
                    add       c4,KeepDate1,KeepDate2
          elseif (n1 =5)      ;it's Thrusday
                    clock timestamp,timestamp     
                    unpack    timestamp,cc,yy,mm,dd
                    unpack    timestamp,cc,yy,mm,dd
                    call      cvtjul
                    sub       c3,juldays
                    move      juldays to KeepDate1
                                        sub                 c7 from KeepDate1
                    add       c4,KeepDate1,KeepDate2
          elseif (n1 =6)      ;it's Friday
                    clock timestamp,timestamp     
                    unpack    timestamp,cc,yy,mm,dd
                    unpack    timestamp,cc,yy,mm,dd
                    call      cvtjul
                    sub       c4,juldays
                    move      juldays to KeepDate1
                                        sub                 c7 from KeepDate1
                    add       c4,KeepDate1,KeepDate2
          elseif (n1 =7)      ;it's Saturday
                    clock timestamp,timestamp     
                    unpack    timestamp,cc,yy,mm,dd
                    unpack    timestamp,cc,yy,mm,dd
                    call      cvtjul
                    sub       c5,juldays
                    move      juldays to KeepDate1
                                        sub                 c7 from KeepDate1
                    add       c5,KeepDate1,KeepDate2
          elseif (n1 =1)      ;it's Sunday
                    clock timestamp,timestamp     
                    unpack    timestamp,cc,yy,mm,dd
                    unpack    timestamp,cc,yy,mm,dd
                    call      cvtjul
                    sub       c6,juldays
                    move      juldays to KeepDate1
.                                        sub                 c7 from KeepDate1
                    add       c4,KeepDate1,KeepDate2
          endif
          
          move keepdate1 to juldays
          call      cvtgreg   
          pack      str8,mm,slash,dd,slash,yy                 
          move keepdate2 to juldays
          call      cvtgreg   
          pack      str10,mm,slash,dd,slash,yy                 
                    Display   *P6:10,"The dates for the pick are ",str8,b1,dash,b1,str10
.patch xxx
.turn off record locking          
          Move      C3,Nordlock
FileCreate
.Creation of Diskin File
          MOVE      "01",FILENUM
FILENAME 
          CLEAR     NEWNAME
          APPEND    "DISKIN",NEWNAME
          MOVE      FILENUM,F2
          REP       " 0",F2
          APPEND    F2,NEWNAME
          RESET     NEWNAME TO 8
          RESET     NEWNAME
          TRAP      GOODFILE GIVING ERROR IF IO
          OPEN      RECMST,NEWNAME
          CLOSE     RECMST
ADDFILE  
          ADD       "1",FILENUM
          GOTO      FILENAME
GOODFILE 
          TRAPCLR   IO
          NORETURN
          SCAN      "0030-0031" IN ERROR
          GOTO      ADDFILE IF EQUAL
          RESET     ERROR
          SCAN      "I * Y" IN ERROR
          GOTO      ADDFILE IF EQUAL
          reset     error  
          SCAN      "I10" IN ERROR
          GOTO      ADDFILE IF EQUAL
          CLEAR     RECNAME
          IFNZ      PC
          APPEND    NEWNAME,RECNAME
          APPEND    "/TEXT:PRINT",RECNAME
          XIF
          IFZ       PC
          APPEND    "\\nins1\e\data\",RECNAME                                         ."
          APPEND    NEWNAME,RECNAME
          XIF
          RESET     RECNAME
          MOVE      B1 TO ERROR
          PREPARE   RECMST,RECNAME
          PREPARE   RECMST,RECNAME,CREATE
.newname is our diskin
          Display   *P6:12,"The Diskin I am going to use is: ",newname
          MOVE      C1 TO NORDPATH
.begin patch 1.2
          packkey   Nordfld,"776000"  .**********.776000 FIRST NIN LR OF 2014
          call      Nordtst
.end patch 1.2
          loop
LoopThis
.begin patch 1.2
.                    CALL      NORDSEQ
                    CALL      NORDKS
.end patch 1.2
                    
          until     over
          add c1 to n9
          Display   *P6:16,"Records I have Read   ",N9
CHKDATES
.get date from order
                    MOVE      OODTEM,MM
                    MOVE      OODTEY,YY
                    MOVE      OODTED,DD
                    call      cvtjul
                    COMPARE   KEEPDATE1 TO juldays
                    GOTO      LoopThis IF LESS              NO. TRY NEXT ORDER.
                    COMPARE   juldays TO KEEPDATE2
                    GOTO      LoopThis IF LESS              NO. TRY NEXT ORDER.
WRITEIT
                    ADD       "1",FOUND
                    move      found to str9
                    Display   *P6:20,"Records Written   ",Found
WRTALPH
                    PACK      MKEY FROM OMLRNUM,OCOBN
                    MATCH     MKEY,HOLDMLR                 *same mlr?
                    GOTO      WRTALPH2 IF EQUAL           *yes, no mlr read needed.
                    MOVE      MKEY,HOLDMLR                 *no, get mlr
                    CALL      NMLRKEY
                    MOVE      MCOMP TO BCNAME      *was remmed for patch #4.3
                    PACK      mNAME FROM MCCTO,B6,CAREOF
                    clear     nbrkfld
                    move      c1 to nbrkpath
                    match     obrknum to holdbrk
                    goto      wrtalph2 if equal
                    move      obrknum to holdbrk
                    pack      nbrkfld from obrknum,z3
                    cmatch    b1 to nbrkfld       
                    if        not eos
                              call      nbrkkey
                              if        not over
                                        move      brcomp to mname
                              endif 
                    endif
                    MOVE      MCOMP TO BCNAME      *was remmed for patch #4.3
WRTALPH2
                    rep       lowup in o2des
RITERECA
                    WRITE     RECMST,SEQ;ORDVARS:
                              MNAME:
                                        BCNAME
          repeat

         
Endit
	  WEOF		RECMST,SEQ
          Close     RECMST
          
          
.                    pack      taskname with "\\nins1\e\apps\winbatch\monday.exe ",NEWNAME

.Begin Patch 1.3 RVW
.We need to check that NEWFILE exists, copy it to to diskname then run nord0046 instead
.		     pack      taskname with "\\Nins1\winbatch\monday.exe ",NEWNAME		   
		     	     
		     	append "\\nins1\e\DATA\",FULL
		     	append NEWNAME,FULL
			append ".dat",FULL		     
		     
		     	ERASE "\\nins1\e\DATA\diskweek.dat"
		     	
		     	Pack DEST,NTWKPATH1,"\diskweek.dat"
		     	reset FULL
		     	COPYFILE FULL,DEST
		     	
		     	if (zero)
		     	     	pack      taskname with "\\nins1\e\apps\plb\code\plbwin.exe nord0046.plc"
                     		EXECUTE   TASKNAME
                     		PAUSE "1"
		     		ERASE "\\nins1\e\DATA\diskweek.dat"
		     	else
		     		GOTO FileWorldProblems
		     	endif
.End Patch 1.3 RVW                     
      
Ender
          stop


.START PATCH 1.1 REPLACED LOGIC
.        include nmlrio.inc
.        include nbrkio.inc
          include   compio.inc
          include   cntio.inc
.END PATCH 1.1 REPLACED LOGIC
          include nordio.inc 
          include comlogic.inc

.Begin Patch 1.3 RVW
FileWorldProblems
	  clear MailSubjct
	  clear MailBody
	  move c1,check
          append    "Error copying when running Monday.pls",mailsubjct
          reset     mailsubjct
          append    "Monday - Report",mailbody
          append    CRLF,mailbody
          append    "I atried to acopy ",mailbody
          append    NEWNAME,mailbody
          append    ".dat",mailbody
          append    " to ",mailbody
          append    "diskweek.dat ",mailbody       
          append    "but it wouldn't let me.  =(",mailbody
          append    CRLF,mailbody
          append    CRLF,mailbody
          move      "computerRequest@nincal.com",mailfrom
	  move      "computerRequest@nincal.com",mailTo
          reset     Mailbody
	  Call 	    Sendmail
	  STOP
.End Patch 1.3 RVW	  