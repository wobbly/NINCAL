PC       EQU       0
         INC       COMMON.inc
         INCLUDE   CONS.inc
         INC       NSHPDD.inc
         inc       hp.inc
;Patch2.8
;         INC       NMLRDD.inc
                              Include   compdd.inc
                              include   CNTDD.INC
;Patch2.8
         INC       NORDDD.inc
;begin patch 2.3
;START PATCH 2.5 REPLACED LOGIC
;         include   contact1.inc
         include   NCNTDD.inc
;END PATCH 2.5 REPLACED LOGIC
         include   winapi.inc
last     form      3
;end patch 2.3

;
;              "APPLY TRIPLEX SHIPPING INFO" TO STITLE
release  init      "2.82"               DLH change srv2008a to NINS2
Reldate   Init      "11 January 2012"
.release  init      "2.81"               DMB 08AUG2004 added code to prevent updates of billed records
.elease  init      "2.8"               DMB 26MAY2004 Mailer Conversion
;release  init      "2.601"               DMB 27APR2004 skip total records
;release  init      "2.60"               JD 11Mar2004 new tab input format, added postage cost.
;release  init      "2.58"              ASH 14MAY2003 Increased Record length of NINSHP - Added vars
;release  init      "2.57"              DLH 12jul2002 Use GetWinVer
;release  init      "2.56"              jd13jun2002 expanded delete/ren vars.
;release  init      "2.55"             jd04mar2002 added duplex.
;release  init      "2.54"             jd 12DEC2001 ONLY APPLY LIVE ORDER INFO.
;release  init      "2.53"             DLH 30Mar2001 replace c:\windows logic
;release  init      "2.52"             ASH 02OCT2000 NEW SERVER ADDED
;release  init      "2.51"             ASH 01MAR2000 new logic for email addresses
;release  init      "2.5"             ASH 16MAR2000 REPLACED CONTACT1.INC WITH NCNTDD.INC
;release  init      "2.4"             JD  28feb00 rtrim to trim in sendnews paragraph.
;release  init      "2.3"            DLH 20Dec99 email variance message
;release  init      "2.2"            DLH 24Jun99 fix of Jose's bad boy
;RELEASE  INIT      "2.1"            ASH 30DEC98 NINORD Y2K, File expansion
;release  init      "2,0"             JD 17NOV98 updated for CC from TDMC.
;release  init      "1,9"             JD   16OCT98 added ship tracking var/ copy to \\nts2\fax
;release  init      "1,8"            DLH 15Jul98 y2 on ninshp.dat
;RELEASE         INIT      "1.7"            JD15apr98 auto send error report
;ELEASE  INIT      "1.6"            DLH 08DEC94 WRITE TO LOG FILE FOR LIST
;                                   MANAGEMENT ORDERS FOR FAXING OF INFO TO
;                                   BROKERS DAILY.
;RELEASE  INIT      "1.5"         JD 23NOV93 IF EXISTS UPDATE RECORD.
;RELEASE  INIT      "1.4"        JD 12APR93 SPOOL OUTPUT &
;                                turn off logfile.
;
;RELEASE  INIT      "1.3"        DLH 25FEB92  NEW INCLUDES NORDXX,NSHPXX
;                               UPDATE POSTAGE ON CHESHIRE & PS LABEL ORDERS.
;
;RELEASE  INIT      "1.2"        DLH 06SEP91
;                               ADDITION OF LOGFILE FOR DAILY SHIPPED REPORT.
;                               "APPLY TRIPLEX SHIPPING INFO"

INPUT    FILE      
SAVE     FILE      
;Start Patch #2.1 - increased file size
;LOGFILE  IFILE     KEYLEN=6,var=344
LOGFILE  IFILE     KEYLEN=6,var=498
;end Patch #2.1 - increased file size
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
;START PATCH 2.52 REPLACED LOGIC
;DELETE   init      "ERASE g:\DATA\SHIPINFO.SAV"
;REN      INIT      "REN g:\DATA\SHIPINFO.DAT SHIPINFO.SAV"
DELETE   DIM       100
REN      DIM       100
scost    dim       5
savefle  dim       35
         PACK      DELETE,"ERASE ",NTWKPATH1,"SHIPINFO.SAV"
         PACK      REN,"REN ",NTWKPATH1,"SHIPINFO.DAT SHIPINFO.SAV"
         PACK      savefle,NTWKPATH1,"SHIPINFO.dat"
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
         OPEN      INPUT,"shipinfo"
         OPEN      LOGFILE,"SHIPfax"         *TURNED On 12/08/94.
;
; 
         MOVE      "NSHP0003" TO PROGRAM
         MOVE      "APPLY TRIPLEX SHIPPING INFO" TO STITLE
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
;         SPLOPEN   "g:\DATA\TDMCSHP.LST"
         PACK   STR35,NTWKPATH1,"TDMCSHP.LST"
         SPLOPEN   STR35
;END PATCH 2.52 REPLACED LOGIC
         CALL      HEADER
;end patch 2.60
INPUT        cmatch    yes to newflag
         goto      input2 if equal
         READ      INPUT,SEQ;SLRNUM:
                   SINFO:
                   SCODE:
                   str8:               .date mm,dd,cc,yy
                   SPOST:
                   STR7:
                   strack
         GOTO      EOJ IF OVER
         MOVE      NO TO V
                              type      slrnum
         if        not equal
                              move      b55 to sinfo
                              move      b55 to str8
                              move      b55 to str7
                              move      b55 to strack
                              move      b55 to scost
                              move      yes to newflag
                              goto      input2
                              endif
         unpack    str8 into mm,dd,cc,yy
         pack      sdate from cc,yy,mm,dd
                              clear     spost
                              goto      sqty
INPUT2   READ      INPUT,SEQ;*edion=9,SLRNUM:
                                                             b55:
                                                             b55:
                                                             scost:
                                                             str7:
                   SINFO:
                   str10:               .date mm,dd,cc,yy
                   strack
         GOTO      EOJ IF OVER
;Patch2.601
                              scan       "TOTAL" in slrnum
                              goto input2 if equal
                              reset slrnum
                              scan       "Row" in slrnum
                              goto input2 if equal
                              reset slrnum
                              goto input2 if (slrnum = b6 or slrnum=" ")
;Patch2.601
         MOVE      NO TO V
         CMATCH    " " TO SLRNUM
         GOTO      INPUT2 IF EOS
                              move       "." to str1
        call       removechar using scost,str1
                      move       scost to spost
        unpack    str10 into mm,slash,dd,slash,cc,yy
         pack      sdate from cc,yy,mm,dd
;end patch 2.60
;begin patch 2.2
;         MOVE      STR7 TO SQUANT
SQTY
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
         MOVE      "T" TO SCODE
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
          move      str18,SRDATE
          move      "IS",SINITS
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
         PRINT     *L,*L,"^[D14153827088^[NAngelica Alvarado-TDMC  ^]",*n,032,hpreset:
                   hpttray:
                   hpport:
                   hpdupl:
                   033,"&l66P":               page length
                   033,"&l65F":
                   033,"&l1E",033,"&a0c0R":
                   *10,"TRIPLEX SHIPPING REPORT",*60,DATE:
                   *L,*60,"PAGE: ",PAGE,*L
         else          
          PRINT    *F,*L,*L,*L,*10,"TRIPLEX SHIPPING REPORT",*60,DATE:
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
;         path      exist,"c:\windows"
;         if        over
;START PATCH 2.52 REPLACED LOGIC
;         Execute   "c:\winnt\system32\cmd.exe /c copy g:\DATA\TDMCSHP.LST \\NINs2\Laser2 "
;         else
;         EXECUTE   "c:\command.com /c copy g:\DATA\TDMCSHP.LST \\NINs2\Laser2 "
         PACK      TASKNAME,"c:\winnt\system32\cmd.exe /c copy ",NTWKPATH1,"TDMCSHP.LST \\NINs2\Laser2 "
         Execute   TASKNAME
;         else
                    ElseIf              (osflag = c3 | osflag = c4)
         PACK      TASKNAME,"c:\command.com /c copy ",NTWKPATH1,"TDMCSHP.LST \\NINs2\Laser2 "
         EXECUTE   TASKNAME
                    Elseif              (osflag = c6)
         PACK      TASKNAME,"c:\windows\system32\cmd.exe /c copy ",NTWKPATH1,"TDMCSHP.LST \\NINs2\Laser2 "
         Execute   TASKNAME
;end patch 2.57
;END PATCH 2.52 REPLACED LOGIC
         endif
         cmatch    yes to faxflag
         if         equal
;begin patch 2.53
;         path      exist,"c:\windows"
;         if        over
;begin patch 2.57
;         if        (osflag = c2)
         if        (osflag = c1 | osflag = C5)
;end patch 2.53
;START PATCH 2.52 REPLACED LOGIC
;         Execute   "c:\winnt\system32\cmd.exe /c copy /b g:\DATA\TDMCSHP.LST \\nts2\fax "
;         else
;         EXECUTE   "c:\command.com /c copy g:\DATA\TDMCSHP.LST \\nts2\fax "
         PACK      TASKNAME,"c:\winnt\system32\cmd.exe /c copy /b ",NTWKPATH1,"TDMCSHP.LST \\NINS2\fax "
         Execute   TASKNAME
;         else
         Elseif        (osflag = c3 | osflag = C4)
         PACK      TASKNAME,"c:\command.com /c copy ",NTWKPATH1,"TDMCSHP.LST \\NINS2\fax "
         EXECUTE   TASKNAME
         Elseif        (osflag = c6)
         PACK      TASKNAME,"c:\windows\system32\cmd.exe /c copy /b ",NTWKPATH1,"TDMCSHP.LST \\NINS2\fax "
         Execute   TASKNAME
;end patch 2.57
;END PATCH 2.52 REPLACED LOGIC
         endif
         DISPLAY   *P1:24,*R,*EL,"Faxing copy TDMC ",*W5
         endif
;         OPEN       SAVE,"g:\DATA\SHIPINFO.SAV",EXCLUSIVE
;         CLOSE      SAVE,DELETE         
         DISPLAY   *P1:24,*R,*EL,"COPING FILE TO .SAV",*B,*W5
;begin patch 2.53
;         path      exist,"c:\windows"
;         if        over
;         if         (osflag = c2)
;end patch 2.53
;START PATCH 2.52 REPLACED LOGIC
;         Execute   "c:\winnt\system32\cmd.exe /c copy /b g:\data\SHIPINFO.DAT g:\data\shipinfo.SAV/Y"
;         else
;         EXECUTE   "c:\command.com /c copy g:\DATA\SHIPINFO.DAT g:\data\shipinfo.SAV/Y"
;         PACK      TASKNAME,"c:\winnt\system32\cmd.exe /c copy /b ",NTWKPATH1,"SHIPINFO.DAT ",NTWKPATH1,"shipinfo.SAV/Y"
;         Execute   TASKNAME
;         else
;         PACK      TASKNAME,"c:\command.com /c copy ",NTWKPATH1,"SHIPINFO.DAT ",NTWKPATH1,"shipinfo.SAV/Y"
;         EXECUTE   TASKNAME
;END PATCH 2.52 REPLACED LOGIC
;         endif
;         DISPLAY   *P1:24,*R,*EL,"COPIED FILE ",*W5
;START PATCH 2.52 REPLACED LOGIC
;         erase      "g:\data\shipinfo.dat"
         PACK       STR35,NTWKPATH1,"SHIPINFO.sav"
         erase      STR35
         rename     savefle,str35
;END PATCH 2.52 REPLACED LOGIC
;         open      input,"g:\data\shipinfo.dat",exclusive
;         close     input,delete
DONE     STOP
;...........................
sendnews
;START PATCH 2.5 REPLACED LOGIC
;        MOVE      ococode,n3
;.
;         LOAD      str35 FROM N3 OF ocnt1,ocnt2,ocnt3,ocnt4,ocnt5,ocnt6,ocnt7:
;                   ocnt8,ocnt9,ocnt10,ocnt11,ocnt12,ocnt13,ocnt14,ocnt15,ocnt16,ocnt17
;
;         SCAN      "(" IN str35
;         clear      str25
;         MOVEFPTR  str35 TO LAST
;         sub       c1 from last
;         SETLPTR   str35 TO LAST
;         RESET     STR35
;         APPEND    str35 TO str25
;
;         reset      str25
;         clear       str1
;         append      str25 to str1         ..1st init
;         reset        str1
;         scan       b1 in str25
;         bump       str25 by 1
;         clear      str6
;         append     str25 to str6
;         reset      str6
;         call       trim using str6
;.         call       rtrim using str6
;         pack       str7 from str1,str6
;         RESET      STR25
;.........
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
;START PATCH 2.51 - REPLACED LOGIC
;        clear   str6
;        append  CNTNAME,str6
;        reset   str6
        move    CNTNAME,str7
        call    RemoveChar using str7,B1
        move    str7,str6
;END PATCH 2.51 - REPLACED LOGIC
        call    trim using str6
        pack    str7 from str1,str6
        RESET   CNTNAME
        move    cntname to str35
;END PATCH 2.5 REPLACED LOGIC
 
      Move    "This is a Informational e-mail from  the Triplex shipping program",SmtpSubject Subject
;   Set the text message that is send with the attachments

       Move    "This is a Informational e-mail from  the Triplex shipping program",SmtpTextMessage(1)   Array <Text message >
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
         Move    "10.10.30.74",SmtpEmailServer                   Address of email serverc
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
                              Include   COMPIO.inc
                              include   CNTIO.INC
.         INCLUDE   NMLRIO.inc
;PATCH2.8
         INCLUDE   NSHPIO.inc
         INCLUDE   NORDIO.inc
;START PATCH 2.5 ADDED LOGIC
         include   NCNTIO.inc
;END PATCH 2.5 ADDED LOGIC
         INCLUDE   COMLOGIC.inc

