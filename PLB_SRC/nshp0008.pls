PC       EQU       0
          INC        COMMON.inc
          INC        CONS.inc
          INC        NSHPDD.inc
          inc        hp.inc
.,Patch1.1
          include   compdd.inc
          include   cntdd.inc
.         INC        NMLRDD.inc
.,Patch1.1
          INC        NORDDD.inc
          INC        NCNTDD.inc
          INC        winapi.inc
last      FORM      3

release   INIT      "1.3"              DLH SendMail
RelDate   INit      "24 April 2008"
.release  INIT      "1.2"              DMB 01JUN2005 Add code to verify if records has changed if dupe
.,release INIT      "1.1"              DMB 26MAY2004 Mailer Conversion 
.,release INIT      "1.0"              DMB 23FRI2003 
INPUT     FILE      
SAVE      FILE      
LOGFILE   IFILE     KEYLEN=6,var=498
COUNT     FORM      4
UPDATE    FORM      4
WRITE     FORM      4
UPD       FORM      4
DIFF      FORM      10
TENPER    FORM      10
KEY       DIM       6
LINES     FORM      2
FIFTY4    FORM      "54"
PAGE      FORM      2
DATE      DIM       8
SALES     DIM       2
HOLDMKEY  DIM       7
BRKFLAG   FORM      1            0=LIST MANAGEMENT
BCNAME    DIM       25
CAREOF    INIT      "C/O"
DATE1     INIT      "99/99/99"
LISTINFO  DIM       36

DELETE    DIM       100
REN       DIM       100
SAVEFLE   DIM       35

.>Patch 1.2 
TEMPSLRNUM      DIM       6           LR NUMBER
TEMPSINFO       DIM       36          SHIPPING TEXT EXPLANATION
TEMPSCODE       DIM       1           HOW SHIP INFO WAS RECEIVED (C,P,S,I,R)
TEMPSDATE       DIM       8           SHIPMENT DATE     ccyymmdd
TEMPSPOST           DIM       4           SHIPPING COST
TEMPSQUANT          DIM       9           SHIP QUANTITY
TEMPStrack      DIM       25          Tracking number
TEMPSINITS          DIM       3           Initials of person who created record
TEMPSRDATE          DIM       8           Date record was created
TEMPSPINITS         DIM       3           Initials of person who Printed record
TEMPSPDATE          DIM       8           Date record was Printed
TEMPSFILLER     DIM       25
.>Patch 1.2




          PACK      DELETE,"ERASE ",NTWKPATH1,"TARGETSHIPINFO.SAV"
          PACK      REN,"REN ",NTWKPATH1,"TARGETSHIPINFO.DAT TARGETSHIPINFO.SAV"
          PACK      SAVEFLE,NTWKPATH1,"TARGETSHIPINFO.dat"
Var       DIM       1
FAXFLAG   DIM       1
OKSTATS   INIT      "0B"
          OPEN      INPUT,"TARGETSHIPINFO"
          OPEN      LOGFILE,"SHIPFAX"
          MOVE      "NSHP0008" TO PROGRAM
        MOVE    "APPLY SOI SHIPPING INFO" TO STITLE
          MOVE      "Names In The News Ca." TO COMPNME
          MOVE      C0 TO PAGE
          CLOCK     DATE TO DATE
          clock     timestamp to str18
          unpack    str18 to str2,str16
          move      str2 to cc
          call      GetWinVer
          CALL      PAINT
          MOVE      C1 TO NORDPATH
          PACK      STR35,NTWKPATH1,"targs.LST"
          SPLOPEN   STR35
          CALL      HEADER
INPUT     
.>Patch 1.2 Comment Out
.         READ      INPUT,SEQ;SLRNUM:                       ;6 bytes  1-6
.                   LISTINFO:                     ;37 bytes           7-42
.                   sdate:                        ;date cc,yy,mm,dd   43-50
.                   STR13:              ;shipping qty soon to be squant 51-63
.                   STR6:                         ;SINFO    64-69
.                   strack                        ;         70-94
.>Patch 1.2 Comment Out
.>Patch 1.2 Change Vars
          READ      INPUT,SEQ;TEMPSLRNUM:                       ;6 bytes        1-6
                    LISTINFO:                     ;37 bytes           7-42
                    TEMPSDATE:                    ;date cc,yy,mm,dd   43-50
                    STR13:              ;shipping qty soon to be squant 51-63
                    STR6:                         ;SINFO    64-69
                    TEMPSTRACK                              ;         70-94
.>Patch 1.2 Change Vars



.>Patch 1.2 Modfied Var
          GOTO      EOJ IF OVER
          call      trim using str6
.>Patch 1.2 Modfied Var       
.         move      str6 to SINFO
          move      str6 to TEMPSINFO
.>Patch 1.2 Modfied Var       
          MOVE      NO TO V
.>Patch 1.2 Modfied Var                 
.         CMATCH    " " TO SLRNUM
          CMATCH    " " TO TEMPSLRNUM
.>Patch 1.2 Modfied Var                 
          GOTO      INPUT IF EOS
.         unpack    str8 into mm,dd,cc,yy
.         pack      sdate from cc,yy,mm,dd
.>Patch 1.2 Modfied Var                           
.         clear     squant
          clear     TEMPSQUANT
.>Patch 1.2 Modfied Var                           
          call      trim using str13
          count     n9,str13
.field is 13 bytes and squant is only 9 if this occurs do not apply   
          goto      badqty if (n9 > c9)
.>Patch 1.2 Modfied Var                 
.         move      str13 to squant
          move      str13 to TEMPSQUANT
.         call      zfillit using squant
          call      zfillit using TEMPSQUANT
.         pack      squant from c0,c0,str7
.        TYPE       SQUANT
        TYPE        TEMPSQUANT
.>Patch 1.2 Modfied Var               
        GOTO        BADQTY IF NOT EQUAL
.         CLEAR     SPOST                   *2/6/91 DO NOT APPPLY TDMC $
.>Patch 1.2 Modfied Var       
.         REP       ZFILL IN SLRNUM
          REP       ZFILL IN TEMPSLRNUM
.>Patch 1.2 Modfied Var                 
          MOVE      "A" TO TEMPSCODE
          ADD       C1 TO COUNT
          DISPLAY   *P10:12,"RECORDS IN : ",COUNT
.>Patch 1.2 Modfied Var                           
.         MOVE      SLRNUM TO NORDFLD
          MOVE      TEMPSLRNUM TO NORDFLD
.>Patch 1.2 Modfied Var                           
          CALL      NORDKEY
          GOTO      NOORD IF OVER
          RESET     OKSTATS
          SCAN      OSTAT IN OKSTATS
          GOTO      NOLIVE IF NOT EQUAL
          
.         MOVE      C0 TO N2
.         MOVE      OFOCODE TO N2
.         BRANCH    N2 OF NOPOST,POST,POST,POST,POST,NOPOST,NOPOST,NOPOST:
.         NOPOST
.NOPOST   
.         CLEAR     SPOST
POST
        MOVE        C0 TO N10
        MOVE        OQTY TO N10
        DIV         C10 INTO N10
        MOVE        N10 TO TENPER
.>Patch 1.2 Modfied Var               
.        MOVE       SQUANT TO N10
        MOVE        TEMPSQUANT TO N10
.>Patch 1.2 Modfied Var               
        MOVE        OQTY TO DIFF
          SUB       N10 FROM DIFF
          COMPARE   C0 TO DIFF
          CALL      NEG IF LESS
          COMPARE   DIFF TO TENPER
          CALL      VAR IF NOT GREATER
.>Patch 1.2 Modfied Var                 
.         move      str18,SRDATE
          move      str18,TEMPSRDATE
.         move      "IS",SINITS
          move      "IS",TEMPSINITS
.        MOVE       SLRNUM TO NSHPFLD
        MOVE        TEMPSLRNUM TO NSHPFLD
.>Patch 1.2 Modfied Var                       
        CALL        NSHPTST
        GOTO        DUPE IF NOT OVER
.>Patch 1.2 Added Routine to move variables
          call MoveShipVars
.>Patch 1.2
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
.
DUPE
.>Patch 1.2 Code Added Duplication Verification
          call  NSHPKEY
          MATCH SCODE,TEMPSCODE                  HOW SHIP INFO WAS RECEIVED (C,P,S,I,R,A)
          if not equal
                    call MoveShipVars
                    Goto UpdateShipping
          endif
          if (TEMPSDATE <> "")          
                    MATCH SDATE,TEMPSDATE                  SHIPMENT DATE     ccyymmdd
                    if not equal
                              call MoveShipVars
                              Goto UpdateShipping
                    endif     
          endif
.         MATCH SPOST,TEMPSPOST                    SHIPPING COST
.         if not equal
.                   call MoveShipVars
.                   Goto UpdateShipping
.         endif     
          if (TEMPSQUANT <> "")
                    MATCH SQUANT,TEMPSQUANT                  SHIP QUANTITY
                    if not equal
                              call MoveShipVars
                              Goto UpdateShipping
                    endif     
          endif
          if (TEMPSTRACK <> "")         
                    MATCH STRACK,TEMPStrack       
                    if not equal
                              call MoveShipVars
                              Goto UpdateShipping
                    endif     
          endif
          GOTO INPUT
.>Patch 1.2

.>Patch 1.2 Added Label
UpdateShipping                
.>Patch 1.2
          call      nshpupd
          ADD       C1 TO UPD
          DISPLAY   *P10:14,"RECORDS UPDATED : ",UPD
          COMPARE   LINES TO FIFTY4
          CALL      HEADER IF LESS
          PRINT     *L,*10,"LR ## ",SLRNUM," RECORD UPDATED"
          ADD       C2 TO LINES
.dh temp? 24jun99
        goto        writelog
        GOTO        INPUT
VAR
          COMPARE   LINES TO FIFTY4
          CALL      HEADER IF LESS
.>Patch 1.2 Modfied Var                 
.         PRINT     *L,*10,"LR ## ",SLRNUM," VARIES > +/- 10%":
.                   "ORDER QTY ",OQTY," Target A QTY ",SQUANT
          PRINT     *L,*10,"LR ## ",TEMPSLRNUM," VARIES > +/- 10%":
                    "ORDER QTY ",OQTY," Target A QTY ",TEMPSQUANT
.>Patch 1.2 Modfied Var                           
                    ADD       C2 TO LINES
                    move      yes to faxflag
                    MOVE      YES TO V
                    call      sendnews
          RETURN
NOORD
          COMPARE   LINES TO FIFTY4
          CALL      HEADER IF LESS
.>Patch 1.2 Modfied Var                 
.         PRINT     *L,*10,"LR ## ",SLRNUM," NO ORDER FOUND ",SCODE:
.                   " ",SQUANT," ",SPOST,*L,*10,SINFO
          PRINT     *L,*10,"LR ## ",TEMPSLRNUM," NO ORDER FOUND ",TEMPSCODE:
                    " ",TEMPSQUANT," ",TEMPSPOST,*L,*10,TEMPSINFO
.>Patch 1.2 Modfied Var                           
          ADD       C3 TO LINES
          GOTO      INPUT
NOlive
          COMPARE   LINES TO FIFTY4
          CALL      HEADER IF LESS
.>Patch 1.2 Modfied Var                 
.         PRINT     *L,*10,"LR ## ",SLRNUM," NOT A LIVE ORDER! ",SCODE:
.                   " ",SQUANT," ",SPOST,*L,*10,SINFO
          PRINT     *L,*10,"LR ## ",TEMPSLRNUM," NOT A LIVE ORDER! ",TEMPSCODE:
                    " ",TEMPSQUANT," ",TEMPSPOST,*L,*10,TEMPSINFO
.>Patch 1.2 Modfied Var                           
          ADD       C3 TO LINES
          GOTO      INPUT
HEADER   
          ADD       C1 TO PAGE
.         compare   c1 to page
.         if        equal
.                   PRINT     *L,*L,"^[D14153827088^[NAngelica Alvarado-TDMC  ^]",*n,032,hpreset:
.                   hpttray:
.                   hpport:
.                   hpdupl:
.                   033,"&l66P":                         page length
.                   033,"&l65F":
.                   033,"&l1E",033,"&a0c0R":
.                   *10,"TRIPLEX SHIPPING REPORT",*60,DATE:
.                   *L,*60,"PAGE: ",PAGE,*L
.         else          
          PRINT    *F,*L,*L,*L,*10,"Target Analysis Shipping Report",*60,DATE:
                    *L,*60,"PAGE: ",PAGE,*L
.         endif          
          MOVE      C6 TO LINES
          RETURN
BADQTY
          COMPARE   LINES TO FIFTY4
          CALL      HEADER IF LESS
.>Patch 1.2 Modfied Var                 
.         PRINT     *L,*10,"LR ## ",SLRNUM," QUANTITY NOT NUMERIC OR TOO LARGE"
          PRINT     *L,*10,"LR ## ",TEMPSLRNUM," QUANTITY NOT NUMERIC OR TOO LARGE"
.>Patch 1.2 Modfied Var                 
          ADD       C2 TO LINES
          GOTO      INPUT

NEG
          MULT      SEQ BY DIFF
          RETURN
WRITELOG
          PACK      MKEY FROM OMLRNUM,OCOBN
          MATCH     MKEY TO HOLDMKEY
          IF        NOT EQUAL
                    CALL    NMLRKEY
                    MOVE    MKEY TO HOLDMKEY
          ENDIF
        MOVE        C0 TO N2
        PACK        SALES FROM OSALES10,OSALES
        MOVE        SALES TO N2
          COMPARE   C6 TO N2
        goto        wrtalph2 if equal
          COMPARE   "19" TO N2
          goto      wrtalph2 if equal
          GOTO      INPUT
WRTALPH2 
          FILEPI 3;LOGFILE
                    READ      LOGFILE,NORDFLD;;
                    GOTO      INPUT IF NOT OVER
                    WRITE     LOGFILE,NORDFLD;ORDVARS
                    GOTO      INPUT
EOJ      
          CLOSE     INPUT
          COMPARE   C0 TO PAGE
          CALL      HEADER IF EQUAL
          COMPARE   LINES TO FIFTY4
          CALL      HEADER IF LESS
          PRINT     *L,*10,"NUMBER OF RECORDS RECEIVED: ",COUNT
          PRINT     *FLUSH
          RELEASE
          SPLCLOSE 
.begin patch 2.57
        CALL                getwinver
        If                  (osflag = c1 | osflag = c5)
                    PACK      TASKNAME,"c:\winnt\system32\cmd.exe /c copy ",NTWKPATH1,"targs.LST \\NINs2\Laser2 "
                    Execute   TASKNAME
        ElseIf              (osflag = c3 | osflag = c4)
                    PACK      TASKNAME,"c:\command.com /c copy ",NTWKPATH1,"targs.LST \\NINs2\Laser2 "
                    EXECUTE   TASKNAME
        Elseif              (osflag = c6)
                    PACK      TASKNAME,"c:\windows\system32\cmd.exe /c copy ",NTWKPATH1,"targs.LST \\NINs2\Laser2 "
                    Execute   TASKNAME
          Endif
.         cmatch    yes to faxflag
.         if        equal
.                   if        (osflag = c1 | osflag = C5)
.                             PACK      TASKNAME,"c:\winnt\system32\cmd.exe /c copy /b ",NTWKPATH1,"TDMCSHP.LST \\nts2\fax "
.                             Execute   TASKNAME
.                   Elseif    (osflag = c3 | osflag = C4)
.                             PACK      TASKNAME,"c:\command.com /c copy ",NTWKPATH1,"TDMCSHP.LST \\nts2\fax "
.                             EXECUTE   TASKNAME
.                   Elseif    (osflag = c6)
.                             PACK      TASKNAME,"c:\windows\system32\cmd.exe /c copy /b ",NTWKPATH1,"TDMCSHP.LST \\nts2\fax "
.                             Execute   TASKNAME
.                   endif
.                   DISPLAY   *P1:24,*R,*EL,"Faxing copy TDMC ",*W5
.         endif
          DISPLAY   *P1:24,*R,*EL,"COPING FILE TO .SAV",*B,*W5
          PACK      STR35,NTWKPATH1,"TARGETSHIPINFO.SAV"
          erase     STR35
          rename    savefle,str35
DONE     STOP
............................
sendnews
          pack      NCNTFLD,OCOCODE
          move      "SVCREP-NCNTKEY",Location
          pack      KeyLocation,"Key: ",NCNTFLD
          clear     cntname
          clear     str35
          call      NCNTKEY
                    clear     str1
                    append    CNTNAME,str1         ..1st init
                    reset     str1
                    scan      B1,CNTNAME
                    bump      CNTNAME,1
                    move      CNTNAME,str7
                    call      RemoveChar using str7,B1
                    move      str7,str6
                    call      trim using str6
                    pack      str7 from str1,str6
                    RESET     CNTNAME
                    move      cntname to str35
            Move    "This is a Informational e-mail from  the Target Analysis shipping program",MailSubjct
          Clear     MailBody
          Append    "This is a Informational e-mail from  the Target Analysis shipping program",MailBody
          append    CRLF,MailBody
            append  "record## ",MailBody
          append    olrn,MailBody
          append    b1,MailBody
          append    CRLF,MailBody
          append    " Your above LR had a shipping qty variance:",MailBody
          append    CRLF,MailBody
          append    "Order qty ",MailBody
          append    oqty,MailBody
          append    ", Shipped qty ",MailBody
.>Patch 1.2 Modfied Var                           
.                    append   squant,str55
          append    TEMPsquant,MailBody
.>Patch 1.2 Modfied Var                           
          append    CRLF,MailBody
          Append    "Please review & correct as necessary.",MailBody
          REset     MailBOdy
          Pack      MailTO From str7,"@nincal.com"
          Pack      MailFrom From str7,"@nincal.com"
          Call      SendMail
          winshow
          return
.>Patch 1.2
MoveShipVars
          MOVE TEMPSLRNUM,SLRNUM             LR NUMBER
          REP  ZFILL IN SLRNUM          
          MOVE TEMPSINFO,SINFO               SHIPPING TEXT EXPLANATION
          MOVE TEMPSCODE,SCODE               HOW SHIP INFO WAS RECEIVED (C,P,S,I,R)
          MOVE TEMPSDATE,SDATE               SHIPMENT DATE     ccyymmdd
          MOVE TEMPSPOST,SPOST                     SHIPPING COST
          MOVE TEMPSQUANT,SQUANT                   SHIP QUANTITY
          call ZFILLIT using SQUANT     
          MOVE TEMPStrack,Strack             Tracking number
          MOVE TEMPSINITS,SINITS                   Initials of person who created record
          MOVE TEMPSRDATE,SRDATE                   Date record was created
          return
.>Patch 1.2
.Patch1.1
          include   compio.inc
          include   cntio.inc
.                    INCLUDE  NMLRIO.inc
.patch1.1
                    INCLUDE   NSHPIO.inc
                    INCLUDE   NORDIO.inc
                    include   NCNTIO.inc
                    INCLUDE   COMLOGIC.inc

