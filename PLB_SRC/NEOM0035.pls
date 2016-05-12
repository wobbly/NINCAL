PC        EQU       0
          include common.inc
          include cons.inc
.Patch1.1
                              include   compdd.inc
                              include   cntdd.inc
.         include nmlrdd.inc
.Patch1.1
          include norddd.inc
.Patch1.1
.         include nbrkdd.inc
.Patch1.1
.begin patch 1.3
          include   Ndatdd.inc
.end patch 1.3
          


release  init      "1.3"        DLH     add more documentation and information when no records
Reldate   Init      "2013 June 3"
.release  init      "1.2"        DMB     05OCT2004 Added Code for EOQ reports
.release  init      "1.1"        DMB    26MAY2004 Mailer Conversion 
.release             init                "1.0"    28Aug2003 Automation of Monday.

RECMST    FILE      VAR=498,COMP         .was 328
EOMLIST   FILE      
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
KEYPAD1  INIT      "01L"
KEYPAD2  INIT      "02L"
KEYPAD4  INIT      "04L"
QUES     INIT      "??????"
LSTNO    DIM       6              LIST NUMBER SEARCH FIELD
FOMDATE FORM        5         First of the MONTH COnverted to Julian
EOMDATE  FORM       5         EOM DATE CONVERTED TO JULIAN
.Vars Added Patch 1.2
FOQDATE FORM        5         First of the MONTH COnverted to Julian
QMM       DIM       2
.Vars Added Patch 1.2

.EOMLIST FILE VARIABLES
NUMBER    DIM       6         1-6 LIST or Mailer NUMBER
ITEM      FORM      1         7-7    1=list,2=comselect order, 3=mailer
BYDATES   DIM       1         8-8   USE DATES A=all, M=Maildates ' '=orderdates
REPTYPE   FORM      1         9-9
.1 =   NIN LIST BOOK - ALPHA BY MAILER NAME, WITH TOTALS
.2 =               NIN LIST BOOK - NUMERIC BY OFFER
.3 =               NIN MAILER BOOK - ALPHA BY LIST NAME
COPIES    FORM      1        10-10
.Patch1.2
REPNAME   DIM       1        11-11            Q=QUARTERLY
.Patch1.2
RECIPIENT DIM       255      12-266 

        CMATCH    B1 TO PROGRAM       .CHAINED FROM DSINIT WITH INFO?
        IF    NOT    EOS                 .YES
                              UNPACK TODAY,MM,SLASH,DD,SLASH,YY
                              CALL CVTJUL
                              MOVE JULDAYS TO EOMDATE
                              call cvtgreg
                              MOVE "01" TO dd
                              CALL CVTJUL
                              MOVE JULDAYS TO FOMDATE
                              move FOMDATE to juldays
                              call      cvtgreg   
                              pack      str8,mm,slash,dd,slash,yy                 
                              move EOMDATE to juldays
                              call      cvtgreg   
                              pack      str10,mm,slash,dd,slash,yy                 
                    display   *P6:8,"The dates for the pick are ",str8,b1,dash,b1,str10
.Patch 1.2
                              UNPACK TODAY,MM,SLASH,DD,SLASH,YY
                              if (mm = "03" or mm = "06" or mm="09" or mm="12")
                                        move mm to n2
                                        sub c2 from n2
                                        move n2 to mm
                                        MOVE "01" TO dd
                                        CALL CVTJUL
                                        MOVE JULDAYS TO FOQDATE
                                        move FOQDATE to juldays
                                        call      cvtgreg   
                                        pack      str8,mm,slash,dd,slash,yy   
            display   *P6:10,"This is also a quarter run."
            display   *P6:12,"Dates for the Quarter pick are ",str8,b1,dash,b1,str10
                              endif
                    call                paint

.Patch 1.2

          ELSE   
      shutdown  "cls"      
                    STOP
        ENDIF

LISTVARS varlist NUMBER:
          ITEM:
          BYDATES:
          REPTYPE:
          COPIES:
          REPNAME:                                .For quarter report .Patch 1.2
          RECIPIENT


          Open EOMLIST,"EOMLIST.DAT",READ

FileCreate
.Creation of Diskin File
          MOVE      "59",FILENUM
Readit
          READ      EOMLIST,SEQ;LISTVARS
          If over
                    Shutdown  "cls"
                    STOP 
          endif
....testing
          if        (number = "006005")            .turned off per susan Nov 1 2013. left in eom file as example
          goto      readit
          endif
          if        (number = "002479")
          goto      readit
          endif
....testing

.Patch 1.2 Logic Added
          if (repname = "Q")
                    UNPACK TODAY,MM,SLASH,DD,SLASH,YY
                    if (mm = "03" or mm = "06" or mm="09" or mm="12")
                    else
                              goto readit
                    endif
          endif
.Patch 1.2 Logic Added
          Goto      Addfile
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
          APPEND    "\\nins1\e\data\",RECNAME                              ."
          APPEND    NEWNAME,RECNAME
          XIF
          RESET     RECNAME
          MOVE      B1 TO ERROR
          PREPARE   RECMST,RECNAME
          PREPARE   RECMST,RECNAME,CREATE
.newname is our diskin
          Display   *P6:14,"The Diskin I am going to use is: ",newname
          MOVE      C1 TO NORDPATH

        Branch ITEM,NEXTLIST,NEXTComselect,NextMailer
          GOTO      READIT
          

NextList

          CLEAR     NORDFLD2
          CLEAR     NORDFLD1
          CLEAR     NORDFLD3
          clear     nordfld4
          move      str10 to lstno
          PACK      NORDFLD1 FROM KEYPAD1,QUES
          PACK      NORDFLD2 FROM KEYPAD2,NUMBER
          MOVE      C2 TO NORDPATH
          CALL      NORDAIM
          GOTO      readit if over
          goto      CHKDATES
ListSearch
          call NORDKG
        goto batchit if over
          add c1 to n9
          Display   *P6:16,"Records I have Read   ",N9
        goto CHKDATES
.;;
NextComselect
         MOVE      "550000" TO NORDFLD
         MOVE      C1 TO NORDPATH
         CALL      NORDKEY
         GOTO      readit IF OVER
         CMATCH    "C",OCOMSLCT
         GOTO      CHKDATES IF EQUAL
         GOTO      ComselectSearch
ComselectSearch
         MOVE      C1 TO NORDPATH
         CALL      NORDKS
         GOTO      BATCHIT IF OVER
         add c1 to n9
           Display  *P6:16,"Records I have Read   ",N9
         CMATCH    "C",OCOMSLCT
         GOTO      CHKDATES IF EQUAL
         GOTO      ComselectSearch
NextMailer
        Clear NORDFLD1
        Clear NORDFLD2
        Clear NORDFLD3
        Clear NORDFLD4
          bump number,2
        move  number to str4
          PACK  NORDFLD1 FROM KEYPAD1,str4
          MOVE  C2 TO NORDPATH
        CALL  NORDAIM
        goto  READIT if over
        goto  CHKDATES
MailerSearch
        call NORDKG
        GOTO BATCHIT IF OVER
        add c1 to n9
        Display     *P6:16,"Records I have Read   ",N9
        goto CHKDATES


CHKDATES
.get date from order
          GOTO      WRITEIT IF (bydates = "A")
          IF (bydates = "M")
                    MOVE      OMDTEM,MM
                    MOVE      OMDTEY,YY
                    MOVE      OMDTED,DD
          else
                    MOVE      OODTEM,MM
                    MOVE      OODTEY,YY
                    MOVE      OODTED,DD
          endif
          call      cvtjul
.Patch 1.2 Logic Added
          if (repname = "Q")
                    COMPARE   FOQDATE TO juldays
          else
                    COMPARE   FOMDATE TO juldays
          endif
.Patch 1.2 Logic Added
          GOTO      RECYCLE IF LESS              NO. TRY NEXT ORDER.
          COMPARE   juldays TO EOMDATE
          GOTO      RECYCLE IF LESS              NO. TRY NEXT ORDER.
        CMATCH    "p" TO OSTAT       Pending order ?
        GOTO      RECYCLE IF EQUAL     YES, skip.
        CMATCH    "x" TO OSTAT       Cancelled Pending order ?
        GOTO      RECYCLE IF EQUAL     YES, skip.
        CMATCH    "l" TO OSTAT       lcr order ?
        GOTO      RECYCLE IF EQUAL     YES, skip.
        CMATCH    "z" TO OSTAT       Cancelled lcr order ?
        GOTO      RECYCLE IF EQUAL     YES, skip.
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

RECYCLE
        Branch item,ListSearch,ComselectSearch,MailerSearch



         
BATCHIT
          Close     RECMST
.          MOve      "DHerric",RECIPIENT
          if (found > c0)
.Regular
                    IF (reptype = C1)
                              PACK      TASKNAME,"\\nins1\winbatch\butil job=O24 INfile=",NEWNAME," C=",copies," B=",RECIPIENT," PA=1 ","PDF=Y P="
                    ELSEIF (reptype = C2)
.NUMERIC
                              PACK      TASKNAME,"\\nins1\winbatch\butil job=O22 INfile=",NEWNAME," C=",copies," B=",RECIPIENT," PA=1 ","PDF=Y P="
                    ELSEIF (reptype = C3)
                              PACK      TASKNAME,"\\nins1\winbatch\butil job=O5 INfile=",NEWNAME," C=",copies," B=",RECIPIENT," PA=1 ","PDF=Y P="
                    ENDIF
                    EXECUTE   TASKNAME
          else
                    call nogoodies

          endif
          move c0 to found
          move c0 to n9
          goto readit
Ender
   shutdown  "cls"
          stop
NoGoodies
.begin patch 1.3
          if        (item = 1)
          packkey   Ndatfld from number
          rep       zfill,ndatfld
          call      ndatkey
                    pack taskname,"There is no EOM usage for this list: ", NUMBER,b1,mlstname
          elseif    (item = 2)
                    pack taskname,"There is no EOM usage for Consumer Direct: "
          else
.number was bumped 2 previously
          packkey   mkey from number
          rep       zfill,mkey
          call      nmlrkey
                    pack taskname,"There is no EOM usage for this Mailer: ", str4,b1,mcomp
          endif
          
.end patch 1.3
                    move    taskname,MailSubjct
                    Clear     Mailbody
                    append    Taskname,Mailbody
                    append    CRLF,Mailbody
                    reset     Mailbody
                    pack      Mailto from "Creques@nincal.com"
                    pack      MailFRom from Recipient,"@nincal.com"
                    call      Sendmail
                    return
.Patch1.1
                              include   compio.inc
                              include   cntio.inc
.begin patch 1.3
          include   Ndatio.inc
.end patch 1.3
.         include nmlrio.inc
.Patch1.1
          include nordio.inc
.Patch1.1
.         include nbrkio.inc
.Patch1.1
          include comlogic.inc
