PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
.patch1.6
                                        include   compdd.inc
                                        include   cntdd.inc
.         INCLUDE   NMLRDD.inc
.patch1.6
         INCLUDE   NORDDD.inc
         INCLUDE   HP.INC
.START PATCH 1.4 REPLACED LOGIC
.         INCLUDE   CONTACT1.INC
         INCLUDE   NCNTDD.INC
.END PATCH 1.4 REPLACED LOGIC
.START PATCH 1.2 - ADDED LOGIC
         INCLUDE   NOFRDD.INC
.END PATCH 1.2 - ADDED LOGIC
.START PATCH 1.5 - ADDED LOGIC
         INCLUDE   NSEL2DD.INC
.END PATCH 1.5 - ADDED LOGIC

release  init      "2.11"        DLH    Replace Reuben with Amy & Jennifer C, change email to HTML
Reldate   Init      "2015 May 18"     
.release  init      "2.1"        DLH    Send Emails cleanup
.Reldate   Init      "2013 April 16"     
.release  init      "2.0"        DLH    Send Emails - no print
.Reldate   Init      "09 September 2010"     
.release  init      "1.8"        DLH    remove PLI, Don't print if no records
.Reldate   Init      "05 March 2010"     
.release  init      "1.7"        ASH    19JUN2007 PLI Inclusion
.release  init      "1.6"        DMB    26MAY2004 Mailer Conversion
.RELEASE  INIT      "1.5"          29jan04 ASH DATACARD CONVERSION
.RELEASE  INIT      "1.4"          ASH 16MAR2000 REPLACED CONTACT1.INC WITH NCNTDD.INC
.RELEASE  INIT      "1.4"          ASH 16MAR2000 REPLACED CONTACT1.INC WITH NCNTDD.INC
.RELEASE  INIT      "1.3"          ASH 07MAY99 REPLACED OODES{NINORD.DAT} --> OFDESC{NINOFR.DAT}
.RELEASE  INIT      "1.2"          ASH 07MAY99 REPLACED OODES{NINORD.DAT} --> OFDESC{NINOFR.DAT}
.RELEASE  INIT      "1.1"          ASH 30DEC98 NINORD Y2K, File expansion
.RELEASE  INIT      "1.0"          D.L.H.  26may94
.Start patch #1.1 - increased file size
.NINORD   FILE      FIXED=566
INPFILE   FILE      FIXED=696
.end patch #1.1 - increased file size
.
DATE     DIM       8
. 
. OTHER  VARIABLES.
. ....................
.
.Start Patch #1.1 - new var to replace N7
NUM9     FORM      9
.End Patch #1.1 - new var to replace N7
result1  form      3
remain   form      3.1
.Start Patch #1.1 - expanded vars
.CONTACT  DIM       25
CONTACT  DIM       35
.Start Patch #1.1 - expanded vars
.HOLDCNT  DIM       1
.savCNcT  DIM       1
HOLDCNT  DIM       2
savCNcT  DIM       2
.Start Patch #1.1 - expanded vars
TOTAL    FORM      5
TOTQTY   FORM      9
QTYMASK  INIT      "ZZZ,ZZZ,ZZ9"
qtyout   dim       11
.Start Patch #1.1 - expanded var
.PRTQTY   INIT      "Z,ZZZ,ZZ9"
PRTQTY   INIT      "ZZZ,ZZZ,ZZ9"
.End Patch #1.1 - expanded var
PAGE     FORM      4
LINES    FORM      2
LOCAL    INIT      "LOCAL"
PRTFLAG  DIM       1
FUNCBR   FORM      "0"
.Start Patch #1.1 - expanded var
.RDATE    dim       8
RDATE    dim       10
.End Patch #1.1 - expanded var
countp   form      3
NUM      FORM      "1"
.START PATCH 1.4 REMOVED LOGIC
.fullCNT      DIM       34
.cnt          dim       20
.cntphone     dim       14
.END PATCH 1.4 REMOVED LOGIC
BEGIN    FORM      2
LAST     FORM      2
.
.
         MOVE      "DAILY ORDERS Not approved" TO STITLE
         MOVE      "Names In The News" TO COMPNME
         move      c0 to page
         CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO MM,DD,YY
         PACK      DATE FROM MM,SLASH,DD,SLASH,YY
         XIF
         IFZ       PC
         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         XIF
         MOVE      FUNC TO FUNCBR
         MATCH     "NORD0033" TO PROGRAM        *CHAINED FROM DSINIT?
         IF        NOT EQUAL                    *NO
         MOVE      "NORD0033 " TO PROGRAM
         MOVE      "NREGntok" TO PRTNAME
         ENDIF
         MOVE      "NPRINTng" TO INPNAME
.Start Patch #1.1 - replaced var
.         MOVE      C0 TO N7
         MOVE      C0 TO NUM9
.End Patch #1.1 - replaced var
         MOVE      C0 TO N8
         MOVE      C0 TO N9
         MOVE      DATE TO TODAY
         CALL      PAINT
          MOVE      "Exit" TO PF5
         TRAP      END IF F5
         CALL      FUNCDISP
         DISPLAY   *P01:06,"Input File  : ":
                   *P01:07,"Print File  : ":
                   *P01:08,"Input Count : "
.
INPGET   TRAP      FILENG IF IO
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         DISPLAY   *P15:06,INPNAME
         CLOSE     TESTFILE
         OPEN      INPFILE,INPNAME,READ
          GOTO      PRTGET
FILENG   NORETURN
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *P15:06,INPNAME
         GOTO      INPGET
.
PRTGET
.begin patch 2.0
.          MATCH     B8 TO PRTNAME
.         GOTO      PRTNG IF EQUAL
.         MOVE      C1 TO PRTFLAG
.         MATCH     LOCAL TO PRTNAME
.         GOTO      START IF EQUAL
.         MOVE      C2 TO PRTFLAG
.         PACK      PRTFILE WITH pdrive,PRTNAME
.         SPLOPEN   PRTFILE
..         PRINT     hp17ptch,hpdupl,hptop,*F                .compressed
.         PRINT     HPtmsr17,hpdupl,hptop,*F                .compressed
.         DISPLAY   *P15:07,PRTNAME
.end patch 2.0
         GOTO      START
.begin patch 2.0
.PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
.                   *P15:07,PRTNAME
.         GOTO      PRTGET
.end patch 2.0
.
START
.begin patch 1.8
.          CALL      HD1
.end patch 1.8
.
READ
         READ    INPFILE,seq;ORDVARS
         goto    eoj if over
.START PATCH 1.2 - NEW LOGIC
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
.END PATCH 1.2 - NEW LOGIC
.START PATCH 1.5 REPLACED LOGIC
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
          endif
.END PATCH 1.5 REPLACED LOGIC
         ADD       C1 TO N9
         DISPLAY   *P15:8,N9
         COMPARE   C1 TO NUM
         reset     cancodes
         scan      ostat in cancodes
         goto      read if equal
         CMATCH    "R" TO OSTAT
         GOTO      READ if equal
         add       c1 to countp
         match     ococode to holdcnt
         call      break if not equal
         ADD       C1 TO TOTAL
.Start Patch #1.1 - replaced var
.         MOVE      C0 TO N7
.         MOVE      OQTY TO N7
.         ADD       N7 TO TOTQTY
         MOVE      C0 TO NUM9
         MOVE      OQTY TO NUM9
         ADD       NUM9 TO TOTQTY
.End Patch #1.1 - replaced var
.
         PACK      MKEY FROM OMLRNUM,OCOBN
         CALL      NMLRKEY
.
.Start Patch #1.1 - replaced var
.         MOVE      "Z,ZZZ,ZZ9" TO PRTQTY
.         EDIT      N7 TO PRTQTY
         MOVE      "ZZZ,ZZZ,ZZ9" TO PRTQTY
         EDIT      NUM9 TO PRTQTY
.End Patch #1.1 - replaced var
.begin patch 2.0
.         COMPARE   "61" TO LINES
.         CALL      HD1 IF NOT LESS
..begin patch 1.8
.          Compare   c0,page
.          CALL      HD1 IF equal
.end patch 2.0
.end patch 1.8
         
.Start Patch #1.1 - expanded var
.         pack      rdate from ortndtem,slash,ortndted,slash,ortndtey
.         PRINT     *N,*1,OLRN,*9,OMLRPON,*20,MCOMP:
.                   *69,O1DES,*106,PRTQTY,*119,rdate:
.                   *N,*9,OODTEM,SLASH,OODTED,SLASH,OODTEY,*20,OODES,*69,O2DES
.START PATCH 1.2 - REPLACED LOGIC, OODES --> OFDESC
.         pack      rdate from ortndtem,slash,ortndted,slash,ortndtec,ortndtey
.         PRINT     *N,*1,OLRN,*9,OMLRPON,*20,MCOMP:
.                   *69,O1DES,*106,PRTQTY,*119,rdate:
.                   *N,*9,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY,*20,OODES,*69,O2DES                   
         pack      rdate from ortndtem,slash,ortndted,slash,ortndtec,ortndtey
.START PATCH 1.5 - REPLACED LOGIC
.         PRINT     *N,*1,OLRN,*9,OMLRPON,*20,MCOMP:
.                   *69,O1DES,*106,PRTQTY,*119,rdate:
.                   *N,*9,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY,*20,OFDESC,*69,O2DES                   
.START PATCH 1.7 REPLACED LOGIC
.         PRINT     *N,*1,OLRN,*9,OMLRPON,*20,MCOMP:
.                   *69,O1DES,*106,PRTQTY,*119,rdate:
.                   *N,*9,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY,*20,OFDESC,*69,NSEL2NAME                   
          if (OCompID = "P" | OCompID2 = "P")
                    move      " P",str2
          else
                    clear     str2
          endif
.begin patch 2.0
          append    "LR ## ",Mailbody
          append    OLRn,Mailbody
          append    "  PO ## ",Mailbody
          append    OMLRpon,Mailbody
          append    "  Mailer ",Mailbody
          append    MComp,Mailbody
          append    "<br>",mailbody
          append    "Qty ",Mailbody
          append    prtqty,Mailbody
          append    " List: ",Mailbody
          append    O1DES,Mailbody
          append    "<br>",mailbody
          append    "Return Date ",Mailbody
          append    Rdate,Mailbody
          append    " Mail Date: ",Mailbody
          append    OMdtem,Mailbody
          append    "/",Mailbody
          append    Omdted,Mailbody
          append    "/",Mailbody
          append    Omdtec,Mailbody
          append    Omdtey,Mailbody
          append    "<br>",mailbody
          append    "<br>",mailbody

.          PRINT     *N,*1,OLRN,str2,*9,OMLRPON,*20,MCOMP:
.                    *69,O1DES,*106,PRTQTY,*119,rdate:
.                    *N,*9,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY,*20,OFDESC,*69,NSEL2NAME                   
.end patch 2.0
.end patch 2.0
.END PATCH 1.7 REPLACED LOGIC
.END PATCH 1.5 - REPLACED LOGIC
.END PATCH 1.2 - REPLACED LOGIC, OODES --> OFDESC
.End Patch #1.1 - expanded var
.begin patch 2.0
.         ADD       C3 TO LINES
.end patch 2.0
         GOTO      READ
break    compare   c1 to countp
         if        equal
         move      ococode to holdcnt
         call      getcnt
         else      
         call      total
          MOve      "Daily Orders NOT Approved",Mailsubjct
          Move      "Creques@nincal.com",MailFrom
           move       c3,mailtype
          call      sendMail
         move      ococode to holdcnt
         call      getcnt
.begin patch 2.0
.         call      hd1
.end patch 2.0
         endif
         return
getcnt   move      c0 to n2
         move      holdcnt to savcnct
.START PATCH 1.4 REPLACED LOGIC
.         TYPE      holdcnt
.         GOTO      CON10 IF NOT EQUAL
.         MOVE      holdcnt TO N2
.         GOTO      DISCON2A
.CON10    REP       "A0B1C2D3E4F5G6H7I8J9" IN holdcnt
.         TYPE      holdcnt
.         GOTO      CON20 IF NOT EQUAL
.         MOVE      holdcnt TO N2
.         ADD       C10 TO N2
.         GOTO      DISCON2A
.CON20    REP       "K0L1M2N3O4P5Q6R7S8T9" IN holdcnt
.         TYPE      holdcnt
.         GOTO      CON30 IF NOT EQUAL
.         MOVE      holdcnt TO N2
.         ADD       "20" TO N2
.         GOTO      DISCON2A
.CON30    REP       "U0V1X2Y3Z4" IN holdcnt
.         MOVE      holdcnt TO N2
.         ADD       "30" TO N2
.DISCON2A
..         MOVE      OCNT0 TO CONTACT
..         LOAD      CONTACT FROM N2 OF OCNT1,OCNT2,OCNT3,OCNT4,OCNT5,OCNT6:
..                   OCNT7,OCNT8,OCNT9,OCNT10,OCNT11,OCNT12,OCNT13,OCNT14,OCNT15
.         MOVE      OCNT0 TO contact
.         clear     cntphone
..         clear     fullcnt
.         clear     cnt
..START PATCH 1.3 - REPLACED LOGIC
..         LOAD      FULLCNT FROM N2 OF OCNT1,OCNT2,OCNT3,OCNT4,OCNT5,OCNT6:
..                   OCNT7,OCNT8,OCNT9,OCNT10,OCNT11,OCNT12,OCNT13,OCNT14,OCNT15
.         LOAD      FULLCNT FROM N2 OF OCNT1,OCNT2,OCNT3,OCNT4,OCNT5,OCNT6:
.                   OCNT7,OCNT8,OCNT9,OCNT10,OCNT11,OCNT12,OCNT13,OCNT14,OCNT15,OCNT16,OCNT17
..END PATCH 1.3 - REPLACED LOGIC
.         MOVEFPTR  fullcnt TO BEGIN   
.         SCAN      "(" IN fullcnt
.         If        equal
.         MOVEFPTR  fullcnt TO LAST
.         APPEND    fullcnt TO cntphone
.         reset     cntphone
.         SUB       C1 FROM LAST
.         RESET     fullcnt
.         SETLPTR   fullcnt TO LAST
..         BUMP      fullcnt BY BEGIN
.         APPEND    fullcnt TO cnt
.         reset     cnt
.         move      cnt to contact
.         endif 
.
.         move      savcnct to holdcnt          
.         return
...............
        pack    NCNTFLD,holdcnt
        move    "NCNTKEY",Location
        pack    KeyLocation,"Key: ",NCNTFLD
        call    NCNTKEY
        move    savcnct to holdcnt          
        move    cntname to contact
.Begin patch 2.0
          clear     Mailto
          reset     Contact
          scan      "BILLING",Contact
.
          if not equal
                    reset     Contact
                    scan      "Billing",Contact
                    if equal
                              reset     Contact
                              Move      "CReques",Contact
                    endif
          else
                    reset     Contact
                    Move      "CReques",Contact
          endif
          reset     Contact
          call      RemoveChar using Contact,B1
          call      Trim using Contact
          if (Contact = "")
          Move      "CReques",Contact
          endif
          pack      MailTO,Contact,"@nincal.com"
          clear     MailCC
.          pack      MailCC,"ReubenHolland@nincal.com"
          pack      MailCC,"AmyFrey@nincal.com,JenniferCox@nincal.com,DeniseHubbard@nincal.com"
.end patch 2.0
          clear     MailBCC
          pack      MailBCC,"Creques@nincal.com"




        return
.END PATCH 1.4 REPLACED LOGIC
HD
         ADD       C1 TO PAGE
         PRINT     *F:
                   *n,*1,"CONFIDENTIAL":
                   *34,"***   N I N   O R D E R ' S  P E N D I N G   ":
                   "R E P O R T  ***":
                   *119,"DATE: ",DATE:
                   *N,*119,"PAGE:     ",PAGE;
         MOVE      C3 TO LINES
         RETURN
.
HD1      CALL      HD
.START PATCH 1.7 REPLACED LOGIC
.         PRINT     *N,*1,"  LR",*9,"MAILER":
.                   *20,"CLIENT COMPANY NAME",*69,"LIST":
.                   *N,*1,"NUMBER",*9,"  PO##",*20,"OFFER DESCRIPTION":
.                   *69,"DESCRIPTION",*108,"QUANTITY",*118,"RETURN DATE"
.         ADD       C2 TO LINES
.         PRINT     *N,*1,"'P' following LR indicates Pacific Lists record"
         PRINT     *N,*1,"  LR",*9,"MAILER":
                   *20,"CLIENT COMPANY NAME",*69,"LIST":
                   *N,*1,"NUMBER",*9,"  PO##",*20,"OFFER DESCRIPTION":
                   *69,"DESCRIPTION",*108,"QUANTITY",*118,"RETURN DATE"
         ADD       C3 TO LINES
.END PATCH 1.7 REPLACED LOGIC
         RETURN
.
.
TOTAL    
.begin patch 2.0
.          COMPARE   "61" TO LINES
.         CALL      HD IF NOT LESS
.         PRINT     *1,*RPTCHAR "-":132
         move      qtymask to qtyout
         EDIT      TOTQTY TO QTYout
          Append    "Totals",mailbody
          append    "<br>",mailbody
          append    "DAILY QUANTITY    : ",Mailbody
          append    QtyOut,Mailbody
          append    "<br>",mailbody
          append    "NUMBER OF ORDERS  : ",Mailbody
          append    Total,Mailbody
          Reset     Mailbody
.         print     *N,*7,"total for         :       ",CONTACT:
.                   *N,*7,"DAILY QUANTITY    : ",QTYout:
.                   *N,*7,"NUMBER OF ORDERS  :       ",TOTAL
         move      c0 to totqty
         move      c0 to total
         move      page to remain
         div       c2 into remain
         move      c0 to result1
         add       remain to result1
         compare   result1 to remain
.         if        not equal
.         print     *f
.         endif
.begin patch 2.0
         move      c0 to page
         return
eoj      
.begin patch 1.8
.          Compare   c0,page
          if        (total <> 0)
          call      total
          MOve      "Daily Orders NOT Approved",Mailsubjct
          Move      "Creques@nincal.com",MailFrom
          move      c1,MailType         .HTML
          call      sendMail
          endif
.          call      total if not equal
.end patch 1.8
.begin patch 2.0
.         splclose
.         release
.end patch 2.0
         stop
.patch1.6
                                        include   compio.inc
                                        include   cntio.inc
.         INCLUDE   NMLRIO.inc
.patch1.6
.START PATCH 1.2 - ADDED LOGIC
         INCLUDE   NOFRIO.INC
.END PATCH 1.2 - ADDED LOGIC
.START PATCH 1.4 ADDED LOGIC
         INCLUDE   NCNTIO.INC
.END PATCH 1.4 ADDED LOGIC
.START PATCH 1.5 - ADDED LOGIC
         INCLUDE   NSEL2IO.INC
.END PATCH 1.5 - ADDED LOGIC
         INCLUDE   COMLOGIC.inc
