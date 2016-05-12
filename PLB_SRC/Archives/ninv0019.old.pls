...............................................................................
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
.begin patch 1.2
;begin patch 1.6
;         INCLUDE   NINVDD.inc
               INCLUDE        ninvdd.inc
               include        NInvAcddd.inc
;end patch 1.6
         include   consacct.inc
         include   nacddd.inc
.end patch 1.2
         inc       hp.inc
.begin patch 1.3
         INCLUDE   NJstDD.inc
mrgsw    dim      1
shipsw   dim      1
.end patch 1.3
;patch1.5
                              include   compdd.inc
                              include   cntdd.inc
.         include   nmlrdd.inc
;patch1.5
          include   norddd.inc
          include   ndatdd.inc
          include   ndat3dd.inc
          include   nmrgdd.inc
          include   nowndd.inc
          include   nshpdd.inc
          include   oslspern.inc
          INCLUDE          PRTPAGEDD.INC
release   init    "1.6"                 08Mar2007  DLH      Oslspern.inc expansion
.release        init           "1.6"        DLH   8March2005          Invoice Conversion    
.release  init      "1.5"        DMB    26MAY2004 Mailer Conversion    
.release  init      "1.4"           ASH 02OCT2000 NEW SERVER ADDED
.release  init      "1.3"           ASH 03MAR99 NINORD Y2K, File expansion
.release  init      "1.2"           ASH 03MAR99 NINORD Y2K, File expansion
.release  init      "1.1"           ASH 03MAR99 NINORD Y2K, File expansion
.release  init      "1.0"           DLH DEC96 new by sls person
.
. .FILES DECLARATIONS
.
OUTPUT   IFILE     keylen=8,nocomp,fixed=37           OUTPUT FILE 'LRDETMST'
slskey   dim       8
.
.CC      DIM       2        1-2  \
.YY      DIM       2        3-4   \KEY
.MM      DIM       2        5-6   /KEY
.XX      DIM       2        7-8  /
.                                  
.TOTINV   FORM      5             9-13  
.TOTAR    FORM      9.2    14-25
.TOTAP    FORM      9.2    26-37
.
. ..........................................................................
.RN       FORM      "026"      RECORD NUMBER
.
.
.
.MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
.MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
NUM10    FORM      10
INDEXa    FORM      "01"
.CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
DATE     DIM       8
TIME     DIM       8
.START PATCH #1.1 - VAR NOW FOUND IN CONS.INC
.F1       DIM       1         POS 1-1    FILLER
.END PATCH #1.1 - VAR NOW FOUND IN CONS.INC
YR       FORM      2         POS 2-3    YEAR
MO       FORM      2         POS 5-6    MONTH
DA       FORM      2         POS 8-9    DAY
F4       DIM       4         POS 10-13  FILLER
HR       DIM       2         POS 14-15  HOUR
MINute   DIM       2         POS 17-18  MINUTE
SEC      DIM       2         POS 20-21  SECOND
.
. .WORK SPACE
.
slspern  dim       2
FINAL    DIM       1         CHECKS END OF PROGRAM
C        DIM       1         CHECKS FOR "0" IN TOTAL
OK       DIM       1
CHECK    DIM       1
.
COUNT    FORM      2
COUNT1   FORM      5         COUNTS INVOICE RECORDS
COUNT2   FORM      5         OUTPUT MONTH COUNT
COUNT3   FORM      5         OUTPUT YEAR COUNT
WORK     FORM      10.4
B14      DIM       14
B17      DIM       17
B20      DIM       20
B23      DIM       23
.
.
. .TOTAL INVOICE WORK FIELDS
.
TOTINV   FORM      5
FININV   FORM      5         TOTAL INVOICES PLACED IN YEAR
. *******CHANGE YEARS IN FOLLOWING LINES*****
FININVA  DIM       5         TOTAL INVOICES PLACED DURING YEAR1
FININVB  DIM       5         TOTAL INVOICES PLACED DURING YEAR2
FININVC  DIM       5         TOTAL INVOICES PLACED DURING YEAR3
grndINVA form      5         TOTAL INVOICES PLACED DURING YEAR1
grndiNVB form      5         TOTAL INVOICES PLACED DURING YEAR2
grndiNVC form      5         TOTAL INVOICES PLACED DURING YEAR3
FORM5    FORM      5
.
. .ACCOUNTS PAYABLE WORK FIELDS
.
TOTAP    FORM      9.2
FINAP    FORM      9.2         TOTAL NAMES INVOICED DURING YEAR
. *******CHANGE YEARS IN FOLLOWING LINES*****
FINAPA   DIM       10         TOTAL NAMES INVOICED DURING YEAR1
FINAPB   DIM       10         TOTAL NAMES INVOICED DURING YEAR2
FINAPC   DIM       10         TOTAL NAMES INVOICED DURING YEAR3
grndAPA  form      9.2         TOTAL NAMES INVOICED DURING YEAR1
grndAPB  form      9.2         TOTAL NAMES INVOICED DURING YEAR2
grndAPC  form      9.2         TOTAL NAMES INVOICED DURING YEAR3
FORM9P   FORM      10
ACCPAY   FORM      10
FORM9    FORM      10
.
. .ACCOUNTS RECIEVABLE WORK FIELDS
.
TOTAR    FORM      9.2
FINAR    FORM      9.2
FINARA   DIM       10
FINARB   DIM       10
FINARC   DIM       10
FINARB1  DIM       10
grndARA  form      9.2
grndARB  form      9.2
grndARC  form      9.2
FORM9R   FORM      10
ACCREC   FORM      10
ENDAR    FORM      10
FORM7    FORM      7
.START PATCH #1.1 - VAR NOW FOUND IN CONSACCT.INC
.form92   form      9.2
.END PATCH #1.1 - VAR NOW FOUND IN CONSACCT.INC
.
. .PERCENTAGE COMPUTATION FIELDS
.
PCT      DIM       3
PCTB     DIM       4         % CHANGE IN YEAR2
PCTC     DIM       4         % CHANGE IN YEAR3
NUMPCT   FORM      3         PERCENTAGE WORK SPACE
.
F2       DIM       2         FILLER
.
.V        FORM      "01"      SPLIT SCREEN POSITION
.V1       FORM      "15"      SPLIT SCREEN POSITION
V1       FORM      "14"      SPLIT SCREEN POSITION
.V2       FORM      "26"      SPLIT SCREEN POSITION
V2       FORM      "25"      SPLIT SCREEN POSITION
.
. **********CHANGE THE FOLLOWING INV# NUMBER IN JAN*********
. 000000 FIRST INV# OF 1981
. 001000 FIRST INV# OF 1982
. 015124 FIRST INV# OF 1983
. 032197 FIRST INV# OF 1984
. 052364 FIRST INV# OF 1985
. 075180 FIRST INV# OF 1986
. 098419 FIRST INV# OF 1987
. 121541 FIRST INV# OF 1988
. 145392 FIRST INV# OF 1989
. 172279 FIRST INV# OF 1990
. 197750 FIRST INV# OF 1991
. 218003 FIRST INV# OF 1992
. 241530 FIRST INV# OF 1993
. 262880 FIRST INV# OF 1994
. 282911 FIRST INV# OF 1995
. 302647 FIRST INV# OF 1996
. 321449 FIRST INV# OF 1997
.
.FIRSTN   INIT      "321449"  first NIN INV# OF 1997
.firstn   init      "262880" 
FirstN    INit      "566458"    FIRST NIN INV# OF 2009
.FIRSTN   INIT      "500807"   FIRST NIN INV# OF 2005
FIRSTC   INIT      "025905"  FIRST CMP INV# OF 1982
.
YR2      DIM       2         HOLDS YEAR
HOLDYR   FORM      2         HOLDS YEAR
DA2      DIM       2         HOLDS DAY
NUMDA    FORM      2         HOLDS DAY
MO2      DIM       2         HOLDS MONTH
HOLDMO   DIM       2         HOLDS MONTH
NUMMO    FORM      2         HOLDS MONTH
HRSTRT   DIM       2         HOLDS STARTING HOUR
NUMHR    FORM      2         HOLDS HOUR
MINSTRT  DIM       2         HOLDS STARTING MIN
SECSTRT  DIM       2         HOLDS STARTING SEC
MONTH    DIM       9
.
TEST     FORM      "12"
MNTH     FORM      "01"
TAB      FORM      "01"
ZERO     FORM      "0"
TITLE    INIT      "COMPUNAME        "
KEY      INIT      "96"
TEN      FORM      "10"
IND      FORM      2
pass     form      1
.
. HOLD TOTAL INVOICES
.
JANX     FORM      5
FEBX     FORM      5
MARX     FORM      5
APRX     FORM      5
MAYX     FORM      5
JUNX     FORM      5
JULX     FORM      5
AUGX     FORM      5
SEPX     FORM      5
OCTX     FORM      5
NOVX     FORM      5
DECX     FORM      5
.
. .HOLD ACC/PAY
.
JAN      FORM      10
FEB      FORM      10
MAR      FORM      10
APR      FORM      10
MAY      FORM      10
JUN      FORM      10
JUL      FORM      10
AUG      FORM      10
SEP      FORM      10
OCT      FORM      10
NOV      FORM      10
DEC      FORM      10
.
. .HOLD ACC/REC
.
JANR     FORM      10
FEBR     FORM      10
MARR     FORM      10
APRR     FORM      10
MAYR     FORM      10
JUNR     FORM      10
JULR     FORM      10
AUGR     FORM      10
SEPR     FORM      10
OCTR     FORM      10
NOVR     FORM      10
DECR     FORM      10
.
FINALSAV FORM      10.2
SAVE     FORM      10.2
FIN10     FORM      10
WORK14   DIM       14
FORM14   FORM      14
DIM10    DIM       10
.
MASKARA  INIT      "99999999.99"
MASKARB  INIT      "99999999.99"
MASKARC  INIT      "99999999.99"
.
. AVERAGE INVOICE FIELDS
.
FORM102  FORM      11.2
AVERAGA  FORM      5.2
AVERAGB  FORM      5.2
AVERAGC  FORM      5.2
.
.
SPOOL    DIM       1
FMESG    DIM       20
ANS      DIM       1
LASTMOSW DIM       1               HOLDS 'Y' IF LAST MONTH OF CURRENT YEAR
         MOVE      "NINV0019" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "CALCULATE Income by Salesperson" TO STITLE
         move      c1 to v
         CALL      PAINT
         move      c1 to nownpath
         move      c1 to nordpath
         move      c1 to nmlrpath
          move      c1 to pass
           MOVE      "ABORT" TO PF5
           CALL      FUNCDISP
           TRAP      STOP IF F5
. SPOOLING Y/N
         KEYIN    *P15:10,"DO YOU WANT THE OUTPUT SPOOLED ?":
                   *T20,SPOOL;
         CMATCH    "N",SPOOL
         GOTO      BEGIN IF EQUAL
         DISPLAY   *P15:12,"SPOOL FILE IS NAMED 'slsdolr.prn'";
           IFNZ        PC
         SPLOPEN   "slsdolr/PRT:PRINT","Q"
           XIF
           IFZ         PC
.START PATCH 1.4 REPLACED LOGIC
.         SPLOPEN   "g:\data\slsdolr","Q"
         PACK      STR35,NTWKPATH1,"slsdolr"
         SPLOPEN   STR35,"Q"
.END PATCH 1.4 REPLACED LOGIC
           XIF
.
BEGIN
.
.THIS ROUTINE CALLS DISPLAY, WHICH DISPLAY THE RECORD AND DATE COUNT ON THE;
.SCREEN AND TIME WHICH READS THE DATE AND TIME FORM 'ARCCLOCK'. THE TIME IS
.THEN STORED AS THE STARTING TIME OF THE REPORT.
         CALL      DISPLAY
         CALL      TIME
         MOVE      HR,HRSTRT
         REP       zfill,HRSTRT
         MOVE      minute,MINSTRT
         REP       zfill,MINSTRT
         MOVE      SEC,SECSTRT
         MOVE      DA,DA2
         REP       zfill,DA2
         MOVE      YR,YR2
.**********************************************************************
         move      "94" to yy
.**********************************************************************
         MOVE      MO,MO2
         REP       zfill,MO2
         TRAP      START IF IO
.
.THE FOLLOWING ROUTINE OPENS 'LRDETMST'. IF 'LRDETMST' DOES NOT EXIST THE TRAP
.CAUSES CONTROL TO BE TRANSFERED TO START, WHICH BEGINS READING THE INVOICE
.FILE FORM THE BEGINNING.  IF THE TRAP IS NOT ENCOUNTERED, THIS MEANS THAT
.'LRDETMST' ALREADY EXISTS AND THE FIRST READ OF THE INVOICE FILE IS THE FIRST
.LR OF 1980.
         MOVE      "NAMES IN THE NEWS",TITLE

         OPEN      OUTPUT,"lRdetMST.ISI|10.10.30.103:502",exclusive
         trapclr   IO
         move       C3,Nordlock
. cleanup this years numbers as we are going to recalc them
         move       "00" to str2         .start with sales person 1
         move      "01" to mm            .start with jan
         move       "20",CC
wiper    move       c0 to n2
          move      str2 to n2
          if        (n2 < "35")          
          display    *p1:24,*el,"doing a little cleanup";
          move     c0 to n2
          move     mm to n2                   
                   if        (n2 < "13")
                   clear     slskey
                   pack      slskey from cc,yy,mm,str2
                   rep       zfill in slskey
                   move      slskey to str8
                   display    *p1:24,*el,"record",slskey;
                   filepi    1;output 
                   read      output,slskey;str1
                             if        not over
                             move      c0 to totinv
                             move      c0 to totar
                             move      c0 to totap
                             filepi    1;output 
                             update    output;slskey,totinv,totar,totap
                             else
                             move      str8 to slskey
                             move      c0 to totinv
                             move      c0 to totar
                             move      c0 to totap
                             filepi    1;output 
                             write      output,slskey;slskey,totinv,totar,totap
                             endif
                   move       mm to n2
                   add        c1 to n2
                   endif
         move        c1 to mm
         move        str2 to n2
         add         c1 to n2
         move        n2 to str2
         goto        wiper
         endif

         display    *p1:24,*el;
         TRAPCLR   IO
         MOVE      C2 TO NINVPATH           .SET ACCESS TO ISAM BY INV#.
         MOVE      "                    ",FMESG
         MOVE      "Q",CHECK
         MOVE      "NAMES IN THE NEWS",TITLE
         MOVE      FIRSTN TO NINVFLD
         rep       zfill in ninvfld
         CALL      NINVKEY
         GOTO      NOINV IF OVER
         move      c0 to mo
         move      c0 to da
         move      c0 to yr
         MOVE      INVDTEM TO MO
         MOVE      INVDTED TO DA
         MOVE      INVDTEY TO YR
.begin patch 1.2
.         REP       zfill,AP1
.         REP       zfill,AP2
.         REP       zfill,AR
.end patch 1.2
         CALL      ZERO
.................................******
.         goto      end1a         .temp******
.................................******
.          goto     calcadj
         goto      read1
.
.THE FOLLOWING ROUTINE IS ONLY ENCOUNTERED IF 'LRDETMST' DOES NOT EXIST. IT
.CREATES 'LRDETMST' AND OPENS ALL OF THE NECESSARY INVOICE AND ADJUSTMENT FILES
.
START    TRAPCLR   IO
         NORETURN
         DISPLAY   *P10:12,*EF,*HON,"LRDETMST.dat NOT FOUND. IT WILL BE CREATED."
         BEEP
.START PATCH 1.4 REPLACED LOGIC
.         PREPARE   OUTPUT,"g:\data\text\LRDETMST","g:\data\index\lrdetmst","8","33"
         PACK      STR35,NTWKPATH1,"LRDETMST"
         PACK      STR45,NTWKPATH1,"LRDETMST"
         PREPARE   OUTPUT,STR35,STR45,"8","33"
.END PATCH 1.4 REPLACED LOGIC
         DISPLAY   *P10:12,*EF,*HON,"LRDETMST CREATED.",*b,*w5,*p10:12,*el;
.
......READS INVOICE FILE AND CREATES 'LRDETMST'
.
READ1    
         CALL      NINVKS
         GOTO      calcadj IF OVER
.begin patch 1.2
.         REP       zfill,AP1
.         REP       zfill,AR
.         REP       zfill,AP2
.end patch 1.2
         move      c0 to mo
         move      c0 to da
         move      c0 to yr
         MOVE      INVDTEM TO MO
         MOVE      INVDTEM TO Mnth
         MOVE      INVDTED TO DA
         MOVE      INVDTEY TO YR
         ADD       "1",COUNT1
         DISPLAY   *PV1:6,COUNT1,*PV2:9,MO,"/",DA,"/",YR," ",LRN,b1,invnum;
.         COMPARE   "1",COUNT1
         move      c1 to nordpath
         MOVE      LRN TO NORDFLD
         CALL      NORDKEY
.
         clear     slspern
         pack      slspern from osales10,osales
         MOVE      OLNUM TO NDATFLD
         MOVE      C1 TO NDATPATH
         CALL      NDATKEY
.dlh 25June97 ???? why is this in here
.         cmatch    "F" to onetfm
.         if        equal
.           MOVE      NORDFLD to nshpfld
.           REP       ZFILL IN NshpFLD
.           CALL        NshpKEY
.         if        not over
.         move      squant to nmrgiqty
.           MOVE      squant TO FORM7
.        else
.         move      oqty to nmrgiqty
.           MOVE      oqty TO FORM7
.         endif
.         endif
.replaced above with
.
             MOVE      no TO SUBPPSW
           MOVE      NordFLD to nmrgfld
           REP       ZFILL IN NMRGFLD
         move      c0 to nmrgrqty
         move      c0 to nmrgiqty
         move      c0 to nmrgnet
         CALL        NMRGKEY
         move      olon to nownfld
         call      nownkey
         pack      mkey from omlrnum,z3
         call      nmlrkey
.begin patch 1.2
         move      olnum to ndatfld
         MOVE      C1 TO NDATPATH
         call      ndatkey
         move      lrn to nshpfld
         move      no to shipsw
         move      no to mrgsw
         call      nshpkey
         if        not over
         move     yes to shipsw
         endif
         MOVE      LRN TO NmrgFLD
         call      nmrgkey
         if        not over
         move     yes to mrgsw
         endif
         call      wipecvars
.end patch 1.2
               call           NInvAcdRecClear
               CLEAR          NInvAcdfld
               packkey           NInvAcdFld from Invnum
;               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
         call      compute
         add       formap2 to ap
         goto      wrtdet
calcadj
         move    c0 to count1
         move     c2 to pass
         CALL      NJstseq
         GOTO      end1 IF OVER
.begin patch 1.3
.         unpack    jstdate into mo,da,yr
         unpack     jstdate into str2,yr,mo,da
.end patch 1.3

         move      mo to mnth
         ADD       "1",COUNT1
         DISPLAY   *PV1:8,cOUNT1,*PV2:9,MO,"/",DA,"/",str2,YR," ",jstLR,b1,jstinvno;
         move      c0 to n2
         move      yr to n2
         if        (n2 < "94")
         goto       calcadj      
         endif
         MOVE      jstlr TO NORDFLD
         rep       zfill in nordfld
         CALL      NORDKEY
.
         clear     slspern
         pack      slspern from osales10,osales
         move      c0 to ap
         move      c0 to formar
.begin patch 1.3
.         REP       zfill,jstar
.         REP       zfill,jstap1
.         REP       zfill,jstap2
.         clear     cvtfld
.end patch 1.3

.begin patch 1.3
.         MOVE      JSTAR TO CVTFLD
.         CALL      CVT
.         MOVE      ZERO TO form102
.         MOVE      CVTFLD TO form102
.         MULTIPLY  ".01"  BY form102
.         add       form102 to formar
          add       jstar to formar

.         clear     cvtfld
.         MOVE      JSTAP1 TO CVTFLD
.         CALL      CVT
.         MOVE      ZERO TO form102
.         MOVE      CVTFLD TO form102
.         mult      ".01" by form102
.         add       form102 to ap
          add       jstap1 to ap

          
.         clear     cvtfld
.         MOVE      JSTAP2 TO CVTFLD
.         CALL      CVT
.         MOVE      ZERO TO FORM102
.         MOVE      CVTFLD TO FORM102
.         mult      ".01" by form102
.         ADD       FORM102 TO AP
          add       jstap2 to ap
.end patch 1.3
.

.
. .....WRITES DATA TO 'LRDETMST'
.
wrtdet
NEWYEAR   if        (yr > 50 & Yr <= 99)
          MOve      "19",cc
          ELse
          MOve      "20",cc
          endif
          PACK      SLSKEY FROM CC,yr,mo,SLSPERN 
        rep       zfill in slskey
        display   *p1:22,*el,"prepping ",slskey,b1,totinv,b1,ap,b1,formar
        filepi    1;output
        READ      OUTPUT,SLSKEY;str1
        if        not over
        filepi   1;output
        READ      OUTPUT,SLSKEY;str8,TOTINV,TOTAP,TOTAR

        compare   c1 to pass
        if        equal  
        add      c1 to totinv
        endif  

        move     c0 to form92
        move     ap to form92
.........        mult     "100" by form92
        add      form92 to totap

        move     c0 to form92
        move     formar to form92
............        mult     "100" by form92
        add      form92 to totar
        display   *p1:23,*el,"updating ",slskey,b1,totinv,b1,totap,b1,totar
        filepi   1;output
        UPDATE    OUTPUT;slskey,TOTINV,TOTAP,TOTAR
        flush    output
.end of update section
        else
.No previous record lets create one        
        compare   c1 to pass
          if        equal  
          move     c1 to totinv
          endif
        move     c0 to totap
        move     c0 to totar 
        move     c0 to form92
        move     ap to form92
.      mult     "100" by form92
        add      form92 to totap

        move     c0 to form92
        move     formar to form92
*       mult     "100" by form92
        add      form92 to totar
        filepi   1;output
        write    OUTPUT,slskey;slskey,TOTINV,TOTAP,TOTAR
        display   *p1:24,*el,"writing ",slskey,b1,totinv,b1,totap,b1,totar
        flush    output
        endif
        branch   pass of read1,calcadj
        goto     read1
.
.THE FOLLOWING ROUTINE MOVES ZEROES INTO THE WORK FIELDS IN BETWEEN DOING
.NAMES IN THE NEWS AND COMPUNAME.
.
ZEROFILL
         MOVE      "0",FININV
         MOVE      "0",FORM9R
         MOVE      "0",FORM9P
         MOVE      "0",FORM7
         MOVE      "0",FORM9
         MOVE      "0",ACCREC
         MOVE      "0",ACCPAY
         MOVE      "0",FINAP
         MOVE      "0",FINAR
         MOVE      "01",MNTH
         branch   pass of read1,calcadj
         GOTO      READ1
.
.'LRDETMST' IS CLOSED
.
END1
         flush     output
         CLOSE     OUTPUT
end1a
. /////////////////////////////////////////////////////////////////////////////
..........BUILD 1ST KEY
.            YEAR\    /MONTH
.       IE      "19940100"  
.                       \SLSPERSON       
         pack     slskey from "20060100"
         move     osls0 to str25
         move     "00" to slspern
         move     c1 to mnth
. /////////////////////////////////////////////////////////////////////////////
         OPEN      OUTPUT,"LRDETMST"
         move      totinv to fininva
         move      totap to finapa
         move      totar to finara
.        
......DISPLAYS TIME, DATE, AND HEADINGS;
.
DISP
         call       zero
         filepi     1;output
         read      output,slskey;slskey,totinv,totap,totar
         move      totinv to fininva
         move      totap to finapa
         move      totar to finara
         add       totinv to grndinva
         add       totap to grndapa
         add       totar to grndara
         display   *p1:24,*el,slskey,b1,fininva,b1,finapa,finara;
.
          call      pdf995auto
            move              "LRdetprt" to  str25
            PRTOPEN           Laser,"PDF995",str25
            pack              str55,str25,".pdf"
............
          PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*Portrait

          prTPAGE    LASER;*FONT=prtpg10,*p=1:150,"Confidential":
                    *Pictrect=*off,*PICT=0:1000:2250:8550:NINLogo:
                              *FONT=prtpg10,*P=7000:150,"DATE: ",tODAY:
                              *FONT=prtpg10,*P=7000:375,"Time: ",Time:
                    *p=1475:1100,"2006":
                    *p=3725:1100,"2007":
                    *p=5725:1100,"2008":
.begin patch 3.35
                    *p=column1a:1250,*PENSIZE=10,*line=column1b:1250
.END PATCH 3.22 REPLACED LOGIC
                    move      Column2B to ColumnB
                    add     "500" to ColumnB
         prTPAGE    LASER;*FONT=prtpg10,*p=column2a:1250,*PENSIZE=10,*line=ColumnB:1250
                    move      Column3B to ColumnB
                    add     "500" to ColumnB
         prTPAGE    LASER;*FONT=prtpg10,*p=column3a:1250,*PENSIZE=10,*line=columnb:1250:
                    *font=prtpg10B,*p=1:1800,"Month":
                    *font=prtpg10,*p=COLUMN1A:1800,"Invoices":
                    *font=prtpg10,*p=COLUMN1B:1800,*alignment=*Right,"AR/AP":
                    *p=column1a:1950,*PENSIZE=10,*line=column1b:1950:
                    *font=prtpg10,*p=column2a:1800,*alignment=*Left,"Invoices":
                    *font=prtpg10,*p=column2b:1800,*alignment=*Right,"AR/AP"
                    move      Column2B to ColumnB
                    add     "500" to ColumnB
         PrtPage    Laser;*font=prtpg10,*p=columnb:1800,*alignment=*Right,"Chng ":
                    *FONT=prtpg10,*p=column2a:1950,*PENSIZE=10,*line=ColumnB:1950:
                    *font=prtpg10,*p=column3a:1800,*alignment=*Left,"Invoices":
                    *font=prtpg10,*p=column3b:1800,*alignment=*Right,"AR/AP"
                    move      Column3B to ColumnB
                    add     "500" to ColumnB
         PrtPage    Laser;*font=prtpg10,*p=columnb:1800,*alignment=*Right,"Chng ":
                    *FONT=prtpg10,*p=column3a:1950,*PENSIZE=10,*line=columnb:1950:
                    *font=prtpg10,*p=1:2175,*alignment=*Left,"January":
                    *font=prtpg10,*p=1:2550,"February":
                    *font=prtpg10,*p=1:2925,"March":
                    *font=prtpg10,*p=1:3300,"April":
                    *font=prtpg10,*p=1:3675,"May":
                    *font=prtpg10,*p=1:4050,"June":
                    *font=prtpg10,*p=1:4425,"July":
                    *font=prtpg10,*p=1:4800,"August":
                    *font=prtpg10,*p=1:5175,"September":
                    *font=prtpg10,*p=1:5550,"October":
                    *font=prtpg10,*p=1:5925,"November":
                    *font=prtpg10,*p=1:6300,"December":
                    *p=column1a:7025,*PENSIZE=10,*line=column1b:7025:
                    *p=column1a:7575,*PENSIZE=10,*line=column1b:7575
                    move      Column2B to ColumnB
                    add     "500" to ColumnB
         prTPAGE    LASER;*FONT=prtpg10,*p=column2a:7025,*PENSIZE=10,*line=columnb:7025:
                    *p=column2a:7575,*PENSIZE=10,*line=columnb:7575
                    move      Column3B to ColumnB
                    add     "500" to ColumnB
         prTPAGE    LASER;*FONT=prtpg10,*p=column3a:7025,*PENSIZE=10,*line=columnb:7025:
                    *p=column3a:7575,*PENSIZE=10,*line=columnb:7575:
                    *font=prtpg10,*p=1:7050,"Totals":
                    *p1:8000,"*** This report includes adjustments done in the same year as the associated invoice ***"
.  
. PRINT LOCAL
.         print     hpptch,HPTOP,*f
.         print     *n,*N,"Confidential",B7,B7,B7,TITLE,B5,B5,B5,B5,"Date",B2;
.         PRINT     MO2,"/",DA2,"/",YR2
.         PRINT   *L,B7,B7,B7,str25,B3,B7,B7,B7,"Time",B2,HRSTRT,":";
.         PRINT     MINSTRT," ",SECSTRT
.. ******CHANGE YEARS IN FOLLOWING LINES******
..         PRINT     *L,B4,B4,B7,"------1995------",B3," --------1996---------";
..         PRINT     B4,"---------1997---------"
.         PRINT     *L,B4,B4,B7,"------2006------",B3," --------2007---------";
.         PRINT     B4,"---------2008---------"
.         PRINT     B1,"Month",B4,B3,B3,"Inv",B4,"AR/AP",B5,B3,"Inv",B4;
.         PRINT     "AR/AP",B4,"Chng",B3,B2,"Inv",B5,"AR/AP",B4,"Chng"
..
. .....PRINTS DATA BY MONTH AND YEAR
.
END2     
********changes every year
.         pack     slskey from "1996",mnth,slspern
         pack     slskey from "2007",mnth,slspern
         rep      zfill in slskey
         filepi     1;output
         read      output,slskey;slskey,totinv,totap,totar
         move      totinv to fininvb
         move      totap to finapb
         move      totar to finarb
         add       totinv to grndinvb
         add       totap to grndapb
         add       totar to grndarb
********changes every year
.         pack     slskey from "1997",mnth,slspern
         pack     slskey from "2008",mnth,slspern
         rep      zfill in slskey
         filepi     1;output
         read      output,slskey;slskey,totinv,totap,totar
         move      totinv to fininvc
         move      totap to finapc
         move      totar to finarc
         add       totinv to grndinvc
         add       totap to grndapc
         add       totar to grndarc
         MOVE      Mnth,NUMMO
         BRANCH    NUMMO OF JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC
.
JAN      MOVE      "January",MONTH
         GOTO      END3
FEB      MOVE      "February",MONTH
         GOTO      END3
MAR      MOVE      "March",MONTH
         GOTO      END3
APR      MOVE      "April",MONTH
         GOTO      END3
MAY      MOVE      "May",MONTH
         GOTO      END3
JUN      MOVE      "June",MONTH
         GOTO      END3
JUL      MOVE      "July",MONTH
         GOTO      END3
AUG      MOVE      "August",MONTH
         GOTO      END3
SEP      MOVE      "September",MONTH
         GOTO      END3
OCT      MOVE      "October",MONTH
         GOTO      END3
NOV      MOVE      "November",MONTH
         GOTO      END3
DEC      MOVE      "December",MONTH
.
END3
         MOVE      MO2,NUMMO
         COMPARE   NUMMO,MO
         GOTO      END3B IF EQUAL
         GOTO      END4 IF NOT LESS
         MOVE      FINARB,FIN10
         ADD       FIN10,FINALSAV
         GOTO      END4
END3B    move      c0 to save
         MOVE      FINARB,SAVE
         DIV       "31",SAVE
         MOVE      DA2,NUMDA
         MULT      NUMDA,SAVE
         ADD       SAVE,FINALSAV
         MOVE      "Y" TO LASTMOSW
         MOVE      SAVE TO FINARB1
END4 
         CALL      PERCENT
         CMATCH    "X",C
         GOTO      END5 IF EQUAL
.
.THE FOLLOWING SEQUENCE EDITS THE DATA TO A MORE SUITABLE PRINT FORMAT
.
         EDIT      FINARA,MASKARA
         EDIT      FINARB,MASKARB
         EDIT      FINARC,MASKARC
         GOTO      END6
END5     MOVE      "           ",MASKARC
         EDIT      FINARA,MASKARA
         EDIT      FINARB,MASKARB
END6     DISPLAY   MONTH,B5,FININVA,B1,MASKARA,B3,FININVB,B1,MASKARB,B1,PCTB:
                   "%",B3,FININVC,B1,MASKARC,B2,PCTC,"%";

.         PRINT     MONTH,B5,FININVA,B1,MASKARA,B3,FININVB,B1,MASKARB,B1,PCTB;
.         PRINT     "%",B2,FININVC,B1,MASKARC,B2,PCTC,"%"
         MOVE      "99999999.99",MASKARA
         MOVE      "99999999.99",MASKARB
         MOVE      "99999999.99",MASKARC
         CMATCH    "X",C
         GOTO      END7 IF EQUAL
         EDIT      FINAPA,MASKARA
         EDIT      FINAPB,MASKARB
         EDIT      FINAPC,MASKARC
         GOTO      END8
END7     MOVE      "           ",MASKARC
         MOVE      " ",C
         EDIT      FINAPA,MASKARA
         EDIT      FINAPB,MASKARB
END8
         PRINT     B20,MASKARA,B9,MASKARB,B14,MASKARC
         MOVE      "99999999.99",MASKARA
         MOVE      "99999999.99",MASKARB
         MOVE      "99999999.99",MASKARC
        compare   "12" to mnth
        if        not equal
        add       c1 to mnth
.        pack      slskey from "1994",mnth,slspern 
        pack      slskey from "2006",mnth,slspern 
        rep       zfill in slskey
         call       zero
         filepi     1;output
         read      output,slskey;slskey,totinv,totap,totar
         move      totinv to fininva
         move      totap to finapa
         move      totar to finara
         add       totinv to grndinva
         add       totap to grndapa
         add       totar to grndara
        GOTO      end2 
        endif
.
. .....PRINTS YEARLY TOTALS
.
         move      c0 to fininva
         move      c0 to finapa
         move      c0 to finara
         move      c0 to fininvb
         move      c0 to finapb
         move      c0 to finarb
         move      c0 to fininvc
         move      c0 to finapc
         move      c0 to finarc
         move      grndinva to fininva
         move      grndinvb to fininvb
         move      grndinvc to fininvc
         move       grndara to finara
         move       grndarb to finarb
         move       grndarc to finarc
         move       grndapa to finapa
         move       grndapb to finapb
         move       grndapc to finapc

         PRINT     B1,"             _____ ___________   _____ _______";
         PRINT     "____ ____   _____ ___________ ____"
         MOVE      "A",OK
         CALL      PERCENT
         EDIT      FINARA,MASKARA
         EDIT      FINARB,MASKARB
         EDIT      FINARC,MASKARC
         PRINT     B2,B6,B6,FININVA,B1,MASKARA,B3,FININVB,B1,MASKARB,B1,PCTB;
         PRINT     "%",B2,FININVC,B1,MASKARC,B1,PCTC,"%"
         MOVE      "99999999.99",MASKARA
         MOVE      "99999999.99",MASKARB
         MOVE      "99999999.99",MASKARC
         EDIT      FINAPA,MASKARA
         EDIT      FINAPB,MASKARB
         EDIT      FINAPC,MASKARC
         DISPLAY   B20,MASKARA,B9,MASKARB,B14,MASKARC;
         PRINT     B20,MASKARA,B9,MASKARB,B14,MASKARC
         MOVE      "99999999.99",MASKARA
         MOVE      "99999999.99",MASKARB
         MOVE      "99999999.99",MASKARC
.
.THE FOLLOWING SEQUENCE COMPUTES THE AVERAGE INVOICE OF EACH YEAR
.
         MOVE      FINARA,FORM102
         MOVE      FININVA,FORM5
         DIV       FORM5,FORM102
         DIV       "100",FORM102
         MOVE      FORM102,AVERAGA
         MOVE      FINARB,FORM102
         MOVE      FININVB,FORM5
         DIV       FORM5,FORM102
         DIV       "100",FORM102
         MOVE      FORM102,AVERAGB
         MOVE      FINARC,FORM102
         MOVE      FININVC,FORM5
         DIV       FORM5,FORM102
         DIV       "100",FORM102
         MOVE      FORM102,AVERAGC
         PRINT     B14,"Average: ",AVERAGA,B12,AVERAGB,B17,AVERAGC
         MOVE      FINAPA,FORM102
         MOVE      FININVA,FORM5
         DIV       FORM5,FORM102
         DIV       "100",FORM102
         MOVE      FORM102,AVERAGA
         MOVE      FINAPB,FORM102
         MOVE      FININVB,FORM5
         DIV       FORM5,FORM102
         DIV       "100",FORM102
         MOVE      FORM102,AVERAGB
         MOVE      FINAPC,FORM102
         MOVE      FININVC,FORM5
         DIV       FORM5,FORM102
         DIV       "100",FORM102
         MOVE      FORM102,AVERAGC
         DISPLAY   B23,AVERAGA,B12,AVERAGB,B17,AVERAGC;
         PRINT     B23,AVERAGA,B12,AVERAGB,B17,AVERAGC
         CALL      TIME
         REP       zfill,HR
         REP       zfill,minute
         PRINT     "*** THIS REPORT INCLUDES ADJUSTMENTS ***",B20;
         PRINT     B4,B3,"End Time",B1,HR,":",minute," ",SEC
         MOVE      "NAMES IN THE NEWS",TITLE
         MOVE      " ",FINAL
         MOVE      "0",FINALSAV
         MOVE      "0",SAVE
         MOVE      "0",FIN10
         move      c0 to n2
         move      slspern to n2
         add       c1 to n2
         compare   "23" to n2
         goto      stop if equal
         move      n2 to slspern
         move      c1 to mnth
.         pack      slskey from "1994",mnth,slspern
         pack      slskey from "2006",mnth,slspern
         rep       zfill in slskey
         MOVE      OSLS0 TO str25
         LOAD      str25 FROM N2 OF OSLS1,OSLS2,OSLS3,OSLS4,OSLS5:
                   OSLS6,OSLS7,OSLS8,OSLS9,OSLS10,OSLS11,OSLS12,OSLS13:
                   OSLS14,OSLS15,OSLS16,OSLS17,OSLS18,OSLS19,OSLS20,OSLS21:
                   OSLS22,osls23,osls24,osls25:
                    osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
         move      c0 to grndinva
         move      c0 to grndinvb
         move      c0 to grndinvc
         move      c0 to grndara
         move      c0 to grndarb
         move      c0 to grndarc
         move      c0 to grndapa
         move      c0 to grndapb
         move      c0 to grndapc
         GOTO      DISP
.
. .....ZERO FILLS TOTAL FIELDS
.
ZERO     MOVE      "0",TOTINV
         MOVE      "0",COUNT1
         MOVE      "0",COUNT2
         MOVE      "0",COUNT3
         MOVE      "0",TOTAP
         MOVE      "0",FININV
         MOVE      "0",FINAP
         MOVE      "0",FININVA
         MOVE      "0",FININVB
         MOVE      "0",FININVC
         MOVE      "0",FINAPA
         MOVE      "0",FINAPB
         MOVE      "0",FINAPC
         MOVE      "0",FINARA
         MOVE      "0",FINARB
         MOVE      "0",FINARC
         RETURN
.
. .....READS TIME AND DATE FROM 'ARCCLOCK'
.
TIME
         CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO MM,DD,YY
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         XIF
         IFZ       PC
         MOVE      DATE TO TODAY
         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         XIF
         MOVE      MM TO MO
         MOVE      DD TO DA
         MOVE      YY TO YR
         CLOCK     TIME TO TIME   (DIM 8)
         UNPACK    TIME INTO HR,ANS,minute,ANS,SEC
         MOVE      HR,NUMHR
         COMPARE   "12",NUMHR
         GOTO      TIME4 IF EQUAL
         GOTO      TIME3 IF LESS
         MOVE      "PM",SEC
         SUB       "12",NUMHR
         MOVE      NUMHR,HR
         RETURN
.TIME3    CLOSE     TIME
TIME3
         MOVE      "AM",SEC
         RETURN
.TIME4    CLOSE     TIME
TIME4
         MOVE      "PM",SEC
         RETURN
.
. .....FIGURES PERCENTAGE CHANGE
.
PERCENT  
         MOVE      FINARA,FINAR
         MOVE      FINARB,ENDAR
         COMPARE   FINAR,ENDAR
         GOTO      PERCENT2 IF NOT LESS
         MOVE      ENDAR,WORK
         DIV       FINAR,WORK
         MULT      "100",WORK
         SUB       "100" FROM WORK
         MOVE      WORK,NUMPCT
         MULT      "-1",NUMPCT
         MOVE      NUMPCT,PCT
         RESET     PCT
         CLEAR     PCTB
         APPEND    "-",PCTB
         APPEND    PCT,PCTB
         RESET     PCTB
         GOTO      PERCENT3
PERCENT2
         MOVE      ENDAR TO WORK
         DIV       FINAR INTO WORK
         MULT      "100",WORK
         SUB       "100" FROM WORK
         MOVE      WORK,NUMPCT
         MOVE      NUMPCT,PCT
         RESET     PCT
         CLEAR     PCTB
         APPEND    "+",PCTB
         APPEND    PCT,PCTB
         RESET     PCTB
.
.THE FOLLOWING ROUTINE CHECKS A FIELD CALLED 'OK' FOR AN 'A'. THIS FIELD IS A
.SIGNAL WHICH SIGNIFIES THE END OF THE FILE AND TELLS THE PROGRAM TO MOVE THE
.PREVIOUS YEAR'S TOTAL AT THE SAME POINT TO DATE TO A TOTAL FIELD SO THE ACTUAL
.CHANGE TO DATE CAN BE RECORDED
.
PERCENT3 CMATCH    "A",OK
         GOTO      PERCENT4 IF NOT EQUAL
         MOVE      FINALSAV,FINAR
         MOVE      " ",OK
         GOTO      PERCENT5
PERCENT4 CMATCH    "Y" TO LASTMOSW
         GOTO      PERCENTX IF NOT EQUAL
         MOVE      FINARB1 TO FINAR
         MOVE      " " TO LASTMOSW
         GOTO      PERCENTY
PERCENTX MOVE      FINARB,FINAR
PERCENTY
         MATCH     "        0",FINARC
         GOTO      PERCENT7 IF EQUAL
         MATCH     "000000000",FINARC
         GOTO      PERCENT7 IF EQUAL
PERCENT5 MOVE      FINARC,ENDAR
         MATCH     "         0",FINARC
         GOTO      PERCENT7 IF EQUAL
         MATCH     "000000000",FINARC
         GOTO      PERCENT7 IF EQUAL
         COMPARE   FINAR,ENDAR
         GOTO      PERCENT6 IF NOT LESS
         MOVE      ENDAR,WORK
         DIV       FINAR,WORK
         MULT      "100",WORK
         SUB       "100" FROM WORK
         MOVE      WORK,NUMPCT
         MULT      "-1",NUMPCT
         MOVE      NUMPCT,PCT
         RESET     PCT
         CLEAR     PCTC
         APPEND    "-",PCTC
         APPEND    PCT,PCTC
         RESET     PCTC
         RETURN
PERCENT6
         MOVE      ENDAR TO WORK
         DIV       FINAR INTO WORK
         MULT      "100",WORK
         SUB       "100" FROM WORK
         MOVE      WORK,NUMPCT
         MOVE      NUMPCT,PCT
         RESET     PCT
         CLEAR     PCTC
         APPEND    "+",PCTC
         APPEND    PCT,PCTC
         RESET     PCTC
         RETURN
PERCENT7 CLEAR     PCTC
         REP       "0 ",FINARC
         REP       "0 ",FINAPC
         REP       "0 ",FININVC
         MOVE      "X",C
         RETURN
.
. .....SCREEN DISPLAY;
.
DISPLAY  DISPLAY   *ES,*P31:24,"***** INVOCPRT *****",*R,*R:
                   *P12:24,"NAMES IN THE NEWS",*P40:24,"|",*P56:24,"COMPUNAME";
DISPLAY2 DISPLAY   *R,*P40:24,"|";
         ADD       "1",COUNT
         COMPARE   "21",COUNT
         GOTO      DISPLAY2 IF NOT EQUAL
DISPLAY3 DISPLAY   *PV:6,"RECORDS READ: ",*PV:9,"CURRENT DATE BEING READ: ";
         COMPARE   "42",V
         RETURN    IF EQUAL
         ADD       "41",V
         GOTO      DISPLAY3
.
. ...ERROR DISPLAY;
.
IOERROR
         DISPLAY   *P1:24,*EL,*HON," ",FMESG," COULD NOT BE FOUND";
         BEEP
         KEYIN     *P78:24,ANS;
         CMATCH    "Q",ANS
         GOTO      IOERROR IF NOT EQUAL
         GOTO      STOP
.
NOINV
         MOVE      "FIRST INVOICE ## ",FMESG
         DISPLAY   *P1:24,*EL,*HON," ",FMESG,FIRSTN," COULD NOT BE FOUND";
         BEEP
         KEYIN     *P78:24,ANS;
         CMATCH    "Q",ANS
         GOTO      NOINV IF NOT EQUAL
.STOP     PRINT     *FLUSH
STOP     PRINT     *F,033,033,"H,P,FR1;",033,046,"l8C",*FLUSH
         flush     output
         SPLCLOSE
.         CHAIN     "NORD0011"
         shutdown  "CLS"
.begin patch 1.2
;begin patch 1.6
;         include   compute.inc
               include        compute.inc
;end patch 1.6
         include   nacdio.inc
.end patch 1.2
;begin patch 1.6
;         INCLUDE   NINVIO.inc
               INCLUDE        ninvio.inc
               include        NInvAcdio.inc
;end patch 1.6
.begin patch 1.3
         INCLUDE   NJstIO.inc
.end patch 1.3
         include   nordio.inc
         include   ndatio.inc
         include   ndat3io.inc
         include   nownio.inc
         include   nmrgio.inc
         include   nshpio.inc
;patch1.5
                              include   compio.inc
                              include   cntio.inc
.         include   nmlrio.inc
;patch1.5
.         include   cvt.inc
         INCLUDE   COMLOGIC.inc

