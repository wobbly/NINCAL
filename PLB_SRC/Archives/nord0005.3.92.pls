............................................................................
.
. PROGRAM    : NORD005
. DATE       : 04/15/88
. AUTHOR     : E.W. LAKE
. DESCRIPTION: PRODUCES NIN LIST BOOK.
.
............................................................................
.
PC       EQU        0
         INCLUDE   COMMON.inc
+...........................................................................
.
         INCLUDE   CONS.inc
+
         INCLUDE    NORD2DD.inc
.
         INCLUDE    HP.inc
+
.patch3.8
          include   compdd.inc
          include   cntdd.inc
.         INCLUDE    NMLRDD.inc
.patch3.8
+
.Begin patch 3.92
          Include   Ninvdd.inc
.end patch 3.92
         INCLUDE   NSHPDD.inc
         INCLUDE   NMTXDD.inc
..............................................................................
.START PATCH 3.2 - ADDED LOGIC
         INCLUDE   NOFRDD.INC
.END PATCH 3.2 - ADDED LOGIC
.START PATCH 3.6 ADDED LOGIC
          INCLUDE   NSEL2DD.INC
          include   nmoddd.inc
.END PATCH 3.6 ADDED LOGIC
release   init      "3.92"         DLH     added billed date to excel 
RelDate   Init      "27 Jan 2011"
.release  init      "3.91"         JD     Moved taxprt info to 2nd line
.release  init      "3.90"         JD     Copyrite footer update
.RelDate        Init           "06 Jan 2009"
.release  init      "3.89"         DLH     Sendmail wait for attachment
.RelDate        Init           "07 August 2008"
.release  init      "3.88"         DLH     Sendmail
.RelDate        Init           "24 April 2008"
.release  init      "3.87"         JD     Copyrite footer update
.RelDate        Init           "01/04/2008"
.release  init      "3.86"         DLH 22may2007 Pacific LIsts  uses company from common.inc
.RelDate        Init           "03/27/2007"
.release  init      "3.85"         DLH  8Sep2006  Added net print info to Excel.
.release  init      "3.84"         JD   18JAN2006 Added net print info.
.release  init      "3.83"        ASH   03MAR2005 Added Excel option
.Note:    At this time only the version labeled 'ALPHMLRFLAT' in BUTIL is using the Excel logic.
.         If we decide to go live with more programs, we can add an 'Excel' checkbox
.         in NORD0006.PLS and then use a variable to let BUTIL know that the report
.         should be dumped into Excel.  Modifying PRTNAME might be a good way to do this.
.         I am currently depending on COMMENT="TOTAL FLAT" to trigger the Excel logic.  This
.         is only called by 'ALPHMLRFLAT'.
.
.release  init      "3.82"        ASH   06AUG2004 Logo Conversion
.release  init      "3.81"        DLH   29July2004          CopyRite footer
.release  init      "3.8"        DMB    26MAY2004 Mailer Conversion
.RELEASE  INIT      "3.7"        DMB 27APR2004  Added code for diskin flat file
.RELEASE  INIT      "3.6"        ASH 29JAN2004  DATACARD CONVERSION
.RELEASE  INIT      "3.5"        DMB 06may2002 Newcode added for new options on order pick using inits
.RELEASE  INIT      "3.4"       ASH 02OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "3.3"       ASH 10JUN99 NINSHP Y2K, File expansion
.RELEASE  INIT      "3.2"       ASH 06MAY99 Replaced OODES{NINORD.DAT} --> OFDESC{NINOFR.DAT}
.RELEASE  INIT      "3.1A"      JD 29apr99  decreased total sub 1 line to fit on page.
.RELEASE  INIT      "3.1"      ASH 11JAN99 NINORD Y2K, File expansion
.                            NOTE:  There may be some formatting glitches,
.                            especially if full value of Quantity fields is used
.release  init      "3.0"      DLH 06Jan99 Banner page
.Release  init      "2.9"     28Sep98 DLH added code to handle pending orders
.                            See norddd.inc patch 5
.                            Currently it just suppresses them will need option to
.                            select in the future
.release   init      "2.8"        24Sep98 ASH NINMLR Y2K File expansion
.Release   init      "2.7"        18Aug98 DLH add flat file option
.RELEASE  INIT      "2.6"        01apr98 Jd keyin title for summary's
.RELEASE  INIT      "2.5"        04dec97 Jd turned off percent
.RELEASE  INIT      "2.4"       20APR94 DLH DUPLEX OPTION.
.RELEASE  INIT      "2.3"       03FEB94 JD SKIP DUMMY ORDERS WITH FAX OPT.
.RELEASE   INIT      "2.2"     JD   09SEP93     SKIP CANCEL ORDERS/FAX OPT.
.
.RELEASE   INIT      "2.1"     DLH 28JUL93     FAX/LASER PRINT OPTION
.RELEASE          INIT      "2.0"     DLH 23APR92   CONVERTED TO USE DSINIT
.
.RELEASE         INIT       "1.9"    DLH 08APR92    LIFESTYLE & IC SYSTEMS.
.
.RELEASE  INIT      "1.8"    D.L. HERRICK 17OCT91
.                           ADDED 501Cx TAX STATUS OPTION
.
.RELEASE  INIT      "1.7"    D.L.HERRICK 07AUG91
.                           CLEANED UP SHIPPING OPTION TO WORK WITH CA FORMAT.
.
.RELEASE  INIT      "1.6"    D.L.HERRICK 07/24/91
.                           CONVERT TO NINCA. VARIABLES
.                           CONVERT TO RUN UNDER RMS OR PC-DATABUS.
.                           REMOVED '1513' FROM RETURN-TO LIST.
.
.RELEASE  INIT      "1.3"    E.W. LAKE   04/14/90
.                           NEW TOF AT 57 LINES.
.
.RELEASE INIT      "1.2"    E.W. LAKE   04/18/89
.                           ADD RETURN-TO#'1513' TO THE LIST.
.
.RELEASE INIT      "1.1"    E.W. LAKE   07/12/88
.                           IF RETURN-TO IS '0000' OR '0001' USE THE ORDERED
.                           QTY AS THE SHIPPED QTY.
.
.RELEASE INIT      "1.0"    E.W. LAKE   04/15/88
.                           INITIAL RELEASE
.
............................................................................
.
PRTFLAG  FORM      1
LOCAL    INIT      "LOCAL"
PAGE     FORM      5
PRTLINES FORM      2
DUPLFLAG FORM      1         DUPLEX FLAG 1= NO 2 = YES.
FAXFLAG  FORM      1         PRINT FLAG, 1=laser duplexx PRINTER 2=FAX FORMAT
SRTFLAG  FORM      1         SORT FLAG, 1=LIST#, 2=LIST NAME
SHPFLAG  FORM      1         SHIP FLAG, 1=NO SHIP INFO, 2=SHIP INFO
CFLAG    FORM      1         501Cx FLAG, 1=NO INFO, 2=PRINT INFO
TOTFLAG  FORM      1         TOTL FLAG, 1=NO TOTALS, 2=TOTALS
RUNFLAG  FORM      1         RUNNING CHARGE FLAG, 1=NONE, 2=INCLUDE
BRKFLAG  FORM      1         BROKER FLAG, 1=NO BROKER INVOLVED, 2=BROKER INVLVE
SUMFLAG  FORM      1         SUMMARY FLAG, 2=NO DETAIL, 1=INCLUDE
TITLFLAG FORM      1         SUMMARY FLAG, 2=NO title, 1=INCLUDE
Flatfile file
flatflag form      1         Create flat file for output 2=yes
banrflag form      1      1=no banner page, 2 = banner page
HOLDLIST INIT      "      "
HOLDCNT  INIT      "   "
HOLDOFR  INIT      "   "
HOLDNAME INIT      "                                   "
LSTTAB   FORM      3
.begin patch 3.89
FileCheck FIle
trapcount form      4
.end patch 3.89
.Start patch #3.1 - increased var
.DATEMASK INIT      "XX/XX/XX"
.DATEPRT1 DIM       8
.DATEPRT2 DIM       8
.DATEPRT3 DIM       8
.NUMMASK  INIT      "Z,ZZZ,ZZZ"
DATEMASK INIT      "XX/XX/XXXX"
DATEPRT1 DIM       10
DATEPRT2 DIM       10
DATEPRT3 DIM       10
.begin patch 3.92
DATEPRT4  DIM       10
.end patch 3.92
NUMMASK  INIT      "ZZZ,ZZZ,ZZZ"
.END patch #3.1 - increased var
NUMMASKA INIT      "ZZZZ"
TAXPRT   INIT      "      "
.Start patch #3.1 - increased var
.NUMPRT1  DIM       9
.NUMPRT2  DIM       9
.NUMPRT3  DIM       9
.TORDQTY  FORM      9
.TORDQTYE FORM      9         TOTAL EXHANGE
.TORDQTYR FORM      9         TOTAL RENTAL
NUMPRT1  DIM       11
NUMPRT2  DIM       11
NUMPRT3  DIM       11
TORDQTY  FORM      11
TORDQTYE FORM      11        TOTAL EXHANGE
TORDQTYR FORM      11        TOTAL RENTAL
.End patch #3.1 - increased var
TORDNUM  FORM      6         TOTAL ORDERS PRINTED
TORDNUME FORM      6         TOTAL EXCHANGE ORDERS PRINTED.
TORDNUMR FORM      6         TOTAL RENTAL ORDERS PRINTED,
TORDNUMS FORM      6         TOTAL SPLIT ORDERS  PRINTED.
.Start patch #3.1 - increased var
.TSHPQTY  FORM      9
TSHPQTY  FORM      11
.End patch #3.1 - increased var
TSHPNUM  FORM      6
GORDQTY  FORM      9
GSHPQTY  FORM      9
.Start patch #3.1 - increased var
.NUMMASKB INIT      "ZZZ,ZZZ,ZZZ"
NUMMASKB INIT      "ZZ,ZZZ,ZZZ,ZZZ"
.End patch #3.1 - increased var
NUMPRTA  DIM       4
.Start patch #3.1 - increased var
.NUMPRT1B DIM       11
.NUMPRT2B DIM       11
.NUMPRT3B DIM       11
.NUMPRT4B DIM       11
NUMPRT1B DIM       14
NUMPRT2B DIM       14
NUMPRT3B DIM       14
NUMPRT4B DIM       14
.end patch #3.1 - increased var
.to find version of excel  DH 04/02/09
#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
.to find version of excel
PPMMASK  INIT      "$$$$.ZZ"
PPMPRT   DIM       7
CANTEXT  DIM       11
ENTTEXT  DIM       3
TAPETEXT DIM       14
EXCHTEXT DIM       10
COMTEXT  DIM       11
B11      INIT      "           "
LISTNAME DIM       35
.Start Patch #2.8 - remmed and replaced line, increase var to reflect NINMLR expansion
.CLIENT   DIM       38
CLIENT   DIM       45
.End Patch #2.8 - remmed and replaced line, increase var to reflect NINMLR expansion
REURTNS  INIT      "0000-0001"
C57      FORM      "57"
N52      FORM      5.2
DATE     DIM       8
N42      FORM      4.2
holdsel  dim       35
PERCENT  FORM      4.2
CALCPER  FORM      7.4
nordin    form      8
nordinb    form      8
nordoutb   form      8
nordout   form      8
.START PATCH 3.6 ADDED LOGIC
O2DES2    DIM       40                  .Second half of Select Name, if necessary
.END PATCH 3.6 ADDED LOGIC
.START PATCH 3.83 ADDED LOGIC
books   automation
book    automation
sheets  automation
sheet   automation
ex      automation      class="Excel.Application"
.Variant objects used to talk to outside applications
xlRowHeight         variant
VT_R8     EQU 5           .Double - 8 byte Real
.Booleans
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
VT_I4   EQU 3           .4 byte integer
Zoom85  variant
.Formatting vars needed
SheetsDefault integer 4,"0x00000000"
xlMinimized integer 4,"0xFFFFEFD4"
xlMaximized integer 4,"0xFFFFEFD7"
xlAlignCenter integer 4,"0xffffeff4"
xlUnderlineStyleSingle integer 4,"0x2"
MedThick integer 4,"0xFFFFEFD6"
.
range     dim       20
range2    dim       20
.END PATCH 3.83 ADDED LOGIC
.start PATCH 3.84 ADDED LOGIC
prtnet   dim       3              
.End PATCH 3.84 ADDED LOGIC
.
+.........................................................................
.
. DEFAULT OPTIONS ARE : BREAK BY LIST NUMBER
.                       NO SHIPPING QTY
.                       NO TOTALS
.                       print on laser
.
. $COMMENT CAN MODIFY THE DEFAULTS: ALPHA  :  BREAK BY LIST NAME
.                                   SHIP   :  INCLUDE SHIPPING QTY
.                                   TOTAL  :  INCLUDE TOTALS
.                                   SUM    :  SUMMARY ONLY SUPPRESS DETAILS
.                                   501C   :  INCLUDE MAILER 501Cx STATUS
.                                   FLAT
.
. $INPNAME WILL CONTAIN THE INPUT FILENAME: ??????/TXT OR ??????
. $PRTNAME WILL CONTAIN THE PRINT FILE NAME or 'LOCAL'
.
. IF ANY OF THE ABOVE INFO IS MISSING OR INVALID, REQUEST IT.
.........................................................................
.
         MOVE      "List Usage" TO STITLE
         CMATCH    B1 TO PROGRAM       .CHAINED FROM DSINIT WITH INFO?
         IF        EOS                 .NO
         MOVE      "NORD0005" TO PROGRAM
         MOVE      "TOTAL" TO COMMENT
         MOVE      "NINCAL" TO COMPNME
         MOVE      "DISKIN" TO INPNAME
         MOVE      "LOCAL" TO PRTNAME
         ENDIF
         move      c0 to page
         CALL      PAINT
         TRAP      END IF F5
         MOVE      "Exit" TO PF5
         MOVE      "Options" TO PF4
         TRAP      OPTGET IF F4
         CALL      FUNCDISP
.
         MATCH     "        " TO TODAY
         IF        EQUAL
         GOTO      GETDATE
         ELSE
         IF        EOS
         GOTO      GETDATE
         ENDIF
         ENDIF
         GOTO      BEGIN
.
GETDATE  CLOCK     DATE TO DATE
         IFNZ      PC
         MOVE      "ZZ/ZZ/ZZ" TO TODAY
         MOVE      DATE TO N6
         EDIT      N6 TO TODAY
         MOVE      C0 TO N6
         XIF
         IFZ       PC
         MOVE      DATE TO TODAY
         XIF
.
BEGIN    DISPLAY   *P01:05,"Options     :":
                   *P01:06,"Input File  :":
                   *P01:07,"Print File  :":
                   *P01:09,"Record Count:"
         GOTO      OPTION
.
OPTGET   MOVE      C0 TO SRTFLAG
         MOVE      C0 TO SHPFLAG
         MOVE      C0 TO SUMFLAG
         move      c0 to banrflag
         MOVE      C0 TO TOTFLAG
         MOVE      C0 TO CFLAG
         MOVE      C0 TO FAXFLAG
         move      c0 to flatflag
         RESET     COMMENT
         RESET     levels
         MATCH     "                         " TO levels
         if        equal
         move      c2 to titlflag
         else
         move      c1 to titlflag
         endif
         KEYIN     *P20:10,"ALPHA  :  BREAK BY MAILER NAME":
                   *P20:11,"SHIP   :  INCLUDE SHIPPING QTY":
                   *P20:12,"TOTAL  :  INCLUDE TOTALS":
                   *P20:13,"501C   :  INCLUDE CLIENT TAX STATUS":
                   *P20:15,"FAX    :  FORMAT FOR FAX         ":
                   *P20:14,"SUM    :  SUMMARY ONLY SUPPRESS DETAILS":
                   *P20:16,"DUPLEX :  DUPLEX        ":
                   *P20:17,"FLAT   :  Flat file        ":
                   *p20:18,"Banner  : PRINT Banner Page":
                   *P01:24,*EL,"The Option setting is invalid.":
                   *P15:05,COMMENT:
                   *P20:10,*EL,*P20:11,*EL,*P20:12,*EL,*P20:13,*EL:
                   *P20:14,*EL,*P20:15,*EL,*P20:16,*EL,*P20:17,*EL:
                   *p20:18,*el,*p20:19,*el;
         MATCH     "                         " TO COMMENT
         GOTO      OPTDEFLT IF EQUAL
         KEYIN     *P15:05,COMMENT
OPTION   DISPLAY   *P15:05,COMMENT,b1,LEVELS
         MOVE      C0 TO SRTFLAG
         move      c0 to banrflag
         MOVE      C0 TO SHPFLAG
         MOVE      C0 TO TOTFLAG
         MOVE      C0 TO SUMFLAG
         MOVE      C0 TO CFLAG
         MOVE      C0 TO FAXFLAG
         MOVE      C0 to brkflag
         MOVE      C0 TO DUPLFLAG
         move      c0 to flatflag
         RESET     LEVELS
         MATCH     "                         " TO LEVELS
         if        equal
         move      c2 to titlflag
         else
         move      c1 to titlflag
         endif

.START PATCH 3.83 ADDED LOGIC
          if (COMMENT = "TOTAL FLAT")
.Force Excel Option!!!
.Create the Variant objects
.Booleans
                    create  OTRUE,VarType=VT_BOOL,VarValue=1
                    create  OFALSE,VarType=VT_BOOL,VarValue=0
                    create  Zoom85,VarType=VT_I4,VarValue=1
.
                    create    xlRowHeight,VarType=VT_R8,VarValue="75.0"
.Open Excel application
                    create  ex
          getprop ex,*SheetsInNewWorkbook=SheetsDefault
                  setprop ex,*SheetsInNewWorkbook=C1
                    setprop ex,*WindowState=xlMinimized
.get exel version info
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
.get exel version info

.Create Workbooks collection
                    getprop ex,*Workbooks=books
.Create/Add a single Workbook
                    books.add
                    books.item giving book using 1
.Create Worksheets collection
                    getprop book,*Sheets=sheets
.Create a single Worksheet - we did not need to add it as we set the default above to
.add one new Worksheet each time a Workbook is created.
                    sheets.item giving sheet using 1
                    setprop   sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
                    sheet.range("A1:E1").Merge
                    sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
.Header information
                    setprop   sheet.Range("A5"),*Value="Order Date"
                    setprop   sheet.Range("B5"),*Value="Client P.O."
                    setprop   sheet.Range("C5"),*Value="LR Number"
                    setprop   sheet.Range("D5"),*Value="Client"
                    setprop   sheet.Range("E5"),*Value=""
                    setprop   sheet.Range("F5"),*Value="Price"
                    setprop   sheet.Range("G5"),*Value="Quantity"
                    setprop   sheet.Range("H5"),*Value=""
                    setprop   sheet.Range("I5"),*Value=""
                    setprop   sheet.Range("J5"),*Value="Mail Date"
                    setprop   sheet.Range("K5"),*Value="Return Date"
                    setprop   sheet.Range("L5"),*Value=""
                    setprop   sheet.Range("M5"),*Value="Offer"
                    setprop   sheet.Range("N5"),*Value=""
                    setprop   sheet.Range("O5"),*Value=""
                    setprop   sheet.Range("P5"),*Value="Exchange/Rent"
                    setprop   sheet.Range("Q5"),*Value=""
                    setprop   sheet.Range("R5"),*Value=""
                    setprop   sheet.Range("S5"),*Value="Select"
.begin patch 3.85             
                    setprop   sheet.Range("T5"),*Value="Net"
.Begin patch 3.92
                    setprop   sheet.Range("W5"),*Value="Invoiced"
.Header Formatting
.                   setprop   sheet.Range("A5:S5"),*HorizontalAlignment=xlAlignCenter
.                   setprop   sheet.Range("A5:S5").Font,*Bold="True"
.                   sheet.range("A5:S5").BorderAround using *LineStyle=1,*Weight=MedThick
                    setprop   sheet.Range("A5:W5"),*HorizontalAlignment=xlAlignCenter
                    setprop   sheet.Range("A5:W5").Font,*Bold="True"
                    sheet.range("A5:W5").BorderAround using *LineStyle=1,*Weight=MedThick
.end patch 3.92
.end patch 3.85               
.
                    move      C6,howmany
          endif
.END PATCH 3.83 ADDED LOGIC
         MATCH     "                         " TO COMMENT
         GOTO      OPTDEFLT IF EQUAL
         RESET     COMMENT
         SCAN      "ALPHA" IN COMMENT
         CALL      OPTALPH IF EQUAL
         RESET     COMMENT
         SCAN      "SHIP" IN COMMENT
         CALL      OPTSHIP IF EQUAL
         RESET     COMMENT
         SCAN      "FLAT" IN COMMENT
         CALL      OPTFLAT IF EQUAL
         reset     comment
         SCAN      "SUM" IN COMMENT
         CALL      OPTSUM IF EQUAL
         RESET     COMMENT
         SCAN      "501C" IN COMMENT
         CALL      OPT501C IF EQUAL
         RESET     COMMENT
         SCAN      "BANNER" IN COMMENT
         CALL      optBanr IF EQUAL
         RESET     COMMENT
         SCAN      "FAX" IN COMMENT
         CALL      OPTPRT IF EQUAL
         RESET     COMMENT
         SCAN      "DUPLEX" IN COMMENT
         CALL      OPTDUPL IF EQUAL
         RESET     COMMENT
         SCAN      "BRK" IN COMMENT
         CALL      OPTBRK IF EQUAL
         RESET     COMMENT
         SCAN      "TOTAL" IN COMMENT
         GOTO      OPTNG IF NOT EQUAL
         CALL      OPTTOTL
         GOTO      INPGET
optng    KEYIN     *P20:10,"ALPHA  :  BREAK BY MAILER NAME":
                   *P20:11,"SHIP   :  INCLUDE SHIPPING QTY":
                   *P20:12,"TOTAL  :  INCLUDE TOTALS":
                   *P20:13,"501C   :  INCLUDE CLIENT TAX STATUS":
                   *P20:15,"FAX    :  FORMAT FOR FAX         ":
                   *P20:14,"SUM    :  SUMMARY ONLY SUPPRESS DETAILS":
                   *P20:16,"DUPLEX :  DUPLEX        ":
                   *P20:17,"FLAT   :  Flat File        ":                                                                         *P20:16,"DUPLEX :  DUPLEX        ":
                   *p20:18,"Banner  : PRINT Banner Page":
                   *P01:24,*EL,"The Option setting is invalid.":
                   *P15:05,COMMENT:
                   *P20:10,*EL,*P20:11,*EL,*P20:12,*EL,*P20:13,*EL:
                   *P20:14,*EL,*P20:15,*EL,*P20:16,*EL,*P20:17,*EL:
                   *p20:18,*el,*p20:19,*el;
         GOTO      OPTGET
OPTDEFLT MOVE      C1 TO SRTFLAG
         MOVE      C1 TO SHPFLAG
         MOVE      C1 TO TOTFLAG
         MOVE      C1 TO CFLAG
         MOVE      C1 TO SUMFLAG
         MOVE      C1 TO FAXFLAG
         MOVE      C1 TO BRKFLAG
         move      c1 to banrflag
         MOVE      C1 TO duplFLAG
         move      c1 to flatflag
         GOTO      INPGET
OPTALPH  MOVE      C2 TO SRTFLAG
         RETURN
OPTSHIP  MOVE      C2 TO SHPFLAG
         RETURN
OPT501C  MOVE      C2 TO CFLAG
         RETURN
OPTSUM   MOVE      C2 TO SUMFLAG
         RETURN
OPTFLAT  MOVE      C2 TO FLATFLAG
         RETURN
OPTTOTL  MOVE      C2 TO TOTFLAG
         RETURN
OPTDUPL  MOVE      C2 TO DUPLFLAG
         RETURN
OPTBRK  MOVE      C1 TO BRKFLAG
        move      c3 to SHPFLAG
         RETURN
OPTPRT   MOVE      C2 TO FAXFLAG
         RETURN
optbanr
         MOVE      C2 TO banrFLAG
         return
.
INPGET   TRAP      INPNG IF IO
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         DISPLAY   *P15:06,INPNAME
          compare   c2 to flatflag
          if equal
.START PATCH 3.83 ADDED LOGIC
                    RESET     COMMENT
                    if (COMMENT <> "TOTAL FLAT")
.END PATCH 3.83 ADDED LOGIC
                              clear     str30
.START PATCH 3.4 REPLACED LOGIC
.          append    "g:\data\" to str30
                              append    NTWKPATH1 to str30
.END PATCH 3.4 REPLACED LOGIC
.patch3.7
                              scan      ".SRT",INPNAME
                              if equal
                                        movefptr INPNAME,n2
                                        sub       c1 from n2
                                        reset     INPNAME
                                        setlptr   INPNAME,n2
                              endif
.patch3.7
                              append    inpname to str30
                              append    ".csv" to str30
                              reset     str30
                              prepare   flatfile,str30
.START PATCH 3.83 ADDED LOGIC
                    endif
.END PATCH 3.83 ADDED LOGIC
          endif
         MOVE      INPNAME TO NORDNAME
         GOTO      PRTGET
INPNG    NORETURN
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *P15:06,INPNAME
         GOTO      INPGET
.
PRTGET   MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MOVE      C1 TO PRTFLAG
         MATCH     LOCAL TO PRTNAME
         GOTO      PRESTART IF EQUAL
         MOVE      C2 TO PRTFLAG
         PACK      PRTFILE WITH pdrive,PRTNAME
         SPLOPEN   PRTFILE
         compare   c2 to banrflag
         call      prtbanr if equal
         DISPLAY   *P15:07,PRTNAME
         GOTO      PRESTART
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
.
+........................................................................
.
PRESTART KEYIN     *P1:24,*T05,STR1;
         CLEAR     PF4
         TRAPCLR   F4
         CALL      FUNCDISP
START    CALL      NORD2SEQ
         GOTO      EOJ IF OVER
.START PATCH 3.2 - NEW LOGIC
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
.END PATCH 3.2 - NEW LOGIC
.START PATCH 3.6 ADDED LOGIC
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
                    clear     O2DES2
                    unpack    OPPM,str3,str2
                    pack      str6,str3,".",str2
                    rep       zfill,str6
                    move      C0,NSEL2PRICE
                    move      str6,NSEL2PRICE
                    move      "/M",NMODDESC
          else
                    call      PARSITUP using O2DES,NSEL2NAME,C1
                    call      PARSITUP using O2DES2,NSEL2NAME,C1
                    reset     NSEL2NAME
                    call      Trim using NSEL2NAME
                    pack      NMODFLD,NSEL2DESC
                    rep       zfill,NMODFLD
                    move      "NMODKEY",Location
                    pack      KeyLocation,"Key: ",NMODFLD
                    call      NMODKEY
                    if over
                              move      "/M",NMODDESC
                    endif
          endif
.END PATCH 3.6 ADDED LOGIC
         ADD       C1 TO N6
         DISPLAY   *P15:09,N6

.         compare   c2 to faxflag
.        if        equal
.Start patch #3.1 - increased var
.         move      c0 to n7
.         move      oqty to n7
.         compare   c0 to n7
         move      c0 to n9
         move      oqty to n9
         compare   c0 to n9
.end patch #3.1 - increased var
         if        equal
         goto      start
         endif
.        endif
.
.begin patch 3.5
.patch3.5
        call trim using inits
.subpatch3.5
.only lcrs
                              if (inits = "L")
          CMATCH    "l" TO OSTAT       lcr order ?
          GOTO      Start IF NOT EQUAL     NO,skip .
            MOVE      "(LCR)" TO CANTEXT
                                        goto      CAN
                              endif
.sub patch3.5
.only pending orders
                    if (inits = "P")
          CMATCH    "p" TO OSTAT       pending order ?
          GOTO      Start IF NOT EQUAL     NO,skip .
            MOVE      "(Pending)" TO CANTEXT
                                        goto      CAN
                              endif
.sub patch3.5
.skip cancelled
                if (inits = "E")
          RESET     CANCODES
          SCAN      OSTAT IN CANCODES
          GOTO      Start IF EQUAL     YES,skip .
                              endif
.sub patch3.5
                if (inits = "N")
                                        if (onetper = "  ")
                    GOTO Start
                                        endif
                              endif
.sub patch3.5
.begin patch 2.9
         CMATCH    "p" TO OSTAT       Pending order ?
         GOTO      Start IF EQUAL     YES, skip.
         CMATCH    "x" TO OSTAT       Cancelled Pending order ?
         GOTO      Start IF EQUAL     YES, skip.
         CMATCH    "l" TO OSTAT       lcr order ?
         GOTO      Start IF EQUAL     YES, skip.
         CMATCH    "z" TO OSTAT       Cancelled lcr order ?
         GOTO      Start IF EQUAL     YES, skip.

.note cancodes also updated to skip cancelled pending orders.
.end patch 2.9
.begin patch 3.92
          if        (flatflag = c2 & (Ostat = "B" or ostat = "Q"))    .dumping to excel and billed order
          packkey   Ninvfld from Olrn
          move      c1,NInvpath
          Clear     Dateprt4
          clear     invdtem
          clear     invdted
          clear     invdtec
          clear     invdtey
          call      Ninvkey   
                    if        Not Over
                    MOVE      DATEMASK TO DATEPRT4
                    PACK      DATEPRT4 FROM INVDTEM,SLASH,INVDTED,SLASH,INVDTEC,INVDTEY
                    endif
         Endif           
.end patch 3.92
         MOVE      B11 TO CANTEXT
CAN
         RESET     CANCODES
         SCAN      OSTAT IN CANCODES
         GOTO      ENTCHK IF NOT EQUAL
         MOVE      "(CANCELLED)" TO CANTEXT
         COMPARE   C2 TO FAXFLAG
         GOTO      START IF EQUAL
.
.
ENTCHK   MOVE      B3 TO ENTTEXT
         RESET     ENTCODES
         SCAN      OELCODE IN ENTCODES
         GOTO      TAPECHK IF NOT EQUAL
         MOVE      "ALL" TO ENTTEXT
.
TAPECHK  CLEAR     TAPETEXT
         CMATCH    "N" TO OTAPERET      *MAG TAPE RETURNABLE ?
         GOTO      COMCHK IF NOT EQUAL
         MOVE      "NON-RETURNABLE" TO TAPETEXT
.
COMCHK   MOVE      B11 TO COMTEXT
         CMATCH    "C" TO OCOMSLCT
         IF        EQUAL
         MOVE      "(COMSELECT)" TO COMTEXT
         ENDIF
         CMATCH    "L" TO OCOMSLCT
         IF        EQUAL
         MOVE      "(LIFESTYLE)" TO COMTEXT
         ENDIF
         CMATCH    "I" TO OCOMSLCT
         IF        EQUAL
         MOVE      "(IC SYSTEMS)" TO COMTEXT
         ENDIF
.
SRTCHK
.         display   *p10:14,"srtchk",olnum,holdlist,o1des,holdname,*b,*w2
         branch    SRTFLAG TO CHKNUM,CHKALPH
.         goto      exchk
CHKNUM
         MATCH     OLNUM TO HOLDLIST
         GOTO      EXCHK IF EQUAL
         CALL      NEWLST
         GOTO      EXCHK
.
CHKALPH  MATCH     O1DES TO HOLDNAME
         CALL      NEWLST IF NOT EQUAL
.
EXCHK
         call      calcnet
         MOVE      B10 TO EXCHTEXT
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES
         GOTO      RENT IF NOT EQUAL
         MOVE      "*EXCHANGE*" TO EXCHTEXT
         RESET     CANCODES
         SCAN      OSTAT IN CANCODES
         GOTO      DETAIL IF EQUAL
.Start patch #3.1 - increased var
.         MOVE      C0 TO N7
.         MOVE      OEXQTY TO N7
.         COMPARE   C0 TO N7
.         IF        EQUAL
.         ADD       C1 TO TORDNUME
.         MOVE      C0 TO N7
.         MOVE      OQTY TO N7
.         ADD       N7 TO TORDQTYE
.         ELSE
.         ADD       C1 TO TORDNUMS
.         ADD       N7 TO TORDQTYE
.         SUB       N7 FROM TORDQTYR
.         MOVE      C0 TO N7
.         MOVE      OQTY TO N7
.         ADD       N7 TO TORDQTYR
.
         MOVE      C0 TO N9
         MOVE      OEXQTY TO N9
         COMPARE   C0 TO N9
         IF        EQUAL
         ADD       C1 TO TORDNUME
         MOVE      C0 TO N9
         MOVE      OQTY TO N9
         ADD       N9 TO TORDQTYE
         ELSE
         ADD       C1 TO TORDNUMS
         ADD       N9 TO TORDQTYE
         SUB       N9 FROM TORDQTYR
         MOVE      C0 TO N9
         MOVE      OQTY TO N9
         ADD       N9 TO TORDQTYR
.end patch #3.1 - increased var
         ENDIF
         GOTO      DETAIL
RENT     RESET     CANCODES
         SCAN      OSTAT IN CANCODES
         GOTO      DETAIL IF EQUAL
         ADD       C1 TO TORDNUMR
.Start patch #3.1 - increased var
.         MOVE      C0 TO N7
.         MOVE      OQTY TO N7
.         ADD       N7 TO TORDQTYR
         MOVE      C0 TO N9
         MOVE      OQTY TO N9
         ADD       N9 TO TORDQTYR
.End patch #3.1 - increased var
.
.start PATCH 3.84 ADDED LOGIC
         move      b4 to prtnet
         cmatch    b1 to onetfm
         goto      detail if equal
         move      c0 to n2
         move      onetper to n2
         compare   c0 to n2
         if        not equal
         clear     prtnet
         append    onetper to prtnet
         append    "%" to prtnet
         reset     prtnet
         endif
.End patch #3.84 
.
DETAIL   MOVE      DATEMASK TO DATEPRT1
.         EDIT      ORDDATE  TO DATEPRT1
.         MOVE      DATEMASK TO DATEPRT2
.         EDIT      ORDMLDDT TO DATEPRT2
.         MOVE      DATEMASK TO DATEPRT3
.         EDIT      ORDRTNDT TO DATEPRT3
.Start Patch #3.1 - added century
.         PACK      DATEPRT1 FROM OODTEM,SLASH,OODTED,SLASH,OODTEY
.         MOVE      DATEMASK TO DATEPRT2
.         PACK      DATEPRT2 FROM OMDTEM,SLASH,OMDTED,SLASH,OMDTEY
.         MOVE      DATEMASK TO DATEPRT3
.         PACK      DATEPRT3 FROM ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEY
.
         PACK      DATEPRT1 FROM OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
         MOVE      DATEMASK TO DATEPRT2
         PACK      DATEPRT2 FROM OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
         MOVE      DATEMASK TO DATEPRT3
         PACK      DATEPRT3 FROM ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY
.End Patch #3.1 - added century
         MOVE      NUMMASKA TO NUMPRTA
         MOVE      OMLRNUM TO N4
         EDIT      N4 TO NUMPRTA
         MOVE      NUMMASK  TO NUMPRT1
.Start patch #3.1 - increased var
.         MOVE      OQTY   TO N7
.         EDIT      N7       TO NUMPRT1
.         MOVE      NUMMASK  TO NUMPRT3          SET UP SPLIT QTY
.         MOVE      C0 TO N7
.         MOVE      OEXQTY   TO N7
.         EDIT      N7       TO NUMPRT3
.
         MOVE      OQTY   TO N9
         EDIT      N9       TO NUMPRT1
         MOVE      NUMMASK  TO NUMPRT3          SET UP SPLIT QTY
         MOVE      C0 TO N9
         MOVE      OEXQTY   TO N9
         EDIT      N9       TO NUMPRT3
.end patch #3.1 - increased var
         MOVE      PPMMASK TO PPMPRT
         MOVE      C0 TO N52
.START PATCH 3.6 REPLACED LOGIC
.         MOVE      OPPM TO N52
.         DIV       "100" INTO N52
tester
          move      NSEL2PRICE,N52
.END PATCH 3.6 REPLACED LOGIC
         MOVE      N52 TO N42
         EDIT      N42 TO PPMPRT
         RESET     CANCODES
         SCAN      OSTAT IN CANCODES
         GOTO      CHKMLR IF EQUAL
.Start patch #3.1 - increased var
.         MOVE      C0  TO    N7
.         MOVE      OQTY TO N7
.         ADD       N7       TO TORDQTY
         MOVE      C0  TO    N9
         MOVE      OQTY TO N9
         ADD       N9       TO TORDQTY
.End patch #3.1 - increased var
         ADD       C1 TO TORDNUM
.
. FORMULATE THE CLIENT LINE.
.
CHKMLR   compare   c1 to brkflag
         if        equal
         move      ordmname to client
         goto      c501
         endif
         PACK      STR7 WITH OMLRNUM,OCOBN
         CLEAR     TAXPRT
         MATCH     STR7 TO MKEY
         GOTO      DTL0 IF EQUAL
         MOVE      STR7 TO MKEY
         CALL      NMLRKEY
         call      MLRNG IF OVER
.         SCAN      "C/O" IN MCOMP
.        GOTO      NOBRK IF NOT EQUAL
.         BUMP      MCOMP BY -1
.         APPEND    B3 TO MCOMP
.         RESET     MCOMP
.         SETLPTR   MCOMP
.         ENDSET    MCOMP
.BRKA     CMATCH    B1 TO MCOMP
.         GOTO      BRKB IF NOT EQUAL
.         BUMP      MCOMP BY -1
.         GOTO      BRKA IF NOT EOS
.BRKB     LENSET    MCOMP
.         RESET     MCOMP
.         CLEAR     CLIENT
.         APPEND    MCOMP TO CLIENT
.         APPEND    SLASH   TO CLIENT
.         APPEND    MNAME TO CLIENT
.         RESET     CLIENT
.         GOTO      C501
NOBRK    MOVE      MCOMP TO CLIENT
C501     BRANCH    CFLAG OF DTL0,C501A
         GOTO      DTL0
C501A
.         MOVE      OMLRNUM TO NMTXFLD
         MOVE      compnum TO nmtxfld
         MOVE      NO TO OVER
         CLEAR     TAXPRT
         CALL      NMTXKEY
         GOTO      DTL0 IF OVER
         MOVE      C0 TO N1
         MOVE      MTXC501 TO N1
         BRANCH    N1 OF DTL0,DTL0,C5013,C5014,C5015,C5016
         GOTO      DTL0
C5013    MOVE      "501C-3" TO TAXPRT
         GOTO      DTL0
C5014    MOVE      "501C-4" TO TAXPRT
         GOTO      DTL0
C5015    MOVE      "501C-5" TO TAXPRT
         GOTO      DTL0
C5016    MOVE      "501C-6" TO TAXPRT
         GOTO      DTL0
.
DTL0
          branch    sumflag of dtl0a,start

dtl0a    COMPARE   C57 TO PRTLINES
         CALL      HEADING IF NOT LESS
         BRANCH    SHPFLAG TO DTL1,DTL2,DTL1
.
DTL1
.START PATCH 3.2 - REPLACED LOGIC, OODES --> OFDESC
.Start patch #3.1 - remmed and replaced logic
.         PRINT     *N,*03,DATEPRT1:
.                      *12,OMLRPON:
.                      *25,OLRN:
.                      *34,CLIENT:
.                      *65,TAXPRT:
.                      *83,NUMPRT1,B1,ENTTEXT:
.                      *99,NUMPRT2:
.                      *114,DATEPRT2:
.                      *125,DATEPRT3:
.                   *N,*26,NUMPRTA:
.                      *34,OODES:                       OFFER
.                      *01,CANTEXT:
.                      *72,COMTEXT:
.                      *83,EXCHTEXT:
.                      *94,NUMPRT3:
.                      *119,TAPETEXT:
.                   *N,*34,O2DES,*72,ppmprt,"/M"
         PRINT     *N,*01,DATEPRT1:
                      *12,OMLRPON:
                      *25,OLRN:
                      *34,CLIENT:
.                      *65,TAXPRT:
                      *81,NUMPRT1,B1,ENTTEXT:
                      *96,NUMPRT2:
                      *110,DATEPRT2:
                      *123,DATEPRT3:
                   *N,*26,NUMPRTA:
                      *34,OFDESC:                       OFFER
                      *65,TAXPRT:
                      *01,CANTEXT:
                      *72,COMTEXT:
                      *83,EXCHTEXT:
                      *97,NUMPRT3:
                      *119,TAPETEXT:
.                   *N,*34,O2DES,*72,ppmprt,NMODDESC
.START PATCH 3.84
                   *N,*34,O2DES,*72,ppmprt,NMODDESC,prtnet
.End PATCH 3.84 
.START PATCH 3.6 ADDED LOGIC
          call      Trim using O2DES2
          if (O2DES2 <> "")
                    PRINT     *34,O2DES2
                    ADD       C1,PRTLINES
          endif
.END PATCH 3.6 ADDED LOGIC
.End patch #3.1 - remmed and replaced logic
.END PATCH 3.2 - REPLACED LOGIC, OODES --> OFDESC
         compare    c2 to flatflag
         if         equal
.START PATCH 3.2 - REPLACED LOGIC, OODES --> OFDESC
.         write      flatfile,seq;*cdfon:
.                    DATEPRT1:
.                    OMLRPON:
.                    OLRN:
.                    CLIENT:
.                    TAXPRT:
.                    PPMPRT:
.                    NUMPRT1:
.                    ENTTEXT:
.                    NUMPRT2:
.                    DATEPRT2:
.                    DATEPRT3:
.                    NUMPRTA:
.                    OODES:                       OFFER
.                    CANTEXT:
.                    COMTEXT:
.                    EXCHTEXT:
.                    NUMPRT3:
.                    TAPETEXT:
.                    O2DES
.                    endif
.START PATCH 3.6 REPLACED LOGIC
.         write      flatfile,seq;*cdfon:
.                    DATEPRT1:
.                    OMLRPON:
.                    OLRN:
.                    CLIENT:
.                    TAXPRT:
.                    PPMPRT:
.                    NUMPRT1:
.                    ENTTEXT:
.                    NUMPRT2:
.                    DATEPRT2:
.                    DATEPRT3:
.                    NUMPRTA:
.                    OFDESC:                       OFFER
.                    CANTEXT:
.                    COMTEXT:
.                    EXCHTEXT:
.                    NUMPRT3:
.                    TAPETEXT:
.                    O2DES
.START PATCH 3.83 ADDED LOGIC
          RESET     COMMENT
          if (COMMENT = "TOTAL FLAT")
                    call      WriteExcelRecord
          else
.END PATCH 3.83 ADDED LOGIC
                   write      flatfile,seq;*cdfon:
                              DATEPRT1:
                              OMLRPON:
                              OLRN:
                              CLIENT:
                              TAXPRT:
                              PPMPRT:
                              NUMPRT1:
                              ENTTEXT:
                              NUMPRT2:
                              DATEPRT2:
                              DATEPRT3:
                              NUMPRTA:
                              OFDESC:                       OFFER
                              CANTEXT:
                              COMTEXT:
                              EXCHTEXT:
                              NUMPRT3:
                              TAPETEXT:
                              NSEL2NAME:
                              o1des
.END PATCH 3.6 REPLACED LOGIC
.START PATCH 3.83 ADDED LOGIC
          endif
.END PATCH 3.83 ADDED LOGIC
        endif
.END PATCH 3.2 - REPLACED LOGIC, OODES --> OFDESC
.                   *N,*40,O2DES,b1,percent,"%"
         ADD       C4 TO PRTLINES
         GOTO      START
.
DTL2
.Start patch #3.1 - increased var
.Took care of SQUANT for when it is converted
.         MOVE      C0 TO N7
.         MOVE      B7 TO SQUANT
         MOVE      C0 TO N9
         MOVE      B9 TO SQUANT
.End patch #3.1 - increased var
         RESET     REURTNS
         SCAN      ORTNNUM IN REURTNS        RE-USE RETURN-TO #?
         GOTO      DTL2B IF NOT EQUAL       NO. CHECK FOR SHIPPING RECORD
.START PATCH 3.3 - REPLACED LOGIC - UN-REMMED CODE
..Start Patch #3.1 - logic until SQAUNT increased
         MOVE      OQTY TO SQUANT         YES. USE ORDERED QTY AS SHIPPED QTY.
.         COUNT     N1,OQTY
.         IF (N1 = 9)
.                   bump      OQTY,C2
.                   MOVE      OQTY TO SQUANT         YES. USE ORDERED QTY AS SHIPPED QTY.
.                   RESET     OQTY
.         ELSEIF (N1 = 8)
.                   bump      OQTY,C1
.                   MOVE      OQTY TO SQUANT         YES. USE ORDERED QTY AS SHIPPED QTY.
.                   RESET     OQTY
.         ELSE   .N1 = 7
.                   MOVE      OQTY TO SQUANT         YES. USE ORDERED QTY AS SHIPPED QTY.
.         ENDIF
..End Patch #3.1 - logic until SQAUNT increased
.END PATCH 3.3 - REPLACED LOGIC - UN-REMMED CODE
         GOTO      DTL2C
DTL2B    MOVE      OLRN TO NSHPFLD
         CALL      NSHPKEY
DTL2C    MOVE      NUMMASK  TO NUMPRT2
.Start patch #3.1 - increased var
.Took care of SQUANT for when it is converted
.         MOVE      SQUANT   TO N7
.         EDIT      N7       TO NUMPRT2
         MOVE      SQUANT   TO N9
         EDIT      N9       TO NUMPRT2
.End patch #3.1 - increased var
         RESET     CANCODES
         SCAN      OSTAT IN CANCODES
         GOTO      DTL2D IF EQUAL
.Start patch #3.1 - increased var
.         ADD       N7       TO TSHPQTY
.         COMPARE   C0 TO N7
         ADD       N9       TO TSHPQTY
         COMPARE   C0 TO N9
.End patch #3.1 - increased var
         IF        NOT EQUAL
         ADD       C1 TO TSHPNUM
         ENDIF
DTL2D    COMPARE   C57 TO PRTLINES
         CALL      HEADING IF NOT LESS
.START PATCH 3.2 - REPLACED LOGIC, OODES --> OFDESC
.Start patch #3.1 - remmed and replaced logic
.         PRINT     *N,*03,DATEPRT1:
.                      *12,OMLRPON:
.                      *25,OLRN:
.                      *34,CLIENT:
.                      *65,TAXPRT:
.                      *83,NUMPRT1,B1,ENTTEXT:
.                      *99,NUMPRT2:
.                      *114,DATEPRT2:
.                      *125,DATEPRT3:
.                   *N,*26,NUMPRTA:
.                      *34,OODES:                       OFFER
.                      *01,CANTEXT:
.                      *72,COMTEXT:
.                      *83,EXCHTEXT:
.                      *94,NUMPRT3:
.                      *119,TAPETEXT:
.                   *N,*34,O2DES,*72,PPMPRT,"/M"
         PRINT     *N,*01,DATEPRT1:
                      *12,OMLRPON:
                      *25,OLRN:
                      *34,CLIENT:
                      *65,TAXPRT:
                      *81,NUMPRT1,B1,ENTTEXT:
                      *96,NUMPRT2:
                      *110,DATEPRT2:
                      *123,DATEPRT3:
                   *N,*26,NUMPRTA:
                      *34,OFDESC:                       OFFER
                      *01,CANTEXT:
                      *72,COMTEXT:
                      *83,EXCHTEXT:
                      *97,NUMPRT3:
                      *119,TAPETEXT:
.                   *N,*34,O2DES,*72,PPMPRT,NMODDESC
.START PATCH 3.84
                   *N,*34,O2DES,*72,PPMPRT,NMODDESC,prtnet
.End PATCH 3.84
.START PATCH 3.6 ADDED LOGIC
          call      Trim using O2DES2
          if (O2DES2 <> "")
                    PRINT     *34,O2DES2
                    ADD       C1,PRTLINES
          endif
.END PATCH 3.6 ADDED LOGIC
.End patch #3.1 - remmed and replaced logic
.END PATCH 3.2 - REPLACED LOGIC, OODES --> OFDESC
         compare   c2 to flatflag
         if        equal
.START PATCH 3.2 - REPLACED LOGIC, OODES --> OFDESC
.         write     flatfile,seq;*cdfon:
.                   DATEPRT1:
.                   OMLRPON:
.                   OLRN:
.                   CLIENT:
.                   TAXPRT:
.                   PPMPRT:
.                   NUMPRT1:
.                   ENTTEXT:
.                   NUMPRT2:
.                   DATEPRT2:
.                   DATEPRT3:
.                   NUMPRTA:
.                   OODES:                       OFFER
.                   CANTEXT:
.                   COMTEXT:
.                   EXCHTEXT:
.                   NUMPRT3:
.                   TAPETEXT:
.                   O2DES
.          endif
.START PATCH 3.6 REPLACED LOGIC
.         write     flatfile,seq;*cdfon:
.                   DATEPRT1:
.                   OMLRPON:
.                   OLRN:
.                   CLIENT:
.                   TAXPRT:
.                   PPMPRT:
.                   NUMPRT1:
.                   ENTTEXT:
.                   NUMPRT2:
.                   DATEPRT2:
.                   DATEPRT3:
.                   NUMPRTA:
.                   OFDESC:                       OFFER
.                   CANTEXT:
.                   COMTEXT:
.                   EXCHTEXT:
.                   NUMPRT3:
.                   TAPETEXT:
.                   O2DES
.START PATCH 3.83 ADDED LOGIC
          RESET     COMMENT
          if (COMMENT = "TOTAL FLAT")
                    call      WriteExcelRecord
          else
.END PATCH 3.83 ADDED LOGIC
                   write     flatfile,seq;*cdfon:
                             DATEPRT1:
                             OMLRPON:
                             OLRN:
                             CLIENT:
                             TAXPRT:
                             PPMPRT:
                             NUMPRT1:
                             ENTTEXT:
                             NUMPRT2:
                             DATEPRT2:
                             DATEPRT3:
                             NUMPRTA:
                             OFDESC:                       OFFER
                             CANTEXT:
                             COMTEXT:
                             EXCHTEXT:
                             NUMPRT3:
                             TAPETEXT:
                             NSEL2NAME:
                             o1des
.END PATCH 3.6 REPLACED LOGIC
.START PATCH 3.83 ADDED LOGIC
          endif
.END PATCH 3.83 ADDED LOGIC
        endif
.END PATCH 3.2 - REPLACED LOGIC, OODES --> OFDESC
.                   *N,*40,O2DES,b1,percent,"%"
         ADD       C4 TO PRTLINES
         GOTO      START
+............................................................................
.
. NEW LIST. PRINT TOTALS FOR OLD LIST IF REQUESTED.
.           EJECT TO NEW PAGE WITH NEW LIST NAME.
.
NEWLST   BRANCH    TOTFLAG TO NEWLSTB
         COMPARE   C0 TO PAGE
         GOTO      NEWLSTB IF EQUAL
         CALL      LSTTOTLS
.
NEWLSTB  BRANCH    SRTFLAG TO BYNUM,BYALPH
BYNUM    MOVE      OLNUM  TO HOLDLIST
         MOVE      O1DES TO LISTNAME
         GOTO      CNTRLST
.
BYALPH   MOVE      O1DES TO HOLDNAME
         MOVE      O1DES TO LISTNAME
.
CNTRLST  SETLPTR   LISTNAME
         ENDSET    LISTNAME
CHKLHEAD CMATCH    B1 TO LISTNAME
         GOTO      SETLHEAD IF NOT EQUAL
         BUMP      LISTNAME BY -1
         GOTO      CHKLHEAD IF NOT EOS
SETLHEAD MOVEFPTR  LISTNAME TO N3
         MOVE      "132" TO LSTTAB
         SUBTRACT  N3 FROM LSTTAB
         DIVIDE    C2 INTO LSTTAB
         RESET     LISTNAME
         SETLPTR   LISTNAME
         CALL      HEADING
         RETURN
.
LSTTOTLS COMPARE   C57 TO PRTLINES
         CALL      HEADING IF NOT LESS
         MOVE      NUMMASKB TO NUMPRT1B
         EDIT      TORDQTY  TO NUMPRT1B
         MOVE      NUMMASKB TO NUMPRT2B
         EDIT      TSHPQTY  TO NUMPRT2B
         MOVE      NUMMASKB TO NUMPRT3B
         EDIT      TORDQTYE TO NUMPRT3B
         MOVE      NUMMASKB TO NUMPRT4B
         EDIT      TORDQTYR TO NUMPRT4B
         COMPARE   C0 TO TORDQTY
         RETURN    IF EQUAL
         BRANCH    SHPFLAG TO NEWTOT1,NEWTOT2
.Start patch #3.1 - remmed and replaced logic
.NEWTOT1  PRINT     *N,*83,"---------":
.                   *N,*1,"EXCHANGE ORDERS: ",TORDNUME:
.                      *63,"EXCHANGE USAGE:",*81,NUMPRT3B:
.                   *N,*1,"RENTAL   ORDERS: ",TORDNUMR:
.                      *63,"RENTAL   USAGE:",*81,NUMPRT4B:
.                   *N,*1,"RENT/EX  ORDERS: ",TORDNUMS:
.                   *N,*1,"TOTAL    ORDERS: ",TORDNUM:
.                      *63,"TOTAL    USAGE:":
.                      *81,NUMPRT1B
.         goto       totexit
..
.NEWTOT2  PRINT     *N,*83,"---------":
.                   *N,*1,"EXCHANGE ORDERS: ",TORDNUME:
.                      *63,"EXCHANGE USAGE:",*81,NUMPRT3B:
.                   *N,*1,"RENTAL   ORDERS: ",TORDNUMR:
.                      *63,"RENTAL   USAGE:",*81,NUMPRT4B:
.                   *N,*1,"RENT/EX  ORDERS: ",TORDNUMS:
.                   *N,*1,"TOTAL    ORDERS: ",TORDNUM:
.                      *63,"TOTAL    USAGE:":
.                      *81,NUMPRT1B:
.                   *N,*1,"TOTAL   SHIPPED:",TSHPNUM,*81,NUMPRT2B
NEWTOT1  PRINT     *80,"------------":
                   *N,*1,"EXCHANGE ORDERS: ",TORDNUME:
                      *63,"EXCHANGE USAGE:",*78,NUMPRT3B:
                   *N,*1,"RENTAL   ORDERS: ",TORDNUMR:
                      *63,"RENTAL   USAGE:",*78,NUMPRT4B:
                   *N,*1,"RENT/EX  ORDERS: ",TORDNUMS:
                   *N,*1,"TOTAL    ORDERS: ",TORDNUM:
                      *63,"TOTAL    USAGE:":
                      *78,NUMPRT1B
.START PATCH 3.83 ADDED LOGIC
          if (COMMENT = "TOTAL FLAT")
                    add       C2,howmany,result
                    move      result,str9
                    call      Trim using str9
                    pack      str12,"A",str9
                    setprop sheet.range(str12),*Value="Exchange Orders:"
                    pack      str12,"C",str9
                    setprop sheet.range(str12),*Value=TORDNUME
                    pack      str12,"E",str9
                    setprop sheet.range(str12),*Value="Exchange Usage:"
                    pack      str12,"G",str9
                    setprop sheet.range(str12),*Value=NUMPRT3B
.
                    add       C1,result
                    move      result,str9
                    call      Trim using str9
                    pack      str12,"A",str9
                    setprop sheet.range(str12),*Value="Rental Orders:"
                    pack      str12,"C",str9
                    setprop sheet.range(str12),*Value=TORDNUMR
                    pack      str12,"E",str9
                    setprop sheet.range(str12),*Value="Rental Usage:"
                    pack      str12,"G",str9
                    setprop sheet.range(str12),*Value=NUMPRT4B
.
                    add       C1,result
                    move      result,str9
                    call      Trim using str9
                    pack      str12,"A",str9
                    setprop sheet.range(str12),*Value="Rent/Ex Orders:"
                    pack      str12,"C",str9
                    setprop sheet.range(str12),*Value=TORDNUMS
.
                    add       C1,result
                    move      result,str9
                    call      Trim using str9
                    pack      str12,"A",str9
                    setprop sheet.range(str12),*Value="Total Orders:"
                    pack      str12,"C",str9
                    setprop sheet.range(str12),*Value=TORDNUM
                    pack      str12,"E",str9
                    setprop sheet.range(str12),*Value="Total Usage:"
                    pack      str12,"G",str9
                    setprop sheet.range(str12),*Value=NUMPRT1B
          endif
.END PATCH 3.83 ADDED LOGIC
         goto       totexit
.
NEWTOT2  PRINT     *80,"------------":
                   *N,*1,"EXCHANGE ORDERS: ",TORDNUME:
                      *63,"EXCHANGE USAGE:",*78,NUMPRT3B:
                   *N,*1,"RENTAL   ORDERS: ",TORDNUMR:
                      *63,"RENTAL   USAGE:",*78,NUMPRT4B:
                   *N,*1,"RENT/EX  ORDERS: ",TORDNUMS:
                   *N,*1,"TOTAL    ORDERS: ",TORDNUM:
                      *63,"TOTAL    USAGE:":
                      *78,NUMPRT1B:
                   *N,*1,"TOTAL   SHIPPED:",TSHPNUM,*78,NUMPRT2B
.End patch #3.1 - remmed and replaced logic
totexit
         MOVE      C0 TO TORDQTY
         MOVE      C0 TO TORDQTYE
         MOVE      C0 TO TORDQTYR
         MOVE      C0 TO TORDNUM
         MOVE      C0 TO TORDNUMR
         MOVE      C0 TO TORDNUME
         MOVE      C0 TO TORDNUMS
         MOVE      C0 TO TSHPNUM
         MOVE      C0 TO TSHPQTY
         move      c0 to page
.         branch    faxflag to totexit1,totexit2
.totexit1 return
.begin patch 3.86
totexit2  
.begin patch 3.87
.         print     *1,copyrite,"1981-2007, Names in the News"
.         print     *1,copyrite,"1981-2008, Names in the News"
.begin patch 3.90
          print     *1,copyrite,"1981-2013, Names in the News"
.end patch 3.90
.end patch 3.87
.totexit2 print     *1,copyrite,"1981-2005, Names in the News"
.end patch 3.86
.totexit2 print     *1,copyrite,"1981-2001, Names in the News/CA"
         add       c1 to prtlines
         compare   c2 to flatflag
         if        equal
.START PATCH 3.83 ADDED LOGIC
                    RESET     COMMENT
                    if (COMMENT <> "TOTAL FLAT")
.END PATCH 3.83 ADDED LOGIC
                              write     flatfile,seq;*cdfon:
.                                       copyrite,"1981-2007, Names in the News"
.begin patch 3.87
.                                       copyrite,"1981-2008, Names in the News"
                                        copyrite,"1981-2013, Names in the News"
.end patch 3.87
.                         copyrite,"1981-2001, Names in the News/CA"
.START PATCH 3.83 ADDED LOGIC
                    endif
.END PATCH 3.83 ADDED LOGIC
         endif
         RETURN
.
EOJ      BRANCH    TOTFLAG TO EOJB
         CALL      LSTTOTLS
EOJB     compare   c2 to flatflag
         if        equal
.START PATCH 3.83 ADDED LOGIC
                    RESET     COMMENT
                    if (COMMENT = "TOTAL FLAT")
                              move      howmany,str9
                              call      Trim using str9
                              pack    range,"A5"
                              pack    range2,"A",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"B5"
                              pack    range2,"B",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"C5"
                              pack    range2,"C",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"D5"
                              pack    range2,"D",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"E5"
                              pack    range2,"E",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"F5"
                              pack    range2,"F",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"G5"
                              pack    range2,"G",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"H5"
                              pack    range2,"H",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"I5"
                              pack    range2,"I",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"J5"
                              pack    range2,"J",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"K5"
                              pack    range2,"K",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"L5"
                              pack    range2,"L",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"M5"
                              pack    range2,"M",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"N5"
                              pack    range2,"N",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"O5"
                              pack    range2,"O",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"P5"
                              pack    range2,"P",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"Q5"
                              pack    range2,"Q",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"R5"
                              pack    range2,"R",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"S5"
                              pack    range2,"S",str9
                              sheet.range(range,range2).Columns.Autofit
.begin patch 3.92
                              pack    range,"T5"
                              pack    range2,"T",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"U5"
                              pack    range2,"U",str9
                              sheet.range(range,range2).Columns.Autofit
.end patch 3.92
CampaignFileNameSelect
                              clear   taskname
                              if        (#ver = c1)
                              Move  "C:\WORK\ALPHMLRFLAT.xlsx",taskname
                              else
                              move  "C:\WORK\ALPHMLRFLAT.XLS",taskname
                              endif
                              erase     taskname
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
                              trap    TrapCampaignObject if Object
                              book.saveas giving N9 using *Filename=taskname
                              trapclr Object
.
                              setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
CampaignCleanUp
.Clean up after myself
                              destroy   OTRUE
.I was getting Excel.exe errors before I included following line.
.All automation objects need to be destroyed before you close down spreadsheet!
                              destroy sheet
                              destroy sheets
                              destroy book
                              destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
                              setprop ex,*DisplayAlerts=OFALSE
                              destroy   OFALSE
                              setprop ex,*SheetsInNewWorkbook=SheetsDefault
                              ex.quit
                              destroy ex
.Email new XLS to User
                              move    "Here is your Diskin File in Excel",MailSubjct
                              pack      MailBOdy,"Input File:  ",INPNAME
                              Pack      MailTO from User,"@nincal.com"
                              Pack      MailFrom from User,"@nincal.com"
                              if        (#ver = c1)
                              Pack      MailAttach from "c:\work\ALPHMLRFLAT.XLSX"
                              else
                              Pack      MailAttach from "c:\work\ALPHMLRFLAT.XLS"
                              endif
.begin patch 3.89
                              Move      c0,TrapCount                   .reset

CheckFile

                              trap      WaitForEnd giving error if IO
                              open      FileCheck,MailAttach,Exclusive          
                              Close     FIleCHeck

.end patch 3.89
                              
                              call      SendMail
                    else
.END PATCH 3.83 ADDED LOGIC
                              weof      flatfile,seq
                              close     flatfile
.START PATCH 3.83 ADDED LOGIC
                    endif
.END PATCH 3.83 ADDED LOGIC
         endif
         BRANCH    PRTFLAG TO END
         print     hpreset
         release
         SPLCLOSE
         CALL      REMVTOF
         shutdown  "cls"
         GOTO      END
.
HEADING  ADD       C1 TO PAGE
         COMPARE   C1 TO PAGE
         IF        EQUAL
         BRANCH    FAXFLAG TO NOFAX,FAX
NOFAX    BRANCH    DUPLFLAG TO SIMPLX,DUPLX
.SIMPLX   PRINT     HPcour,HP17PTCH,HPTOP
SIMPLX   PRINT     HPtmsr17,HPTOP
         GOTO      HD0
DUPLX    PRINT     hptmsr17,hpdupl,hptop,*F                .compressed
.DUPLX    PRINT     hp17ptch,hpdupl,hptop,*F                .compressed
         GOTO      HD0
FAX      print     diskport                     .20apr94 dlh
         ENDIF
HD0      BRANCH    SHPFLAG TO HD1,HD2,HD3
.                   *N,*LSTTAB,LISTNAME:
.START PATCH 3.82 REPLACED LOGIC
.HD1      PRINT     *F,*n,"CONFIDENTIAL":
.                      *52,"NAMES IN THE NEWS CA., INC.":
.                      *119,"DATE: ",TODAY:
.                   *N,*01,"(",OLNUM,")":
.                      *53,"REPORT OF USAGES OF LIST:":
.                      *119,"PAGE:    ",PAGE:
.                   *N:
.                   *N,*lsttab,LISTNAME;

HD1       
.begin patch 3.86
          if        (Company = c2)
          MOVE      "Pacific Lists, Inc.     " TO COMPNME
          else
          MOVE      "Names in the News       " TO COMPNME
          endif     

          PRINT     *F,*n,"CONFIDENTIAL":
                      *52,Compnme:
                      *119,"DATE: ",TODAY:
                   *N,*01,"(",OLNUM,")":
                      *53,"REPORT OF USAGES OF LIST:":
                      *119,"PAGE:    ",PAGE:
                   *N:
                   *N,*lsttab,LISTNAME;

.         PRINT     *F,*n,"CONFIDENTIAL":
.                      *52,"NAMES IN THE NEWS":
.                      *119,"DATE: ",TODAY:
.                   *N,*01,"(",OLNUM,")":
.                      *53,"REPORT OF USAGES OF LIST:":
.                      *119,"PAGE:    ",PAGE:
.                   *N:
.                   *N,*lsttab,LISTNAME;
.end patch 3.86

.END PATCH 3.82 REPLACED LOGIC
         compare    c1 to titlflag
         if         equal
         PRINT      *N,*lsttab,LEVELS
         else
         print     *N
         endif
.Start patch #3.1 - remmed and replaced logic
.         PRINT        *N,*06,"ORDER":
.                      *15,"CLIENT":
.                      *85,"ORDERED":
.                      *118,"MAIL":
.                      *127,"RETURN":
.                   *N,*07,"DATE":
.                      *15,"PO":
.                      *25,"LR##":
.                      *34,"CLIENT/OFFER/SELECTION":
.                      *76,"PRICE":
.                      *84,"QUANTITY":
.                      *99,"LIST KEY":
.                      *118,"DATE":
.                      *129,"DATE":
.                   *N,*03,"--------":
.                      *15,"-------":
.                      *25,"------":
.                      *34,"-----------------------------------":
.                      *73,"--------":
.                      *83,"---------":
.                      *99,"------------":
.                      *114,"--------":
.                      *125,"--------"
         PRINT        *N,*06,"ORDER":
                      *15,"CLIENT":
                      *85,"ORDERED":
                      *118,"MAIL":
                      *127,"RETURN":
                   *N,*07,"DATE":
                      *15,"PO":
                      *25,"LR##":
                      *34,"CLIENT/OFFER/SELECTION":
                      *76,"PRICE":
                      *84,"QUANTITY":
                      *96,"LIST KEY":
                      *118,"DATE":
                      *129,"DATE":
                   *N,*01,"----------":
                      *15,"-------":
                      *25,"------":
                      *34,"-----------------------------------":
                      *73,"--------":
                      *83,"---------":
                      *96,"------------":
                      *110,"----------":
                      *123,"----------"
.End patch #3.1 - remmed and replaced logic
         compare   c2 to flatflag
         if        equal
.START PATCH 3.83 ADDED LOGIC
                    RESET     COMMENT
                    if (COMMENT = "TOTAL FLAT")
                              setprop   sheet.Range("A3"),*Value="Mailers using List: "
                              setprop   sheet.Range("C3"),*Value=LISTNAME
                              setprop sheet.Range("C3").Font,*Bold="True",*Size=12
                    else
.END PATCH 3.83 ADDED LOGIC
                              compare   c1 to page
                              if        equal
                                        write     flatfile,seq;*cdfon:
                                                  "Confidential"
.START PATCH 3.82 REPLACED LOGIC
.         write     flatfile,seq;*cdfon:
.                   "Names in the News California., Inc."
.begin patch 3.86
          if        (Company = c2)
          MOVE      "Pacific Lists, Inc.     " TO COMPNME
          else
          MOVE      "Names in the News       " TO COMPNME
          endif     

                                        write     flatfile,seq;*cdfon:
                                                  Compnme
.                                                 "Names in the News"
.end patch 3.86
.END PATCH 3.82 REPLACED LOGIC
                                        write     flatfile,seq;*cdfon:
                                                  "Date: ",b1,b1,today
                                        write     flatfile,seq;*cdfon:
                                                  "REPORT OF USAGES OF LIST:",b1,b1,LISTNAME
                                        write     flatfile,seq;*cdfon:
                                                  "ORDER":
                                                  "CLIENT":
                                                  "ORDERED":
                                                  "MAIL":
                                                  "RETURN"
                                        write      flatfile,seq;*cdfon,"DATE":
                                                  "PO":
                                                  "LR##":
                                                  "CLIENT/OFFER/SELECTION":
                                                  "PRICE":
                                                  "QUANTITY":
                                                  "LIST KEY":
                                                  "DATE":
                                                  "DATE"
                              endif
.START PATCH 3.83 ADDED LOGIC
                    endif
.END PATCH 3.83 ADDED LOGIC
          endif
         MOVE      C8 TO PRTLINES
         RETURN
.
.Start patch #3.1 - remmed and replaced logic
.HD2      PRINT     *F,*n,"CONFIDENTIAL":
.                      *52,"NAMES IN THE NEWS CA., INC.":
.                      *119,"DATE: ",TODAY:
.                   *N,*01,"(",OLNUM,")":
.                      *53,"REPORT OF USAGES OF LIST:":
.                      *119,"PAGE:    ",PAGE:
.                   *N:
.                   *N,*lsttab,LISTNAME:
.                   *N:
.                   *N,*06,"ORDER":
.                      *15,"CLIENT":
.                      *85,"ORDERED":
.                      *100,"SHIPPED":
.                      *118,"MAIL":
.                      *127,"RETURN":
.                   *N,*07,"DATE":
.                      *15,"PO":
.                      *25,"LR##":
.                      *34,"CLIENT/OFFER/SELECTION":
.                      *76,"PRICE":
.                      *84,"QUANTITY":
.                      *99,"QUANTITY":
.                      *118,"DATE":
.                      *129,"DATE":
.                   *N,*03,"--------":
.                      *15,"-------":
.                      *25,"------":
.                      *34,"-----------------------------------":
.                      *73,"--------":
.                      *83,"---------":
.                      *99,"----------":
.                      *114,"--------":
.                      *125,"--------"
.START PATCH 3.82 REPLACED LOGIC
.HD2      PRINT     *F,*n,"CONFIDENTIAL":
.                      *52,"NAMES IN THE NEWS CA., INC.":
.                      *119,"DATE: ",TODAY:
.                   *N,*01,"(",OLNUM,")":
.                      *53,"REPORT OF USAGES OF LIST:":
.                      *119,"PAGE:    ",PAGE:
.                   *N:
.                   *N,*lsttab,LISTNAME:
.                   *N:
.                   *N,*06,"ORDER":
.                      *15,"CLIENT":
.                      *85,"ORDERED":
.                      *101,"SHIPPED":
.                      *118,"MAIL":
.                      *127,"RETURN":
.                   *N,*07,"DATE":
.                      *15,"PO":
.                      *25,"LR##":
.                      *34,"CLIENT/OFFER/SELECTION":
.                      *76,"PRICE":
.                      *84,"QUANTITY":
.                      *100,"QUANTITY":
.                      *118,"DATE":
.                      *129,"DATE":
.                   *N,*01,"----------":
.                      *15,"-------":
.                      *25,"------":
.                      *34,"-----------------------------------":
.                      *73,"--------":
.                      *83,"---------":
.                      *98,"----------":
.                      *110,"----------":
.                      *123,"----------"
HD2
.begin patch 3.86
          if        (Company = c2)
          MOVE      "Pacific Lists, Inc.     " TO COMPNME
          else
          MOVE      "Names in the News       " TO COMPNME
          endif     
.         PRINT     *F,*n,"CONFIDENTIAL":
.                      *52,"NAMES IN THE NEWS":
.end patch 3.86

          PRINT     *F,*n,"CONFIDENTIAL":
                      *52,Compnme:
                      *119,"DATE: ",TODAY:
                   *N,*01,"(",OLNUM,")":
                      *53,"REPORT OF USAGES OF LIST:":
                      *119,"PAGE:    ",PAGE:
                   *N:
                   *N,*lsttab,LISTNAME:
                   *N:
                   *N,*06,"ORDER":
                      *15,"CLIENT":
                      *85,"ORDERED":
                      *101,"SHIPPED":
                      *118,"MAIL":
                      *127,"RETURN":
                   *N,*07,"DATE":
                      *15,"PO":
                      *25,"LR##":
                      *34,"CLIENT/OFFER/SELECTION":
                      *76,"PRICE":
                      *84,"QUANTITY":
                      *100,"QUANTITY":
                      *118,"DATE":
                      *129,"DATE":
                   *N,*01,"----------":
                      *15,"-------":
                      *25,"------":
                      *34,"-----------------------------------":
                      *73,"--------":
                      *83,"---------":
                      *98,"----------":
                      *110,"----------":
                      *123,"----------"
.END PATCH 3.82 REPLACED LOGIC
.End patch #3.1 - remmed and replaced logic
         compare    c2 to flatflag
         if         equal
.START PATCH 3.83 ADDED LOGIC
                    RESET     COMMENT
                    if (COMMENT = "TOTAL FLAT")
                              setprop   sheet.Range("A3"),*Value=LISTNAME
                              setprop sheet.Range("A3").Font,*Bold="True",*Underline=xlUnderlineStyleSingle,*Size=12
                    else
.END PATCH 3.83 ADDED LOGIC
                              compare    c1 to page
                              if         equal
                                        write     flatfile,seq;*cdfon:
                                                  "Confidential"
.START PATCH 3.82 REPLACED LOGIC
.         write     flatfile,seq;*cdfon:
.                   "Names in the News California., Inc."
.begin patch 3.86
          if        (Company = c2)
          MOVE      "Pacific Lists, Inc.     " TO COMPNME
          else
          MOVE      "Names in the News       " TO COMPNME
          endif     
                                        write     flatfile,seq;*cdfon:
                                                  Compnme
.                                                 "Names in the News"
.end patch 3.86
.END PATCH 3.82 REPLACED LOGIC
                                        write     flatfile,seq;*cdfon:
                                                  "Date: ",b1,b1,today
                                        write     flatfile,seq;*cdfon:
                                                  "REPORT OF USAGES OF LIST:",b1,b1,LISTNAME
                                        write     flatfile,seq;*cdfon:
                                                  "ORDER":
                                                  "CLIENT":
                                                  "ORDERED":
                                                  "MAIL":
                                                  "RETURN"
                                        write      flatfile,seq;*cdfon,"DATE":
                                                  "PO":
                                                  "LR##":
                                                  "CLIENT/OFFER/SELECTION":
                                                  "PRICE":
                                                  "QUANTITY":
                                                  "LIST KEY":
                                                  "DATE":
                                                  "DATE"
         endif
.START PATCH 3.83 ADDED LOGIC
                    endif
.END PATCH 3.83 ADDED LOGIC
         endif
         RETURN
.Start patch #3.1 - remmed and replaced logic
.HD3      PRINT     *F,*n,"CONFIDENTIAL":
.                      *52,"NAMES IN THE NEWS CA., INC.":
.                      *119,"DATE: ",TODAY:
.                   *N,*01,"(",OLNUM,")":
.                      *53,"REPORT OF USAGES OF LIST:":
.                      *119,"PAGE:    ",PAGE:
.                   *N:
.                   *N,*lsttab,LISTNAME:
.                   *N:
.                   *N,*06,"ORDER":
.                      *15,"CLIENT":
.                      *85,"ORDERED":
.                      *118,"MAIL":
.                      *127,"RETURN":
.                   *N,*07,"DATE":
.                      *15,"PO":
.                      *25,"LR##":
.                      *34,"BROKER/OFFER/SELECTION":
.                      *76,"PRICE":
.                      *84,"QUANTITY":
.                      *99,"LIST KEY":
.                      *118,"DATE":
.                      *129,"DATE":
.                   *N,*03,"--------":
.                      *15,"-------":
.                      *25,"------":
.                      *34,"-----------------------------------":
.                      *73,"--------":
.                      *83,"---------":
.                      *99,"------------":
.                      *114,"--------":
.                      *125,"--------"
.START PATCH 3.82 REPLACED LOGIC
.HD3      PRINT     *F,*n,"CONFIDENTIAL":
.                      *52,"NAMES IN THE NEWS CA., INC.":
.                      *119,"DATE: ",TODAY:
.                   *N,*01,"(",OLNUM,")":
.                      *53,"REPORT OF USAGES OF LIST:":
.                      *119,"PAGE:    ",PAGE:
.                   *N:
.                   *N,*lsttab,LISTNAME:
.                   *N:
.                   *N,*06,"ORDER":
.                      *15,"CLIENT":
.                      *85,"ORDERED":
.                      *118,"MAIL":
.                      *127,"RETURN":
.                   *N,*07,"DATE":
.                      *15,"PO":
.                      *25,"LR##":
.                      *34,"BROKER/OFFER/SELECTION":
.                      *76,"PRICE":
.                      *84,"QUANTITY":
.                      *96,"LIST KEY":
.                      *118,"DATE":
.                      *129,"DATE":
.                   *N,*01,"----------":
.                      *15,"-------":
.                      *25,"------":
.                      *34,"-----------------------------------":
.                      *73,"--------":
.                      *83,"---------":
.                      *96,"------------":
.                      *110,"----------":
.                      *123,"----------"
HD3
.begin patch 3.86
          if        (Company = c2)
          MOVE      "Pacific Lists, Inc.     " TO COMPNME
          else
          MOVE      "Names in the News       " TO COMPNME
          endif     
.end patch 3.86

          PRINT     *F,*n,"CONFIDENTIAL":
                      *52,Compnme:
                      *119,"DATE: ",TODAY:
                   *N,*01,"(",OLNUM,")":
                      *53,"REPORT OF USAGES OF LIST:":
                      *119,"PAGE:    ",PAGE:
                   *N:
                   *N,*lsttab,LISTNAME:
                   *N:
                   *N,*06,"ORDER":
                      *15,"CLIENT":
                      *85,"ORDERED":
                      *118,"MAIL":
                      *127,"RETURN":
                   *N,*07,"DATE":
                      *15,"PO":
                      *25,"LR##":
                      *34,"BROKER/OFFER/SELECTION":
                      *76,"PRICE":
                      *84,"QUANTITY":
                      *96,"LIST KEY":
                      *118,"DATE":
                      *129,"DATE":
                   *N,*01,"----------":
                      *15,"-------":
                      *25,"------":
                      *34,"-----------------------------------":
                      *73,"--------":
                      *83,"---------":
                      *96,"------------":
                      *110,"----------":
                      *123,"----------"
.END PATCH 3.82 REPLACED LOGIC
.End patch #3.1 - remmed and replaced logic
         MOVE      C8 TO PRTLINES
         compare   c2 to flatflag
         if        equal
.START PATCH 3.83 ADDED LOGIC
                    RESET     COMMENT
                    if (COMMENT = "TOTAL FLAT")
                              setprop   sheet.Range("A3"),*Value=LISTNAME
                              setprop sheet.Range("A3").Font,*Bold="True",*Underline=xlUnderlineStyleSingle,*Size=12
                    else
.END PATCH 3.83 ADDED LOGIC
                              compare   c1 to page
                              if        equal
                                        write     flatfile,seq;*cdfon:
                                                     "Confidential"
.START PATCH 3.82 REPLACED LOGIC
.         write     flatfile,seq;*cdfon:
.                   "Names in the News California., Inc."
.begin patch 3.86
          if        (Company = c2)
          MOVE      "Pacific Lists Inc.     " TO COMPNME
          else
          MOVE      "Names in the News       " TO COMPNME
          endif     

                                        write     flatfile,seq;*cdfon:
                                                  Compnme
.                                                 "Names in the News"
.end patch 3.86
.END PATCH 3.82 REPLACED LOGIC
                                        write     flatfile,seq;*cdfon:
                                                  "Date: ",b1,b1,today
                                        write     flatfile,seq;*cdfon:
                                                  "REPORT OF USAGES OF LIST:",b1,b1,LISTNAME
                                        write     flatfile,seq;*cdfon:
                                                  "ORDER":
                                                  "CLIENT":
                                                  "ORDERED":
                                                  "MAIL":
                                                  "RETURN"
                                        write      flatfile,seq;*cdfon,"DATE":
                                                  "PO":
                                                  "LR##":
                                                  "CLIENT/OFFER/SELECTION":
                                                  "PRICE":
                                                  "QUANTITY":
                                                  "LIST KEY":
                                                  "DATE":
                                                  "DATE"
                             endif
.START PATCH 3.83 ADDED LOGIC
                    endif
.END PATCH 3.83 ADDED LOGIC
         endif
         RETURN
.
calcnet  compare   c0 to nordout
         if        equal
         move      c0 to percent
         else
         move      c0 to calcper
         move       nordout to calcper
         DIVIDE    nordin INTO CALCPER
         MULT      "100" BY CALCPER
         MOVE      C0 TO PERCENT
         ADD       CALCPER TO PERCENT
         endif
         return

prtbanr  Print      *l,*rptchar "*":80:
                    *l,*N:
                    "User : ",user,*l,*n:
                    "Date: ",today,*l,*n:
                    "input file : ",inpname,*l,*n:
                    "copies : ":
                    *n,*rptchar "*":80:
                    *f
         compare    c2 to DUPLFLAG
         if         equal
         print      *f
         endif
         return

.
MLRNG    KEYIN     *P01:24,*EL,*B,"Invalid mailer number: ",*DV,OMLRNUM,*r:
                                  " in LR: ",*DV,OLRN,*CL,STR1
         return
         GOTO      END
.START PATCH 3.83 ADDED LOGIC
WriteExcelRecord
          add       C1,howmany
          move      howmany,str9
          call      Trim using str9
          pack      str12,"A",str9
          setprop   sheet.Range(str12),*Value=DATEPRT1
          pack      str12,"B",str9
          call      Trim using OMLRPON
          setprop   sheet.Range(str12),*Value=OMLRPON
          pack      str12,"C",str9
          setprop   sheet.Range(str12),*Value=OLRN
          pack      str12,"D",str9
          call      Trim using CLIENT
          setprop   sheet.Range(str12),*Value=CLIENT
          pack      str12,"E",str9
          setprop   sheet.Range(str12),*Value=TAXPRT
          pack      str12,"F",str9
          setprop   sheet.Range(str12),*Value=PPMPRT
          pack      str12,"G",str9
          setprop   sheet.Range(str12),*Value=NUMPRT1
          pack      str12,"H",str9
          setprop   sheet.Range(str12),*Value=ENTTEXT
          pack      str12,"I",str9
          setprop   sheet.Range(str12),*Value=NUMPRT2
          pack      str12,"J",str9
          setprop   sheet.Range(str12),*Value=DATEPRT2
          pack      str12,"K",str9
          setprop   sheet.Range(str12),*Value=DATEPRT3
          pack      str12,"L",str9
          setprop   sheet.Range(str12),*Value=NUMPRTA
          pack      str12,"M",str9
          call      Trim using OFDESC
          setprop   sheet.Range(str12),*Value=OFDESC
          pack      str12,"N",str9
          setprop   sheet.Range(str12),*Value=CANTEXT
          pack      str12,"O",str9
          setprop   sheet.Range(str12),*Value=COMTEXT
          pack      str12,"P",str9
          setprop   sheet.Range(str12),*Value=EXCHTEXT
          pack      str12,"Q",str9
          setprop   sheet.Range(str12),*Value=NUMPRT3
          pack      str12,"R",str9
          setprop   sheet.Range(str12),*Value=TAPETEXT
          pack      str12,"S",str9
          setprop   sheet.Range(str12),*Value=NSEL2NAME
.begin patch
          pack      str12,"T",str9
          setprop   sheet.Range(str12),*Value=PrtNet
.end patch
.begin patch    DEB
          pack      str12,"U",str9
          setprop   sheet.Range(str12),*Value=o1des

          pack      str12,"V",str9
          setprop   sheet.Range(str12),*Value=OrdMname
.begin patch 3.92
          pack      str12,"W",str9
          setprop   sheet.Range(str12),*Value=Dateprt4

.end patch 3.92
          return
.begin patch 3.89
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
.                   if        (trapcount > 60)   . 5 min are you kidding me
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Nord0005 - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
                    Move      "dherric@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
                    Move      "dherric@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    mailattach,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return
                    endif
          
                    goto      checkfile

.end patch 3.89

.
TrapCampaignObject
.This routine tripped when Saveas method is called.
.
.We are trapping for instances where the User has selected a filename that: 1) Already exists
.and is open by another instance of Excel. 2) Already exists but not open elsewhere.  This instance
.will provoke Excel to produce a message asking User if they want to overwrite the file.  If they
.answer No or Cancel they will come to this routine.  Answering Yes will overwrite the file at the
.Saveas method found in above code.
        noreturn
        move    taskname,str50
        getinfo exception,taskname
        unpack  taskname,str55,str55,str10,str55
        scan    "Cannot access",str55
        if equal
.Instance 1 - exists and open elsewhere
                pack    taskname,str50," already exists and is open!!",newline,"Select another Filename!!"
                alert   caution,taskname,result
.                goto CampaignFileNameSelect
        endif
.Send them back to select another File name and try to Save again.
        goto CampaignFileNameSelect
.END PATCH 3.83 ADDED LOGIC
+.........................................................................
         INCLUDE   NORD2IO.inc
+
.patch3.8
                                        include   compio.inc
                                        include   cntio.inc
.         INCLUDE   NMLRIO.inc
.+
.patch3.8
         INCLUDE   NSHPIO.inc
+
         INCLUDE   NMTXIO.inc
+
.START PATCH 3.2 - ADDED LOGIC
         INCLUDE   NOFRIO.INC
.END PATCH 3.2 - ADDED LOGIC
.Begin patch 3.92
          Include   Ninvio.inc
.end patch 3.92
.START PATCH 3.6 ADDED LOGIC
          INCLUDE   NSEL2IO.INC
          include   nmodio.inc
.END PATCH 3.6 ADDED LOGIC
         INCLUDE   COMLOGIC.inc
