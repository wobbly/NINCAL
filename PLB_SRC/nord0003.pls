............................................................................
.
. PROGRAM    : NORD0003
. DESCRIPTION: PRODUCES NIN MAILER BOOK.
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
         INCLUDE    HP.inc
+
.patch3.7
           include        compdd.inc
           include        cntdd.inc
.         INCLUDE    NMLRDD.inc
.patch3.7
+
         INCLUDE   NXRFDD.inc
.
         INCLUDE   NMTXDD.inc
.
         INCLUDE    NSHPDD.inc
.;begin patch 3,8
.;         INCLUDE    NINVDD.INC
         INCLUDE              ninvdd.inc
.;end patch 3.8
          include   Ndatdd.inc
.START PATCH 3.3 - ADDED LOGIC
         INCLUDE   NOFRDD.INC
.END PATCH 3.3 - ADDED LOGIC
.START PATCH 3.6 ADDED LOGIC
              INCLUDE         NSEL2DD.INC
              include         nmoddd.inc
.END PATCH 3.6 ADDED LOGIC
.START PATCH 3.9 ADDED LOGIC
              INCLUDE         NOWNDD.INC
              INCLUDE         NORD5DD.INC
.END PATCH 3.9 ADDED LOGIC
.begin patch 4.0
          include   NCntdd.inc
          INCLUDE   LOGDATA.inc
          include   Nusedd.inc
          include   prtpagedd.inc
.end patch 4.0

Release    INIT    "4.12"               DLH      .turn off list hyperlink
Reldate   Init      "2016 April 14"
.Release    INIT    "4.11"               DLH      .add Mailer List code to excel version
.Reldate   Init      "2016 March 16"
.Release    INIT    "4.10"               DLH      .add reuse info to excel version, autfit that column and rows
.Reldate   Init      "2016 March 11"
.Release    INIT    "4.05"               DLH      .IF Pending orders selected include pending cancelled
.Reldate   Init      "2014 September 30"
.Release    INIT    "4.04"               DLH      .Excel add test/retest
.Reldate   Init      "2014 September 16"
.Release    INIT    "4.03"               DLH      .Excel 2013 *WindowState=xlMinimized
.Reldate   Init      "2014 January 22"
.release   init      "4.02"         DLH  convert to sunbelt PDF           .turned off
.RelDate   Init      "04 April 2013"
.release   init      "4.01"         DLH  convert to PRTPAGE
.RelDate   Init      "05 MARCH 2013"
.release   init      "4.00"         DLH  convert to PRTPAGE
.RelDate   Init      "16 January 2013"
.release   init      "3.98"         DLH  Hyperlinks for lists
.RelDate   Init      "13 May 2009"
.release   init      "3.97"         DLH  CHeck for excel version
.RelDate   Init      "20 April 2009"
.release   init      "3.96"         JD06Jan2009     Copyrite footer update
.RelDate   Init      "06 Jan 2009"
.release  init      "3.95"         DLH     Sendmail wait for attachment
.RelDate        Init           "07 August 2008"
.release            init      "3.94"         DLH SEndmail
.RelDate     Init           "24 April 2008"
.release  init      "3.93"         JD04Jan2008     Copyrite footer update
.RelDate        Init           "01/04/2008"
.release  init      "3.92"         DLH Trouble shooting flat file
.RelDate        Init           "10/15/2007"
.release  init      "3.91"         DLH 22may2007 Pacific LIsts  uses company from common.inc
.RelDate        Init           "05/22/2007"
.release        init      "3.9"        ASH    21DEC2005      Modification of diskin XLS
.release        init      "3.8"        DLH   09MAR2005      Invoice COnversion
.release  init      "3.72"        ASH        03MAR2005      Added Excel option
.Note:        At this time only the version labeled 'NMLRALF' in BUTIL is using the Excel logic.
.             If we decide to go live with more programs, we can add an 'Excel' checkbox
.             in NORD0006.PLS and then use a variable to let BUTIL know that the report
.             should be dumped into Excel.  Modifying PRTNAME might be a good way to do this.
.             I am currently depending on COMMENT="TOTAL BANNER FLAT" to trigger the Excel logic.  This
.             is only called by 'NMLRALF'.
.
.release  init      "3.71"        DLH        29July2004     Copyrite footer
.release  init      "3.7"        DMB         26MAY2004      Mailer Conversion
.RELEASE  INIT      "3.6"       ASH  28JAN2004  DATACARD CONVERSION
.RELEASE  INIT      "3.5"       DMB 06may2002 Newcode added for new options on order pick using inits
.release  init      "3.42"     02OCT2000 ASH NEW SERVER ADDED
.release  init      "3.41"     11Sep00 DLH add XTRA - extra lines option.
.release  init      "3.4"      10JUN99 NINSHP Y2K, File expansion
.RELEASE  INIT      "3.3"      06MAY99 ASH Replaced OODES{NINORD.DAT} --> OFDESC{NINOFR.DAT}
.RELEASE  INIT      "3.2"      08APR99 JD download summary diskin.
.RELEASE  INIT      "3.1"      19Mar99 JD skip orders 0 qty's
.RELEASE  INIT      "3.0"     11JAN99 ASH NINORD Y2K, File expansion
.release  init      "2.9"     06Jan99 Banner pages
.                            NOTE:  There may be some formatting glitches,
.                            especially if full value of Quantity fields is used
.Release  init      "2.8"     28Sep98 DLH added code to handle pending orders
.                            See norddd.inc patch 5
.                            Currently it just suppresses them will need option to
.                            select in the future
.Release   init      "2.7"        18Aug98 DLH add flat file option
.RELEASE  INIT      "2.6"        15MAR95 DLH PREPAID SUMMARY
.RELEASE  INIT      "2.5"        17aug94 restart page numbers at mailer break.
.RELEASE  INIT      "2.4"       20APR94 DLH DUPLEX OPTION.
.RELEASE  INIT      "2.3"       03FEB94 JD SKIP DUMMY ORDERS WITH FAX OPT.
.RELEASE  INIT      "2.2"      07JUL93 DLH ADDED FAX PRINT OPTION.
.
.RELEASE   INIT      "2.1"     JD 06OCT92  ADDED TAX STATUS PRINT
.
.RELEASE          INIT      "2.0"     DLH 23APR92   CONVERTED TO USE DSINIT
.
.RELEASE   INIT      "1.9"    DLH 08APR92   ADDED LIFESTYLE & IC SYSTEMS
.                             OVERLAY'S.
.
.RELEASE   INIT     "1.8"    DLH 10MAR92  ADD GUARANTY OPTION.
.
.RELEASE  INIT      "1.7"    D.L.HERRICK 06AUG91. MAKE SHIPPING OPTION USABLE
.                           WITH NINCAL LAYOUT.
.
.RELEASE  INIT      "1.6"    D.L.HERRICK 07/24/91
.                           CONVERT TO NINCA. VARIABLES
.                           CONVERT TO RUN UNDER RMS OR PC-DATABUS.
.                           REMOVED '1513' FROM RETURN-TO LIST.
.                           DEFAULT SUPPRESS RUNNING CHARGE ORDERS WITH OPTION
.                           TO PRINT THEM.
.                           FIXED BUG AT PAGE + MAILER BREAK RESULTING IN OFFER
.                           DESCRIPTION PRINTING TWICE.
.RELEASE  INIT      "1.5"    E.W.LAKE   02/28/90
.                           FIX BUG. MAILER NAME PRINTING WHEN BROKER WAS
.                           NOT INVOLVED.
.                           CHECK FOR BOTTOM OF PAGE WHEN OFFER CHANGES SO
.                           THAT THIS REPORT CAN RUN ON THE LASER.
.
.RELEASE INIT      "1.4"    W.L. LOO   01/11/90
.                           CORRECT MAILER NAME IF USE ALPHAMN
.
.RELEASE  INIT     "1.3"    E.W. LAKE  04/18/89
.                           ADD RETURN-TO# '1513' TO THE LIST.
.
.RELEASE INIT      "1.2"    E.W. LAKE  07/14/88
.                           PCBUS CONVERSION
.
.RELEASE INIT      "1.1"    E.W. LAKE   07/12/88
.                           IF RETURN-TO IS '0000' OR '0001' USE THE ORDERED
.                           QTY AS THE SHIPPED QTY.
.
.RELEASE INIT      "1.0"    E.W. LAKE   03/23/88
.                           INITIAL RELEASE
.
............................................................................
.
.begin 3.97 to find version of excel  DH 04/02/09
#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
.end 3.97 to find version of excel

PRTFLAG  FORM      1
LOCAL    INIT      "LOCAL"
PAGE     FORM      5
PRTLINES FORM      2
TAXPRT   INIT      "      "
DUPLFLAG FORM      1        DUPLEX FLAG 1= NO 2 = YES.
PPFLAG   FORM      1         PRepay FLAG, 1=NO 2=yes
FAXFLAG  FORM      1         PRINT FLAG, 1=laser PRINTER 2=FAX
SRTFLAG  FORM      1         SORT FLAG, 1=MAILER#, 2=MAILER NAME
SHPFLAG  FORM      1         SHIP FLAG, 1=NO SHIP INFO, 2=SHIP INFO
TOTFLAG  FORM      1         TOTL FLAG, 1=NO TOTALS, 2=TOTALS
CFLAG    FORM      1         501Cx FLAG, 1=NO INFO, 2=PRINT INFO
RUNFLAG  FORM      1         RUNNING CHARGE FLAG, 1=NONE, 2=INCLUDE
BRKFLAG  FORM      1         BROKER FLAG, 1=NO BROKER INVOLVED, 2=BROKER INVLVE
SUMFLAG  FORM      1         SUMMARY FLAG, 2=NO DETAIL, 1=INCLUDE
GUARFLAG FORM      1         GUARANTY FLAG, 1=NONE, 2=INCLUDE
banrflag form      1      1=no banner page, 2 = banner page
Flatfile file
flatflag form      1         Create Excel file for output 2=yes
.begin patch 3.41
XTRAFlag form      1
.end patch 3.41
HOLDMLR  INIT      "    "
HOLDBRK  INIT      "    "
HOLDCNT  INIT      "   "
HOLDOFR  INIT      "   "
HOLDNAME INIT      "                        "
HOLDbNME INIT      "                        "
BRKTAB   FORM      3
MLRTAB   FORM      3
OFRTAB   FORM      3
ORDOFR   DIM       3         HOLDS ORDER OFFER #.
NUMMASKA INIT      "ZZZZZZ"
.Start patch #3.0 - increased vars
.DATEMASK INIT      "XX/XX/XX"
.DATEPRT1 DIM       8
.DATEPRT2 DIM       8
.DATEPRT3 DIM       8
.NUMMASK  INIT      "Z,ZZZ,ZZZ"
.NUMPRT1  DIM       9
.NUMPRT2  DIM       9
.NUMPRT3  DIM       9
.TORDQTY  FORM      9
.TORDQTYE FORM      9         TOTAL EXHANGE
.TORDQTYR FORM      9         TOTAL RENTAL
.begin patch 3.95
FileCheck FIle
trapcount form      4
.end patch 3.95
.Begin patch 4.0
PDFFLAG   Form      1
PrintFlag Form      1
ANS      DIM       1
PORTX    FORM      3
DIM3     FORM      3
SFILE    sndfile
Sapi          Automation                               //Define the ActiveX control
NAME     DIM       25
PORTNUM  DIM       3
SoundFLag form   1
font8    font
        create  font8,"Times New Roman",size=8
font8i    font
        create  font8i,"Times New Roman",size=8,italic        
.needed for printing to PDF for non native fonts and things like Copyright to appear correctly in the pdf
PDF_FLAGS_WIN_ANSI_ENCODING EQU 128
.end patch 4.0
.
.NEW VAR USED FOR EDITING AS N10 WILL NOT WORK AND N9 ALREADY USED
NUM9     FORM      9
DATEMASK INIT      "XX/XX/XXXX"
DATEPRT1 DIM       10
DATEPRT2 DIM       10
DATEPRT3 DIM       10
NUMMASK  INIT      "ZZZ,ZZZ,ZZZ"
NUMPRT1  DIM       11
NUMPRT2  DIM       11
NUMPRT3  DIM       11
TORDQTY  FORM      11
TORDQTYE FORM      11         TOTAL EXHANGE
TORDQTYR FORM      11         TOTAL RENTAL
.End patch #3.0 - increased vars
TORDNUM  FORM      6         TOTAL ORDERS PRINTED
TORDNUME FORM      6         TOTAL EXCHANGE ORDERS PRINTED.
TORDNUMR FORM      6         TOTAL RENTAL ORDERS PRINTED,
TORDNUMS FORM      6         TOTAL SPLIT ORDERS  PRINTED.
TSHPNUM  FORM      6         TOTAL NUMBER OF ORDERS SHIPPED
.Start patch #3.0 - increased vars
.TSHPQTY  FORM      9         TOTAL NAMES SHIPPED
.BORDQTY  FORM      9
.BORDQTYE FORM      9         TOTAL EXHANGE
.BORDQTYR FORM      9         TOTAL RENTAL
TSHPQTY  FORM      11         TOTAL NAMES SHIPPED
BORDQTY  FORM      11
BORDQTYE FORM      11         TOTAL EXHANGE
BORDQTYR FORM      11         TOTAL RENTAL
.END patch #3.0 - increased vars
BORDNUM  FORM      6         TOTAL ORDERS PRINTED
BORDNUME FORM      6         TOTAL EXCHANGE ORDERS PRINTED.
BORDNUMR FORM      6         TOTAL RENTAL ORDERS PRINTED,
BORDNUMS FORM      6         TOTAL SPLIT ORDERS  PRINTED.
.Start patch #3.0 - increased vars
.BSHPQTY  FORM      9         TOTAL NAMES SHIPPED
BSHPQTY  FORM      11         TOTAL NAMES SHIPPED
.END patch #3.0 - increased vars
BSHPNUM  FORM      6         TOTAL NUMBER OF ORDERS SHIPPED
GORDQTY  FORM      9
GSHPQTY  FORM      9
NUMPRTA  DIM       6
.Start patch #3.0 - increased vars
.NUMMASKB INIT      "ZZZ,ZZZ,ZZZ"
.NUMPRT1B DIM       11
.NUMPRT2B DIM       11
.NUMPRT3B DIM       11
.NUMPRT4B DIM       11
NUMMASKB INIT      "ZZ,ZZZ,ZZZ,ZZZ"
NUMPRT1B DIM       14
NUMPRT2B DIM       14
NUMPRT3B DIM       14
NUMPRT4B DIM       14
.END patch #3.0 - increased vars
PPMMASK  INIT      "$$$$.ZZ"
PPMPRT   DIM       7
.begin patch 4.0
PPMPRT1   DIM       9
.end patch 4.0
CANTEXT  DIM       11
ENTTEXT  DIM       3
TAPETEXT DIM       14
EXCHTEXT DIM       6              .was 10 01sep95 DLH
prtnet   dim       3              .dlh 01sep95
COMTEXT  DIM       11
NINPPATH FORM      1                 '0' FILE CLOSED '1'=OPEN
B11      INIT      "           "
REURTNS  INIT      "0000-0001"
C55      FORM      "55"
C57      FORM      "57"
N52      FORM      5.2
N42      FORM      4.2
GUARTEXT DIM       12
DAY30    INIT      "30 DAY"
DAY45    INIT      "45 DAY"
DAY60    INIT      "60 DAY"
DAY0     INIT      "GUARANTEED"
PRE      INIT      "PREPAY"
PRE30    INIT      "PREPAY 30"
PRE45    INIT      "PREPAY 45"
PRE60    INIT      "PREPAY 60"
.START PATCH 3.6 ADDED LOGIC
O2DES2        DIM             40                            .Second half of Select Name, if necessary
.END PATCH 3.6 ADDED LOGIC
.START PATCH 3.72 ADDED LOGIC
books   automation
book    automation
sheets  automation
Range1    automation
sheet   automation
ex      automation      class="Excel.Application"
.Variant objects used to talk to outside applications
xlRowHeight   variant
VT_R8         EQU 5           .Double - 8 byte Real
.Booleans
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
VT_I4   EQU 3           .4 byte integer
.START PATCH 3.9 REPLACED LOGIC
.Zoom85  variant
Zoom80  variant
.END PATCH 3.9 REPLACED LOGIC
.Formatting vars needed
SheetsDefault integer 4,"0x00000000"
xlMinimized integer 4,"0xFFFFEFD4"
xlMaximized integer 4,"0xFFFFEFD7"
xlAlignCenter integer 4,"0xffffeff4"
xlUnderlineStyleSingle integer 4,"0x2"
MedThick integer 4,"0xFFFFEFD6"
.START PATCH 3.9 ADDED LOGIC
AllMargin     variant
xlColWidth    variant
xlLandscape integer 4,"0x2"                     .2
xlAlignRight integer 4,"0xFFFFEFC8"
xlAlignLeft integer 4,"0xFFFFEFDD"
xlPaperLegal integer 4,"0x5"
SReturn       init            0x0a                                                        .soft return/line feed
LOText        dim             100
.END PATCH 3.9 ADDED LOGIC
.
range         dim             20
range2        dim             20
.END PATCH 3.72 ADDED LOGIC
..............................................................................
.
DATE     DIM       8
.

+.........................................................................
.
. DEFAULT OPTIONS ARE : BREAK BY MAILER NUMBER
.                       NO SHIPPING QTY
.                       TOTALS
.                       NO RUNNING CHARGE ORDERS
.                       PRINT DETAILS
.                       NO GUARANTY INFO
.                       NO CLIENT TAX INFO
.                       Laser PRINTER FORMAT
.                       Prepaid summary
.
. COMMENT CAN MODIFY THE DEFAULTS:  ALPHA  :  BREAK BY MAILER NAME
.                                   SHIP   :  INCLUDE SHIPPING QTY
.                                   TOTAL  :  INCLUDE TOTALS
.                                   RUN    :  INCLUDE RUNNING CHARGE ORDERS
.                                   SUM    :  SUMMARY ONLY SUPPRESS DETAILS
.                                   GUAR   :  INCLUDE GUARANTY DETAIL
.                                   501C   :  INCLUDE TAX INFO
.                                   FAX    :  FAX CONTROL
.begin patch 3.41
.                                   XTRA   :  Extra blank lines
.end patch 3.41
.                   *p20:14,"Banner  : PRINT Banner Page":
.                                   Prepay :   Prepaid Summary
.                                   Flat   :   create flat file
.
. INPNAME WILL CONTAIN THE INPUT FILENAME: ??????/TXT OR ??????
. PRTNAME WILL CONTAIN THE PRINT FILE NAME or 'LOCAL'
.
. IF ANY OF THE ABOVE INFO IS MISSING OR INVALID, REQUEST IT.
.........................................................................
.
.TESTING LOGIC
.             move            "NORD0003",PROGRAM
.             move            "1",COMPANY
.             move            "aharkin",USER
.             move            "TOTAL BANNER FLAT",COMMENT
.             move            "diskin09.srt",INPNAME
.             move            "diskin09.lst",PRTNAME
.             move            "AH",INITS
.testing 2016 January 11 DLH
           if         (user = "MBATCH3")
           move       "CREQUES",user
           endif
.testing 2016 January 11 DLH

         MOVE      "Mailer Book" TO STITLE
         CMATCH    B1 TO PROGRAM       .CHAINED FROM DSINIT WITH INFO?
         IF        EOS                 .NO
         MOVE      "NORD0003" TO PROGRAM
         MOVE      "DISKIN" TO INPNAME
         MOVE      "LOCAL" TO PRTNAME
         MOVE      "TOTAL" TO COMMENT
         MOVE      "NINCAL" TO COMPNME
         ENDIF
         CALL      PAINT
         MOVE      C0 TO NINPPATH
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
.
.
         GOTO      OPTION
OPTGET   MOVE      C0 TO SRTFLAG
         MOVE      C0 TO SHPFLAG
         MOVE      C0 TO TOTFLAG
         MOVE      C0 TO SUMFLAG
         MOVE      C0 TO RUNFLAG
         MOVE      C0 TO GUARFLAG
         MOVE      C1 TO CFLAG
         MOVE      C0 TO FAXFLAG
         MOVE      C0 TO DUPLFLAG
         MOVE      C0 TO ppFLAG
         move      c0 to flatflag
         move      c0 to banrflag
         RESET     COMMENT
         KEYIN     *P20:10,"ALPHA  :  BREAK BY MAILER NAME":
                   *P20:11,"SHIP   :  INCLUDE SHIPPING QTY":
                   *P20:12,"TOTAL  :  INCLUDE TOTALS":
                   *P20:13,"RUN    :  INCLUDE RUNNING CHARGE ORDERS":
                   *P20:14,"SUM    :  SUMMARY ONLY SUPPRESS DETAILS":
                   *P20:15,"GUAR   :  INCLUDE ORDER GUARANTY DETAIL":
                   *P20:16,"501C   :  INCLUDE CLIENT TAX STATUS":
                   *P20:17,"FAX    :  FORMAT FOR FAX         ":
                   *P20:18,"BRK    :  Break on Broker        ":
                   *P20:19,"DUPLEX :  DUPLEX        ":
                   *P20:20,"PREPAY :  Pre paid summary        ":
                   *P20:21,"Flat   :  Flat file diskinxx.csv  ":
                   *p20:22,"Banner : PRINT Banner Page":
.begin patch 3.41
                   *p20:23,"XTRA   : PRINT Extra blank lines":
.end patch 3.41
                   *P01:24,*EL,"The Option setting is invalid.":
                   *P15:05,COMMENT:
                   *P20:10,*EL,*P20:11,*EL,*P20:12,*EL,*P20:13,*EL:
                   *P20:14,*EL,*P20:15,*EL,*P20:16,*EL,*P20:17,*EL;
         MATCH     "                         " TO COMMENT
         GOTO      OPTDEFLT IF EQUAL
OPTION   MOVE      C0 TO SRTFLAG
         MOVE      C0 TO SHPFLAG
         MOVE      C0 TO TOTFLAG
         MOVE      C0 TO SUMFLAG
         MOVE      C0 TO RUNFLAG
.begin patch 3.41
         move      c0 to XTRAFlag
.end patch 3.41
         MOVE      C1 TO BRKFLAG
         MOVE      C0 TO GUARFLAG
         MOVE      C1 TO CFLAG
         MOVE      C0 TO FAXFLAG
         MOVE      C0 TO DUPLFLAG
         MOVE      C0 TO PPFLAG
         move      c0 to flatflag
         move      c0 to banrflag
.begin patch 3.92
..START PATCH 3.72 ADDED LOGIC
..              if (COMMENT = "TOTAL BANNER FLAT" | COMMENT = "TOTAL FLAT")
.              if (FlatFlag = c2)
..end patch 3.92
..Force Excel Option!!!
..Create the Variant objects
..Booleans
.               create  OTRUE,VarType=VT_BOOL,VarValue=1
.               create  OFALSE,VarType=VT_BOOL,VarValue=0
..START PATCH 3.9 REPLACED LOGIC
..              create  Zoom85,VarType=VT_I4,VarValue=1
.               create  Zoom80,VarType=VT_I4,VarValue=80
.."1" increment in Excel interface equals "1.3888" in OLE logic
.               create         AllMargin,VarType=VT_R8,VarValue="18"                       .Roughly equals .25 inches:  18 * 1.388 = 25
.               create         xlColWidth,VarType=VT_R8,VarValue="0.0"                     .Default
..END PATCH 3.9 REPLACED LOGIC
..
.               create         xlRowHeight,VarType=VT_R8,VarValue="75.0"
..Open Excel application
.               create  ex
.              getprop ex,*SheetsInNewWorkbook=SheetsDefault
.                      setprop ex,*SheetsInNewWorkbook=C1
.               setprop ex,*WindowState=xlMinimized
..Create Workbooks collection
.               getprop ex,*Workbooks=books
..Create/Add a single Workbook
.               books.add
.               books.item giving book using 1
..Create Worksheets collection
.               getprop book,*Sheets=sheets
..Create a single Worksheet - we did not need to add it as we set the default above to
..add one new Worksheet each time a Workbook is created.
.               sheets.item giving sheet using 1
..START PATCH 3.9 ADDED LOGIC
.               setprop sheet.PageSetup,*Orientation=xlLandscape
.               setprop sheet.PageSetup,*Zoom=Zoom80
.               setprop sheet.PageSetup,*TopMargin=AllMargin
.               setprop sheet.PageSetup,*BottomMargin=AllMargin
.               setprop sheet.PageSetup,*RightMargin=AllMargin
.               setprop sheet.PageSetup,*LeftMargin=AllMargin
.               //Using xlColWidth for dual purposes!!
.               setprop sheet.PageSetup,*HeaderMargin=xlColWidth
.               setprop sheet.PageSetup,*FooterMargin=xlColWidth
.               pack    str11,"1:8"
.               setprop sheet.PageSetup,*PrintTitleRows=str11
.               setprop sheet.PageSetup,*PaperSize=xlPaperLegal
..END PATCH 3.9 ADDED LOGIC
.               setprop        sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
.               sheet.range("A1:E1").Merge
.               sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
..Header information
..START PATCH 3.9 REPLACED LOGIC
..              setprop        sheet.Range("A5"),*Value="LR Number"
..              setprop        sheet.Range("B5"),*Value="List Name"
..              setprop        sheet.Range("C5"),*Value="Select"
..              setprop        sheet.Range("D5"),*Value="Offer"
..              setprop        sheet.Range("E5"),*Value="Quantity"
..              setprop        sheet.Range("F5"),*Value="Price"
..              setprop        sheet.Range("G5"),*Value="Client P.O."
..              setprop        sheet.Range("H5"),*Value="List Key"
..              setprop        sheet.Range("I5"),*Value="Order Date"
..              setprop        sheet.Range("J5"),*Value="Mail Date"
..              setprop        sheet.Range("K5"),*Value="Return Date"
..              setprop        sheet.Range("L5"),*Value="Cancelled"
..              setprop        sheet.Range("M5"),*Value="Net"
..              setprop        sheet.Range("N5"),*Value="Exchange"
..              setprop        sheet.Range("O5"),*Value="Split Qty"
...Header Formatting
..              setprop        sheet.Range("A5:S5"),*HorizontalAlignment=xlAlignCenter
..              setprop        sheet.Range("A5:S5").Font,*Bold="True"
..              sheet.range("A5:S5").BorderAround using *LineStyle=1,*Weight=MedThick
.......................................................
.               setprop        sheet.Range("A8"),*Value="LR Number"
.               setprop        sheet.Range("B8"),*Value="List Name"
.               setprop        sheet.Range("C8"),*Value="Select"
.               setprop        sheet.Range("D8"),*Value="Quantity"
.               setprop        sheet.Range("E8"),*Value="All"
.               setprop        sheet.Range("F8"),*Value="Actual Quantity"
.               setprop        sheet.Range("G8"),*Value="Price"
.               setprop        sheet.Range("H8"),*Value="Order Date"
.               setprop        sheet.Range("I8"),*Value="Mail Date"
.               setprop        sheet.Range("J8"),*Value="Status"
.               setprop        sheet.Range("K8"),*Value="Net"
.               setprop        sheet.Range("L8"),*Value="Exchange"
.               setprop        sheet.Range("M8"),*Value="Split Qty"
.               setprop        sheet.Range("N8"),*Value="Comments"
.               setprop        sheet.Range("O8"),*Value="LM-Contact"
..Following Columns will not print!!
.               setprop        sheet.Range("P8"),*Value="List Key"
.               setprop        sheet.Range("Q8"),*Value="Client P.O."
.               setprop        sheet.Range("R8"),*Value="Return Date"
.;dh goes bad               
.               setprop        sheet.Range("S8"),*Value="Sales Person"
.;dh goes bad               
..
.               setprop xlRowHeight,VarValue="27.0"
.               setprop        sheet.range("A8:S8").Rows,*RowHeight=xlRowHeight
..Header Formatting
.               setprop        sheet.Range("A8:S8"),*HorizontalAlignment=xlAlignCenter
.               setprop        sheet.Range("A8:S8").Font,*Bold="True"
.               //Setting up 2 sets of Borders so that user is clear that
.               //the second portion does not actually print
.               sheet.range("A8:O8").BorderAround using *LineStyle=1,*Weight=MedThick
.               sheet.range("P8:S8").BorderAround using *LineStyle=1,*Weight=MedThick
..END PATCH 3.9 REPLACED LOGIC
..
..START PATCH 3.9 REPLACED LOGIC
..              move           C6,howmany
.               move           C8,howmany
..END PATCH 3.9 REPLACED LOGIC
.              endif
..END PATCH 3.72 ADDED LOGIC
.         RESET     COMMENT
.START PATCH 3.72 REPLACED LOGIC
.         SETLPTR   COMMENT
.         DISPLAY   *P15:05,COMMENT
.         MATCH     "                         " TO COMMENT
.         GOTO      OPTDEFLT IF EQUAL
.         RESET     COMMENT
...............................
              call            Trim using COMMENT
              DISPLAY         *P15:05,COMMENT
              if (COMMENT = "")
               GOTO OPTDEFLT IF EQUAL
              endif
.END PATCH 3.72 REPLACED LOGIC
         SCAN      "ALPHA" IN COMMENT
         CALL      OPTALPH IF EQUAL
         RESET     COMMENT
         SCAN      "SUM" IN COMMENT
         CALL      OPTSUM IF EQUAL
         RESET     COMMENT
         SCAN      "SHIP" IN COMMENT
         CALL      OPTSHIP IF EQUAL
         RESET     COMMENT
         SCAN      "TOTAL" IN COMMENT
         CALL      OPTTOTL IF EQUAL
         RESET     COMMENT
         SCAN      "RUN" IN COMMENT
         CALL      OPTRUN IF EQUAL
         RESET     COMMENT
         SCAN      "BANNER" IN COMMENT
         CALL      optBanr IF EQUAL
         RESET     COMMENT
         SCAN      "GUAR" IN COMMENT
         CALL      OPTGUAR IF EQUAL
         RESET     COMMENT
         SCAN      "501C" IN COMMENT
         CALL      OPT501C IF EQUAL
         RESET     COMMENT
         SCAN      "DUPLEX" IN COMMENT
         CALL      OPTDUPL IF EQUAL
         RESET     COMMENT
         SCAN      "BRK" IN COMMENT
         CALL      OPTBRK IF EQUAL
         RESET     COMMENT
         SCAN      "FLAT" IN COMMENT
         CALL      OPTFLAT IF EQUAL
         RESET     COMMENT
         SCAN      "FAX" IN COMMENT
         CALL      OPTPRT IF EQUAL
         RESET     COMMENT
         SCAN      "PREPAY" IN COMMENT
         CALL      OPTPP IF EQUAL
.begin patch 3.41
         RESET     COMMENT
         SCAN      "XTRA" IN COMMENT
         CALL      OPTXTRA IF EQUAL
.end patch 3.41
.begin patch 4.0
         RESET     COMMENT
         SCAN      "PDF" IN COMMENT
         CALL      OPTPDF IF EQUAL
.end patch 4.0
         GOTO      INPGET
OPTNG    KEYIN     *P20:10,"ALPHA  :  BREAK BY MAILER NAME":
                   *P20:09,"PDF    :  Print to PDF":
                   *P20:11,"SHIP   :  INCLUDE SHIPPING QTY":
                   *P20:12,"TOTAL  :  INCLUDE TOTALS":
                   *P20:13,"RUN    :  INCLUDE RUNNING CHARGE ORDERS":
                   *P20:14,"SUM    :  SUMMARY ONLY SUPPRESS DETAILS":
                   *P20:15,"GUAR   :  INCLUDE ORDER GUARANTY DETAIL":
                   *P20:16,"501C   :  INCLUDE CLIENT TAX STATUS":
                   *P20:17,"FAX    :  FORMAT FOR FAX         ":
                   *P20:18,"BRK    :  Break on Broker        ":
                   *P20:19,"DUPLEX :  DUPLEX        ":
                   *P20:20,"PREPAY :  Pre paid summary        ":
                   *P01:24,*EL,"The Option setting is invalid.":
                   *P20:21,"Flat   :  Flat file diskinxx.csv  ":
                   *p20:22,"Banner  : PRINT Banner Page":
.begin patch 3.41
                   *p20:23,"XTRA   : PRINT Extra blank lines":
.end patch 3.41
                   *P15:05,COMMENT:
                   *P20:10,*EL,*P20:11,*EL,*P20:12,*EL,*P20:13,*EL:
                   *P20:14,*EL,*P20:15,*EL,*P20:16,*EL,*P20:17,*EL;
         GOTO      OPTGET
OPTDEFLT MOVE      C1 TO SRTFLAG
         MOVE      C1 TO SHPFLAG
         MOVE      C2 TO TOTFLAG
         MOVE      C1 TO RUNFLAG
         MOVE      C1 TO SUMFLAG
         MOVE      C1 TO GUARFLAG
         MOVE      C1 TO CFLAG
         MOVE      C1 TO FAXFLAG
         MOVE      C1 TO BRKFLAG
         MOVE      C1 TO DUPLFLAG
         MOVE      C1 TO PPFLAG
         move      c1 to flatflag
         move      c1 to banrflag
.begin patch 4.0
         move      c1 to PDFFlag
.end patch 4.0
.begin patch 3.41
         move      c1 to XtraFlag
.end patch 3.41
         GOTO      INPGET
OPTALPH  MOVE      C2 TO SRTFLAG
         RETURN
OPTSHIP  MOVE      C2 TO SHPFLAG
         RETURN
OPTTOTL  MOVE      C2 TO TOTFLAG
         RETURN
OPTRUN   MOVE      C2 TO RUNFLAG
         RETURN
OPTSUM   MOVE      C2 TO SUMFLAG
         RETURN
OPTGUAR  MOVE      C2 TO GUARFLAG
         RETURN
OPT501C  MOVE      C2 TO CFLAG
         RETURN
OPTBRK   MOVE      C2 TO BRKFLAG
         RETURN
OPTDUPL  MOVE      C2 TO DUPLFLAG
         RETURN
OPTFLat  MOVE      C2 TO FlatFLAG
.begin patch 3.92
          call      CreateSheet
.end patch 3.92
         RETURN

OPTPRT   MOVE      C2 TO FAXFLAG
         RETURN
OPTPP    MOVE      C2 TO PPFLAG
         RETURN
.begin patch 3.41
OPTXTRA  MOVE      C2 TO XtraFlag
         RETURN
.end patch 3.41
.begin patch 4.0
OPTPDF   MOVE      C2 TO PDFFlag
         RETURN
.end patch 4.0
optbanr
         MOVE      C2 TO banrFLAG
         return
.
INPGET   TRAP      INPNG GIVING ERROR IF IO
         BRANCH    NINPPATH TO PRTGET
         OPEN      TESTFILE,INPNAME
         MOVE      C1 TO NINPPATH
         TRAPCLR   IO
         DISPLAY   *P15:06,INPNAME
         MOVE      INPNAME TO NORDNAME
.begin patch 3.92
..START PATCH 3.72 ADDED LOGIC
..               RESET          COMMENT
.               if (COMMENT <> "TOTAL BANNER FLAT")
..END PATCH 3.72 ADDED LOGIC
.                              clear     str30
..START PATCH 3.42 REPLACED LOGIC
..          append    "g:\data\" to str30
.                              append    NTWKPATH1 to str30
..END PATCH 3.42 REPLACED LOGIC
..crap 31Oct2000 how long has this been wrong?
.                              scan       "." in inpname
.                              if         equal                .we already have an extension
.                                             bump       inpname,-1           .back up one
.                                             lenset     inpname
.                                             reset      inpname
.                                             append    inpname to str30
.                                             append    ".csv" to str30
.                                             reset     inpname,25
.                                             reset     inpname
.                              else
.                                             reset      inpname
.                              endif
..end of crap hope that did it
.                              reset     str30
.                              prepare   flatfile,str30
..START PATCH 3.72 ADDED LOGIC
.               endif
.END PATCH 3.72 ADDED LOGIC
.         endif
.end patch 3.92
         GOTO      PRTGET
INPNG    NORETURN
         TRAPCLR   IO
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *DV,ERROR:
                   *P15:06,INPNAME
         GOTO      INPGET
.
PRTGET   
.Begin patch 4.0
          IF        (FLATFLAG = C2)
          GOTO      PRESTART
          ENDIF
.enD patch 4.0
          MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MOVE      C1 TO PRTFLAG
         MATCH     LOCAL  TO PRTNAME
         GOTO      PRESTART IF EQUAL
         MOVE      C2 TO PRTFLAG
         PACK      PRTFILE WITH PRTNAME
.Begin patch 4.0
...for automated jobs see if we already have the user

.Find Printer Aread
.BEGIN PATCH 4.01
          if        (PDFFLAG <> 2 or USER = "")
          call      FindPort
          move      portn to ncntfld1
          rep       zfill in ncntfld1
          Move      c3,NCNTPATH
          call      ncntkey
          if        over
                    move      c2 to cntprint
          endif
          move      CntPrint,PrintFlag
          endif
          call      GetWinVer
          if        (PDFFLAG = 2)
.begin patch 4.02                   
.          Call      GetPDFPath
.          Call      PDF995Auto
.          call      SetPDFFlag
.          PRTOPEN   Laser,"PDF995",prtname
          pack      str55 from "c:\work\pdf\",prtname,".pdf"
          PRTOPEN   Laser,"pdf:",str55,Flags=PDF_FLAGS_WIN_ANSI_ENCODING
.end patch 4.02                   
          pack    str45,prtname

          else
          
                    if (PrintFlag = C0 | PrintFlag = 1)     .Laser2 
                              if (osflag >= c6)         .
                                          PRTOPEN Laser,"\\NINS2\laser2",prtname
                                  elseif (osflag = c3 | OSflag =c4)         .win 95 98
                                          PRTOPEN Laser,"Laser2",prtname
                                  else   .(osflag = c0)         .Don't know prompt for printer
                                          PRTOPEN Laser,"-",prtname
                                  endif
                          Elseif        (user = "SMCGUIR")
                                 DISPLAY   *P15:07,PRTNAME,b1,prtflag," Suzie's Printer"
                                            if (osflag = c2)         .nt
                                            PRTOPEN Laser,"Kyocera FS-1030D KX",Str45
                                            elseif (osflag = c6 | osflag = c8 | osflag = c9)         .XP
                                         PRTOPEN Laser,"Kyocera FS-1030D KX",Str45
                                            elseif (osflag = c1)         .win 95 98
                                         PRTOPEN Laser,"FSD-1030D",Str45
                                            else   .(osflag = c0)         .Don't know prompt for printer
                                         PRTOPEN Laser,"@",Str45
                                            endif        
                          elseif (PrintFlag = 2)  .Laser3
                              if (osflag >= c6)         .
                                          PRTOPEN Laser,"\\NINS2\laser3 Blankstock",prtname
                                  elseif (osflag = c3 | osflag =c4)         .win 95 98
                                          PRTOPEN Laser,"Laser3 Blankstock",prtname
                                  else   .(osflag = c0)         .Don't know prompt for printer
                                          PRTOPEN Laser,"-",prtname
                                  endif
                          elseif (PrintFlag = 5)  .Susan
                                    if (osflag = c2)         .nt
                                            PRTOPEN Laser,"Kyocera FS-1030D",Str45
                                    elseif (osflag = c6 | osflag = c8 | osflag = c9)         .XP
                                            PRTOPEN Laser,"Kyocera FS-1030D",Str45
                                    elseif (osflag = c1 | osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .win 95 98
                                            PRTOPEN Laser,"KYOCERAS",Str45
                                    else   .(osflag = c0)         .Don't know prompt for printer
                                            PRTOPEN Laser,"@",Str45
                                    endif
                          elseif (PrintFlag = 7)  .david
                                    if (osflag = c2)         .nt +
                                            PRTOPEN Laser,"Kyocera FS-C5030N KX",Str45
                                    elseif (osflag = c6 | osflag = c8 | osflag = c9)         .XP
                                            PRTOPEN Laser,"Kyocera FS-C5030N KX",Str45
                                    elseif (osflag = c1 | osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .win 95 98
                                            PRTOPEN Laser,"KYOCERAM",Str45
                                    else   .(osflag = c0)         .Don't know prompt for printer
                                            PRTOPEN Laser,"@",Str45
                                    endif
                    endif
          endif
.         SPLOPEN   PRTFILE
.end patch 4.0
         DISPLAY   *P15:07,PRTNAME
         compare   c2 to banrflag
         call      prtbanr if equal
         GOTO      PRESTART
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
.
+........................................................................
.
PRESTART KEYIN     *P1:24,*T5,STR1;
         CLEAR     PF4
         TRAPCLR   F4
         CALL      FUNCDISP
START    CALL      NORD2SEQ
         GOTO      EOJ IF OVER
         ADD       C1 TO N9
         DISPLAY   *P15:09,N9
.START PATCH 3.3 - NEW LOGIC
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
.END PATCH 3.3 - NEW LOGIC
.Start Patch #3.1.
.         compare   c2 to faxflag
.         if        equal
.End Patch #3.1 -
.Start Patch #3.0 - replaced var
.        move      c0 to n7
.        move      oqty to n7
.        compare   c0 to n7
         move      c0 to NUM9
         move      oqty to NUM9
         compare   c0 to NUM9
.end Patch #3.0 - replaced var
         if        equal
         goto      start
         endif
.         endif
.
.
         BRANCH    RUNFLAG OF CHKRUN,PROCESS
.
CHKRUN   RESET     RUNCODES
         SCAN      OLNUM IN RUNCODES
         GOTO      START IF EQUAL
.
         RESET     EXFEELST
         SCAN      OLNUM IN EXFEELST
         GOTO      START IF EQUAL
PROCESS  MOVE      B11 TO CANTEXT
.begin patch 3.5
.only lcrs
.patch3.5
        call trim using inits
        rep  "lL" in inits
.subpatch3.5
        if (inits = "L")
                              if        (Ostat = "l")
                              MOVE      "(LCR)" TO CANTEXT
                              goto      PPCHK
                              Elseif    (ostat = "z")
                              MOVE      "(Denied)" TO CANTEXT
.              CMATCH    "l" TO OSTAT       lcr order ?
.              GOTO      Start IF NOT EQUAL     NO,skip .
.            MOVE      "(LCR)" TO CANTEXT
                              goto      PPCHK
                              else
                              goto      Start
                              endif
                    endif
.sub patch3.5
.only pending orders
           rep        "pP" in inits
           if         (inits = "P")
.begin patch 4.05
                      IF (Ostat <> "p" & Ostat <> "x")
                      goto       start
                      endif
                      if         (ostat = "p")
                      MOVE      "(Pending)" TO CANTEXT
                      Elseif     (ostat = "x")
                      MOVE      "(Canc.Pend)" TO CANTEXT
.              CMATCH    "p" TO OSTAT       pending order ?
.              GOTO      Start IF NOT EQUAL     NO,skip .
.            MOVE      "(Pending)" TO CANTEXT
.end patch 4.05
                      endif
           goto      PPCHK
           endif
.sub patch3.5
.skip cancelled
        rep  "eE" in inits
       if (inits = "E")
              RESET     CANCODES
              SCAN      OSTAT IN CANCODES
              GOTO      Start IF EQUAL     YES,skip .
                              endif
.sub patch3.5
        rep  "nN" in inits
        if (inits = "N")
                                             if (onetper = "  ")
               GOTO Start
                                             endif
                              endif
.sub patch3.5
.end patch 3.5
.begin patch 2.8
.START PATCH 3.9 REPLACED LOGIC
.         move      "pxlz" to str4
.         scan      ostat in str4
.         GOTO      Start IF EQUAL     YES, skip.
              if (OSTAT = "p" | OSTAT = "x" | OSTAT = "z")
               goto Start
              elseif (OSTAT = "l")
.begin patch 3.92
.               if (COMMENT = "TOTAL BANNER FLAT")
               if (FlatFlag = c2)
.end patch 3.92
                              pack           NORD5FLD,OLRN
                              move           "PROCESS-NORD5KEY",Location
                              pack           KeyLocation,"Key: ",NORD5FLD
                              call           NORD5KEY
                              if over
                                             goto Start
                              else
                                             if (NORD5STAT <> "08")
                                                            goto Start
                                             endif
                              endif
               else
                              goto Start
               endif
              endif
.END PATCH 3.9 REPLACED LOGIC
.        CMATCH    "p" TO OSTAT       Pending order ?
.        GOTO      Start IF EQUAL     YES, skip.
.        CMATCH    "x" TO OSTAT       Cancelled Pending order ?
.        GOTO      Start IF EQUAL     YES, skip.
.note cancodes also updated to skip cancelled pending orders.
.end patch 2.8
canc
                              RESET     CANCODES
         SCAN      OSTAT IN CANCODES
         GOTO      PPCHK IF NOT EQUAL
         MOVE      "(CANCELLED)" TO CANTEXT
         GOTO      PPCHK
.
PPCHK    Branch    ppflag to entchk,ppchk1
         goto      entchk
ppchk1   MOVE      C0 TO N1
         MOVE      GUARCODE TO N1
         branch    n1 of chkbrk,chkbrk,chkbrk,chkbrk,chkbrk,chkinv,chkinv:
                   chkinv,chkinv
chkbrk   goto      start                            .currently no prepaid brk flag
chkinv
         RESET     CANCODES
         SCAN      OSTAT IN CANCODES
         GOTO      start IF EQUAL            .suppress cancelled orders
         move      olrn to ninvfld
         move      c1 to ninvpath
         rep       zfill in ninvfld
         call      ninvkey
         goto      entchk if over
         goto      start
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
         BRANCH    BRKFLAG TO SRTCHK,SRTCHK1
SRTCHK   BRANCH    SRTFLAG TO CHKNUM,CHKALPH
CHKNUM   MATCH     OMLRNUM TO HOLDMLR
         GOTO      CHKOFR IF EQUAL
         CALL      NEWMLR
         GOTO      EXCHK
CHKALPH  MATCH     ORDcNAME TO HOLDNAME
         CALL      NEWMLR IF NOT EQUAL
.
CHKOFR   UNPACK    OODNUM INTO STR4,ORDOFR
         MOVE      B4 TO STR4
         MATCH     ORDOFR TO HOLDOFR
         CALL      NEWOFR IF NOT EQUAL
         GOTO      EXCHK
.
SRTCHK1  BRANCH    SRTFLAG TO CHKNUM1,CHKALPH1
CHKNUM1  MATCH     OBRKNUM TO HOLDBRK
         IF        EQUAL
         GOTO      srtchk
         ENDIF
         CALL      NEWBRK
         GOTO      EXCHK
CHKALPH1 MATCH     ORDmNAME TO HOLDbNME
         GOTO      srtchk IF EQUAL
         CALL      NEWBRK
.
.
EXCHK    MOVE      B6 TO EXCHTEXT
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES
.         GOTO      RENT IF NOT EQUAL
         GOTO      EXCHANGE IF EQUAL
         GOTO      RENT
EXCHANGE MOVE      "*EXCH*" TO EXCHTEXT
         RESET     CANCODES
         SCAN      OSTAT IN CANCODES
         GOTO      DETAIL IF EQUAL
.Start Patch #3.0 - replaced var
.         MOVE      C0 TO N7
.         MOVE      OEXQTY TO N7
.         COMPARE   C0 TO N7
         MOVE      C0 TO NUM9
         MOVE      OEXQTY TO NUM9
         COMPARE   C0 TO NUM9
.End Patch #3.0 - replaced var
.         IF        EQUAL
         GOTO      SPLIT IF NOT EQUAL
         ADD       C1 TO TORDNUME
         ADD       C1 TO bORDNUME
.Start Patch #3.0 - replaced var
.         MOVE      C0 TO N7
.         MOVE      OQTY TO N7
.         ADD       N7 TO TORDQTYE
.         ADD       N7 TO bORDQTYE
.
         MOVE      C0 TO NUM9
         MOVE      OQTY TO NUM9
         ADD       NUM9 TO TORDQTYE
         ADD       NUM9 TO bORDQTYE
.End Patch #3.0 - replaced var
         GOTO      DETAIL
.         ELSE
SPLIT    ADD       C1 TO TORDNUMS
         add       c1 to bordnums
.Start Patch #3.0 - replaced var
.         ADD       N7 TO TORDQTYE
.         SUB       N7 FROM TORDQTYR
.         ADD       N7 TO bORDQTYE
.         SUB       N7 FROM bORDQTYR
.         MOVE      C0 TO N7
.         MOVE      OQTY TO N7
.         ADD       N7 TO TORDQTYR
.         ADD       N7 TO bORDQTYR
.
         ADD       NUM9 TO TORDQTYE
         SUB       NUM9 FROM TORDQTYR
         ADD       NUM9 TO bORDQTYE
         SUB       NUM9 FROM bORDQTYR
         MOVE      C0 TO NUM9
         MOVE      OQTY TO NUM9
         ADD       NUM9 TO TORDQTYR
         ADD       NUM9 TO bORDQTYR
.End Patch #3.0 - replaced var
.         ENDIF
         GOTO      DETAIL
RENT     RESET     CANCODES
         SCAN      OSTAT IN CANCODES
         GOTO      DETAIL IF EQUAL
         ADD       C1 TO TORDNUMR
         ADD       C1 TO bORDNUMR
.Start Patch #3.0 - replaced var
.         MOVE      C0 TO N7
.         MOVE      OQTY TO N7
.         ADD       N7 TO TORDQTYR
.         ADD       N7 TO bORDQTYR
         MOVE      C0 TO NUM9
         MOVE      OQTY TO NUM9
         ADD       NUM9 TO TORDQTYR
         ADD       NUM9 TO bORDQTYR
.End Patch #3.0 - replaced var
.
DETAIL   MOVE      DATEMASK TO DATEPRT1
.         EDIT      ORDDATE  TO DATEPRT1
.Start Patch #3.0 - added century
.         PACK      DATEPRT1 FROM OODTEM,SLASH,OODTED,SLASH,OODTEY
.         MOVE      DATEMASK TO DATEPRT2
..         EDIT      ORDMLDDT TO DATEPRT2
.         PACK      DATEPRT2 FROM OMDTEM,SLASH,OMDTED,SLASH,OMDTEY
.         MOVE      DATEMASK TO DATEPRT3
..         EDIT      ORDRTNDT TO DATEPRT3
.         PACK      DATEPRT3 FROM ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEY
....
         PACK      DATEPRT1 FROM OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
         MOVE      DATEMASK TO DATEPRT2
.         EDIT      ORDMLDDT TO DATEPRT2
         PACK      DATEPRT2 FROM OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
         MOVE      DATEMASK TO DATEPRT3
.         EDIT      ORDRTNDT TO DATEPRT3
         PACK      DATEPRT3 FROM ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY
.End Patch #3.0 - added century
         MOVE      NUMMASKA TO NUMPRTA
         MOVE      OLNUM TO N6
         EDIT      N6 TO NUMPRTA
         MOVE      NUMMASK  TO NUMPRT1
.Start Patch #3.0 - replaced var
.         MOVE      OQTY   TO N7
.         EDIT      N7       TO NUMPRT1
.         MOVE      NUMMASK  TO NUMPRT3          SET UP SPLIT QTY
.         MOVE      C0 TO N7
.         MOVE      OEXQTY   TO N7
.         EDIT      N7       TO NUMPRT3
.
         MOVE      OQTY   TO NUM9
         EDIT      NUM9       TO NUMPRT1
         MOVE      NUMMASK  TO NUMPRT3          SET UP SPLIT QTY
         MOVE      C0 TO NUM9
         MOVE      OEXQTY   TO NUM9
         EDIT      NUM9       TO NUMPRT3
.End Patch #3.0 - replaced var
         MOVE      PPMMASK TO PPMPRT
         MOVE      C0 TO N52
.START PATCH 3.6 REPLACED LOGIC
.         MOVE      OPPM TO N52
.         DIV       "100" INTO N52
              packkey         NSEL2FLD,"1",OLRN
              move            "NSEL2KEY",Location
              pack            KeyLocation,"Key: ",NSEL2FLD
              call            NSEL2KEY
              if over
               move           O2DES,NSEL2NAME
               clear          O2DES2
               unpack         OPPM,str3,str2
               pack           str6,str3,".",str2
               rep            zfill,str6
               move           C0,NSEL2PRICE
               move           str6,NSEL2PRICE
               move           "/M",NMODDESC
              else
               call           PARSITUP using O2DES,NSEL2NAME,C1
               call           PARSITUP using O2DES2,NSEL2NAME,C1
               reset          NSEL2NAME
               call           Trim using NSEL2NAME
               pack           NMODFLD,NSEL2DESC
               rep            zfill,NMODFLD
               move           "NMODKEY",Location
               pack           KeyLocation,"Key: ",NMODFLD
               call           NMODKEY
               if over
                              move           "/M",NMODDESC
               endif
              endif
              MOVE            NSEL2PRICE,N52
.END PATCH 3.6 REPLACED LOGIC
         MOVE      N52 TO N42
         EDIT      N42 TO PPMPRT
         RESET     CANCODES
         SCAN      OSTAT IN CANCODES
         GOTO      CHKCNT IF EQUAL
.Start Patch #3.0 - replaced var
.         MOVE      C0  TO    N7
.         MOVE      OQTY TO N7
.         ADD       N7       TO TORDQTY
.         ADD       C1 TO TORDNUM
.         ADD       N7       TO bORDQTY
.
         MOVE      C0  TO    NUM9
         MOVE      OQTY TO NUM9
         ADD       NUM9       TO TORDQTY
         ADD       C1 TO TORDNUM
         ADD       NUM9       TO bORDQTY
.End Patch #3.0 - replaced var
         ADD       C1 TO bORDNUM
         move      b4 to prtnet
         cmatch    b1 to onetfm
         goto      chkcnt if equal
         move      c0 to n2
         move      onetper to n2
         compare   c0 to n2
         if        not equal
         clear     prtnet
         append    onetper to prtnet
         append    "%" to prtnet
         reset     prtnet
         endif
.
. IF THERE IS A BROKER INVOLVED, AND ITS A NUMERIC BREAK, READ THE MAILER
. FILE TO GET THE MAILER NAME.
.
CHKCNT   BRANCH    BRKFLAG TO DTL0
         BRANCH    SRTFLAG OF CHKCNT1,DTL0
CHKCNT1  MATCH     OCOBN TO HOLDCNT
         GOTO      DTL0 IF EQUAL
         MOVE      OMLRNUM TO HOLDMLR
         MOVE      OCOBN TO HOLDCNT
.        MOVE      C1 TO BRKFLAG
.        MATCH     B3 TO OCOBN
.        GOTO      DTL0 IF EQUAL
         PACK      MKEY WITH OMLRNUM,OCOBN
.         DISPLAY   *P1:22,"READING MAILER FILE CHKCNT",*B
         CALL      NMLRKEY
         GOTO      MLRNG IF OVER
.        MOVE      C2 TO BRKFLAG
.
DTL0     BRANCH    SUMFLAG OF C501,START
.
C501     BRANCH    CFLAG OF DTL0A,C501A
.
C501A    CLEAR     NXRFMLR                 *CLEAR VARIABLE IN CASE OVER.
         CLEAR     TAXPRT
         MATCH     OLNUM TO NXRFFLD
         GOTO      DTL0A IF EQUAL
         MOVE      OLNUM TO NXRFFLD
         MOVE      C1 TO NXRFPATH
         CALL      NXRFKEY
         GOTO      DTL0A IF OVER
         MOVE      compnum TO nmtxfld
.         MOVE      NXRFMLR TO NMTXFLD
         CALL      NMTXKEY
         GOTO      DTL0A IF OVER
         MOVE      C0 TO N1
         MOVE      MTXC501 TO N1
         BRANCH    N1 OF DTL0A,DTL0A,C5013,C5014,C5015,C5016
         GOTO      DTL0A
C5013    MOVE      "501C-3" TO TAXPRT
         GOTO      DTL0A
C5014    MOVE      "501C-4" TO TAXPRT
         GOTO      DTL0A
C5015    MOVE      "501C-5" TO TAXPRT
         GOTO      DTL0A
C5016    MOVE      "501C-6" TO TAXPRT
         GOTO      DTL0A
.
DTL0A    
.          COMPARE   C57 TO PRTLINES
.         CALL      HEADING IF NOT LESS
                    if        (row >= 10000)
                    call      heading
                    endif
         BRANCH    SHPFLAG TO DTL1,DTL2
.
.Start Patch #3.0 - remmed and replaced logic
.DTL1     PRINT     *N,*03,DATEPRT1:
.                      *15,OMLRPON:
.                      *25,OLRN:
.                      *34,O1DES:
.                      *72,PPMPRT,"/M":
.                      *83,NUMPRT1,B1,ENTTEXT:
.                      *99,OMLRKY:
.                      *114,DATEPRT2:
.                      *125,DATEPRT3:
.                   *N,*15,TAXPRT:
.                      *26,NUMPRTA:
.                      *34,O2DES:
.                      *01,CANTEXT:
.                      *72,COMTEXT:
.                      *83,prtnet:
.                      *87,EXCHTEXT:
.                      *94,NUMPRT3:
.                      *119,TAPETEXT
.START PATCH 3.6 REPLACED LOGIC
.DTL1     PRINT     *N,*01,DATEPRT1:
.                      *15,OMLRPON:
.                      *25,OLRN:
.                      *34,O1DES:
.                      *72,PPMPRT,"/M":
.                      *81,NUMPRT1,B1,ENTTEXT:
.                      *96,OMLRKY:
.                      *110,DATEPRT2:
.                      *123,DATEPRT3:
.                   *N,*15,TAXPRT:
.                      *26,NUMPRTA:
.                      *34,O2DES:
.                      *01,CANTEXT:
.                      *72,COMTEXT:
.                      *83,prtnet:
.                      *87,EXCHTEXT:
.                      *97,NUMPRT3:
.                      *119,TAPETEXT
.begin patch 4.0
DTL1      if        (flatFlag <> c2)
          add       Sixlpi,row
          add       Sixlpi,Row
          Clear     ppmprt1
          pack      ppmprt1 from ppmprt,nmoddesc
          PRTPAGE   Laser;*p=1:row,DATEPRT1:
                    *p=1075:row,omlrpon:
                    *p=1575:row,olrn:
                    *p=2094:row,O1Des:
                    *p5000:row,*alignment=*right,ppmprt1,*alignment=*Left:
                    *p5625:row,*alignment=*right,numprt1,*alignment=*Left,b1,enttext:
                    *p6000:row,Omlrky:
                    *p=6775:row,DatePrt2:
                    *p=8160:row,*alignment=*right,DatePrt3,*alignment=*Left
          add       sixlpi,row
          PRTPAGE   Laser;*p=1:row,CANTEXT:
                    *p=1075:row,TaxPrt:
                    *p=1575:row,Numprta:
                    *p=2094:row,O2Des:
                    *p5000:row,*alignment=*right,ComText,*alignment=*Left:
                    *p5100:row,prtnet:
                    *p5300:row,ExchText:
                    *p=6775:row,Numprt3:
                    *p=7775:row,TapeTExt
.DTL1     PRINT     *N,*01,DATEPRT1:
.                      *15,OMLRPON:
.                      *25,OLRN:
.                      *34,O1DES:
.                      *72,PPMPRT,NMODDESC:
.                      *81,NUMPRT1,B1,ENTTEXT:
.                      *96,OMLRKY:
.                      *110,DATEPRT2:
.                      *123,DATEPRT3:
.                   *N,*15,TAXPRT:
.                      *26,NUMPRTA:
.                      *34,O2DES:
.                      *01,CANTEXT:
.                      *72,COMTEXT:
.                      *83,prtnet:
.                      *87,EXCHTEXT:
.                      *97,NUMPRT3:
.                      *119,TAPETEXT
              call            Trim using O2DES2
              if (O2DES2 <> "")
                    add       sixlpi,row
                    PrtPage   Laser;*p=2094:row,O2DES2
.               PRINT          *34,O2DES2
          endif
.end patch 4.0
               ADD            C1,PRTLINES
              endif
.END PATCH 3.6 REPLACED LOGIC
.end Patch #3.0 - remmed and replaced logic
         ADD       C3 TO PRTLINES
         compare   c2 to flatflag
         if        equal
.         write     flatfile,seq;*cdfon:
.                   OLRN:
.                   O1DES:
.                   O2DES:
.                   b1:
.                   NUMPRT1:
.                   B1:
.                   b1:
.                   b1:
.                   b1:
.                   PPMPRT,"/M":
.                   OMLRKY:
.                   DATEPRT2:
.                   DATEPRT3:
.                   TAXPRT:
.                   NUMPRTA:
.                   CANTEXT:
.                   COMTEXT:
.                   prtnet:
.                   EXCHTEXT:
.                   NUMPRT3:
.                   TAPETEXT
.                   endif
.START PATCH 3.6 REPLACED LOGIC
.         write     flatfile,seq;*cdfon:
.                   OLRN:
.                   O1DES:
.                   O2DES:
.                   ofdesc:
.                   NUMPRT1:
.                   PPMPRT:
.                   OMLRpon:
.                   OMLRKY:
.                   DATEPRT1:
.                   DATEPRT2:
.                   DATEPRT3:
.                   CANTEXT:
.                   prtnet:
.                   EXCHTEXT:
.                   NUMPRT3
.begin patch 3.92
.START PATCH 3.72 ADDED LOGIC
.              RESET           COMMENT
.              if (COMMENT = "TOTAL BANNER FLAT")
          if        (FlatFlag = c2)
               call           WriteExcelRecord
.end patch 3.92
.              else
..END PATCH 3.72 ADDED LOGIC
.         write     flatfile,seq;*cdfon:
.                   OLRN:
.                   O1DES:
.                   NSEL2NAME:
.                   ofdesc:
.                   NUMPRT1:
.                   PPMPRT:
.                   OMLRpon:
.                   OMLRKY:
.                   DATEPRT1:
.                   DATEPRT2:
.                   DATEPRT3:
.                   CANTEXT:
.                   prtnet:
.                   EXCHTEXT:
.                   NUMPRT3
.END PATCH 3.6 REPLACED LOGIC
.START PATCH 3.72 ADDED LOGIC
              endif
.end patch 3.92
.END PATCH 3.72 ADDED LOGIC
       endif
.
         BRANCH    BRKFLAG TO GUAR
.begin patch 4.0
          if        (flatflag <>c2)
          add       Sixlpi,row
          Prtpage   Laser;*p=2150:row,ORDMNAME
.         PRINT        *40,ORDMNAME
          endif
.end patch 4.0
         ADD       C1 TO PRTLINES
.begin patch 3.41
.GUAR     BRANCH    GUARFLAG TO START,GUARA
.         GOTO      START
GUAR     BRANCH    GUARFLAG TO chkXtra,GUARA
         GOTO      CHKXtra
.end patch 3.41
GUARA    CMATCH    B1 TO GUARCODE
         GOTO      START IF EQUAL
         MOVE      C0 TO N1
         MOVE      GUARCODE TO N1
         LOAD      GUARTEXT FROM N1 OF DAY30,DAY45,DAY60,DAY0,B1,PRE,PRE30:
                   PRE45,PRE60
.        PRINT     *34,HPBON,HPUNON,GUARTEXT,HPBOFF,HPUNOFF
.begin patch 4.0
          if        (flatflag <> c2)
          add       Sixlpi,row
          Prtpage   Laser;*p=2094:row,*ulon,GuarText,*UlOff
          endif
.         IFNZ       PC
.         PRINT     *34,GUARTEXT,*FLUSH,*34,"____________"
.         XIF
.         IFZ        PC
.         PRINT     *34,GUARTEXT,*34,"____________"
.         XIF
.end patch 4.0
         ADD       C1 TO PRTLINES
.begin patch 3.41
         GOTO      CHKXtra
.         GOTO      START
chkxtra   compare  c2 to Xtraflag
          if       equal
.begin patch 4.0
          add       Sixlpi,row
.          print    *1,b1
.end patch 4.0
          add      c1 to prtlines
          endif
          goto     start
.end patch 3.41
.
DTL2
.Start Patch #3.0 - replaced var
.         MOVE      C0 TO N7
.         MOVE      B7 TO SQUANT
         MOVE      C0 TO NUM9
         MOVE      B9 TO SQUANT
.End Patch #3.0 - replaced var
         RESET     REURTNS
         SCAN      ORTNNUM IN REURTNS        RE-USE RETURN-TO #?
         GOTO      DTL2B IF NOT EQUAL       NO. CHECK FOR SHIPPING RECORD
.START PATCH 3.4 - REPLACED LOGIC - UN-REMMED CODE
..Start Patch #3.0 - logic until SQUANT increased
         MOVE      OQTY TO SQUANT         YES. USE ORDERED QTY AS SHIPPED QTY.
.         COUNT     N1,OQTY
.         IF (N1 = 9)
.                   bump      SQUANT,C2
.                   MOVE      OQTY TO SQUANT         YES. USE ORDERED QTY AS SHIPPED QTY.
.                   RESET     OQTY
.         ELSEIF (N1 = 8)
.                   bump      SQUANT,C1
.                   MOVE      OQTY TO SQUANT         YES. USE ORDERED QTY AS SHIPPED QTY.
.                   RESET     OQTY
.         ELSE   .N1 = 7
.                   MOVE      OQTY TO SQUANT         YES. USE ORDERED QTY AS SHIPPED QTY.
.         ENDIF
..End Patch #3.0 - logic until SQAUNT increased
.END PATCH 3.4 - REPLACED LOGIC - UN-REMMED CODE
         GOTO      DTL2C
DTL2B    MOVE      OLRN TO NSHPFLD
         CLEAR     SINFO
         CALL      NSHPKEY
DTL2C    MOVE      NUMMASK  TO NUMPRT2
.Start Patch #3.0 - replaced var
.         MOVE      SQUANT   TO N7
.         EDIT      N7       TO NUMPRT2
         MOVE      SQUANT   TO NUM9
         EDIT      NUM9      TO NUMPRT2
.End Patch #3.0 - replaced var
         RESET     CANCODES
         SCAN      OSTAT IN CANCODES
         GOTO      DTL2D IF EQUAL
.Start Patch #3.0 - replaced var
.         ADD       N7  TO TSHPQTY
.         COMPARE   C0 TO N7
         ADD       NUM9 TO TSHPQTY
         COMPARE   C0 TO NUM9
.End Patch #3.0 - replaced var
         IF        NOT EQUAL
         ADD       C1 TO TSHPNUM
         ENDIF
DTL2D
.         COMPARE   C57 TO PRTLINES
.         CALL      HEADING IF NOT LESS
                    if        (row >= 10000)
                    call      heading
                    endif
.Start Patch #3.0 - remmed and replaced logic
.         PRINT     *N,*03,DATEPRT1:
.                      *15,OMLRPON:
.                      *25,OLRN:
.                      *34,O1DES:
.                      *72,PPMPRT,"/M":
.                      *83,NUMPRT1,B1,ENTTEXT:
.                      *99,NUMPRT2:
.                      *114,DATEPRT2:
.                      *125,DATEPRT3:
.                   *N,*26,NUMPRTA:
.                      *34,O2DES:
.                      *01,CANTEXT:
.                      *72,COMTEXT:
.                      *83,prtnet:
.                      *87,EXCHTEXT:
.                      *94,NUMPRT3:
.                      *119,TAPETEXT:
.                   *N,*98,SINFO
.START PATCH 3.6 REPLACED LOGIC
.         PRINT     *N,*01,DATEPRT1:
.                      *15,OMLRPON:
.                      *25,OLRN:
.                      *34,O1DES:
.                      *72,PPMPRT,"/M":
.                      *81,NUMPRT1,B1,ENTTEXT:
.                      *96,NUMPRT2:
.                      *110,DATEPRT2:
.                      *123,DATEPRT3:
.                   *N,*26,NUMPRTA:
.                      *34,O2DES:
.                      *01,CANTEXT:
.                      *72,COMTEXT:
.                      *83,prtnet:
.                      *87,EXCHTEXT:
.                      *97,NUMPRT3:
.                      *119,TAPETEXT:
.                   *N,*98,SINFO
          if        (flatflag <> c2)
          add       Sixlpi,row
          add       Sixlpi,row
          Clear     ppmprt1
          pack      ppmprt1 from ppmprt,nmoddesc
          PRTPAGE   Laser;*p=1:row,DATEPRT1:
                    *p=1075:row,omlrpon:
                    *p=1575:row,olrn:
                    *p=2094:row,O1Des:
                    *p5000:row,*alignment=*right,ppmprt1,*alignment=*Left:
                    *p5625:row,*alignment=*right,numprt1,*alignment=*Left,b1,enttext:
                    *p6000:row,numprt2:
                    *p=6775:row,DatePrt2:
                    *p=8160:row,*alignment=*right,DatePrt3,*alignment=*Left
          add       sixlpi,row
          PRTPAGE   Laser;*p=1:row,CANTEXT:
                    *p=1575:row,Numprta:
                    *p=2094:row,O2Des:
                    *p5000:row,*alignment=*right,ComText,*alignment=*Left:
                    *p5100:row,prtnet:
                    *p5300:row,ExchText:
                    *p=6775:row,Numprt3:
                    *p=7775:row,TapeTExt
          add       Sixlpi,row
          Prtpage   Laser;*p=6000:row,Sinfo

.         PRINT     *N,*01,DATEPRT1:
.                      *15,OMLRPON:
.                      *25,OLRN:
.                      *34,O1DES:
.                      *72,PPMPRT,NMODDESC:
.                      *81,NUMPRT1,B1,ENTTEXT:
.                      *96,NUMPRT2:
.                      *110,DATEPRT2:
.                      *123,DATEPRT3:
.                   *N,*26,NUMPRTA:
.                      *34,O2DES:
.                      *01,CANTEXT:
.                      *72,COMTEXT:
.                      *83,prtnet:
.                      *87,EXCHTEXT:
.                      *97,NUMPRT3:
.                      *119,TAPETEXT:
.                   *N,*98,SINFO
              call            Trim using O2DES2
              if (O2DES2 <> "")
          add       Sixlpi,row
          Prtpage   Laser;*p=2094:row,o2des2
.          PRINT          *34,O2DES2
          endif
.end patch 4.0
               ADD            C1,PRTLINES
              endif
.END PATCH 3.6 REPLACED LOGIC
.End Patch #3.0 - remmed and replaced logic
         ADD       C4 TO PRTLINES
         compare   c2 to flatflag
         if        equal
.START PATCH 3.6 REPLACED LOGIC
.         write     flatfile,seq;*cdfon:
.                   OLRN:
.                   O1DES:
.                   O2DES:
.                   NUMPRT1:
.                   B1:
.                   b1:
.                   b1:
.                   PPMPRT,"/M":
.                   OMLRKY:
.                   DATEPRT2:
.                   DATEPRT3:
.                   TAXPRT:
.                   NUMPRTA:
.                   CANTEXT:
.                   COMTEXT:
.                   prtnet:
.                   EXCHTEXT:
.                   NUMPRT3:
.                   TAPETEXT:
.                   sinfo
.begin patch 3.92
.START PATCH 3.72 ADDED LOGIC
.              RESET           COMMENT
.              if (COMMENT <> "TOTAL BANNER FLAT")
..END PATCH 3.72 ADDED LOGIC
.         write     flatfile,seq;*cdfon:
.                   OLRN:
.                   O1DES:
.                   NSEL2NAME:
.                   NUMPRT1:
.                   B1:
.                   b1:
.                   b1:
.                   PPMPRT,NMODDESC:
.                   OMLRKY:
.                   DATEPRT2:
.                   DATEPRT3:
.                   TAXPRT:
.                   NUMPRTA:
.                   CANTEXT:
.                   COMTEXT:
.                   prtnet:
.                   EXCHTEXT:
.                   NUMPRT3:
.                   TAPETEXT:
.                   sinfo
..END PATCH 3.6 REPLACED LOGIC
.        endif
.START PATCH 3.72 ADDED LOGIC
.end patch 3.92
              endif
.END PATCH 3.72 ADDED LOGIC
         BRANCH    BRKFLAG TO GUAR1
.begin patch 4.0
          if        (FlatFlag <> c2)
          add       Sixlpi,row
          Prtpage   Laser;*p=2095:row,ORDMNAME,*p=6000:row,Sinfo
.         PRINT        *34,ORDMNAME,*98,SINFO
.         PRINT        *40,ORDMNAME
          endif
.end patch 4.0

         ADD       C1 TO PRTLINES
GUAR1    BRANCH    GUARFLAG TO START,GUAR1A
         GOTO      START
GUAR1A   CMATCH    B1 TO GUARCODE
         GOTO      START IF EQUAL
         MOVE      C0 TO N1
         MOVE      GUARCODE TO N1
         LOAD      GUARTEXT FROM N1 OF DAY30,DAY45,DAY60,DAY0,B1,PRE,PRE30:
                   PRE45,PRE60
.begin patch 4.0
          if        (FlatFlag <> c2)
          add       Sixlpi,row
          Prtpage   Laser;*p=2095:row,*ULON,GUARTEXT,*ULOFF
          endif
.         IFNZ       PC
.         PRINT     *34,GUARTEXT,*FLUSH,*34,"____________"
.         XIF
.         IFZ        PC
.         PRINT     *34,GUARTEXT,*34,"____________"
.         XIF
.end patch 4.0
.        PRINT     *34,HPBON,HPUNON,GUARTEXT,HPBOFF,HPUNOFF
         ADD       C1 TO PRTLINES
         GOTO      START
+............................................................................
.
. NEW BROKER. PRINT TOTALS FOR OLD BROKER IF REQUESTED.
.             EJECT TO NEW PAGE WITH NEW BROKER NAME.
.
NEWBRK   BRANCH    TOTFLAG TO NEWBRKB
         COMPARE   C0 TO PAGE
         GOTO      NEWBRKB IF EQUAL
         CALL      BRKTOTLS
.
NEWBRKB   BRANCH    SRTFLAG TO BYNUM1,BYALPH1
BYNUM1    MOVE      OBRKNUM TO HOLDBRK
          MOVE      OMLRNUM TO HOLDMLR
          MOVE      OCOBN TO HOLDCNT
          UNPACK    OODNUM INTO STR4,ORDOFR      .SET NEW OFFER ALSO
          GOTO      CNTRBRK
BYALPH1   MOVE      ORDmNAME TO HOLDbNME
          move      ordcname to holdname
          UNPACK    OODNUM INTO STR4,ORDOFR      .SET NEW OFFER ALSO
          MOVE      ORDCNAME TO MCOMP
          MOVE      ORDMNAME TO MNAME
.
CNTRBRK  SETLPTR   MNAME
         ENDSET    MNAME
CHKBHEAD CMATCH    B1 TO MNAME
         GOTO      SETBHEAD IF NOT EQUAL
         BUMP      MNAME BY -1
         GOTO      CHKBHEAD IF NOT EOS
SETBHEAD MOVEFPTR  MNAME TO N3
         MOVE      "132" TO BRKTAB
         SUBTRACT  N3 FROM BRKTAB
         DIVIDE    C2 INTO BRKTAB
         RESET     MNAME
         SETLPTR   MNAME
         call      heading
+............................................................................
.
. NEW MAILER. PRINT TOTALS FOR OLD MAILER IF REQUESTED.
.             EJECT TO NEW PAGE WITH NEW MAILER NAME.
.
NEWMLR   BRANCH    TOTFLAG TO NEWMLRB
         COMPARE   C0 TO PAGE
         GOTO      NEWMLRB IF EQUAL
         CALL      MLRTOTLS
.
NEWMLRB  BRANCH    SRTFLAG TO BYNUM,BYALPH
BYNUM    MOVE      OMLRNUM TO HOLDMLR
         MOVE      OCOBN TO HOLDCNT
         UNPACK    OODNUM INTO STR4,ORDOFR      .SET NEW OFFER ALSO
         PACK      MKEY WITH OMLRNUM,OCOBN
         CALL      NMLRKEY
         GOTO      MLRNG IF OVER
.         GOTO      CHKBRK
         GOTO      CNTRMLR
BYALPH   MOVE      ORDcNAME TO HOLDNAME
         MOVE      ORDCNAME TO MCOMP
         MOVE      ORDMNAME TO MNAME
         UNPACK    OODNUM INTO STR4,ORDOFR      .SET NEW OFFER ALSO
.
.CHKBRK   MOVE      C1 TO BRKFLAG
.         RESET     MCOMP
.         SCAN      "C/O" IN MCOMP
.         GOTO      CNTRMLR IF NOT EQUAL
.         BUMP      MCOMP BY -1
.         APPEND    B3 TO MCOMP
.         RESET     MCOMP
.         cmatch    b1 to mname
.         goto      cntrmlr if eos
.         match     b12 to mname
.         goto     cntrmlr if equal
.         MOVE      C2 TO BRKFLAG
.
CNTRMLR
.CNTRMLR  SETLPTR   MCOMP
.         ENDSET    MCOMP
.CHKMHEAD CMATCH    B1 TO MCOMP
.         GOTO      SETMHEAD IF NOT EQUAL
.         BUMP      MCOMP BY -1
.         GOTO      CHKMHEAD IF NOT EOS
.SETMHEAD MOVEFPTR  MCOMP TO N3
.         MOVE      "132" TO MLRTAB
.         SUBTRACT  N3 FROM MLRTAB
.         DIVIDE    C2 INTO MLRTAB
.         RESET     MCOMP
.         SETLPTR   MCOMP
          call      Trim using Mcomp
         branch    brkflag of nobrk,nohd
nobrk    BRANCH    SUMFLAG OF HDOK,NOHD
HDOK     CALL      HEADING
         RETURN
NOHD     
.         COMPARE   C55 TO PRTLINES
.         IF        NOT LESS
.         CALL      HEADING
                    if        (row >= 10000)
                    call      heading
         RETURN
                    endif

.         ENDIF
         COMPARE   C0 TO PAGE
         IF        EQUAL
         CALL      HEADING
         RETURN
         ENDIF
.begin patch 4.0
          if        (flatFlag <> c2)
          add       SixLPI,row
          prtpage   laser;*p=1:row,*RPTCHAR "*":132,*alignment=*Right,"------------",*alignment=*Left
          add       SixLPI,row
          prtpage   laser;*p=4250:row,*alignment=*Center,Mcomp,*alignment=*Left
.         IFNZ      PC
.         PRINT     *1,"*************************************************":
.                   "*************************************************":
.                   "*************************************":
.                   *N,*MLRTAB,MCOMP
.         ADD       C2 TO PRTLINES
.         RETURN
.         XIF
.         IFZ       PC
.         PRINT     *1,*RPTCHAR "*":132,*N,*MLRTAB,MCOMP
          endif
.end patch 4.0
         ADD       C2 TO PRTLINES
         compare   c2 to brkflag
         call      newofr if equal
         RETURN
         XIF
MLRTOTLS 
.         COMPARE   C57 TO PRTLINES
.         CALL      HEADING IF NOT LESS
                    if        (row >= 10000)
                    call      heading
                    endif
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
NEWTOT1  BRANCH    SUMFLAG OF NEWTOT1A,NEWTOT1B
.Start Patch #3.0 - remmed and replaced logic
.NEWTOT1A PRINT     *N,*83,"---------";
.NEWTOT1B PRINT     *N,*1,"EXCHANGE ORDERS: ",TORDNUME:
.                      *63,"EXCHANGE USAGE:",*81,NUMPRT3B:
.                   *N,*1,"RENTAL   ORDERS: ",TORDNUMR:
.                      *63,"RENTAL   USAGE:",*81,NUMPRT4B:
.                   *N,*1,"RENT/EX  ORDERS: ",TORDNUMS:
.                   *N,*1,"TOTAL    ORDERS: ",TORDNUM:
.                      *63,"TOTAL    USAGE:":
.                      *81,NUMPRT1B
NEWTOT1A 
.begin patch 4.0
          if        (FlatFlag <> c2)
          add       SixLPI,row
          prtpage   laser;*p=6000:row,*alignment=*Right,"------------",*alignment=*Left
.          PRINT     *N,*80,"------------";
NEWTOT1B  
          add       SixLPI,row
          prtpage   laser;*p=01:row,"EXCHANGE ORDERS: ",*p=1750:row,*alignment=*right,TORDNUME:
                    *p=4000:row,"EXCHANGE USAGE:",*p=6000:row,*alignment=*right,NUMPRT3B
          add       SixLPI,row
          prtpage   laser;*p=01:row,"RENTAL   ORDERS: ",*p=1750:row,*alignment=*right,TORDNUMR:
                    *p=4000:row,"RENTAL   USAGE:",*p=6000:row,*alignment=*right,NUMPRT4B
          add       SixLPI,row
          prtpage   laser;*p=01:row,"RENT/EX  ORDERS: ",*p=1750:row,*alignment=*right,TORDNUMS:
                    *p=4000:row,"RENTAL   USAGE:",*p=6000:row,*alignment=*right,NUMPRT4B
          add       SixLPI,row
          prtpage   laser;*p=01:row,"TOTAL    ORDERS: ",*p=1750:row,*alignment=*right,TORDNUM:
                    *p=4000:row,"TOTAL    USAGE:",*p=6000:row,*alignment=*right,NUMPRT1B

.          PRINT     *N,*1,"EXCHANGE ORDERS: ",TORDNUME:
.                      *63,"EXCHANGE USAGE:",*78,NUMPRT3B:
.                   *N,*1,"RENTAL   ORDERS: ",TORDNUMR:
.                      *63,"RENTAL   USAGE:",*78,NUMPRT4B:
.                   *N,*1,"RENT/EX  ORDERS: ",TORDNUMS:
.                   *N,*1,"TOTAL    ORDERS: ",TORDNUM:
.                      *63,"TOTAL    USAGE:":
.                      *78,NUMPRT1B
          endif
.end patch 4.0
.End Patch #3.0 - remmed and replaced logic
         ADD       C5 TO PRTLINES
         goto      totexit
NEWTOT2  BRANCH    SUMFLAG OF NEWTOT2A,NEWTOT2B
.Start Patch #3.0 - remmed and replaced logic
.NEWTOT2A PRINT     *N,*83,"---------";
.NEWTOT2B PRINT     *N,*1,"EXCHANGE ORDERS: ",TORDNUME:
.                      *63,"EXCHANGE USAGE:",*81,NUMPRT3B:
.                   *N,*1,"RENTAL   ORDERS: ",TORDNUMR:
.                      *63,"RENTAL   USAGE:",*81,NUMPRT4B:
.                   *N,*1,"RENT/EX  ORDERS: ",TORDNUMS:
.                   *N,*1,"TOTAL    ORDERS: ",TORDNUM:
.                      *63,"TOTAL    USAGE:":
.                      *81,NUMPRT1B:
.                   *N,*81,"---------":
.                   *N,*1,"TOTAL   SHIPPED: ",TSHPNUM,*81,NUMPRT2B
NEWTOT2A 
.begin patch 4.0
          if        (flatFlag <> c2)
          add       SixLPI,row
          prtpage   laser;*p=6000:row,*alignment=*Right,"------------",*alignment=*Left
.          PRINT     *N,*80,"------------";
NEWTOT2B 
          add       SixLPI,row
          prtpage   laser;*p=01:row,"EXCHANGE ORDERS: ",*p=1750:row,*alignment=*right,TORDNUME:
                    *p=4000:row,"EXCHANGE USAGE:",*p=6000:row,*alignment=*right,NUMPRT3B
          add       SixLPI,row
          prtpage   laser;*p=01:row,"RENTAL   ORDERS: ",*p=1750:row,*alignment=*right,TORDNUMR:
                    *p=4000:row,"RENTAL   USAGE:",*p=6000:row,*alignment=*right,NUMPRT4B
          add       SixLPI,row
          prtpage   laser;*p=01:row,"RENT/EX  ORDERS: ",*p=1750:row,*alignment=*right,TORDNUMS:
                    *p=4000:row,"RENTAL   USAGE:",*p=6000:row,*alignment=*right,NUMPRT4B
          add       SixLPI,row
          prtpage   laser;*p=01:row,"TOTAL    ORDERS: ",*p=1750:row,*alignment=*right,TORDNUM:
                    *p=4000:row,"TOTAL    USAGE:",*p=6000:row,*alignment=*right,NUMPRT1B
          add       SixLPI,row
          prtpage   laser;*p=6000:row,*alignment=*Right,"------------",*alignment=*Left
          add       SixLPI,row
          prtpage   laser;*p=01:row,"TOTAL   SHIPPED: ",*p=1750:row,*alignment=*right,TSHPNUM:
                    *p=4000:row,*p=6000:row,*alignment=*right,NUMPRT2B

.          PRINT     *N,*1,"EXCHANGE ORDERS: ",TORDNUME:
.                      *63,"EXCHANGE USAGE:",*78,NUMPRT3B:
.                   *N,*1,"RENTAL   ORDERS: ",TORDNUMR:
.                      *63,"RENTAL   USAGE:",*78,NUMPRT4B:
.                   *N,*1,"RENT/EX  ORDERS: ",TORDNUMS:
.                   *N,*1,"TOTAL    ORDERS: ",TORDNUM:
.                      *63,"TOTAL    USAGE:":
.                      *78,NUMPRT1B:
.                   *N,*80,"------------":
.                   *N,*1,"TOTAL   SHIPPED: ",TSHPNUM,*78,NUMPRT2B
          endif
.end patch 4.0
.End Patch #3.0 - remmed and replaced logic
         ADD       C6 TO PRTLINES
         goto      totexit
totexit  MOVE      C0 TO TORDQTY
         MOVE      C0 TO TORDQTYE
         MOVE      C0 TO TORDQTYR
         MOVE      C0 TO TORDNUM
         MOVE      C0 TO TORDNUMR
         MOVE      C0 TO TORDNUME
         MOVE      C0 TO TORDNUMS
         MOVE      C0 TO TSHPQTY
         MOVE      C0 TO TSHPNUM
         move      c0 to page
         compare   c2 to brkflag
         return    if equal
.;         print     *1,copyrite,"1981-2001, Names in the News/CA"
.;         print     *1,copyrite,"1981-2004, Names in the News"
.;         print     *1,copyrite,"1981-2007, Names in the News"
.begin patch 3.93
.         print     *1,copyrite,"1981-2008, Names in the News"
.end patch 3.93
.begin patch 3.96
.Begin patch 4.0
          if        (flatFlag < c2)
          add       SixLPI,row
          prtpage   laser;*p=10:row," ©1981-2016, Names in the News"
.          print     *1,copyrite,"1981-2012, Names in the News"
          endif
.end patch 4.0
.end patch 3.96
         add       c1 to prtlines
         return
.
BRKTOTLS 
.          COMPARE   C57 TO PRTLINES
.         CALL      HEADING IF NOT LESS
                    if        (row >= 10000)
                    call      heading
                    endif
         MOVE      NUMMASKB TO NUMPRT1B
         EDIT      BORDQTY  TO NUMPRT1B
         MOVE      NUMMASKB TO NUMPRT2B
         EDIT      BSHPQTY  TO NUMPRT2B
         MOVE      NUMMASKB TO NUMPRT3B
         EDIT      BORDQTYE TO NUMPRT3B
         MOVE      NUMMASKB TO NUMPRT4B
         EDIT      BORDQTYR TO NUMPRT4B
         COMPARE   C0 TO BORDQTY
         RETURN    IF EQUAL
         BRANCH    SHPFLAG TO BRKTOT1,BRKTOT2
BRKTOT1  BRANCH    SUMFLAG OF BRKTOT1A,BRKTOT1B
.Start Patch #3.0 - remmed and replaced logic
.BRKTOT1A PRINT     *N,*83,"---------";
.BRKTOT1B PRINT     *N,*1,"EXCHANGE ORDERS: ",BORDNUME:
.                   *63,"EXCHANGE USAGE:",*81,NUMPRT3B:
.                   *N,*1,"RENTAL   ORDERS: ",BORDNUMR:
.                      *63,"RENTAL   USAGE:",*81,NUMPRT4B:
.                   *N,*1,"RENT/EX  ORDERS: ",BORDNUMS:
.                   *N,*1,"TOTAL    ORDERS: ",BORDNUM:
.                      *63,"TOTAL    USAGE:":
.                      *81,NUMPRT1B
BRKTOT1A 
.begin patch 4.0
          if        (flatFlag <> c2)
          add       SixLPI,row
          prtpage   laser;*p=6000:row,*alignment=*Right,"------------",*alignment=*Left
.          PRINT     *N,*80,"------------";
BRKTOT1B 
          add       SixLPI,row
          prtpage   laser;*p=01:row,"EXCHANGE ORDERS: ",*p=1750:row,*alignment=*right,BORDNUME:
                    *p=4000:row,"EXCHANGE USAGE:",*p=6000:row,*alignment=*right,NUMPRT3B
          add       SixLPI,row
          prtpage   laser;*p=01:row,"RENTAL   ORDERS: ",*p=1750:row,*alignment=*right,BORDNUMR:
                    *p=4000:row,"RENTAL   USAGE:",*p=6000:row,*alignment=*right,NUMPRT4B
          add       SixLPI,row
          prtpage   laser;*p=01:row,"RENT/EX  ORDERS: ",*p=1750:row,*alignment=*right,BORDNUMS:
                    *p=4000:row,"RENTAL   USAGE:",*p=6000:row,*alignment=*right,NUMPRT4B
          add       SixLPI,row
          prtpage   laser;*p=01:row,"TOTAL    ORDERS: ",*p=1750:row,*alignment=*right,BORDNUM:
                    *p=4000:row,"TOTAL    USAGE:",*p=6000:row,*alignment=*right,NUMPRT1B

.          PRINT     *N,*1,"EXCHANGE ORDERS: ",BORDNUME:
.                   *63,"EXCHANGE USAGE:",*78,NUMPRT3B:
.                   *N,*1,"RENTAL   ORDERS: ",BORDNUMR:
.                      *63,"RENTAL   USAGE:",*78,NUMPRT4B:
.                   *N,*1,"RENT/EX  ORDERS: ",BORDNUMS:
.                   *N,*1,"TOTAL    ORDERS: ",BORDNUM:
.                      *63,"TOTAL    USAGE:":
.                      *78,NUMPRT1B
          endif
.end patch 4.0
.End Patch #3.0 - remmed and replaced logic
         ADD       C5 TO PRTLINES
         goto      totexit1
BRKTOT2  BRANCH    SUMFLAG OF BRKTOT2A,BRKTOT2B
.Start Patch #3.0 - remmed and replaced logic
.BRKTOT2A PRINT     *N,*83,"---------";
.BRKTOT2B PRINT     *N,*1,"EXCHANGE ORDERS: ",BORDNUME:
.                   *63,"EXCHANGE USAGE:",*81,NUMPRT3B:
.                   *N,*1,"RENTAL   ORDERS: ",BORDNUMR:
.                      *63,"RENTAL   USAGE:",*81,NUMPRT4B:
.                   *N,*1,"RENT/EX  ORDERS: ",BORDNUMS:
.                   *N,*1,"TOTAL    ORDERS: ",BORDNUM:
.                      *63,"TOTAL    USAGE:":
.                      *81,NUMPRT1B:
.                   *N,*81,"---------":
.                   *N,*1,"TOTAL   SHIPPED: ",BSHPNUM,*81,NUMPRT2B
BRKTOT2A 
.begin patch 4.0
          if        (flatFlag <> c2)
          add       SixLPI,row
          prtpage   laser;*p=6000:row,*alignment=*Right,"------------",*alignment=*Left
.          PRINT     *N,*80,"------------";
BRKTOT2B 
          add       SixLPI,row
          prtpage   laser;*p=01:row,"EXCHANGE ORDERS: ",*p=1750:row,*alignment=*right,BORDNUME:
                    *p=4000:row,"EXCHANGE USAGE:",*p=6000:row,*alignment=*right,NUMPRT3B
          add       SixLPI,row
          prtpage   laser;*p=01:row,"RENTAL   ORDERS: ",*p=1750:row,*alignment=*right,BORDNUMR:
                    *p=4000:row,"RENTAL   USAGE:",*p=6000:row,*alignment=*right,NUMPRT4B
          add       SixLPI,row
          prtpage   laser;*p=01:row,"RENT/EX  ORDERS: ",*p=1750:row,*alignment=*right,BORDNUMS:
                    *p=4000:row,"RENTAL   USAGE:",*p=6000:row,*alignment=*right,NUMPRT4B
          add       SixLPI,row
          prtpage   laser;*p=01:row,"TOTAL    ORDERS: ",*p=1750:row,*alignment=*right,BORDNUM:
                    *p=4000:row,"TOTAL    USAGE:",*p=6000:row,*alignment=*right,NUMPRT1B
          add       SixLPI,row
          prtpage   laser;*p=6000:row,*alignment=*Right,"------------",*alignment=*Left
          add       SixLPI,row
          prtpage   laser;*p=01:row,"TOTAL   SHIPPED: ",*p=1750:row,*alignment=*right,BSHPNUM:
                    *p=4000:row,*p=6000:row,*alignment=*right,NUMPRT2B

.          PRINT     *N,*1,"EXCHANGE ORDERS: ",BORDNUME:
.                   *63,"EXCHANGE USAGE:",*78,NUMPRT3B:
.                   *N,*1,"RENTAL   ORDERS: ",BORDNUMR:
.                      *63,"RENTAL   USAGE:",*78,NUMPRT4B:
.                   *N,*1,"RENT/EX  ORDERS: ",BORDNUMS:
.                   *N,*1,"TOTAL    ORDERS: ",BORDNUM:
.                      *63,"TOTAL    USAGE:":
.                      *78,NUMPRT1B:
.                   *N,*80,"------------":
.                   *N,*1,"TOTAL   SHIPPED: ",BSHPNUM,*78,NUMPRT2B
          endif
.end patch 4.0
.End Patch #3.0 - remmed and replaced logic
         ADD       C6 TO PRTLINES
         goto      totexit1
totexit1 MOVE      C0 TO BORDQTY
         MOVE      C0 TO BORDQTYE
         MOVE      C0 TO BORDQTYR
         MOVE      C0 TO BORDNUM
         MOVE      C0 TO BORDNUMR
         MOVE      C0 TO BORDNUME
         MOVE      C0 TO BORDNUMS
         MOVE      C0 TO BSHPQTY
         MOVE      C0 TO BSHPNUM
         compare   c2 to flatflag
         if        equal
.begin patch 3.92
.START PATCH 3.72 ADDED LOGIC
.               RESET          COMMENT
.               if (COMMENT <> "TOTAL BANNER FLAT")
..END PATCH 3.72 ADDED LOGIC
.         write     flatfile,seq;*cdfon:
.                    copyrite,"1981-2007, Names in the News"
.;                   copyrite,"1981-2004, Names in the News"
.;                   copyrite,"1981-2001, Names in the News/CA"
.                   endif
.end patch 3.92
..START PATCH 3.72 ADDED LOGIC
               endif
.END PATCH 3.72 ADDED LOGIC

.;         print     *1,copyrite,"1981-2001, Names in the News/CA"
.;         print     *1,copyrite,"1981-2004, Names in the News"
.;         print     *1,copyrite,"1981-2007, Names in the News"
.begin patch 3.93
.         print     *1,copyrite,"1981-2008, Names in the News"
.end patch 3.93
.begin patch 3.96
.Begin patch 4.0
          if        (flatFlag <> c2)
          add       SixLPI,row
          prtpage   laser;*p=10:row," ©1981-2016, Names in the News"
.          print     *1,copyrite,"1981-2012, Names in the News"
          endif
.end patch 4.0
.end patch 3.96
         add       c1 to prtlines
         return
.
.START PATCH 3.3 - REPLACED LOGIC, OODES --> OFDESC
.NEWOFR   MOVE      ORDOFR TO HOLDOFR
.         SETLPTR   OODES
.         ENDSET    OODES
.CHKOHEAD CMATCH    B1 TO OODES
.         GOTO      SETOHEAD IF NOT EQUAL
.         BUMP      OODES BY -1
.         GOTO      CHKOHEAD IF NOT EOS
.SETOHEAD MOVEFPTR  OODES TO N3
.         MOVE      "132" TO OFRTAB
.         SUBTRACT  N3 FROM OFRTAB
.         DIVIDE    C2 INTO OFRTAB
.         RESET     OODES
.         SETLPTR   OODES
.         BRANCH    SUMFLAG OF PRTOFR,OFREXIT
.PRTOFR   COMPARE   C55 TO PRTLINES
.         CALL      HEADING IF NOT LESS
.         PRINT     *N,*OFRTAB,OODES
.         ADD       C2 TO PRTLINES
NEWOFR   MOVE      ORDOFR TO HOLDOFR
         SETLPTR   OFDESC
         ENDSET    OFDESC
CHKOHEAD CMATCH    B1 TO OFDESC
         GOTO      SETOHEAD IF NOT EQUAL
         BUMP      OFDESC BY -1
         GOTO      CHKOHEAD IF NOT EOS
SETOHEAD MOVEFPTR  OFDESC TO N3
         MOVE      "132" TO OFRTAB
         SUBTRACT  N3 FROM OFRTAB
         DIVIDE    C2 INTO OFRTAB
         RESET     OFDESC
         SETLPTR   OFDESC
         BRANCH    SUMFLAG OF PRTOFR,OFREXIT
PRTOFR   
.          COMPARE   C55 TO PRTLINES
.         CALL      HEADING IF NOT LESS
          if        (flatflag <> c2)
                    if        (row >= 10000)
                    call      heading
                    endif

          add       Sixlpi,Row
          prtpage   Laser;*p=4250:row,*Alignment=*Center,ofdesc,*alignment=*left
          endif
.         PRINT     *N,*OFRTAB,OFDESC
         ADD       C2 TO PRTLINES
.END PATCH 3.3 - REPLACED LOGIC, OODES --> OFDESC
OFREXIT  RETURN
.
EOJ      BRANCH    TOTFLAG TO EOJB
         CALL      MLRTOTLS
         compare   c2 to flatflag
         if        equal
.begin patch 3.92
.START PATCH 3.72 ADDED LOGIC
.               RESET          COMMENT
.               if (COMMENT = "TOTAL BANNER FLAT")
.          If        (flatFlag = c2)
          If        (flatFlag <> c2)
.end patch 3.92
.Begin patch 4.0
          add       SixLPI,row
          prtpage   laser;*p=10:row," ©1981-2016, Names in the News"
.          print     *1,copyrite,"1981-2012, Names in the News"
          endif
.end patch 4.0
.begin patch dh add copyrite
          If        (flatFlag = c2)
                              add       c2,howmany
.end patch dh add copyrite

                              move           howmany,str9
                              call           Trim using str9
.begin patch dh add copyrite
                    Pack      Str12 from "B",str9
                    Pack      Taskname from "=Hyperlink(#"http://www.namesinthenews.com#",#"© 1981-2016, Names in the News#")"
                    setprop         sheet.Range(str12),*Formula=taskname
.end patch dh add copyrite
.begin patch 4.04
                    Pack      Str12 from "M",str9
                    setprop         sheet.Range(str12),*Value="(T)=Test, (R)=Retest"
                      pack       Range from "M",str9
                      pack       Range2 from "O",str9
                      sheet.range(Range,Range2).Merge

.end patch 4.04
.START PATCH 3.9 REPLACED LOGIC
.                             pack    range,"A5"
.                             pack    range2,"A",str9
.                             sheet.range(range,range2).Columns.Autofit
.                             pack    range,"B5"
.                             pack    range2,"B",str9
.                             sheet.range(range,range2).Columns.Autofit
.                             pack    range,"C5"
.                             pack    range2,"C",str9
.                             sheet.range(range,range2).Columns.Autofit
.                             pack    range,"D5"
.                             pack    range2,"D",str9
.                             sheet.range(range,range2).Columns.Autofit
.                             pack    range,"E5"
.                             pack    range2,"E",str9
.                             sheet.range(range,range2).Columns.Autofit
.                             pack    range,"F5"
.                             pack    range2,"F",str9
.                             sheet.range(range,range2).Columns.Autofit
.                             pack    range,"G5"
.                             pack    range2,"G",str9
.                             sheet.range(range,range2).Columns.Autofit
.                             pack    range,"H5"
.                             pack    range2,"H",str9
.                             sheet.range(range,range2).Columns.Autofit
.                             pack    range,"I5"
.                             pack    range2,"I",str9
.                             sheet.range(range,range2).Columns.Autofit
.                             pack    range,"J5"
.                             pack    range2,"J",str9
.                             sheet.range(range,range2).Columns.Autofit
.                             pack    range,"K5"
.                             pack    range2,"K",str9
.                             sheet.range(range,range2).Columns.Autofit
.                             pack    range,"L5"
.                             pack    range2,"L",str9
.                             sheet.range(range,range2).Columns.Autofit
.                             pack    range,"M5"
.                             pack    range2,"M",str9
.                             sheet.range(range,range2).Columns.Autofit
.                             pack    range,"N5"
.                             pack    range2,"N",str9
.                             sheet.range(range,range2).Columns.Autofit
.                             pack    range,"O5"
.                             pack    range2,"O",str9
.                             sheet.range(range,range2).Columns.Autofit
...........................................
.begin patch 4.04 - insert new M column for test/retest move everthing else to the right
                              setprop xlColWidth,VarValue="2.57"
                              pack    range,"E8"
                              pack    range2,"E",str9
                              setprop sheet.range(range,range2).Columns,*ColumnWidth=xlColWidth

                              pack    range,"M8"
                              pack    range2,"M",str9
                              setprop sheet.range(range,range2).Columns,*ColumnWidth=xlColWidth

                              setprop xlColWidth,VarValue="4.00"
                              pack    range,"K8"
                              pack    range2,"K",str9
                              setprop sheet.range(range,range2).Columns,*ColumnWidth=xlColWidth
                              setprop xlColWidth,VarValue="7.57"
                              pack    range,"A8"
                              pack    range2,"A",str9
.                              setprop sheet.range(range,range2).Columns,*ColumnWidth=xlColWidth
                             sheet.range(range,range2).Columns.Autofit
                             sheet.range(range,range2).Rows.Autofit
                             
                               pack    range,"F8"
                              pack    range2,"F",str9
                              setprop sheet.range(range,range2).Columns,*ColumnWidth=xlColWidth
                              pack    range,"G8"
                              pack    range2,"G",str9
                              setprop sheet.range(range,range2).Columns,*ColumnWidth=xlColWidth
                              setprop xlColWidth,VarValue="8.00"
                              pack    range,"D8"
                              pack    range2,"D",str9
                              setprop sheet.range(range,range2).Columns,*ColumnWidth=xlColWidth
                              pack    range,"N8"
                              pack    range2,"N",str9
                              setprop sheet.range(range,range2).Columns,*ColumnWidth=xlColWidth
                              setprop xlColWidth,VarValue="8.86"
                              pack    range,"I8"
                              pack    range2,"I",str9
                              setprop sheet.range(range,range2).Columns,*ColumnWidth=xlColWidth
                              setprop xlColWidth,VarValue="9.29"
                              pack    range,"L8"
                              pack    range2,"L",str9
                              setprop sheet.range(range,range2).Columns,*ColumnWidth=xlColWidth
                              setprop xlColWidth,VarValue="10.14"
                              pack    range,"H8"
                              pack    range2,"H",str9
                              setprop sheet.range(range,range2).Columns,*ColumnWidth=xlColWidth
                              setprop xlColWidth,VarValue="12.14"
                              pack    range,"J8"
                              pack    range2,"J",str9
                              setprop sheet.range(range,range2).Columns,*ColumnWidth=xlColWidth
                              setprop xlColWidth,VarValue="24.86"
                              pack    range,"P8"
                              pack    range2,"P",str9
                              setprop sheet.range(range,range2).Columns,*ColumnWidth=xlColWidth
                              setprop xlColWidth,VarValue="32.25"
                              pack    range,"C8"
                              pack    range2,"C",str9
                              setprop sheet.range(range,range2).Columns,*ColumnWidth=xlColWidth
                              pack    range,"O8"
                              pack    range2,"O",str9
                              setprop sheet.range(range,range2).Columns,*ColumnWidth=xlColWidth
                              setprop xlColWidth,VarValue="34.00"
                              pack    range,"B8"
                              pack    range2,"B",str9
                              setprop sheet.range(range,range2).Columns,*ColumnWidth=xlColWidth
.Following Columns do not Print
                              pack    range,"Q8"
                              pack    range2,"Q",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"R8"
                              pack    range2,"R",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"S8"
                              pack    range2,"S",str9
                              sheet.range(range,range2).Columns.Autofit
.;bad dh bad
                              pack    range,"T8"
                              pack    range2,"T",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"U8"
                              pack    range2,"U",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"V8"
                              pack    range2,"V",str9
                              sheet.range(range,range2).Columns.Autofit
.begin patch 4.11
                              pack    range,"W8"
                              pack    range2,"W",str9
                              sheet.range(range,range2).Columns.Autofit
.end patch 4.11
.;bad dh bad
.Establish Print Area
                              pack           str25,"A1:O",str9
                              setprop        sheet.PageSetup,*PrintArea=str25
.Set Word Wrap to all Cells
                              pack    range,"A8"
.;bad dh bad
.;                             pack    range2,"R",str9
                              pack    range2,"T",str9
.;bad dh bad
                              setprop sheet.range(range,range2),*Wraptext=OTRUE
.END PATCH 3.9 REPLACED LOGIC
CampaignFileNameSelect
                              clear   taskname
.BEGIN PATCH 3.97
.begin 3.97 get exel version info
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
.end 3.97 get exel version info
                              Append    "C:\WORK\NMLRALF",taskname    
                              if        (#ver = c1)
                              append  ".xlsx",taskname
                              else
                              append  ".xls",taskname
                              endif
                              Reset     Taskname                              
.                              move  "C:\WORK\NMLRALF.XLS",taskname
.End PATCH 3.97
                              erase          taskname
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
                              trap    TrapCampaignObject if Object
                              book.saveas giving N9 using *Filename=taskname
                              trapclr Object
.
                              setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
CampaignCleanUp
.Clean up after myself
                              destroy        OTRUE
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
                              destroy        OFALSE
                              setprop ex,*SheetsInNewWorkbook=SheetsDefault
                              ex.quit
                              destroy ex
.Email new XLS to User
                              move    "Here is your Diskin File in Excel",MailSubjct
                              pack    MailBody,"Input File:  ",INPNAME
                              pack      MailBOdy,"Input File:  ",INPNAME
                              Pack      MailTO from User,"@nincal.com"
                              Pack      MailFrom from User,"@nincal.com"
.                              Pack      MailAttach from "c:\work\NMLRALF.XLS"
                              Pack      MailAttach from taskname
.begin patch 3.95
                              Move      c0,TrapCount                   .reset

CheckFile

                              trap      WaitForEnd giving error if IO
                              open      FileCheck,MailAttach,Exclusive          
                              Close     FIleCHeck

.end patch 3.95
                              call      SendMail
               else
.begin patch 3.92
.END PATCH 3.72 ADDED LOGIC
.                              weof      flatfile,seq
.                              close     flatfile
.end patch 3.92
.START PATCH 3.72 ADDED LOGIC
               endif
.END PATCH 3.72 ADDED LOGIC
         endif
         compare   c2 to brkflag
         call      brktotls if equal
EOJB     BRANCH    PRTFLAG TO END
.begin patch 4.0
.         print     hpreset
.         release
.         SPLCLOSE
.         CALL      REMVTOF
          PrtClose  Laser
          if        (pdfflag = c2)
.begin patch 4.02
.          Call      PDF995Auto0
.end patch 4.02
          move    "Here is your Diskin File in PDF",MailSubjct
          pack    MailBody,"Input File:  ",INPNAME
          pack      MailBOdy,"Input File:  ",INPNAME
....xxx
          scan      "@",levels
          if        equal                   .we supplied a full email addy
          reset     levels
          Pack      MailTO from levels,",CReques@nincal.com"
          else
          Pack      MailTO from User,"@nincal.com"
          Pack      MailFrom from User,"@nincal.com"
          endif
          Pack      MailAttach from "c:\work\PDF\",prtname,".pdf"
          Move      c0,TrapCount                   .reset
       
CheckFile1

            trap      WaitForEnd giving error if IO
            open      FileCheck,MailAttach,Exclusive          
            Close     FIleCHeck
            call      SendMail
            Pause   "2"
          Erase     MailAttach
          endif
.check for pdf before going to END (end is a comlogic subroutine)

.end patch 4.0
         GOTO      END
.
HEADING  ADD       C1 TO PAGE
         COMPARE   C1 TO PAGE
         IF        EQUAL
.        PRINT     hpletter,hpland,hp14ptch,hplin8
         BRANCH    FAXFLAG TO NOFAX,FAX
.        COMPARE   C2 TO FAXFLAG
.        IF        not EQUAL
NOFAX    BRANCH    DUPLFLAG TO SIMPLX,DUPLX
.SIMPLX   PRINT     HP17PTCH,HPTOP
SIMPLX   
.Begin patch 4.0
          IF        (FLATFLAG <> C2)
          PRTPAGE   Laser;*NEWPAGE:
                    *UNITS=*HIENGLISH:
                    *ORIENT=*PORTRAIT,*Font=Font8
          ENDIF
.          PRINT     HPtmsr17,HPTOP
         GOTO      HD0
DUPLX    
.Begin patch 4.0
          IF        (FLATFLAG <> C2)
          PRTPAGE   Laser;*NEWPAGE:
                    *UNITS=*HIENGLISH:
                    *ORIENT=*PORTRAIT:
                    *Duplex=2,*Font=Font8                                                   
          ENDIF
.          PRINT     hptmsr17,hpdupl,hptop,*F                .compressed
.        PRINT     hp17ptch,hptop,*F                .compressed simplex
         GOTO      HD0
.         else
.        PRINT     hp17ptch,hpdupl,hptop,*F                .compressed
FAX      
.Begin patch 4.0
          IF        (FLATFLAG <> C2)
          PRTPAGE   Laser;*NEWPAGE:
                    *UNITS=*HIENGLISH:
                    *ORIENT=*PORTRAIT,*Font=Font8
          ENDIF
.          print     diskport                     .20apr94 dlh
.        ENDIF
          Else
.Begin patch 4.0
          IF        (FLATFLAG <> C2)
          PRTPAGE   Laser;*NEWPAGE
          ENDIF
          ENDIF
HD0      
.begin patch 3.91
          If        (Company = c2)
          MOVE      "Pacific Lists, Inc." TO COMPNME
          else
          MOVE      "Names In The News" TO COMPNME
          endif
.end patch 3.91
          BRANCH    SHPFLAG TO HD1,HD2
HD1
.begin patch 4.0
          IF        (FLATFLAG <> C2)
          Move      "250",row
          PrtPAge   Laser;*p=1:row,"CONFIDENTIAL",*p=4250:row,*alignment=*center,compnme,*alignment=*Left,*p=7250:row,"DATE:":
                    *alignment=*right,*p=8160:row,TODAY,*alignment=*left
          add       sixlpi,row
          ENDIF
.begin patch 3.91
.          PRINT     *f,*N,"CONFIDENTIAL":
.                      *52,Compnme:
.                      *119,"DATE: ",TODAY;
.end patch 4.0
.         PRINT     *f,*N,"CONFIDENTIAL":
.                      *52,"NAMES IN THE NEWS CA., INC.":
.                      *119,"DATE: ",TODAY;
.end patch 3.91
         BRANCH    BRKFLAG OF HD1MLR,HD1BRK
.
HD1MLR   
.begin patch 4.0
          IF        (FLATFLAG <> C2)
          PrtPAge   Laser;*p=1:row,"(",OMLRNUM,")",*p=4250:row,*alignment=*center,"LISTS ORDERED BY MAILER:",*alignment=*Left,*p=7250:row,"PAGE:":
                    *alignment=*right,*p=8160:row,PAGE,*ALIGNMENT=*LEFT          
.          PRINT     *N,*01,"(",OMLRNUM,")":
.                      *54,"LISTS ORDERED BY MAILER:":
.                      *119,"PAGE:    ",PAGE:
          add       sixlpi,row
          add       sixlpi,row
          PrtPAge   Laser;*p=4250:row,*alignment=*center,Mcomp,*alignment=*Left
.                   *N:
.                   *N,*MLRTAB,MCOMP;
         MOVE      C6 TO PRTLINES
         compare   c2 to ppflag
         if         equal
          add       sixlpi,row
          PrtPAge   Laser;*p=3500:row,"(Prepaid - Unbilled)"
.          print     *n,hpbon,*56,"(Prepaid - Unbilled)",hpboff;
.end patch 4.0
         add       c2 to prtlines
         endif
          endif
         BRANCH    SUMFLAG OF HD1A,HDEXIT
         GOTO      HD1A
.
HD1BRK   
.begin patch 4.0
          IF        (FLATFLAG <> C2)
          PrtPAge   Laser;*p=4250:row,*alignment=*center,"LISTS ORDERED THRU BROKER:",*p=7250:row,*alignment=*Left,"PAGE:    ":
                    *alignment=*right,*p=8250:row,PAGE,*ALIGNMENT=*LEFT          
.          PRINT     *N:
.                      *52,"LISTS ORDERED THRU BROKER:":
.                      *119,"PAGE:    ",PAGE:
          add       sixlpi,row
          add       sixlpi,row
          PrtPAge   Laser;*p=4250:row,*alignment=*center,MName,*alignment=*Left
.                   *N:
.                   *N,*BRKTAB,MNAME;
          endif
.end patch 4.0
          
         MOVE      C6 TO PRTLINES
         BRANCH    SUMFLAG OF HD1A,HDEXIT
.
.Start Patch #3.0 - remmed and replaced logic
.HD1A     PRINT     *N:
.                   *N,*06,"ORDER":
.                      *15,"CLIENT":
.                      *85,"ORDERED":
.                      *118,"MAIL":
.                      *127,"RETURN":
.                   *N,*07,"DATE":
.                      *15,"PO":
.                      *25,"LR##":
.                      *34,"LIST DESCRIPTION":
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
HD1A     
.begin patch 4.0
          IF        (FLATFLAG <> C2)
          add       sixlpi,row
          add       sixlpi,row
          PrtPAge   Laser;*p=1:row,"ORDER",*p=1075:row,"CLIENT",*p=5250:row,"ORDERED",*p=6775:row,"MAIL",*p=7710:row,"RETURN"
          add       sixlpi,row
          PrtPAge   Laser;*p=1:row,*ULON," DATE",*p=1075:row,"PO",*p=1575:row,"LR##",*p=2094:row,"LIST DESCRIPTION",*p=4500:row,"PRICE":
                    *p=5250:row,"QUANTITY",*p=6000:row,"LIST KEY",*p=6775:row,"DATE",*p=7710:row,"DATE",*ULOFF
          endif
.          PRINT     *N:

.                   *N,*06,"ORDER":
.                      *15,"CLIENT":
.                      *85,"ORDERED":
.                      *118,"MAIL":
.                      *127,"RETURN":
.                   *N,*07,"DATE":
.                      *15,"PO":
.                      *25,"LR##":
.                      *34,"LIST DESCRIPTION":
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
.end patch 4.0
.End Patch #3.0 - remmed and replaced logic
         ADD       C5 TO PRTLINES
         UNPACK    OODNUM INTO STR4,ORDOFR
         MOVE      B4 TO STR4
         compare   c2 to flatflag
         if        equal
         compare   c1 to page
         if        equal
.begin patch 3.92
.START PATCH 3.72 ADDED LOGIC
.               RESET          COMMENT
.               if (COMMENT = "TOTAL BANNER FLAT")
          If        (FlatFlag = c2)
.end patch 3.92
.START PATCH 3.9 REPLACED LOGIC
.                             setprop        sheet.Range("A3"),*Value="Lists Ordered by Mailer: "
.                             setprop        sheet.Range("C3"),*Value=MCOMP
.                             setprop sheet.Range("C3").Font,*Bold="True",*Size=12
                              setprop        sheet.Range("B3"),*Value="Lists Ordered by Mailer:",*HorizontalAlignment=xlAlignRight
                              setprop        sheet.Range("C3"),*Value=MCOMP,*HorizontalAlignment=xlAlignLeft
                              setprop sheet.Range("C3").Font,*Bold="True",*Size=12
                              setprop        sheet.Range("B4"),*Value="List Key:",*HorizontalAlignment=xlAlignRight
                              setprop        sheet.Range("B5"),*Value="Client P.O.:",*HorizontalAlignment=xlAlignRight
                              setprop        sheet.Range("B6"),*Value="Return Date:",*HorizontalAlignment=xlAlignRight
.END PATCH 3.9 REPLACED LOGIC
.begin patch 3.92
.               else
..END PATCH 3.72 ADDED LOGIC
.                        write     flatfile,seq;*cdfon:
.                                  "Confidential"
.                        write     flatfile,seq;*cdfon:
.                                  "Names in the News California., Inc."
.                        write     flatfile,seq;*cdfon:
.                                  "Date: ",today
.                        write     flatfile,seq;*cdfon:
.                                  "Lists ordered by Mailer:",Mcomp
.                        write     flatfile,seq;*cdfon:
.                                  "LR##":
.                                  "List Name":
.                                  "Select Ordered":
.                                  "Offer Desc.":
.                                  "Qty Ordered":
.                                  "PPM":
.                                  "Client PO":
.                                  "List Key":
.                                  "Order Date":
.                                  "Mail Date":
.                                  "Return Date":
.                                  "Cancelled Info":
.                                  "Net":
.                                  "Exchange Info":
.                                  "Split Qty"
..START PATCH 3.72 ADDED LOGIC
.end patch 3.92
               endif
.END PATCH 3.72 ADDED LOGIC
          ENDIF
          endif
.         write      flatfile,seq;*cdfon,b1:
.                    b1:
.                    b1:
.                    b1:
.                    b1:
.                    b1:
.                    b1:
.                    b1
.                    endif

         branch    brkflag of hdofr,hdexit
hdofr    CALL      NEWOFR
HDEXIT   RETURN
.
HD2
.begin patch 4.0
          IF        (FLATFLAG <> C2)
          Move      "250",row
          PrtPAge   Laser;*p=1:row,"CONFIDENTIAL",*p=3250:row,compnme,*p=7250:row,"DATE: ":
                    *alignment=*right,*p=8160:row,tODAY,*ALIGNMENT=*LEFT          
          add       sixlpi,row
          endif
.begin patch 3.91
.          PRINT     *f,*N,"CONFIDENTIAL":
.                      *52,Compnme:
.                      *119,"DATE: ",TODAY;
.end patch 4.0
.         PRINT     *f,*N,"CONFIDENTIAL":
.                      *52,"NAMES IN THE NEWS CA., INC.":
.                      *119,"DATE: ",TODAY;
.end patch 3.91
         BRANCH    BRKFLAG OF HD2MLR,HD2BRK
.
HD2MLR   
.begin patch 4.0
          IF        (FLATFLAG <> C2)
          PrtPAge   Laser;*p=1:row,"(",OMLRNUM,")",*p=3375:row,"LISTS ORDERED BY MAILER:",*p=7250:row,"PAGE:":
                    *alignment=*right,*p=8160:row,PAGE,*ALIGNMENT=*LEFT          
.          PRINT     *N,*01,"(",OMLRNUM,")":
.                      *54,"LISTS ORDERED BY MAILER:":
.                      *119,"PAGE:    ",PAGE:
          add       sixlpi,row
          add       sixlpi,row
          PrtPAge   Laser;*p=4250:row,*alignment=*center,Mcomp,*alignment=*Left
          endif
.end patch 4.0

         MOVE      C6 TO PRTLINES
         BRANCH    SUMFLAG OF HD2A,HDEXIT
         GOTO      HD2A
.
HD2BRK   
.begin patch 4.0
          IF        (FLATFLAG <> C2)
          PrtPAge   Laser;*p=3375:row,"LISTS ORDERED THRU BROKER:",*p=7250:row,"PAGE:":
                    *alignment=*right,*p=8160:row,PAGE,*ALIGNMENT=*LEFT          
.          PRINT     *N:
.                      *52,"LISTS ORDERED THRU BROKER:":
.                      *119,"PAGE:    ",PAGE:
          add       sixlpi,row
          add       sixlpi,row
          PrtPAge   Laser;*p=4250:row,*alignment=*center,MName,*alignment=*Left
.                   *N:
.                   *N,*BRKTAB,MNAME;
          endif
.end patch 4.0
         MOVE      C6 TO PRTLINES
         BRANCH    SUMFLAG OF HD2A,HDEXIT
.
.Start Patch #3.0 - remmed and replaced logic
.HD2A     PRINT     *N:
.                   *N,*06,"ORDER":
.                      *15,"CLIENT":
.                      *85,"ORDERED":
.                      *100,"SHIPPED":
.                      *118,"MAIL":
.                      *127,"RETURN":
.                   *N,*07,"DATE":
.                      *15,"PO":
.                      *25,"LR##":
.                      *34,"LIST DESCRIPTION":
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
HD2A    
.begin patch 4.0
          IF        (FLATFLAG <> C2)
          add       sixlpi,row
          add       sixlpi,row
          PrtPAge   Laser;*p=1:row,"ORDER",*p=1075:row,"CLIENT",*p=5250:row,"ORDERED",*p=6000:row,"SHIPPED",*p=6775:row,"MAIL",*p=7710:row,"RETURN"
          add       sixlpi,row
          PrtPAge   Laser;*p=1:row,*ULON," DATE",*p=1075:row,"PO",*p=1575:row,"LR##",*p=2094:row,"LIST DESCRIPTION",*p=4500:row,"PRICE":
                    *p=5250:row,"QUANTITY",*p=6000:row,"QUANTITY",*p=6775:row,"DATE",*p=7710:row,"DATE",*ULOFF
.          PRINT     *N:
.                   *N,*06,"ORDER":
.                      *15,"CLIENT":
.                      *85,"ORDERED":
.                      *101,"SHIPPED":
.                      *118,"MAIL":
.                      *127,"RETURN":
.                   *N,*07,"DATE":
.                      *15,"PO":
.                      *25,"LR##":
.                      *34,"LIST DESCRIPTION":
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
          endif
.End Patch #3.0 - remmed and replaced logic
         ADD       C5 TO PRTLINES
         compare   c2 to flatflag
         if        equal
.begin patch 3.92
.START PATCH 3.72 ADDED LOGIC
.               RESET          COMMENT
.               if (COMMENT = "TOTAL BANNER FLAT")
          If        (FlatFlag = c2)
.START PATCH 3.9 REPLACED LOGIC
.                             setprop        sheet.Range("A3"),*Value="Lists Ordered by Mailer: "
.                             setprop        sheet.Range("C3"),*Value=MCOMP
.                             setprop sheet.Range("C3").Font,*Bold="True",*Underline=xlUnderlineStyleSingle,*Size=12
                              setprop        sheet.Range("B3"),*Value="Lists Ordered by Mailer:",*HorizontalAlignment=xlAlignRight
                              setprop        sheet.Range("C3"),*Value=MCOMP
.                              setprop sheet.Range("C3").Font,*Bold="True",*Underline=xlUnderlineStyleSingle,*Size=12
                              setprop sheet.Range("C3").Font,*Bold="True",*Size=12
                              setprop        sheet.Range("B4"),*Value="List Key:",*HorizontalAlignment=xlAlignRight
                              setprop        sheet.Range("B5"),*Value="Client P.O.:",*HorizontalAlignment=xlAlignRight
                              setprop        sheet.Range("B6"),*Value="Return Date:",*HorizontalAlignment=xlAlignRight
.END PATCH 3.9 REPLACED LOGIC
.               else
..END PATCH 3.72 ADDED LOGIC
.                        write     flatfile,seq;*cdfon:
.                                  "Confidential"
..begin patch 3.91
.                        write     flatfile,seq;*cdfon:
.                                  "Compnme"
..                        write     flatfile,seq;*cdfon:
..                                  "Names in the News California., Inc."
..end patch 3.91
.                        write     flatfile,seq;*cdfon:
.                                  "Date: ",today
.                        write     flatfile,seq;*cdfon:
.                                  "Lists ordered by Mailer:",Mcomp
.                        write     flatfile,seq;*cdfon:
.                                  "ORDER":
.                                  "CLIENT":
.                                  "ORDERED":
.                                  "SHIPPED":
.                                  "MAIL":
.                                  "RETURN"
.                        write      flatfile,seq;*cdfon,"DATE":
.                                   "PO":
.                                   "LR##":
.                                   "LIST DESCRIPTION":
.                                   "PRICE":
.                                   "QUANTITY":
.                                   "QUANTITY":
.                                   "LIST KEY":
.                                   "DATE":
.                                   "DATE"
..START PATCH 3.72 ADDED LOGIC
.end patch 3.92
               endif
.END PATCH 3.72 ADDED LOGIC
         endif
         UNPACK    OODNUM INTO STR4,ORDOFR
         MOVE      B4 TO STR4
         compare   c2 to brkflag
         cALL      NEWOFR if not equal
         RETURN
.
MLRNG    KEYIN     *P01:24,*EL,*B,"Invalid mailer number: ",*DV,OMLRNUM:
                                  " in LR: ",*DV,OLRN,*CL,STR1
         GOTO      END
prtbanr  
.begin patch 4.0
          IF        (FLATFLAG <> C2)
          PRTPAGE   Laser;*UNITS=*HIENGLISH:
                    *ORIENT=*PORTRAIT,*Font=Font8
          add       SixLPI,row
          prtpage   laser;*p=1:row,*RPTCHAR "*":132
          add       SixLPI,row
          prtpage   laser;*p=1:row,"User : ",user
          add       SixLPI,row
          add       SixLPI,row
          prtpage   laser;*p=1:row,"Date: ",today
          add       SixLPI,row
          add       SixLPI,row
          prtpage   laser;*p=1:row,"input file : ",inpname
          add       SixLPI,row
          prtpage   laser;*p=1:row,*RPTCHAR "*":132
.
.          Print      *l,*rptchar "*":80:
.                    *l,*N:
.                    "User : ",user,*l,*n:
.                    "Date: ",today,*l,*n:
.                    "input file : ",inpname,*l,*n:
.                    "copies : ":
.                    *n,*rptchar "*":80:
.                    *f
         compare    c2 to DUPLFLAG
         if         equal
          PrtPage   Laser;*Newpage
.         print      *f
          endif
.end patch 4.0
         endif
         return
.START PATCH 3.72 ADDED LOGIC
WriteExcelRecord
.START PATCH 3.9 ADDED LOGIC
              if (howmany = 8)               .First Record - Adjust Header information
               setprop        sheet.Range("C4"),*Value=OMLRKY,*HorizontalAlignment=xlAlignLeft
               setprop        sheet.Range("C5"),*Value=OMLRpon,*HorizontalAlignment=xlAlignLeft
               setprop        sheet.Range("C6"),*Value=DATEPRT3,*HorizontalAlignment=xlAlignLeft
              endif
.END PATCH 3.9 ADDED LOGIC
              add             C1,howmany
              move            howmany,str9
              call            Trim using str9
              pack            str12,"A",str9
.handle reuse info here  OREUSE, ORTNNUM
              if              (ORTNNUM = "0001" and Oreuse <> "")
              clear           taskname
              pack            taskname from Olrn,crlf,"Reuse of:",crlf,oreuse
              setprop         sheet.Range(str12),*Value=taskname
              else
              setprop         sheet.Range(str12),*Value=OLRN
              endif
              pack            str12,"B",str9
              call            Trim using O1DES
.April 23 2010 DLH
              REP             "#"'" in  O1DES     ."       Hyperlink does not like double quotes
              
.              setprop         sheet.Range(str12),*Value=O1DES
.dave does a test
.add hyperlink   DH 11 May 2008
          packkey      Ndatfld from olnum
          rep       Zfill,Ndatfld
          move      c1,ndatpath
          move      c3,ndatlock
          call      Ndatkey
.begin patch 4.12
.          if        ((ELSTCDE = "P" | ELSTCDE = "C") & NDATWEB <> "1")
.          Pack      Taskname from "=Hyperlink(#"http://www.ninlists.com/Datacards/Data",olnum,".htm","#",#"",O1des,"#")"
.          setprop         sheet.Range(str12),*Formula=taskname
.          else
          setprop         sheet.Range(str12),*Value=O1DES

.          endif
.end patch 4.12
.add hyperlink in  DH 11 May 2008

              pack            str12,"C",str9
              setprop         sheet.Range(str12),*Value=NSEL2NAME
.START PATCH 3.9 REPLACED LOGIC
.             pack            str12,"D",str9
.             call            Trim using ofdesc
.             setprop         sheet.Range(str12),*Value=ofdesc
.             pack            str12,"E",str9
.             setprop         sheet.Range(str12),*Value=NUMPRT1
.             pack            str12,"F",str9
.             setprop         sheet.Range(str12),*Value=PPMPRT
.             pack            str12,"G",str9
.             setprop         sheet.Range(str12),*Value=OMLRpon
.             pack            str12,"H",str9
.             setprop         sheet.Range(str12),*Value=OMLRKY
.             pack            str12,"I",str9
.             setprop         sheet.Range(str12),*Value=DATEPRT1
.             pack            str12,"J",str9
.             setprop         sheet.Range(str12),*Value=DATEPRT2
.             pack            str12,"K",str9
.             setprop         sheet.Range(str12),*Value=DATEPRT3
.             pack            str12,"L",str9
.             setprop         sheet.Range(str12),*Value=CANTEXT
.             pack            str12,"M",str9
.             setprop         sheet.Range(str12),*Value=prtnet
.             pack            str12,"N",str9
.             setprop         sheet.Range(str12),*Value=EXCHTEXT
.             pack            str12,"O",str9
.             setprop         sheet.Range(str12),*Value=NUMPRT3
....................................................
              pack            str12,"D",str9
              setprop         sheet.Range(str12),*Value=NUMPRT1
              pack            str12,"E",str9
              if (OELCODE = "1" | OELCODE = "3")
               move           YES,str1
              else
               clear          str1
              endif
              setprop         sheet.Range(str12),*Value=str1
              pack            str12,"F",str9
              setprop         sheet.Range(str12),*Value=""
              pack            str12,"G",str9
              setprop         sheet.Range(str12),*Value=PPMPRT
              pack            str12,"H",str9
              setprop         sheet.Range(str12),*Value=DATEPRT1
              pack            str12,"I",str9
              setprop         sheet.Range(str12),*Value=DATEPRT2
              pack            str12,"J",str9
              setprop         sheet.Range(str12),*Value=CANTEXT
              pack            str12,"K",str9
              setprop         sheet.Range(str12),*Value=prtnet
              pack            str12,"L",str9
              setprop         sheet.Range(str12),*Value=EXCHTEXT
.begin patch 4.04
               Clear  str1                             
               if                (OTOCODE = "1")
               move              "T",str1
               elseif (OTOCODE = "2")
               move              "R",str1
               endif                        
              pack            str12,"M",str9
              setprop         sheet.Range(str12),*Value=str1

              pack            str12,"N",str9
              setprop         sheet.Range(str12),*Value=NUMPRT3
              pack            str12,"O",str9
              setprop         sheet.Range(str12),*Value=""
              pack            str12,"P",str9
              move            OLON,NOWNFLD
              rep             ZFILL,NOWNFLD
              call            NOWNKEY
              if not over
               call           Trim using OWNOCPY
               call           Trim using OWNTELE
               count          result,OWNTELE
               if (result = 7)
                              unpack         OWNTELE,str3,str4
                              pack           str15,str3,DASH,str4
               elseif (result = 10)
                              unpack         OWNTELE,str2,str1,str3,str4
                              pack           str15,"(",str2,str1,") ",str3,DASH,str4
               else
                              clear          str15
               endif
               if (str15 <> "")
                              pack           LOText,OWNOCPY,SReturn,str15
               else
                              pack           LOText,OWNOCPY
               endif
              else
               clear          LOText
              endif
              setprop         sheet.Range(str12),*Value=LOText
.Following Columns will not be printed
              pack            str12,"Q",str9
              setprop         sheet.Range(str12),*Value=OMLRKY
              pack            str12,"R",str9
              setprop         sheet.Range(str12),*Value=OMLRpon
              pack            str12,"S",str9
              setprop         sheet.Range(str12),*Value=DATEPRT3
.bad dh
              pack            str12,"T",str9
              clear           str2
              pack            str2 from Osales10,Osales
              setprop         sheet.Range(str12),*Value=Str2
.bad dh
.bad dh again 8/30/10
              pack            str12,"U",str9
              setprop         sheet.Range(str12),*Value=Mcomp
.Begin Patch xxx
              pack            str12,"V",str9
              setprop         sheet.Range(str12),*Value=ORDmNAME
.end Patch xxx
.Begin Patch 4.11
              pack            str12,"W",str9
              setprop         sheet.Range(str12),*Value=OMlrLstCd
.end Patch 4.11

.
.bad dh again 8/30/10
              setprop xlRowHeight,VarValue="36.0"
              pack            str25,"A",str9,":U",str9
              setprop         sheet.range(str25).Rows,*RowHeight=xlRowHeight
.END PATCH 3.9 REPLACED LOGIC
              return
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
.END PATCH 3.72 ADDED LOGIC
...........................................................................
.begin patch 3.92
CreateSheet
.START PATCH 3.72 ADDED LOGIC
.              if (COMMENT = "TOTAL BANNER FLAT" | COMMENT = "TOTAL FLAT")
.end patch 3.92
.Force Excel Option!!!
.Create the Variant objects
.Booleans
               create  OTRUE,VarType=VT_BOOL,VarValue=1
               create  OFALSE,VarType=VT_BOOL,VarValue=0
.START PATCH 3.9 REPLACED LOGIC
.              create  Zoom85,VarType=VT_I4,VarValue=1
               create  Zoom80,VarType=VT_I4,VarValue=80
."1" increment in Excel interface equals "1.3888" in OLE logic
               create         AllMargin,VarType=VT_R8,VarValue="18"                       .Roughly equals .25 inches:  18 * 1.388 = 25
               create         xlColWidth,VarType=VT_R8,VarValue="0.0"                     .Default
.END PATCH 3.9 REPLACED LOGIC
.
               create         xlRowHeight,VarType=VT_R8,VarValue="75.0"
.Open Excel application
               create  ex
              getprop ex,*SheetsInNewWorkbook=SheetsDefault
                      setprop ex,*SheetsInNewWorkbook=C1
.begin patch 4.03
.               setprop ex,*WindowState=xlMinimized
.end patch 4.03
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
.START PATCH 3.9 ADDED LOGIC
               setprop sheet.PageSetup,*Orientation=xlLandscape
               setprop sheet.PageSetup,*Zoom=Zoom80
               setprop sheet.PageSetup,*TopMargin=AllMargin
               setprop sheet.PageSetup,*BottomMargin=AllMargin
               setprop sheet.PageSetup,*RightMargin=AllMargin
               setprop sheet.PageSetup,*LeftMargin=AllMargin
               //Using xlColWidth for dual purposes!!
               setprop sheet.PageSetup,*HeaderMargin=xlColWidth
               setprop sheet.PageSetup,*FooterMargin=xlColWidth
               pack    str11,"1:8"
               setprop sheet.PageSetup,*PrintTitleRows=str11
               setprop sheet.PageSetup,*PaperSize=xlPaperLegal
.END PATCH 3.9 ADDED LOGIC
               setprop        sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
               sheet.range("A1:E1").Merge
.add hyperlink   DH 11 May 2008
.          GetProp  Sheet,*Cells=Range1
          Pack      Taskname from "=Hyperlink(#"http://www.namesinthenews.com#",#"Names in the News#")"
          setprop         sheet.Range("a1:c1"),*Formula=taskname
.          pack      taskname from "http://www.namesinthenews.com"
.          Sheet.HYPERLINKS.ADD  using *Anchor=Range1, *ADDRESS=taskname
.add hyperlink in  DH 11 May 2008
               sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
               
......................................................
               setprop        sheet.Range("A8"),*Value="LR Number"
               setprop        sheet.Range("B8"),*Value="List Name"
               setprop        sheet.Range("C8"),*Value="Select"
               setprop        sheet.Range("D8"),*Value="Quantity"
               setprop        sheet.Range("E8"),*Value="All"
               setprop        sheet.Range("F8"),*Value="Actual Quantity"
               setprop        sheet.Range("G8"),*Value="Price"
               setprop        sheet.Range("H8"),*Value="Order Date"
               setprop        sheet.Range("I8"),*Value="Mail Date"
               setprop        sheet.Range("J8"),*Value="Status"
               setprop        sheet.Range("K8"),*Value="Net"
               setprop        sheet.Range("L8"),*Value="Exchange"
.begin patch 4.04
               setprop        sheet.Range("M8"),*Value="Test"
                setprop        sheet.Range("M8:M8"),*Orientation="-90"
               setprop        sheet.Range("N8"),*Value="Split Qty"
               setprop        sheet.Range("O8"),*Value="Comments"
               setprop        sheet.Range("P8"),*Value="LM-Contact"
.Following Columns will not print!!
               setprop        sheet.Range("Q8"),*Value="List Key"
               setprop        sheet.Range("R8"),*Value="Client P.O."
               setprop        sheet.Range("S8"),*Value="Return Date"
.;dh goes bad               
               setprop        sheet.Range("T8"),*Value="Sales Person ##"
               setprop        sheet.Range("U8"),*Value="Mailer"
               setprop        sheet.Range("V8"),*Value="Broker/Consultant"
.;dh goes bad               
.begin patch 4.11
               setprop        sheet.Range("W8"),*Value="Mailer List Code"
.end patch 4.11
.
               setprop xlRowHeight,VarValue="27.0"
               setprop        sheet.range("A8:T8").Rows,*RowHeight=xlRowHeight
.Header Formatting
               setprop        sheet.Range("A8:T8"),*HorizontalAlignment=xlAlignCenter
               setprop        sheet.Range("A8:T8").Font,*Bold="True"
               //Setting up 2 sets of Borders so that user is clear that
               //the second portion does not actually print
               sheet.range("A8:P8").BorderAround using *LineStyle=1,*Weight=MedThick
               sheet.range("Q8:V8").BorderAround using *LineStyle=1,*Weight=MedThick
.END PATCH 3.9 REPLACED LOGIC
.
.START PATCH 3.9 REPLACED LOGIC
.              move           C6,howmany
               move           C8,howmany
.END PATCH 3.9 REPLACED LOGIC
.              endif
.END PATCH 3.72 ADDED LOGIC
         RESET     COMMENT
          REturn
.end patch 3.92     
.begin patch 3.95
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
.                   if        (trapcount > 60)   . 5 min are you kidding me
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Nord0003 - ",str35,b1,str55
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
                    if        (pdfflag = c2)
                    goto      checkfile1
                    else          
                    goto      checkfile
                    endif
.end patch 3.95

+.........................................................................
.
         INCLUDE   NORD2IO.inc
+
.patch3.7
                              include        compio.inc
                              include        cntio.inc
.         INCLUDE   NMLRIO.inc
.patch3.7
+
         INCLUDE   NSHPIO.inc
+
         INCLUDE   NMTXIO.inc
          include   Ndatio.inc
.
         INCLUDE   NXRFIO.inc
.;begin patch 3.8
.;         include   ninvio.inc
               include        ninvio.inc
.;end patch 308
.START PATCH 3.3 - ADDED LOGIC
         INCLUDE   NOFRIO.INC
.END PATCH 3.3 - ADDED LOGIC
.START PATCH 3.6 ADDED LOGIC
              INCLUDE         NSEL2IO.INC
              include         nmodio.inc
.END PATCH 3.6 ADDED LOGIC
.START PATCH 3.9 ADDED LOGIC
              INCLUDE         NOWNIO.INC
              INCLUDE         NORD5IO.INC
.END PATCH 3.9 ADDED LOGIC
.begin patch 4.0
          include   PortCalc.inc
          include   NuseIO.inc
          include   NCntIO.inc
.end patch 4.0
         INCLUDE   COMLOGIC.inc