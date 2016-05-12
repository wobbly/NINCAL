PC       EQU       0
         INC       COMMON.inc
         INC       CONS.inc
         include   winapi.inc
         include   ndat3dd.inc
         include   ndatdd.inc
         INCLUDE              NPRJDD.INC
         include              Nrevdd.inc
;patch1.1
                              include        compdd.inc
                              include        cntdd.inc
.         include   nmlrdd.inc
;patch1.1
         include   nxrfdd.inc
              include         compnotesdd.inc
              INCLUDE         OSLSPERN.INC.............................................................................
................................................
release 	init    "1.4"		08Mar2007  DLH	Oslspern.inc expansion
.release  init      "1.3"       19MAY2005 ASH LISTMLR Conversion
.release  init      "1.2"       17NOV2004    Mailer Conversion Completion
.release  init      "1.1"       26MAY2004    Mailer Conversion
.release  init      "1.0"       03October2000 DLH first implementation.
input    file
output   ifile     keylen=8,fix=692,NODUPLICATES
input2   file
key15    dim       15   *FILE KEY.
key8     dim       8
key6     dim       6
PRJStr6       Dim             6
ANS      DIM       1
TIPE     DIM       1
CHECK    FORM      1
SOURCE   DIM       1
M        INIT      "M"
B        INIT      "B"
R        INIT      "R"
owner    dim       4
INCNAME  DIM       45        11-55
ARTOT    FORM      10.2        56-68
APTOT    FORM      10.2        69-81
LRTOT    FORM      10.2        82-94
NINTOT    FORM      10.2       95-107
QTYTOT   FORM      8          108-115
ADJARTOT FORM      10.2       116-128
ADJAPTOT FORM      10.2       129-141
ADJLRTOT FORM      10.2       142-154
ADJNINTOT FORM      10.2      155-167
unbill   form      8.2        168-190
unbilinc form      8.2       110-120
.
LRcalc   FORM      10.2
NINcalc  FORM      10.2
.output record
.byte 1 nothing
.source   dim      1                2-2
.ID       dim      6                3-8
.name     dim      45               9-53
.owner    dim      4               54-57
jan1     FORM      10.2            58-70
feb1     FORM      10.2
mar1     FORM      10.2
apr1     FORM      10.2
may1     FORM      10.2
jun1     FORM      10.2
jul1     FORM      10.2
aug1     FORM      10.2
sep1     FORM      10.2
oct1     FORM      10.2
nov1     FORM      10.2
dec1     FORM      10.2
janNIN1  FORM      10.2
febNIN1  FORM      10.2
marNIN1  FORM      10.2
aprNIN1  FORM      10.2
mayNIN1  FORM      10.2
junNIN1  FORM      10.2
julNIN1  FORM      10.2
augNIN1  FORM      10.2
sepNIN1  FORM      10.2
octNIN1  FORM      10.2
novNIN1  FORM      10.2
decNIN1  FORM      10.2
janx1    FORM      10.2           308-
febx1    FORM      10.2
marx1    FORM      10.2
aprx1    FORM      10.2
mayx1    FORM      10.2
junx1    FORM      10.2
julx1    FORM      10.2
augx1    FORM      10.2
sepx1    FORM      10.2
octx1    FORM      10.2
novx1    FORM      10.2
decx1    FORM      10.2
janNINx1 FORM      10.2
febNINx1 FORM      10.2
marNINx1 FORM      10.2
aprNINx1 FORM      10.2
mayNINx1 FORM      10.2
junNINx1 FORM      10.2
julNINx1 FORM      10.2
augNINx1 FORM      10.2
sepNINx1 FORM      10.2
octNINx1 FORM      10.2
novNINx1 FORM      10.2
decNINx1 FORM      10.2               681
;unbilled form      8.2            682-692

Total    form      10.2
RTotal   form      10.2
ETotal   form      10.2
NINTotal    form      10.2
RNINTotal   form      10.2
ENINTotal   form      10.2
Mpercent form      10.4
rpercent form      10.4
Epercent form      10.4
NINperc  form      10.4
percent  dim       16
Yr2003NIN      form           10.2
Yr2003LR       form           10.2
Yr2002NIN      form           10.2
Yr2002LR       form           10.2
Yr2001NIN      form           10.2
Yr2001LR       form           10.2
Yr2003NINR      form           10.2
Yr2003LRR       form           10.2
Yr2003NINE      form           10.2
Yr2003LRE       form           10.2
....................................................................
.
.hold vars
Proj2001  form      11
NIN2001   form      11
projnew   form      11          hold exchange portion  LR
NINnew    form      11          hold exchange portion NIN
.......................................................................
.SORTVAR       INIT              "c:\data\average7.dat,c:\data\average7.srt;2,9-53"
SORTVAR        INIT              "c:\work\average7.dat,c:\work\average7.srt;2,9-53"
countout form      5
sheetno    form      2
Hmany1   form      9
Hmany2   form      9
Hmany3   form      9
Hmany4   form      9
Hmany5   form      9
Hmany6   form      9
Hmany7   form      9
Hmany8   form      9
NumberofSheets Integer        4,"0x00000000"
endRow   form      9
RowNumber     Dim             9
.............................................................................................
.some excel goodies
books   automation
book    automation
sheets  automation
sheet   automation
Rowcol  automation
ex      automation      class="Excel.Application"
RecordHeader form 9
RecordTop form  9
N34     form    3.4
N92     form    9.2
MailDate dim    4
.hexeight integer 4,"4294967295"
VT_BOOL EQU 11          .Boolean
OTRUE   variant
OFALSE  variant
VT_I4   EQU 3           .4 byte integer
Zoom95  variant
VT_R8   EQU 5           .Double - 8 byte Real
Inch25  variant         ..25 Inch
Inch50  variant         ..50 Inch
Inch100 variant         .1.0 Inch
.Formatting vars needed
.This constant was found in the Object BRowser in Excel under the Help topic for the
.HorizontalAlignment property of the Range object.
xlHAlignLeft integer 4,"0xffffefdd"             .-4131
xlHAlignCenter integer 4,"0xffffeff4"           .-4108
xlEdgeTop integer 4,"0x8"                       .8
xlEdgeBottom integer 4,"0x9"                    .9
xlEdgeRight integer 4,"0xA"                     .10
xlContinuous integer 4,"0x1"                    .1 - Doubles for xlPaperLetter, xlDownThenOver
xlDouble integer 4,"0xffffefe9"                 .-4119
xlLandscape integer 4,"0x2"                     .2
xlPrintNoComments integer 4,"0xffffefd2"        .-4142
.Create the Variant objects
.Booleans
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
        create  Zoom95,VarType=VT_I4,VarValue=95
.Convert Inches to Points.  There are 72 Points to an Inch
        create  Inch25,VarType=VT_R8,VarValue="18"      ..25 Inch
        create  Inch50,VarType=VT_R8,VarValue="36"      ..5 Inch
        create  Inch100,VarType=VT_R8,VarValue="72"     .1.0 Inch
.............................................................................................
.end some excel goodies

.         goto      pass2

.         open      input,"c:\data\income7.dat",exclusive
.         prepare   output,"c:\data\average7.dat","c:\data\average7.isi","8","692",exclusive
         move     c2 to NXRFPATH         .set which key to use
.START PATCH 1.2 REMOVED LOGIC
.         move     c1 to nmlrpath         .set which key to use
.         move     c3 to nmlrlock         .turn off file locking
.END PATCH 1.2 REMOVED LOGIC
         move     c1 to nDATpath         .set which key to use
         move     c3 to nDATlock         .turn off file locking
.income7 currenlyt holds last 12 months income, adjustment, and current REVunbld info for
.exclususive clients & lists
         open      input,"c:\work\income7.dat",exclusive
.output the above info will be compiled into the form I want it
         prepare   output,"c:\work\average7.dat","c:\work\average7.isi","8","692",exclusive
;.projdolr - previous projections
;         open      projdolr,"c:\work\projdolr"
input
         READ      inPUT,seq;tipe,source,key6,cc,yy,mm,ans,incname,owner,ARTOT,APTOT,LRTOT,NINTOT,QTYTOT:
                   ADJARTOT,ADJAPTOT,ADJLRTOT,adjnintot,unbill
         goto      sortit if over

         if        (key6 = "000055")
         call      debug
         endif

         if        (source = M)
         move      b1 to tipe
         endif

         packkey      key8 from b1,source,key6
.         pack      key8 from b1,source,key6

         read      output,key8;key8,incname,owner,jan1,feb1,mar1,apr1,may1,jun1,jul1,aug1,sep1,oct1,nov1,dec1:
                 janNIN1,febNIN1,marNIN1,aprNIN1,mayNIN1,junNIN1,julNIN1,augNIN1,sepNIN1,octNIN1,novNIN1,decNIN1:
                  janx1,febx1,marx1,aprx1,mayx1,junx1,julx1,augx1,sepx1,octx1,novx1,decx1:
                 janNINx1,febNINx1,marNINx1,aprNINx1,mayNINx1,junNINx1,julNINx1,augNINx1,sepNINx1,octNINx1,novNINx1,decNINx1:
                   REVunbld
       goto      write if over

.         add       unbill to REVunbld

         move      mm to n2
         if        (tipe = b1 or tipe = "R")
         load      LRcalc from n2 of jan1,feb1,mar1,apr1,may1,jun1,jul1,aug1,sep1,oct1,nov1,dec1
         add       lrtot to lrcalc
         add       adjlrtot to lrcalc
         store     LRcalc into n2 of jan1,feb1,mar1,apr1,may1,jun1,jul1,aug1,sep1,oct1,nov1,dec1

         load      NINcalc from n2 of janNIN1,febNIN1,marNIN1,aprNIN1,mayNIN1,junNIN1,julNIN1,augNIN1,sepNIN1,octNIN1,novNIN1,decNIN1
         add       NINtot to NINcalc
         add       adjNINtot to NINcalc
         store     NINcalc into n2 of janNIN1,febNIN1,marNIN1,aprNIN1,mayNIN1,junNIN1,julNIN1,augNIN1,sepNIN1,octNIN1,novNIN1,decNIN1
         endif
         if        (tipe = "E")
         load      LRcalc from n2 of janx1,febx1,marx1,aprx1,mayx1,junx1,julx1,augx1,sepx1,octx1,novx1,decx1
         add       lrtot to lrcalc
         add       adjlrtot to lrcalc
         store     LRcalc into n2 of janx1,febx1,marx1,aprx1,mayx1,junx1,julx1,augx1,sepx1,octx1,novx1,decx1

         load      NINcalc from n2 of janNINx1,febNINx1,marNINx1,aprNINx1,mayNINx1,junNINx1,julNINx1,augNINx1,sepNINx1,octNINx1,novNINx1,decNINx1
         add       NINtot to NINcalc
         add       adjNINtot to NINcalc
         store     NINcalc into n2 of janNINx1,febNINx1,marNINx1,aprNINx1,mayNINx1,junNINx1,julNINx1,augNINx1,sepNINx1,octNINx1,novNINx1,decNINx1
         endif
.????? problem with update on my machine at home ?????? so delete and write
.on my system record was written instead of updated, resulting in multiple records with the same key
         read      output,key8;;
         delete    output,key8
.

         goto      write1
write
         move      mm to n2
         move       c0 to lrcalc
         add       lrtot to lrcalc
         add       adjlrtot to lrcalc
         move      c0 to nincalc
         add       NINtot to NINcalc
         add       adjNINtot to NINcalc
.
         if        (tipe = b1 or tipe = "R")
         store     LRcalc into n2 of jan1,feb1,mar1,apr1,may1,jun1,jul1,aug1,sep1,oct1,nov1,dec1
         store     NINcalc into n2 of janNIN1,febNIN1,marNIN1,aprNIN1,mayNIN1,junNIN1,julNIN1,augNIN1,sepNIN1,octNIN1,novNIN1,decNIN1
         endif
         if        (tipe = "E")
         store     LRcalc into n2 of janx1,febx1,marx1,aprx1,mayx1,junx1,julx1,augx1,sepx1,octx1,novx1,decx1
         store     NINcalc into n2 of janNINx1,febNINx1,marNINx1,aprNINx1,mayNINx1,junNINx1,julNINx1,augNINx1,sepNINx1,octNINx1,novNINx1,decNINx1
         endif
.
         pack      key8 from B1,source,key6

write1
         add       unbill to REVunbld
         if        (source = "B")
.START PATCH 1.2 REPLACED LOGIC
.         unpack    key8 into str2,str2,str4
.         pack      mkey from str4,z3
.         call      nmlrkey
.         if        not over
.         move      mcomp to incname
..................................
         unpack    key8 into str2,PrjStr6
         pack      COMPFLD,PrjStr6
         call      COMPKEY
         if        not over
         move      COMPCOMP to incname
.END PATCH 1.2 REPLACED LOGIC
         endif
         endif
         if        (source = "M")
         unpack    key8 into str2,PrjStr6
         pack      ndatfld from PrjStr6
         call      ndatkey
         if        not over
         move      olstname to incname
         endif
         endif
               write     output,key8;key8,incname,owner,jan1,feb1,mar1,apr1,may1,jun1,jul1,aug1,sep1,oct1,nov1,dec1:
                 janNIN1,febNIN1,marNIN1,aprNIN1,mayNIN1,junNIN1,julNIN1,augNIN1,sepNIN1,octNIN1,novNIN1,decNIN1:
                  janx1,febx1,marx1,aprx1,mayx1,junx1,julx1,augx1,sepx1,octx1,novx1,decx1:
                 janNINx1,febNINx1,marNINx1,aprNINx1,mayNINx1,junNINx1,julNINx1,augNINx1,sepNINx1,octNINx1,novNINx1,decNINx1:
                   REVunbld
         move      c0 to REVunbld
         goto      input
.sortit
sortit  close     output
              SORT            SORTVAR
              IF OVER
              DISPLAY         *N,"Sort ERROR ",S$ERROR$;
        keyin   str1
              ENDIF
.......................................................................................
.pass2
.......................................................................................
.create workbook with eight worksheets and populate with data and formula's
pass2
.        open      input2,"c:\data\average7.srt",exclusive
         open      input2,"c:\work\average7.srt",exclusive
.Open Excel application
        create  ex
.Reset Default of Worksheets found in a Workbook         .eight
.should try getting the property here and reseting it when done.
        getprop ex,*SheetsInNewWorkbook=NumberofSheets
        setprop ex,*SheetsInNewWorkbook=C8
.Create Workbooks collection
        getprop ex,*Workbooks=books
.Create/Add a single Workbook
        books.add
        books.item giving book using 1
.Create Worksheets collection
        getprop book,*Sheets=sheets
.Create a Worksheet(s) - we did not need to add it as we set the default above to
.add 8 new Worksheets each time a Workbook is created.
sheetsetup
        sheets.item giving sheet using 1
        setprop sheet,*Name="Brokerage LR Totals"
        setprop sheet.PageSetup,*Orientation=xlLandscape
        setprop sheet.PageSetup,*CenterFooter=" Page &P"
        setprop sheet.PageSetup,*PrintComments=xlPrintNoComments
        setprop sheet.PageSetup,*Draft=OFALSE
        setprop sheet.PageSetup,*PaperSize=xlContinuous
        setprop sheet.PageSetup,*Order=xlContinuous
        setprop sheet.PageSetup,*BlackAndWhite=OFALSE
        setprop sheet.PageSetup,*Zoom=Zoom95
        setprop sheet.PageSetup,*LeftMargin=Inch25
        setprop sheet.PageSetup,*RightMargin=Inch25
        setprop sheet.PageSetup,*TopMargin=Inch100
        setprop sheet.PageSetup,*BottomMargin=Inch100
        setprop sheet.PageSetup,*HeaderMargin=Inch50
        setprop sheet.PageSetup,*FooterMargin=Inch50
.        add        c1 to n1
.        compare    c9 to n1.
.        repeat     until equal
        sheets.item giving sheet using 2
        setprop sheet,*Name="Brokerage LR Rent"
        setprop sheet.PageSetup,*Orientation=xlLandscape
        setprop sheet.PageSetup,*CenterFooter=" Page &P"
        setprop sheet.PageSetup,*PrintComments=xlPrintNoComments
        setprop sheet.PageSetup,*Draft=OFALSE
        setprop sheet.PageSetup,*PaperSize=xlContinuous
        setprop sheet.PageSetup,*Order=xlContinuous
        setprop sheet.PageSetup,*BlackAndWhite=OFALSE
        setprop sheet.PageSetup,*Zoom=Zoom95
        setprop sheet.PageSetup,*LeftMargin=Inch25
        setprop sheet.PageSetup,*RightMargin=Inch25
        setprop sheet.PageSetup,*TopMargin=Inch100
        setprop sheet.PageSetup,*BottomMargin=Inch100
        setprop sheet.PageSetup,*HeaderMargin=Inch50
        setprop sheet.PageSetup,*FooterMargin=Inch50
        sheets.item giving sheet using 3
        setprop sheet,*Name="Brokerage LR Exch"
        setprop sheet.PageSetup,*Orientation=xlLandscape
        setprop sheet.PageSetup,*CenterFooter=" Page &P"
        setprop sheet.PageSetup,*PrintComments=xlPrintNoComments
        setprop sheet.PageSetup,*Draft=OFALSE
        setprop sheet.PageSetup,*PaperSize=xlContinuous
        setprop sheet.PageSetup,*Order=xlContinuous
        setprop sheet.PageSetup,*BlackAndWhite=OFALSE
        setprop sheet.PageSetup,*Zoom=Zoom95
        setprop sheet.PageSetup,*LeftMargin=Inch25
        setprop sheet.PageSetup,*RightMargin=Inch25
        setprop sheet.PageSetup,*TopMargin=Inch100
        setprop sheet.PageSetup,*BottomMargin=Inch100
        setprop sheet.PageSetup,*HeaderMargin=Inch50
        setprop sheet.PageSetup,*FooterMargin=Inch50
        sheets.item giving sheet using 4
        setprop sheet,*Name="List Management LR"
        setprop sheet.PageSetup,*Orientation=xlLandscape
        setprop sheet.PageSetup,*CenterFooter=" Page &P"
        setprop sheet.PageSetup,*PrintComments=xlPrintNoComments
        setprop sheet.PageSetup,*Draft=OFALSE
        setprop sheet.PageSetup,*PaperSize=xlContinuous
        setprop sheet.PageSetup,*Order=xlContinuous
        setprop sheet.PageSetup,*BlackAndWhite=OFALSE
        setprop sheet.PageSetup,*Zoom=Zoom95
        setprop sheet.PageSetup,*LeftMargin=Inch25
        setprop sheet.PageSetup,*RightMargin=Inch25
        setprop sheet.PageSetup,*TopMargin=Inch100
        setprop sheet.PageSetup,*BottomMargin=Inch100
        setprop sheet.PageSetup,*HeaderMargin=Inch50
        setprop sheet.PageSetup,*FooterMargin=Inch50
        sheets.item giving sheet using 5
        setprop sheet,*Name="Total NIN Income"
        setprop sheet.PageSetup,*Orientation=xlLandscape
        setprop sheet.PageSetup,*CenterFooter=" Page &P"
        setprop sheet.PageSetup,*PrintComments=xlPrintNoComments
        setprop sheet.PageSetup,*Draft=OFALSE
        setprop sheet.PageSetup,*PaperSize=xlContinuous
        setprop sheet.PageSetup,*Order=xlContinuous
        setprop sheet.PageSetup,*BlackAndWhite=OFALSE
        setprop sheet.PageSetup,*Zoom=Zoom95
        setprop sheet.PageSetup,*LeftMargin=Inch25
        setprop sheet.PageSetup,*RightMargin=Inch25
        setprop sheet.PageSetup,*TopMargin=Inch100
        setprop sheet.PageSetup,*BottomMargin=Inch100
        setprop sheet.PageSetup,*HeaderMargin=Inch50
        setprop sheet.PageSetup,*FooterMargin=Inch50
        sheets.item giving sheet using 6
        setprop sheet,*Name="Brokerage NIN Rent"
        setprop sheet.PageSetup,*Orientation=xlLandscape
        setprop sheet.PageSetup,*CenterFooter=" Page &P"
        setprop sheet.PageSetup,*PrintComments=xlPrintNoComments
        setprop sheet.PageSetup,*Draft=OFALSE
        setprop sheet.PageSetup,*PaperSize=xlContinuous
        setprop sheet.PageSetup,*Order=xlContinuous
        setprop sheet.PageSetup,*BlackAndWhite=OFALSE
        setprop sheet.PageSetup,*Zoom=Zoom95
        setprop sheet.PageSetup,*LeftMargin=Inch25
        setprop sheet.PageSetup,*RightMargin=Inch25
        setprop sheet.PageSetup,*TopMargin=Inch100
        setprop sheet.PageSetup,*BottomMargin=Inch100
        setprop sheet.PageSetup,*HeaderMargin=Inch50
        setprop sheet.PageSetup,*FooterMargin=Inch50
        sheets.item giving sheet using 7
        setprop sheet,*Name="Brokerage NIN Exch"
        setprop sheet.PageSetup,*Orientation=xlLandscape
        setprop sheet.PageSetup,*CenterFooter=" Page &P"
        setprop sheet.PageSetup,*PrintComments=xlPrintNoComments
        setprop sheet.PageSetup,*Draft=OFALSE
        setprop sheet.PageSetup,*PaperSize=xlContinuous
        setprop sheet.PageSetup,*Order=xlContinuous
        setprop sheet.PageSetup,*BlackAndWhite=OFALSE
        setprop sheet.PageSetup,*Zoom=Zoom95
        setprop sheet.PageSetup,*LeftMargin=Inch25
        setprop sheet.PageSetup,*RightMargin=Inch25
        setprop sheet.PageSetup,*TopMargin=Inch100
        setprop sheet.PageSetup,*BottomMargin=Inch100
        setprop sheet.PageSetup,*HeaderMargin=Inch50
        setprop sheet.PageSetup,*FooterMargin=Inch50
        sheets.item giving sheet using 8
        setprop sheet,*Name="List Management NIN"
        setprop sheet.PageSetup,*Orientation=xlLandscape
        setprop sheet.PageSetup,*CenterFooter=" Page &P"
        setprop sheet.PageSetup,*PrintComments=xlPrintNoComments
        setprop sheet.PageSetup,*Draft=OFALSE
        setprop sheet.PageSetup,*PaperSize=xlContinuous
        setprop sheet.PageSetup,*Order=xlContinuous
        setprop sheet.PageSetup,*BlackAndWhite=OFALSE
        setprop sheet.PageSetup,*Zoom=Zoom95
        setprop sheet.PageSetup,*LeftMargin=Inch25
        setprop sheet.PageSetup,*RightMargin=Inch25
        setprop sheet.PageSetup,*TopMargin=Inch100
        setprop sheet.PageSetup,*BottomMargin=Inch100
        setprop sheet.PageSetup,*HeaderMargin=Inch50
        setprop sheet.PageSetup,*FooterMargin=Inch50

        call      hdout
.Reset Default of Worksheets to users original default
booger2
        setprop ex,*SheetsInNewWorkbook=NumberofSheets

looper2  add      c1 to countout
        display   *p20:12,"Populating spreadsheets record number ",countout
              read      input2,seq;key8,incname,owner,jan1,feb1,mar1,apr1,may1,jun1,jul1,aug1,sep1,oct1,nov1,dec1:
                 janNIN1,febNIN1,marNIN1,aprNIN1,mayNIN1,junNIN1,julNIN1,augNIN1,sepNIN1,octNIN1,novNIN1,decNIN1:
                  janx1,febx1,marx1,aprx1,mayx1,junx1,julx1,augx1,sepx1,octx1,novx1,decx1:
                 janNINx1,febNINx1,marNINx1,aprNINx1,mayNINx1,junNINx1,julNINx1,augNINx1,sepNINx1,octNINx1,novNINx1,decNINx1:
                   REVunbld
        goto      eoj if over
detout
        unpack    key8 into str1,source,PrjStr6
.
..........................................................................................
        move      c0 to proj2001
        move      c0 to nin2001
        move      c0 to projnew
        move      c0 to ninnew
               if        (source = "B")
               move      "EB" to str2
               pack      Nprjfld from str2,PrjStr6,"200401"
               rep       zfill in Nprjfld
               CALL      NPRJKEY
                              iF             NOT OVER
                              MOVE           Projlr to projnew
                              move           projnin to ninnew
                            endif
               Move           c0 to Yr2003LRE
               move           c0 to Yr2003NINE
.START PATCH 1.2 REPLACED LOGIC - TEMPORARY PATCH
.               packkey        NREVFLD from str2,PrjStr6,"2003"
               pack                 COMPFLD,PrjStr6
               call           COMPKEY
               packkey        NREVFLD from str2,"00",COMPOLDMLR,"2003"
.END PATCH 1.2 REPLACED LOGIC
               call           Nrevkey
               if             Not over
               Calc           Yr2003LRE=(JANlr+Feblr+marLR+Aprlr+maylr+junlr+jullr+augLR+seplr+octlr+novlr+declr)
               calc           Yr2003NINE=(JANNIN+FebNIN+marNIN+AprNIN+mayNIN+junNIN+julNIN+augNIN+sepNIN+octNIN+novNIN+decNIN)
               endif
.........
               move      "RB" to str2
               pack      Nprjfld from str2,PrjStr6,"200401"
               rep       zfill in Nprjfld
;        read      projdolr,Nprjfld;projvars
               CALL      NPRJKEY
                              iF             NOT OVER
                              MOVE           Projlr to proj2001
                              move           projnin to NIN2001
                              endif
               Move           c0 to Yr2003LRr
               move           c0 to Yr2003NINr
.START PATCH 1.2 REPLACED LOGIC - TEMPORARY PATCH
.               packkey        NREVFLD from str2,PrjStr6,"2003"
               pack                 COMPFLD,PrjStr6
               call           COMPKEY
               packkey        NREVFLD from str2,"00",COMPOLDMLR,"2003"
.END PATCH 1.2 REPLACED LOGIC - TEMPORARY PATCH
               call           Nrevkey
               if             Not over
               Calc           Yr2003LRr=(JANlr+Feblr+marLR+Aprlr+maylr+junlr+jullr+augLR+seplr+octlr+novlr+declr)
               calc           Yr2003NINr=(JANNIN+FebNIN+marNIN+AprNIN+mayNIN+junNIN+julNIN+augNIN+sepNIN+octNIN+novNIN+decNIN)
               endif
               endif
................................................................................
        if        (source = "M")
        move      " M" to str2
        pack      Nprjfld from str2,PrjStr6,"200401"
;        read      projdolr,Nprjfld;projvars
         CALL      NPRJKEY
         iF             NOT OVER
          MOVE           Projlr to proj2001
         move           projnin to NIN2001
        endif
               Move           c0 to Yr2003LR
               move           c0 to Yr2003NIN
               Move           c0 to Yr2002LR
               move           c0 to Yr2002NIN
               Move           c0 to Yr2001LR
               move           c0 to Yr2001NIN
.START PATCH 1.2 REPLACED LOGIC - TEMPORARY PATCH
.               packkey        NREVFLD from str2,PrjStr6,"2003"
               pack                 COMPFLD,PrjStr6
               call           COMPKEY
               packkey        NREVFLD from str2,"00",COMPOLDMLR,"2003"
.END PATCH 1.2 REPLACED LOGIC - TEMPORARY PATCH
               call           Nrevkey
               if             Not over
               Calc           Yr2003LR=(JANlr+Feblr+marLR+Aprlr+maylr+junlr+jullr+augLR+seplr+octlr+novlr+declr)
               calc           Yr2003NIN=(JANNIN+FebNIN+marNIN+AprNIN+mayNIN+junNIN+julNIN+augNIN+sepNIN+octNIN+novNIN+decNIN)
               endif
.START PATCH 1.2 REPLACED LOGIC - TEMPORARY PATCH
.               packkey        NREVFLD from str2,PrjStr6,"2002"
               packkey        NREVFLD from str2,"00",COMPOLDMLR,"2002"
.END PATCH 1.2 REPLACED LOGIC - TEMPORARY PATCH
               call           Nrevkey
               if             Not over
               Calc           Yr2002LR=(JANlr+Feblr+marLR+Aprlr+maylr+junlr+jullr+augLR+seplr+octlr+novlr+declr)
               calc           Yr2002NIN=(JANNIN+FebNIN+marNIN+AprNIN+mayNIN+junNIN+julNIN+augNIN+sepNIN+octNIN+novNIN+decNIN)
               endif
.START PATCH 1.2 REPLACED LOGIC - TEMPORARY PATCH
.               packkey        NREVFLD from str2,PrjStr6,"2001"
               packkey        NREVFLD from str2,"00",COMPOLDMLR,"2001"
.END PATCH 1.2 REPLACED LOGIC - TEMPORARY PATCH
               call           Nrevkey
               if             Not over
               Calc           Yr2001LR=(JANlr+Feblr+marLR+Aprlr+maylr+junlr+jullr+augLR+seplr+octlr+novlr+declr)
               calc           Yr2001NIN=(JANNIN+FebNIN+marNIN+AprNIN+mayNIN+junNIN+julNIN+augNIN+sepNIN+octNIN+novNIN+decNIN)
               endif
        endif
.................................................................................
        if        (source = "B")
        clear     ndattdmc
        clear     NXRFLIST
.START PATCH 1.2 REPLACED LOGIC
.        unpack    PrjStr6 into str2,RowNumber
.        pack      mkey from RowNumber,z3
.        rep       zfill in mkey
.        call      nmlrkey
.        if        not over
.        move      mcomp to incname
.        endif
.        move      RowNumber to nxrffld2
...............................
        pack      COMPFLD,PrjStr6
        rep       zfill in COMPFLD
        call      COMPKEY
        if        not over
                        move      COMPCOMP to incname
              endif
.START PATCH 1.3 REPLACED LOGIC
..Temporary logic until Cross Ref file is converted!!
.        move      COMPOLDMLR to nxrffld2
        move      PrjStr6,nxrffld2
.END PATCH 1.3 REPLACED LOGIC
.END PATCH 1.2 REPLACED LOGIC
        call      nxrfkey
        move      NXRFLIST to ndat3fld
        call      ndat3key
.start with totals sheet for LR income
        display   *p20:13,"Populating spreadsheet",*el," Lr income totals "
        move      c1 to sheetno
        sheets.item giving sheet using 1
        load      howmany from sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
        add     C1,howmany
        store    howmany to sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value=PrjStr6
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"B",str9
        setprop sheet.range(RowNumber),*Value=incname
.Jan total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"C",str9
              clear   taskname
        append  "=SUM('Brokerage LR Rent'!C" to taskname
        append   str9 to taskname
        append  "+'Brokerage LR Exch'!C" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.feb total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"D",str9
              clear   taskname
        append  "=SUM('Brokerage LR Rent'!D" to taskname
        append   str9 to taskname
        append  "+'Brokerage LR Exch'!D" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.mar total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"E",str9
              clear   taskname
        append  "=SUM('Brokerage LR Rent'!E" to taskname
        append   str9 to taskname
        append  "+'Brokerage LR Exch'!E" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Apr total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"F",str9
              clear   taskname
        append  "=SUM('Brokerage LR Rent'!F" to taskname
        append   str9 to taskname
        append  "+'Brokerage LR Exch'!F" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.May total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"G",str9
              clear   taskname
        append  "=SUM('Brokerage LR Rent'!G" to taskname
        append   str9 to taskname
        append  "+'Brokerage LR Exch'!G" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.JUne total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"H",str9
              clear   taskname
        append  "=SUM('Brokerage LR Rent'!H" to taskname
        append   str9 to taskname
        append  "+'Brokerage LR Exch'!H" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.July total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"I",str9
              clear   taskname
        append  "=SUM('Brokerage LR Rent'!I" to taskname
        append   str9 to taskname
        append  "+'Brokerage LR Exch'!I" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Aug total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"J",str9
              clear   taskname
        append  "=SUM('Brokerage LR Rent'!J" to taskname
        append   str9 to taskname
        append  "+'Brokerage LR Exch'!J" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Sep total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"K",str9
              clear   taskname
        append  "=SUM('Brokerage LR Rent'!K" to taskname
        append   str9 to taskname
        append  "+'Brokerage LR Exch'!K" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Oct total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"L",str9
              clear   taskname
        append  "=SUM('Brokerage LR Rent'!L" to taskname
        append   str9 to taskname
        append  "+'Brokerage LR Exch'!L" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Nov total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"M",str9
              clear   taskname
        append  "=SUM('Brokerage LR Rent'!M" to taskname
        append   str9 to taskname
        append  "+'Brokerage LR Exch'!M" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Dec total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"N",str9
              clear   taskname
        append  "=SUM('Brokerage LR Rent'!N" to taskname
        append   str9 to taskname
        append  "+'Brokerage LR Exch'!N" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.total total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"O",str9
        move    c0 to total
        move    c0 to rtotal
        move    c0 to etotal
        add     jan1 to rtotal
        add     feb1 to rtotal
        add     mar1 to rtotal
        add     apr1 to rtotal
        add     may1 to rtotal
        add     jun1 to rtotal
        add     jul1 to rtotal
        add     aug1 to rtotal
        add     sep1 to rtotal
        add     oct1 to rtotal
        add     nov1 to rtotal
        add     dec1 to rtotal
        add     janx1 to etotal
        add     febx1 to etotal
        add     marx1 to etotal
        add     aprx1 to etotal
        add     mayx1 to etotal
        add     junx1 to etotal
        add     julx1 to etotal
        add     augx1 to etotal
        add     sepx1 to etotal
        add     octx1 to etotal
        add     novx1 to etotal
        add     decx1 to etotal
        add     etotal to total
        add     rtotal to total
bugger
        setprop sheet.range(RowNumber),*Value=total
.........................................................................................
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"P",str9
              clear   taskname
        append  "=SUM('Brokerage LR Rent'!P" to taskname
        append   str9 to taskname
        append  "+'Brokerage LR Exch'!P" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"

.
.        setprop sheet.range(RowNumber),*Value="0.00"
.salesperson  - add code to pull
              clear           str25
              Move            COMPCONTACT to n2
               load           str25 from n2 of osls1,osls2,osls3,osls4,osls5,osls6,osls7:
                              osls8,osls9,osls10,osls11,osls12,osls13,osls14,osls15,osls16:
                              osls17,osls18,osls19,osls20,osls21,osls22,osls23,osls24,osls25:
		      osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35

        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"Q",str9
        setprop sheet.range(RowNumber),*Value=str25
.REVunbld
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"R",str9
        setprop sheet.range(RowNumber),*Value=REVunbld,*NumberFormat="##,####0_);[Red](##,####0)"
........................
.ok lets do sheet 2
........................
.brokerage rental
        display   *p20:13,"Populating spreadsheet",*el," Lr rental income totals "
        move      c2 to sheetno
        sheets.item giving sheet using 2
        load      howmany from sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
        add     C1,howmany
        store    howmany to sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value=PrjStr6
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"B",str9
        setprop sheet.range(RowNumber),*Value=incname
.calculate rent %
        move    c0 to rpercent
        move    rtotal to rpercent
        divide  total into rpercent
.Jan rental portion
        move     c0 to mpercent
        move    jan1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"C",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Feb rental portion
        move     c0 to mpercent
        move    feb1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"D",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.mar rental portion
        move     c0 to mpercent
        move    mar1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"E",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Apr rental portion
        move     c0 to mpercent
        move    apr1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"F",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.May rental portion
        move     c0 to mpercent
        move    may1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"G",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jun rental portion
        move     c0 to mpercent
        move    jun1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"H",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jul rental portion
        move     c0 to mpercent
        move    jul1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"I",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Aug rental portion
        move     c0 to mpercent
        move    Aug1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"J",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Sep rental portion
        move     c0 to mpercent
        move    sep1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"K",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Oct rental portion
        move     c0 to mpercent
        move    Oct1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"L",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Nov rental portion
        move     c0 to mpercent
        move    nov1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"M",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Dec rental portion
        move     c0 to mpercent
        move    dec1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"N",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.rtotal total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"O",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    rpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*'Brokerage LR Totals'!O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.lastyear  - add code to pull
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"P",str9
        setprop sheet.range(RowNumber),*Value=proj2001,*NumberFormat="##,####0_);[Red](##,####0)"
.salesperson  - add code to pull
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"Q",str9
        setprop sheet.range(RowNumber),*Value=b1
. Prev year lr -
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"T",str9
        setprop sheet.range(RowNumber),*Value=Yr2003LRR,*NumberFormat="##,####0_);[Red](##,####0)"

.end of sheet 2 rental portion
.ok lets do sheet 3
........................
.brokerage exchange
        display   *p20:13,"Populating spreadsheet",*el," Lr Exchange income totals "
        move      c3 to sheetno
        sheets.item giving sheet using 3
        load      howmany from sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
        add     C1,howmany
        store    howmany to sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value=PrjStr6
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"B",str9
        setprop sheet.range(RowNumber),*Value=incname
.calculate exchange %
        move    c0 to rpercent
        move    etotal to epercent
        divide  total into epercent
.Jan exchange portion
        move     c0 to mpercent
        move    janx1 to mpercent
        divide  Etotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"C",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Feb exchange portion
        move     c0 to mpercent
        move    febx1 to mpercent
        divide  Etotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"D",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.mar exchange portion
        move     c0 to mpercent
        move    marx1 to mpercent
        divide  Etotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"E",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Apr exchange portion
        move     c0 to mpercent
        move    aprx1 to mpercent
        divide  Etotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"F",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.May exchange portion
        move     c0 to mpercent
        move    mayx1 to mpercent
        divide  Etotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"G",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jun exchange portion
        move     c0 to mpercent
        move    junx1 to mpercent
        divide  Etotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"H",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jul exchange portion
        move     c0 to mpercent
        move    julx1 to mpercent
        divide  Etotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"I",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Aug exchange portion
        move     c0 to mpercent
        move    Augx1 to mpercent
        divide  Etotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"J",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Sep exchange portion
        move     c0 to mpercent
        move    sepx1 to mpercent
        divide  Etotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"K",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Oct exchange portion
        move     c0 to mpercent
        move    Octx1 to mpercent
        divide  Etotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"L",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Nov exchange portion
        move     c0 to mpercent
        move    novx1 to mpercent
        divide  Etotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"M",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Dec exchange portion
        move     c0 to mpercent
        move    decx1 to mpercent
        divide  Etotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"N",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Etotal total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"O",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    Epercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*'Brokerage LR Totals'!O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.lastyear  - add code to pull
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"P",str9
        setprop sheet.range(RowNumber),*Value=projnew,*NumberFormat="##,####0_);[Red](##,####0)"
.salesperson  - add code to pull
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"Q",str9
        setprop sheet.range(RowNumber),*Value=b1
. Prev year lr -
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"T",str9
        setprop sheet.range(RowNumber),*Value=Yr2003LRE,*NumberFormat="##,####0_);[Red](##,####0)"
.end of sheet 3 exchange portion
.............................
.ok lets do sheet 5 & 6 & 7
        move    c0 to NINtotal
        move    c0 to rNINtotal
        move    c0 to eNINtotal
        if      (Source = "B" or ndattdmc = "Y")
        add     janNIN1 to rnintotal
        add     febNIN1 to rnintotal
        add     marNIN1 to rnintotal
        add     aprNIN1 to rnintotal
        add     mayNIN1 to rnintotal
        add     junNIN1 to rnintotal
        add     julNIN1 to rnintotal
        add     augNIN1 to rnintotal
        add     sepNIN1 to rnintotal
        add     octNIN1 to rnintotal
        add     novNIN1 to rnintotal
        add     decNIN1 to rnintotal
        add     janninx1 to enintotal
        add     febninx1 to enintotal
        add     marninx1 to enintotal
        add     aprninx1 to enintotal
        add     mayninx1 to enintotal
        add     junninx1 to enintotal
        add     julninx1 to enintotal
        add     augninx1 to enintotal
        add     sepninx1 to enintotal
        add     octninx1 to enintotal
        add     novninx1 to enintotal
        add     decninx1 to enintotal
        add     enintotal to nintotal
        add     rnintotal to nintotal
        endif
....................................................................
.start with totals sheet for NIN income
        display   *p20:13,"Populating spreadsheet",*el," NIN income totals "
        move      c5 to sheetno
        sheets.item giving sheet using 5
        load      howmany from sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
        add     C1,howmany
        store    howmany to sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value=PrjStr6
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"B",str9
        setprop sheet.range(RowNumber),*Value=incname
.Jan total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"C",str9
              clear   taskname
        append  "=SUM('Brokerage NIN Rent'!C" to taskname
        append   str9 to taskname
        append  "+'Brokerage NIN Exch'!C" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.feb total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"D",str9
              clear   taskname
        append  "=SUM('Brokerage NIN Rent'!D" to taskname
        append   str9 to taskname
        append  "+'Brokerage NIN Exch'!D" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.mar total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"E",str9
              clear   taskname
        append  "=SUM('Brokerage NIN Rent'!E" to taskname
        append   str9 to taskname
        append  "+'Brokerage NIN Exch'!E" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Apr total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"F",str9
              clear   taskname
        append  "=SUM('Brokerage NIN Rent'!F" to taskname
        append   str9 to taskname
        append  "+'Brokerage NIN Exch'!F" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.May total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"G",str9
              clear   taskname
        append  "=SUM('Brokerage NIN Rent'!G" to taskname
        append   str9 to taskname
        append  "+'Brokerage NIN Exch'!G" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.JUne total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"H",str9
              clear   taskname
        append  "=SUM('Brokerage NIN Rent'!H" to taskname
        append   str9 to taskname
        append  "+'Brokerage NIN Exch'!H" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.July total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"I",str9
              clear   taskname
        append  "=SUM('Brokerage NIN Rent'!I" to taskname
        append   str9 to taskname
        append  "+'Brokerage NIN Exch'!I" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Aug total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"J",str9
              clear   taskname
        append  "=SUM('Brokerage NIN Rent'!J" to taskname
        append   str9 to taskname
        append  "+'Brokerage NIN Exch'!J" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Sep total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"K",str9
              clear   taskname
        append  "=SUM('Brokerage NIN Rent'!K" to taskname
        append   str9 to taskname
        append  "+'Brokerage NIN Exch'!K" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Oct total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"L",str9
              clear   taskname
        append  "=SUM('Brokerage NIN Rent'!L" to taskname
        append   str9 to taskname
        append  "+'Brokerage NIN Exch'!L" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Nov total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"M",str9
              clear   taskname
        append  "=SUM('Brokerage NIN Rent'!M" to taskname
        append   str9 to taskname
        append  "+'Brokerage NIN Exch'!M" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Dec total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"N",str9
              clear   taskname
        append  "=SUM('Brokerage NIN Rent'!N" to taskname
        append   str9 to taskname
        append  "+'Brokerage NIN Exch'!N" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.brokerage nin totals
        move    NINTOTAL to NINPERc
        divide  total into NINPERc
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"O",str9
              clear   taskname
        append  "=('Brokerage LR Totals'!o" to taskname
        append   str9 to taskname
        append  "*" to taskname
        append   NINPERC to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
...................................................................................
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"P",str9
              clear   taskname
        append  "=SUM('Brokerage NIN Rent'!P" to taskname
        append   str9 to taskname
        append  "+'Brokerage NIN Exch'!P" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"

........................
.sheet 6
.brokerage rental  NIN INCome
         display   *p20:13,"Populating spreadsheet",*el," NIN Rent income totals "
          move      c6 to sheetno
        sheets.item giving sheet using 6
        load      howmany from sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
        add     C1,howmany
        store    howmany to sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value=PrjStr6
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"B",str9
        setprop sheet.range(RowNumber),*Value=incname
.calculate rent %
        move    c0 to rpercent
        move    rNINtotal to rpercent
        divide  Rtotal into rpercent
.Jan rental portion
        move     c0 to mpercent
        move    janNIN1 to mpercent
        divide  RNINTotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"C",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Feb rental portion
        move     c0 to mpercent
        move    febNIN1 to mpercent
        divide  RNINTotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"D",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.mar rental portion
        move     c0 to mpercent
        move    marNIN1 to mpercent
        divide  RNINTotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"E",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Apr rental portion
        move     c0 to mpercent
        move    aprNIN1 to mpercent
        divide  RNINTotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"F",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.May rental portion
        move     c0 to mpercent
        move    mayNIN1 to mpercent
        divide  RNINTotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"G",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jun rental portion
        move     c0 to mpercent
        move    junNIN1 to mpercent
        divide  RNINTotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"H",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jul rental portion
        move     c0 to mpercent
        move    julNIN1 to mpercent
        divide  RNINTotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"I",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Aug rental portion
        move     c0 to mpercent
        move    AugNIN1 to mpercent
        divide  RNINTotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"J",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Sep rental portion
        move     c0 to mpercent
        move    sepNIN1 to mpercent
        divide  RNINTotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"K",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Oct rental portion
        move     c0 to mpercent
        move    OctNIN1 to mpercent
        divide  RNINTotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"L",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Nov rental portion
        move     c0 to mpercent
        move    novNIN1 to mpercent
        divide  RNINTotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"M",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Dec rental portion
        move     c0 to mpercent
        move    decNIN1 to mpercent
        divide  RNINTotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"N",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.rtotal total
        move    c0 to ninperc
        move    RNINTOTAL to NINPERc
        divide  Rtotal into NINPERc
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"O",str9
              clear   taskname
        append  "=('Brokerage LR Rent'!o" to taskname
        append   str9 to taskname
        append  "*" to taskname
        move    c0 to percent
        move    NINperc to percent
        call    trim using percent
        append  percent to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"

.        move    howmany,str9
.        call    Trim using str9
.        pack    RowNumber,"O",str9
.             clear   taskname
.        append  "=SUM(" to taskname
.        append  Rpercent to taskname
.        append  "*'Total NIN Income'!O" to taskname
.        append   str9 to taskname
.        append   ")" to taskname
.        reset    taskname
.        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.lastyear  - add code to pull
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"P",str9
        setprop sheet.range(RowNumber),*Value=nin2001,*NumberFormat="##,####0_);[Red](##,####0)"
.salesperson  - add code to pull
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"Q",str9
        setprop sheet.range(RowNumber),*Value=b1
.last years actual
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"T",str9
        setprop sheet.range(RowNumber),*Value=Yr2003NINR,*NumberFormat="##,####0_);[Red](##,####0)"
.end of sheet 6 rental portion
.ok lets do sheet 7
........................
.brokerage NIN exchange
        display   *p20:13,"Populating spreadsheet",*el," NIN Exch income totals "
        move      c7 to sheetno
        sheets.item giving sheet using 7
        load      howmany from sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
        add     C1,howmany
        store    howmany to sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value=PrjStr6
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"B",str9
        setprop sheet.range(RowNumber),*Value=incname
.calculate exchange %
        move    c0 to rpercent
        move    eNINtotal to epercent
        divide  Etotal into epercent
.Jan exchange portion
        move     c0 to mpercent
        move    janninx1 to mpercent
        divide  ENINtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"C",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Feb exchange portion
        move     c0 to mpercent
        move    febninx1 to mpercent
        divide  ENINtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"D",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.mar exchange portion
        move     c0 to mpercent
        move    marninx1 to mpercent
        divide  ENINtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"E",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Apr exchange portion
        move     c0 to mpercent
        move    aprninx1 to mpercent
        divide  ENINtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"F",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.May exchange portion
        move     c0 to mpercent
        move    mayninx1 to mpercent
        divide  ENINtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"G",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jun exchange portion
        move     c0 to mpercent
        move    junninx1 to mpercent
        divide  ENINtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"H",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jul exchange portion
        move     c0 to mpercent
        move    julninx1 to mpercent
        divide  ENINtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"I",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Aug exchange portion
        move     c0 to mpercent
        move    Augninx1 to mpercent
        divide  ENINtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"J",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Sep exchange portion
        move     c0 to mpercent
        move    sepninx1 to mpercent
        divide  ENINtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"K",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Oct exchange portion
        move     c0 to mpercent
        move    Octninx1 to mpercent
        divide  ENINtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"L",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Nov exchange portion
        move     c0 to mpercent
        move    novninx1 to mpercent
        divide  ENINtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"M",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Dec exchange portion
        move     c0 to mpercent
        move    decninx1 to mpercent
        divide  ENINtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"N",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.ENIN total total
        move    c0 to ninperc
        move    eNINTOTAL to NINPERc
        divide  etotal into NINPERc
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"O",str9
              clear   taskname
        append  "=('Brokerage LR Exch'!o" to taskname
        append   str9 to taskname
        append  "*" to taskname
        move    c0 to percent
        move    NINperc to percent
        call    trim using percent
        append  percent to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"

.
.        move    howmany,str9
.        call    Trim using str9
.        pack    RowNumber,"O",str9
.             clear   taskname
.        append  "=SUM(" to taskname
.        append  Epercent to taskname
.        append  "*'Total NIN Income'!O" to taskname
.        append   str9 to taskname
.        append   ")" to taskname
.        reset    taskname
.        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.lastyear  - add code to pull
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"P",str9
        setprop sheet.range(RowNumber),*Value=ninnew,*NumberFormat="##,####0_);[Red](##,####0)"
.salesperson  - add code to pull
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"Q",str9
        setprop sheet.range(RowNumber),*Value=b1
.last years actual
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"T",str9
        setprop sheet.range(RowNumber),*Value=Yr2003NINE,*NumberFormat="##,####0_);[Red](##,####0)"
        endif
.end of sheet 7 exchange NIN portion

        if        (source = "M")
.Lets do List Management Commission (LR) first
        clear     ndattdmc
        move      PrjStr6 to ndat3fld
        call      ndat3key
        move      PrjStr6 to ndatfld
        call      ndatkey

        move      c4 to sheetno
        sheets.item giving sheet using 4
        load      howmany from sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
        add     C1,howmany
        store    howmany to sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value=PrjStr6
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"B",str9
        setprop sheet.range(RowNumber),*Value=olstname

        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"O",str9
        move    c0 to total
        move    c0 to Rtotal
        add     jan1 to rtotal
        add     feb1 to rtotal
        add     mar1 to rtotal
        add     apr1 to rtotal
        add     may1 to rtotal
        add     jun1 to rtotal
        add     jul1 to rtotal
        add     aug1 to rtotal
        add     sep1 to rtotal
        add     oct1 to rtotal
        add     nov1 to rtotal
        add     dec1 to rtotal
        add     janx1 to rtotal
        add     febx1 to rtotal
        add     marx1 to rtotal
        add     aprx1 to rtotal
        add     mayx1 to rtotal
        add     junx1 to rtotal
        add     julx1 to rtotal
        add     augx1 to rtotal
        add     sepx1 to rtotal
        add     octx1 to rtotal
        add     novx1 to rtotal
        add     decx1 to rtotal
        add     rtotal to total

        display   *p20:13,"Populating spreadsheet",*el," NIN LM income totals "

.Jan LM total
        move     c0 to mpercent
        move    jan1 to mpercent
        add     janx1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"C",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Feb LM portion
        move     c0 to mpercent
        move    feb1 to mpercent
        add     febx1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"D",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.mar  LM portion
        move     c0 to mpercent
        move    mar1 to mpercent
        add     marx1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"E",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Apr LM portion
        move     c0 to mpercent
        move    apr1 to mpercent
        add     aprx1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"F",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.May LM portion
        move     c0 to mpercent
        move    may1 to mpercent
        add     mayx1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"G",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jun LM portion
        move     c0 to mpercent
        move    jun1 to mpercent
        add     junx1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"H",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jul LM portion
        move     c0 to mpercent
        move    jul1 to mpercent
        add     julx1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"I",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Aug LM portion
        move     c0 to mpercent
        move    Aug1 to mpercent
        add     Augx1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"J",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Sep LM portion
        move     c0 to mpercent
        move    sep1 to mpercent
        add     sepx1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"K",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Oct LM portion
        move     c0 to mpercent
        move    Oct1 to mpercent
        add     Octx1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"L",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Nov LM portion
        move     c0 to mpercent
        move    nov1 to mpercent
        add     novx1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"M",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Dec LM portion
        move     c0 to mpercent
        move    dec1 to mpercent
        add     decx1 to mpercent
        divide  rtotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"N",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.LM total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"O",str9
        setprop sheet.range(RowNumber),*Value=Total
.lastyear  - add code to pull
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"P",str9
        setprop sheet.range(RowNumber),*Value=proj2001,*NumberFormat="##,####0_);[Red](##,####0)"
.salesperson  - Not nec for LM
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"Q",str9
        setprop sheet.range(RowNumber),*Value=b1
.Owner
        move    howmany,str9       ."7"
        call    Trim using str9
        pack    RowNumber,"R",str9
        setprop sheet.range(RowNumber),*Value=Owner
.REVunbld
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"S",str9
        setprop sheet.range(RowNumber),*Value=REVunbld,*NumberFormat="##,####0_);[Red](##,####0)"
.2003 years actual
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"T",str9
        setprop sheet.range(RowNumber),*Value=Yr2003LR,*NumberFormat="##,####0_);[Red](##,####0)"
.2002 years actual
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"u",str9
        setprop sheet.range(RowNumber),*Value=Yr2002LR,*NumberFormat="##,####0_);[Red](##,####0)"
.2001 years actual
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"v",str9
        setprop sheet.range(RowNumber),*Value=Yr2001LR,*NumberFormat="##,####0_);[Red](##,####0)"

.end of sheet 4 LM LR portion
.
.Lets do List Management INcome (NIN)
        move      c8 to sheetno
        sheets.item giving sheet using 8
        load      howmany from sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
        add     C1,howmany
        store    howmany to sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value=PrjStr6
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"B",str9
        setprop sheet.range(RowNumber),*Value=olstname

        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"O",str9
        move    c0 to NINtotal
        add     JanNin1 to nintotal
        add     FebNin1 to nintotal
        add     MarNin1 to nintotal
        add     AprNin1 to nintotal
        add     MayNin1 to nintotal
        add     JunNin1 to nintotal
        add     JulNin1 to nintotal
        add     AugNin1 to nintotal
        add     SepNin1 to nintotal
        add     OctNin1 to nintotal
        add     NovNin1 to nintotal
        add     DecNin1 to nintotal
        add     JanNinx1 to nintotal
        add     FebNinx1 to nintotal
        add     MarNinx1 to nintotal
        add     AprNinx1 to nintotal
        add     MayNinx1 to nintotal
        add     JunNinx1 to nintotal
        add     JulNinx1 to nintotal
        add     AugNinx1 to nintotal
        add     SepNinx1 to nintotal
        add     OctNinx1 to nintotal
        add     NovNinx1 to nintotal
        add     DecNinx1 to nintotal

.Jan LM total
        move     c0 to mpercent
        move    JanNin1 to mpercent
        add     JanNinx1 to mpercent
        divide  nintotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"C",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Feb LM portion
        move     c0 to mpercent
        move    FebNin1 to mpercent
        add     FebNinx1 to mpercent
        divide  nintotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"D",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.mar  LM portion
        move     c0 to mpercent
        move    MarNin1 to mpercent
        add     MarNinx1 to mpercent
        divide  nintotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"E",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Apr LM portion
        move     c0 to mpercent
        move    AprNin1 to mpercent
        add     AprNinx1 to mpercent
        divide  nintotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"F",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.May LM portion
        move     c0 to mpercent
        move    MayNin1 to mpercent
        add     MayNinx1 to mpercent
        divide  nintotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"G",str9
              clear   taskname
        append  "=SUM(" to taskname
        append  mpercent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jun LM portion
        move     c0 to mpercent
        move    JunNin1 to mpercent
        add     JunNinx1 to mpercent
        divide  nintotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"H",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jul LM portion
        move     c0 to mpercent
        move    JulNin1 to mpercent
        add     JulNinx1 to mpercent
        divide  nintotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"I",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Aug LM portion
        move     c0 to mpercent
        move    AugNin1 to mpercent
        add     AugNinx1 to mpercent
        divide  nintotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"J",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Sep LM portion
        move     c0 to mpercent
        move    SepNin1 to mpercent
        add     SepNinx1 to mpercent
        divide  nintotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"K",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Oct LM portion
        move     c0 to mpercent
        move    OctNin1 to mpercent
        add     OctNinx1 to mpercent
        divide  nintotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"L",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Nov LM portion
        move     c0 to mpercent
        move    NovNin1 to mpercent
        add     NovNinx1 to mpercent
        divide  nintotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"M",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Dec LM portion
        move     c0 to mpercent
        move    DecNin1 to mpercent
        add     DecNinx1 to mpercent
        divide  nintotal into mpercent
.
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"N",str9
              clear   taskname
        append  "=SUM(" to taskname
        move    c0 to percent
        move    mpercent to percent
        call    trim using percent
        append  percent to taskname
        append  "*O" to taskname
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.LM total
        move    c0 to ninperc
        move    ninTOTAL to NINPERc              .nin total
        divide  Rtotal into NINPERc               .lr total
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"O",str9
              clear   taskname
        append  "=('List Management LR'!o" to taskname
        append   str9 to taskname
        append  "*" to taskname
        move    c0 to percent
        move    NINperc to percent
        call    trim using percent
        append  percent to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.        move    howmany,str9
.        call    Trim using str9
.        pack    RowNumber,"O",str9
.        setprop sheet.range(RowNumber),*Value=Total
.lastyear  - add code to pull
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"P",str9
        setprop sheet.range(RowNumber),*Value=nin2001,*NumberFormat="##,####0_);[Red](##,####0)"
.salesperson  - Not nec for LM
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"Q",str9
        setprop sheet.range(RowNumber),*Value=b1
.Owner
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"R",str9
        setprop sheet.range(RowNumber),*Value=Owner
.2003 years actual
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"T",str9
        setprop sheet.range(RowNumber),*Value=Yr2003NIN,*NumberFormat="##,####0_);[Red](##,####0)"
.2002 years actual
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"U",str9
        setprop sheet.range(RowNumber),*Value=Yr2002NIN,*NumberFormat="##,####0_);[Red](##,####0)"
.2003 years actual
        move    howmany,str9
        call    Trim using str9
        pack    RowNumber,"V",str9
        setprop sheet.range(RowNumber),*Value=Yr2001NIN,*NumberFormat="##,####0_);[Red](##,####0)"
        endif
.end of sheet 4 LM LR portion

        goto       looper2
.
hdout
.Excel Header sheet One (1) Brokerage LR Income
        move    c1 to sheetno
        sheets.item giving sheet using 1
        move    C0,RecordTop
        move    C0,RecordHeader
        move    C1,howmany
        move    howmany,str9       ."1"
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value="NAMES IN THE NEWS"
        setprop sheet.range(RowNumber).Font,*Bold="True"
        add     C1,howmany
        move    howmany,str9       ."2"
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value="Brokerage Commission Income Projections"
        call    hdout1
.
.Excel Header sheet two (2) Brokerage LR rental Income
        move    c2 to sheetno
        sheets.item giving sheet using 2
        move    C0,RecordTop
        move    C0,RecordHeader
        move    C1,howmany
        move    howmany,str9       ."1"
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value="NAMES IN THE NEWS"
        setprop sheet.range(RowNumber).Font,*Bold="True"
        add     C1,howmany
        move    howmany,str9       ."2"
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value="Brokerage Commission Rental Income Projections"
        call    hdout1
.
.Excel Header sheet (3) Brokerage LR exchange Income
        move    c3 to sheetno
        sheets.item giving sheet using 3
        move    C0,RecordTop
        move    C0,RecordHeader
        move    C1,howmany
        move    howmany,str9       ."1"
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value="NAMES IN THE NEWS"
        setprop sheet.range(RowNumber).Font,*Bold="True"
        add     C1,howmany
        move    howmany,str9       ."2"
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value="Brokerage Commission Exchange Income Projections"
        call    hdout1
.
.Excel Header sheet (4) List Management LR Income
        move    c4 to sheetno
        sheets.item giving sheet using 4
        move    C0,RecordTop
        move    C0,RecordHeader
        move    C1,howmany
        move    howmany,str9       ."1"
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value="NAMES IN THE NEWS"
        setprop sheet.range(RowNumber).Font,*Bold="True"
        add     C1,howmany
        move    howmany,str9       ."2"
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value="List Managenent LR Income Projections"
        call    hdout1
.
.Excel Header sheet  (5) Brokerage NIN Income
        move    c5 to sheetno
        sheets.item giving sheet using 5
        move    C0,RecordTop
        move    C0,RecordHeader
        move    C1,howmany
        move    howmany,str9       ."1"
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value="NAMES IN THE NEWS"
        setprop sheet.range(RowNumber).Font,*Bold="True"
        add     C1,howmany
        move    howmany,str9       ."2"
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value="Brokerage NIN Income Projections"
        call    hdout1
.
.Excel Header sheet  (6) Brokerage NIN Rental Income
        move    c6 to sheetno
        sheets.item giving sheet using 6
        move    C0,RecordTop
        move    C0,RecordHeader
        move    C1,howmany
        move    howmany,str9       ."1"
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value="NAMES IN THE NEWS"
        setprop sheet.range(RowNumber).Font,*Bold="True"
        add     C1,howmany
        move    howmany,str9       ."2"
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value="Brokerage NIN Rental Income Projections"
        call    hdout1
.
.Excel Header sheet (7) Brokerage NIN exch Income
        move    c7 to sheetno
        sheets.item giving sheet using 7
        move    C0,RecordTop
        move    C0,RecordHeader
        move    C1,howmany
        move    howmany,str9       ."1"
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value="NAMES IN THE NEWS"
        setprop sheet.range(RowNumber).Font,*Bold="True"
        add     C1,howmany
        move    howmany,str9       ."2"
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value="Brokerage NIN Exchange Income Projections"
        call    hdout1
.
.Excel Header sheet (8) List Managemnet NIN Income
        move    c8 to sheetno
        sheets.item giving sheet using 8
        move    C0,RecordTop
        move    C0,RecordHeader
        move    C1,howmany
        move    howmany,str9       ."1"
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value="NAMES IN THE NEWS"
        setprop sheet.range(RowNumber).Font,*Bold="True"
        add     C1,howmany
        move    howmany,str9       ."2"
        call    Trim using str9
        pack    RowNumber,"A",str9
        setprop sheet.range(RowNumber),*Value="List Management NIN Income Projections"
        call    hdout1
        return
.
.hdout1         sets headers for all sheets
hdout1
        pack    RowNumber,"F",str9
        setprop sheet.range(RowNumber),*Formula="=TODAY()",*NumberFormat="dd-mmm-yy"
.column A   MLR #
        add     C2,howmany
        move    howmany,str9       ."5"
        call    Trim using str9
        pack    RowNumber,"A",str9
        IF      (SHEETNO = 4 OR SHEETNO = 8)
        setprop sheet.range(RowNumber),*Value="LIST ##"
        ELSE
        setprop sheet.range(RowNumber),*Value="MLR ##"
        ENDIF
        setprop sheet.range(RowNumber).Font,*Bold="True"
.Column b   mlr OR LIST name
        move    howmany,str9       ."6"
        call    Trim using str9
        pack    RowNumber,"B",str9
        setprop sheet.range(RowNumber),*Value="Name"
.Jan
        move    howmany,str9       ."7"
        call    Trim using str9
        pack    RowNumber,"C",str9
        setprop sheet.range(RowNumber),*Value="January"
.Feb
        move    howmany,str9       ."7"
        call    Trim using str9
        pack    RowNumber,"D",str9
        setprop sheet.range(RowNumber),*Value="February"
.Mar
        move    howmany,str9       ."7"
        call    Trim using str9
        pack    RowNumber,"E",str9
        setprop sheet.range(RowNumber),*Value="March"
.Apr
        move    howmany,str9       ."7"
        call    Trim using str9
        pack    RowNumber,"F",str9
        setprop sheet.range(RowNumber),*Value="April"
.may
        move    howmany,str9       ."7"
        call    Trim using str9
        pack    RowNumber,"G",str9
        setprop sheet.range(RowNumber),*Value="May"
.Jun
        move    howmany,str9       ."7"
        call    Trim using str9
        pack    RowNumber,"H",str9
        setprop sheet.range(RowNumber),*Value="June"
.July
        move    howmany,str9       ."7"
        call    Trim using str9
        pack    RowNumber,"I",str9
        setprop sheet.range(RowNumber),*Value="July"
.aug
        move    howmany,str9       ."7"
        call    Trim using str9
        pack    RowNumber,"J",str9
        setprop sheet.range(RowNumber),*Value="August"
.sep
        move    howmany,str9       ."7"
        call    Trim using str9
        pack    RowNumber,"K",str9
        setprop sheet.range(RowNumber),*Value="September"
.oct
        move    howmany,str9       ."7"
        call    Trim using str9
        pack    RowNumber,"L",str9
        setprop sheet.range(RowNumber),*Value="October"
.Nov
        move    howmany,str9       ."7"
        call    Trim using str9
        pack    RowNumber,"M",str9
        setprop sheet.range(RowNumber),*Value="November"
.Dec
        move    howmany,str9       ."7"
        call    Trim using str9
        pack    RowNumber,"N",str9
        setprop sheet.range(RowNumber),*Value="December"
.Total
         move    howmany,str9       ."7"
        call    Trim using str9
        pack    RowNumber,"O",str9
        setprop sheet.range(RowNumber),*Value="Total"
.Previous year  projection
        move    howmany,str9       ."7"
        call    Trim using str9
        pack    RowNumber,"P",str9
        setprop sheet.range(RowNumber),*Value="Last Year"
.Salesperson
        move    howmany,str9       ."7"
        call    Trim using str9
        pack    RowNumber,"Q",str9
        setprop sheet.range(RowNumber),*Value="Salesperson"
.Owner
        if      (sheetno = 4 or sheetno = 8)
        move    howmany,str9       ."7"
        call    Trim using str9
        pack    RowNumber,"R",str9
        setprop sheet.range(RowNumber),*Value="Owner ##"
        endif
.
        add     C1,howmany
.        setprop sheet.range(RowNumber),*Value=OMLRKY,*HorizontalAlignment=xlHAlignCenter

        store   howmany into sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
        return
.
EOJ
        move      c1 to sheetno

eoj1    loop
        if       (sheetno = 1)
        sheets.item giving sheet using 1
        sheet.range("B1","Q1000").Columns.AutoFit
        endif
        if       (sheetno = 2)
        sheets.item giving sheet using 2
        sheet.range("B1","Q1000").Columns.AutoFit
        endif
        if       (sheetno = 3)
        sheets.item giving sheet using 3
        sheet.range("B1","Q1000").Columns.AutoFit
        endif
        if       (sheetno = 4)
        sheets.item giving sheet using 4
        sheet.range("B1","Q1000").Columns.AutoFit
        endif
        if       (sheetno = 5)
        sheets.item giving sheet using 5
        sheet.range("B1","Q1000").Columns.AutoFit
        endif
        if       (sheetno = 6)
        sheets.item giving sheet using 6
        sheet.range("B1","Q1000").Columns.AutoFit
        endif
        if       (sheetno = 7)
        sheets.item giving sheet using 7
        sheet.range("B1","Q1000").Columns.AutoFit
        endif
        if       (sheetno = 8)
        sheets.item giving sheet using 8
        sheet.range("B1","Q1000").Columns.AutoFit
        endif
        load      howmany from sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
        reset     str15
        clear     str15
        move      "CDEFGHIJKLMNOPQ" to str15 setprop the column
        move      str15 to str1
        move    howmany to endRow
        add     C2,endRow

        call    totRow
        add     c1 to sheetno
        repeat  while (Sheetno < 9)
        goto    filenameselect

totRow  loop
        move    endRow,str9
        call    Trim using str9
        pack    RowNumber,str1,str9
        clear   taskname
        append  "=sum(",taskname
        append  str1,taskname
        append  "6:" to taskname
        append  str1 to taskname
        move    howmany to str9
        call    trim using str9
        append   str9 to taskname
        append   ")" to taskname
        reset    taskname
        setprop sheet.range(RowNumber),*Value=taskname,*NumberFormat="##,####0_);[Red](##,####0)"
        bump     str15 by 1
        move     str15 to str1
        repeat   while (str1 <> "Q")
        return
.
FileNameSelect
..Trap in case a workbook with the same name is already open.  In such a case, the saveas will
..not occur
        move    N2,str2
        rep     zfill,str2
        clear   taskname
.        append  "c:\data\projections",taskname
        append  "c:\work\projections",taskname
        append  "_",taskname
        append  str2,taskname
        append  ".xls",taskname
        reset   taskname
        setprop ex,*DisplayAlerts=OFALSE
        clear   APIFileName
        pack    APIFileName,taskname,hexzero
        call    FindFirstFile
        if (APIResult <> 0 & APIResult <> hexeight)
.File exists, try incrementing name
                goto TrapObject2
        endif
        trap    TrapObject if Object
        book.saveas giving N9 using *Filename=taskname
        trapclr Object
CleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
.do some housekeeping first

        destroy Rowcol
        destroy sheet
        destroy sheets
        destroy book
        destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
.        setprop ex,*DisplayAlerts=OFALSE
        ex.quit
        destroy ex
.END PATCH 3.5 REPLACED LOGIC
         shutdown  "cls"
         STOP
TrapObject
        noreturn
.This routine tripped when Saveas method is called.
.
.We are trapping for instances where the User has selected a filename that: 1) Already exists
.and is open by another instance of Excel. 2) Already exists but not open elsewhere.
TrapObject2
        if (N2 >= 99)
                goto CleanUp     .Quit the job
        endif
        add     C1,N2
.Send them back to select another File name and try to Save again.
        goto FileNameSelect

        stop
.debug   return
..................................................................................................
        include     nxrfio.inc
        include     ndat3io.inc
;patch1.1
                              include        compio.inc
                              include        cntio.inc
.        include     nmlrio.inc
;patch1.1
        include     ndatio.inc
        INCLUDE              NPRJIO.INC
        INclude               Nrevio.inc

        include     comlogic.inc
