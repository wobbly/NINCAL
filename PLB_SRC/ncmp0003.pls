.ORDER PLACEMENT SPREADSHEET
PC        equ       0          
        include common.inc
        include cons.inc
        include norddd.inc
.START PATCH 1.2 REPLACED LOGIC
.        include nmlrdd.inc
.        include nbrkdd.inc
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
.END PATCH 1.2 REPLACED LOGIC
        include nrtndd.inc
        include ndatdd.inc
        include nofrdd.inc
        include nsmpdd.inc
        include ncntdd.inc
        include media.inc
          include   nxrfdd.inc
          include   oslspern.inc
.START PATCH 1.1 ADDED LOGIC
          include   nsel2dd.inc
.END PATCH 1.1 ADDED LOGIC
.
Release    INIT    "1.26"               DLH      .Include sub status in spreadsheet
Reldate   Init      "2014 February 20"
.Release    INIT    "1.25"               DLH      .Excel 2013 *WindowState=xlMinimized
.Reldate   Init      "2014 January 22"
.release init    "1.24"                 DLH      add Mailer company to every detail line
.Reldate   Init      "2013 June 05"
.release init    "1.23"                 DLH      add maildate to spreadsheet
.Reldate   Init      "2013 May 03"
.release init    "1.2.2"                 08Mar2007  DLH      Oslspern.inc expansion
.release init    "1.2.1"                02NOV2004  ASH      Sample Conversion - increased Mailer size to 6 bytes
.release init    "1.2"                  27may2004  ASH      Mailer Conversion
.release init    "1.1"                  27APR2004  ASH      Datacard Conversion
.release init    "1.0"                  17JUN2003  ASH      INITIAL RELEASE
.
infile    file
FilePtr   dim       ^
ComboPtr ComboBox ^
.In order to use any of the properties/methods associated with all parent objects
.of the Worksheet, I need to create automation objects for each of them.
.
.Look at Excel Object Model to understand heirarchy.  This can be found in hard
.documentation:  Microsoft Office 2000 Object Model Guide (found in MS Office 2000 Developers Edition).
.Software available via PL/B Designer - create a Container object on a form, create an Excel
.Spreadsheet, right click on Container object and Browse object.  This will invoke the PL/B Object
.Browser, which will give you SOME of the components of the Object Model.  To browse the Object
.Model in its entirety, open Excel.  Under Tools menu select Macro, select Visual Basic Editor.
.In the Visual Basic Editor screen, under the View menu, select Object Browser.  There you can 
.view all of the objects/methods/properties in Excel.  Right clicking on an item will give you
.option to locate Help topics to see specifics.
.
.General heirarchy:
. Excel Application
.       Workbooks Collection (all open Workbooks)
.               Single Workbook
.                       Worksheets Collection (all Worksheets in this Workbook)
.                               Single Worksheet
.                                       SortColumn (a Single Column in that Worksheet used for sorting)
.
books   automation
book    automation
sheets  automation
sheet   automation
sortcol automation
ex      automation      class="Excel.Application"
firstrow form   9
.Variant objects used to talk to outside applications
.See PL/B help in order to understand use of Variant objects.
.
.Booleans
.PL/B does not have a Boolean datatype, so we have to create our own.
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
VT_I4   EQU 3           .4 byte integer
Zoom65  variant
Zoom80  variant
xlPaperLetter integer 4,"0x1"
xlPaperLegal integer 4,"0x5"
.Formatting vars needed
.This constant was found in the Object Browser in Excel under the Help topic for the
.HorizontalAlignment property of the Range object.
AlignLeft integer 4,"0xffffefdd"
AlignRight integer 4,"0xffffefc8"
AlignCenter integer 4,"0xffffeff4"
SheetsDefault   integer 4,"0x00000000"
xlLandscape integer 4,"0x2"                     .2
xlMinimized integer 4,"0xFFFFEFD4"
xlUnderlineStyleSingle integer 4,"0x2"
xlMedium integer  4,"0xFFFFEFD6"                  .-4138

.Colors
marroon   color
colornum form       24

        
CreateOrderSheet Routine FilePtr,ComboPtr
.Create the Variant objects
.Booleans
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
.Others
        create  marroon=128:0:64
          getitem   marroon,0,colornum
.Initialize variables
.        move    C0,SubFlag
.
          open      infile,FilePtr
.         open      infile,"c:\work\andrew.dat"
          if over
                    return
          endif
          read      infile,SEQ;ORDVARS
          if over
                    return
          endif
          create    Zoom65,VarType=VT_I4,VarValue=65
          create    Zoom80,VarType=VT_I4,VarValue=80
.Open Excel application
        create  ex
.begin patch 1.25
.        setprop ex,*WindowState=xlMinimized
.end patch 1.25
        setprop ex,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
          setprop ex.CommandBars("Standard"),*Visible="True"
          setprop ex.CommandBars("Formatting"),*Visible="True"
          setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
.Reset Default of Worksheets found in a Workbook
        getprop ex,*SheetsInNewWorkbook=SheetsDefault
        setprop ex,*SheetsInNewWorkbook=C1
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
          setprop sheet.PageSetup,*Orientation=xlLandscape,*CenterFooter=" Page &P of &N"
.          setprop sheet.PageSetup,*CenterFooter=" Page &P of &N"
.Margins
          setprop sheet.PageSetup,*TopMargin=".25",*BottomMargin="33",*RightMargin=".25",*LeftMargin=".25",*HeaderMargin="0",*FooterMargin="0"
.Set the Zoom property of the actual window
        setprop book.windows(1),*Zoom=Zoom65
.Set Printing Sizes
        setprop sheet.PageSetup,*Zoom=Zoom80
        setprop sheet.PageSetup,*PaperSize=xlPaperLegal
.
        setprop sheet.range("A1"),*Value="CREATED:"
.
        setprop sheet.range("A2"),*Value="REVISED:"
.
        setprop sheet.range("B1"),*Formula="=TODAY()"
.
        pack    MKEY,OMLRNUM,"000"
        move    "CreateCamp.-NMLRKEY",Location
        pack    KeyLocation,"Key: ",MKEY
        call    NMLRKEY
        call    Trim using MCOMP
          pack      taskname,"MAILER:   ",MCOMP
        setprop sheet.range("C1"),*Value=taskname
        setprop sheet.range("C1").Font,*Bold="True"
.
        pack    NBRKFLD,OBRKNUM,OBRKCNT
          rep       zfill,NBRKFLD
          if (NBRKFLD <> "0000000")
                  move    "CreateCamp.-NBRKKEY",Location
                  pack    KeyLocation,"Key: ",NBRKFLD
                  call    NBRKKEY
                  call    Trim using BRCOMP
          else
                    clear     BRCOMP
          endif
          pack      taskname,"THRU:   ",BRCOMP
        setprop sheet.range("C2"),*Value=taskname
        setprop sheet.range("C2").Font,*Bold="True"
.
        move    C0,UNIVERSE
        clear   NXRFFLD2
        clear   NXRFLIST
        move    MKEY,NXRFFLD2
        rep     zfill,NXRFFLD2
        move    C2,NXRFPATH
        move    "CreateCamp.-NXRFKEY",Location
        pack    KeyLocation,"Key: ",NXRFFLD2
        call    NXRFKEY
        if not over
                move    NXRFLIST,NDATFLD
                move    C1,NDATPATH
                move    "CreateCamp.-NDATKEY",Location
                pack    KeyLocation,"Key: ",NDATFLD
                call    NDATKEY
        endif
          pack      taskname,"MAILER/List##:   ",OMLRNUM,SLASH,NXRFLIST
        setprop sheet.range("C4"),*Value=taskname
        setprop sheet.range("C4").Font,*Bold="True"
.
          call      FormatNumeric using UNIVERSE,str11
          pack      taskname,"UNIVERSE:   ",str11," names"
        setprop sheet.range("C6"),*Value=taskname
.
          bump      OODNUM,4
          pack      NOFRFLD,OMLRNUM,OODNUM
        rep     zfill in NOFRFLD
        move    "CreateCamp.-NOFRKEY",Location
        pack    KeyLocation,"Key: ",NOFRFLD
        call    NOFRKEY
          call      Trim using OFDESC
          pack      taskname,"OFFER:   ",OFNUM,B1,OFDESC
        setprop sheet.range("C7"),*Value=taskname
.
          clear     NSMPFLD
.START PATCH 1.2.1 REPLACED LOGIC - TEMPORARY PATCH
.         pack      NSMPFLD,OMLRNUM,OSAMCDE
          pack      COMPFLD3,OMLRNUM
          move      "CreateCamp.-COMPKEY3",Location
          pack      KeyLocation,"Key: ",COMPFLD3
          call      COMPKEY3
          if over
                    clear     COMPNUM
          endif
          pack      NSMPFLD,COMPNUM,OSAMCDE
.END PATCH 1.2.1 REPLACED LOGIC - TEMPORARY PATCH
          rep       zfill,NSMPFLD
          move      "CreateCamp.-NSMPKEY",Location
          pack      KeyLocation,"Key: ",NSMPFLD
          call      NSMPKEY
          call      Trim using OSAMCDE
          move      C0,N9
          move      OSAMCDE,N9
          if (N9 > 0)
                    pack      taskname,"SAMPLE ##",OSAMCDE," - ",NSMPDES1
          else
                    pack      taskname,"SAMPLE ##"
          endif
        setprop sheet.range("C8"),*Value=taskname
.
        setprop sheet.range("C10"),*Value="Comments:"
.......................
        move    " ",MEDIA
        move    C0,N2
        move    OFOCODE,N2
        move    N2,N3
        add     C1,N2   .File begins with '0', Combo begins with '1' and  first item is null
        if (N3 = 20)
                move    C0,N2   .First item has blank filled string of MED20
        else
                if (N3 < 20)
                        add     C1,N2   
                endif 
        endif
        load    MEDIA from N2 of MED20,MED0,MED1,MED2,MED3,MED4,MED5:
                MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
                MED15,MED16,MED17,MED18,MED19,MED21,MED22:
                MED23,MED24,MED25,MED26,MED27,MED28,MED29
        call    Trim using MEDIA
          pack      taskname,"MEDIA:   ",MEDIA 
        setprop sheet.range("E4"),*Value=taskname
        setprop sheet.range("E4").Font,*Bold="True"
.
        call    Trim using OSHP
        clear   str55
        if (OSHP <> "")
                move    C0,N2
                move    OSHP,N2
                add     C2,N2
                    getitem ComboPtr,N2,str55
        endif
          pack      taskname,"SHIP VIA:   ",str55
        setprop sheet.range("E5"),*Value=taskname
.
        move    ORTNNUM,NRTNFLD
        move    "CreateCamp.-NRTNKEY",Location
        pack    KeyLocation,"Key: ",NRTNFLD
        call    NRTNKEY
        call    Trim using RTCOMP
          if (RTCOMP <> "")
                    pack      taskname,"SHIP TO: ##",ORTNNUM,COMMA,B1,RTCOMP
          else
                    pack      taskname,"SHIP TO: ##",ORTNNUM
          endif
        setprop sheet.range("E7"),*Value=taskname
        setprop sheet.range("E7").Font,*Bold="True"
.
          pack      taskname,"ATTN: "
        setprop sheet.range("E8"),*Value=taskname
        setprop sheet.range("E8").Font,*Bold="True"
.
          pack      taskname,""         .Future email address!!!!
        setprop sheet.range("E9"),*Value=taskname
.
          pack      taskname,"MP##: ",OMLRKY
        setprop sheet.range("E10"),*Value=taskname
        setprop sheet.range("E10").Font,*Bold="True"
.
          pack      taskname,"P.O.##: ",OMLRPON
        setprop sheet.range("E11"),*Value=taskname
        setprop sheet.range("E11").Font,*Bold="True"
.......................
          call      Trim using ORTNDTEM
          if (ORTNDTEM <> "")
                    pack      str10,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY
          else
                    clear     str10
          endif
          pack      taskname,"Return Date:  ",str10
        setprop sheet.range("I5"),*Value=taskname
        setprop sheet.range("I5").Font,*Bold="True"
.
          call      Trim using OMDTEM
          if (OMDTEM <> "")
                    pack      str10,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
          else
                    clear     str10
          endif
          pack      taskname,"Mail Date:  ",str10
        setprop sheet.range("I6"),*Value=taskname
        setprop sheet.range("I6").Font,*Bold="True"
.
          pack      taskname,"Absolute: "
        setprop sheet.range("I7"),*Value=taskname
.
        move    C1,NCNTPATH
        move    OCOCODE,NCNTFLD
        move    "CreateCamp.-NCNTKEY",Location
        pack    KeyLocation,"Key: ",NCNTFLD
        call    NCNTKEY
        if over
                clear   CNTNAME
        endif
        call    Trim using CNTNAME
          pack      taskname,"NIN CONTACT:   ",CNTNAME
        setprop sheet.range("I9"),*Value=taskname
        setprop sheet.range("I9").Font,*Bold="True"
.
          pack      str2,OSALES10,OSALES
          move      C0,howmany
          move      str2,HowMany
          move      osls0,str45
          load      str45 from HowMany of osls1,osls2,osls3,osls4,osls5,osls6,osls7:
                    osls8,osls9,osls10,osls11,osls12,osls13,osls14,osls15,osls16:
                    osls17,osls18,osls19,osls20,osls21,osls22,osls23,osls24,osls25:
                    osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
          pack      taskname,"NIN SALES:   ",str45
        setprop sheet.range("I10"),*Value=taskname
        setprop sheet.range("I10").Font,*Bold="True"
.
        setprop sheet.range("I12"),*Value="*Italicized items have already been ordered*"
        setprop sheet.range("I12").Font,*Bold="True"
.
.....Record Header.....
.
          move      "15",firstrow
          setprop sheet.range("K14"),*Value="Key",*HorizontalAlignment=AlignCenter       
. 
          setprop sheet.range("A15"),*Value="LCR ##",*HorizontalAlignment=AlignCenter       
          setprop sheet.range("B15"),*Value="LIST ##",*HorizontalAlignment=AlignCenter
          setprop sheet.range("C15"),*Value="LIST NAME",*HorizontalAlignment=AlignCenter
          setprop sheet.range("D15"),*Value="SELECT",*HorizontalAlignment=AlignCenter
          setprop sheet.range("E15"),*Value="Universe",*HorizontalAlignment=AlignCenter
          setprop sheet.range("F15"),*Value="QTY",*HorizontalAlignment=AlignCenter
          setprop sheet.range("G15"),*Value="ALL?",*HorizontalAlignment=AlignCenter
          setprop sheet.range("H15"),*Value="R/EX",*HorizontalAlignment=AlignCenter
          setprop sheet.range("I15"),*Value="Net",*HorizontalAlignment=AlignCenter
          setprop sheet.range("J15"),*Value="RC",*HorizontalAlignment=AlignCenter
          setprop sheet.range("K15"),*Value="Codes",*HorizontalAlignment=AlignCenter
          setprop sheet.range("L15"),*Value="Omit?",*HorizontalAlignment=AlignCenter
          setprop sheet.range("M15"),*Value="Date",*HorizontalAlignment=AlignCenter
          setprop sheet.range("N15"),*Value="Qty",*HorizontalAlignment=AlignCenter
.begin patch 1.23
          setprop sheet.range("O15"),*Value="Mail Date",*HorizontalAlignment=AlignCenter
.begin patch 1.24
          setprop sheet.range("P15"),*Value="Mailer",*HorizontalAlignment=AlignCenter
.
..        sheet.range("A14:N15").BorderAround using *LineStyle=1,*Weight=xlMedium,*Color=colornum
..          setprop sheet.range("A14:N15").Font,*Bold="True"
.        sheet.range("A14:O15").BorderAround using *LineStyle=1,*Weight=xlMedium,*Color=colornum
.          setprop sheet.range("A14:O15").Font,*Bold="True"
.begin patch 1.26
          setprop sheet.range("Q15"),*Value="Sub Status",*HorizontalAlignment=AlignCenter
.        sheet.range("A14:P15").BorderAround using *LineStyle=1,*Weight=xlMedium,*Color=colornum
        sheet.range("A14:Q15").BorderAround using *LineStyle=1,*Weight=xlMedium,*Color=colornum
.          setprop sheet.range("A14:P15").Font,*Bold="True"
          setprop sheet.range("A14:Q15").Font,*Bold="True"
.end patch 1.26
.end patch 1.24
.end patch 1.23
.
          move      firstrow,str9
          call      Trim using str9
          pack    str11,"1:",str9
          setprop sheet.PageSetup,*PrintTitleRows=str11
.
.....Records.....
.
.Records for this report always start 1 row after Top Row of Headers
.howmany holds top row of Records.
.firstrow holds top row of Headers.
        add     C1,firstrow,howmany
          move      howmany,N9
          loop
.First record has already been read!!
                    call    LoadDetail
                    read      infile,SEQ;ORDVARS
                    until over
          repeat
          close     infile
.....Set Up Total.....
.         add       C1,N9,N10
          sub       C1,N9,N10
          move      N9,str10
          call      Trim using str10
          pack      str11,"D",str10
          setprop sheet.range(str11),*Value="Total:",*HorizontalAlignment=AlignRight
          setprop sheet.range(str11).Font,*Bold="True"
.
          pack      str11,"F",str10
          move      howmany,str9
          call      Trim using str9
          move      N10,str10
          call      Trim using str10
          pack      taskname,"=SUM(F",str9,":F",str10,")"
          setprop sheet.range(str11),*Formula=taskname
          setprop sheet.range(str11).Font,*Bold="True"
          setprop sheet.range(str11).Borders(8),*Weight=4             .8 - Top Border
.....Sort by LCR Number.....
.        trap    errortrap if object
.Select a column on which to sort
.This is ugly code.  You need to set the key value of the Sort method to a specific column.
.The Columns property returns a Range object, which is then used by the Sort method.
.Again, all this info found in the Object Browser in Excel.
.LCR Number
        move    howmany,str9
        call    Trim using str9
        pack    str11,"C",str9
          move      N9,str10
          call      Trim using str10
        pack    str12,"Z",str10                   .Really only need to sort to 'N', but I add a few columns in case Worksheet is expanded!!
        getprop sheet.range(str11),*Columns(1)=sortcol
        pack    str11,"A",str9
.Key1 set to LCR Number 1(Ascending) or 2(Descending)
        sheet.range(str11,str12).sort using *Key1=sortcol,*Order1=1             .Currently NO secondary sort:  ,*Key2=sortcol1,*Order2=1
.
          setprop   sheet.range(str11,str12),*RowHeight="31.0"
.Top Record already established, now establish last record
          add       C1,N10
          move      N10,str11
          call      Trim using str11
...Format whole document..
.Font change needs to happen before Autofit, otherwise Autofit would have to be run again.
        pack    str10,"A1"
          pack      str12,"Z",str11
        setprop sheet.range(str10,str12).Font,*Name="Arial",*Size=12
.Autofit.
        pack    str10,"A1"
        pack    str12,"A",str11
        sheet.range(str10,str12).Columns.Autofit
        pack    str10,"B1"
        pack    str12,"B",str11
        sheet.range(str10,str12).Columns.Autofit
        pack    str10,"C1"
        pack    str12,"C",str11
.        sheet.range(str10,str12).Columns.Autofit
          setprop   sheet.range(str10,str12),*ColumnWidth="40.0"
        pack    str10,"D1"
        pack    str12,"D",str11
.        sheet.range(str10,str12).Columns.Autofit
          setprop   sheet.range(str10,str12),*ColumnWidth="30.0",*WrapText="True"
        pack    str10,"E1"
        pack    str12,"E",str11
.        sheet.range(str10,str12).Columns.Autofit
          setprop   sheet.range(str10,str12),*ColumnWidth="15.3"
        pack    str10,"F1"
        pack    str12,"F",str11
        sheet.range(str10,str12).Columns.Autofit
        pack    str10,"G1"
        pack    str12,"G",str11
        sheet.range(str10,str12).Columns.Autofit
        pack    str10,"H1"
        pack    str12,"H",str11
        sheet.range(str10,str12).Columns.Autofit
        pack    str10,"I1"
        pack    str12,"I",str11
.        sheet.range(str10,str12).Columns.Autofit
          setprop   sheet.range(str10,str12),*ColumnWidth="10.5"
        pack    str10,"J1"
        pack    str12,"J",str11
        sheet.range(str10,str12).Columns.Autofit
        pack    str10,"K1"
        pack    str12,"K",str11
        sheet.range(str10,str12).Columns.Autofit
        pack    str10,"L1"
        pack    str12,"L",str11
        sheet.range(str10,str12).Columns.Autofit
        pack    str10,"M1"
        pack    str12,"M",str11
        sheet.range(str10,str12).Columns.Autofit
        pack    str10,"N1"
        pack    str12,"N",str11
        sheet.range(str10,str12).Columns.Autofit
.begin patch 1.23
        pack    str10,"O1"
        pack    str12,"O",str11
        sheet.range(str10,str12).Columns.Autofit
.end patch 1.23
.begin patch 1.24
        pack    str10,"P1"
        pack    str12,"P",str11
        sheet.range(str10,str12).Columns.Autofit
.end patch 1.24
.begin patch 1.26
        pack    str10,"Q1"
        pack    str12,"Q",str11
        sheet.range(str10,str12).Columns.Autofit
.end patch 1.26

.
CampaignFileNameSelect
          move      "c:\work\",taskname                               ."
          setprop ex,*DefaultFilePath=taskname
.
.         pack    taskname,taskname,"OrderSheet"
.         setmode *mcursor=*arrow
.         ex.GetSaveAsFilename giving taskname using *InitialFilename=taskname
.         if (taskname <> "0")
.                   movelptr taskname,N9
.                   reset   taskname,N9
.                   append  "xls",taskname
.                   reset   taskname
..Trap in case a workbook with the same name is already open.  In such a case, the saveas will
..not occur
.                   trap    TrapCampaignObject if Object
.                   book.saveas giving N9 using *Filename=taskname
.                   trapclr Object
.         endif
          setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
CampaignCleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
          destroy Zoom65
          destroy Zoom80
        destroy sortcol
        destroy sheet
        destroy sheets
        destroy book
        destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
.        setprop ex,*DisplayAlerts=OFALSE
        setprop ex,*SheetsInNewWorkbook=SheetsDefault
        destroy ex
        return

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
.        goto CampaignCleanUp

errortrap
.testing purposes
        getinfo exception,taskname
        return

LoadDetail
.Prep work
        move    N9,str9
        call    Trim using str9
.
          pack      str10,"A",str9
          setprop sheet.range(str10),*Value=OLRN,*HorizontalAlignment=AlignLeft   
.
          pack      str10,"B",str9
          setprop sheet.range(str10),*Value=OLNUM,*HorizontalAlignment=AlignCenter
.
          move    OLNUM,NDATFLD
          move    C1,NDATPATH
          move    "L.Detail-NDATKEY",Location
          pack    KeyLocation,"Key: ",NDATFLD
          call    NDATKEY
          pack      str10,"C",str9
          setprop sheet.range(str10),*Value=OLSTNAME
.
          pack      str10,"D",str9
.START PATCH 1.1 REPLACED LOGIC
.         call      Trim using O2DES
.         setprop sheet.range(str10),*Value=O2DES
..
.         pack      str10,"E",str9
.         setprop sheet.range(str10),*Value=UNIVERSE,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=AlignCenter
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
                    move      OUQTY,NSEL2QTY
          endif
          call      Trim using NSEL2NAME
          setprop sheet.range(str10),*Value=NSEL2NAME
.
          pack      str10,"E",str9
          setprop sheet.range(str10),*Value=NSEL2QTY,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=AlignCenter
.END PATCH 1.1 REPLACED LOGIC
.
          pack      str10,"F",str9
          setprop sheet.range(str10),*Value=OQTY,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=AlignCenter
.
          if (OELCODE = "1" | OELCODE = "3")
                    move      YES,str1
          else
                    clear     str1
          endif
          pack      str10,"G",str9
          setprop sheet.range(str10),*Value=str1,*HorizontalAlignment=AlignCenter
.
          if (OSTAT = "p" | OSTAT = "x" | OSTAT = "l" | OSTAT = "z")  .LCR/Pending
                    if (OCLRSTAT = "1")
                              move      "EX",str15
                    elseif (OCLRSTAT = "2")
                              move      "R",str15
                    elseif (OCLRSTAT = "3")
                              move      "Split",str15
                    else
                              clear     str15
                    endif
          else
.Italicize whole Row!!!  This is a Live Order
                    pack      str45,"A",str9,":Z",str9
                    setprop sheet.range(str45).Font,*Italic="True"
                    if (OELCODE = "2" | OELCODE = "3")
                              move      C0,result
                              move      OEXQTY,result
                              if (result > 0)
                                        move      "Live Split",str15
                              else
                                        move      "Live Exch.",str15
                              endif
                    else
                              move      "Live Rental",str15
                    endif
          endif
          pack      str10,"H",str9
          setprop sheet.range(str10),*Value=str15,*HorizontalAlignment=AlignCenter
          setprop sheet.range(str10).Font,*Bold="True"
.
          pack      str10,"I",str9
          move      C0,N32
          move      ONETPER,N32
          mult      ".01",N32
          if (N32 > 0)
                    setprop sheet.range(str10),*Value=N32,*NumberFormat="##,####0%_);_(#"-#"_)",*HorizontalAlignment=AlignCenter
          endif
.
          if (ONETRC > 0)
                    pack      str10,"J",str9
                    setprop sheet.range(str10),*Value=ONETRC,*NumberFormat="$##,####0.00_);_(#"-#"_)",*HorizontalAlignment=AlignCenter
          endif
.
          pack      str10,"K",str9
          setprop sheet.range(str10),*Value=OMLRKY,*HorizontalAlignment=AlignCenter
          setprop sheet.range(str10).Font,*Bold="True"
.
          if (OCCODE = "1")   .Omit
                    pack      str10,"L",str9
                    setprop sheet.range(str10),*Value=OLRNCO,*HorizontalAlignment=AlignCenter
.
                    call      Trim using OODTECOM
                    count     N2,OODTECOM
                    if (N2 <> 0 AND     OODTECOM <> "00")
                              pack      str25,OODTECOM,SLASH,OODTECOD,SLASH,OODTECOC,OODTECOY
                    else
                              clear     str25
                    endif
                    pack      str10,"M",str9
                    setprop sheet.range(str10),*Value=str25,*HorizontalAlignment=AlignCenter
.
                    pack      str10,"N",str9
                    setprop sheet.range(str10),*Value=OQTYCO,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=AlignCenter
          endif
.begin patch 1.23
          pack      str25,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
          pack      str10,"O",str9
          setprop sheet.range(str10),*Value=str25,*HorizontalAlignment=AlignCenter
.end patch 1.23
.begin patch 1.24
        pack    MKEY,OMLRNUM,"000"
        move    "CreateCamp.-NMLRKEY",Location
        pack    KeyLocation,"Key: ",MKEY
        call    NMLRKEY
        call    Trim using MCOMP
          pack      str10,"P",str9
          setprop sheet.range(str10),*Value=MCOMP,*HorizontalAlignment=AlignLeft
.end patch 1.24
.begin patch 1.26
	Clear	Str12
	if	(Ohist = "E")
	move	"Cleared",str12
	elseif	(Ohist = "z")
	move	"Denied",str12
	endif
          pack      str10,"Q",str9
          setprop sheet.range(str10),*Value=str12,*HorizontalAlignment=AlignLeft
.end patch 1.26

        add     C1,N9
        return
        
        include nordio.inc
.START PATCH 1.2 REPLACED LOGIC
.        include nmlrio.inc
.        include nbrkio.inc
          INCLUDE   COMPio.inc
          INCLUDE   CNTio.inc
.END PATCH 1.2 REPLACED LOGIC
        include nrtnio.inc
        include nxrfio.inc
        include ndatio.inc
        include nofrio.inc
        include nsmpio.inc
        include ncntio.inc
.START PATCH 1.1 ADDED LOGIC
          include   nsel2io.inc
.END PATCH 1.1 ADDED LOGIC
        include comlogic.inc
