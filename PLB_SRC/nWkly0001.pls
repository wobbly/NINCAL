PC        Equ       0
          inc       common.inc
          inc       cons.inc
          inc       norddd.inc
          include   compdd.inc
          include   cntdd.inc
.begin patch 1.04
          INCLUDE   NSEL2DD.INC
.end patch 1.04
.
Release    INIT    "1.09"   DLH Remove Reuben
Reldate   Init      "2015 May 18"
.Release    INIT    "1.08"   DLH add Jennifer M
.Reldate   Init      "2015 April 1"
.Release    INIT    "1.07"   DLH add Denise
.Reldate   Init      "2015 march x"
.Release    INIT    "1.06"   DLH remove Inga
.Reldate   Init      "2014 September 24"
.Release    INIT    "1.05"               DLH      .moved to rolling month and wednesday run
.Reldate   Init      "2014 June 5"
.Release    INIT    "1.04"               DLH      .add select and qty
.Reldate   Init      "2014 April 14"
.Release    INIT    "1.03"               DLH      .Excel 2013 *WindowState=xlMinimized
.Reldate   Init      "2014 January 22"
.Release   Init      "1.02"          DLH add mailer # to output
.Reldate   Init      "2014 January 10"
.Release   Init      "1.01"        DLH  Remove Pia
.Reldate   Init      "2013 September 9"
.Release   Init      "1.0"      DLH    Produce spreadsheet of the weeks List Management lcrs and orders. runs every friday at 11pm
.Reldate   Init      "2013 April 10"
FileCheck FIle
trapcount form      4
Input     File
output    File

.variables used to output to Excel
outmlr    dim       55    21-75      =mailer name
outbrk    dim       55    76-130     =broker comp name
outbrkc   dim       55   131-185     =broker contact
outbrkE   Dim       55   186-240     =broker contact email
outnum    form      5    241-245     =number of requests
RecType   Dim       9

RangeStr  dim       12
RangeStr2 dim       12
#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
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
Zoom80  variant
.Formatting vars needed
SheetsDefault integer 4,"0x00000000"
xlMinimized integer 4,"0xFFFFEFD4"
xlMaximized integer 4,"0xFFFFEFD7"
xlAlignCenter integer 4,"0xffffeff4"
xlUnderlineStyleSingle integer 4,"0x2"
MedThick integer 4,"0xFFFFEFD6"
AllMargin     variant
xlColWidth    variant
xlLandscape integer 4,"0x2"                     .2
xlAlignRight integer 4,"0xFFFFEFC8"
xlAlignLeft integer 4,"0xFFFFEFDD"
xlPaperLegal integer 4,"0x5"
SReturn       init            0x0a                                                        .soft return/line feed
LOText        dim             100
.
range         dim             20
range2        dim             20
SJdate        Form            5            Todays date in Julian


          Move      "775000",nordfld                           .change periodically
          move      c1,nordpath
          move      c3,nordlock
          move      c3,nMLRlock
          move      c3,nsel2lock
          call      Nordkey
          goto      exit if over        .something really wrong if over
          Pack      Taskname from "e:\data\WeeklyLM|NINS1:502"
          Prepare   output,taskname,Exclusive
          clock     timestamp,str8
.          MOVE       "20150630",STR8
          unpack    str8 into CC,yy,mm,dd
          call      cvtjul
          move      Juldays,Sjdate
          
          loop
Loop1     call      nordks
          until     over
          pack      str2 from osales10,osales
          rep       Zfill,str2
          if        (str2 <> "06" & str2 <> "27")           .skip if not LM
          goto      Loop1
          endif
          if        (ostat <> "p" & ostat <> "l" & ostat <> "0" & ostat <> "B")           .skip if not lcr, pending, or LIVe order
          goto      Loop1
          endif
          MOVe      oodtec,cc
          move      oodtey,yy
          move      oodtem,mm
          move      oodted,dd
          call      cvtjul


.          if        (juldays  < sjdate-4)    .current week only
          if        (juldays  < sjdate-7)    .current week only rolling midweek
          goto      Loop1
          endif
          Write     Output,seq;ordvars        
          repeat
          weof      output,seq
          close     output
.sort by order date, mailer, mail date
SortOut   pack      taskname with "\\nins1\e\data\WeeklyLM.dat,\\nins1\e\data\WeeklyLM.srt;202-209,3-6,66-73"
.          sort      taskname
          sort      taskname,SUNDM="NINS1:502"
          If        Over
.sort failed
                    pack      str255 from "Sort to output.srt failed, ",CRLF,"Error: ",S$ERROR$,crlf:
                              "Yes = you fixed, No=try again, Canncel= abort"                 
                    ALERT    PLAIN,str255,RESULT
                               IF       (RESULT = 1)
                               ALERT    NOTE,"YES was pressed.",RESULT
                               ELSEIF   (RESULT = 2)
                               ALERT    NOTE,"NO was pressed.",RESULT
                               Goto     SortOut
                               ELSEIF  (RESULT = 3)
                               ALERT   NOTE,"CANCEL was pressed.",RESULT
                               Shutdown "Cls"
                               ENDIF
                     endif
          Pack      Taskname from "e:\data\WeeklyLM.srt|NINS1:502"
          open      Input,taskname,read

CreateSheet
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
.
               create         xlRowHeight,VarType=VT_R8,VarValue="75.0"
.Open Excel application
               create  ex

        setprop ex,*Visible="False",*IgnoreRemoteRequests="True",*Interactive="False"

              getprop ex,*SheetsInNewWorkbook=SheetsDefault
                      setprop ex,*SheetsInNewWorkbook=C1
.begin patch 1.03
.               setprop ex,*WindowState=xlMinimized
.end patch 1.03
.Create Workbooks collection
               getprop ex,*Workbooks=books
.Create/Add a single Workbook
               books.add
               books.item giving book using 1
.Create Worksheets collection
               getprop book,*Sheets=sheets
               sheets.item giving sheet using 1
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
               setprop        sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
          Pack      Taskname from "=Hyperlink(#"http://www.namesinthenews.com#",#"Names in the News#")"
          setprop         sheet.Range("D1:D1"),*Formula=taskname
          sheet.range("D1:F1").Merge

          setprop        sheet.Range("A1"),*Value="Weekly List Management Clearance Report:",*HorizontalAlignment=xlAlignLeft
          sheet.range("A1:C1").Merge
          setprop        sheet.Range("A2"),*Value="Date",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("A3"),*Value="entered",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("B3"),*Value="Mailer",*HorizontalAlignment=xlAlignLeft
.begin patch 1.02
          setprop        sheet.Range("C3"),*Value="Mailer##",*HorizontalAlignment=xlAlignLeft

          setprop        sheet.Range("D3"),*Value="Company",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("E3"),*Value="Contact",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("F3"),*Value="Email",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("G2"),*Value="List",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("G3"),*Value="Name",*HorizontalAlignment=xlAlignLeft
.begin patch 1.04
          setprop        sheet.Range("H2"),*Value="List",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("H3"),*Value="Select",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("I3"),*Value="Quantity",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("J2"),*Value="Mail",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("J3"),*Value="Date",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("K3"),*Value="Type",*HorizontalAlignment=xlAlignLeft
          setprop sheet.Range("a1:K3").Font,*Bold="True",*Size=12

.          setprop        sheet.Range("H2"),*Value="Mail",*HorizontalAlignment=xlAlignLeft
.          setprop        sheet.Range("H3"),*Value="Date",*HorizontalAlignment=xlAlignLeft
.          setprop        sheet.Range("I3"),*Value="Type",*HorizontalAlignment=xlAlignLeft
.          setprop sheet.Range("a1:I3").Font,*Bold="True",*Size=12
.end patch 1.04

.          setprop        sheet.Range("C3"),*Value="Company",*HorizontalAlignment=xlAlignLeft
.          setprop        sheet.Range("D3"),*Value="Contact",*HorizontalAlignment=xlAlignLeft
.          setprop        sheet.Range("E3"),*Value="Email",*HorizontalAlignment=xlAlignLeft
.          setprop        sheet.Range("F2"),*Value="List",*HorizontalAlignment=xlAlignLeft
.          setprop        sheet.Range("F3"),*Value="Name",*HorizontalAlignment=xlAlignLeft
.          setprop        sheet.Range("G2"),*Value="Mail",*HorizontalAlignment=xlAlignLeft
.          setprop        sheet.Range("G3"),*Value="Date",*HorizontalAlignment=xlAlignLeft
.          setprop        sheet.Range("H3"),*Value="Type",*HorizontalAlignment=xlAlignLeft
.          setprop sheet.Range("a1:H3").Font,*Bold="True",*Size=12
.end patch 1.02
          move      c3,Howmany



          loop
loop2     read      input,seq;ordvars
          until     over

          if        (ostat = "l")         .lcr
          MOve      "lcr",rECtYPE
          elseif    (ostat = "p")           .pending
          MOve      "Pending",rECtYPE
          elseif    (ostat = "0" or Ostat = "B")           .Live Order
          MOve      "Order",rECtYPE
          endif



New       pack      Mkey from omlrnum,"000"
          move      "Get-NMLREY",Location
          pack      KeyLocation,"Key: ",Mkey
          call      Nmlrkey
          move      compcomp,outmlr


          pack      NBRKFLD,OBRKNUM,OBRKCNT
          rep       zfill,NBRKFLD
          move      "Get-NBRKKEY",Location
          pack      KeyLocation,"Key: ",NBRKFLD
          clear     CNCTEMAIL           .clear contact email 
          call      NBRKKEY             .
          move      compcomp,outbrk
          move      brcntct,outbrkc
          move      BREmail,outbrkE
          move      c1,outnum
          
LoadExcel

              add             C1,howmany
              move            howmany,str9
              call            Trim using str9
              pack            str12,"A",str9
              pack            str10 from oodtem,slash,oodted,slash,oodtec,oodtey
              setprop         sheet.Range(str12),*Value=str10
              pack            str12,"B",str9
              setprop         sheet.Range(str12),*Value=OutMLR
.begin patch 1.12
              pack            str12,"C",str9
              setprop         sheet.Range(str12),*Value=Omlrnum
              pack            str12,"D",str9
              setprop         sheet.Range(str12),*Value=Outbrk
              pack            str12,"E",str9
              setprop         sheet.Range(str12),*Value=OutbrkC
              pack            str12,"F",str9
              setprop         sheet.Range(str12),*Value=OutbrkE
              pack            str12,"G",str9
              setprop         sheet.Range(str12),*Value=O1des
              pack            str10 from omdtem,slash,omdted,slash,omdtec,omdtey
.begin patch 1.04
.....select
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
          endif
           move       howmany,str9
           call       Trim using str9
           pack       str12,"H",str9
           call    TRIM using NSEL2NAME
           if (NSEL2NAME <> "")
           setprop    sheet.Range(str12),*Value=NSEL2NAME,*Wraptext=OTRUE
           endif
              pack            str12,"I",str9        
              setprop         sheet.Range(str12),*Value=oqty,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter


.              pack            str12,"H",str9        
              pack            str12,"J",str9        
              setprop         sheet.Range(str12),*Value=str10
.              pack            str12,"I",str9        
              pack            str12,"K",str9        
              setprop         sheet.Range(str12),*Value=RecType
.end patch 1.04

.              pack            str12,"C",str9
.              setprop         sheet.Range(str12),*Value=Outbrk
.              pack            str12,"D",str9
.              setprop         sheet.Range(str12),*Value=OutbrkC
.              pack            str12,"E",str9
.              setprop         sheet.Range(str12),*Value=OutbrkE
.              pack            str12,"F",str9
.              setprop         sheet.Range(str12),*Value=O1des
.              pack            str10 from omdtem,slash,omdted,slash,omdtec,omdtey
.              pack            str12,"G",str9        
.              setprop         sheet.Range(str12),*Value=str10
.              pack            str12,"H",str9        
.              setprop         sheet.Range(str12),*Value=RecType
                   
          repeat

               pack    RangeStr,"A2"
.begin patch 1.04
.               pack    RangeStr2,"I",str9
               pack    RangeStr2,"K",str9
.end patch 1.04
.               pack    RangeStr2,"H",str9
.end patch 1.02
        sheet.range(RangeStr,RangeStr2).Columns.Autofit



ExcelFileNameSelect
          clear   taskname
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
                              Append    "C:\WORK\WeeklyLM",taskname    
                              if        (#ver = c1)
                              append  ".xlsx",taskname
                              else
                              append  ".xls",taskname
                              endif
                              Reset     Taskname                              
                              erase          taskname
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
                              trap    TrapExcelObject if Object
                              book.saveas giving N9 using *Filename=taskname
                              trapclr Object
.
                              setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
CleanUp
                    setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
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
                              move    "Here is your Weekly Managment data in Excel",MailSubjct
                              pack    MailBody,"Input File:  ",INPNAME
                              pack      MailBOdy,"Input File:  ",INPNAME
.                              Pack      MailTO from User,"PiaPayne@nincal.com,SuzieMcGuire@nincal.com"
                              Pack      MailTO from User,"SuzieMcGuire@nincal.com,SusanAnstrand@nincal.com,DeniseHubbard@nincal.com"
.                              pack      MailCc from User,"ReubenHolland@nincal.com,KrsniWatkins@nincal.com,DeniseHubbard@nincal.com,JenniferMagee@nincal.com"
                              pack      MailCc from User,"KrsniWatkins@nincal.com,KatherineQuamina@nincal.com,JenniferMagee@nincal.com"
.                              pack      MailCc from User,"SusanAnstrand@nincal.com,IngaBeck@nincal.com,ReubenHolland@nincal.com,KrsniWatkins@nincal.com"
.                              Pack      MailTO from User,"Dherric@nincal.com"
                              Pack      MailFrom from User,"creques@nincal.com"
                              Pack      MailAttach from taskname
                              Move      c0,TrapCount                   .reset

CheckFile

                              trap      WaitForEnd giving error if IO
                              open      FileCheck,MailAttach,Exclusive          
                              Close     FIleCHeck

                              call      SendMail

Exit      Shutdown  "cls"          
TrapExcelObject
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
        goto ExcelFileNameSelect
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Ndly0001 - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
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




          include   Nordio.inc
          include   compio.inc
          include   cntio.inc
.begin patch 1.04
          INCLUDE   NSEL2IO.INC
.end patch 1.04
          inc       comlogic.inc
          