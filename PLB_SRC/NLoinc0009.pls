.Nloinc0009 -- produce listing of reports in excel
pc        equ       0
          Include   COmmon.inc
          Include   Cons.inc
          include   Lincdd.inc
           include    ndatdd.inc
Release   init      "1.0"
Reldate   Init      "2014 April 1" New
File      File
Output    File
From      Dim       55
To        Dim       55
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
.
.Formatting vars needed
SheetsDefault integer 4,"0x00000000"
xlMinimized integer 4,"0xFFFFEFD4"
xlMaximized integer 4,"0xFFFFEFD7"
xlAlignCenter integer 4,"0xffffeff4"
xlUnderlineStyleSingle integer 4,"0x2"
MedThick integer 4,"0xFFFFEFD6"
.
AllMargin     variant
xlColWidth    variant
xlLandscape integer 4,"0x2"                     .2
xlAlignRight integer 4,"0xFFFFEFC8"
xlAlignLeft integer 4,"0xFFFFEFDD"
xlPaperLegal integer 4,"0x5"


SReturn       init            0x0a                                                        .soft return/line feed
LOText        dim             100
range         dim             20
range2        dim             20
XLSName    Dim        255



           Clock      Date to Today
          Open      File,"e:\data\text\ListIncome.dat|NINS1:502"
CreateSheet
.Create the Variant objects
.Booleans
               create  OTRUE,VarType=VT_BOOL,VarValue=1
               create  OFALSE,VarType=VT_BOOL,VarValue=0
               create  Zoom80,VarType=VT_I4,VarValue=80
."1" increment in Excel interface equals "1.3888" in OLE logic
               create         AllMargin,VarType=VT_R8,VarValue="18"                       .Roughly equals .25 inches:  18 * 1.388 = 25
               create         xlColWidth,VarType=VT_R8,VarValue="0.0"                     .Default
.
               create         xlRowHeight,VarType=VT_R8,VarValue="75.0"
.Open Excel application
               create  ex
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
               setprop sheet.PageSetup,*Orientation=xlLandscape
               setprop sheet.PageSetup,*Zoom=Zoom80
               setprop sheet.PageSetup,*TopMargin=AllMargin
               setprop sheet.PageSetup,*BottomMargin=AllMargin
               setprop sheet.PageSetup,*RightMargin=AllMargin
               setprop sheet.PageSetup,*LeftMargin=AllMargin
.               //Using xlColWidth for dual purposes!!
               setprop sheet.PageSetup,*HeaderMargin=xlColWidth
               setprop sheet.PageSetup,*FooterMargin=xlColWidth
               pack    str11,"1:8"
               setprop sheet.PageSetup,*PrintTitleRows=str11
               setprop sheet.PageSetup,*PaperSize=xlPaperLegal

               setprop        sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
               sheet.range("A1:E1").Merge
          Pack      Taskname from "=Hyperlink(#"http://www.namesinthenews.com#",#"Names in the News#")"
           setprop         sheet.Range("a1:c1"),*Formula=taskname
               sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75

           setprop    ex,*visible="True"
               
......................................................
.master heading
           setprop    sheet.Range("f3"),*Value="Date: "                       
           setprop    sheet.Range("g3"),*Value=today,*HorizontalAlignment=xlAlignRight,*NumberFormat="MMMM DD, YYYY"
           
......................................................
.detail heading
               setprop        sheet.Range("A5"),*Value="List ##"
               setprop        sheet.Range("B5"),*Value="List Name"
               setprop        sheet.Range("C5"),*Value="Report Type"
               setprop        sheet.Range("D5"),*Value="Date Select"
               setprop        sheet.Range("e5"),*Value="Accrual"
               setprop        sheet.Range("f5"),*Value="Auto."
               setprop        sheet.Range("G5"),*Value="Sent To"
.
.
               setprop xlRowHeight,VarValue="27.0"
               setprop        sheet.range("A3:S5").Rows,*RowHeight=xlRowHeight
.Header Formatting
               setprop        sheet.Range("A2:S5"),*HorizontalAlignment=xlAlignCenter
               setprop        sheet.Range("A2:S5").Font,*Bold="True"
.               //Setting up 2 sets of Borders so that user is clear that
.               //the second portion does not actually print
               sheet.range("A12:G12").BorderAround using *LineStyle=1,*Weight=MedThick
               move           "5",howmany
.          
Loop      Loop
          read      File,seq;LIncVARS
          until     over
           packkey    ndatfld from LIncList
           call       ndatkey
.LR
           add        c1,Howmany
           move       howmany,str9
           call       Trim using str9
           pack       str12,"A",str9
           setprop    sheet.Range(str12),*Value=LIncList

           call       TRim using MLstName
           move       howmany,str9
           call       Trim using str9
           pack       str12,"B",str9
           setprop    sheet.Range(str12),*Value=Mlstname

.report type
           move       howmany,str9
           call       Trim using str9
           pack       str12,"C",str9
           if         (LIncREP1 = "M")
           setprop    sheet.Range(str12),*Value="Monthly"
           elseif     (LIncREP1 = "Q")
           setprop    sheet.Range(str12),*Value="Quarterly"
           endif
.date used
           move       howmany,str9
           call       Trim using str9
           pack       str12,"D",str9
           if         (LINCDATEBY = "M")
           setprop    sheet.Range(str12),*Value="Mail Date"
           elseif     (LINCDATEBY = "O")
           setprop    sheet.Range(str12),*Value="Order Date"
           endif
.accrual
           move       howmany,str9
           call       Trim using str9
           pack       str12,"E",str9
           if         (LIncTYPE = "C")
           setprop    sheet.Range(str12),*Value="Cash"
           elseif     (LIncTYPE = "I")
           setprop    sheet.Range(str12),*Value="Invoice"
           endif

.auto
           move       howmany,str9
           call       Trim using str9
           pack       str12,"F",str9

           if         (LIncAuto = "Y")
           setprop    sheet.Range(str12),*Value="Yes"
           else
           setprop    sheet.Range(str12),*Value="No"
           endif
.Email
           call       TRim using LIncRECIPIENT
           move       howmany,str9
           call       Trim using str9
           pack       str12,"g",str9
           setprop    sheet.Range(str12),*Value=LIncRECIPIENT

          repeat          

        sheet.range("a5",str12).Columns.Autofit  

.if in excel mode we need to save the file
CampaignFileNameSelect
           Clear      Taskname
           clear   str45
           append  "LO Inc",str45
           append  "_",str45
           clock     timestamp,timestamp
           append  "_",str45
           append    timestamp,str45
           reset   str45
           Append    "C:\WORK\",taskname                          ."
           APPEND     STR45,TASKNAME
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
                      if        (#ver = c1)
                      append  ".xlsx",taskname
                      else
           append  ".xls",taskname
                    endif
                    Reset     Taskname                              
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
                              Move       Taskname,XLSName
           Shutdown
          
......................................................
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
        endif
.Send them back to select another File name and try to Save again.
        goto CampaignFileNameSelect

          include    ndatio.inc
          include   Lincio.inc
          include   Comlogic.inc
