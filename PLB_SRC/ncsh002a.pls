.SHORT PAYMENT NOTICE MS WORD DOCUMENT
pc        equ       0
        include common.inc
        include cons.inc

release   init    "1.8"       DLH use New line instead of Carriage Return Word 2007 treats CR as new paragragh
.                                       we want Sift+enter newline
RElDate   Init      "18 May 2009"
.release   init    "1.7"       DLH double check for company code
.RElDate   Init      "11 December 2008"
.release  init    "1.6"       DLH Logo
.RElDate  Init      "16 October 2008"
.release init    "1.5"        ASH 16MAY2006 RECONCILIATION OF LANGUAGE BETWEEN:  NCSH002A, NORD002L, NINV002L, NADJ002L, NORD0024, NORD024B
.release init    "1.4"        ASH 06FEB2006 Sarah Tan out on maternity leave
.release init    "1.3"        ASH 05AUG2004 LOGO CONVERSION
.release init    "1.2"        ASH 06JUN2003 SMALL FIX
.release init    "1.1"        ASH 07MAR2003 ADDED MS WORD FORMAT OF ADJUSTMENT INVOICE
.release init    "1.0"        ASH 20FEB2003 NEW RELEASE


.EXTERNAL ROUTINES FROM       NADJ002L.PLC
PrintMSWordInvoice external "NADJ002L;PrintMSWordInvoice"

Docs      Automation
Doc       Automation
Range     Automation
ex        Automation          class="Word.Application"

.will need to add code for word 2013 see sunbelt forum
Window    AUTOMATION
View      AUTOMATION  
wdNormalView        INTEGER   4,"0x1"

DimPtr    Dim       ^
DimPtr1   Dim       ^
DimPtr2   Dim       ^
DimPtr3   Dim       ^
DimPtr4   Dim       ^
.START PATCH 1.1 ADDED LOGIC
DimPtr5   Dim       ^
.END PATCH 1.1 ADDED LOGIC
.Begin PATCH 1.6
FrmPtr1   Form      ^
.end PATCH 1.6
CRRet     Init    0x0D
.begin patch 1.8
CRNewLn   Init      0x0A
.end patch 1.8           all occurances if CRRet replaced in body of code
CRTab     Init      0x09

wdWindowStateMinimize integer 4,"0x02"
.Variant objects used to talk to outside applications
.See PL/B help in order to understand use of Variant objects.
.
.Booleans
.PL/B does not have a Boolean datatype, so we have to create our own.
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant

.START PATCH 1.1 REPLACED LOGIC
.CreateShortPayNotice Routine DimPtr,DimPtr1,DimPtr2,DimPtr3,DimPtr4
.Begin PATCH 1.6
.CreateShortPayNotice Routine DimPtr,DimPtr1,DimPtr2,DimPtr3,DimPtr4,DimPtr5
CreateShortPayNotice Routine DimPtr,DimPtr1,DimPtr2,DimPtr3,DimPtr4,DimPtr5,FrmPTr1
.end PATCH 1.6
.END PATCH 1.1 REPLACED LOGIC
.Begin PATCH 1.6
          Move      FrmPtr1,Company
.end PATCH 1.6

.Create the Variant objects
.Booleans
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
.Open Word application
         create    ex
          setprop ex,*WindowState=2     .Mininmized
          setprop ex,*Visible=OFALSE
          getprop   ex,*Documents=Docs
         Docs.Add giving Doc
.will need to add code for word 2013 see sunblet forum
.           GETPROP  Docs,*ActiveWindow=Window
.           GETPROP  Window,*View=View
.           SETPROP  View,*Type=wdNormalView 




.              pack            Taskname,"\\nins1\d\Accounting\Normal.dotm"
.            docs.Open Giving Doc Using taskname
..              Doc.Open using *Filename=taskname   


.Note:  Setting a property will apply that property for all subsequent text!!
.         The End of the Range is always implied to be the last text that was entered.
          Doc.Activate
          getprop   Doc,*Content=Range
          setprop Range.Font,*Name="Times New Roman"        .Default Font Name
          setprop Range.Font,*Size=12                       .Default Font Size
          setprop Range.PageSetup,*LeftMargin=60            .Default Left Margin
          setprop Range.PageSetup,*RightMargin=60           .Default Right Margin
.testing DH
                    Setprop Range.ParagraphFormat,*LineSpacingRule="0"  ...single space
 
.          Doc.range.CloseUp 
.          setprop Range.CloseUp
.          setprop Range.ParagraphFormat,*LineSpacing=12
.          setprop Range.PageSetup,*LineSpacing=12

.START PATCH 1.3 REPLACED LOGIC
.         setprop Range,*Text="Names"
.         setprop   Range,*Start=0
.         setprop Range,*Bold=OTRUE
.         setprop Range.Font,*Size=20
..
.         Range.InsertAfter using " in the News"
.         setprop   Range,*Start=6
.         getprop   Range,*End=result
.         setprop Range,*Bold=OFALSE
.         setprop Range,*Italic=OTRUE
.         setprop   Range,*Start=0
.         setprop Range,*Underline=OTRUE
..
.         Range.InsertAfter using CRRet
.         Range.InsertAfter using "C A L I F O R N I A   I N C"
.         setprop   Range,*Start=result
.         getprop   Range,*End=result
.         setprop Range.Font,*Size=14
.         setprop Range,*Underline=OFALSE
.         setprop Range,*Italic=OFALSE
.         setprop   Range,*Start=result
.         Range.InsertAfter using CRRet
.         getprop   Range,*End=result
.         move      result,howmany
.         Range.InsertAfter using CRRet
.         Range.InsertAfter using CRRet
.         Range.InsertAfter using CRRet
................................
.Set up Header Image
.begin patch 1.6
.          If        (company = c2)
.          Doc.Shapes.AddPicture using "\\nins1\e\netutils\pacificlists.jpg"
.          else
          Doc.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg"
          Move      c1,company                   .12/11/08 DLH force it
.          endif
.         Doc.Shapes.AddPicture using "\\nts0\c\netutils\Logocolornotag.jpg"
.begin patch 1.6
          Doc.Shapes(1).ScaleHeight using ".65",OFALSE
          Doc.Shapes(1).ScaleWidth using ".65",OFALSE
.Need to set up the RelativeVerticalPosition in order for Top property to behave properly
.I have set the RelativeVerticalPosition to "wdRelativeVerticalPositionPage"=1
          setprop   Doc.Shapes(1),*RelativeVerticalPosition=1,*Top="30"
.Start Body
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using CRNewLn
          getprop   Range,*End=result
.END PATCH 1.3 REPLACED LOGIC
          Range.InsertAfter using "NOTICE OF SHORT PAYMENT"
          setprop   Range,*Start=result
          getprop   Range,*End=result
          setprop Range,*Bold=OTRUE
          Range.InsertAfter using CRNewLn
          setprop   Range,*Start=result
          setprop Range,*Bold=OFALSE
.START PATCH 1.3 ADDED LOGIC
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using CRNewLn
.END PATCH 1.3 ADDED LOGIC
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using "NIN LR #:"
          Range.InsertAfter using CRTab
          Range.InsertAfter using DimPtr
.Add 6 Tab characters, and then todays date
          Range.InsertAfter using CRTab
          Range.InsertAfter using CRTab
          Range.InsertAfter using CRTab
          Range.InsertAfter using CRTab
          Range.InsertAfter using CRTab
          Range.InsertAfter using CRTab
          getprop   Range,*End=result
          setprop   Range,*Start=result
          Range.InsertDateTime using "MMMM dd, yyyy",OFALSE
.I never figured out how to set the Range End after InsertDateTime, and so I cheated by adding "50" to it.
          getprop   Range,*End=result
          add       "50",result
          setprop   Range,*Start=result
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using "Payment Information"
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using "Check:"
          Range.InsertAfter using CRTab
          Range.InsertAfter using CRTab
          Range.InsertAfter using DimPtr1
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using "Date:"
          Range.InsertAfter using CRTab
          Range.InsertAfter using CRTab
          Range.InsertAfter using DimPtr2
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using "Amount:"
          Range.InsertAfter using CRTab
          Range.InsertAfter using "$"
          Range.InsertAfter using DimPtr3
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using "The enclosed invoice has an unpaid balance of:"
          Range.InsertAfter using CRTab
          Range.InsertAfter using "$"
          Range.InsertAfter using DimPtr4
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using "Please remit balance or supply the following information:"
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using "____ Please fax computer verification.  "
          getprop   Range,*End=result
          Range.InsertAfter using "Computer verification must identify deductions taken."
          setprop   Range,*Start=result
          getprop   Range,*End=result
          setprop Range,*Bold=OTRUE
          setprop Range,*Underline=OTRUE
          Range.InsertAfter using CRNewLn
          setprop   Range,*Start=result
          setprop Range,*Bold=OFALSE
          setprop Range,*Underline=OFALSE
          Range.InsertAfter using "____ Quantity mailed greater than net name quantity."
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using "____ Net name does not apply"
          Range.InsertAfter using CRNewLn
.START PATCH 1.5 REPLACED LOGIC
.         Range.InsertAfter using "____ Mailer did not pay for magnetic tape charges."
          Range.InsertAfter using "____ Mailer did not pay for email charges."
.END PATCH 1.5 REPLACED LOGIC
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using "____ Mailer did not pay for shipping charges."
          Range.InsertAfter using CRNewLn
.START PATCH 1.2 REPLACED LOGIC
.         Range.InsertAfter using "____ No deductions allowed on net name arrangements."
.START PATCH 1.5 REPLACED LOGIC
.         Range.InsertAfter using "____ No deductions allowed on List Management discount name arrangements"
          Range.InsertAfter using "____ No deductions allowed on List Management net name arrangements"
.END PATCH 1.5 REPLACED LOGIC
.END PATCH 1.2 REPLACED LOGIC
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using "____ Broker took 20% commission, this is a 10% list."
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using "____ 5,000 minimum applies to this order."
.START PATCH 1.5 REMOVED LOGIC
.         Range.InsertAfter using CRNewLn
.         Range.InsertAfter using "____ No deductions on volume discounts.  Must pay ____________ of names shipped."
.END PATCH 1.5 REMOVED LOGIC
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using "____ Please explain your deductions:________________________________________________"
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using "          _________________________________________________________________________"
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using "____ Incorrect math."
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using "____ Other:    __________________________________________________________________"
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using "          _________________________________________________________________________"
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using "          _________________________________________________________________________"
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using CRNewLn
          getprop   Range,*End=result
.START PATCH 1.4 REPLACED LOGIC
.         Range.InsertAfter using "If you have any questions, please call me at (510) 302-4671."
.         Range.InsertAfter using CRNewLn
.         Range.InsertAfter using "Fax all replies to (415) 433-7796.  Thank you.  Sarah Tan"
          Range.InsertAfter using "If you have any questions, please call Accounting at (415) 989-3350."
          Range.InsertAfter using CRNewLn
          Range.InsertAfter using "Fax all replies to (415) 433-7796.  Thank you.  Accounting Department"
.END PATCH 1.4 REPLACED LOGIC
          Range.InsertAfter using CRNewLn
.START PATCH 1.3 REMOVED LOGIC
.         Range.InsertAfter using CRNewLn
.         Range.InsertAfter using CRNewLn
.         Range.InsertAfter using CRNewLn
.         Range.InsertAfter using "1300 Clay Street, "
..Prep to format later.
.         getprop   Range,*End=N9
.         Range.InsertAfter using "11th"
.         getprop   Range,*End=N8
.         Range.InsertAfter using " floor, Oakland, CA 94612-1429 · (415) 989-3350 · FAX (415) 433-7796"
.         setprop   Range,*Start=0
.         setprop Range.Font,*Name="Times New Roman"        .Default Font Name
.         setprop   Range,*Start=howmany
.         setprop Range.Font,*Size=12                       .Default Font Size
..Format some stuff
.         setprop   Range,*Start=N9
.         setprop   Range,*End=N8
.         Range.AutoFormat
.END PATCH 1.3 REMOVED LOGIC
.Reset the Range
          setprop   Range,*Start=0
.Format Rows which need to be centered
.0 = Align Left
.1 = Align Center
.2 = Align Right
.**I did not set up any Paragraphs, so each line is interpreted as its' own Paragraph.
.START PATCH 1.3 REPLACED LOGIC
.         setprop   Range.Paragraphs(1),*Alignment=1
.         setprop   Range.Paragraphs(2),*Alignment=1
.         setprop   Range.Paragraphs(40),*Alignment=1
.         setprop   Range.Paragraphs(41),*Alignment=1
.         setprop   Range.Paragraphs(45),*Alignment=1
.START PATCH 1.5 REPLACED LOGIC
.         setprop   Range.Paragraphs(44),*Alignment=1
.         setprop   Range.Paragraphs(45),*Alignment=1
          setprop   Range.Paragraphs(43),*Alignment=1
          setprop   Range.Paragraphs(44),*Alignment=1
.END PATCH 1.5 REPLACED LOGIC
.END PATCH 1.3 REPLACED LOGIC
.ShortPayFileNameSelect
.         clear     taskname
.         move      "c:\work\",taskname
.         setprop   ex,*DefaultFilePath=taskname
.         ex.GetSaveAsFilename giving taskname using *InitialFilename=taskname
.         if (taskname <> "0")
.                   movelptr taskname,N9
.                   reset     taskname,N9
.                   append    "doc",taskname
.                   reset     taskname
..Trap in case a workbook with the same name is already open.  In such a case, the saveas will
..not occur
.                   trap    TrapShortPayObject if Object
.                   Doc.SaveAs using taskname
.                   trapclr Object
.         endif
.START PATCH 1.1 ADDED LOGIC
.Begin PATCH 1.6
.         call      PrintMSWordInvoice using Doc,DimPtr5
.begin patch 1.7
          call      Debug
          if        (company <> c1 & company <> c2)
          move      c1,Company
          endif
          move      Company,FRmPTr1
.         call      PrintMSWordInvoice using Doc,DimPtr5,Company
          call      PrintMSWordInvoice using Doc,DimPtr5,FrmPtr1
.end patch 1.7
.end PATCH 1.6
.END PATCH 1.1 ADDED LOGIC
ShortPayCleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.word.exe will still be running.
          destroy   Range
          destroy   Doc
          destroy   Docs
.Suppress any alert boxes produced by Word.  We want to close down this instance of Word now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Document has not been saved.  If we did not suppress these message, instances of Word might
.be left open.
          setprop   ex,*DisplayAlerts=OFALSE
          setprop ex,*WindowState=1     .Maximized
          setprop ex,*Visible=OTRUE
          destroy   ex
          return

.TrapShortPayObject
..This routine tripped when Saveas method is called.
..
..We are trapping for instances where the User has selected a filename that: 1) Already exists
..and is open by another instance of Word. 2) Already exists but not open elsewhere.  This instance
..will provoke Word to produce a message asking User if they want to overwrite the file.  If they
..answer No or Cancel they will come to this routine.  Answering Yes will overwrite the file at the
..Saveas method found in above code.
.         noreturn
.         move    taskname,str50
.         getinfo exception,taskname
.         unpack  taskname,str55,str55,str10,str55
.         scan    "Cannot access",str55
.         if equal
..Instance 1 - exists and open elsewhere
.                   pack    taskname,str50," already exists and is open!!",newline,"Select another Filename!!"
.                   alert   caution,taskname,result
..                goto ShortPayFileNameSelect
.         endif
..Send them back to select another File name and try to Save again.
.         goto ShortPayFileNameSelect

        include comlogic.inc
