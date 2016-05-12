pc       equ       0
         inc       common.inc
         inc       cons.inc
          include   compdd.inc
          include   cntdd.inc
          Include   Nescdd.inc
         INCLUDE   NMOaDD.INC
         INCLUDE   NMOBDD.INC
          INclude   Excel.inc
Release    INIT    "2.01"               DLH      .Excel 2013 *WindowState=xlMinimized
Reldate   Init      "2014 January 22"
.release  init      "2.00"      DLH add second pass showing non-Escrow clients and balances
.REldate   init      "08 March 2011"
.release  init      "1.21"      DLH remove Kim
.REldate   init      "30 October 2009"
.release  init      "1.2"      DLH add Beth preparing to remove Kim
.REldate   init      "21 October 2009"
.release  init      "1.1"      DLH sift detail for more accurate ESCROW Balance not entire balance
.                             and add new "type"
.REldate   init      "3 June 2009"
.release  init      "1.0"      DLH Weekly Escrow report
.REldate   init      "20 April 2009"
OUTPUT    IFILE      Keyl=8
CHANGE   FORM      7.2         CHANGE TO BE APPLIED TO BALANCE.
input    form      6
count    form      6
ques     init      "???????????????"
quotcom  init      "#","
comquot  init      ",#""
check    form       6
lastmoa  dim        6
HoldBrk             Dim       4
HoldMLr             Dim       4
HOldBrkNme          Dim       55
HoldMlrNme          Dim       55
.begin patch 1.1
EscBal    Form      7.2
.end patch 1.1
FileCheck FIle
trapcount form      4
.to find version of excel  DH 04/02/09

#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER

.to find version of excel

.
         MATCH     "NONA0003" TO PROGRAM   .CHAINED FROM DSINIT?
         if         equal
         Else  
         MOVE      "NONA0003" TO PROGRAM
         CLOCK     DATE TO Today
         ENDIF

.         clock     date to today
         move      "exit" to pf5
         trap      EXIT if f5
         call      paint
         call      funcdisp
         move      c1 to nmlrpath
         move      c1 to nEscpath
         move      c2 to nMObpath
         CALL      NMOBOPEN
         

         display   *p1:24,*el,"preparing output";
.begin patch 1.1
.         PREPARE    OUTPUT,"c:\work\mobHold.dat","C:\work\Mobhold.isi","8","21"
         PREPARE    OUTPUT,"c:\work\mobHold.dat","C:\work\Mobhold.isi","8","31"
.end patch 1.1

.build book here
CreateSheet
.Force Excel Option!!!
.Create the Variant objects
.Booleans
               create  Zoom80,VarType=VT_I4,VarValue=80
."1" increment in Excel interface equals "1.3888" in OLE logic
               create         AllMargin,VarType=VT_R8,VarValue="18"                       .Roughly equals .25 inches:  18 * 1.388 = 25
               create         xlColWidth,VarType=VT_R8,VarValue="0.0"                     .Default
.
               create         xlRowHeight,VarType=VT_R8,VarValue="75.0"
.Open Excel application
               create  ex
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF

              getprop ex,*SheetsInNewWorkbook=SheetsDefault
.begin patch 2.0
.                      setprop ex,*SheetsInNewWorkbook=C1
                      setprop ex,*SheetsInNewWorkbook=C2
.end patch 2.0
.begin patch 2.01
.               setprop ex,*WindowState=xlMinimized
.end patch 2.01
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
               //Using xlColWidth for dual purposes!!
               setprop sheet.PageSetup,*HeaderMargin=xlColWidth
               setprop sheet.PageSetup,*FooterMargin=xlColWidth
               pack    str11,"1:8"
               setprop sheet.PageSetup,*PrintTitleRows=str11
               setprop sheet.PageSetup,*PaperSize=xlPaperLegal
               setprop        sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
               sheet.range("A1:E1").Merge
               sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
               Setprop sheet,*Name="Escrow"
......................................................
               setprop        sheet.Range("A8"),*Value="Consultant / Broker"
               setprop        sheet.Range("B8"),*Value="Mailer Name"
               setprop        sheet.Range("C8"),*Value="Consultant ##"
               setprop        sheet.Range("D8"),*Value="Mailer ##"
.begin patch 1.1
               setprop        sheet.Range("E8"),*Value="Total Bal"
               setprop        sheet.Range("F8"),*Value="Escrow Bal"
               setprop        sheet.Range("G8"),*Value="Account Type"
               setprop        sheet.Range("H8"),*Value="Old Consultant ##"
               setprop        sheet.Range("I8"),*Value="Old Mailer ##"
.               setprop        sheet.Range("E8"),*Value="Balance"
.               setprop        sheet.Range("F8"),*Value="Account Type"
.               setprop        sheet.Range("G8"),*Value="Old Consultant ##"
.               setprop        sheet.Range("H8"),*Value="Old Mailer ##"
.
               setprop xlRowHeight,VarValue="27.0"
.               setprop        sheet.range("A8:H8").Rows,*RowHeight=xlRowHeight
               setprop        sheet.range("A8:I8").Rows,*RowHeight=xlRowHeight
.Header Formatting
               setprop        sheet.Range("A8:I8"),*HorizontalAlignment=xlAlignCenter
               setprop        sheet.Range("A8:I8").Font,*Bold="True"
.               setprop        sheet.Range("A8:H8"),*HorizontalAlignment=xlAlignCenter
.               setprop        sheet.Range("A8:H8").Font,*Bold="True"
               //Setting up 2 sets of Borders so that user is clear that
               //the second portion does not actually print
.               sheet.range("A8:H8").BorderAround using *LineStyle=1,*Weight=MedThick
               sheet.range("A8:I8").BorderAround using *LineStyle=1,*Weight=MedThick
.end patch 1.1
               move           C8,howmany
.         display   *p1:24,*el,"Lets get to it";

loop     
          call      NEscSeq
          goto      Eoj if Over
.          if        (nescMlr = "000780")
.          call      debug
.          endif

          clear          brcomp
          Clear     HoldBrk
          Clear     HoldBrkNme
          packkey        Compfld from nEscBrk
          rep       Zfill,Compfld
          call           CompKey
          if        Not over
          Move                COMPOLDBRK,HoldBrk
          move                COmpCOmp,HoldBrkNme
          endif
          
          clear          Mcomp
          Clear     HoldMlr
          Clear     HoldMlrNme
          packkey        Compfld from nEscMlr
          rep       Zfill,Compfld
          call           CompKey
          Move      COMPOLDMlr,Holdmlr
          move      COmpCOmp,HoldMLrNme



          move      c0,balance
          Packkey   Nmoafld4 from HOldbrk,HoldMLr
          rep       Zfill,Nmoafld4
.          call      debug
          READ      NMOBFLE2,NMOAFLD4;nmobmlr,nmobmcnt,BALANCE,nmobbrk
          goto      Loop if over
.begin patch 1.1
          if        (NescType = "3")
          move      "Acquisition",str25
          Elseif        (NescType = "2")
.          if        (NescType = "2")
.end patch 1.1
          move      "Monthly",str25
          Elseif    (NescType = "1")
          move      "Quarterly",str25
          else
          move      B25,Str25
          endif
          
...................................................

write
.first lets not duplicate
          read      output,nmoafld4;str1;
          if        Over
.begin patch 1.1   search for just Escrow entries
          move      c0,escBal
          if        (Balance <> 0)
          move      c4,nmoapath
          call      Nmoakey
          call      Calcit                    
          loop
          call      Nmoaks
          pack      str8 from Nmoabrk,mlr
          rep       zfill,str8
                    if        (str8 <> Nmoafld4)
                    break
                    endif
                    call      Calcit                    

          until     over
          repeat
          endif
          if        (Nmoafld4 = "00000006")
          call      debug
.01 sep 2011 desparate attempt to make SPLC work
.          sub       "107980.08" from escbal
.01 sep 2011 desparate attempt to make SPLC work
          call      debug
          endif
          if        (EscBal > c0)
          move      c0,EscBal
          endif
          if        (balance > EscBal)
          move      Balance,EscBal
          endif
          
.          Write     Output,Nmoafld4;nmobmlr,nmobmcnt,BALANCE,nmobbrk
          Write     Output,Nmoafld4;nmobmlr,nmobmcnt,BALANCE,nmobbrk,EscBal
.end patch 1.1
          
              add             C1,howmany
              move            howmany,str9
              call            Trim using str9
              pack            str12,"A",str9
              setprop         sheet.Range(str12),*Value=HoldBrkNme
              pack            str12,"B",str9
              setprop         sheet.Range(str12),*Value=HoldMlrNme
              pack            str12,"C",str9
              setprop         sheet.Range(str12),*Value=NescBrk
              pack            str12,"D",str9
              setprop         sheet.Range(str12),*Value=Nescmlr
              pack            str12,"E",str9
              setprop         sheet.Range(str12),*Value=Balance,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.begin patch 1.1
              pack            str12,"F",str9
              setprop         sheet.Range(str12),*Value=EscBal,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
              pack            str12,"G",str9
              setprop         sheet.Range(str12),*Value=str25
              pack            str12,"H",str9
              setprop         sheet.Range(str12),*Value=holdBrk
              pack            str12,"I",str9
              setprop         sheet.Range(str12),*Value=holdmlr
.              pack            str12,"F",str9
.              setprop         sheet.Range(str12),*Value=str25
.              pack            str12,"G",str9
.              setprop         sheet.Range(str12),*Value=holdBrk
.              pack            str12,"H",str9
.              setprop         sheet.Range(str12),*Value=holdmlr
.end patch 1.1
              
         add       c1 to count
         display   *p1:12,"written: ",count,b1,mm,slash,dd,slash,yy
         Else
         endif
         goto     loop

.begin patch 2.0
Eoj
.eoj      WEOF     OUTPUT,SEQ
.         CLOSE    OUTPUT
.end patch 2.0
         move      yes to str1

          call      Trim using str9
.begin patch 1.1
          move      str9,str10
.end patch 1.1
          pack      str20 from "=Sum(E9:E",str9,")"
          call      Trim using str20
.         display   *p1:16,"written: ",str20

          add       c2,howmany
          move      howmany,str9
          call      Trim using str9
          pack            str12,"E",str9
          setprop   sheet.range(Str12),*Value=str20,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"

.begin patch 1.1
          call      Trim using str10
          pack      str20 from "=Sum(F9:F",str10,")"
          call      Trim using str20

          pack            str12,"F",str9
          setprop   sheet.range(Str12),*Value=str20,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.
          pack            str12,"B",str9
          setprop   sheet.range(Str12),*Value="Totals:"
          setprop   sheet.Range(STR12).Font,*Bold="True"   
.          
.subtotals
.quarterly
          call      Trim using str10
          pack      taskname from "=SumIF(G9:G",str10,",#"Quarterly#",","F9:F",str10,")"            ."
          call      Trim using taskname

          add       c2,howmany
          move      howmany,str9
          call      Trim using str9
          pack            str12,"F",str9
          setprop   sheet.range(Str12),*Value=taskname,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
          pack            str12,"E",str9
          setprop   sheet.range(Str12),*Value="Quarterly:"
          setprop   sheet.Range(STR12).Font,*Bold="True"   
.          
.Monthly
          call      Trim using str10
          pack      taskname from "=SumIF(G9:G",str10,",#"Monthly#",","F9:F",str10,")"            ."
          call      Trim using taskname

          add       c1,howmany
          move      howmany,str9
          call      Trim using str9
          pack            str12,"F",str9
          setprop   sheet.range(Str12),*Value=taskname,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
          pack            str12,"E",str9
          setprop   sheet.range(Str12),*Value="Monthly:"
          setprop   sheet.Range(STR12).Font,*Bold="True"   
.
.Acquisition
          call      Trim using str10
          pack      taskname from "=SumIF(G9:G",str10,",#"Acquisition#",","F9:F",str10,")"            ."
          call      Trim using taskname

          add       c1,howmany
          move      howmany,str9
          call      Trim using str9
          pack            str12,"F",str9
          setprop   sheet.range(Str12),*Value=taskname,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
          pack            str12,"E",str9
          setprop   sheet.range(Str12),*Value="Acquisition:"
          setprop   sheet.Range(STR12).Font,*Bold="True"   


.end patch 1.1

                              pack    range,"A8"
.begin patch 1.1
.                              pack    range2,"H",str9
                              pack    range2,"I",str9
                              sheet.range(range,range2).Columns.Autofit
.Establish Print Area
.                              pack           str25,"A1:H",str9
                              pack           str25,"A1:I",str9
                              setprop        sheet.PageSetup,*PrintArea=str25
.end patch 1.1
.begin patch 2.0
          Move      c0,nmobflag                         .for reopening if balance file
.add one new Worksheet
               sheets.item giving sheet using 2
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
               sheet.range("A1:E1").Merge
               sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
               Setprop sheet,*Name="Suspense"
......................................................
               setprop        sheet.Range("A8"),*Value="Consultant / Broker"
               setprop        sheet.Range("B8"),*Value="Mailer Name"
               setprop        sheet.Range("C8"),*Value="Consultant ##"
               setprop        sheet.Range("D8"),*Value="Mailer ##"
               setprop        sheet.Range("E8"),*Value="Total Bal"
               setprop        sheet.Range("F8"),*Value="Escrow Bal"
               setprop        sheet.Range("G8"),*Value="Account Type"
               setprop        sheet.Range("H8"),*Value="Old Consultant ##"
               setprop        sheet.Range("I8"),*Value="Old Mailer ##"
.
               setprop xlRowHeight,VarValue="27.0"
               setprop        sheet.range("A8:I8").Rows,*RowHeight=xlRowHeight
.Header Formatting
               setprop        sheet.Range("A8:I8"),*HorizontalAlignment=xlAlignCenter
               setprop        sheet.Range("A8:I8").Font,*Bold="True"
               sheet.range("A8:I8").BorderAround using *LineStyle=1,*Weight=MedThick
               move           C8,howmany

loop2
          call      Nmobseq
          goto      Eoj2 if over
          if        (Balance = c0)
          goto      Loop2
          endif

          Packkey   Nmoafld4 from Nmobbrk,Nmobmlr
          rep       Zfill,Nmoafld4

          read      output,nmoafld4;str1;
          Goto      Loop2 if not Over

.          call      debug
          clear     brcomp
          Clear     HoldBrk
          Clear     HoldBrkNme
          packkey        Nbrkfld from nmobbrk
          rep       Zfill,Nbrkfld
          call           Nbrkkey
          if        Not over
          Move                COMPOLDBRK,HoldBrk
          move                COmpCOmp,HoldBrkNme
          endif
          
          clear          Mcomp
          Clear     HoldMlr
          Clear     HoldMlrNme
          packkey        Mkey from nmobmlr
          rep       Zfill,Mkey
          call           Nmlrkey
          Move      COMPOLDMlr,Holdmlr
          move      COmpCOmp,HoldMLrNme
          MOve      "On Account",str25
          move      C0,EscBal
              add             C1,howmany
              move            howmany,str9
              call            Trim using str9
              pack            str12,"A",str9
              setprop         sheet.Range(str12),*Value=HoldBrkNme
              pack            str12,"B",str9
              setprop         sheet.Range(str12),*Value=HoldMlrNme
              pack            str12,"C",str9
              setprop         sheet.Range(str12),*Value=NMobBrk
              pack            str12,"D",str9
              setprop         sheet.Range(str12),*Value=NMobmlr
              pack            str12,"E",str9
              setprop         sheet.Range(str12),*Value=Balance,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
              pack            str12,"F",str9
              setprop         sheet.Range(str12),*Value=EscBal,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
              pack            str12,"G",str9
              setprop         sheet.Range(str12),*Value=str25
              pack            str12,"H",str9
              setprop         sheet.Range(str12),*Value=holdBrk
              pack            str12,"I",str9
              setprop         sheet.Range(str12),*Value=holdmlr
          Goto      Loop2
Eoj2
          call      Trim using str9
          move      str9,str10
          pack      str20 from "=Sum(E9:E",str9,")"
          call      Trim using str20
.         display   *p1:16,"written: ",str20

          add       c2,howmany
          move      howmany,str9
          call      Trim using str9
          pack            str12,"E",str9
          setprop   sheet.range(Str12),*Value=str20,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"

          call      Trim using str10
          pack      str20 from "=Sum(F9:F",str10,")"
          call      Trim using str20

          pack            str12,"F",str9
          setprop   sheet.range(Str12),*Value=str20,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.
          pack            str12,"B",str9
          setprop   sheet.range(Str12),*Value="Totals:"
          setprop   sheet.Range(STR12).Font,*Bold="True"   

.end patch 2.0
.begin patch 1.1 --- was in workbook creation area - moved as we reused taksname
          Clear     Taskname
          Append    "C:\WORK\WeeklyEscrow",taskname    
                              if        (#ver = c1)
                              append  ".xlsx",taskname
                              else
                              append  ".xls",taskname
                              endif
                              Reset     Taskname                              
                             erase          taskname
.end patch 1.1


                              book.saveas giving N9 using *Filename=taskname
 
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
                              move    "Here is your Weekly Escrow File in Excel",MailSubjct
                              pack    MailBody,"Input File:  ",INPNAME
                              pack      MailBOdy,"Input File:  ",INPNAME
.                              Pack      MailTO from "GemmaSpranza@nincal.com,Kimtine.Ki@lakegroupmedia.com,beth@bghbookkeeping.com"
                              Pack      MailTO from "GemmaSpranza@nincal.com,creques@nincal.com"
                              Pack      MailCC from "dherric@nincal.com"
.                              Pack      MailTO from "dherric@nincal.com"

                              Pack      MailFrom from "creques@nincal.com"
                              
                              Pack      MailAttach from taskname
                              Move      c0,TrapCount                   .reset

CheckFile

                              trap      WaitForEnd giving error if IO
                              open      FileCheck,MailAttach,Exclusive          
                              Close     FIleCHeck

                              call      SendMail
         
EXIT     stop
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
          
                    goto      checkfile
Calcit
          if        (MLr = "0006")
.          call      debug
          endif
          
          if        (mlr = "1498" | mlr = "0014" | mlr = "0774" | mlr = "5794" | mlr = "1921" | mlr = "0774" | mlr = "5791" | mlr = "6912")         .old accounts
                    if        (reason = "19" | reason = "22" | Reason = "99" | Reason = "16")
                    add       ONAMOUNT,EscBal
                    endif
          Else
                    if        (TRANDATE > "20090401" & (reason = "19" | reason = "22" | Reason = "99" | Reason = "16"))
                    add       ONAMOUNT,EscBal
                    endif
          Endif
          REturn

         INCLUDE   NMOaIO.INC
         INCLUDE   NMOBIO.INC
         include   compio.inc
         include   cntio.inc
         include   nescio.inc
         inc       comlogic.inc                 
