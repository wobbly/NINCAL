PC            EQU             0
........................................
. Program:      NCLT0002.PLS
. Function:     Print button to excel for comp0001 4th screen Associated Website Mailers Consultant/Mailer
. Author:       David Strahan
. Date:         February 16,2006
. Release:      1.0
. Notes:
........................................

.
              include         common.inc
              include                        ncltdd.inc
              include         cons.inc
              include                        compdd.inc
              include                        cntdd.inc
.
Release       Init            "1.0"
hold           dim            200
MonthYearDay  dim             10
ConsultClient dim             12             // will sort on this value consult. + client
previousConsult               dim            6  // value of most recently read consultant
//RecType      dim            50
SReturn        init           0x0a                     ;soft return/line feed
.
ex      automation      class="Excel.Application"
books   automation
book    automation
sheets  automation
sheet   automation
range1        automation
ListViewtemp  listview  // create LV to sort records
.
.Variant objects used to talk to outside applications
.Booleans
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
VT_I4   EQU 3           .4 byte integer
Zoom85  variant
VT_R8         EQU 5           .Double - 8 byte Real
xlRowHeight   variant
.
.Formatting vars needed
xlMinimized integer 4,"0xFFFFEFD4"
xlMaximized integer 4,"0xFFFFEFD7"
xlDouble    integer 4, "0x2"

xlLeft integer 4,"0xffffefDD"
xlTop integer 4,"0xffffefc0"
xlAlignCenter integer 4,"0xffffeff4"
xlUnderlineStyleSingle integer 4,"0x2"
.Program MUST be called!!!
              shutdown
.
LoadListViewColumns routine  // enter here
.
              CREATE          ListViewtemp=1:50:1:50,SORTHEADER=1,SORTORDER=1:
               visible=0,ENABLED=0
              ListViewtemp.InsertColumn using "sort consultant+client", 0,0
              ListViewtemp.InsertColumn using "record", 0,1
. now load the ListView
.Initialize for multiple runs
              call            NCLTCLOSE
              loop
               move           "NCLTSEQ",Location
               pack           KeyLocation,"Key: SEQ"
               call           NCLTSEQ
               until over
               pack ConsultClient, NCLTCONSULT, NCLTCLIENT  // will be column to sort listview
               // now get flags for type, along with Company Name
               move           NCLTCONSULT,COMPFLD
               move           "Body-COMPKEY",Location
               pack           KeyLocation,"Key: ",COMPFLD
               call           COMPKEY
.
               pack           hold, NCLTCONSULT, NCLTCLIENT, NCLTNUM, NCLTSDATE, NCLTEDATE, NCLTTYPE, COMPCLRFLG, COMPMLRFLG,COMPBRKFLG,COMPOWNFLG,COMPMNGFLG,COMPSVBFLG,COMPCOMP
.
               ListViewtemp.InsertItem giving result using hold
               // creates new row and assigns value to 1st column
               ListViewtemp.SetItemText using result,ConsultClient, 1 // col 2
              repeat
              ListViewtemp.GetItemCount giving howmany
              if (howmany <= 0)
              //If for some strange reason the file is empty, do not even continue
               return
              endif
              // sort the listview on consult + client numbers
              ListViewTemp.SortColumn using 1,3  // numeric ascending sort
.Create the Variant objects
.Booleans
              create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
        create  Zoom85,VarType=VT_I4,VarValue=1
        create xlRowHeight,VarType=VT_R8,VarValue="75.0"
.Open Excel application
              create  ex
        setprop ex,*Visible="FALSE"
.Create Workbooks collection
              getprop ex,*Workbooks=books
.Create/Add a single Workbook
              books.add
              books.item giving book using 1
.Create Sheets collection
              getprop book,*workSheets=sheets
.Create a single Worksheet - we did not need to add it as we set the default above to
.add one new Worksheet each time a Workbook is created.
              sheets.item giving sheet using 1
              setprop         sheet.pagesetup,*PrintGridlines=OTRUE
              setprop         sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
              sheet.range("A1:E1").Merge
              sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
.ColumnHeaders
              setprop sheet.range("A4"), *Value="Consultant Number" ,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("B4"), *Value="Consultant Name" ,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("C4"), *Value="Type" ,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("D4"), *Value="Client Number" ,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("E4"), *Value="Client Name" ,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("F4"), *Value="ID" ,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("G4"), *Value="Start Date" ,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("H4"), *Value="End Date" ,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("I4"), *Value="Special Access" ,*HorizontalAlignment=xlAlignCenter
.
              sheet.range("A4:I4").BorderAround using *LineStyle=1,*Weight=3
              setprop sheet.range("A4:I4").Font,*Name="Arial", *Size=10
        setprop sheet.range("A4:I4").Font,*Bold="True"
.
.Body
// start filling excel sheet at row 6
three
              move C6, N5
              move            SEQ,result
              loop
               move           result,N9
               ListViewtemp.GetNextItem giving result using 0,N9
               until (result = SEQ)
               ListViewtemp.GetItemText giving hold using result, 0 // use col 0
               unpack hold, NCLTCONSULT, NCLTCLIENT, NCLTNUM, NCLTSDATE, NCLTEDATE, NCLTTYPE, COMPCLRFLG, COMPMLRFLG,COMPBRKFLG,COMPOWNFLG,COMPMNGFLG,COMPSVBFLG,COMPCOMP
               if (NCLTCONSULT <> "")
                              move N5, str10
                              //have we entered this consultant yet?
                              //if so, leave number field blank
                              if (previousConsult = NCLTCONSULT)
                                             move "      ", NCLTCONSULT  // NCLTCONSULT=blanks will be check later
                              else
                                             move NCLTCONSULT,previousConsult
                              endif

                              call trim using str10
                              pack str15, "A", str10
                              setprop sheet.range(str15),*NumberFormat="@",*Value=NCLTCONSULT
                              clear          taskname
                              pack str15, "B", str10
                              if (NCLTCONSULT <> "      ")
                                             //COMPKEY called during ListView load
                                             call           Trim using COMPCOMP
                                             setprop sheet.range(str15),*Value=COMPCOMP
.
                                             if (COMPCLRFLG="T")
                                                            append "Consultant", taskname
                                             else
                                                            pack str15, "A", str10
                                                            setprop sheet.range(str15).Font,*ColorIndex=3
                                                            pack str15, "B", str10
                                                            setprop sheet.range(str15).Font,*ColorIndex=3
                                                            pack str15, "C", str10
                                                            setprop sheet.range(str15).Font,*ColorIndex=3
                                             endif
                                             if (COMPMLRFLG="T" & taskname <> "")
                                                            append SReturn, taskname
                                                            append "Mailer", taskname
                                             else if (COMPMLRFLG="T")  // taskname is empty
                                                            append "Mailer", taskname
                                             endif
                                             if (COMPBRKFLG="T" & taskname <> "")
                                                            append SReturn, taskname
                                                            append "Broker", taskname
                                             else if (COMPBRKFLG="T")  // taskname is empty
                                                            append "Broker", taskname
                                             endif
                                             if (COMPOWNFLG="T" & taskname <> "")
                                                            append SReturn, taskname
                                                            append "Owner", taskname
                                             elseif (COMPOWNFLG="T")
                                                            append "Owner", taskname
                                             endif
                                             if (COMPMNGFLG="T" & taskname <> "")
                                                            append SReturn, taskname
                                                            append "Manager", taskname
                                             elseif (COMPMNGFLG="T")
                                                            append "Manager", taskname
                                             endif
                                             if (COMPSVBFLG="T" & taskname <> "")
                                                            append SReturn, taskname
                                                            append "Service B", taskname
                                             elseif (COMPSVBFLG="T")
                                                            append "Service B", taskname
                                             endif
                                             if (taskname <> "")
                                                            reset taskname // this will put fp at 1 - ok if we have valid info in taskname
                                             endif                                        // not what we'd want if taskname was supposed to be clear
                                             call Trim using taskname
                              else
                              //do nothing
                              endif
              // put type code here
                              pack str15, "C", str10
                              setprop sheet.range(str15),*Value=taskname
.
                              pack str15, "D", str10
                              call trim using NCLTCLIENT
                              setprop sheet.range(str15),*NumberFormat="@",*Value=NCLTCLIENT

                              pack str15, "E", str10
                              pack           COMPFLD,NCLTCLIENT
                              move           "C.L.Web-COMPKEY",Location
                              pack           KeyLocation,"Key: ",COMPFLD
                              call           COMPKEY
                              call           Trim using COMPCOMP
                              setprop sheet.range(str15),*Value=COMPCOMP
                              pack str15, "F", str10
                              setprop sheet.range(str15),*NumberFormat="@", *Value=NCLTNUM
                              pack str15, "G", str10
                              if (NCLTSDATE<>"        ")
                                             unpack NCLTSDATE, cc,yy,mm,dd
                                             pack MonthYearDay,mm,"/",dd,"/",cc,yy
                                             call trim using MonthYearDay
                              else
                                             clear          MonthYearDay
                              endif
                              setprop sheet.range(str15),*Value=MonthYearDay
                              pack str15, "H", str10
                              if (NCLTEDATE<>"        ")
                                             unpack NCLTEDATE, cc,yy,mm,dd
                                             pack MonthYearDay,mm,"/",dd,"/",cc,yy
                                             call trim using MonthYearDay
                              else
                                             clear          MonthYearDay
                              endif
                              setprop sheet.range(str15),*Value=MonthYearDay
                              pack str15, "I", str10
                              call trim using NCLTTYPE
                              if (NCLTTYPE="1")
                                             move "Y", NCLTTYPE
                              endif
                              setprop sheet.range(str15),*Value=NCLTTYPE
                              add C1, N5     .// move to next row
               endif
              repeat
four
......Formatting.......
.Autofit
              move            N5,str5
              call            Trim using str5
              pack            str25,"A4:A",str5
              sheet.range(str25).Columns.Autofit
              pack            str25,"B4:B",str5
              sheet.range(str25).Columns.Autofit
              pack            str25,"C4:C",str5
              sheet.range(str25).Columns.Autofit
              pack            str25,"D4:D",str5
              sheet.range(str25).Columns.Autofit
              pack            str25,"E4:E",str5
              sheet.range(str25).Columns.Autofit
              pack            str25,"F4:F",str5
              sheet.range(str25).Columns.Autofit
              pack            str25,"G4:G",str5
              sheet.range(str25).Columns.Autofit
              pack            str25,"H4:H",str5
              sheet.range(str25).Columns.Autofit
              pack            str25,"I4:I",str5
              sheet.range(str25).Columns.Autofit
.Alignment
              pack            str25,"A6:I",str5
              setprop sheet.range(str25),*HorizontalAlignment=xlLeft,*VerticalAlignment=xlTop
.Font Style/Color
              pack            str25,"D6:I",str5
              setprop sheet.range(str25).Font, *Italic="True", *ColorIndex=5  // 5 for blue
.
              setprop ex,*Visible="TRUE"
CampaignFileNameSelect
        clear   taskname
.        move "c:\work\",taskname
        move  "c:\",taskname
        setprop ex,*DefaultFilePath=taskname
        pack    taskname,taskname,"members"
        ex.GetSaveAsFilename giving taskname using *InitialFilename=taskname
        if (taskname <> "0")
                movelptr taskname,N9
                reset   taskname,N9
                append  "xls",taskname
                reset   taskname
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
                trap    TrapCampaignObject if Object
                book.saveas giving N9 using *Filename=taskname
                trapclr Object
        endif
CampaignCleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
        destroy sheet
        destroy sheets
        destroy book
        destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
        setprop ex,*DisplayAlerts=OFALSE
        setprop ex,*Visible="True"
.             ex.quit
        destroy ex
        destroy ListViewtemp
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
                pack    taskname,str50," already exists and is open!!"
                alert   caution,taskname,result
.                goto CampaignFileNameSelect
        endif
.Send them back to select another File name and try to Save again.
        goto CampaignFileNameSelect
.
errortrap
.testing purposes
        getinfo exception,taskname
        return
.
Nofile
              noreturn
              return          // return to calling program
.
              include compio.inc
              include         cntio.inc
              include         comlogic.inc
              include ncltio.inc