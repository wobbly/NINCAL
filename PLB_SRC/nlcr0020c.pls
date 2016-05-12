          include   common.inc
          include   cons.inc
          include   norddd.inc

release   init      "1.1"     DLH   26Sep07 PLI
.release  init      "1.0"

input     file
books     automation
book      automation
sheets    automation
sheet1    automation
sheet2    automation
ex        automation      class="Excel.Application"
DateRange init      "2002"
.....
.2002
.....
Sales2002 form      9(2,12)
SLCR2002  form      9(2,12)
SOrder2002          form      9(2,12)
.
LM2002              form      9(2,12)
LLCR2002  form      9(2,12)
LPend2002 form      9(2,12)
LOrder2002          form      9(2,12)
.....
.2003
.....
Sales2003 form      9(2,12)
SLCR2003  form      9(2,12)
SOrder2003          form      9(2,12)
.
LM2003              form      9(2,12)
LLCR2003  form      9(2,12)
LPend2003 form      9(2,12)
LOrder2003          form      9(2,12)
.....
.2004
.....
Sales2004 form      9(2,12)
SLCR2004  form      9(2,12)
SOrder2004          form      9(2,12)
.
LM2004              form      9(2,12)
LLCR2004  form      9(2,12)
LPend2004 form      9(2,12)
LOrder2004          form      9(2,12)

.         goto OpenFile
          clear   taskname
        pack    taskname,"!\\nins1\e\netutils\sort32.exe \\nins1\E\DATA\TEXT\NINORD.DAT C:\WORK\NINORD.SRT /inc(202,4,n,ge,\""",DateRange,"\"") VER"
          execute   taskname
OpenFile
          open      input,"C:\WORK\NINORD.SRT",EXCLUSIVE
          loop
                    read      input,SEQ;ORDVARS
                    until over
                    pack      str4,OODTEC,OODTEY
                    pack      str2,OSALES10,OSALES
                    move      C1,N2
                    move      OODTEM,N2
                    move      C0,howmany
                    move      OQTY,howmany
                    if (str4 = "2002")
.begin patch 1,1
.                             if (str2 = "06")
                              if (str2 = "06" | str2 = "19" | str2 = "27" | str2 = "28")
.end patch 1,1
                                        add       C1,LM2002(1,N2)
                                        add       howmany,LM2002(2,N2)
                                        if (OSTAT = "l" | OSTAT = "z")
                                                  add       C1,LLCR2002(1,N2)
                                                  add       howmany,LLCR2002(2,N2)
                                        elseif (OSTAT = "p" | OSTAT = "x")
                                                  add       C1,LPend2002(1,N2)
                                                  add       howmany,LPend2002(2,N2)
                                        else
                                                  add       C1,LOrder2002(1,N2)
                                                  add       howmany,LOrder2002(2,N2)
                                        endif
                              else
                                        add       C1,Sales2002(1,N2)
                                        add       howmany,Sales2002(2,N2)
                                        if (OSTAT = "l" | OSTAT = "z" | OSTAT = "p" | OSTAT = "x")
                                                  add       C1,SLCR2002(1,N2)
                                                  add       howmany,SLCR2002(2,N2)
                                        else
                                                  add       C1,SOrder2002(1,N2)
                                                  add       howmany,SOrder2002(2,N2)
                                        endif
                              endif
                    elseif (str4 = "2003")
.begin patch 1,1
.                             if (str2 = "06")
                              if (str2 = "06" | str2 = "19" | str2 = "27" | str2 = "28")
.end patch 1,1
                                        add       C1,LM2003(1,N2)
                                        add       howmany,LM2003(2,N2)
                                        if (OSTAT = "l" | OSTAT = "z")
                                                  add       C1,LLCR2003(1,N2)
                                                  add       howmany,LLCR2003(2,N2)
                                        elseif (OSTAT = "p" | OSTAT = "x")
                                                  add       C1,LPend2003(1,N2)
                                                  add       howmany,LPend2003(2,N2)
                                        else
                                                  add       C1,LOrder2003(1,N2)
                                                  add       howmany,LOrder2003(2,N2)
                                        endif
                              else
                                        add       C1,Sales2003(1,N2)
                                        add       howmany,Sales2003(2,N2)
                                        if (OSTAT = "l" | OSTAT = "z" | OSTAT = "p" | OSTAT = "x")
                                                  add       C1,SLCR2003(1,N2)
                                                  add       howmany,SLCR2003(2,N2)
                                        else
                                                  add       C1,SOrder2003(1,N2)
                                                  add       howmany,SOrder2003(2,N2)
                                        endif
                              endif
                    elseif (str4 = "2004")
.begin patch 1,1
.                             if (str2 = "06")
                              if (str2 = "06" | str2 = "19" | str2 = "27" | str2 = "28")
.end patch 1,1
                                        add       C1,LM2004(1,N2)
                                        add       howmany,LM2004(2,N2)
                                        if (OSTAT = "l" | OSTAT = "z")
                                                  add       C1,LLCR2004(1,N2)
                                                  add       howmany,LLCR2004(2,N2)
                                        elseif (OSTAT = "p" | OSTAT = "x")
                                                  add       C1,LPend2004(1,N2)
                                                  add       howmany,LPend2004(2,N2)
                                        else
                                                  add       C1,LOrder2004(1,N2)
                                                  add       howmany,LOrder2004(2,N2)
                                        endif
                              else
                                        add       C1,Sales2004(1,N2)
                                        add       howmany,Sales2004(2,N2)
                                        if (OSTAT = "l" | OSTAT = "z" | OSTAT = "p" | OSTAT = "x")
                                                  add       C1,SLCR2004(1,N2)
                                                  add       howmany,SLCR2004(2,N2)
                                        else
                                                  add       C1,SOrder2004(1,N2)
                                                  add       howmany,SOrder2004(2,N2)
                                        endif
                              endif
                    endif
          repeat
.
;Open Excel application
          create  ex
          setprop ex,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
;Reset Default of Worksheets found in a Workbook
          setprop ex,*SheetsInNewWorkbook=C2
;Create Workbooks collection
          getprop ex,*Workbooks=books
;Create/Add a single Workbook
          books.add
          books.item giving book using 1
;Create Worksheets collection
          getprop book,*Sheets=sheets
;Create a single Worksheet - we did not need to add it as we set the default above to
;add one new Worksheet each time a Workbook is created.
          sheets.item giving sheet1 using 1
          setprop   sheet1,*Name="Records"
.
.2002 Header
          setprop sheet1.range("A1"),*Value="2002"
.
          setprop sheet1.range("C2"),*Value="Jan"
          setprop sheet1.range("D2"),*Value="Feb"
          setprop sheet1.range("E2"),*Value="Mar"
          setprop sheet1.range("F2"),*Value="Apr"
          setprop sheet1.range("G2"),*Value="May"
          setprop sheet1.range("H2"),*Value="Jun"
          setprop sheet1.range("I2"),*Value="Jul"
          setprop sheet1.range("J2"),*Value="Aug"
          setprop sheet1.range("K2"),*Value="Sep"
          setprop sheet1.range("L2"),*Value="Oct"
          setprop sheet1.range("M2"),*Value="Nov"
          setprop sheet1.range("N2"),*Value="Dec"
.
          setprop sheet1.range("A3"),*Value="Sales"
                    setprop sheet1.range("C3"),*Value=Sales2002(1,1)
                    setprop sheet1.range("D3"),*Value=Sales2002(1,2)
                    setprop sheet1.range("E3"),*Value=Sales2002(1,3)
                    setprop sheet1.range("F3"),*Value=Sales2002(1,4)
                    setprop sheet1.range("G3"),*Value=Sales2002(1,5)
                    setprop sheet1.range("H3"),*Value=Sales2002(1,6)
                    setprop sheet1.range("I3"),*Value=Sales2002(1,7)
                    setprop sheet1.range("J3"),*Value=Sales2002(1,8)
                    setprop sheet1.range("K3"),*Value=Sales2002(1,9)
                    setprop sheet1.range("L3"),*Value=Sales2002(1,10)
                    setprop sheet1.range("M3"),*Value=Sales2002(1,11)
                    setprop sheet1.range("N3"),*Value=Sales2002(1,12)
          setprop sheet1.range("B4"),*Value="LCRs"
                    setprop sheet1.range("C4"),*Value=SLCR2002(1,1)
                    setprop sheet1.range("D4"),*Value=SLCR2002(1,2)
                    setprop sheet1.range("E4"),*Value=SLCR2002(1,3)
                    setprop sheet1.range("F4"),*Value=SLCR2002(1,4)
                    setprop sheet1.range("G4"),*Value=SLCR2002(1,5)
                    setprop sheet1.range("H4"),*Value=SLCR2002(1,6)
                    setprop sheet1.range("I4"),*Value=SLCR2002(1,7)
                    setprop sheet1.range("J4"),*Value=SLCR2002(1,8)
                    setprop sheet1.range("K4"),*Value=SLCR2002(1,9)
                    setprop sheet1.range("L4"),*Value=SLCR2002(1,10)
                    setprop sheet1.range("M4"),*Value=SLCR2002(1,11)
                    setprop sheet1.range("N4"),*Value=SLCR2002(1,12)
          setprop sheet1.range("B5"),*Value="Orders"
                    setprop sheet1.range("C5"),*Value=SOrder2002(1,1)
                    setprop sheet1.range("D5"),*Value=SOrder2002(1,2)
                    setprop sheet1.range("E5"),*Value=SOrder2002(1,3)
                    setprop sheet1.range("F5"),*Value=SOrder2002(1,4)
                    setprop sheet1.range("G5"),*Value=SOrder2002(1,5)
                    setprop sheet1.range("H5"),*Value=SOrder2002(1,6)
                    setprop sheet1.range("I5"),*Value=SOrder2002(1,7)
                    setprop sheet1.range("J5"),*Value=SOrder2002(1,8)
                    setprop sheet1.range("K5"),*Value=SOrder2002(1,9)
                    setprop sheet1.range("L5"),*Value=SOrder2002(1,10)
                    setprop sheet1.range("M5"),*Value=SOrder2002(1,11)
                    setprop sheet1.range("N5"),*Value=SOrder2002(1,12)
.                                              
          setprop sheet1.range("A7"),*Value="List Management"
                    setprop sheet1.range("C7"),*Value=LM2002(1,1)
                    setprop sheet1.range("D7"),*Value=LM2002(1,2)
                    setprop sheet1.range("E7"),*Value=LM2002(1,3)
                    setprop sheet1.range("F7"),*Value=LM2002(1,4)
                    setprop sheet1.range("G7"),*Value=LM2002(1,5)
                    setprop sheet1.range("H7"),*Value=LM2002(1,6)
                    setprop sheet1.range("I7"),*Value=LM2002(1,7)
                    setprop sheet1.range("J7"),*Value=LM2002(1,8)
                    setprop sheet1.range("K7"),*Value=LM2002(1,9)
                    setprop sheet1.range("L7"),*Value=LM2002(1,10)
                    setprop sheet1.range("M7"),*Value=LM2002(1,11)
                    setprop sheet1.range("N7"),*Value=LM2002(1,12)
          setprop sheet1.range("B8"),*Value="LCRs"
                    setprop sheet1.range("C8"),*Value=LLCR2002(1,1)
                    setprop sheet1.range("D8"),*Value=LLCR2002(1,2)
                    setprop sheet1.range("E8"),*Value=LLCR2002(1,3)
                    setprop sheet1.range("F8"),*Value=LLCR2002(1,4)
                    setprop sheet1.range("G8"),*Value=LLCR2002(1,5)
                    setprop sheet1.range("H8"),*Value=LLCR2002(1,6)
                    setprop sheet1.range("I8"),*Value=LLCR2002(1,7)
                    setprop sheet1.range("J8"),*Value=LLCR2002(1,8)
                    setprop sheet1.range("K8"),*Value=LLCR2002(1,9)
                    setprop sheet1.range("L8"),*Value=LLCR2002(1,10)
                    setprop sheet1.range("M8"),*Value=LLCR2002(1,11)
                    setprop sheet1.range("N8"),*Value=LLCR2002(1,12)
          setprop sheet1.range("B9"),*Value="Pending"
                    setprop sheet1.range("C9"),*Value=LPend2002(1,1)
                    setprop sheet1.range("D9"),*Value=LPend2002(1,2)
                    setprop sheet1.range("E9"),*Value=LPend2002(1,3)
                    setprop sheet1.range("F9"),*Value=LPend2002(1,4)
                    setprop sheet1.range("G9"),*Value=LPend2002(1,5)
                    setprop sheet1.range("H9"),*Value=LPend2002(1,6)
                    setprop sheet1.range("I9"),*Value=LPend2002(1,7)
                    setprop sheet1.range("J9"),*Value=LPend2002(1,8)
                    setprop sheet1.range("K9"),*Value=LPend2002(1,9)
                    setprop sheet1.range("L9"),*Value=LPend2002(1,10)
                    setprop sheet1.range("M9"),*Value=LPend2002(1,11)
                    setprop sheet1.range("N9"),*Value=LPend2002(1,12)
          setprop sheet1.range("B10"),*Value="Orders"
                    setprop sheet1.range("C10"),*Value=LOrder2002(1,1)
                    setprop sheet1.range("D10"),*Value=LOrder2002(1,2)
                    setprop sheet1.range("E10"),*Value=LOrder2002(1,3)
                    setprop sheet1.range("F10"),*Value=LOrder2002(1,4)
                    setprop sheet1.range("G10"),*Value=LOrder2002(1,5)
                    setprop sheet1.range("H10"),*Value=LOrder2002(1,6)
                    setprop sheet1.range("I10"),*Value=LOrder2002(1,7)
                    setprop sheet1.range("J10"),*Value=LOrder2002(1,8)
                    setprop sheet1.range("K10"),*Value=LOrder2002(1,9)
                    setprop sheet1.range("L10"),*Value=LOrder2002(1,10)
                    setprop sheet1.range("M10"),*Value=LOrder2002(1,11)
                    setprop sheet1.range("N10"),*Value=LOrder2002(1,12)
.
.2003
          setprop sheet1.range("A13"),*Value="2003"
.
          setprop sheet1.range("C14"),*Value="Jan"
          setprop sheet1.range("D14"),*Value="Feb"
          setprop sheet1.range("E14"),*Value="Mar"
          setprop sheet1.range("F14"),*Value="Apr"
          setprop sheet1.range("G14"),*Value="May"
          setprop sheet1.range("H14"),*Value="Jun"
          setprop sheet1.range("I14"),*Value="Jul"
          setprop sheet1.range("J14"),*Value="Aug"
          setprop sheet1.range("K14"),*Value="Sep"
          setprop sheet1.range("L14"),*Value="Oct"
          setprop sheet1.range("M14"),*Value="Nov"
          setprop sheet1.range("N14"),*Value="Dec"
.
          setprop sheet1.range("A15"),*Value="Sales"
                    setprop sheet1.range("C15"),*Value=Sales2003(1,1)
                    setprop sheet1.range("D15"),*Value=Sales2003(1,2)
                    setprop sheet1.range("E15"),*Value=Sales2003(1,3)
                    setprop sheet1.range("F15"),*Value=Sales2003(1,4)
                    setprop sheet1.range("G15"),*Value=Sales2003(1,5)
                    setprop sheet1.range("H15"),*Value=Sales2003(1,6)
                    setprop sheet1.range("I15"),*Value=Sales2003(1,7)
                    setprop sheet1.range("J15"),*Value=Sales2003(1,8)
                    setprop sheet1.range("K15"),*Value=Sales2003(1,9)
                    setprop sheet1.range("L15"),*Value=Sales2003(1,10)
                    setprop sheet1.range("M15"),*Value=Sales2003(1,11)
                    setprop sheet1.range("N15"),*Value=Sales2003(1,12)
          setprop sheet1.range("B16"),*Value="LCRs"
                    setprop sheet1.range("C16"),*Value=SLCR2003(1,1)
                    setprop sheet1.range("D16"),*Value=SLCR2003(1,2)
                    setprop sheet1.range("E16"),*Value=SLCR2003(1,3)
                    setprop sheet1.range("F16"),*Value=SLCR2003(1,4)
                    setprop sheet1.range("G16"),*Value=SLCR2003(1,5)
                    setprop sheet1.range("H16"),*Value=SLCR2003(1,6)
                    setprop sheet1.range("I16"),*Value=SLCR2003(1,7)
                    setprop sheet1.range("J16"),*Value=SLCR2003(1,8)
                    setprop sheet1.range("K16"),*Value=SLCR2003(1,9)
                    setprop sheet1.range("L16"),*Value=SLCR2003(1,10)
                    setprop sheet1.range("M16"),*Value=SLCR2003(1,11)
                    setprop sheet1.range("N16"),*Value=SLCR2003(1,12)
          setprop sheet1.range("B17"),*Value="Orders"
                    setprop sheet1.range("C17"),*Value=SOrder2003(1,1)
                    setprop sheet1.range("D17"),*Value=SOrder2003(1,2)
                    setprop sheet1.range("E17"),*Value=SOrder2003(1,3)
                    setprop sheet1.range("F17"),*Value=SOrder2003(1,4)
                    setprop sheet1.range("G17"),*Value=SOrder2003(1,5)
                    setprop sheet1.range("H17"),*Value=SOrder2003(1,6)
                    setprop sheet1.range("I17"),*Value=SOrder2003(1,7)
                    setprop sheet1.range("J17"),*Value=SOrder2003(1,8)
                    setprop sheet1.range("K17"),*Value=SOrder2003(1,9)
                    setprop sheet1.range("L17"),*Value=SOrder2003(1,10)
                    setprop sheet1.range("M17"),*Value=SOrder2003(1,11)
                    setprop sheet1.range("N17"),*Value=SOrder2003(1,12)
.
          setprop sheet1.range("A19"),*Value="List Management"
                    setprop sheet1.range("C19"),*Value=LM2003(1,1)
                    setprop sheet1.range("D19"),*Value=LM2003(1,2)
                    setprop sheet1.range("E19"),*Value=LM2003(1,3)
                    setprop sheet1.range("F19"),*Value=LM2003(1,4)
                    setprop sheet1.range("G19"),*Value=LM2003(1,5)
                    setprop sheet1.range("H19"),*Value=LM2003(1,6)
                    setprop sheet1.range("I19"),*Value=LM2003(1,7)
                    setprop sheet1.range("J19"),*Value=LM2003(1,8)
                    setprop sheet1.range("K19"),*Value=LM2003(1,9)
                    setprop sheet1.range("L19"),*Value=LM2003(1,10)
                    setprop sheet1.range("M19"),*Value=LM2003(1,11)
                    setprop sheet1.range("N19"),*Value=LM2003(1,12)
          setprop sheet1.range("B20"),*Value="LCRs"
                    setprop sheet1.range("C20"),*Value=LLCR2003(1,1)
                    setprop sheet1.range("D20"),*Value=LLCR2003(1,2)
                    setprop sheet1.range("E20"),*Value=LLCR2003(1,3)
                    setprop sheet1.range("F20"),*Value=LLCR2003(1,4)
                    setprop sheet1.range("G20"),*Value=LLCR2003(1,5)
                    setprop sheet1.range("H20"),*Value=LLCR2003(1,6)
                    setprop sheet1.range("I20"),*Value=LLCR2003(1,7)
                    setprop sheet1.range("J20"),*Value=LLCR2003(1,8)
                    setprop sheet1.range("K20"),*Value=LLCR2003(1,9)
                    setprop sheet1.range("L20"),*Value=LLCR2003(1,10)
                    setprop sheet1.range("M20"),*Value=LLCR2003(1,11)
                    setprop sheet1.range("N20"),*Value=LLCR2003(1,12)
          setprop sheet1.range("B21"),*Value="Pending"
                    setprop sheet1.range("C21"),*Value=LPend2003(1,1)
                    setprop sheet1.range("D21"),*Value=LPend2003(1,2)
                    setprop sheet1.range("E21"),*Value=LPend2003(1,3)
                    setprop sheet1.range("F21"),*Value=LPend2003(1,4)
                    setprop sheet1.range("G21"),*Value=LPend2003(1,5)
                    setprop sheet1.range("H21"),*Value=LPend2003(1,6)
                    setprop sheet1.range("I21"),*Value=LPend2003(1,7)
                    setprop sheet1.range("J21"),*Value=LPend2003(1,8)
                    setprop sheet1.range("K21"),*Value=LPend2003(1,9)
                    setprop sheet1.range("L21"),*Value=LPend2003(1,10)
                    setprop sheet1.range("M21"),*Value=LPend2003(1,11)
                    setprop sheet1.range("N21"),*Value=LPend2003(1,12)
          setprop sheet1.range("B22"),*Value="Orders"
                    setprop sheet1.range("C22"),*Value=LOrder2003(1,1)
                    setprop sheet1.range("D22"),*Value=LOrder2003(1,2)
                    setprop sheet1.range("E22"),*Value=LOrder2003(1,3)
                    setprop sheet1.range("F22"),*Value=LOrder2003(1,4)
                    setprop sheet1.range("G22"),*Value=LOrder2003(1,5)
                    setprop sheet1.range("H22"),*Value=LOrder2003(1,6)
                    setprop sheet1.range("I22"),*Value=LOrder2003(1,7)
                    setprop sheet1.range("J22"),*Value=LOrder2003(1,8)
                    setprop sheet1.range("K22"),*Value=LOrder2003(1,9)
                    setprop sheet1.range("L22"),*Value=LOrder2003(1,10)
                    setprop sheet1.range("M22"),*Value=LOrder2003(1,11)
                    setprop sheet1.range("N22"),*Value=LOrder2003(1,12)
.2004
          setprop sheet1.range("A25"),*Value="2004"
.
          setprop sheet1.range("C26"),*Value="Jan"
          setprop sheet1.range("D26"),*Value="Feb"
          setprop sheet1.range("E26"),*Value="Mar"
          setprop sheet1.range("F26"),*Value="Apr"
          setprop sheet1.range("G26"),*Value="May"
          setprop sheet1.range("H26"),*Value="Jun"
          setprop sheet1.range("I26"),*Value="Jul"
          setprop sheet1.range("J26"),*Value="Aug"
          setprop sheet1.range("K26"),*Value="Sep"
          setprop sheet1.range("L26"),*Value="Oct"
          setprop sheet1.range("M26"),*Value="Nov"
          setprop sheet1.range("N26"),*Value="Dec"
.
          setprop sheet1.range("A27"),*Value="Sales"
                    setprop sheet1.range("C27"),*Value=Sales2004(1,1)
                    setprop sheet1.range("D27"),*Value=Sales2004(1,2)
                    setprop sheet1.range("E27"),*Value=Sales2004(1,3)
                    setprop sheet1.range("F27"),*Value=Sales2004(1,4)
                    setprop sheet1.range("G27"),*Value=Sales2004(1,5)
                    setprop sheet1.range("H27"),*Value=Sales2004(1,6)
                    setprop sheet1.range("I27"),*Value=Sales2004(1,7)
                    setprop sheet1.range("J27"),*Value=Sales2004(1,8)
                    setprop sheet1.range("K27"),*Value=Sales2004(1,9)
                    setprop sheet1.range("L27"),*Value=Sales2004(1,10)
                    setprop sheet1.range("M27"),*Value=Sales2004(1,11)
                    setprop sheet1.range("N27"),*Value=Sales2004(1,12)
          setprop sheet1.range("B28"),*Value="LCRs"
                    setprop sheet1.range("C28"),*Value=SLCR2004(1,1)
                    setprop sheet1.range("D28"),*Value=SLCR2004(1,2)
                    setprop sheet1.range("E28"),*Value=SLCR2004(1,3)
                    setprop sheet1.range("F28"),*Value=SLCR2004(1,4)
                    setprop sheet1.range("G28"),*Value=SLCR2004(1,5)
                    setprop sheet1.range("H28"),*Value=SLCR2004(1,6)
                    setprop sheet1.range("I28"),*Value=SLCR2004(1,7)
                    setprop sheet1.range("J28"),*Value=SLCR2004(1,8)
                    setprop sheet1.range("K28"),*Value=SLCR2004(1,9)
                    setprop sheet1.range("L28"),*Value=SLCR2004(1,10)
                    setprop sheet1.range("M28"),*Value=SLCR2004(1,11)
                    setprop sheet1.range("N28"),*Value=SLCR2004(1,12)
          setprop sheet1.range("B29"),*Value="Orders"
                    setprop sheet1.range("C29"),*Value=SOrder2004(1,1)
                    setprop sheet1.range("D29"),*Value=SOrder2004(1,2)
                    setprop sheet1.range("E29"),*Value=SOrder2004(1,3)
                    setprop sheet1.range("F29"),*Value=SOrder2004(1,4)
                    setprop sheet1.range("G29"),*Value=SOrder2004(1,5)
                    setprop sheet1.range("H29"),*Value=SOrder2004(1,6)
                    setprop sheet1.range("I29"),*Value=SOrder2004(1,7)
                    setprop sheet1.range("J29"),*Value=SOrder2004(1,8)
                    setprop sheet1.range("K29"),*Value=SOrder2004(1,9)
                    setprop sheet1.range("L29"),*Value=SOrder2004(1,10)
                    setprop sheet1.range("M29"),*Value=SOrder2004(1,11)
                    setprop sheet1.range("N29"),*Value=SOrder2004(1,12)
.
          setprop sheet1.range("A31"),*Value="List Management"
                    setprop sheet1.range("C31"),*Value=LM2004(1,1)
                    setprop sheet1.range("D31"),*Value=LM2004(1,2)
                    setprop sheet1.range("E31"),*Value=LM2004(1,3)
                    setprop sheet1.range("F31"),*Value=LM2004(1,4)
                    setprop sheet1.range("G31"),*Value=LM2004(1,5)
                    setprop sheet1.range("H31"),*Value=LM2004(1,6)
                    setprop sheet1.range("I31"),*Value=LM2004(1,7)
                    setprop sheet1.range("J31"),*Value=LM2004(1,8)
                    setprop sheet1.range("K31"),*Value=LM2004(1,9)
                    setprop sheet1.range("L31"),*Value=LM2004(1,10)
                    setprop sheet1.range("M31"),*Value=LM2004(1,11)
                    setprop sheet1.range("N31"),*Value=LM2004(1,12)
          setprop sheet1.range("B32"),*Value="LCRs"
                    setprop sheet1.range("C32"),*Value=LLCR2004(1,1)
                    setprop sheet1.range("D32"),*Value=LLCR2004(1,2)
                    setprop sheet1.range("E32"),*Value=LLCR2004(1,3)
                    setprop sheet1.range("F32"),*Value=LLCR2004(1,4)
                    setprop sheet1.range("G32"),*Value=LLCR2004(1,5)
                    setprop sheet1.range("H32"),*Value=LLCR2004(1,6)
                    setprop sheet1.range("I32"),*Value=LLCR2004(1,7)
                    setprop sheet1.range("J32"),*Value=LLCR2004(1,8)
                    setprop sheet1.range("K32"),*Value=LLCR2004(1,9)
                    setprop sheet1.range("L32"),*Value=LLCR2004(1,10)
                    setprop sheet1.range("M32"),*Value=LLCR2004(1,11)
                    setprop sheet1.range("N32"),*Value=LLCR2004(1,12)
          setprop sheet1.range("B33"),*Value="Pending"
                    setprop sheet1.range("C33"),*Value=LPend2004(1,1)
                    setprop sheet1.range("D33"),*Value=LPend2004(1,2)
                    setprop sheet1.range("E33"),*Value=LPend2004(1,3)
                    setprop sheet1.range("F33"),*Value=LPend2004(1,4)
                    setprop sheet1.range("G33"),*Value=LPend2004(1,5)
                    setprop sheet1.range("H33"),*Value=LPend2004(1,6)
                    setprop sheet1.range("I33"),*Value=LPend2004(1,7)
                    setprop sheet1.range("J33"),*Value=LPend2004(1,8)
                    setprop sheet1.range("K33"),*Value=LPend2004(1,9)
                    setprop sheet1.range("L33"),*Value=LPend2004(1,10)
                    setprop sheet1.range("M33"),*Value=LPend2004(1,11)
                    setprop sheet1.range("N33"),*Value=LPend2004(1,12)
          setprop sheet1.range("B34"),*Value="Orders"
                    setprop sheet1.range("C34"),*Value=LOrder2004(1,1)
                    setprop sheet1.range("D34"),*Value=LOrder2004(1,2)
                    setprop sheet1.range("E34"),*Value=LOrder2004(1,3)
                    setprop sheet1.range("F34"),*Value=LOrder2004(1,4)
                    setprop sheet1.range("G34"),*Value=LOrder2004(1,5)
                    setprop sheet1.range("H34"),*Value=LOrder2004(1,6)
                    setprop sheet1.range("I34"),*Value=LOrder2004(1,7)
                    setprop sheet1.range("J34"),*Value=LOrder2004(1,8)
                    setprop sheet1.range("K34"),*Value=LOrder2004(1,9)
                    setprop sheet1.range("L34"),*Value=LOrder2004(1,10)
                    setprop sheet1.range("M34"),*Value=LOrder2004(1,11)
                    setprop sheet1.range("N34"),*Value=LOrder2004(1,12)
.Formatting
          setprop sheet1.range("A1:A34").Font,*Bold="True"
          setprop sheet1.range("B1:B34").Font,*Bold="True",*Italic="True"
          setprop sheet1.range("C2:N2").Font,*Bold="True"
          setprop sheet1.range("C14:N14").Font,*Bold="True"
          setprop sheet1.range("C26:N26").Font,*Bold="True"
.
          setprop sheet1.range("A1").Font,*Size="14"
          setprop sheet1.range("A13").Font,*Size="14"
          setprop sheet1.range("A25").Font,*Size="14"
.
          setprop sheet1.range("C3:N10"),*NumberFormat="##,####0_);[Red](##,####0);_(#"-#"_)"
          setprop sheet1.range("C15:N22"),*NumberFormat="##,####0_);[Red](##,####0);_(#"-#"_)"
          setprop sheet1.range("C27:N34"),*NumberFormat="##,####0_);[Red](##,####0);_(#"-#"_)"
tester
.
          books.item giving book using 1
;Create Worksheets collection
          getprop book,*Sheets=sheets
;Create a single Worksheet - we did not need to add it as we set the default above to
;add one new Worksheet each time a Workbook is created.
          sheets.item giving sheet2 using 2
          setprop   sheet2,*Name="Names"

.Header information
.2002 Header
          setprop sheet2.range("A1"),*Value="2002"
.
          setprop sheet2.range("C2"),*Value="Jan"
          setprop sheet2.range("D2"),*Value="Feb"
          setprop sheet2.range("E2"),*Value="Mar"
          setprop sheet2.range("F2"),*Value="Apr"
          setprop sheet2.range("G2"),*Value="May"
          setprop sheet2.range("H2"),*Value="Jun"
          setprop sheet2.range("I2"),*Value="Jul"
          setprop sheet2.range("J2"),*Value="Aug"
          setprop sheet2.range("K2"),*Value="Sep"
          setprop sheet2.range("L2"),*Value="Oct"
          setprop sheet2.range("M2"),*Value="Nov"
          setprop sheet2.range("N2"),*Value="Dec"
.
          setprop sheet2.range("A3"),*Value="Sales"
                    setprop sheet2.range("C3"),*Value=Sales2002(2,1)
                    setprop sheet2.range("D3"),*Value=Sales2002(2,2)
                    setprop sheet2.range("E3"),*Value=Sales2002(2,3)
                    setprop sheet2.range("F3"),*Value=Sales2002(2,4)
                    setprop sheet2.range("G3"),*Value=Sales2002(2,5)
                    setprop sheet2.range("H3"),*Value=Sales2002(2,6)
                    setprop sheet2.range("I3"),*Value=Sales2002(2,7)
                    setprop sheet2.range("J3"),*Value=Sales2002(2,8)
                    setprop sheet2.range("K3"),*Value=Sales2002(2,9)
                    setprop sheet2.range("L3"),*Value=Sales2002(2,10)
                    setprop sheet2.range("M3"),*Value=Sales2002(2,11)
                    setprop sheet2.range("N3"),*Value=Sales2002(2,12)
          setprop sheet2.range("B4"),*Value="LCRs"
                    setprop sheet2.range("C4"),*Value=SLCR2002(2,1)
                    setprop sheet2.range("D4"),*Value=SLCR2002(2,2)
                    setprop sheet2.range("E4"),*Value=SLCR2002(2,3)
                    setprop sheet2.range("F4"),*Value=SLCR2002(2,4)
                    setprop sheet2.range("G4"),*Value=SLCR2002(2,5)
                    setprop sheet2.range("H4"),*Value=SLCR2002(2,6)
                    setprop sheet2.range("I4"),*Value=SLCR2002(2,7)
                    setprop sheet2.range("J4"),*Value=SLCR2002(2,8)
                    setprop sheet2.range("K4"),*Value=SLCR2002(2,9)
                    setprop sheet2.range("L4"),*Value=SLCR2002(2,10)
                    setprop sheet2.range("M4"),*Value=SLCR2002(2,11)
                    setprop sheet2.range("N4"),*Value=SLCR2002(2,12)
          setprop sheet2.range("B5"),*Value="Orders"
                    setprop sheet2.range("C5"),*Value=SOrder2002(2,1)
                    setprop sheet2.range("D5"),*Value=SOrder2002(2,2)
                    setprop sheet2.range("E5"),*Value=SOrder2002(2,3)
                    setprop sheet2.range("F5"),*Value=SOrder2002(2,4)
                    setprop sheet2.range("G5"),*Value=SOrder2002(2,5)
                    setprop sheet2.range("H5"),*Value=SOrder2002(2,6)
                    setprop sheet2.range("I5"),*Value=SOrder2002(2,7)
                    setprop sheet2.range("J5"),*Value=SOrder2002(2,8)
                    setprop sheet2.range("K5"),*Value=SOrder2002(2,9)
                    setprop sheet2.range("L5"),*Value=SOrder2002(2,10)
                    setprop sheet2.range("M5"),*Value=SOrder2002(2,11)
                    setprop sheet2.range("N5"),*Value=SOrder2002(2,12)
.                                              
          setprop sheet2.range("A7"),*Value="List Management"
                    setprop sheet2.range("C7"),*Value=LM2002(2,1)
                    setprop sheet2.range("D7"),*Value=LM2002(2,2)
                    setprop sheet2.range("E7"),*Value=LM2002(2,3)
                    setprop sheet2.range("F7"),*Value=LM2002(2,4)
                    setprop sheet2.range("G7"),*Value=LM2002(2,5)
                    setprop sheet2.range("H7"),*Value=LM2002(2,6)
                    setprop sheet2.range("I7"),*Value=LM2002(2,7)
                    setprop sheet2.range("J7"),*Value=LM2002(2,8)
                    setprop sheet2.range("K7"),*Value=LM2002(2,9)
                    setprop sheet2.range("L7"),*Value=LM2002(2,10)
                    setprop sheet2.range("M7"),*Value=LM2002(2,11)
                    setprop sheet2.range("N7"),*Value=LM2002(2,12)
          setprop sheet2.range("B8"),*Value="LCRs"
                    setprop sheet2.range("C8"),*Value=LLCR2002(2,1)
                    setprop sheet2.range("D8"),*Value=LLCR2002(2,2)
                    setprop sheet2.range("E8"),*Value=LLCR2002(2,3)
                    setprop sheet2.range("F8"),*Value=LLCR2002(2,4)
                    setprop sheet2.range("G8"),*Value=LLCR2002(2,5)
                    setprop sheet2.range("H8"),*Value=LLCR2002(2,6)
                    setprop sheet2.range("I8"),*Value=LLCR2002(2,7)
                    setprop sheet2.range("J8"),*Value=LLCR2002(2,8)
                    setprop sheet2.range("K8"),*Value=LLCR2002(2,9)
                    setprop sheet2.range("L8"),*Value=LLCR2002(2,10)
                    setprop sheet2.range("M8"),*Value=LLCR2002(2,11)
                    setprop sheet2.range("N8"),*Value=LLCR2002(2,12)
          setprop sheet2.range("B9"),*Value="Pending"
                    setprop sheet2.range("C9"),*Value=LPend2002(2,1)
                    setprop sheet2.range("D9"),*Value=LPend2002(2,2)
                    setprop sheet2.range("E9"),*Value=LPend2002(2,3)
                    setprop sheet2.range("F9"),*Value=LPend2002(2,4)
                    setprop sheet2.range("G9"),*Value=LPend2002(2,5)
                    setprop sheet2.range("H9"),*Value=LPend2002(2,6)
                    setprop sheet2.range("I9"),*Value=LPend2002(2,7)
                    setprop sheet2.range("J9"),*Value=LPend2002(2,8)
                    setprop sheet2.range("K9"),*Value=LPend2002(2,9)
                    setprop sheet2.range("L9"),*Value=LPend2002(2,10)
                    setprop sheet2.range("M9"),*Value=LPend2002(2,11)
                    setprop sheet2.range("N9"),*Value=LPend2002(2,12)
          setprop sheet2.range("B10"),*Value="Orders"
                    setprop sheet2.range("C10"),*Value=LOrder2002(2,1)
                    setprop sheet2.range("D10"),*Value=LOrder2002(2,2)
                    setprop sheet2.range("E10"),*Value=LOrder2002(2,3)
                    setprop sheet2.range("F10"),*Value=LOrder2002(2,4)
                    setprop sheet2.range("G10"),*Value=LOrder2002(2,5)
                    setprop sheet2.range("H10"),*Value=LOrder2002(2,6)
                    setprop sheet2.range("I10"),*Value=LOrder2002(2,7)
                    setprop sheet2.range("J10"),*Value=LOrder2002(2,8)
                    setprop sheet2.range("K10"),*Value=LOrder2002(2,9)
                    setprop sheet2.range("L10"),*Value=LOrder2002(2,10)
                    setprop sheet2.range("M10"),*Value=LOrder2002(2,11)
                    setprop sheet2.range("N10"),*Value=LOrder2002(2,12)
.
.2003
          setprop sheet2.range("A13"),*Value="2003"
.
          setprop sheet2.range("C14"),*Value="Jan"
          setprop sheet2.range("D14"),*Value="Feb"
          setprop sheet2.range("E14"),*Value="Mar"
          setprop sheet2.range("F14"),*Value="Apr"
          setprop sheet2.range("G14"),*Value="May"
          setprop sheet2.range("H14"),*Value="Jun"
          setprop sheet2.range("I14"),*Value="Jul"
          setprop sheet2.range("J14"),*Value="Aug"
          setprop sheet2.range("K14"),*Value="Sep"
          setprop sheet2.range("L14"),*Value="Oct"
          setprop sheet2.range("M14"),*Value="Nov"
          setprop sheet2.range("N14"),*Value="Dec"
.
          setprop sheet2.range("A15"),*Value="Sales"
                    setprop sheet2.range("C15"),*Value=Sales2003(2,1)
                    setprop sheet2.range("D15"),*Value=Sales2003(2,2)
                    setprop sheet2.range("E15"),*Value=Sales2003(2,3)
                    setprop sheet2.range("F15"),*Value=Sales2003(2,4)
                    setprop sheet2.range("G15"),*Value=Sales2003(2,5)
                    setprop sheet2.range("H15"),*Value=Sales2003(2,6)
                    setprop sheet2.range("I15"),*Value=Sales2003(2,7)
                    setprop sheet2.range("J15"),*Value=Sales2003(2,8)
                    setprop sheet2.range("K15"),*Value=Sales2003(2,9)
                    setprop sheet2.range("L15"),*Value=Sales2003(2,10)
                    setprop sheet2.range("M15"),*Value=Sales2003(2,11)
                    setprop sheet2.range("N15"),*Value=Sales2003(2,12)
          setprop sheet2.range("B16"),*Value="LCRs"
                    setprop sheet2.range("C16"),*Value=SLCR2003(2,1)
                    setprop sheet2.range("D16"),*Value=SLCR2003(2,2)
                    setprop sheet2.range("E16"),*Value=SLCR2003(2,3)
                    setprop sheet2.range("F16"),*Value=SLCR2003(2,4)
                    setprop sheet2.range("G16"),*Value=SLCR2003(2,5)
                    setprop sheet2.range("H16"),*Value=SLCR2003(2,6)
                    setprop sheet2.range("I16"),*Value=SLCR2003(2,7)
                    setprop sheet2.range("J16"),*Value=SLCR2003(2,8)
                    setprop sheet2.range("K16"),*Value=SLCR2003(2,9)
                    setprop sheet2.range("L16"),*Value=SLCR2003(2,10)
                    setprop sheet2.range("M16"),*Value=SLCR2003(2,11)
                    setprop sheet2.range("N16"),*Value=SLCR2003(2,12)
          setprop sheet2.range("B17"),*Value="Orders"
                    setprop sheet2.range("C17"),*Value=SOrder2003(2,1)
                    setprop sheet2.range("D17"),*Value=SOrder2003(2,2)
                    setprop sheet2.range("E17"),*Value=SOrder2003(2,3)
                    setprop sheet2.range("F17"),*Value=SOrder2003(2,4)
                    setprop sheet2.range("G17"),*Value=SOrder2003(2,5)
                    setprop sheet2.range("H17"),*Value=SOrder2003(2,6)
                    setprop sheet2.range("I17"),*Value=SOrder2003(2,7)
                    setprop sheet2.range("J17"),*Value=SOrder2003(2,8)
                    setprop sheet2.range("K17"),*Value=SOrder2003(2,9)
                    setprop sheet2.range("L17"),*Value=SOrder2003(2,10)
                    setprop sheet2.range("M17"),*Value=SOrder2003(2,11)
                    setprop sheet2.range("N17"),*Value=SOrder2003(2,12)
.
          setprop sheet2.range("A19"),*Value="List Management"
                    setprop sheet2.range("C19"),*Value=LM2003(2,1)
                    setprop sheet2.range("D19"),*Value=LM2003(2,2)
                    setprop sheet2.range("E19"),*Value=LM2003(2,3)
                    setprop sheet2.range("F19"),*Value=LM2003(2,4)
                    setprop sheet2.range("G19"),*Value=LM2003(2,5)
                    setprop sheet2.range("H19"),*Value=LM2003(2,6)
                    setprop sheet2.range("I19"),*Value=LM2003(2,7)
                    setprop sheet2.range("J19"),*Value=LM2003(2,8)
                    setprop sheet2.range("K19"),*Value=LM2003(2,9)
                    setprop sheet2.range("L19"),*Value=LM2003(2,10)
                    setprop sheet2.range("M19"),*Value=LM2003(2,11)
                    setprop sheet2.range("N19"),*Value=LM2003(2,12)
          setprop sheet2.range("B20"),*Value="LCRs"
                    setprop sheet2.range("C20"),*Value=LLCR2003(2,1)
                    setprop sheet2.range("D20"),*Value=LLCR2003(2,2)
                    setprop sheet2.range("E20"),*Value=LLCR2003(2,3)
                    setprop sheet2.range("F20"),*Value=LLCR2003(2,4)
                    setprop sheet2.range("G20"),*Value=LLCR2003(2,5)
                    setprop sheet2.range("H20"),*Value=LLCR2003(2,6)
                    setprop sheet2.range("I20"),*Value=LLCR2003(2,7)
                    setprop sheet2.range("J20"),*Value=LLCR2003(2,8)
                    setprop sheet2.range("K20"),*Value=LLCR2003(2,9)
                    setprop sheet2.range("L20"),*Value=LLCR2003(2,10)
                    setprop sheet2.range("M20"),*Value=LLCR2003(2,11)
                    setprop sheet2.range("N20"),*Value=LLCR2003(2,12)
          setprop sheet2.range("B21"),*Value="Pending"
                    setprop sheet2.range("C21"),*Value=LPend2003(2,1)
                    setprop sheet2.range("D21"),*Value=LPend2003(2,2)
                    setprop sheet2.range("E21"),*Value=LPend2003(2,3)
                    setprop sheet2.range("F21"),*Value=LPend2003(2,4)
                    setprop sheet2.range("G21"),*Value=LPend2003(2,5)
                    setprop sheet2.range("H21"),*Value=LPend2003(2,6)
                    setprop sheet2.range("I21"),*Value=LPend2003(2,7)
                    setprop sheet2.range("J21"),*Value=LPend2003(2,8)
                    setprop sheet2.range("K21"),*Value=LPend2003(2,9)
                    setprop sheet2.range("L21"),*Value=LPend2003(2,10)
                    setprop sheet2.range("M21"),*Value=LPend2003(2,11)
                    setprop sheet2.range("N21"),*Value=LPend2003(2,12)
          setprop sheet2.range("B22"),*Value="Orders"
                    setprop sheet2.range("C22"),*Value=LOrder2003(2,1)
                    setprop sheet2.range("D22"),*Value=LOrder2003(2,2)
                    setprop sheet2.range("E22"),*Value=LOrder2003(2,3)
                    setprop sheet2.range("F22"),*Value=LOrder2003(2,4)
                    setprop sheet2.range("G22"),*Value=LOrder2003(2,5)
                    setprop sheet2.range("H22"),*Value=LOrder2003(2,6)
                    setprop sheet2.range("I22"),*Value=LOrder2003(2,7)
                    setprop sheet2.range("J22"),*Value=LOrder2003(2,8)
                    setprop sheet2.range("K22"),*Value=LOrder2003(2,9)
                    setprop sheet2.range("L22"),*Value=LOrder2003(2,10)
                    setprop sheet2.range("M22"),*Value=LOrder2003(2,11)
                    setprop sheet2.range("N22"),*Value=LOrder2003(2,12)
.2004
          setprop sheet2.range("A25"),*Value="2004"
.
          setprop sheet2.range("C26"),*Value="Jan"
          setprop sheet2.range("D26"),*Value="Feb"
          setprop sheet2.range("E26"),*Value="Mar"
          setprop sheet2.range("F26"),*Value="Apr"
          setprop sheet2.range("G26"),*Value="May"
          setprop sheet2.range("H26"),*Value="Jun"
          setprop sheet2.range("I26"),*Value="Jul"
          setprop sheet2.range("J26"),*Value="Aug"
          setprop sheet2.range("K26"),*Value="Sep"
          setprop sheet2.range("L26"),*Value="Oct"
          setprop sheet2.range("M26"),*Value="Nov"
          setprop sheet2.range("N26"),*Value="Dec"
.
          setprop sheet2.range("A27"),*Value="Sales"
                    setprop sheet2.range("C27"),*Value=Sales2004(2,1)
                    setprop sheet2.range("D27"),*Value=Sales2004(2,2)
                    setprop sheet2.range("E27"),*Value=Sales2004(2,3)
                    setprop sheet2.range("F27"),*Value=Sales2004(2,4)
                    setprop sheet2.range("G27"),*Value=Sales2004(2,5)
                    setprop sheet2.range("H27"),*Value=Sales2004(2,6)
                    setprop sheet2.range("I27"),*Value=Sales2004(2,7)
                    setprop sheet2.range("J27"),*Value=Sales2004(2,8)
                    setprop sheet2.range("K27"),*Value=Sales2004(2,9)
                    setprop sheet2.range("L27"),*Value=Sales2004(2,10)
                    setprop sheet2.range("M27"),*Value=Sales2004(2,11)
                    setprop sheet2.range("N27"),*Value=Sales2004(2,12)
          setprop sheet2.range("B28"),*Value="LCRs"
                    setprop sheet2.range("C28"),*Value=SLCR2004(2,1)
                    setprop sheet2.range("D28"),*Value=SLCR2004(2,2)
                    setprop sheet2.range("E28"),*Value=SLCR2004(2,3)
                    setprop sheet2.range("F28"),*Value=SLCR2004(2,4)
                    setprop sheet2.range("G28"),*Value=SLCR2004(2,5)
                    setprop sheet2.range("H28"),*Value=SLCR2004(2,6)
                    setprop sheet2.range("I28"),*Value=SLCR2004(2,7)
                    setprop sheet2.range("J28"),*Value=SLCR2004(2,8)
                    setprop sheet2.range("K28"),*Value=SLCR2004(2,9)
                    setprop sheet2.range("L28"),*Value=SLCR2004(2,10)
                    setprop sheet2.range("M28"),*Value=SLCR2004(2,11)
                    setprop sheet2.range("N28"),*Value=SLCR2004(2,12)
          setprop sheet2.range("B29"),*Value="Orders"
                    setprop sheet2.range("C29"),*Value=SOrder2004(2,1)
                    setprop sheet2.range("D29"),*Value=SOrder2004(2,2)
                    setprop sheet2.range("E29"),*Value=SOrder2004(2,3)
                    setprop sheet2.range("F29"),*Value=SOrder2004(2,4)
                    setprop sheet2.range("G29"),*Value=SOrder2004(2,5)
                    setprop sheet2.range("H29"),*Value=SOrder2004(2,6)
                    setprop sheet2.range("I29"),*Value=SOrder2004(2,7)
                    setprop sheet2.range("J29"),*Value=SOrder2004(2,8)
                    setprop sheet2.range("K29"),*Value=SOrder2004(2,9)
                    setprop sheet2.range("L29"),*Value=SOrder2004(2,10)
                    setprop sheet2.range("M29"),*Value=SOrder2004(2,11)
                    setprop sheet2.range("N29"),*Value=SOrder2004(2,12)
.
          setprop sheet2.range("A31"),*Value="List Management"
                    setprop sheet2.range("C31"),*Value=LM2004(2,1)
                    setprop sheet2.range("D31"),*Value=LM2004(2,2)
                    setprop sheet2.range("E31"),*Value=LM2004(2,3)
                    setprop sheet2.range("F31"),*Value=LM2004(2,4)
                    setprop sheet2.range("G31"),*Value=LM2004(2,5)
                    setprop sheet2.range("H31"),*Value=LM2004(2,6)
                    setprop sheet2.range("I31"),*Value=LM2004(2,7)
                    setprop sheet2.range("J31"),*Value=LM2004(2,8)
                    setprop sheet2.range("K31"),*Value=LM2004(2,9)
                    setprop sheet2.range("L31"),*Value=LM2004(2,10)
                    setprop sheet2.range("M31"),*Value=LM2004(2,11)
                    setprop sheet2.range("N31"),*Value=LM2004(2,12)
          setprop sheet2.range("B32"),*Value="LCRs"
                    setprop sheet2.range("C32"),*Value=LLCR2004(2,1)
                    setprop sheet2.range("D32"),*Value=LLCR2004(2,2)
                    setprop sheet2.range("E32"),*Value=LLCR2004(2,3)
                    setprop sheet2.range("F32"),*Value=LLCR2004(2,4)
                    setprop sheet2.range("G32"),*Value=LLCR2004(2,5)
                    setprop sheet2.range("H32"),*Value=LLCR2004(2,6)
                    setprop sheet2.range("I32"),*Value=LLCR2004(2,7)
                    setprop sheet2.range("J32"),*Value=LLCR2004(2,8)
                    setprop sheet2.range("K32"),*Value=LLCR2004(2,9)
                    setprop sheet2.range("L32"),*Value=LLCR2004(2,10)
                    setprop sheet2.range("M32"),*Value=LLCR2004(2,11)
                    setprop sheet2.range("N32"),*Value=LLCR2004(2,12)
          setprop sheet2.range("B33"),*Value="Pending"
                    setprop sheet2.range("C33"),*Value=LPend2004(2,1)
                    setprop sheet2.range("D33"),*Value=LPend2004(2,2)
                    setprop sheet2.range("E33"),*Value=LPend2004(2,3)
                    setprop sheet2.range("F33"),*Value=LPend2004(2,4)
                    setprop sheet2.range("G33"),*Value=LPend2004(2,5)
                    setprop sheet2.range("H33"),*Value=LPend2004(2,6)
                    setprop sheet2.range("I33"),*Value=LPend2004(2,7)
                    setprop sheet2.range("J33"),*Value=LPend2004(2,8)
                    setprop sheet2.range("K33"),*Value=LPend2004(2,9)
                    setprop sheet2.range("L33"),*Value=LPend2004(2,10)
                    setprop sheet2.range("M33"),*Value=LPend2004(2,11)
                    setprop sheet2.range("N33"),*Value=LPend2004(2,12)
          setprop sheet2.range("B34"),*Value="Orders"
                    setprop sheet2.range("C34"),*Value=LOrder2004(2,1)
                    setprop sheet2.range("D34"),*Value=LOrder2004(2,2)
                    setprop sheet2.range("E34"),*Value=LOrder2004(2,3)
                    setprop sheet2.range("F34"),*Value=LOrder2004(2,4)
                    setprop sheet2.range("G34"),*Value=LOrder2004(2,5)
                    setprop sheet2.range("H34"),*Value=LOrder2004(2,6)
                    setprop sheet2.range("I34"),*Value=LOrder2004(2,7)
                    setprop sheet2.range("J34"),*Value=LOrder2004(2,8)
                    setprop sheet2.range("K34"),*Value=LOrder2004(2,9)
                    setprop sheet2.range("L34"),*Value=LOrder2004(2,10)
                    setprop sheet2.range("M34"),*Value=LOrder2004(2,11)
                    setprop sheet2.range("N34"),*Value=LOrder2004(2,12)
.Formatting
          setprop sheet2.range("A1:A34").Font,*Bold="True"
          setprop sheet2.range("B1:B34").Font,*Bold="True",*Italic="True"
          setprop sheet2.range("C2:N2").Font,*Bold="True"
          setprop sheet2.range("C14:N14").Font,*Bold="True"
          setprop sheet2.range("C26:N26").Font,*Bold="True"
.
          setprop sheet2.range("A1").Font,*Size="14"
          setprop sheet2.range("A13").Font,*Size="14"
          setprop sheet2.range("A25").Font,*Size="14"
.
          setprop sheet2.range("C3:N10"),*NumberFormat="##,####0_);[Red](##,####0);_(#"-#"_)"
          setprop sheet2.range("C15:N22"),*NumberFormat="##,####0_);[Red](##,####0);_(#"-#"_)"
          setprop sheet2.range("C27:N34"),*NumberFormat="##,####0_);[Red](##,####0);_(#"-#"_)"
.
          setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
XReportCleanUp
;Clean up after myself
;All created automation objects MUST be destroyed.  If not ex.quit will fail and
;Excel.exe will still be running.
          destroy sheet1
          destroy sheet2
          destroy sheets
          destroy book
          destroy books
;Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
;If User has quit out of the SaveAs routine we do not want any prompts informing them their
;Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
;be left open.
          ex.quit
          destroy ex
          STOP
          
          include   nordio.inc
          include   comlogic.inc