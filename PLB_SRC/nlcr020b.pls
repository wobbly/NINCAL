pc         Equ        0
          include   common.inc
          include   cons.inc
          include   norddd.inc
          include   nord5dd.inc
          include   nord4dd.inc
          include   npnddd.inc
          include   ncntdd.inc
.START PATCH 1.2 REPLACED LOGIC
.         include   nmlrdd.inc
          include   compdd.inc
          include   cntdd.inc
.END PATCH 1.2 REPLACED LOGIC
        include winapi.inc

release   init        "1.5"        DLH      New Pending Status added
Reldate   Init      "2014 February 7"
.release   init      "1.4"     ASH       03JUN2013 New LCR Status
.release   init      "1.3"     ASH       06AUG2004 Logo Conversion
.release  init      "1.2"     ASH       28may2004 MAILER CONVERSION
.release  init      "1.1"     ASH       22JUL2002 STANDARDIZED GETINFO ROUTINE
.release  init      "1.0"

.tempfile file
tempfile1 file
tempfile2 file
tempfile3 file
prfile  pfile
.Filters/Flags
FromDate form       "00000000"
ToDate    form      "99999999"
.START PATCH 1.1 REMOVED LOGIC
.osflag   form   1       .1=win 95,98, 2=NT
.END PATCH 1.1 REMOVED LOGIC
.begin patch xxx
.DpmtFlag form       1       .which Departments do you wish to see/Looking at Caller or Contact?
DpmtFlag form       1       .which Departments do you wish to see/Looking at Caller or Contact or Salesperson?
.end patch xxx
RptFlag form    1       .Summary or Detail Report
SrtFlag form    1       .Sort Order for Detail Report
PrtFlag   form      1
LastRecord form     1
.START PATCH 1.4 REPLACED LOGIC
.SalesLCR form       9(15)
.LMLCR     form      9(15)
SalesLCR form       9(20)
LMLCR     form      9(20)
.END PATCH 1.4 REPLACED LOGIC
SalesPend form  9(20)
.begin patch 1.5
.LMPend  form    9(20)
LMPend  form    9(21)
.end patch 1.5
F44     form    4.4
.N11     form    11
TotRec    form      11
str7b   dim     7
COUNTER   form      9
DimPtr    dim       ^
DimPtr1 dim         ^
FrmPtr  form    ^
.hexeight integer 4,"4294967295"
page    form    9
page2     form    9
.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font
font6   font
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=9
        create  font3,"Helvetica",size=9
        create  font4,"Fixed",size=10
        create  font5,"Arial",size=11
        create  font6,"Arial",size=14
.START PATCH 1.3 ADDED LOGIC
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 1.3 ADDED LOGIC
        clock   timestamp,timestamp
          unpack    COMMENT,FromDate,ToDate,DpmtFlag,NCNTFLD,RptFlag,PrtFlag
.         move      "20010101",FromDate
.         move      "20010201",ToDate
.         move      C4,DpmtFlag
.        move    C1,RptFlag
.         move      "02",NCNTFLD
.        move    C1,SrtFlag
.         move      C3,PrtFlag
.Find out system information
.START PATCH 1.1 REPLACED LOGIC
.        getinfo system,str6
.        unpack  str6 into str1,str1
.        move    C0,osflag
.        if (str1 = "3" or str1 = "4")           .95/98
.                move    C1,osflag
.        elseif (str1 = "1"or str1 = "5")        .NT4/NT5
.                move    C2,osflag
.        endif
          call      GetWinVer
.END PATCH 1.1 REPLACED LOGIC

.LCR Section
        move    C3,NORDLOCK
        move    C1,NORDPATH

.Set up columns
        move    "500",column
        move    "1200",column1
        move    "2700",column2
        move    "4200",column4
        move    "4950",column5
        move    "6000",column6
        move    "7000",column3
        move    "8500",column7
        if (PrtFlag = 1 | PrtFlag = 2)      .Sales
.START PATCH 3.61 REPLACED LOGIC
.                if (osflag = c2 | osflag = C5)         .nt
.                        PRTOPEN prfile,"\\NTS0\Laser3 Blankstock","FAXFILE.PRN"
.                elseif (osflag = c1)         .win 95 98
.                        PRTOPEN prfile,"Laser3 Blankstock","FAXFILE.PRN"
.                else   .(osflag = c0)         .Don't know prompt for printer
.                        PRTOPEN prfile,"-","FAXFILE.PRN"
.                endif
.
                    if (osflag >= c6) 
                              PRTOPEN   prfile,"\\NINs2\Laser3 Blankstock","FAXFILE.PRN"
                    elseif (osflag = c3 | osflag =c4)         .win 95 98
                              PRTOPEN   prfile,"Laser3 Blankstock","FAXFILE.PRN"
                    else   .(osflag = c0)         .Don't know prompt for printer
                              PRTOPEN   prfile,"-","FAXFILE.PRN"
                    endif
.END PATCH 3.61 REPLACED LOGIC
.          elseif (PrtFlag = 2)          .List Management/Accounting/Data Entry/HR/Marketing
..START PATCH 3.61 REPLACED LOGIC
..                if (osflag = c2)         .nt
..                        PRTOPEN prfile,"\\NTS0\Laser2","FAXFILE.PRN"
..                elseif (osflag = c1)         .win 95 98
..                        PRTOPEN prfile,"Laser2","FAXFILE.PRN"
..                else   .(osflag = c0)         .Don't know prompt for printer
..                        PRTOPEN prfile,"-","FAXFILE.PRN"
..                endif
.                    if (osflag = c1 | osflag = C5 | osflag = c6)         .nt 2k xp
.                              PRTOPEN   prfile,"\\NINs2\Laser2","FAXFILE.PRN"
.                    elseif (osflag = c3 | osflag =c4)         .win 95 98
.                              PRTOPEN   prfile,"Laser2","FAXFILE.PRN"
.                    else   .(osflag = c0)         .Don't know prompt for printer
.                              PRTOPEN   prfile,"-","FAXFILE.PRN"
.                    endif
.END PATCH 3.61 REPLACED LOGIC
        else                            .Information Services
.START PATCH 3.61 REPLACED LOGIC
.                if (osflag = c2)         .nt
.                        PRTOPEN prfile,"\\NTS0\Laser6","FAXFILE.PRN"
.                elseif (osflag = c1)         .win 95 98
.                        PRTOPEN prfile,"Laser6","FAXFILE.PRN"
.                else   .(osflag = c0)         .Don't know prompt for printer
.                        PRTOPEN prfile,"-","FAXFILE.PRN"
.                endif
                    if (osflag >= c6)         .
                              PRTOPEN   prfile,"\\NINs2\Laser3 Blankstock","FAXFILE.PRN"
                    elseif (osflag = c3 | osflag =c4)         .win 95 98
                              PRTOPEN   prfile,"Laser3 Blankstock","FAXFILE.PRN"
                    else   .(osflag = c0)         .Don't know prompt for printer
                              PRTOPEN   prfile,"-","FAXFILE.PRN"
                    endif
.END PATCH 3.61 REPLACED LOGIC
.        elseif (PrtFlag = 4)  .PDF
.                clear   str25
.                append  OLON,str25
.                append  "_",str25
.                append  OLNUM,str25
.                reset   str25
.                PRTOPEN prfile,"Acrobat Distiller",str25
.                pack    str55,str25,".pdf"
        endif
.
          if (DpmtFlag > 1)
.Skip Sales Report
                    goto      PendingReport
          endif
        DISPLAY *P10:12,"Sorting NINORD5.DAT"
        clear   taskname
..        pack    taskname,"\\nins1\E\DATA\TEXT\NINORD5.DAT,C:\WORK\NINORD5.SRT;S=#"9>='",FromDate,"'&9<='",ToDate,"'#""
.        pack    taskname,"c:\work\NINORD5.DAT,C:\WORK\NINORD5.SRT;S=#"9>='",FromDate,"'&9<='",ToDate,"'#""
.        sort    taskname
        pack    taskname,"!\\nins1\e\netutils\sort32.exe \\nins1\E\DATA\TEXT\NINORD5.DAT C:\WORK\NINORD5.SRT /inc(9,8,n,ge,\""",FromDate,"\"",and,9,8,n,le,\""",ToDate,"\"") VER"
          execute   taskname
          pack    APIFileName,"C:\WORK\NINORD5.SRT",hexzero
        call    FindFirstFile
        if (APIResult = 0 | APIResult = hexeight)
.End Job Now!
                    goto EndOfJob
          endif
        if (RptFlag = C2)
                prepare tempfile2,"c:\work\stats1.dat",EXCLUSIVE
                prepare tempfile3,"c:\work\stats2.dat",EXCLUSIVE
        endif
.
        move        "NLCR020B-Open NINORD5.SRT",Location
          pack      KeyLocation,"Sequential"
        TRAP    IOMssg Giving Error if IO
        open    tempfile1,"C:\WORK\NINORD5.SRT"
        TRAPCLR IO
          loop
                    move      "NLCR020B-Read NINORD5.SRT",Location
                    pack      KeyLocation,"Sequential"
                TRAP    IOMssg Giving Error if IO
                    READ    tempfile1,SEQ;ord5vars
                    until over
                TRAPCLR IO
                add     C1,TotRec
                DISPLAY *P10:12,"LCR Records Processed : ",TotRec
                call    Trim using NORD5LR
                if (NORD5LR <> "")
                        move    NORD5LR,NORDFLD
                    move      "NLCR020B-NORDKEY",Location
                        pack  KeyLocation,"Key: ",NORDFLD
                        call  NORDKEY
                        if not over
                                if (OSALES10 = "0" AND OSALES = "6")    .List Management
                                        add     C1,LMLCR(1)
                                        if (OSTAT = "l")
                                                if (RptFlag = C2)
                                                                      pack      MKEY,OMLRNUM,"000"
                                                                      call      NMLRKEY
                                                        write   tempfile3,SEQ;NORD5STAT,OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                endif
                                                add     C1,LMLCR(2)
                                                if (NORD5STAT = "00")           .LCR
                                                        add     C1,LMLCR(3)
                                                elseif (NORD5STAT = "01")       .1st Request
                                                        add     C1,LMLCR(4)
                                                elseif (NORD5STAT = "02")       .2nd Request
                                                        add     C1,LMLCR(5)
                                                elseif (NORD5STAT = "03")       .Revised Request
                                                        add     C1,LMLCR(6)
                                                elseif (NORD5STAT = "04")       .Approved - Should not be here!!!
                                                        add     C1,LMLCR(7)
                                                elseif (NORD5STAT = "05")       .Cancelled
                                                        add     C1,LMLCR(8)
                                                elseif (NORD5STAT = "06")       .Pending
                                                        add     C1,LMLCR(9)
                                                elseif (NORD5STAT = "07")       .Denied
                                                        add     C1,LMLCR(10)
.START PATCH 1.23 ADDED LOGIC
                                                                  elseif (NORD5STAT = "08")       .Pending Internal
                                                                             add     C1,LMLCR(16)
                                                                  elseif (NORD5STAT = "09")       .Tentative Approval
                                                                             add     C1,LMLCR(17)
.END PATCH 1.23 ADDED LOGIC
                                                endif
                                        elseif (OSTAT = "z")                    .Cancelled
                                                if (RptFlag = C2)
                                                                      pack      MKEY,OMLRNUM,"000"
                                                                      call      NMLRKEY
                                                        write   tempfile3,SEQ;"05",OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                endif
                                                add     C1,LMLCR(2)
.                                                        if (NORD5STAT = "05")       .Cancelled
                                                add     C1,LMLCR(8)
.                                                       elseif (NORD5STAT = "06")       .Pending
.                                                               add     C1,LMLCR(9)
.                                                       endif
                                        else                                    .Live Orders
                                                add     C1,LMLCR(11)
                                                if (OSTAT = "0")                .Live
                                                        if (RptFlag = C2)
                                                                                pack      MKEY,OMLRNUM,"000"
                                                                                call      NMLRKEY
                                                                write   tempfile3,SEQ;"LL",OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,LMLCR(12)
                                                elseif (OSTAT = "B")            .Billed
                                                        if (RptFlag = C2)
                                                                                pack      MKEY,OMLRNUM,"000"
                                                                                call      NMLRKEY
                                                                write   tempfile3,SEQ;"BB",OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,LMLCR(13)
                                                elseif (OSTAT = "Q")            .Cancelled/Billed
                                                        if (RptFlag = C2)
                                                                                pack      MKEY,OMLRNUM,"000"
                                                                                call      NMLRKEY
                                                                write   tempfile3,SEQ;"CB",OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,LMLCR(14)
                                                elseif (OSTAT = "X")            .Cancelled Order
                                                        if (RptFlag = C2)
                                                                                pack      MKEY,OMLRNUM,"000"
                                                                                call      NMLRKEY
                                                                write   tempfile3,SEQ;"CO",OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,LMLCR(15)
                                                endif
                                        endif
                                else
                                        add     C1,SalesLCR(1)
                                        if (OSTAT = "l")
                                                if (RptFlag = C2)
                                                                      pack      MKEY,OMLRNUM,"000"
                                                                      call      NMLRKEY
                                                        write   tempfile2,SEQ;NORD5STAT,OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                endif
                                                add     C1,SalesLCR(2)
                                                if (NORD5STAT = "00")           .LCR
                                                        add     C1,SalesLCR(3)
                                                elseif (NORD5STAT = "01")       .1st Request
                                                        add     C1,SalesLCR(4)
                                                elseif (NORD5STAT = "02")       .2nd Request
                                                        add     C1,SalesLCR(5)
                                                elseif (NORD5STAT = "03")       .Revised Request
                                                        add     C1,SalesLCR(6)
                                                elseif (NORD5STAT = "04")       .Approved - Should not be here!!!
                                                        add     C1,SalesLCR(7)
                                                elseif (NORD5STAT = "05")       .Cancelled
                                                        add     C1,SalesLCR(8)
                                                elseif (NORD5STAT = "06")       .Pending
                                                        add     C1,SalesLCR(9)
                                                elseif (NORD5STAT = "07")       .Denied
                                                        add     C1,SalesLCR(10)
.START PATCH 1.23 ADDED LOGIC
                                                                  elseif (NORD5STAT = "08")       .Pending Internal
                                                                             add     C1,SalesLCR(16)
                                                                  elseif (NORD5STAT = "09")       .Tentative Approval
                                                                             add     C1,SalesLCR(17)
.END PATCH 1.23 ADDED LOGIC
                                                endif
                                        elseif (OSTAT = "z")                    .Cancelled
                                                if (RptFlag = C2)
                                                                      pack      MKEY,OMLRNUM,"000"
                                                                      call      NMLRKEY
                                                        write   tempfile2,SEQ;"05",OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                endif
                                                add     C1,SalesLCR(2)
.                                                        if (NORD5STAT = "05")       .Cancelled
                                                add     C1,SalesLCR(8)
.                                                       elseif (NORD5STAT = "06")       .Pending
.                                                               add     C1,LMLCR(9)
.                                                       endif
                                        else                                    .Live Orders
                                                add     C1,SalesLCR(11)
                                                if (OSTAT = "0")                .Live
                                                        if (RptFlag = C2)
                                                                                pack      MKEY,OMLRNUM,"000"
                                                                                call      NMLRKEY
                                                                write   tempfile2,SEQ;"LL",OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,SalesLCR(12)
                                                elseif (OSTAT = "B")            .Billed
                                                        if (RptFlag = C2)
                                                                                pack      MKEY,OMLRNUM,"000"
                                                                                call      NMLRKEY
                                                                write   tempfile2,SEQ;"BB",OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,SalesLCR(13)
                                                elseif (OSTAT = "Q")            .Cancelled/Billed
                                                        if (RptFlag = C2)
                                                                                pack      MKEY,OMLRNUM,"000"
                                                                                call      NMLRKEY
                                                                write   tempfile2,SEQ;"CB",OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,SalesLCR(14)
                                                elseif (OSTAT = "X")            .Cancelled Order
                                                        if (RptFlag = C2)
                                                                                pack      MKEY,OMLRNUM,"000"
                                                                                call      NMLRKEY
                                                                write   tempfile2,SEQ;"CO",OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,SalesLCR(15)
                                                endif
                                        endif
                                        endif
                        endif
                endif
          repeat
          if (RptFlag = C2)
                    close     tempfile2
                    close     tempfile3
                  clear   taskname
          append  "C:\WORK\STATS1.DAT,C:\WORK\STATS1.SRT;",taskname
.                 append  "3-6,7-12,13-24,25-32",taskname
          append  "7-51,52-57,58-69,70-77",taskname
                  reset   taskname
                  sort    taskname
                  clear   taskname
                  append  "C:\WORK\STATS2.DAT,C:\WORK\STATS2.SRT;",taskname
.                 append  "3-6,7-12,13-24,25-32",taskname
                  append  "7-51,52-57,58-69,70-77",taskname
                  reset   taskname
                  sort    taskname
          endif
          move      C0,page2
          call      PrintHeader using C1
        clear   str11
        move    TotRec,str11
          prtpage prfile;*pcolumn1:row,"Total Records Read:";
          call      PrintAmount using str11,str15
        move    C0,N11
        add     SalesLCR(1),LMLCR(1),N11
        move    N11,str11
          prtpage prfile;*pcolumn1:row,"Total LCRs:";
          call      PrintAmount using str11,str15
          add     eightlpi,row
        clear   str7
        move    C0,F44
        div     N11,SalesLCR(1),F44
        mult    "100",F44
        move    F44,str7
        move    SalesLCR(1),str9
          prtpage prfile;*pcolumn1:row,*boldon,"Sales Department";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8,*ALIGNMENT=*LEFT;
          sub       "800",column6,result
          prtpage prfile;*presult:row,*boldoff,"Total LCRs";
          prtpage prfile;*presult:row,*boldon;
          call      PrintAmount using str9,str15
          prtpage prfile;*presult:row,*boldoff;
.................
          add       eightlpi,row
        clear   str7
        move    C0,F44
        div     SalesLCR(1),SalesLCR(2),F44
        mult    "100",F44
        move    F44,str7
        move    SalesLCR(2),str9
          prtpage prfile;*pcolumn1:row,"  Records which are still LCRs:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8,*ALIGNMENT=*LEFT;
          sub       "800",column6,result
          prtpage prfile;*presult:row,"Sales LCRs";
          call      PrintAmount using str9,str15
          sub       "600",column5,result
          prtpage prfile;*presult:row,*boldon,"% of Sales LCRs";
          sub       "500",column6,result
          prtpage prfile;*presult:row,"% of Sales Total",*boldoff;
          add       eightlpi,row
        clear   str7
        move    C0,F44
        div     SalesLCR(2),SalesLCR(3),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesLCR(1),SalesLCR(3),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesLCR(3),str9
          prtpage prfile;*pcolumn1:row,"        Status of 'LCR':";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesLCR(2),SalesLCR(4),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesLCR(1),SalesLCR(4),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesLCR(4),str9
          prtpage prfile;*pcolumn1:row,"        Status of '1st Request':";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesLCR(2),SalesLCR(5),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesLCR(1),SalesLCR(5),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesLCR(5),str9
          prtpage prfile;*pcolumn1:row,"        Status of '2nd Request':";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesLCR(2),SalesLCR(6),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesLCR(1),SalesLCR(6),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesLCR(6),str9
          prtpage prfile;*pcolumn1:row,"        Status of 'Revised Request':";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesLCR(2),SalesLCR(7),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesLCR(1),SalesLCR(7),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesLCR(7),str9
          prtpage prfile;*pcolumn1:row,"        Status of 'Approved':";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesLCR(2),SalesLCR(8),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesLCR(1),SalesLCR(8),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesLCR(8),str9
          prtpage prfile;*pcolumn1:row,"        Status of 'Cancelled':";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesLCR(2),SalesLCR(9),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesLCR(1),SalesLCR(9),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesLCR(9),str9
          prtpage prfile;*pcolumn1:row,"        Status of 'Pending':";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesLCR(2),SalesLCR(10),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesLCR(1),SalesLCR(10),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesLCR(10),str9
          prtpage prfile;*pcolumn1:row,"        Status of 'Denied':";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
          add       eightlpi,row
.START PATCH 1.4 ADDED LOGIC
        clear   str7
        move    C0,F44
        div     SalesLCR(2),SalesLCR(16),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesLCR(1),SalesLCR(16),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesLCR(16),str9
          prtpage prfile;*pcolumn1:row,"        Status of 'Pending Internal':";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
          add       eightlpi,row
...
        clear   str7
        move    C0,F44
        div     SalesLCR(2),SalesLCR(17),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesLCR(1),SalesLCR(17),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesLCR(17),str9
          prtpage prfile;*pcolumn1:row,"        Status of 'Tentatively Approved':";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
          add       eightlpi,row
.END PATCH 1.4 ADDED LOGIC
        clear   str7
        move    C0,F44
        div     SalesLCR(1),SalesLCR(11),F44
        mult    "100",F44
        move    F44,str7
        move    SalesLCR(11),str9
          prtpage prfile;*pcolumn1:row,"  LCRs Turned into Live Orders:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8,*ALIGNMENT=*LEFT;
          sub       "800",column6,result
          prtpage prfile;*presult:row,"Sales LCRs";
          call      PrintAmount using str9,str15
          sub       "600",column5,result
          prtpage prfile;*presult:row,*boldon,"% of Sales LCRs";
          sub       "500",column6,result
          prtpage prfile;*presult:row,"% of Sales Total",*boldoff;
          add       eightlpi,row
          clear   str7
        move    C0,F44
        div     SalesLCR(11),SalesLCR(12),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesLCR(1),SalesLCR(12),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesLCR(12),str9
          prtpage prfile;*pcolumn1:row,"        Currently Live:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesLCR(11),SalesLCR(13),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesLCR(1),SalesLCR(13),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesLCR(13),str9
          prtpage prfile;*pcolumn1:row,"        Billed:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesLCR(11),SalesLCR(14),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesLCR(1),SalesLCR(14),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesLCR(14),str9
          prtpage prfile;*pcolumn1:row,"        Cancelled/Billed:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesLCR(11),SalesLCR(15),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesLCR(1),SalesLCR(15),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesLCR(15),str9
          prtpage prfile;*pcolumn1:row,"        Cancelled Order:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        if (RptFlag = C2)
                pack    APIFileName,"C:\WORK\STATS1.SRT",hexzero
                call    FindFirstFile
                if (APIResult <> 0 & APIResult <> hexeight)
                              add       eightlpi,row
                              call      PrintSubHeader
                        open    tempfile2,"C:\WORK\STATS1.SRT",EXCLUSIVE
                        loop
                                read    tempfile2,SEQ;str2,OMLRNUM,MCOMP,OLNUM,OMLRPON,str8,OLRN,OSTAT
                                        until over
.                                if (OSTAT = "l" | OSTAT = "z")
                                        call    SetVars using C1
                                                  call      PrintDetail using C1
.                                endif
                        repeat
.                             prtpage prfile;*NEWPAGE;
.                             call      PrintHeader using C1
                    close   tempfile2
                    else
                              prtpage prfile;*NEWPAGE;
                    endif
          else
                    prtpage prfile;*NEWPAGE;
        endif
.
          prtpage prfile;*NEWPAGE;
          move      C0,page2
          call      PrintHeader using C1
        clear   str11
        move    TotRec,str11
          prtpage prfile;*pcolumn1:row,"Total Records Read:";
          call      PrintAmount using str11,str15
        move    C0,N11
        add     SalesLCR(1),LMLCR(1),N11
        move    N11,str11
          prtpage prfile;*pcolumn1:row,"Total LCRs:";
          call      PrintAmount using str11,str15
          add     eightlpi,row
        clear   str7
        move    C0,F44
        div     N11,LMLCR(1),F44
        mult    "100",F44
        move    F44,str7
        move    LMLCR(1),str9
          prtpage prfile;*pcolumn1:row,*boldon,"List Management Department";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8,*ALIGNMENT=*LEFT;
          sub       "800",column6,result
          prtpage prfile;*presult:row,*boldoff,"Total LCRs";
          prtpage prfile;*presult:row,*boldon;
          call      PrintAmount using str9,str15
          prtpage prfile;*presult:row,*boldoff;
          add       eightlpi,row
        clear   str7
        move    C0,F44
        div     LMLCR(1),LMLCR(2),F44
        mult    "100",F44
        move    F44,str7
        move    LMLCR(2),str9
          prtpage prfile;*pcolumn1:row,"  Records which are still LCRs:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8,*ALIGNMENT=*LEFT;
          sub       "800",column6,result
          prtpage prfile;*presult:row,"LM LCRs";
          call      PrintAmount using str9,str15
          sub       "600",column5,result
          prtpage prfile;*presult:row,*boldon,"% of LM LCRs";
          sub       "500",column6,result
          prtpage prfile;*presult:row,"% of LM Total",*boldoff;
          add       eightlpi,row
        clear   str7
        move    C0,F44
        div     LMLCR(2),LMLCR(3),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMLCR(1),LMLCR(3),F44
        mult    "100",F44
        move    F44,str7b
        move    LMLCR(3),str9
          prtpage prfile;*pcolumn1:row,"        Status of 'LCR':";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     LMLCR(2),LMLCR(4),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMLCR(1),LMLCR(4),F44
        mult    "100",F44
        move    F44,str7b
        move    LMLCR(4),str9
          prtpage prfile;*pcolumn1:row,"        Status of '1st Request':";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     LMLCR(2),LMLCR(5),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMLCR(1),LMLCR(5),F44
        mult    "100",F44
        move    F44,str7b
        move    LMLCR(5),str9
          prtpage prfile;*pcolumn1:row,"        Status of '2nd Request':";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     LMLCR(2),LMLCR(6),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMLCR(1),LMLCR(6),F44
        mult    "100",F44
        move    F44,str7b
        move    LMLCR(6),str9
          prtpage prfile;*pcolumn1:row,"        Status of 'Revised Request':";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     LMLCR(2),LMLCR(7),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMLCR(1),LMLCR(7),F44
        mult    "100",F44
        move    F44,str7b
        move    LMLCR(7),str9
          prtpage prfile;*pcolumn1:row,"        Status of 'Approved':";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     LMLCR(2),LMLCR(8),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMLCR(1),LMLCR(8),F44
        mult    "100",F44
        move    F44,str7b
        move    LMLCR(8),str9
          prtpage prfile;*pcolumn1:row,"        Status of 'Cancelled':";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     LMLCR(2),LMLCR(9),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMLCR(1),LMLCR(9),F44
        mult    "100",F44
        move    F44,str7b
        move    LMLCR(9),str9
          prtpage prfile;*pcolumn1:row,"        Status of 'Pending':";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     LMLCR(2),LMLCR(10),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMLCR(1),LMLCR(10),F44
        mult    "100",F44
        move    F44,str7b
        move    LMLCR(10),str9
          prtpage prfile;*pcolumn1:row,"        Status of 'Denied':";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
          add       eightlpi,row
.START PATCH 1.4 ADDED LOGIC
        clear   str7
        move    C0,F44
        div     LMLCR(2),LMLCR(16),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMLCR(1),LMLCR(16),F44
        mult    "100",F44
        move    F44,str7b
        move    LMLCR(16),str9
          prtpage prfile;*pcolumn1:row,"        Status of 'Pending Internal':";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
          add       eightlpi,row
...
        clear   str7
        move    C0,F44
        div     LMLCR(2),LMLCR(17),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMLCR(1),LMLCR(17),F44
        mult    "100",F44
        move    F44,str7b
        move    LMLCR(17),str9
          prtpage prfile;*pcolumn1:row,"        Status of 'Tentatively Approved':";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
          add       eightlpi,row
.END PATCH 1.4 ADDED LOGIC
        clear   str7
        move    C0,F44
        div     LMLCR(1),LMLCR(11),F44
        mult    "100",F44
        move    F44,str7
        move    LMLCR(11),str9
          prtpage prfile;*pcolumn1:row,"  LCRs Turned into Live Orders:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8,*ALIGNMENT=*LEFT;
          sub       "800",column6,result
          prtpage prfile;*presult:row,"LM LCRs";
          call      PrintAmount using str9,str15
          sub       "600",column5,result
          prtpage prfile;*presult:row,*boldon,"% of LM LCRs";
          sub       "500",column6,result
          prtpage prfile;*presult:row,"% of LM Total",*boldoff;
          add       eightlpi,row
        clear   str7
        move    C0,F44
        div     LMLCR(11),LMLCR(12),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMLCR(1),LMLCR(12),F44
        mult    "100",F44
        move    F44,str7b
        move    LMLCR(12),str9
          prtpage prfile;*pcolumn1:row,"        Currently Live:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     LMLCR(11),LMLCR(13),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMLCR(1),LMLCR(13),F44
        mult    "100",F44
        move    F44,str7b
        move    LMLCR(13),str9
          prtpage prfile;*pcolumn1:row,"        Billed:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     LMLCR(11),LMLCR(14),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMLCR(1),LMLCR(14),F44
        mult    "100",F44
        move    F44,str7b
        move    LMLCR(14),str9
          prtpage prfile;*pcolumn1:row,"        Cancelled/Billed:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     LMLCR(11),LMLCR(15),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMLCR(1),LMLCR(15),F44
        mult    "100",F44
        move    F44,str7b
        move    LMLCR(15),str9
          prtpage prfile;*pcolumn1:row,"        Cancelled Order:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        if (RptFlag = C2)
                pack    APIFileName,"C:\WORK\STATS2.SRT",hexzero
                call    FindFirstFile
                if (APIResult <> 0 & APIResult <> hexeight)
                              add       eightlpi,row
                              call      PrintSubHeader
                        open    tempfile2,"C:\WORK\STATS2.SRT",EXCLUSIVE
                        loop
                                read    tempfile2,SEQ;str2,OMLRNUM,MCOMP,OLNUM,OMLRPON,str8,OLRN,OSTAT
                                        until over
.                                if (OSTAT = "l" | OSTAT = "z")
                                        call    SetVars using C1
                                                  call      PrintDetail using C1
.                                endif
                        repeat
.                             prtpage prfile;*NEWPAGE;
.                             call      PrintHeader using C1
                        close   tempfile2
                    else
                              prtpage prfile;*NEWPAGE;
                    endif
          else
                    prtpage prfile;*NEWPAGE;
        endif
          if (DpmtFlag = 1)
                    goto EndOfJob
          endif
.Pending Order Section
PendingReport
        clear   taskname
.        pack    taskname,"\\nins1\E\DATA\TEXT\NINORD4.DAT,C:\WORK\NINORD4.SRT;S=#"9>='",FromDate,"'&9<='",ToDate,"'#""
.        sort    taskname
        pack    taskname,"!\\nins1\e\netutils\sort32.exe \\nins1\E\DATA\TEXT\NINORD4.DAT C:\WORK\NINORD4.SRT /inc(9,8,n,ge,\""",FromDate,"\"",and,9,8,n,le,\""",ToDate,"\"") VER"
          execute   taskname
          pack    APIFileName,"C:\WORK\NINORD4.SRT",hexzero
        call    FindFirstFile
        if (APIResult = 0 | APIResult = hexeight)
.End Job Now!
                    goto EndOfJob
          endif
          move    C0,TotRec
          close     tempfile1
          close     tempfile2
          close     tempfile3
        if (RptFlag = C2)
                  prepare tempfile2,"c:\work\stats3.dat",EXCLUSIVE
          prepare tempfile3,"c:\work\stats4.dat",EXCLUSIVE
        endif
        move        "NLCR020B-Open NINORD4.SRT",Location
          pack      KeyLocation,"Sequential"
        TRAP    IOMssg Giving Error if IO
        open    tempfile1,"C:\WORK\NINORD4.SRT"
        TRAPCLR IO
          loop
                    move      "NLCR020B-Read NINORD4.SRT",Location
                    pack      KeyLocation,"Sequential"
                TRAP    IOMssg Giving Error if IO
                    READ    tempfile1,SEQ;ord4vars
                    until over
                TRAPCLR IO
                add     C1,TotRec
                DISPLAY *P10:12,"Pending Records Processed : ",TotRec
                call    Trim using NORD4LR
                if (NORD4LR <> "")
                        move    NORD4LR,NORDFLD
                    move      "NLCR020B-NORDKEY,2",Location
                        pack  KeyLocation,"Key: ",NORDFLD
                        call  NORDKEY
                        if not over
                                if (OSALES10 = "0" AND OSALES = "6")    .List Management
                                        add     C1,LMPend(1)
                                        if (OSTAT = "p")
                                                if (RptFlag = C2)
                                                                      pack      MKEY,OMLRNUM,"000"
                                                                      call      NMLRKEY
                                                        write   tempfile3,SEQ;NORD4STAT,OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                endif
                                                add     C1,LMPend(2)
                                                if (NORD4STAT = "00")           .Pending List Owner Approval
                                                        add     C1,LMPend(3)
                                                elseif (NORD4STAT = "01")       .Waiting for Guarantee
                                                        add     C1,LMPend(4)
                                                elseif (NORD4STAT = "02")       .Waiting for Broker Call Back
                                                        add     C1,LMPend(5)
                                                elseif (NORD4STAT = "03")       .Waiting for Pre-Payment
                                                        add     C1,LMPend(6)
                                                elseif (NORD4STAT = "04")       .Waiting for Revision
                                                        add     C1,LMPend(7)
                                                elseif (NORD4STAT = "05")       .At Service Bureau
                                                        add     C1,LMPend(8)
                                                elseif (NORD4STAT = "06")       .Denied
                                                        add     C1,LMPend(9)
                                                elseif (NORD4STAT = "07")       .Cancelled
                                                        add     C1,LMPend(10)
                                                elseif (NORD4STAT = "08")       .Approved
                                                        add     C1,LMPend(11)
                                                elseif (NORD4STAT = "09")       .Order Received
                                                        add     C1,LMPend(12)
                                                elseif (NORD4STAT = "10")       .Accounting Hold
                                                        add     C1,LMPend(13)
                                                elseif (NORD4STAT = "11")       .2nd Request
                                                        add     C1,LMPend(14)
                                                elseif (NORD4STAT = "12")       .Revised Request
                                                        add     C1,LMPend(15)
.begin patch 1.5
.note discovered these where not ever added 
.          insertitem Nord001AComboPending,15,"Waiting for List Usage Agreement"
.          insertitem Nord001AComboPending,16,"Waiting for Sample"
.          insertitem Nord001AComboPending,17,"Waiting for Counts"
.end note discovered these where not ever added 
                                            elseif (NORD4STAT = "13")       .Waiting for List Usage Agreement
                                                       add     C1,LMPend(13)
                                            elseif (NORD4STAT = "14")       .Waiting for Sample
                                                        add     C1,LMPend(14)
                                            elseif (NORD4STAT = "15")       .Waiting for counts
                                                       add     C1,LMPend(15)
                                            elseif (NORD4STAT = "16")       .Waiting for $ approval
                                                       add     C1,LMPend(16)
.end patch 1.5
                                                endif
                                        elseif (OSTAT = "x")                    .Cancelled/Denied
                                                add     C1,LMPend(2)
                                                if (OHIST = "p")                .Cancelled
                                                        if (RptFlag = C2)
                                                                                pack      MKEY,OMLRNUM,"000"
                                                                                call      NMLRKEY
                                                                write   tempfile3,SEQ;"07",OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,LMPend(10)
                                                elseif (OHIST = "z")            .Denied
                                                        if (RptFlag = C2)
                                                                                pack      MKEY,OMLRNUM,"000"
                                                                                call      NMLRKEY
                                                                write   tempfile3,SEQ;"06",OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,LMPend(9)
                                                endif
                                        else                                    .Live Orders
                                                add     C1,LMPend(16)
                                                if (OSTAT = "0")                .Live
                                                        if (RptFlag = C2)
                                                                                pack      MKEY,OMLRNUM,"000"
                                                                                call      NMLRKEY
                                                                write   tempfile3,SEQ;"LL",OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,LMPend(17)
                                                elseif (OSTAT = "B")            .Billed
                                                        if (RptFlag = C2)
                                                                                pack      MKEY,OMLRNUM,"000"
                                                                                call      NMLRKEY
                                                                write   tempfile3,SEQ;"BB",OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,LMPend(18)
                                                elseif (OSTAT = "Q")            .Cancelled/Billed
                                                        if (RptFlag = C2)
                                                                                pack      MKEY,OMLRNUM,"000"
                                                                                call      NMLRKEY
                                                                write   tempfile3,SEQ;"CB",OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,LMPend(19)
                                                elseif (OSTAT = "X")            .Cancelled Order
                                                        if (RptFlag = C2)
                                                                                pack      MKEY,OMLRNUM,"000"
                                                                                call      NMLRKEY
                                                                write   tempfile3,SEQ;"CO",OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,LMPend(20)
                                                endif
                                        endif
                                else
                                        add     C1,SalesPend(1)
                                        if (OSTAT = "p")
                                                if (RptFlag = C2)
                                                                      pack      MKEY,OMLRNUM,"000"
                                                                      call      NMLRKEY
                                                        write   tempfile2,SEQ;NORD4STAT,OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                endif
                                                add     C1,SalesPend(2)
                                                if (NORD4STAT = "00")           .Pending List Owner Approval
                                                        add     C1,SalesPend(3)
                                                elseif (NORD4STAT = "01")       .Waiting for Guarantee
                                                        add     C1,SalesPend(4)
                                                elseif (NORD4STAT = "02")       .Waiting for Broker Call Back
                                                        add     C1,SalesPend(5)
                                                elseif (NORD4STAT = "03")       .Waiting for Pre-Payment
                                                        add     C1,SalesPend(6)
                                                elseif (NORD4STAT = "04")       .Waiting for Revision
                                                        add     C1,SalesPend(7)
                                                elseif (NORD4STAT = "05")       .At Service Bureau
                                                        add     C1,SalesPend(8)
                                                elseif (NORD4STAT = "06")       .Denied
                                                        add     C1,SalesPend(9)
                                                elseif (NORD4STAT = "07")       .Cancelled
                                                        add     C1,SalesPend(10)
                                                elseif (NORD4STAT = "08")       .Approved
                                                        add     C1,SalesPend(11)
                                                elseif (NORD4STAT = "09")       .Order Received
                                                        add     C1,SalesPend(12)
                                                elseif (NORD4STAT = "10")       .Accounting Hold
                                                        add     C1,SalesPend(13)
                                                elseif (NORD4STAT = "11")       .2nd Request
                                                        add     C1,SalesPend(14)
                                                elseif (NORD4STAT = "12")       .Revised Request
                                                        add     C1,SalesPend(15)
                                                endif
                                        elseif (OSTAT = "x")                    .Cancelled/Denied
                                                add     C1,SalesPend(2)
                                                if (OHIST = "p")                .Cancelled
                                                        if (RptFlag = C2)
                                                                                pack      MKEY,OMLRNUM,"000"
                                                                                call      NMLRKEY
                                                                write   tempfile2,SEQ;"07",str2,OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,SalesPend(10)
                                                elseif (OHIST = "z")            .Denied
                                                        if (RptFlag = C2)
                                                                                pack      MKEY,OMLRNUM,"000"
                                                                                call      NMLRKEY
                                                                write   tempfile2,SEQ;"06",str2,OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,SalesPend(9)
                                                endif
                                        else                                    .Live Orders
                                                add     C1,SalesPend(16)
                                                if (OSTAT = "0")                .Live
                                                        if (RptFlag = C2)
                                                                                pack      MKEY,OMLRNUM,"000"
                                                                                call      NMLRKEY
                                                                write   tempfile2,SEQ;"LL",str2,OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,SalesPend(17)
                                                elseif (OSTAT = "B")            .Billed
                                                        if (RptFlag = C2)
                                                                                pack      MKEY,OMLRNUM,"000"
                                                                                call      NMLRKEY
                                                                write   tempfile2,SEQ;"BB",str2,OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,SalesPend(18)
                                                elseif (OSTAT = "Q")            .Cancelled/Billed
                                                        if (RptFlag = C2)
                                                                                pack      MKEY,OMLRNUM,"000"
                                                                                call      NMLRKEY
                                                                write   tempfile2,SEQ;"CB",str2,OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,SalesPend(19)
                                                elseif (OSTAT = "X")            .Cancelled Order
                                                        if (RptFlag = C2)
                                                                                pack      MKEY,OMLRNUM,"000"
                                                                                call      NMLRKEY
                                                                write   tempfile2,SEQ;"CO",str2,OMLRNUM,MCOMP,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,SalesPend(20)
                                                endif

                                        endif
                                endif
                        endif
                endif
          repeat
        if (RptFlag = C2)
                close   tempfile2
                close   tempfile3
                clear   taskname
                append  "C:\WORK\STATS3.DAT,C:\WORK\STATS3.SRT;",taskname
.                 append  "3-6,7-12,13-24,25-32",taskname
                  append  "7-51,52-57,58-69,70-77",taskname
                reset   taskname
                sort    taskname
                clear   taskname
                append  "C:\WORK\STATS4.DAT,C:\WORK\STATS4.SRT;",taskname
.                 append  "3-6,7-12,13-24,25-32",taskname
                  append  "7-51,52-57,58-69,70-77",taskname
                reset   taskname
                sort    taskname
        endif
.
.Force new page
        DISPLAY *P10:12,"Creating Pending Stat Report"
          prtpage prfile;*NEWPAGE;
          move      C0,page2
          call      PrintHeader using C2
        clear   str11
        move    TotRec,str11
          prtpage prfile;*pcolumn1:row,"Total Records Read:";
          call      PrintAmount using str11,str15
        move    C0,N11
        add     SalesPend(1),LMPend(1),N11
        move    N11,str11
          prtpage prfile;*pcolumn1:row,"Total Pending Records:";
          call      PrintAmount using str11,str15
          add     eightlpi,row
        clear   str7
        move    C0,F44
        div     N11,SalesPend(1),F44
        mult    "100",F44
        move    F44,str7
        move    SalesPend(1),str9
          prtpage prfile;*pcolumn1:row,*boldon,"Sales Department";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8,*ALIGNMENT=*LEFT;
          sub       "800",column6,result
          prtpage prfile;*presult:row,*boldoff,"Total Pending Records";
          prtpage prfile;*presult:row,*boldon;
          call      PrintAmount using str9,str15
          prtpage prfile;*presult:row,*boldoff;
        clear   str7
        move    C0,F44
        div     SalesPend(1),SalesPend(2),F44
        mult    "100",F44
        move    F44,str7
        move    SalesPend(2),str9
          prtpage prfile;*pcolumn1:row,"  Records which are still Pending:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8,*ALIGNMENT=*LEFT;
          sub       "800",column6,result
          prtpage prfile;*presult:row,"Sales Pending Records";
          call      PrintAmount using str9,str15
          sub       "600",column5,result
          prtpage prfile;*presult:row,*boldon,"% of Sales Recs";
          sub       "500",column6,result
          prtpage prfile;*presult:row,"% of Sales Total",*boldoff;
          add       eightlpi,row
        clear   str7
        move    C0,F44
        div     SalesPend(2),SalesPend(3),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesPend(1),SalesPend(3),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesPend(3),str9
          prtpage prfile;*pcolumn1:row,"    Pending List Owner Approval:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesPend(2),SalesPend(4),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesPend(1),SalesPend(4),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesPend(4),str9
          prtpage prfile;*pcolumn1:row,"    Waiting for Guarantee:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesPend(2),SalesPend(5),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesPend(1),SalesPend(5),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesPend(5),str9
          prtpage prfile;*pcolumn1:row,"    Waiting for Broker Call Back:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesPend(2),SalesPend(6),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesPend(1),SalesPend(6),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesPend(6),str9
          prtpage prfile;*pcolumn1:row,"    Waiting for Pre-Payment:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesPend(2),SalesPend(7),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesPend(1),SalesPend(7),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesPend(7),str9
          prtpage prfile;*pcolumn1:row,"    Waiting for Revision:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesPend(2),SalesPend(8),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesPend(1),SalesPend(8),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesPend(8),str9
          prtpage prfile;*pcolumn1:row,"    At Service Bureau:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesPend(2),SalesPend(9),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesPend(1),SalesPend(9),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesPend(9),str9
          prtpage prfile;*pcolumn1:row,"    Denied:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesPend(2),SalesPend(10),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesPend(1),SalesPend(10),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesPend(10),str9
          prtpage prfile;*pcolumn1:row,"    Cancelled:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesPend(2),SalesPend(11),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesPend(1),SalesPend(11),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesPend(11),str9
          prtpage prfile;*pcolumn1:row,"    Approved:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesPend(2),SalesPend(12),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesPend(1),SalesPend(12),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesPend(12),str9
          prtpage prfile;*pcolumn1:row,"    Order Received:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesPend(2),SalesPend(13),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesPend(1),SalesPend(13),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesPend(13),str9
          prtpage prfile;*pcolumn1:row,"    Accounting Hold:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesPend(2),SalesPend(14),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesPend(1),SalesPend(14),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesPend(14),str9
          prtpage prfile;*pcolumn1:row,"    2nd Request:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesPend(2),SalesPend(15),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesPend(1),SalesPend(15),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesPend(15),str9
          prtpage prfile;*pcolumn1:row,"    Revised Request:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
          add       eightlpi,row
        clear   str7
        move    C0,F44
        div     SalesPend(1),SalesPend(16),F44
        mult    "100",F44
        move    F44,str7
        move    SalesPend(16),str9
          prtpage prfile;*pcolumn1:row,"  Pending Records turned into Orders:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8,*ALIGNMENT=*LEFT;
          sub       "800",column6,result
          prtpage prfile;*presult:row,"Sales Records";
          call      PrintAmount using str9,str15
          sub       "600",column5,result
          prtpage prfile;*presult:row,*boldon,"% of Sales Recs";
          sub       "500",column6,result
          prtpage prfile;*presult:row,"% of Sales Total",*boldoff;
          add       eightlpi,row
        clear   str7
        move    C0,F44
        div     SalesPend(16),SalesPend(17),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesPend(1),SalesPend(17),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesPend(17),str9
          prtpage prfile;*pcolumn1:row,"    Currently Live:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesPend(16),SalesPend(18),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesPend(1),SalesPend(18),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesPend(18),str9
          prtpage prfile;*pcolumn1:row,"    Billed:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesPend(16),SalesPend(19),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesPend(1),SalesPend(19),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesPend(19),str9
          prtpage prfile;*pcolumn1:row,"    Cancelled/Billed:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     SalesPend(16),SalesPend(20),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     SalesPend(1),SalesPend(20),F44
        mult    "100",F44
        move    F44,str7b
        move    SalesPend(20),str9
          prtpage prfile;*pcolumn1:row,"    Cancelled Order:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        if (RptFlag = C2)
                pack    APIFileName,"C:\WORK\STATS3.SRT",hexzero
                call    FindFirstFile
                if (APIResult <> 0 & APIResult <> hexeight)
                              add       eightlpi,row
                        call  PrintSubHeader
                        open    tempfile2,"C:\WORK\STATS3.SRT",EXCLUSIVE
                        loop
                                read    tempfile2,SEQ;str2,OMLRNUM,MCOMP,OLNUM,OMLRPON,str8,OLRN,OSTAT
                                until over
.                                if (OSTAT = "l" | OSTAT = "z")
                                        call    SetVars using C2
                                        call      PrintDetail using C2
.                                endif
                        repeat
.                             prtpage prfile;*NEWPAGE;
.                             call      PrintHeader using C2
                        close   tempfile2
                    else
                              prtpage prfile;*NEWPAGE;
                    endif
          else
                    prtpage prfile;*NEWPAGE;
        endif
.
          prtpage prfile;*NEWPAGE;
          move      C0,page2
          call      PrintHeader using C2
        clear   str11
        move    TotRec,str11
          prtpage prfile;*pcolumn1:row,"Total Records Read:";
          call      PrintAmount using str11,str15
        move    C0,N11
        add     SalesPend(1),LMPend(1),N11
        move    N11,str11
          prtpage prfile;*pcolumn1:row,"Total Pending Records:";
          call      PrintAmount using str11,str15
          add     eightlpi,row
        clear   str7
        move    C0,F44
        div     N11,LMPend(1),F44
        mult    "100",F44
        move    F44,str7
        move    LMPend(1),str9
          prtpage prfile;*pcolumn1:row,*boldon,"List Management Department";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8,*ALIGNMENT=*LEFT;
          sub       "800",column6,result
          prtpage prfile;*presult:row,*boldoff,"Total Records";
          prtpage prfile;*presult:row,*boldon;
          call      PrintAmount using str9,str15
          prtpage prfile;*presult:row,*boldoff;
          add       eightlpi,row
        clear   str7
        move    C0,F44
        div     LMPend(1),LMPend(2),F44
        mult    "100",F44
        move    F44,str7
        move    LMPend(2),str9
          prtpage prfile;*pcolumn1:row,"  Records which are still Pending:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8,*ALIGNMENT=*LEFT;
          sub       "800",column6,result
          prtpage prfile;*presult:row,"LM LCRs";
          call      PrintAmount using str9,str15
          sub       "600",column5,result
          prtpage prfile;*presult:row,*boldon,"% of LM Records";
          sub       "500",column6,result
          prtpage prfile;*presult:row,"% of LM Total",*boldoff;
          add       eightlpi,row
        clear   str7
        move    C0,F44
        div     LMPend(2),LMPend(3),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMPend(1),LMPend(3),F44
        mult    "100",F44
        move    F44,str7b
        move    LMPend(3),str9
          prtpage prfile;*pcolumn1:row,"    Pending List Owner Approval:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     LMPend(2),LMPend(4),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMPend(1),LMPend(4),F44
        mult    "100",F44
        move    F44,str7b
        move    LMPend(4),str9
          prtpage prfile;*pcolumn1:row,"    Waiting for Guarantee:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     LMPend(2),LMPend(5),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMPend(1),LMPend(5),F44
        mult    "100",F44
        move    F44,str7b
        move    LMPend(5),str9
          prtpage prfile;*pcolumn1:row,"    Waiting for Broker Call Back:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     LMPend(2),LMPend(6),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMPend(1),LMPend(6),F44
        mult    "100",F44
        move    F44,str7b
        move    LMPend(6),str9
          prtpage prfile;*pcolumn1:row,"    Waiting for Pre-Payment:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     LMPend(2),LMPend(7),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMPend(1),LMPend(7),F44
        mult    "100",F44
        move    F44,str7b
        move    LMPend(7),str9
          prtpage prfile;*pcolumn1:row,"    Waiting for Revision:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     LMPend(2),LMPend(8),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMPend(1),LMPend(8),F44
        mult    "100",F44
        move    F44,str7b
        move    LMPend(8),str9
          prtpage prfile;*pcolumn1:row,"    At Service Bureau:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     LMPend(2),LMPend(9),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMPend(1),LMPend(9),F44
        mult    "100",F44
        move    F44,str7b
        move    LMPend(9),str9
          prtpage prfile;*pcolumn1:row,"    Denied:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     LMPend(2),LMPend(10),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMPend(1),LMPend(10),F44
        mult    "100",F44
        move    F44,str7b
        move    LMPend(10),str9
          prtpage prfile;*pcolumn1:row,"    Cancelled:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15

        clear   str7
        move    C0,F44
        div     LMPend(2),LMPend(11),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMPend(1),LMPend(11),F44
        mult    "100",F44
        move    F44,str7b
        move    LMPend(11),str9
          prtpage prfile;*pcolumn1:row,"    Approved:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15

        clear   str7
        move    C0,F44
        div     LMPend(2),LMPend(12),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMPend(1),LMPend(12),F44
        mult    "100",F44
        move    F44,str7b
        move    LMPend(12),str9
          prtpage prfile;*pcolumn1:row,"    Order Received:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15

        clear   str7
        move    C0,F44
        div     LMPend(2),LMPend(13),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMPend(1),LMPend(13),F44
        mult    "100",F44
        move    F44,str7b
        move    LMPend(13),str9
          prtpage prfile;*pcolumn1:row,"    Accounting Hold:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15

        clear   str7
        move    C0,F44
        div     LMPend(2),LMPend(14),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMPend(1),LMPend(14),F44
        mult    "100",F44
        move    F44,str7b
        move    LMPend(14),str9
          prtpage prfile;*pcolumn1:row,"    2nd Request:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15

        clear   str7
        move    C0,F44
        div     LMPend(2),LMPend(15),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMPend(1),LMPend(15),F44
        mult    "100",F44
        move    F44,str7b
        move    LMPend(15),str9
          prtpage prfile;*pcolumn1:row,"    Revised Request:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
          add       eightlpi,row
.I belive the value of lmpend() needs to be bumped by 1 and the new waiting for $ goes here
        clear   str7
        move    C0,F44
        div     LMPend(1),LMPend(16),F44
        mult    "100",F44
        move    F44,str7
        move    LMPend(16),str9
          prtpage prfile;*pcolumn1:row,"  Pending Records turned into Orders:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8,*ALIGNMENT=*LEFT;
          sub       "800",column6,result
          prtpage prfile;*presult:row,"LM Records";
          call      PrintAmount using str9,str15
          sub       "600",column5,result
          prtpage prfile;*presult:row,*boldon,"% of LM Records";
          sub       "500",column6,result
          prtpage prfile;*presult:row,"% of LM Total",*boldoff;
          add       eightlpi,row
        clear   str7
        move    C0,F44
        div     LMPend(16),LMPend(17),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMPend(1),LMPend(17),F44
        mult    "100",F44
        move    F44,str7b
        move    LMPend(17),str9
          prtpage prfile;*pcolumn1:row,"    Currently Live:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     LMPend(16),LMPend(18),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMPend(1),LMPend(18),F44
        mult    "100",F44
        move    F44,str7b
        move    LMPend(18),str9
          prtpage prfile;*pcolumn1:row,"    Billed:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     LMPend(16),LMPend(19),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMPend(1),LMPend(19),F44
        mult    "100",F44
        move    F44,str7b
        move    LMPend(19),str9
          prtpage prfile;*pcolumn1:row,"    Cancelled/Billed:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        clear   str7
        move    C0,F44
        div     LMPend(16),LMPend(20),F44
        mult    "100",F44
        move    F44,str7
        clear   str7b
        move    C0,F44
        div     LMPend(1),LMPend(20),F44
        mult    "100",F44
        move    F44,str7b
        move    LMPend(20),str9
          prtpage prfile;*pcolumn1:row,"    Cancelled Order:";
          pack      str8,str7,"%"
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,str8;
          pack      str8,str7b,"%"
          prtpage prfile;*pcolumn6:row,str8;
          call      PrintAmount using str9,str15
        if (RptFlag = C2)
                pack    APIFileName,"C:\WORK\STATS4.SRT",hexzero
                call    FindFirstFile
                if (APIResult <> 0 & APIResult <> hexeight)
                              add       eightlpi,row
                        call  PrintSubHeader
                        open    tempfile2,"C:\WORK\STATS4.SRT",EXCLUSIVE
                        loop
                                read    tempfile2,SEQ;str2,OMLRNUM,MCOMP,OLNUM,OMLRPON,str8,OLRN,OSTAT
                                until over
.                                if (OSTAT = "l" | OSTAT = "z")
                                        call    SetVars using C2
                                        call      PrintDetail using C2
.                                endif
                        repeat
.                             prtpage prfile;*NEWPAGE;
.                             call      PrintHeader using C2
                        close   tempfile2
                    else
                              prtpage prfile;*NEWPAGE;
                    endif
          else
                    prtpage prfile;*NEWPAGE;
        endif
        close   tempfile1
.
        close   tempfile
EndOfJob
          PRTCLOSE prfile
.
          close     NORDFLE2
        pack    APIFileName,"c:\work\ninord4.srt",hexzero
        call    DeleteFile
        pack    APIFileName,"c:\work\ninord5.srt",hexzero
        call    DeleteFile
        pack    APIFileName,"c:\work\stats1.dat",hexzero
        call    DeleteFile
        pack    APIFileName,"c:\work\stats1.srt",hexzero
        call    DeleteFile
        pack    APIFileName,"c:\work\stats2.dat",hexzero
        call    DeleteFile
        pack    APIFileName,"c:\work\stats2.srt",hexzero
        call    DeleteFile
        pack    APIFileName,"c:\work\stats3.dat",hexzero
        call    DeleteFile
        pack    APIFileName,"c:\work\stats3.srt",hexzero
        call    DeleteFile
        pack    APIFileName,"c:\work\stats4.dat",hexzero
        call    DeleteFile
        pack    APIFileName,"c:\work\stats4.srt",hexzero
        call    DeleteFile
        shutdown

PrintHeader LRoutine FrmPtr
.Starting point for first row set here - if this changes border value needs to change!!!!!!!
        add     C1,page
        prtpage prfile;*UNITS=*HIENGLISH,*ORIENT=*LANDSCAPE,*DUPLEX=3;
        move    "300",row
        prtpage prfile;*p9000:50,*font=font2,*uloff,"page ",page;
.START PATCH 1.3 REPLACED LOGIC
.        prtpage prfile;*p2700:row,*font=font10,"Names";
.        prtpage prfile;*font=font11,"  in the News";
          prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
.END PATCH 1.3 REPLACED LOGIC
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
.START PATCH 1.3 REPLACED LOGIC
.        prtpage prfile;*p1000:row,*pensize=10,*line=7100:row;
.        add     "60",row
.        prtpage prfile;*p2700:row,*font=font12,"C  A  L  I  F  O  R  N  I  A        I  N  C .";
..Go ahead and print the last line now
..Bullets produced using:  Alt+0149
.        prtpage prfile;*p1500:9950,*font=font13,"1300 Clay St., 11th Floor, Oakland, CA 94612-1429 • 415-989-3350 • Fax 415-433-7796";
        add     "60",row
        add     eightlpi,row
.END PATCH 1.3 REPLACED LOGIC        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        unpack  timestamp,str4,MM,DD
        pack    str10,MM,SLASH,DD,SLASH,str4
        prtpage prfile;*pcolumn7:row,*font=font2,"Today's Date:  ",str10;
        add     eightlpi,row
        add     eightlpi,row
          if (FrmPtr = C1)
                  prtpage prfile;*p4500:row,*font=font5,*boldon,"LCR STATUS REPORT",*boldoff;
          else
                  prtpage prfile;*p3950:row,*font=font5,*boldon,"PENDING RECORD STATUS REPORT",*boldoff;
          endif
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,*font=font13,"From Date:";
        move    FromDate,str10
          rep       zfill,str10
        unpack  str10,str4,MM,DD
        pack    str10,MM,SLASH,DD,SLASH,str4
        prtpage prfile;*p1300:row,str10;
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"To Date:";
        move    ToDate,str10
          rep       zfill,str10
        unpack  str10,str4,MM,DD
        pack    str10,MM,SLASH,DD,SLASH,str4
        prtpage prfile;*p1300:row,str10;
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
.         if (DpmtFlag = C3)
.                 prtpage prfile;*pcolumn:row,"Contact:";
.         else
.                 prtpage prfile;*pcolumn:row,"Caller:";
.         endif
.        move    C1,NCNTPATH
.        move    "NCNTKEY",Location
.        pack    KeyLocation,"Key: ",NCNTFLD
.        call    NCNTKEY
.        prtpage prfile;*p1300:row,CNTNAME;
.        add     eightlpi,row
.        add     eightlpi,row
        return

PrintSubHeader
        prtpage prfile;*pcolumn:row,*font=font5,*boldon,"LR";
        prtpage prfile;*pcolumn1:row,"Mailer";
        prtpage prfile;*pcolumn5:row,"List";
        prtpage prfile;*pcolumn6:row,"Mail Date";
        prtpage prfile;*pcolumn3:row,"P.O.";
        prtpage prfile;*pcolumn7:row,"Sub-Status";
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,*pensize=20,*line=10000:row;
        add     eightlpi,row
        return

PrintDetail LRoutine FrmPtr
.TEST FOR ENOUGH ROOM ON PAGE
        if (row >= 7375)
                prtpage prfile;*NEWPAGE;
                    if (page2 = C1)
                              move      C0,page2
                          call    PrintHeader using FrmPtr
                    else
                              move      C1,page2
                              move      "300",row
                    endif
                call    PrintSubHeader
        endif
        prtpage prfile;*pcolumn:row,*font=font2,*boldoff,OLRN,str2;
        prtpage prfile;*pcolumn1:row,OMLRNUM," ",MCOMP;
        prtpage prfile;*pcolumn5:row,OLNUM;
        prtpage prfile;*pcolumn6:row,str12;
        prtpage prfile;*pcolumn3:row,OMLRPON;
        prtpage prfile;*pcolumn7:row,str25;
        add     eightlpi,row
          return

PrintAmount LRoutine DimPtr,DimPtr1
          call      FormatNumeric using DimPtr,DimPtr1
          rep       zfill,DimPtr1
.         if (DimPtr1 = "")
.                   move      "0",DimPtr1
.         endif
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,DimPtr1,*ALIGNMENT=*LEFT;
        add     eightlpi,row
          return

SetVars LRoutine FrmPtr
        type    str2
        if not equal
                if (str2 = "LL")
                        move    "Live Order",str25
                elseif (str2 = "BB")
                        move    "Billed Order",str25
                elseif (str2 = "CB")
                        move    "Cancelled Billed",str25
                elseif (str2 = "CO")
                        move    "Cancelled Order",str25
                endif
        else
                clear   str25
                if (FrmPtr = C1)       .LCRS
                        pack    NPNDFLD,"l",str2
                elseif (FrmPtr = C2)   .Pending Orders
                        pack    NPNDFLD,"p",str2
                endif
                move    "SetVars-NPNDKEY",Location
                pack    KeyLocation,"Key: ",NPNDFLD
                call    NPNDKEY
                if not over
                        move    NPNDDESC,str25
                endif
        endif
        call    Trim using str25
        unpack  str8,CC,YY,MM,DD
        pack    str12,MM,SLASH,DD,SLASH,CC,YY
        return

          include   nordio.inc
          include   nord5io.inc
          include   nord4io.inc
          include   npndio.inc
          include   ncntio.inc
.START PATCH 1.2 REPLACED LOGIC
.         include   nmlrio.inc
          include   compio.inc
          include   cntio.inc
.END PATCH 1.2 REPLACED LOGIC
          include   comlogic.inc
