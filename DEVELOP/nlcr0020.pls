PC        Equ       0
          include   common.inc
          include   cons.inc
          include   norddd.inc
          include   nord5dd.inc
          include   nord4dd.inc
          include   npnddd.inc
        include winapi.inc

release   init      "1.0"

.tempfile file
tempfile1 file
tempfile2 file
tempfile3 file
.Filters/Flags
FromDate init       "00000000"
ToDate    init      "99999999"
DpmtFlag form       1       .which Departments do you wish to see
RptFlag form    1       .Summary or Detail Report
SrtFlag form    1       .Sort Order for Detail Report
SalesLCR form       9(15)
LMLCR     form      9(15)
SalesPend form  9(20)
LMPend  form    9(20)
F44     form    4.4
.N11     form    11
str7b   dim     7
FrmPtr  form    ^
.hexeight integer 4,"4294967295"

          move      "20070101",FromDate
          move      "20120701",ToDate
          move      C1,DpmtFlag
        move    C1,RptFlag
        move    C1,SrtFlag

.LCR Section
        move    C3,NORDLOCK
        move    C1,NORDPATH
        clock   timestamp,timestamp
        pack    taskname,"c:\work\",timestamp,".dat"
        prepare tempfile,taskname,EXCLUSIVE
        if (RptFlag = C1)
                prepare tempfile2,"c:\work\stats1.dat",EXCLUSIVE
                prepare tempfile3,"c:\work\stats2.dat",EXCLUSIVE
        endif
        call    Paint
.        goto drew
        DISPLAY *P10:12,"Sorting NINORD5.DAT"
        clear   taskname
..        pack    taskname,"\\nins1\E\DATA\TEXT\NINORD5.DAT,C:\WORK\NINORD5.SRT;S=#"9>='",FromDate,"'&9<='",ToDate,"'#""
.        pack    taskname,"c:\work\NINORD5.DAT,C:\WORK\NINORD5.SRT;S=#"9>='",FromDate,"'&9<='",ToDate,"'#""
.        sort    taskname
                  pack    taskname,"\\nins1\c\netutils\sort32.exe \\nins1\E\DATA\TEXT\NINORD5.DAT C:\WORK\NINORD5.SRT /inc(9,8,n,ge,\""",FromDate,"\"",and,9,8,n,le,\""",ToDate,"\"") VER"
                    execute   taskname
        move        "NLCR0020-Open NINORD5.SRT",Location
          pack      KeyLocation,"Sequential"
        TRAP    IOMssg Giving Error if IO
        open    tempfile1,"C:\WORK\NINORD5.SRT",read
        TRAPCLR IO
          loop
                    move      "NLCR0020-Read NINORD5.SRT",Location
                    pack      KeyLocation,"Sequential"
                TRAP    IOMssg Giving Error if IO
                    READ    tempfile1,SEQ;ord5vars
                    until over
                TRAPCLR IO
                add     C1,N11
                DISPLAY *P10:12,"LCR Records Processed : ",N11
                call    Trim using NORD5LR
                if (NORD5LR <> "")
                        move    NORD5LR,NORDFLD
                    move      "NLCR0020-NORDKEY",Location
                        pack  KeyLocation,"Key: ",NORDFLD
                        call  NORDKEY
                        if not over
                                if (OSALES10 = "0" AND OSALES = "6")    .List Management
                                        add     C1,LMLCR(1)
                                        if (OSTAT = "l")
                                                if (RptFlag = C1)
                                                        write   tempfile3,SEQ;NORD5STAT,OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
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
                                                endif
                                        elseif (OSTAT = "z")                    .Cancelled
                                                if (RptFlag = C1)
                                                        write   tempfile3,SEQ;"05",OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
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
                                                        if (RptFlag = C1)
                                                                write   tempfile3,SEQ;"LL",OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,LMLCR(12)
                                                elseif (OSTAT = "B")            .Billed
                                                        if (RptFlag = C1)
                                                                write   tempfile3,SEQ;"BB",OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,LMLCR(13)
                                                elseif (OSTAT = "Q")            .Cancelled/Billed
                                                        if (RptFlag = C1)
                                                                write   tempfile3,SEQ;"CB",OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,LMLCR(14)
                                                elseif (OSTAT = "X")            .Cancelled Order
                                                        if (RptFlag = C1)
                                                                write   tempfile3,SEQ;"CO",OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,LMLCR(15)
                                                endif
                                        endif
                                else
                                        add     C1,SalesLCR(1)
                                        if (OSTAT = "l")
                                                if (RptFlag = C1)
                                                        write   tempfile2,SEQ;NORD5STAT,OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
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
                                                endif
                                        elseif (OSTAT = "z")                    .Cancelled
                                                if (RptFlag = C1)
                                                        write   tempfile2,SEQ;"05",OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
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
                                                        if (RptFlag = C1)
                                                                write   tempfile2,SEQ;"LL",OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,SalesLCR(12)
                                                elseif (OSTAT = "B")            .Billed
                                                        if (RptFlag = C1)
                                                                write   tempfile2,SEQ;"BB",OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,SalesLCR(13)
                                                elseif (OSTAT = "Q")            .Cancelled/Billed
                                                        if (RptFlag = C1)
                                                                write   tempfile2,SEQ;"CB",OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,SalesLCR(14)
                                                elseif (OSTAT = "X")            .Cancelled Order
                                                        if (RptFlag = C1)
                                                                write   tempfile2,SEQ;"CO",OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,SalesLCR(15)
                                                endif
                                        endif
                                endif
                        endif
                endif
          repeat
        if (RptFlag = C1)
                close   tempfile2
                close   tempfile3
                clear   taskname
                append  "C:\WORK\STATS1.DAT,C:\WORK\STATS1.SRT;",taskname
                append  "3-6,7-12,13-24,25-32",taskname
                reset   taskname
                sort    taskname
                clear   taskname
                append  "C:\WORK\STATS2.DAT,C:\WORK\STATS2.SRT;",taskname
                append  "3-6,7-12,13-24,25-32",taskname
                reset   taskname
                sort    taskname
        endif

.
        DISPLAY *P10:12,"Creating LCR Stat Report"
        write   tempfile,SEQ;"LCR Stat Report"
        write   tempfile,SEQ;""
        move    FromDate,str10
        unpack  str10,CC,YY,MM,DD
        pack    str10,MM,SLASH,DD,SLASH,CC,YY
        write   tempfile,SEQ;"From Date: ",str10
        move    ToDate,str10
        unpack  str10,CC,YY,MM,DD
        pack    str10,MM,SLASH,DD,SLASH,CC,YY
        write   tempfile,SEQ;"To Date:   ",str10
        write   tempfile,SEQ;""
        write   tempfile,SEQ;""
        clear   str11
        move    N11,str11
        write   tempfile,SEQ;"Total Records Read:                ",str11
        write   tempfile,SEQ;""
        move    C0,N10
        add     SalesLCR(1),LMLCR(1),N10
        move    N10,str10
        write   tempfile,SEQ;"Total LCRs:                         ",str10
        write   tempfile,SEQ;""
        write   tempfile,SEQ;""
        clear   str7
        move    C0,F44
        div     N10,SalesLCR(1),F44
        mult    "100",F44
        move    F44,str7
        move    SalesLCR(1),str9
        write   tempfile,SEQ;"Sales Department                     ",str9,"       ",str7,"% of Total LCRs"
        write   tempfile,SEQ;""
        if (RptFlag = C1)
                pack    APIFileName,"C:\WORK\STATS1.SRT",hexzero
                call    FindFirstFile
                if (APIResult <> 0 & APIResult <> hexeight)
                        write   tempfile,SEQ;"LR       Mailer  List     Mail Date    PO             Type"
                        write   tempfile,SEQ;""
                        open    tempfile2,"C:\WORK\STATS1.SRT",EXCLUSIVE
                        loop
                                read    tempfile2,SEQ;str2,OMLRNUM,OLNUM,OMLRPON,str8,OLRN,OSTAT
                                until over
                                if (OSTAT = "l" | OSTAT = "z")
                                        call    SetVars using C1
                                        write   tempfile,SEQ;OLRN,"   ",OMLRNUM,"    ",OLNUM,"   ",str12,"   ",OMLRPON,"   ",str25
                                endif
                        repeat
                        close   tempfile2
                        write   tempfile,SEQ;""
                endif
        endif
        clear   str7
        move    C0,F44
        div     SalesLCR(1),SalesLCR(2),F44
        mult    "100",F44
        move    F44,str7
        move    SalesLCR(2),str9
        write   tempfile,SEQ;"  Records which are still LCRs:      ",str9,"       ",str7,"% of Sales LCRs"
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
        write   tempfile,SEQ;"        Status of 'LCR':             ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"        Status of '1st Request':     ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"        Status of '2nd Request':     ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"        Status of 'Revised Request': ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"        Status of 'Approved':        ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"        Status of 'Cancelled':       ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"        Status of 'Pending':         ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"        Status of 'Denied':          ",str9,"       ",str7,"%",str7b,"%"
        write   tempfile,SEQ;""
        clear   str7
        move    C0,F44
        div     SalesLCR(1),SalesLCR(11),F44
        mult    "100",F44
        move    F44,str7
        move    SalesLCR(11),str9
        write   tempfile,SEQ;"  LCRs Turned into Live Orders:      ",str9,"       ",str7,"% of Sales Records"
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
        write   tempfile,SEQ;"        Currently Live:              ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"        Billed:                      ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"        Cancelled/Billed:            ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"        Cancelled Order:             ",str9,"       ",str7,"%",str7b,"%"
.
        write   tempfile,SEQ;""
        write   tempfile,SEQ;""
        clear   str7
        move    C0,F44
        div     N10,LMLCR(1),F44
        mult    "100",F44
        move    F44,str7
        move    LMLCR(1),str9
        write   tempfile,SEQ;"List Management Department           ",str9,"       ",str7,"% of Total LCRs"
        write   tempfile,SEQ;""
        if (RptFlag = C1)
                pack    APIFileName,"C:\WORK\STATS2.SRT",hexzero
                call    FindFirstFile
                if (APIResult <> 0 & APIResult <> hexeight)
                        write   tempfile,SEQ;"LR       Mailer  List     Mail Date    PO             Type"
                        write   tempfile,SEQ;""
                        open    tempfile2,"C:\WORK\STATS2.SRT",EXCLUSIVE
                        loop
                                read    tempfile2,SEQ;str2,OMLRNUM,OLNUM,OMLRPON,str8,OLRN,OSTAT
                                until over
                                if (OSTAT = "l" | OSTAT = "z")
                                        call    SetVars using C1
                                        write   tempfile,SEQ;OLRN,"   ",OMLRNUM,"    ",OLNUM,"   ",str12,"   ",OMLRPON,"   ",str25
                                endif
                        repeat
                        close   tempfile2
                        write   tempfile,SEQ;""
                endif
        endif
        clear   str7
        move    C0,F44
        div     LMLCR(1),LMLCR(2),F44
        mult    "100",F44
        move    F44,str7
        move    LMLCR(2),str9
        write   tempfile,SEQ;"  Records which are still LCRs:      ",str9,"       ",str7,"% of LM LCRs"
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
        write   tempfile,SEQ;"        Status of 'LCR':             ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"        Status of '1st Request':     ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"        Status of '2nd Request':     ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"        Status of 'Revised Request': ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"        Status of 'Approved':        ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"        Status of 'Cancelled':       ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"        Status of 'Pending':         ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"        Status of 'Denied':          ",str9,"       ",str7,"%",str7b,"%"
        write   tempfile,SEQ;""
        clear   str7
        move    C0,F44
        div     LMLCR(1),LMLCR(11),F44
        mult    "100",F44
        move    F44,str7
        move    LMLCR(11),str9
        write   tempfile,SEQ;"  LCRs Turned into Live Orders:      ",str9,"       ",str7,"% of LM Records"
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
        write   tempfile,SEQ;"        Currently Live:              ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"        Billed:                      ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"        Cancelled/Billed:            ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"        Cancelled Order:             ",str9,"       ",str7,"%",str7b,"%"
        close   tempfile1

.Pending Order Section
drew
.         move      "20010101",FromDate
.         move      "20011231",ToDate
        move    C0,N11
        if (RptFlag = C1)
                prepare tempfile2,"c:\work\stats3.dat",EXCLUSIVE
                prepare tempfile3,"c:\work\stats4.dat",EXCLUSIVE
        endif
        call    Paint
        DISPLAY *P10:12,"Sorting NINORD4.DAT"
        clear   taskname
.        pack    taskname,"\\nins1\E\DATA\TEXT\NINORD4.DAT,C:\WORK\NINORD4.SRT;S=#"9>='",FromDate,"'&9<='",ToDate,"'#""
.        sort    taskname
        pack    taskname,"\\nins1\c\netutils\sort32.exe \\nins1\E\DATA\TEXT\NINORD4.DAT C:\WORK\NINORD4.SRT /inc(9,8,n,ge,\""",FromDate,"\"",and,9,8,n,le,\""",ToDate,"\"") VER"
          execute   taskname
        move        "NLCR0020-Open NINORD4.SRT",Location
          pack      KeyLocation,"Sequential"
        TRAP    IOMssg Giving Error if IO
        open    tempfile1,"C:\WORK\NINORD4.SRT"
        TRAPCLR IO
          loop
                    move      "NLCR0020-Read NINORD4.SRT",Location
                    pack      KeyLocation,"Sequential"
                TRAP    IOMssg Giving Error if IO
                    READ    tempfile1,SEQ;ord4vars
                    until over
                TRAPCLR IO
                add     C1,N11
                DISPLAY *P10:12,"Pending Records Processed : ",N11
                call    Trim using NORD4LR
                if (NORD4LR <> "")
                        move    NORD4LR,NORDFLD
                    move      "NLCR0020-NORDKEY,2",Location
                        pack  KeyLocation,"Key: ",NORDFLD
                        call  NORDKEY
                        if not over
                                if (OSALES10 = "0" AND OSALES = "6")    .List Management
                                        add     C1,LMPend(1)
                                        if (OSTAT = "p")
                                                if (RptFlag = C1)
                                                        write   tempfile3,SEQ;NORD4STAT,OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
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
                                                endif
                                        elseif (OSTAT = "x")                    .Cancelled/Denied
                                                add     C1,LMPend(2)
                                                if (OHIST = "p")                .Cancelled
                                                        if (RptFlag = C1)
                                                                write   tempfile3,SEQ;"07",OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,LMPend(10)
                                                elseif (OHIST = "z")            .Denied
                                                        if (RptFlag = C1)
                                                                write   tempfile3,SEQ;"06",OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,LMPend(9)
                                                endif
                                        else                                    .Live Orders
                                                add     C1,LMPend(16)
                                                if (OSTAT = "0")                .Live
                                                        if (RptFlag = C1)
                                                                write   tempfile3,SEQ;"LL",OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,LMPend(17)
                                                elseif (OSTAT = "B")            .Billed
                                                        if (RptFlag = C1)
                                                                write   tempfile3,SEQ;"BB",OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,LMPend(18)
                                                elseif (OSTAT = "Q")            .Cancelled/Billed
                                                        if (RptFlag = C1)
                                                                write   tempfile3,SEQ;"CB",OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,LMPend(19)
                                                elseif (OSTAT = "X")            .Cancelled Order
                                                        if (RptFlag = C1)
                                                                write   tempfile3,SEQ;"CO",OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,LMPend(20)
                                                endif
                                        endif
                                else
                                        add     C1,SalesPend(1)
                                        if (OSTAT = "p")
                                                if (RptFlag = C1)
                                                        write   tempfile2,SEQ;NORD4STAT,OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
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
                                                        if (RptFlag = C1)
                                                                write   tempfile2,SEQ;"07",str2,OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,SalesPend(10)
                                                elseif (OHIST = "z")            .Denied
                                                        if (RptFlag = C1)
                                                                write   tempfile2,SEQ;"06",str2,OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,SalesPend(9)
                                                endif
                                        else                                    .Live Orders
                                                add     C1,SalesPend(16)
                                                if (OSTAT = "0")                .Live
                                                        if (RptFlag = C1)
                                                                write   tempfile2,SEQ;"LL",str2,OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,SalesPend(17)
                                                elseif (OSTAT = "B")            .Billed
                                                        if (RptFlag = C1)
                                                                write   tempfile2,SEQ;"BB",str2,OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,SalesPend(18)
                                                elseif (OSTAT = "Q")            .Cancelled/Billed
                                                        if (RptFlag = C1)
                                                                write   tempfile2,SEQ;"CB",str2,OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,SalesPend(19)
                                                elseif (OSTAT = "X")            .Cancelled Order
                                                        if (RptFlag = C1)
                                                                write   tempfile2,SEQ;"CO",str2,OMLRNUM,OLNUM,OMLRPON,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                                                        endif
                                                        add     C1,SalesPend(20)
                                                endif

                                        endif
                                endif
                        endif
                endif
          repeat
        if (RptFlag = C1)
                close   tempfile2
                close   tempfile3
                clear   taskname
                append  "C:\WORK\STATS3.DAT,C:\WORK\STATS3.SRT;",taskname
                append  "3-6,7-12,13-24,25-32",taskname
                reset   taskname
                sort    taskname
                clear   taskname
                append  "C:\WORK\STATS4.DAT,C:\WORK\STATS4.SRT;",taskname
                append  "3-6,7-12,13-24,25-32",taskname
                reset   taskname
                sort    taskname
        endif
.
.Force new page
        DISPLAY *P10:12,"Creating Pending Stat Report"
        write   tempfile,SEQ;""
        write   tempfile,SEQ;""
        write   tempfile,SEQ;""
        write   tempfile,SEQ;""
        write   tempfile,SEQ;""
        write   tempfile,SEQ;""
        write   tempfile,SEQ;""
        write   tempfile,SEQ;""
        write   tempfile,SEQ;""
        write   tempfile,SEQ;""
        write   tempfile,SEQ;""
        write   tempfile,SEQ;""
        write   tempfile,SEQ;""
        write   tempfile,SEQ;""
.
        write   tempfile,SEQ;""
        write   tempfile,SEQ;""
        write   tempfile,SEQ;"Pending Record Stat Report"
        write   tempfile,SEQ;""
        move    FromDate,str10
        unpack  str10,CC,YY,MM,DD
        pack    str10,MM,SLASH,DD,SLASH,CC,YY
        write   tempfile,SEQ;"From Date: ",str10
        move    ToDate,str10
        unpack  str10,CC,YY,MM,DD
        pack    str10,MM,SLASH,DD,SLASH,CC,YY
        write   tempfile,SEQ;"To Date:   ",str10
        write   tempfile,SEQ;""
        write   tempfile,SEQ;""
        clear   str11
        move    N11,str11
        write   tempfile,SEQ;"Total Records Read:                ",str11
        write   tempfile,SEQ;""
        move    C0,N10
        add     SalesPend(1),LMPend(1),N10
        move    N10,str10
        write   tempfile,SEQ;"Total Pending Records:              ",str10
        write   tempfile,SEQ;""
        write   tempfile,SEQ;""
        clear   str7
        move    C0,F44
        div     N10,SalesPend(1),F44
        mult    "100",F44
        move    F44,str7
        move    SalesPend(1),str9
        write   tempfile,SEQ;"Sales Department                     ",str9,"       ",str7,"% of Total Records"
        write   tempfile,SEQ;""
        if (RptFlag = C1)
                pack    APIFileName,"C:\WORK\STATS3.SRT",hexzero
                call    FindFirstFile
                if (APIResult <> 0 & APIResult <> hexeight)
                        write   tempfile,SEQ;"LR       Mailer  List     Mail Date    PO             Type"
                        write   tempfile,SEQ;""
                        open    tempfile2,"C:\WORK\STATS3.SRT",EXCLUSIVE
                        loop
                                read    tempfile2,SEQ;str2,OMLRNUM,OLNUM,OMLRPON,str8,OLRN,OSTAT
                                until over
                                if (OSTAT = "l" | OSTAT = "z")
                                        call    SetVars using C2
                                        write   tempfile,SEQ;OLRN,"   ",OMLRNUM,"    ",OLNUM,"   ",str12,"   ",OMLRPON,"   ",str25
                                endif
                        repeat
                        close   tempfile2
                        write   tempfile,SEQ;""
                endif
        endif
        clear   str7
        move    C0,F44
        div     SalesPend(1),SalesPend(2),F44
        mult    "100",F44
        move    F44,str7
        move    SalesPend(2),str9
        write   tempfile,SEQ;"  Records which are still Pending:   ",str9,"       ",str7,"% of Sales Records"
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
        write   tempfile,SEQ;"    Pending List Owner Approval:     ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Waiting for Guarantee:           ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Waiting for Broker Call Back:    ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Waiting for Pre-Payment:         ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Waiting for Revision:            ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    At Service Bureau:               ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Denied:                          ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Cancelled:                       ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Approved:                        ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Order Received:                  ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Accounting Hold:                 ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    2nd Request:                     ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Revised Request:                 ",str9,"       ",str7,"%",str7b,"%"
        write   tempfile,SEQ;""
        clear   str7
        move    C0,F44
        div     SalesPend(1),SalesPend(16),F44
        mult    "100",F44
        move    F44,str7
        move    SalesPend(16),str9
        write   tempfile,SEQ;"  Pending Records turned into Orders:",str9,"       ",str7,"% of Sales Records"
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
        write   tempfile,SEQ;"    Currently Live:                  ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Billed:                          ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Cancelled/Billed:                ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Cancelled Order:                 ",str9,"       ",str7,"%",str7b,"%"
.
        write   tempfile,SEQ;""
        write   tempfile,SEQ;""
        clear   str7
        move    C0,F44
        div     N10,LMPend(1),F44
        mult    "100",F44
        move    F44,str7
        move    LMPend(1),str9
        write   tempfile,SEQ;"List Management Department           ",str9,"       ",str7,"% of Total Records"
        write   tempfile,SEQ;""
        if (RptFlag = C1)
                pack    APIFileName,"C:\WORK\STATS4.SRT",hexzero
                call    FindFirstFile
                if (APIResult <> 0 & APIResult <> hexeight)
                        write   tempfile,SEQ;"LR       Mailer  List     Mail Date    PO             Type"
                        write   tempfile,SEQ;""
                        open    tempfile2,"C:\WORK\STATS4.SRT",EXCLUSIVE
                        loop
                                read    tempfile2,SEQ;str2,OMLRNUM,OLNUM,OMLRPON,str8,OLRN,OSTAT
                                until over
                                if (OSTAT = "l" | OSTAT = "z")
                                        call    SetVars using C2
                                        write   tempfile,SEQ;OLRN,"   ",OMLRNUM,"    ",OLNUM,"   ",str12,"   ",OMLRPON,"   ",str25
                                endif
                        repeat
                        close   tempfile2
                        write   tempfile,SEQ;""
                endif
        endif
        clear   str7
        move    C0,F44
        div     LMPend(1),LMPend(2),F44
        mult    "100",F44
        move    F44,str7
        move    LMPend(2),str9
        write   tempfile,SEQ;"  Records which are still Pending:   ",str9,"       ",str7,"% of LM Records"
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
        write   tempfile,SEQ;"    Pending List Owner Approval:     ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Waiting for Guarantee:           ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Waiting for Broker Call Back:    ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Waiting for Pre-Payment:         ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Waiting for Revision:            ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    At Service Bureau:               ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Denied:                          ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Cancelled:                       ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Approved:                        ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Order Received:                  ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Accounting Hold:                 ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    2nd Request:                     ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Revised Request:                 ",str9,"       ",str7,"%",str7b,"%"
        write   tempfile,SEQ;""
        clear   str7
        move    C0,F44
        div     LMPend(1),LMPend(16),F44
        mult    "100",F44
        move    F44,str7
        move    LMPend(16),str9
        write   tempfile,SEQ;"  Pending Records turned into Orders:",str9,"       ",str7,"% of LM Records"
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
        write   tempfile,SEQ;"    Currently Live:                  ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Billed:                          ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Cancelled/Billed:                ",str9,"       ",str7,"%",str7b,"%"
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
        write   tempfile,SEQ;"    Cancelled Order:                 ",str9,"       ",str7,"%",str7b,"%"
        close   tempfile1
.
        close   tempfile
.
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
.        pack    APIFileName,taskname,hexzero
.        call    DeleteFile
        shutdown

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
          include   comlogic.inc
