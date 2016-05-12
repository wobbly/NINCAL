          include   common.inc
          include   cons.inc
          include   norddd.inc
          include   nord5dd.inc
          include   nord4dd.inc
          include   npnddd.inc
          include   ncntdd.inc
.patch1.1
          include   compdd.inc
          include   cntdd.inc
.         include   nmlrdd.inc
.patch1.1
        include winapi.inc
release   init	"1.24"        DLH	New Pending Status added
Reldate   Init      "2014 February 7"
.release   init                "1.23"        ASH	New LCR Status added
.Reldate   Init      "03 JUNE 2013"
.release   init                "1.22"        DLH   Internal AAmdex
.Reldate   Init      "23 April 2008"
.release  init      "1.21"        JD       23Nov2005        PLB 9.0 new aimdex ver.
.release  init      "1.2"        ASH    06AUG2004 Logo Conversion
.release  init      "1.1"        DMB    26MAY2004 Mailer Conversion
.release  init      "1.0"

.tempfile file
tempfile1 file
tempfile2 file
tempfile3 file
prfile  pfile
.Filters/Flags
FromDate form       "00000000"
ToDate    form      "99999999"
TotRec    form      9
.osflag   form   1       .1=win 95,98, 2=NT
DpmtFlag form       1       .Caller or Contact?
RptFlag form    1       .Summary or Detail Report
SrtFlag form    1       .Sort Order for Detail Report
PrtFlag   form      1
LastRecord form     1
SalesLCR form       9(15)
LMLCR     form      9(15)
SalesPend form  9(20)
LMPend  form    9(20)
F44     form    4.4
.N11     form    11
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
font5   font
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=9
        create  font3,"Arial",size=9,italic
        create  font5,"Arial",size=11

.START PATCH 1.2 ADDED LOGIC
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 1.2 ADDED LOGIC

          move      C0,TotRec

        clock   timestamp,timestamp
          unpack    COMMENT,FromDate,ToDate,DpmtFlag,NCNTFLD,RptFlag,PrtFlag

.        pack    APIFileName,"c:\work\ninash.aam",hexzero
.        call    DeleteFile
.        pack    APIFileName,"c:\work\stats1.dat",hexzero
.        call    DeleteFile
.        pack    APIFileName,"c:\work\stats1.srt",hexzero
.        call    DeleteFile

.Find out system information
        getinfo system,str6
        unpack  str6 into str1,str1
        move    C0,osflag
        if (str1 = "3" or str1 = "4")           .95/98
                move    C1,osflag
        elseif (str1 = "1"or str1 = "5")        .NT4/NT5
                move    C2,osflag
        endif

.LCR Section
        move    C3,NORDLOCK
        move    C1,NORDPATH

.Set up columns
        move    "500",column
        move    "1200",column1
        move    "2700",column2
        move    "3450",column3
        move    "4200",column4
        move    "4950",column5
        move    "5700",column6
        move    "6700",column7
        if (RptFlag = C2)
                prepare tempfile2,"c:\work\stats1.dat",EXCLUSIVE
        endif
.        call    Paint
          if (DpmtFlag = C4)  .Caller
                    clear     NORDFLD1
                    pack      NORDFLD2,"02X",NCNTFLD
          else                          .Contact
                    pack      NORDFLD1,"01X",NCNTFLD
                    clear     NORDFLD2
          endif
          clear     taskname
.         pack      taskname,"!f:\apps\plb\code\sunadxnt \\nins1\e\data\text\ninord.dat c:\work\ninash, l408 -198-199,200-201,2,176"
.>Patch 1.21 Begin
.begin patch 1.22
.         pack      taskname,"!f:\apps\plb\code\sunaamdx \\nins1\e\data\text\ninord.dat c:\work\ninash, l408 -198-199,200-201,2,176"
          pack      taskname,"\\nins1\e\data\text\ninord.dat c:\work\ninash, l408 -198-199,200-201,2,176"
.>Patch 1.21 End
          display *P10:12,"Aamdexing NINORD.DAT, Please be patient!"
          AAmdex    Taskname
.         execute   taskname
.end patch 1.22
          display *P10:12,"Opening NINORD.DAT.                     "
          pack      NORDNME2,"c:\work\ninash"
          call      NORDOPN2
          display *P10:12,"Reading NINORD.DAT.                     "
.         for       COUNTER from "1" TO "4"
.                   if (COUNTER = 1)
.                             pack      NORDFLD3,"03Xl"
.                             pack      NORDFLD4,"04XE"
.                   elseif (COUNTER = 2)
.                             pack      NORDFLD3,"03Xp"
.                             pack      NORDFLD4,"04XE"
.                   elseif (COUNTER = 3)
.                             pack      NORDFLD3,"03Xz"
.                             clear     NORDFLD4
.                   elseif (COUNTER = 4)
.                             pack      NORDFLD3,"03Xx"
.                             clear     NORDFLD4
.                   else
.                             break     .safety measure, should never come here
.                   endif
                    move      "NLCR020A-Read NINASH",Location
                    pack      KeyLocation,"Key: ",NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4
                trap          IOMssg Giving Error if IO
                    read      NORDFLE2,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4;ORDVARS
                    trapclr   IO
                    loop
                              until over
                              move      C0,N8
                              pack      str8,OODTEC,OODTEY,OODTEM,OODTED
                              move      str8,N8
                              if (N8 = C0 OR (N8 >= FromDate AND N8 <= ToDate))
                                        add       C1,TotRec
                                        if (OSTAT = "l" | OSTAT = "z" | OSTAT = "p" | OSTAT = "x")
                                        if (OSTAT = "l" | OSTAT = "z")
                                                  move    OLRN,NORD5FLD
                                            rep     zfill in NORD5FLD
                                            clear   NORD5STAT
                                            move    "NORD5KEY",Location
                                            pack    KeyLocation,"Key: ",NORD5FLD
                                            call    NORD5KEY                .get LCR info
                                        elseif (OSTAT = "p" | OSTAT = "x")
                                                  move    OLRN,NORD4FLD
                                            rep     zfill in NORD4FLD
                                            clear   NORD4STAT
                                            move    "NORD4KEY",Location
                                            pack    KeyLocation,"Key: ",NORD4FLD
                                            call    NORD4KEY                .get LCR info
                                        endif
                                        call      LCRLoadInfo
                                        endif
                              endif
                              trap      IOMssg Giving Error if IO
                              move      "NLCR020A-ReadKG NINASH",Location
                              pack      KeyLocation,"Key: ",NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4
                              readkg    NORDFLE2;ORDVARS
                              trapclr   IO
                    repeat
.         repeat
        close   tempfile1
        close   tempfile2
        close   tempfile3
        if (RptFlag = C2)
                  clear   taskname
                  append  "C:\WORK\STATS1.DAT,C:\WORK\STATS1.SRT;",taskname
                  append  "1-1,8-52,67-72,59-66",taskname
                  reset   taskname
                  sort    taskname
        endif
.
        DISPLAY *P10:12,"Creating LCR Stat Report"
.Get default printer
.        move    PORTN,NCNTFLD1
.        rep     zfill,NCNTFLD1
.        move    C3,NCNTPATH
.        move    "NCNTKEY",Location
.        pack    KeyLocation,"Key: ",NCNTFLD1
.        call    NCNTKEY
.        if over
.                move    C2,CNTPRINT    .Laser 3
.        endif
        if (PrtFlag = 1 | PRtFlag = 2)      .Sales
                if (osflag = c2 | osflag = C5)         .nt
                        PRTOPEN prfile,"\\NINs2\Laser3 Blankstock","FAXFILE.PRN"
                elseif (osflag = c1)         .win 95 98
                        PRTOPEN prfile,"Laser3 Blankstock","FAXFILE.PRN"
                else   .(osflag = c0)         .Don't know prompt for printer
                        PRTOPEN prfile,"-","FAXFILE.PRN"
                endif
.          elseif (PrtFlag = 2)          .List Management/Accounting/Data Entry/HR/Marketing
.                if (osflag = c2)         .nt
.                        PRTOPEN prfile,"\\NINs2\Laser2","FAXFILE.PRN"
.                elseif (osflag = c1)         .win 95 98
.                        PRTOPEN prfile,"Laser2","FAXFILE.PRN"
.                else   .(osflag = c0)         .Don't know prompt for printer
.                        PRTOPEN prfile,"-","FAXFILE.PRN"
.                endif
        else                            .Information Services
                if (osflag = c2)         .nt
                        PRTOPEN prfile,"\\NINs2\Laser8","FAXFILE.PRN"
                elseif (osflag = c1)         .win 95 98
                        PRTOPEN prfile,"Laser8","FAXFILE.PRN"
                else   .(osflag = c0)         .Don't know prompt for printer
                        PRTOPEN prfile,"-","FAXFILE.PRN"
                endif
.        elseif (PrtFlag = 4)  .PDF
.                clear   str25
.                append  OLON,str25
.                append  "_",str25
.                append  OLNUM,str25
.                reset   str25
.                PRTOPEN prfile,"Acrobat Distiller",str25
.                pack    str55,str25,".pdf"
        endif
          move      C0,page2
          if (RptFlag = C2)
                    call      PrintHeader
          endif
        if (RptFlag = C2)
                pack    APIFileName,"C:\WORK\stats1.SRT",hexzero
                call    FindFirstFile
                if (APIResult <> 0 & APIResult <> hexeight)
                              call      PrintSubHeader
                              pack      str45,"c:\work\STATS1.SRT"
.                        open    tempfile2,"C:\WORK\STATS1.SRT",EXCLUSIVE
                        open    tempfile2,str45,EXCLUSIVE
                              clear     str3
                            prtpage prfile;*pcolumn:row,*font=font3,*boldon,"Active Records",*boldoff;
                              add       eightlpi,row
                        loop
                                        read    tempfile2,SEQ;str1,str2,OMLRNUM,MCOMP,OLNUM,str8,OLRN,OSTAT
                                        until over
                                if (OSTAT = "l" | OSTAT = "z")
                                        call    SetVars using C1
                                        else
                                        call    SetVars using C2
                                endif
                                        if (OSTAT = "p" or OSTAT = "x")
                                                  move      "p ",str2
                                        else
                                                  move      "  ",str2
                                        endif
                                        if (str3 = "" AND str1 = "2")
                                          add     eightlpi,row
                                  add     eightlpi,row
                                                prtpage prfile;*pcolumn:row,*font=font3,*boldon,"Inactive Records",*boldoff;
                                  add     eightlpi,row
                                                  move      C1,str3
                                        endif
                                        call      PrintDetail
                        repeat
                              prtpage prfile;*NEWPAGE;
                        close   tempfile2
                endif
        endif
          call      PrintTotal
          PRTCLOSE prfile
.
          close     NORDFLE2
        pack    APIFileName,"c:\work\NINASH.aam",hexzero
        call    DeleteFile
        pack    APIFileName,"c:\work\stats1.dat",hexzero
        call    DeleteFile
        pack    APIFileName,"c:\work\stats1.srt",hexzero
        call    DeleteFile
        shutdown

PrintHeader
.Starting point for first row set here - if this changes border value needs to change!!!!!!!
        add     C1,page
        prtpage prfile;*UNITS=*HIENGLISH,*DUPLEX=2;
        move    "300",row
        prtpage prfile;*p7000:50,*font=font2,*uloff,"page ",page;
.START PATCH 1.2 REPLACED LOGIC
.        prtpage prfile;*p2700:row,*font=font10,"Names";
.        prtpage prfile;*font=font11,"  in the News";
          prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
.END PATCH 1.2 REPLACED LOGIC
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
.START PATCH 1.2 REPLACED LOGIC
.        prtpage prfile;*p1000:row,*pensize=10,*line=7100:row;
.        add     "60",row
.        prtpage prfile;*p2700:row,*font=font12,"C  A  L  I  F  O  R  N  I  A        I  N  C .";
..Go ahead and print the last line now
..Bullets produced using:  Alt+0149
.        prtpage prfile;*p1500:9950,*font=font13,"1300 Clay St., 11th Floor, Oakland, CA 94612-1429 • 415-989-3350 • Fax 415-433-7796";
        add     "60",row
        add     eightlpi,row
.END PATCH 1.2 REPLACED LOGIC
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        unpack  timestamp,str4,MM,DD
        pack    str10,MM,SLASH,DD,SLASH,str4
        prtpage prfile;*pcolumn6:row,*font=font2,"Today's Date:  ",str10;
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*p3000:row,*font=font5,*boldon,"LCR STATUS REPORT",*boldoff;
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
          if (DpmtFlag = C3)
                  prtpage prfile;*pcolumn:row,"Contact:";
          else
                  prtpage prfile;*pcolumn:row,"Caller:";
          endif
        move    C1,NCNTPATH
        move    "NCNTKEY",Location
        pack    KeyLocation,"Key: ",NCNTFLD
        call    NCNTKEY
        prtpage prfile;*p1300:row,CNTNAME;
        add     eightlpi,row
        add     eightlpi,row
        return


PrintSubHeader
        prtpage prfile;*pcolumn:row,*font=font5,*boldon,"LR";
        prtpage prfile;*pcolumn1:row,"Mailer";
        prtpage prfile;*pcolumn5:row,"List";
        prtpage prfile;*pcolumn6:row,"Mail Date";
        prtpage prfile;*pcolumn7:row,"Sub-Status";
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,*pensize=20,*line=7800:row;
        add     eightlpi,row
        return

PrintDetail
.TEST FOR ENOUGH ROOM ON PAGE
        if (row >= 9500)
                prtpage prfile;*NEWPAGE;
                    if (page2 = C1)
                              move      C0,page2
                          call    PrintHeader
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
        prtpage prfile;*pcolumn7:row,str25;
        add     eightlpi,row
          return

PrintTotal
          call      PrintHeader
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
          move      TotRec,str9
          call      FormatNumeric using str9,str15
          rep       zfill,str15
        prtpage prfile;*pcolumn:row,*font=font2,*boldon,"Total Records Read:";
        prtpage prfile;*pcolumn2:row,str15;
        add     eightlpi,row
          move      C0,N11
          calc      N11=SalesLCR(2)+LMPend(2)
        move    N11,str11
          call      FormatNumeric using str11,str15
        prtpage prfile;*pcolumn:row,"Total of LCR/Pending Records:";
        prtpage prfile;*pcolumn2:row,str15;
        clear   str7
        move    C0,F44
        div     TotRec,N11,F44
        mult    "100",F44
        move    F44,str7
          pack      str8,str7,"%"
        prtpage prfile;*pcolumn6:row,str8," of Total Records";
        add     eightlpi,row
          sub       N11,TotRec,N11
        move    N11,str11
          call      FormatNumeric using str11,str15
        prtpage prfile;*pcolumn:row,"Total of Records turned into Live Orders:";
        prtpage prfile;*pcolumn2:row,str15;
        clear   str7
        move    C0,F44
        div     TotRec,N11,F44
        mult    "100",F44
        move    F44,str7
          pack      str8,str7,"%"
        prtpage prfile;*pcolumn6:row,str8," of Total Records";
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,*font=font2,*boldoff,"LCRs";
        move    SalesLCR(2),str9
          call      FormatNumeric using str9,str15
          rep       zfill,str15
        prtpage prfile;*pcolumn2:row,str15;
        clear   str7
        move    C0,F44
        div     TotRec,SalesLCR(2),F44
        mult    "100",F44
        move    F44,str7
          pack      str8,str7,"%"
        prtpage prfile;*pcolumn6:row,str8," of Total Records";
        add     eightlpi,row
        add     eightlpi,row
        move    SalesLCR(3),str9
        prtpage prfile;*pcolumn1:row,"Status of 'LCR':";
          call      PrintAmount using str9,str15
        move    SalesLCR(4),str9
        prtpage prfile;*pcolumn1:row,"Status of '1st Request':";
          call      PrintAmount using str9,str15
        move    SalesLCR(5),str9
        prtpage prfile;*pcolumn1:row,"Status of '2nd Request':";
          call      PrintAmount using str9,str15
        move    SalesLCR(6),str9
        prtpage prfile;*pcolumn1:row,"Status of 'Revised Request':";
          call      PrintAmount using str9,str15
        move    SalesLCR(9),str9
        prtpage prfile;*pcolumn1:row,"Status of 'Pending':";
          call      PrintAmount using str9,str15
.START PATCH 1.23 REPLACED LOGIC
.        move    SalesLCR(11),str9
        move    SalesLCR(11),str9
        prtpage prfile;*pcolumn1:row,"Status of 'Pending Internal':";
          call      PrintAmount using str9,str15
        move    SalesLCR(12),str9
        prtpage prfile;*pcolumn1:row,"Status of 'Tentative Approval':";
          call      PrintAmount using str9,str15
        move    SalesLCR(13),str9
.END PATCH 1.23 REPLACED LOGIC
        prtpage prfile;*pcolumn1:row,"No Sub-Status!! :";
          call      PrintAmount using str9,str15
          move      C0,N11
.START PATCH 1.23 REPLACED LOGIC
.          calc      N11=SalesLCR(3)+SalesLCR(4)+SalesLCR(5)+SalesLCR(6)+SalesLCR(9)+SalesLCR(11)
          calc      N11=SalesLCR(3)+SalesLCR(4)+SalesLCR(5)+SalesLCR(6)+SalesLCR(9)+SalesLCR(11)+SalesLCR(12)+SalesLCR(13)
.END PATCH 1.23 REPLACED LOGIC
        move    N11,str11
        add     eightlpi,row
        prtpage prfile;*pcolumn2:row,"TOTAL:";
        clear   str7
        move    C0,F44
        div     SalesLCR(2),N11,F44
        mult    "100",F44
        move    F44,str7
          pack      str8,str7,"%"
        prtpage prfile;*pcolumn6:row,str8," of Total LCRs";
          call      PrintAmount using str11,str15
        add     eightlpi,row
.
        move    SalesLCR(8),str9
        prtpage prfile;*pcolumn1:row,"Status of 'Cancelled':";
          call      PrintAmount using str9,str15
        move    SalesLCR(10),str9
        prtpage prfile;*pcolumn1:row,"Status of 'Denied':";
          call      PrintAmount using str9,str15
          move      C0,N11
          calc      N11=SalesLCR(8)+SalesLCR(10)
        move    N11,str11
        add     eightlpi,row
        prtpage prfile;*pcolumn2:row,"TOTAL:";
        clear   str7
        move    C0,F44
        div     SalesLCR(2),N11,F44
        mult    "100",F44
        move    F44,str7
          pack      str8,str7,"%"
        prtpage prfile;*pcolumn6:row,str8," of Total LCRs";
          call      PrintAmount using str11,str15
        add     eightlpi,row
        add     eightlpi,row
.
        prtpage prfile;*pcolumn:row,*font=font2,*boldoff,"Pending Orders";
        move    LMPend(2),str9
          call      FormatNumeric using str9,str15
          rep       zfill,str15
        prtpage prfile;*pcolumn2:row,str15;
        clear   str7
        move    C0,F44
        div     TotRec,LMPend(2),F44
        mult    "100",F44
        move    F44,str7
          pack      str8,str7,"%"
        prtpage prfile;*pcolumn6:row,str8," of Total Records";
        add     eightlpi,row
        add     eightlpi,row
        move    LMPend(3),str9
        prtpage prfile;*pcolumn1:row,"Status of 'Pending List Owner Approval':";
          call      PrintAmount using str9,str15
        move    LMPend(4),str9
        prtpage prfile;*pcolumn1:row,"Status of 'Waiting for Guarantee':";
          call      PrintAmount using str9,str15
        move    LMPend(5),str9
        prtpage prfile;*pcolumn1:row,"Status of 'Waiting for Broker Call Back':";
          call      PrintAmount using str9,str15
        move    LMPend(6),str9
        prtpage prfile;*pcolumn1:row,"Status of 'Waiting for Pre-Payment':";
          call      PrintAmount using str9,str15
        move    LMPend(7),str9
        prtpage prfile;*pcolumn1:row,"Status of 'Waiting for Revision':";
          call      PrintAmount using str9,str15
        move    LMPend(8),str9
        prtpage prfile;*pcolumn1:row,"Status of 'At Service Bureau':";
          call      PrintAmount using str9,str15
.        move    LMPend(11),str9
.        prtpage prfile;*pcolumn2:row,Status of '"Approved':";
.         call      PrintAmount using str9,str15
        move    LMPend(12),str9
        prtpage prfile;*pcolumn1:row,"Status of 'Order Received':";
          call      PrintAmount using str9,str15
        move    LMPend(13),str9
        prtpage prfile;*pcolumn1:row,"Status of 'Accounting Hold':";
          call      PrintAmount using str9,str15
        move    LMPend(14),str9
        prtpage prfile;*pcolumn1:row,"Status of '2nd Request':";
          call      PrintAmount using str9,str15
        move    LMPend(15),str9
        prtpage prfile;*pcolumn1:row,"Status of 'Revised Request':";
          call      PrintAmount using str9,str15
.begin patch 1.24
        move    LMPend(16),str9
        prtpage prfile;*pcolumn1:row,"Waiting for Price Approval!! :";
          call      PrintAmount using str9,str15
        move    LMPend(17),str9
        prtpage prfile;*pcolumn1:row,"No Sub-Status!! :";
          call      PrintAmount using str9,str15
          move      C0,N11
          calc      N11=LMPend(3)+LMPend(4)+LMPend(5)+LMPend(6)+LMPend(7)+LMPend(8)+LMPend(12)+LMPend(13)+LMPend(14)+LMPend(15)+LMPend(16)+LMPend(17)

.        move    LMPend(16),str9
.        prtpage prfile;*pcolumn1:row,"No Sub-Status!! :";
.          call      PrintAmount using str9,str15
.          move      C0,N11
.          calc      N11=LMPend(3)+LMPend(4)+LMPend(5)+LMPend(6)+LMPend(7)+LMPend(8)+LMPend(12)+LMPend(13)+LMPend(14)+LMPend(15)+LMPend(16)
.end patch 1.24
        move    N11,str11
        add     eightlpi,row
        prtpage prfile;*pcolumn2:row,"TOTAL:";
        clear   str7
        move    C0,F44
        div     LMPend(2),N11,F44
        mult    "100",F44
        move    F44,str7
          pack      str8,str7,"%"
        prtpage prfile;*pcolumn6:row,str8," of Total Pending Records";
          call      PrintAmount using str11,str15
        add     eightlpi,row
.
        move    LMPend(9),str9
        prtpage prfile;*pcolumn1:row,"Status of 'Denied':";
          call      PrintAmount using str9,str15
        move    LMPend(10),str9
        prtpage prfile;*pcolumn1:row,"Status of 'Cancelled':";
          call      PrintAmount using str9,str15
          move      C0,N11
          calc      N11=LMPend(9)+LMPend(10)
        move    N11,str11
        add     eightlpi,row
        prtpage prfile;*pcolumn2:row,"TOTAL:";
        clear   str7
        move    C0,F44
        div     LMPend(2),N11,F44
        mult    "100",F44
        move    F44,str7
          pack      str8,str7,"%"
        prtpage prfile;*pcolumn6:row,str8," of Total Pending Records";
          call      PrintAmount using str11,str15
        add     eightlpi,row
          return

PrintAmount LRoutine DimPtr,DimPtr1
          call      FormatNumeric using DimPtr,DimPtr1
          rep       zfill,DimPtr1
.         if (DimPtr1 = "")
.                   move      "0",DimPtr1
.         endif
        prtpage prfile;*pcolumn4:row,DimPtr1;
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

LCRLoadInfo
        add     C1,SalesLCR(1)
        if (OSTAT = "l")
                    if (RptFlag = C2)
                              pack      MKEY,OMLRNUM,"000"
                              call      NMLRKEY
                              write   tempfile2,SEQ;"1",NORD5STAT,OMLRNUM,MCOMP,OLNUM,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
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
                elseif (NORD5STAT = "04")       .Approved - Only used for Sales and List Management Reports
                        add     C1,SalesLCR(7)
                elseif (NORD5STAT = "05")       .Cancelled
                        add     C1,SalesLCR(8)
                elseif (NORD5STAT = "06")       .Pending
                        add     C1,SalesLCR(9)
                elseif (NORD5STAT = "07")       .Denied
                        add     C1,SalesLCR(10)
.START PATCH 1.23 REPLACED LOGIC
.                    else
.                        add     C1,SalesLCR(11)   .No Sub-Status!
                elseif (NORD5STAT = "08")       .Pending Internal
                        add     C1,SalesLCR(11)
                elseif (NORD5STAT = "09")       .Tentative Approval
                        add     C1,SalesLCR(12)
                    else
                        add     C1,SalesLCR(13)   .No Sub-Status!
.END PATCH 1.23 REPLACED LOGIC
                endif
        elseif (OSTAT = "z")  .Cancelled LCR
                if (RptFlag = C2)
                              pack      MKEY,OMLRNUM,"000"
                              call      NMLRKEY
                        write   tempfile2,SEQ;"205",OMLRNUM,MCOMP,OLNUM,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                endif
                add     C1,SalesLCR(2)
.                if (NORD5STAT = "05")       .Cancelled
                add     C1,SalesLCR(8)
.                   elseif (NORD5STAT = "06")       .Pending
.                             add     C1,LMLCR(9)
.                   endif
        elseif (OSTAT = "p")
                if (RptFlag = C2)
                              pack      MKEY,OMLRNUM,"000"
                              call      NMLRKEY
                        write   tempfile2,SEQ;"1",NORD4STAT,OMLRNUM,MCOMP,OLNUM,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
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
.begin patch 1.24
.note discovered these where not ever added 
.          insertitem Nord001AComboPending,15,"Waiting for List Usage Agreement"
.          insertitem Nord001AComboPending,16,"Waiting for Sample"
.          insertitem Nord001AComboPending,17,"Waiting for Counts"
.end note discovered these where not ever added also missing from nord0001.plf
                elseif (NORD4STAT = "13")       .Waiting for List Usage Agreement
                        add     C1,LMPend(13)
                elseif (NORD4STAT = "14")       .Waiting for Sample
                        add     C1,LMPend(14)
                elseif (NORD4STAT = "15")       .Waiting for counts
                        add     C1,LMPend(15)

                elseif (NORD4STAT = "16")       .Waiting for $ approval
                        add     C1,LMPend(16)
                    else
                        add     C1,LMPend(17)     .No Sub-Status!
.                    else
.                        add     C1,LMPend(16)     .No Sub-Status!
.end patch 1.24
                endif
        elseif (OSTAT = "x")                    .Cancelled/Denied
                add     C1,LMPend(2)
                if (OHIST = "p")                .Cancelled
                        if (RptFlag = C2)
                                        pack      MKEY,OMLRNUM,"000"
                                        call      NMLRKEY
                                write   tempfile2,SEQ;"207",OMLRNUM,MCOMP,OLNUM,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                        endif
                        add     C1,LMPend(10)
                elseif (OHIST = "z")            .Denied
                        if (RptFlag = C2)
                                        pack      MKEY,OMLRNUM,"000"
                                        call      NMLRKEY
                                write   tempfile2,SEQ;"206",OMLRNUM,MCOMP,OLNUM,OMDTEC,OMDTEY,OMDTEM,OMDTED,OLRN,OSTAT
                        endif
                        add     C1,LMPend(9)
                endif
        endif
          return

          include   nordio.inc
          include   nord5io.inc
          include   nord4io.inc
          include   npndio.inc
          include   ncntio.inc
.Patch1.1
          include   compio.inc
          include   cntio.inc
.         include   nmlrio.inc
.Patch1.1
          include   comlogic.inc
