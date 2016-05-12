          include common.inc
          include cons.inc
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
          include nxngdd.inc
          include   nxchdd.inc

release   init    "1.2"       DLH       Sunbelt PDF
Reldate   INit      "2013 April 24"
.release   init    "1.1"       DLH       PLI
.Reldate   INit      "23 May 2007"
.need to establish --- how do we know who's logo to use needs to be passed from the web
.release init    "1.0"        ASH       Initial Release
.
prfile    pfile
DimPtr    dim       ^
MailerName dim      55
MailerNum dim       6
RptDate   dim       10
pagenum   form      1
.Define Fonts to be used
font1     font
font2     font
font3     font
page      form      9
TotalOwes form      12
TotalOwed form      12
.
EXchangeMlr                   dim       55
ExchangeOwes                  dim       15
ExchangeIsOwed                dim       15
ExchangeIsEven                dim       1
mlr1      dim       6
mlr2      dim       6
#fileManagerSettings          dim       260
.
.Testing
          shutdown

PrintWebExchange Routine DimPtr
.DimPtr  = Timestamp + Client ID Number
.
          unpack    DimPtr,timestamp,COMPFLD
          call      Trim using COMPFLD
          if (COMPFLD = "")
                    clear     DimPtr
                    return
          else
                    move      C1,COMPPATH
                    call      COMPKEY
                    if over
                              clear     DimPtr
                              return
                    else
                              pack      taskname,DimPtr,".dat"
                              prep      tempfile,taskname,exclusive
.
                              move      COMPCOMP,MailerName
                              move      COMPNUM,MailerNum
.
                              clear     NXNGFLD2
                              packkey   NXNGFLD1,"01X",MailerNum
                              call      NXNGAIM
                              loop
                                        until over
                                        if (flag != "I")
                                                  move      ENTRY,str5
                                                  rep       zfill,str5
                                                  packkey   NXCHFLD1,ACCKEY,str5
                                                  call      NXCHKEY
                                                  if over
                                                            clear     DimPtr
                                                            return
                                                  endif
                                                  sub       USAGE1,USAGE2,N10
.Initialize all the vars first off
                                                  clear     ExchangeOwes
                                                  clear     ExchangeIsOwed
                                                  clear     ExchangeIsEven
                                                  if (N10 < 0)
                                                            move      "Z,ZZZ,ZZZ,ZZ9",ExchangeOwes
                                                            edit      N10,ExchangeOwes
                                                  elseif (N10 > 0)
                                                            move      "Z,ZZZ,ZZZ,ZZ9",ExchangeIsOwed
                                                            edit      N10,ExchangeIsOwed
                                                  else
                                                            move      "X",ExchangeIsEven
                                                  endif
                                                  unpack    ACCKEY,mlr1,mlr2
                                                  packkey   COMPFLD,mlr2
                                                  call      COMPKEY
                                                  write     tempfile,SEQ;COMPCOMP,ExchangeOwes,ExchangeIsOwed,ExchangeIsEven
                                        endif
                                        call      NXNGKG
                              repeat
                              clear     NXNGFLD1
                              packkey   NXNGFLD2,"02X",MailerNum
                              call      NXNGAIM
                              loop
                                        until over
                                        if (flag != "I")
                                                  move      ENTRY,str5
                                                  rep       zfill,str5
                                                  packkey   NXCHFLD1,ACCKEY,str5
                                                  call      NXCHKEY
                                                  if over
                                                            clear     DimPtr
                                                            return
                                                  endif
                                                  sub       USAGE2,USAGE1,N10
.Initialize all the vars first off
                                                  clear     ExchangeOwes
                                                  clear     ExchangeIsOwed
                                                  clear     ExchangeIsEven
                                                  if (N10 < 0)
                                                            move      "Z,ZZZ,ZZZ,ZZ9",ExchangeOwes
                                                            edit      N10,ExchangeOwes
                                                  elseif (N10 > 0)
                                                            move      "Z,ZZZ,ZZZ,ZZ9",ExchangeIsOwed
                                                            edit      N10,ExchangeIsOwed
                                                  else
                                                            move      "X",ExchangeIsEven
                                                  endif
                                                  unpack    ACCKEY,mlr1,mlr2
                                                  packkey   COMPFLD,mlr1
                                                  call      COMPKEY
                                                  write     tempfile,SEQ;COMPCOMP,ExchangeOwes,ExchangeIsOwed,ExchangeIsEven
                                        endif
                                        call NXNGKG
                              repeat
                    endif
          endif
          close     tempFile
          pack      taskname,DimPtr,".dat,",DimPtr,".srt -U,1-55"
          sort      taskname
          if over
                    clear     DimPtr
                    return
          endif
          pack      taskname,DimPtr,".srt"

          trap      noDataSource if io
                    getmode   *openuseip=#fileManagerSettings
                    setmode   *openuseip=""
                    open      tempfile,taskname,exclusive
                    setmode   *openuseip=#fileManagerSettings
          trapclr   io
.
.begin patch 1.2
.          pack      taskname,DimPtr
.          PRTOPEN   prfile,"PDF995",taskname
          pack      taskname from "c:\work\pdf\",DimPtr,".pdf"
          PRTOPEN   prfile,"PDF:",taskname
.end patch 1.2
.          PRTOPEN   prfile,"HP6300",b1
.          PRTOPEN   prfile,"FAXFILE",taskname
.
.Initialize variables
          clock     timestamp,timestamp
          unpack    timestamp,CC,YY,MM,DD
          pack      RptDate,MM,SLASH,DD,SLASH,CC,YY
          create    font1,"Arial",size=14,bold
          create    font2,"Arial",size=11
          create    font3,"Arial",size=10
        create  font10,"Times New Roman",size=24,bold
        create  font11,"Times New Roman",size=24,italic
        create  font12,"Times New Roman",size=12
        create  font13,"Arial",size=9
NINLogo   PICT
.         CREATE    NINLogo=3:13:30:50:
.                   "\\nins1\e\netutils\NIN logo black outline.jpg"
          CREATE    NINLogo=3:13:30:50:
                    "..\images\NIN logo black outline.jpg"
          move      "250",column
          move      "1000",column1
          move      "4200",column2
          move      "5200",column3
          move      "6200",column4
.
          move      C0,page
.
          call      ExchangePrintHeader
          loop
                    read tempfile,seq;COMPCOMP,ExchangeOwes,ExchangeIsOwed,ExchangeIsEven
                    until over
                    call      ExchangePrintDetail
                    call      RemoveChar using ExchangeOwes,COMMA
                    call      Trim using ExchangeOwes
                    move      C0,N10
                    move      ExchangeOwes,N10
                    add       N10,TotalOwes
                    call      RemoveChar using ExchangeIsOwed,COMMA
                    call      Trim using ExchangeIsOwed
                    move      C0,N10
                    move      ExchangeIsOwed,N10
                    add       N10,TotalOwed
          repeat
.Total Line
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
        prtpage prfile;*pcolumn:row,*font=font3,*boldoff,"TOTAL NAMES:";
        move        TotalOwes,str12
        call        FormatNumeric using str12,str15
        prtpage prfile;*pcolumn2:row,str15;
        move        TotalOwed,str12
        call        FormatNumeric using str12,str15
        prtpage prfile;*pcolumn3:row,str15;
.Clean Up
          close     tempfile
          pack      taskname,DimPtr,".dat"
          trap      noDataSource2 if io
                    getmode   *openuseip=#fileManagerSettings
                    setmode   *openuseip=""
                    erase     taskname
                    pack      taskname,DimPtr,".srt"
                    erase     taskname
                    setmode   *openuseip=#fileManagerSettings
          trapclr   io

          destroy   font1
          destroy   font2
          destroy   font3
        destroy  font10
        destroy  font11
        destroy  font12
        destroy  font13
.Testing
          prtclose prfile
          pause     "10"
        return

ExchangePrintHeader
.Starting point for first row set here - if this changes border value needs to change!!!!!!!
          add       C1,page
          prtpage   prfile;*UNITS=*HIENGLISH;
          move      "300",row
          prtpage   prfile;*p7000:50,*font=font2,*uloff,"page ",page;
          prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          add       "60",row
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          prtpage   prfile;*p3000:row,*font=font1,"CONFIDENTIAL";
          add       eightlpi,row
          add       eightlpi,row
          prtpage   prfile;*pcolumn:row,*font=font2,"NIN EXCHANGE STATUS REPORT";
          add       twelvelpi,row
          add       twelvelpi,row
          prtpage   prfile;*pcolumn:row,"Date:";
          prtpage   prfile;*pcolumn1:row,RptDate;
          add       twelvelpi,row
          add       twelvelpi,row
          prtpage   prfile;*pcolumn:row,"Client:";
          prtpage   prfile;*pcolumn1:row,MailerName;
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          prtpage   prfile;*pcolumn:row,"Has an exchange history with the organizations listed below.";
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          prtpage   prfile;*pcolumn2:row,MailerName;
          add       twelvelpi,row
          add       twelvelpi,row
          prtpage   prfile;*pcolumn2:row,"Owes";
          prtpage   prfile;*pcolumn3:row,"Is Owed";
          prtpage   prfile;*pcolumn4:row,"Is Even";
          add       sixlpi,row
          prtpage prfile;*pcolumn2:row,*pensize=20,*line=7300:row;
          add       eightlpi,row
          add       eightlpi,row
        return

ExchangePrintDetail
          if (row >= 10220)
                    prtpage prfile;*NEWPAGE;
                    call    ExchangePrintHeader
        endif
        prtpage prfile;*pcolumn:row,*font=font3,*boldoff,COMPCOMP;
        prtpage prfile;*pcolumn2:row,ExchangeOwes;
        prtpage prfile;*pcolumn3:row,ExchangeIsOwed;
        pack        str15,"        ",ExchangeIsEven
        prtpage prfile;*pcolumn4:row,str15;
          add       eightlpi,row
          add       eightlpi,row
          return

noDataSource
          clear     DimPtr
          noreturn
          return

noDataSource2
          //do nothing - only problem is that the files were not immediately deleted.
          return

          INCLUDE   COMPIO.inc
          INCLUDE   CNTIO.inc
        include nxngio.inc
          include   nxchio.inc
        include comlogic.inc