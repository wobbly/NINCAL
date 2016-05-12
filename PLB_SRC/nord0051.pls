.....................................
.NOrd0051
.
.Check for Live but Un-approved brokerage orders
.
.
.....................................
PC        Equ       0
          include   common.inc
          include   cons.inc
          include   norddd.inc
          include   ncntdd.inc
          include   compdd.inc
          include   cntdd.inc
          include   ndatdd.inc
release   init                "1.00"        DLH
reldate   Init      "04 Aug 2011"

output    file                .used for testing purposes
prfile    pfile
ORDPRINT  file      
page      form      9
HoldOSTAT dim       1
.Define Fonts to be used
font1   font
font2   font
font3     font
LMTotal   form      9
SalesTotal form     9
LMTotal2 form       9
SalesTotal2 form 9
Counter   form      9
CntTotal form       15
.Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=10
        create  font3,"Arial",size=8
.Set up columns
          move    "1",column
          move    "650",column1
          move    "4350",column2
        move    "6850",column3
        move    "7700",column4
        move    "8700",column5
.
          call      GetWinVer
.
          move      C1,NORDPATH
          move      C1,NCNTPATH
          move      C1,NMLRPATH
          move      C1,NDATPATH
.Set time from which we will start cancelling 
          clock     timestamp,timestamp
          unpack    timestamp,CC,YY,MM,DD
          move      C0,N5
          move      C0,JULDAYS
          call      CVTJUL
          sub       "183",JULDAYS,N5    .6 Months ago from today
          sub       "365",JULDAYS,N6    .12 Months ago from today - Calculated using Order Date
          call      Paint
          pack      str10,MM,SLASH,DD,SLASH,CC,YY
.
.Prepare files
          open      ORDPRINT,"\\nins1\e\data\NINPRINT.dat"
          move      "c:\work\Pndoutput.dat",str35
          erase     str35
          prepare   output,str35
          display   *P10:10,*EL,"Mail Date: ",str10
loopy
          loop
          read      Ordprint,seq;ORDVARS
                    until over
                    if        (ORCODE = "F")                            .not approved
                    add       C1,howmany
                    display   *P10:12,*EL,"Current Record:    ",OLRN
                    display   *P10:13,*EL,"Record Count:   ",howmany
                    display   *P10:14,*EL,"PND Count:     ",N10
                    pack      str2,OSALES10,OSALES
                    if (str2 = "06" | str2 = "19" | str2 = "27" | str2 = "28")
                              add       C1,LMTotal
                    else
                              add       C1,SalesTotal
                    endif
.......................................................
                    pack      str8,OMDTEC,OMDTEY,OMDTEM,OMDTED
                    call      Trim using str8
                    if (str8 = "" | str8 = "00000000" | str8 = "11111111")
                              move      OODTEC,CC
                              move      OODTEY,YY
                              move      OODTEM,MM
                              move      OODTED,DD
                              move      N6,N7
                    else
                              move      OMDTEC,CC
                              move      OMDTEY,YY
                              move      OMDTEM,MM
                              move      OMDTED,DD
                              move      N5,N7
                    endif
                              move      C0,JULDAYS
                              call      CVTJUL
                                        if (str2 = "06" | str2 = "19" | str2 = "27" | str2 = "28")
                                                  add       C1,LMTotal2
                                        else
                                                  add       C1,SalesTotal2
                                        endif
...Create File used for Reports...
                                                  pack      NCNTFLD,OCOCODE
                                                  move      "NCNTKEY",Location
                                                  pack      KeyLocation,"Key: ",NCNTFLD
                                                  call      NCNTKEY
                                                  if over
                                                            clear     CNTNAME
                                                  endif
                                                  pack      str10,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
.
                                                  pack      MKEY,OMLRNUM,"000"
                                                  move      "NMLRKEY",Location
                                                  pack      KeyLocation,"Key: ",MKEY
                                                  call      NMLRKEY
                                                  if over
                                                            clear     MCOMP
                                                  endif
                                                  pack      NBRKFLD,OBRKNUM,OBRKCNT
                                                  rep       zfill,NBRKFLD
                                                  if (NBRKFLD = "0000000")
                                                            clear     BRCOMP
                                                  else
                                                            move      "NBRKKEY",Location
                                                            pack      KeyLocation,"Key: ",NBRKFLD
                                                            call      NBRKKEY
                                                            if over
                                                                      clear     BRCOMP
                                                            endif
                                                  endif
.
                                                  pack      NDATFLD,OLNUM
                                                  move      "NDATKEY",Location
                                                  pack      KeyLocation,"Key: ",NDATFLD
                                                  call      NDATKEY
                                                  if over
                                                            clear     OLSTNAME
                                                  endif
.
                                                  pack      str2,OSALES10,OSALES
                                                  write     output,SEQ;OLRN,OSTAT,str10,CNTNAME,MCOMP,OLSTNAME,BRCOMP,OQTY,str2
                                                  add       C1,N10
          endif
          repeat

          display   *P10:16,*EL,"LM Count:       ",LMTotal
          display   *P10:17,*EL,"Sales Count:    ",SalesTotal
          display   *P10:18,*EL,"LM Unapproved:        ",LMTotal2
          display   *P10:19,*EL,"Sales Unapproved:     ",SalesTotal2
break1
          display   *P10:10,*EL,"Sorting File"
          close     output
          if        (N10 > c0)
          else
          goto      EOJ
          endif
..............Check for ZERO records!!!!!!!!!!!!!!!!!!!!!!!!!!          
          pack      taskname,"c:\work\PNDoutput.dat,c:\work\PNDoutput.srt;18-62,63-107"
          sort      taskname
          open      output,"c:\work\Pndoutput.srt"
.Create Reports for Contacts
          display   *P10:10,*EL,"Creating Reports"
          clear     str45
          loop
                    read      output,SEQ;OLRN,OSTAT,str10,CNTNAME,MCOMP,OLSTNAME,BRCOMP,OQTY
                    until over
                    if (str45 <> CNTNAME)
BREAK
                              if (str45 <> "")    .Not the first record
                                        if (row >= 7450)
                                                  prtpage prfile;*NEWPAGE;
                                                  add       C1,page
                                                  call      PrintHeader
                                        endif
                                        move      Counter,str9
                                        call      FormatNumeric using str9,str11
                                        add       eightlpi,row
                                        prtpage   prfile;*pcolumn:row,*font=font3,"Total Unapproved Ords for this Contact:  ",str11;
                                        move      CntTotal,str15
                                        call      FormatNumeric using str15,str25
                                        prtpage   prfile;*pcolumn4:row,*ulon,str25,*uloff;
                              endif
                              prtclose prfile
                              move      C0,Counter
                              move      C0,CntTotal
                              if (osflag = c1 | osflag = C5 | osflag = c6)         .nt 2k xp
                                        PRTOPEN   prfile,"\\NINs2\Laser8",prtname
                              elseif (osflag = c3 | osflag =c4)         .win 95 98
                                        PRTOPEN   prfile,"Laser8",prtname
                              else   .(osflag = c0)         .Don't know prompt for printer
                                        PRTOPEN   prfile,"-",prtname
                              endif
                              move      C1,page
                              call      PrintHeader
                              move      CNTNAME,str45
                              move      MCOMP,str55
                    endif
                    if (MCOMP <> str55)
                              add       eightlpi,row
                              move      MCOMP,str55
                    endif
                    if (row >= 7450)
                              prtpage prfile;*NEWPAGE;
                              add       C1,page
                              call      PrintHeader
                    endif
                    prtpage   prfile;*pcolumn:row,*font=font3,OLRN;
                    call      Trim using MCOMP
                    call      Trim using BRCOMP
                    if (BRCOMP <> "")
                              pack      taskname,MCOMP,SLASH,BRCOMP
                              prtpage   prfile;*pcolumn1:row,taskname;
                    else
                              prtpage   prfile;*pcolumn1:row,MCOMP;
                    endif
                    prtpage   prfile;*pcolumn2:row,OLSTNAME;
                    prtpage   prfile;*pcolumn3:row,str10;
                    move      C0,N9
                    move      OQTY,N9
                    add       N9,CntTotal
                    call      FormatNumeric using OQTY,str11
                    prtpage   prfile;*pcolumn4:row,str11;
                    add       C1,Counter
                    add       eightlpi,row
          repeat
.Take care of Last Record
          add       eightlpi,row
          if (row >= 7450)
                    prtpage prfile;*NEWPAGE;
                    add       C1,page
                    call      PrintHeader
          endif
          move      Counter,str9
          call      FormatNumeric using str9,str11
          add       eightlpi,row
          prtpage   prfile;*pcolumn:row,*font=font3,"Total Unapproved Ords for this Contact:  ",str11;
          move      CntTotal,str15
          call      FormatNumeric using str15,str25
          prtpage   prfile;*pcolumn4:row,*ulon,str25,*uloff;
.Total Page
          prtpage prfile;*NEWPAGE;
          add       C1,page
        prtpage prfile;*UNITS=*HIENGLISH,*ORIENT=*LANDSCAPE;
          move      "300",row
          prtpage   prfile;*pcolumn:row,*font=font1,*boldon,"Monthly Unapproved Orders Report - 'Mail Dates over 6 Months old'";
          add       sixlpi,row
          add       sixlpi,row
          prtpage   prfile;*pcolumn:row,"Total Page";
        prtpage prfile;*p4750:7800,*font=font2,page;
          add       sixlpi,row
          add       sixlpi,row
          add       sixlpi,row
          clock     timestamp,timestamp
          prtpage   prfile;*pcolumn:row,"Total:       ",n10;
          add       eightlpi,row
          prtpage   prfile;*pcolumn:row,"LM Total:     ",LMTotal;
          add       eightlpi,row
          prtpage   prfile;*pcolumn:row,"Sales Total:  ",SalesTotal;
          add       eightlpi,row
          prtpage   prfile;*pcolumn:row,"Old LM Unapproved :   ",LMTotal2;
          add       eightlpi,row
          prtpage   prfile;*pcolumn:row,"Old Sales Unapproved: ",SalesTotal2;
          prtclose prfile
.......................................
break2
          display   *P10:10,*EL,"Sorting List Management File"
          close     output
          pack      str25,"232='0'&233='6'"
          pack      taskname,"c:\work\Pndoutput.dat,c:\work\Pndoutput2.srt;S=",str25,",98-132"
          sort      taskname
          open      output,"c:\work\Pndoutput2.srt"
.
          move    "3150",column2
.Create Report for List Management
          display   *P10:10,*EL,"Creating List Management Reports"
          clear     str35
          move      C0,Counter
          loop
                    read      output,SEQ;OLRN,OSTAT,str10,CNTNAME,MCOMP,OLSTNAME,BRCOMP,OQTY,str2
                    until over
                    if (Counter = C0)   .The first record
                              if (osflag = c1 | osflag = C5 | osflag = c6)         .nt 2k xp
                                        PRTOPEN   prfile,"\\NINs2\Laser8",prtname
                              elseif (osflag = c3 | osflag =c4)         .win 95 98
                                        PRTOPEN   prfile,"Laser8",prtname
                              else   .(osflag = c0)         .Don't know prompt for printer
                                        PRTOPEN   prfile,"-",prtname
                              endif
                              move      C1,page
                              call      PrintHeader2
                    endif
                    if (row >= 7450)
                              prtpage prfile;*NEWPAGE;
                              add       C1,page
                              call      PrintHeader2
                    endif
                    if (OLSTNAME <> str35)
                              if (Counter <> C0)  .Not the first record
                                        move      CntTotal,str15
                                        call      FormatNumeric using str15,str25
                                        prtpage   prfile;*pcolumn4:row,*ulon,str25,*uloff;
                                        add       eightlpi,row
                                        add       eightlpi,row
                              endif
                              move      OLSTNAME,str35
                              move      C0,CntTotal
                    endif
                    if (row >= 7450)
                              prtpage prfile;*NEWPAGE;
                              add       C1,page
                              call      PrintHeader2
                    endif
                    prtpage   prfile;*pcolumn:row,*font=font3,OLRN;
                    prtpage   prfile;*pcolumn1:row,OLSTNAME;
                    call      Trim using MCOMP
                    call      Trim using BRCOMP
                    if (BRCOMP <> "")
                              pack      taskname,MCOMP,SLASH,BRCOMP
                              prtpage   prfile;*pcolumn2:row,taskname;
                    else
                              prtpage   prfile;*pcolumn2:row,MCOMP;
                    endif
                    prtpage   prfile;*pcolumn3:row,str10;
.Adding to CntTotal has to happen here as FormatNumeric will destroy value of OQTY
                    move      C0,N9
                    move      OQTY,N9
                    add       N9,CntTotal
                    call      FormatNumeric using OQTY,str11
                    prtpage   prfile;*pcolumn4:row,str11;
                    add       C1,Counter
                    add       eightlpi,row
          repeat
.Take care of Last Record
          add       eightlpi,row
          if (row >= 7450)
                    prtpage prfile;*NEWPAGE;
                    add       C1,page
                    call      PrintHeader2
          endif
          move      Counter,str9
          call      FormatNumeric using str9,str11
          add       eightlpi,row
          prtpage   prfile;*pcolumn:row,*font=font3,"Total Unapproved Orders for List Management:  ",str11;
          move      CntTotal,str15
          call      FormatNumeric using str15,str25
          prtpage   prfile;*pcolumn4:row,*ulon,str25,*uloff;
          prtclose prfile
.......................................
EOJ          shutdown

PrintHeader
.Header Information
        prtpage prfile;*UNITS=*HIENGLISH,*ORIENT=*LANDSCAPE;
          move      "300",row
          prtpage   prfile;*pcolumn:row,*font=font1,*boldon,"Daily Unapproved Orders Report ";
          add       sixlpi,row
          add       sixlpi,row
          prtpage   prfile;*pcolumn:row,CNTNAME,*boldoff;
        prtpage prfile;*p4750:7800,*font=font2,page;
          add       sixlpi,row
          add       sixlpi,row
          add       sixlpi,row
          prtpage   prfile;*pcolumn:row,*boldon,*ulon,"LR";
          prtpage   prfile;*pcolumn1:row,"Mailer/Broker";
          prtpage   prfile;*pcolumn2:row,"List";
          prtpage   prfile;*pcolumn3:row,"Mail Date";
          prtpage   prfile;*pcolumn4:row,"Qty";
          prtpage   prfile;*pcolumn5:row,"Old Sub-status",*boldoff,*uloff;
          add       sixlpi,row
          return

PrintHeader2
.Header Information
        prtpage prfile;*UNITS=*HIENGLISH,*ORIENT=*LANDSCAPE;
          move      "300",row
          prtpage   prfile;*pcolumn:row,*font=font1,*boldon,"Monthly Unapproved Report ";
          add       sixlpi,row
          add       sixlpi,row
          prtpage   prfile;*pcolumn:row,"List Management - Sorted by List",*boldoff;
        prtpage prfile;*p4750:7800,*font=font2,page;
          add       sixlpi,row
          add       sixlpi,row
          add       sixlpi,row
          prtpage   prfile;*pcolumn:row,*boldon,*ulon,"LR";
          prtpage   prfile;*pcolumn1:row,"List";
          prtpage   prfile;*pcolumn2:row,"Mailer/Broker";
          prtpage   prfile;*pcolumn3:row,"Mail Date";
          prtpage   prfile;*pcolumn4:row,"Qty";
          prtpage   prfile;*pcolumn5:row,"Old Sub-status",*boldoff,*uloff;
          add       sixlpi,row
          return

          include   nordio.inc
          include   ncntio.inc
          include   compio.inc
          include   cntio.inc
          include   ndatio.inc
          include   comlogic.iNC