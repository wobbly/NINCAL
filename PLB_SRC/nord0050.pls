.....................................
.NOrd0050
.
.Pending ORder PURGE PROGRAM - USED FOR Pending ORders WITH MAIL DATES OVER 6 MONTHS OLD
.
.Running this program will update the following files, which should be backed up ahead of time:
.         NINORD
.         NINORD4
.         NINORD5
.         NINORD6
.         NINSPEC2
.         NINPRINT
.         NINPRINTL
.
.
.FOR TESTING PURPOSES FOLLOW LOGIC AROUND ".DREWTEST" LABELS.  THESE
.WILL DENOTE LINES WHERE FILES ARE UPDATED, AND WHICH SHOULD BE UPDATED FOR TESTING.
.
.....................................
          include   common.inc
          include   cons.inc
          include   norddd.inc
          include   nordpdd.inc
          include   nord4dd.inc
          include   nord5dd.inc
          include   nord6dd.inc
          include   ncntdd.inc
          include   compdd.inc
          include   cntdd.inc
          include   ndatdd.inc
          include   npnddd.inc
          include   nspe2dd.inc
.2014 February reviewed for new pending status
release   Init      "1.01"              DLH . check for zero records
reldate   Init      "2012"
.release   init                "1.00"        DLH
.reldate   Init      "08 June 2010"

output    file                .used for testing purposes
prfile    pfile
ORDPRINT ifile      keylen=6,fixed=696
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
Counter2   form      9                  .second pass
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
          move      C1,NORD5PATH
          move      C1,NORD6PATH
          move      C1,NPNDPATH
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
          display   *P10:10,*EL,"Aamdexing File"
.
.          Goto      printpass
.Prepare files
          clear     taskname
          MOve    "\\nins1\e\data\text\ninord.dat \\nins1\e\data\index\Pndord l408 -2-2",taskname
.          reset     taskname
          AAMdex    Taskname
          open      ORDPRINT,"NINPRINT.isi|10.10.30.103:502"
          move      "c:\work\Pndoutput.dat",str35
          erase     str35
          prepare   output,str35
          display   *P10:10,*EL,"Mail Date: ",str10
          move      "PNDORD.AAM",str45
          trap      IOMssg Giving Error if IO
          open      NORDFLE2,str45
          pack      str4,"01Xp"                            .find pending
          move      "Aamread",Location
          pack      KeyLocation,"AAMKEY"
          read      NORDFLE2,str4;ORDVARS
          loop
                    until over
                    trapclr   IO
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
                              if (JULDAYS <> C0 & JULDAYS < N7)
                                        if (str2 = "06" | str2 = "19" | str2 = "27" | str2 = "28")
                                                  add       C1,LMTotal2
                                        else
                                                  add       C1,SalesTotal2
                                        endif
                                        pack      NORDFLD,OLRN
                                        rep       zfill,NORDFLD
                                        move      "NORDKEY",Location
                                        pack      KeyLocation,"Key: ",NORDFLD
                                        call      NORDKEY
                                        if not over
                                                  move      OSTAT,HoldOSTAT
.
                                                  if (OSTAT = "l")
                                                            move      "z",OSTAT
                                                  elseif (OSTAT = "p")
                                                            move      "x",OSTAT
                                                  endif
                                                  move      "NORDUPD",Location
                                                  pack      KeyLocation,"Key: ",NORDFLD
.drewtest
                                                  call      NORDUPD
.Status Files
                                                  move      "***",NPNDFLD                 .Initialize
                                                  clear     NORD6STA2           .Initialize
                                                  clear     str25                         .Initialize
                                                  if (HoldOSTAT = "l")
. Status file
                                                            move      "PND",str25                   .See Below
                                                            pack      NORD5FLD,NORDFLD
                                                            move      "NORD5KEY",Location
                                                            pack      KeyLocation,"Key: ",NORD5FLD
                                                            call      NORD5KEY
                                                            if not over
.Set Up Key value for Sub-Status Description
                                                                      pack      NPNDFLD,HoldOSTAT,NORD5STAT
                                                                      move      NORD5STAT,NORD6STA2
.
                                                                      move      "NORD5DEL",Location
.drewtest
                                                                      call      NORD5DEL
                                                            endif
                                                  elseif (HoldOSTAT = "p")
.Pending Order Status file
                                                            move      "Pending Order",str25         .See Below
                                                            pack      NORD4FLD,NORDFLD
                                                            move      "NORD4KEY",Location
                                                            pack      KeyLocation,"Key: ",NORD4FLD
                                                            call      NORD4KEY
                                                            if not over
.Set Up Key value for Sub-Status Description
                                                                      pack      NPNDFLD,HoldOSTAT,NORD4STAT
                                                                      move      NORD4STAT,NORD6STA2

                                                                      move      "NORD4DEL",Location
.drewtest
                                                                      call      NORD4DEL
                                                            endif
                                                  endif
.Cancelled Record Status file
                                                  move      HoldOSTAT,NORD6STA1
                                                  move      "01",NORD6STAT
                                                  unpack    timestamp,NORD6CDTE
                                                  pack      NORD6FLD,NORDFLD
                                                  move      "NORD6TST",Location
                                                  pack      KeyLocation,"Key: ",NORD6FLD
                                                  call      NORD6TST
                                                  if over
                                                            move      NORD6FLD,NORD6LR
                                                            unpack    timestamp,NORD6PDTE
                                                            move      "NORD6WRT",Location
.drewtest
                                                            call      NORD6WRT
                                                  else
                                                            move      "NORD6UPD",Location
.drewtest
                                                            call      NORD6UPD
                                                  endif
.NINPRINT File
                                                  trap      IOMssg Giving Error if IO
                                                  move      "readNINPRINT",Location
                                                  pack      KeyLocation,"Key: ",NORDFLD
                                                  filepi    3;ORDPRINT
                                                  read      ORDPRINT,NORDFLD;;
                                                  if not over
                                                            move      "delNINPRINT",Location
.drewtest
                                                            delete    ORDPRINT,NORDFLD
                                                  endif
                                                  trapclr   IO
.NINPRINTL File
                                                  pack      NORDPFLD,NORDFLD
                                                  move      "NORDPTST",Location
                                                  pack      KeyLocation,"Key: ",NORDPFLD
                                                  call      NORDPTST
                                                  if not over
                                                            move      "NORDPDEL",Location
.                                                           call      NORDPDEL
.Delete the ISAM record.  Don't worry about AAM record.  Take care of that after job runs.
.drewtest
                                                            DELETE    NORDPFILE,NORDPFLD
                                                  endif

.Sub-Status Description File
                                                  move      "NPNDKEY",Location
.NPNDFLD set up earlier, see above
                                                  pack      KeyLocation,"Key: ",NPNDFLD
                                                  call      NPNDKEY
.Internal Notes File
                                                  pack      NSPE2FLD,NORDFLD
                                                  move      "NSPE2KEY",Location
                                                  pack      KeyLocation,"Key: ",NSPE2FLD
                                                  call      NSPE2KEY
                                                  if over
                                                            move      NORDFLD,NSPE2LR
                                                            pack      DESC003,"Before Cancellation: Status=",str25,COMMA,B1,"Sub-Status=",NPNDDESC
                                                            move      "NSPE2WRT",Location
.drewtest
                                                            call      NSPE2WRT
                                                  else
                                                            move      "NSPE2DEL",Location
.drewtest
                                                            call      NSPE2DEL
                                                            call      Trim using DESC003
                                                            pack      DESC003,DESC003,B1,"Before Cancellation: Status=",str25,COMMA,B1,"Sub-Status=",NPNDDESC
                                                            move      "NSPE2WRT",Location
.drewtest
                                                            call      NSPE2WRT
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
                                                  pack      NPNDFLD,HoldOSTAT,NORD5STAT
                                                  rep       zfill in NPNDFLD
                                                  move      "NPNDKEY",Location
                                                  pack      KeyLocation,"Key: ",NPNDFLD
                                                  call      NPNDKEY
                                                  if over
                                                            clear     NPNDDESC
                                                  endif
                                                  pack      str2,OSALES10,OSALES
                                                  write     output,SEQ;OLRN,OSTAT,str10,CNTNAME,MCOMP,OLSTNAME,NPNDDESC,BRCOMP,OQTY,str2
                                                  add       C1,N10
                                        endif
                              endif
.                   endif
                    move      "AamKGread",Location
                    pack      KeyLocation,"AAMKEY"
                    trap      IOMssg Giving Error if IO
                    readkg    NORDFLE2;ORDVARS
          repeat
          display   *P10:16,*EL,"LM Count:       ",LMTotal
          display   *P10:17,*EL,"Sales Count:    ",SalesTotal
          display   *P10:18,*EL,"LM Pendings:        ",LMTotal2
          display   *P10:19,*EL,"Sales Pendings:     ",SalesTotal2
break1
          display   *P10:10,*EL,"Sorting File"
          close     output
          if        (N10 > c0)
          else
          goto      EOJ
          endif
PrintPAss
..............Check for ZERO records!!!!!!!!!!!!!!!!!!!!!!!!!!          
          pack      taskname,"c:\work\PNDoutput.dat,c:\work\PNDoutput.srt;18-62,63-107"
          sort      taskname
          open      output,"c:\work\Pndoutput.srt"
.Create Reports for Contacts
          display   *P10:10,*EL,"Creating Reports"
          clear     str45
          loop
                    read      output,SEQ;OLRN,OSTAT,str10,CNTNAME,MCOMP,OLSTNAME,NPNDDESC,BRCOMP,OQTY
                    until over
                    add       c1,counter2
                    display   *P10:20,*EL,"Records printed:     ",counter2
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
                                        prtpage   prfile;*pcolumn:row,*font=font3,"Total Pending Ords for this Contact:  ",str11;
                                        move      CntTotal,str15
                                        call      FormatNumeric using str15,str25
                                        prtpage   prfile;*pcolumn4:row,*ulon,str25,*uloff;
                              endif
.                              prtclose prfile
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
                    prtpage   prfile;*pcolumn5:row,NPNDDESC;
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
          prtpage   prfile;*pcolumn:row,*font=font3,"Total Pending Ords for this Contact:  ",str11;
          move      CntTotal,str15
          call      FormatNumeric using str15,str25
          prtpage   prfile;*pcolumn4:row,*ulon,str25,*uloff;
.Total Page
          prtpage prfile;*NEWPAGE;
          add       C1,page
        prtpage prfile;*UNITS=*HIENGLISH,*ORIENT=*LANDSCAPE;
          move      "300",row
          prtpage   prfile;*pcolumn:row,*font=font1,*boldon,"Monthly Cancelled Pending Orders Report - 'Mail Dates over 6 Months old'";
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
          prtpage   prfile;*pcolumn:row,"Old LM Pendings :   ",LMTotal2;
          add       eightlpi,row
          prtpage   prfile;*pcolumn:row,"Old Sales Pendings: ",SalesTotal2;
.          prtclose prfile
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
                    read      output,SEQ;OLRN,OSTAT,str10,CNTNAME,MCOMP,OLSTNAME,NPNDDESC,BRCOMP,OQTY,str2
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
                    prtpage   prfile;*pcolumn5:row,NPNDDESC;
                    add       C1,Counter
                    add       eightlpi,row
          repeat


.check it out this fails as printer is allread closed????????
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
          prtpage   prfile;*pcolumn:row,*font=font3,"Total Pendings for List Management:  ",str11;
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
          prtpage   prfile;*pcolumn:row,*font=font1,*boldon,"Monthly Cancelled Pending Report - 'Mail Dates over 6 Months old'";
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
          prtpage   prfile;*pcolumn:row,*font=font1,*boldon,"Monthly Cancelled Pending Report - 'Mail Dates over 6 Months old'";
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
          include   nordpio.inc
          include   nord4io.inc
          include   nord5io.inc
          include   nord6io.inc
          include   ncntio.inc
          include   compio.inc
          include   cntio.inc
          include   ndatio.inc
          include   npndio.inc
          include   nspe2io.inc
          include   comlogic.iNC