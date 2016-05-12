.......................................................
.......................................................
.         NSEL001A.PLS
.         Order Select History Report
.         Andrew Harkins
.         October 6, 2004
.
.         Queries Order Select History File (NINSEL2.DAT)
.       and produces a report listing occurances of keyed-in
.       Selects versus Selects found in NINSEL.DAT
.......................................................
.......................................................
          include   common.inc
          include   cons.inc
          include   nseldd.inc
          include   nsel2dd.inc
          include   norddd.inc
          include   nloldd.inc
          include   ndatdd.inc
          include   ncmpdd.inc
          include   ncntdd.inc

release   init      "1.0"               ASH       06OCT2004 Initial Release

prfile    pfile
FromDate dim        8
ToDate dim          8
.
PackList  ListView
SelArray1Tot        form      9(9999)
SelArray2Tot        form      9(9999)
str75     dim       75
str75b    dim       75
result2   form      10
str100    dim       100
ListHold dim        6
ListTotal form      9
SelTotal1 form      9
SelTotal2 form      9
KeyHold   dim       10
SelHold   dim       4
.
font2   font
font5   font

NINLogo   PICT

          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
        create  font2,"Arial",size=9 
        create  font5,"Arial",size=11
          create    PackList=2:20:2:50,SORTORDER=1
          PackList.InsertColumn using "List + Select",5,0
          PackList.InsertColumn using "List Name",5,1
          PackList.InsertColumn using "Select Name",5,2
          PackList.InsertColumn using "Number",5,3
          activate PackList
.
          move      C1,NORDPATH
          move      C1,NLOLPATH
          move      C1,NDATPATH
          move      C1,NCMPPATH
          move      C1,NCNTPATH
          call      Paint
          display   *p10:7,"Hit F5 to Quit"
          trap      STOP IF F5
          display   *p10:8,"Date Range (Enter as MMDDYYYY or MM/DD/YYYY)"
KeyInFromDate
          keyin     *p10:10,"From : ",*t30,*KCON,str10,*KCOFF;
          call      RemoveChar using str10,SLASH
          call      Trim using str10
          if (str10 <> "")
                    unpack    str10,MM,DD,CC,YY
                    pack      str10,MM,SLASH,DD,SLASH,CC,YY
                    pack      FromDate,CC,YY,MM,DD
                    display   *p10:10,"From : ",str10;
                    move      "Y",str1
                    display   *p30:10,"Correct? ",str1;
                    keyin     *p30:10,"Correct? ",*t10,*KCON,*rv,str1,*KCOFF;
                    if (str1 <> "Y" & str1 <> "y")
                              goto KeyInFromDate
                    endif
          else
                    move      "00000000",FromDate
                    pack      str10,"          "
          endif
          display   *p10:10,"From : ",str10;
KeyInToDate
          keyin     *p10:11,"To   : ",*t30,*KCON,str10,*KCOFF;
          call      RemoveChar using str10,SLASH
          call      Trim using str10
          if (str10 <> "")
                    unpack    str10,MM,DD,CC,YY
                    pack      str10,MM,SLASH,DD,SLASH,CC,YY
                    pack      ToDate,CC,YY,MM,DD
                    display   *p10:11,"To   : ",str10;
                    move      "Y",str1
                    display   *p30:11,"Correct? ",str1;
                    keyin     *p30:11,"Correct? ",*t10,*KCON,*rv,str1,*KCOFF;
                    if (str1 <> "Y" & str1 <> "y")
                              goto KeyInToDate
                    endif
          else
                    move      "99999999",ToDate
                    pack      str10,"          "
          endif
          display   *p10:11,"To   : ",str10;
.         goto reportgo
.         open      tempfile,"c:\work\ninsel2.dat"
          loop
nextrec
                    move      "NSEL2SEQ",Location
                    pack      KeyLocation,"Key: Seq"
                    call      NSEL2SEQ
.                   read      tempfile,SEQ;NSEL2VARS
                    until over
                    if (NSEL2DATE >= FromDate & NSEL2DATE <= ToDate)
.                             move      NSEL2NUM,str4
.                             add       C1,howmany
.                             display   *p10:12,"Records Processed : ",howmany
                              if (NSEL2CODE = "1")          .LR Records
                                        add       C1,howmany
                                        display   *p10:12,"Records Processed : ",howmany
                                        move      NSEL2LR,NORDFLD
                                        move      "NORDKEY",Location
                                        pack      KeyLocation,"Key: ",NORDFLD
                                        call      NORDKEY
                                        if over
                                                  goto nextrec
                                        endif
                                        pack      NDATFLD,OLNUM
                                        move      OLNUM,str6
                              else                          .LOL Records
                                        goto nextrec
                                        move      NSEL2LR,NLOLFLD
                                        move      "NLOLKEY",Location
                                        pack      KeyLocation,"Key: ",NLOLFLD
                                        call      NLOLKEY
                                        if over
                                                  goto nextrec
                                        endif
                                        move      NLOLCNum,NCMPFLD
                                        move      "NCMPKEY",Location
                                        pack      KeyLocation,"Key: ",NCMPFLD
                                        call      NCMPKEY
                                        pack      NDATFLD,NLOLList
                                        move      NLOLList,str6
                              endif
                              move      "NDATKEY",Location
                              pack      KeyLocation,"Key: ",NDATFLD
                              call      NDATKEY
                              pack      str10,str6,"!!!!"
                              call      Trim using MLSTNAME
                              if (NDATCONV = "1")
                                        pack      str100,MLSTNAME," --- converted"
                              else
                                        pack      str100,MLSTNAME," --- unconverted"
                              endif
                              PackList.FindItem giving result using *Text=str10
                              if (result = SEQ)   .Add Item
                                        PackList.InsertItem giving N9 using str10
                                        PackList.SetItemText using N9,str100,1
                                        PackList.SetItemText using N9,"1",3
                                        pack      NSELFLD1,"01X",NDATFLD
                                        clear     NSELFLD2
                                        clear     NSELFLD3
                                        move      "NSELAIM",Location
                                        pack      KeyLocation,"Key: ",NSELFLD1
                                        call      NSELAIM
                                        loop
                                                  until over
                                                  pack      str10,str6,NSELNUM
                                                  PackList.InsertItem giving N9 using str10
                                                  PackList.SetItemText using N9,str100,1
                                                  PackList.SetItemText using N9,NSELSNAME,2
                                                  PackList.SetItemText using N9,"0",3
                                                  move      "NSELKG",Location
                                                  pack      KeyLocation,"Key: ",NSELFLD1
                                                  call      NSELKG
                                        repeat
                              else
                                        PackList.GetItemText giving str9 using result,3
                                        call      Trim using str9
                                        move      C0,N9
                                        move      str9,N9
                                        add       C1,N9
                                        move      N9,str9
                                        call      Trim using str9
                                        PackList.SetItemText using result,str9,3
                              endif
                              pack      str10,str6,NSEL2NUM
                              if (NSEL2NUM = "XXXX")
                                        move      NSEL2NAME,str75b
                                        rep       lowup,str75b
.
                                        move      SEQ,N9
                                        loop
                                                  PackList.FindItem giving result using *Start=N9,*Text=str10
                                                  until (result = SEQ)
                                                  PackList.GetItemText giving str75 using result,2
                                                  rep       lowup,str75
                                                  if (str75 = str75b)
                                                            PackList.GetItemText giving str8 using result,3
                                                            call      Trim using str8
                                                            move      C0,N8
                                                            move      str8,N8
                                                            add       C1,N8
                                                            move      N8,str8
                                                            call      Trim using str8
                                                            PackList.SetItemText using result,str8,3
                                                            break
                                                  endif
                                                  move      result,N9
                                        repeat
                                        if (result = SEQ)   .Never found - Add it
                                                  PackList.InsertItem giving N9 using str10
                                                  PackList.SetItemText using N9,str100,1
                                                  PackList.SetItemText using N9,NSEL2NAME,2
                                                  PackList.SetItemText using N9,"1",3
                                        endif
                              else
                                        PackList.FindItem giving result using *Text=str10
                                        if (result = SEQ)   .Add Item - Should never happen!!!   Already loaded all Selects up top
                                                  PackList.InsertItem giving N9 using str10
                                                  PackList.SetItemText using N9,str100,1
                                                  PackList.SetItemText using N9,NSEL2NAME,2
                                                  PackList.SetItemText using N9,"1",3
                                        else
                                                  PackList.GetItemText giving str9 using result,3
                                                  call      Trim using str9
                                                  move      C0,N9
                                                  move      str9,N9
                                                  add       C1,N9
                                                  move      N9,str9
                                                  call      Trim using str9
                                                  PackList.SetItemText using result,str9,3
                                        endif
                              endif
                    endif
          repeat
ReportGo
          call      GetWinVer
          if (osflag = c1 | Osflag = C5 | osflag = c6)         .nt win2k Xp
                    PRTOPEN prfile,"\\NTS0\Laser2","FAXFILE.PRN"
          elseif (osflag = c3 | OSflag =c4)         .win 95 98
                    PRTOPEN prfile,"Laser2","FAXFILE.PRN"
          else   .(osflag = c0)         .Don't know prompt for printer
                    PRTOPEN prfile,"-","FAXFILE.PRN"
          endif
.Print Report
          display   *p10:12,"Creating Report"
        move    "500",column
        move    "1000",column1
        move    "2500",column2
        move    "3725",column3
        move    "5200",column4
        move    "5815",column5
        move    "6425",column6
.
          clock   timestamp,timestamp
          move      SEQ,N10
          move      C0,ListTotal
          move      C0,N1               .Used for Header Page for Regular Selects
          move      C0,SelTotal1
          move      C0,SelTotal2
          loop
                    PackList.GetNextItem giving result USING 0,N10
                    until (result = SEQ)
                    PackList.GetItemText giving KeyHold using result,0
                    unpack    KeyHold,str6,SelHold
                    if (str6 <> ListHold)
                              if (ListHold <> "")
                                        if (row > 9975)
                                                  call      PrintHeader
                                                  call      PrintHeader2
                                        endif
                                        call      PrintListTotal
                                        move      C0,ListTotal
                                        move      C0,N1               .Used for Header Page for Regular Selects
                                        move      C0,SelTotal1
                                        move      C0,SelTotal2
                                        prtpage   prfile;*NewPage;
                              endif
                              call      PrintHeader
                              PackList.GetItemText giving str100 using result,1
                              call      PrintHeader2
                              move      str6,ListHold
                    endif
                    if (SelHold <> "!!!!")
                              PackList.GetItemText giving str9 using result,3
                              call      Trim using str9
                              move      C0,howmany
                              move      str9,howmany
                              if (SelHold = "XXXX")
                                        if (SelTotal2 = 0)
                                                  if (row > 9975)
                                                            call      PrintHeader
                                                            call      PrintHeader2
                                                  endif
                                                  call      PrintHeader4
                                        endif
                                        add       howmany,SelTotal2
                              else
                                        if (N1 = 0)
                                                  if (row > 9975)
                                                            call      PrintHeader
                                                            call      PrintHeader2
                                                  endif
                                                  call      PrintHeader3
                                                  move      C1,N1
                                        endif
                                        add       howmany,SelTotal1
                              endif
                              PackList.GetItemText giving NSEL2NAME using result,2
                              call      PrintDetail
                    else
                              PackList.GetItemText giving str9 using result,3
                              call      Trim using str9
                              move      C0,ListTotal
                              move      str9,ListTotal
                    endif
                    move      result,N10
          repeat
.
          if (ListHold <> "")
                    if (row > 9975)
                              call      PrintHeader
                              call      PrintHeader2
                    endif
                    call      PrintListTotal
          endif
STOP
          prtclose prfile
          stop

PrintHeader
          unpack  timestamp,CC,YY,MM,DD
          pack    str8,MM,SLASH,DD,SLASH,CC,YY
          prtpage prfile;*UNITS=*HIENGLISH;
          move    "300",row
          prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
          add     eightlpi,row
          add     eightlpi,row
          add     eightlpi,row
          add     "60",row
          add     eightlpi,row
          add     eightlpi,row
          add     eightlpi,row 
          add     eightlpi,row 
          prtpage prfile;*p3000:row,*font=font5,*boldon,"SELECT NAME HISTORY REPORT",*boldoff;
          add     eightlpi,row 
          add     eightlpi,row 
          add     eightlpi,row 
          unpack    FromDate,str4,MM,DD
          pack      str10,MM,SLASH,DD,SLASH,str4
          prtpage prfile;*pcolumn:row,"From Date: ",str10;
          unpack    ToDate,str4,MM,DD
          pack      str10,MM,SLASH,DD,SLASH,str4
          prtpage prfile;*p3000:row,"To Date:   ",str10;
          add     eightlpi,row 
          add     eightlpi,row
          return

PrintHeader2 
          prtpage prfile;*pcolumn:row,*font=font5,"List:  ",*BOLDON,str100,*BOLDoFF;
          add       eightlpi,row
          add       eightlpi,row
          return

PrintHeader3
          prtpage prfile;*pcolumn1:row,*ULON,"Valid Selects",*ULOFF;
          add       eightlpi,row
          return

PrintHeader4
          add       eightlpi,row
          add       eightlpi,row
          prtpage prfile;*pcolumn1:row,*ULON,"Keyed-In Selects",*ULOFF;
          add       eightlpi,row
          return

PrintListTotal
          add     eightlpi,row
          add     eightlpi,row
          prtpage prfile;*pcolumn:row,"GRAND TOTAL";
          add     eightlpi,row
          prtpage prfile;*pcolumn:row,"List";
          move      ListTotal,str9
          call      FormatNumeric using str9,str11
          call      Trim using str11
          prtpage prfile;*pcolumn2:row,str11;
          add     eightlpi,row
          prtpage prfile;*pcolumn:row,"Valid Selects";
          move      SelTotal1,str9
          call      FormatNumeric using str9,str11
          call      Trim using str11
          prtpage prfile;*pcolumn2:row,str11;
          add     eightlpi,row
          prtpage prfile;*pcolumn:row,"Keyed-In Selects";
          move      SelTotal2,str9
          call      FormatNumeric using str9,str11
          call      Trim using str11
          prtpage prfile;*pcolumn2:row,str11;
          return
.
PrintDetail
          add       eightlpi,row
          prtpage prfile;*pcolumn2:row,NSEL2NAME;
          call      FormatNumeric using str9,str11
          call      Trim using str11
          prtpage prfile;*pcolumn6:row,str11;
          return

drewtest
          move      str1,str1
          return
.
          include   nselio.inc
          include   nsel2io.inc
          include   ncmpio.inc
          include   ndatio.inc
          include   nlolio.inc
          include   nordio.inc
          include   ncntio.inc
          include   comlogic.INC