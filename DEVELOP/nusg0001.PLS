.LIST USAGE CREATION PROGRAM
........................................
. Program:          NUSG0001.PLS
. Function:         List Usage Creation Program (Runs in batch mode for all Datacards as well as single Datacard mode)
. Author: Andrew Harkins
. Orig.   Date:     July 22,2004
. Release:          1.0
........................................
PC       EQU       1

          include   common.inc
          include   cons.inc
          include   nusgdd.inc
          include   ndatdd.inc
          include   norddd.inc
          include   compdd.inc
          include   cntdd.inc

release  init      "1.5"       DLH                .delete criteria not working - omit running charge lists
reldate   Init      "11 January 2011" 
.release  init      "1.4"       DLH                .weekly run is failing
.reldate   Init      "20 April 2009"

.release  init      "1.31"       DLH 23SEP07       PLI
.release  init      "1.3"       DLH 20SEP07       Delete Old Hand Entered data on exlusive files if over 1 year old
.release  init      "1.2"       ASH 20JAN05       VERSION USING CRITERIA ESTABLISHED BY NP 1/20/2005
.release  init      "1.1"       ASH 15SEP04       Include ALL Datacards except withdrawns
.release  init      "1.0"       ASH 21JUL04       Initial Release

.EXTERNAL ROUTINES FROM       NUSG001a.PLC
FindUsage   external "NUSG001a;FindUsage"
FindUsage2  external "NUSG001a;FindUsage2"
.END PATCH 1.1 ADDED LOGIC
FindUsageLoadErrorWindow external "NUSG001a;FindUsageLoadErrorWindow"
.END PATCH 1.1 ADDED LOGIC
.EXTERNAL ROUTINES FROM NDAT0001.PLC - found under Click_Data3Calc
Data3CalcLoadUsageMessage external "NDAT0001;Data3CalcLoadUsageMessage"

PackData DataList
DimPtr    dim       ^
FrmPtr    form      ^

HoldDate  Form      6
.START PATCH 1.1 ADDED LOGIC
mss1      plform    Error

          formload mss1
          call      FindUsageLoadErrorWindow
.END PATCH 1.1 ADDED LOGIC
          move      "NUSG0001",PROGRAM
          move      "Datacard Weekly Usage ",STITLE
          move      "Names In The News      ",COMPNME
          move      C1,NDATPATH  .SET ACCESS TO ISAM
          move      C1,NORDPATH
          move      C3,NORDLOCK
          move      C3,NDATLOCK
          call      PAINT
          move      "ABORT" TO PF5
          call      FUNCDISP
...////temp         
.          goto      createnew
.          Goto      fudge
.Clean up existing NINUSG
          display   *P1:8,"Backing up NINUSG.DAT"
          move      "\\nins1\e\data\text\ninusg.dat",str35
          move      "\\nins1\e\data\text\ninusgx.dat",str45
          move      "\\nins1\e\data\text\ninusgx.dat|10.10.30.103:502",taskname
.begin patch
          Erase     str45
.end patch
.         move      "\\nts2\d\data\text\ninusg.dat",str35
.         move      "\\nts2\d\data\text\ninusgx.dat",str45
.begin patch 1.4
          Rename    str35,str45
.          copyfile   str35,str45
.end patch 1.4
.begin patch 1.3
          clock     timestamp,timestamp
          unpack    timestamp,CC,YY,MM,DD
          call      CVTJUL
.          Move      Juldays to N6           .todays date
          Move      Juldays to HoldDate           .todays date
.end patch 1.3
.
.begin patch 1.4
Fudge
          display   *P1:8,"Creating New File"
          Prepare   NUSGFILE,"\\nins1\e\data\text\ninusg.dat","\\nins1\e\data\index\ninusg.isi|10.10.30.103:502","1-12","44"
          Prepare   NUSGFLE2,"\\nins1\e\data\text\ninusg.dat","\\nins1\E\data\index\ninusg.aam|10.10.30.103:502","1-6,7-12","44"
.          display   *P1:8,"Deleting Previous Entries"

          display   *P1:8,"Restoring Previous Manual Entries"
          open      tempfile,taskname,exclusive
          loop
                    move      "NUSGSEQ-temp",Location
                    clear     KeyLocation
                    read      tempfile,SEQ;NUSGVARS
                    until over
                    if (Nusglist = "011889")
                    call debug
                    endif
                    if (NUSGCODE = "1") entered via weekly run (this program)
.                              packkey      NUSGFLD,NUSGLIST,NUSGMLR
.                              pack      KeyLocation,"Key: ",NUSGFLD
.                              move      "NUSGKEY",Location
.                              call      NUSGKEY
.                              if not over
.                                        move      "NUSGDEL",Location
.                                        call      NUSGDEL
.                              endif
..begin patch 1.3
..                   elseif (NUSGCODE = "2")       entered by hand
                    elseif (NUSGCODE = "2")       entered by hand
                    Packkey   Ndatfld,NUSGLIST
                    rep       Zfill,nusglist
                    call      Ndatkey
                    packkey      NUSGFLD,NUSGLIST,NUSGMLR
                    If        (Elstcde = "C" | Elstcde = "P")                 .if exclusve clean up
                              Unpack    NUSGDATE,CC,YY,MM,DD                   .get date of entry
                              call      CVTJUL
                                        IF        (HoldDate-Juldays < "365")          .less than a year old? save it
.                                                  packkey      NUSGFLD,NUSGLIST,NUSGMLR
.                                                  pack      KeyLocation,"Key: ",NUSGFLD
.                                                  move      "NUSGtst",Location
.                                                  call      NUSGtst
.                                                  if        over
                                                  move      "NUSGwrt",Location
                                                  call      NUSGWrt
.                                                  endif
                                        Endif               
                              Else                                .not excl
                              move      "NUSGwrt",Location
                              call      NUSGWrt
                              Endif                         
.end patch 1.3
.end patch 1.4
                    endif
          repeat
          close     tempfile
.begin patch 1.4          .ensure we read entire datacard file
          Close     NDATFLIST
          MOVE      C0,NDATFLAG
.end patch 1.4
CreateNew
.Read through Datacard File
          display   *P1:8,*el,"Creating New Usage Records"
          destroy   PackData
.         create    PackData=1:1:1:1
.          create    PackData=6:20:48:70,STYLE=3DOUT
          create    PackData=6:20:57:79,STYLE=3DOUT
          activate  packdata
          clock     timestamp,timestamp
          unpack    timestamp,CC,YY,MM,DD
          call      CVTJUL
          sub       "730",JULDAYS,N6    Outer limit of records to search - 2 years
.START TEST LOGIC TO TEST FOR A BUG
.END TEST LOGIC TO TEST FOR A BUG
          loop
                    move      "NDATSEQ",Location
                    clear     KeyLocation
                    call      NDATSEQ
                    until     over
                    add       C1,howmany
                    display   *P1:10,"Reading Datacard Record ",howmany                  
.begin patch 1.4
                    display   *P1:14,"Reading Datacard ##     ",Lstnum
                    display   *P1:16,"Reading Datacard Named  ",Olstname
.end patch 1.4
.START PATCH 1.1 REPLACED LOGIC
.                   if (ELSTCDE = "C" & (STATUS <> "W" & STATUS <> "T"))        Exclusives Only at this point 7/21/2004
                    call Trim using LSTNUM
.begin patch 1.5
                    Reset     Runcodes
                    scan      lstnum in runcodes
                    if        Not equal
.end patch 1.5
                              if (STATUS <> "W" & STATUS <> "T" & LSTNUM <> "") .Added non-Exclusives 09/15/2004

.END PATCH 1.1 REPLACED LOGIC
                              call      UsageReadOrder using C0
.                             add       C1,N9
.                             display   *P1:12,"Reading Exclusive       ",N9
.                             PackData.ResetContent
.                             pack      NORDFLD2,"02X",LSTNUM
.                             move      "AIM-readlast",Location
.                             pack      KeyLocation,"Key: ",NORDFLD2
..                            call      NORDAIM
.                             branch    NORDFLG2 TO NORDlast9
.                             call      NORDOPN2
.NORDlast9
.                             readlast NORDFLE2,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,NORDFLD6,NORDFLD7,NORDFLD8;ORDVARS
.                             loop
.                                       until over
.                                       move      OODTEC,CC
.                                       move      OODTEY,YY
.                                       move      OODTEM,MM
.                                       move      OODTED,DD
.                                       call      CVTJUL
.                                       until (JULDAYS < N6)
.                                       if (OSTAT = "0" or OSTAT = "B")
.                                                 PackData.FindStringExact giving result using OMLRNUM,0
.                                                 if (result = SEQ)
.                                                           insertitem PackData,999999,OMLRNUM
.                                                           call      FindUsage using LSTNUM,OMLRNUM,N1
.                                                           if (N1 = C1)        Found valid usage
..
.                                                                     move      "COMPKEY3",Location
.                                                                     pack      COMPFLD3,OMLRNUM
.                                                                     pack      KeyLocation,"Key: ",COMPFLD3
.                                                                     call      COMPKEY3
..
.                                                                     pack      NUSGFLD,LSTNUM,COMPNUM
.                                                                     move      "NUSGTST",Location
.                                                                     pack      KeyLocation,"Key: ",NUSGFLD
.                                                                     call      NUSGTST
.                                                                     if over
.                                                                               move      LSTNUM,NUSGLIST
.                                                                               move      COMPNUM,NUSGMLR
.                                                                               move      timestamp,NUSGDATE
.                                                                               move      "1",NUSGCODE
.                                                                               clear     NUSGINITS
.                                                                               move      "NUSGWRT",Location
.                                                                               call      NUSGWRT
.                                                                     endif
.                                                           endif
.                                                 endif
.                                       endif
.                                       move      "NORDKGP",Location
.                                       pack      KeyLocation,"Key: ",NORDFLD2
.                                       call      NORDKGP
.                             repeat
                              endif
.begin patch 1.5
          endif
.end patch 1.5
          repeat
          shutdown

UsageReadOrder LRoutine FrmPtr
.FrmPtr  = Switch to determine if display message should be shown
. 0 = Running in batch mode, 1 = Called from NDAT0001
          add       C1,N9
          if (FrmPtr = C0)
.START PATCH 1.1 REPLACED LOGIC
.                   display   *P1:12,"Reading Exclusive       ",N9
                    display   *P1:12,"Reading Datacard Usage  ",N9
.END PATCH 1.1 REPLACED LOGIC
          endif
          PackData.ResetContent
          Clear     NordFLd1
          Clear     NordFLd3
          Clear     NordFLd4
          Clear     NordFLd5
          Clear     NordFLd6
          Clear     NordFLd7
          Clear     NordFLd8
          packkey   NORDFLD2,"02X",LSTNUM
          move      "AIM-readlast",Location
          pack      KeyLocation,"Key: ",NORDFLD2
.         call      NORDAIM
          branch    NORDFLG2 TO NORDlast9
          call      NORDOPN2
NORDlast9
          readlast NORDFLE2,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,NORDFLD6,NORDFLD7,NORDFLD8;ORDVARS
          loop
                    until over
                    move      OODTEC,CC
                    move      OODTEY,YY
                    move      OODTEM,MM
                    move      OODTED,DD
                    call      CVTJUL
.begin patch 1.31a
.begin patch 1.31
.                    clear     str1
.                    unpack      olrn,str1,str5 
.                   until (JULDAYS < N6 & str1 <> "B" & STR1 <> "M")            .just go till over
.                   until (JULDAYS < N6 & Ocompid <> "P" & Ocompid2 <>"P")
.end patch 1.31a
.                   until (JULDAYS < N6)
.end patch 1.31
                    if (OSTAT = "0" or OSTAT = "B")
                              PackData.FindStringExact giving result using OMLRNUM,0
                              if (result = SEQ)
                                        insertitem PackData,999999,OMLRNUM
                                        if (FrmPtr = C1)
                                                  add       C1,howmany
                                                  call      Data3CalcLoadUsageMessage using OMLRNUM,howmany
                                        endif
.START PATCH 1.2 REPLACED LOGIC
.                                       call      FindUsage using LSTNUM,OMLRNUM,N1
                                        call      FindUsage2 using LSTNUM,OMLRNUM,N1
.END PATCH 1.2 REPLACED LOGIC
                                        if (N1 = C1)        Found valid usage
.
                                                  move      "COMPKEY3",Location
                                                  packkey      COMPFLD3,OMLRNUM
                                                  pack      KeyLocation,"Key: ",COMPFLD3
                                                  call      COMPKEY3
.
                                                  pack      NUSGFLD,LSTNUM,COMPNUM
                                                  move      "NUSGTST",Location
                                                  pack      KeyLocation,"Key: ",NUSGFLD
                                                  call      NUSGTST
                                                  if        over
                                                            move      LSTNUM,NUSGLIST
                                                            move      COMPNUM,NUSGMLR
                                                            move      timestamp,NUSGDATE
                                                            move      "1",NUSGCODE
                                                            clear     NUSGINITS
                                                            move      "NUSGWRT",Location
                                                            call      NUSGWRT
.something wrong should not need to test if cleanup worked add an alert
                                                  Else
                                                  pack      Str55 from "trying to add usage that exists ",lstnum,b1,compnum:
                                                            crlf,"will skip"
.                                                  alert     note,str55,result
                                                  
                                                  endif
                                        endif
                              endif
                    endif
                    move      "NORDKGP",Location
                    pack      KeyLocation,"Key: ",NORDFLD2
                    call      NORDKGP
          repeat
          return
.external call from Ndat0001
UpdateListUsage     Routine DimPtr
.DimPtr  = List Number
          call      Trim using DimPtr
          if (DimPtr = "")
                    return
          endif
          move      C1,NDATPATH  .SET ACCESS TO ISAM
          move      C1,NORDPATH
          move      C3,NORDLOCK
          pack      NDATFLD,DimPtr
          move      "NDATKEY",Location
          pack      KeyLocation,"Key: ",NDATFLD
          call      NDATKEY
          if over
                    alert     note,"Invalid Datacard!",result
                    return
.START PATCH 1.1 REPLACED LOGIC
.         elseif (ELSTCDE <> "C" | STATUS = "W" | STATUS = "T"))                .Exclusives Only at this point 7/21/2004
.                   alert     note,"Usage is allowed only on non-withdrawn Exclusive Datacards!",result
          elseif (STATUS = "W" | STATUS = "T")                       .non-Exclusives Added 09/15/2004
                    alert     note,"Usage is allowed only on non-withdrawn Datacards!",result
.END PATCH 1.1 REPLACED LOGIC
                    return
          endif
.Clean up existing NINUSG
          display   *P1:8,"Backing up NINUSG.DAT"
          move      "\\nins1\e\data\text\ninusg.dat",str35
          move      "c:\work\ninusga.dat",str45
          copyfile str35,str45
.
          pack      NUSGFLD1,"01X",DimPtr
          move      "NUSGAIM",Location
          pack      KeyLocation,"Key: ",NUSGFLD1
          call      NUSGAIM
          loop
                    until over
                    if (NUSGCODE = "1") entered via weekly run (this program)
                              pack      NUSGFLD,NUSGLIST,NUSGMLR
                              pack      KeyLocation,"Key: ",NUSGFLD
                              move      "NUSGKEY",Location
                              call      NUSGKEY
                              if not over
                                        move      "NUSGDEL",Location
                                        call      NUSGDEL
                              endif
                    elseif (NUSGCODE = "2")       entered by hand
                    endif
                    move      "NUSGKG",Location
                    call      NUSGKG
          repeat

.Read through Datacard File
          display   *P1:8,"Creating New Usage Records"
          destroy PackData
          create    PackData=1:1:1:1
          clock     timestamp,timestamp
          unpack    timestamp,CC,YY,MM,DD
          call      CVTJUL
          sub       "730",JULDAYS,N6    Outer limit of records to search - 2 years
          move      C0,howmany
          call      UsageReadOrder using C1
          return

          include   nusgio.inc
          include   ndatio.inc
          include   nordio.inc
          include   compio.inc
          include   cntio.inc
          include   comlogic.inc
