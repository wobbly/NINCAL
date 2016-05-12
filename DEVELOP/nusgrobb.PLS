PC       EQU       1

          include   common.inc
          include   cons.inc
          include   nusgdd.inc
          include   ndatdd.inc
          include   norddd.inc
          include   compdd.inc
          include   cntdd.inc
          
release  init      "2.0"       RVW                .delete criteria not working - omit running charge lists
reldate   Init      "28 April 2014"

FindUsage   external "NUSG001a;FindUsage"
FindUsage2  external "NUSG001a;FindUsage2"

Data3CalcLoadUsageMessage external "NDAT0001;Data3CalcLoadUsageMessage"

PackData DataList
DimPtr    dim       ^
FrmPtr    form      ^

HoldDate  Form      6

mss1      plform    Error
          formload mss1
          
          move      "NUSG0001",PROGRAM
          move      "Datacard Weekly Usage ",STITLE
          move      "Names In The News      ",COMPNME
          call      PAINT

          move      C1,NDATPATH  .SET ACCESS TO ISAM
          move      C1,NORDPATH
          move      C3,NORDLOCK
          move      C3,NDATLOCK

          move      "ABORT" TO PF5

          call      FUNCDISP
          display   *P1:8,"Backing up ninusg.DAT"
          move      "\\nins1\e\data\text\ninusg.dat",str35
          move      "\\nins1\e\data\text\ninusgx.dat",str45
          move      "\\nins1\e\data\text\ninusgx.dat|10.10.30.103:502",taskname
          Erase     str45
          Rename    str35,str45
          clock     timestamp,timestamp
          unpack    timestamp,CC,YY,MM,DD
          call      CVTJUL
          Move      Juldays to HoldDate           .todays 
          
Fudge
          display   *P1:8,"Creating New File"
          Prepare   NUSGFILE,"\\nins1\e\data\text\ninusg.dat","\\nins1\e\data\index\ninusg.isi|10.10.30.103:502","1-12","44"
          Prepare   NUSGFLE2,"\\nins1\e\data\text\ninusg.dat","\\nins1\E\data\index\ninusg.aam|10.10.30.103:502","1-6,7-12","44"

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

                    elseif (NUSGCODE = "2")       entered by hand
                    Packkey   Ndatfld,NUSGLIST
                    rep       Zfill,nusglist
                    call      Ndatkey
                    packkey      NUSGFLD,NUSGLIST,NUSGMLR
                    If        (Elstcde = "C" | Elstcde = "P")                 .if exclusve clean up
                              Unpack    NUSGDATE,CC,YY,MM,DD                  .get date of entry
                              call      CVTJUL
                                        IF        (HoldDate-Juldays < "365")          .less than a year old? save it
                                                  move      "NUSGwrt",Location
                                                  call      NUSGWrt
                                        Endif               
                              Else                                .not excl
                              move      "NUSGwrt",Location
                              call      NUSGWrt
                              Endif                         
                    endif
          repeat
          close     tempfile
          Close     NDATFLIST
          MOVE      C0,NDATFLAG

CreateNew
          display   *P1:8,*el,"Creating New Usage Records"
          destroy   PackData
          create    PackData=6:20:57:79,STYLE=3DOUT
          activate  packdata
          clock     timestamp,timestamp
          unpack    timestamp,CC,YY,MM,DD
          call      CVTJUL
          sub       "730",JULDAYS,N6    Outer limit of records to search - 2 years
          loop
                    move      "NDATSEQ",Location
                    clear     KeyLocation
                    call      NDATSEQ
                    until     over
                    add       C1,howmany
                    display   *P1:10,"Reading Datacard Record ",howmany                  
                    display   *P1:14,"Reading Datacard ##     ",Lstnum
                    display   *P1:16,"Reading Datacard Named  ",Olstname
                    call Trim using LSTNUM
                    Reset     Runcodes
                    scan      lstnum in runcodes
                    if        Not equal
                              if (STATUS <> "W" & STATUS <> "T" & LSTNUM <> "") .Added non-Exclusives 09/15/2004


                              call      UsageReadOrder using C0
                              endif
          endif
          repeat
          shutdown      
          
          
UsageReadOrder LRoutine FrmPtr
.FrmPtr  = Switch to determine if display message should be shown
. 0 = Running in batch mode, 1 = Called from NDAT0001
          add       C1,N9
          if (FrmPtr = C0)
                    display   *P1:12,"Reading Datacard Usage  ",N9
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
                    if (OSTAT = "0" or OSTAT = "B")
                              PackData.FindStringExact giving result using OMLRNUM,0
                              if (result = SEQ)
                                        insertitem PackData,999999,OMLRNUM
                                        if (FrmPtr = C1)
                                                  add       C1,howmany
                                                  call      Data3CalcLoadUsageMessage using OMLRNUM,howmany
                                        endif
                                        call      FindUsage2 using LSTNUM,OMLRNUM,N1
                                        if (N1 = C1)        Found valid usage
                                                  move      "COMPKEY3",Location
                                                  packkey      COMPFLD3,OMLRNUM
                                                  pack      KeyLocation,"Key: ",COMPFLD3
                                                  call      COMPKEY3
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
          elseif (STATUS = "W" | STATUS = "T")                       .non-Exclusives Added 09/15/2004
                    alert     note,"Usage is allowed only on non-withdrawn Datacards!",result
                    return
          endif
          display   *P1:8,"Backing up ninusg.DAT"
          move      "\\nins1\e\data\text\ninusg.dat",str35
          move      "c:\work\ninusga.dat",str45
          copyfile str35,str45
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
        
	include   nusgio.inc
	include   ndatio.inc
	include   nordio.inc
	include   compio.inc
	include   cntio.inc
	include   comlogic.inc          

