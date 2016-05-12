.LIST USAGE CREATION PROGRAM
........................................
. Program:          NUSG0001.PLS
. Function:         List Usage Creation Program (Runs in batch mode for all Datacards as well as single Datacard mode)
. Author: Andrew Harkins
. Orig.   Date:     July 22,2004
. Rewritten:  Robb Whiting
. Orig.   Date:     April 21, 2014
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

release  init      "2.0"       RVW                .rewrite because it's skipping things.  I think it's skipping the existing records.
.						   I don't believe there is a reason to treat existing records and new records differently.
.						   New logic is to check main order file for reasons it shouldn't be in there, and if not put it in.
.						   Doesn't matter if it was in there last week or not.  If we read the whole file it'll get in if it should.
reldate   Init      "11 January 2011"

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
.Clean up existing NINUSGR
          display   *P1:8,"Backing up NINUSGR.DAT"
          move      "\\nins1\e\data\text\NINUSG.dat",str35
          move      "\\nins1\e\data\text\NINUSGRx.dat",str45
          move      "\\nins1\e\data\text\NINUSGRx.dat|NINS1:502",taskname
.begin patch
          Erase     str45
.end patch
.         move      "\\nts2\d\data\text\NINUSGR.dat",str35
.         move      "\\nts2\d\data\text\NINUSGRx.dat",str45
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
          Prepare   NUSGFILE,"\\nins1\e\data\text\NINUSGR.dat","\\nins1\e\data\index\NINUSGR.isi|NINS1:502","1-12","44"
          Prepare   NUSGFLE2,"\\nins1\e\data\text\NINUSGR.dat","\\nins1\E\data\index\NINUSGR.aam|NINS1:502","1-6,7-12","44"
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