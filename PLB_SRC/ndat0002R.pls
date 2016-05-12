PC EQU 0
          INCLUDE   common.inc
          INCLUDE   cons.inc
          include   ndatdd.inc
          include   nseldd.inc
          include   nrefdd.inc
.         include   nmoddd.inc
          include   nadddd.inc
          include   narrdd.inc
          include   ncatdd.inc
          include   NSLTdd.inc
          include   nsrcdd.inc
          include   nmoddd.inc
          include   ntxtdd.inc
          include   nowndd.inc
          include   ncntdd.inc
.START PATCH 1.1.9 REPLACED LOGIC
.         include   nfuldd.inc
          include   compdd.inc
          include cntdd.inc
.END PATCH 1.1.9 REPLACED LOGIC
.begin patch 1.42
          include   ntxt1dd.inc
.end patch 1.42
          include winapi.inc
Release   INit      "1.54"    DLH   replace laser3 with laser4
Reldate   INit      "2013 December 12"
.Release   INit      "1.53"    DLH   reciprocal verbage
.Reldate   INit      "2013 October 24"
.Release   INit      "1.52"    DLH   Move Tertiary selects under Base Selects
.Reldate   INit      "2013 September 03"
.Release   INit      "1.51"    DLH  .Check for PDF from butil
.Reldate   INit      "2013 April 17"
.Release   INit      "1.5"    DLH  .Internal PDF
.Reldate   INit      "2013 April 17"
.Release   INit      "1.43"    DLH  .SPOOL Trap
.Reldate   INit      "6 September 2012"
.Release   INit      "1.42"    DLH  .SRDS
.Reldate   INit      "20 September 2011"
.Release   INit      "1.41"    DLH  switched from Pause verb to subroutine waitin (comlogic)
.Reldate   INit      "14 March 2010"
.Release   INit      "1.40"    DLH  add code check for PlbCLient
.Reldate   INit      "11 March 2010"
.Release   INit      "1.39"    DLH  change file wait logic for emails
.Reldate   INit      "04 March 2010"
.Release   INit      "1.38"    DLH Stop Printing PL
.Reldate   INit      "17 February 2010"
.Release   INit      "1.37"    DLH Stop Printing "exclusive"
.Reldate   INit      "18 December 2009"
.Release   INit      "1.36"    DLH pass company for logo
.Reldate   INit      "13 August 2008"
.Release  INit      "1.35"    DLH sendmail
.Reldate  INit      "23 April 2008"
.Release  INit      "1.34"    DLH Print Minimum/Reuse data
.Reldate  INit      "11 April 2008"
.Release  INit      "1.33"    DLH New Cancellation fee
.Reldate  INit      "09 April 2008"
.Release  INit      "1.32"    DLH 23May 2007      PLI
.Reldate  INit      "23 May 2007"
.Release  INIT      "1.31" 27April2007 DLH 995
.Release  INIT      "1.3" 13March2007 DLH Pacfic Lists Logo
.Release  INIT      "1.2" 1Nov2006 DLH MIn Conversion
.Release  INIT "1.1.9" 21JUN2006 DMS Fulfillment Conversion
.Release INIT "1.1.8.1" 15August2006 DMB Modified Read Var for PDF995 ini to accomodate longer strings using taskname
.Release INIT "1.1.8" 20March2006 DMS multi-page printout retain owner info, (work order 928)
.Release INIT "1.1.7" 26MAY2005 ASH Small Changes in format.
.Release INIT "1.1.6" 07April2005 ASH Converted COMMPER
.Release INIT "1.1.5" 22December2004 DLH 995 cfg file check
.;Release INIT "1.1.4" 05AUG2004 ASH Logo Conversion
.Release INIT "1.1.3" 28JUL2004 ASH Added ability to append Usage information
.Release INIT "1.1.2" 04MAY2004 DMB Added Subroutine for datacardfax
.Release INIT "1.1.1" 22APR2004 ASH Small fix to "Sex:" label
.                                       Added ability to print all Selects if doing In-House Copy
.Release INIT "1.1" 24MAR2004 ASH Converted program to allow dynamic access
.Release INIT "1.0" 16JAN2004 Begin Data Card Creation

SelectTestBase4 external "NDAT001a;SelectTestBase4"
.START PATCH 1.1.3 ADDED LOGIC
PrintUsageToPFile external "NUSG002B;PrintUsageToPFile"
.END PATCH 1.1.3 ADDED LOGIC
.;Begin Paatch 1.1.5
.Reset995Flag   Dim            1                             ;'Y' means we played with pdf99.ini and need to restore
.moved to cons
.;end Paatch 1.1.5
.START PATCH 1.52 ADDED LOGIC
Tertbase  Dim       1         Tertiary base = Y
.END PATCH 1.52 ADDED LOGIC
.begin patch 1.40
Size      FORM      10
BlksLeft  FORM      10
.end patch 1.40
 
.begin patch 1.39

FileCheck FIle
trapcount form      4
.end patch 1.39
DATACARDPRINT       PFILE
PRTFILENAME         DIM       50
PRINTNAME DIM       50
COPY      FORM      3
SelectListView      listview
Select2ListView     listview
White     color
Black     color
CARR           INIT    0x7f
TimesNew7 font
CourierNew8         font
TimesNew11          font
TimesNew16          font
TimesNew16I         font
CourierNew12        font
SingleSpaced        FORM      "180"
.OneandahalfSpaced  FORM      "270"
.DoubleSpaced       FORM      "320"
LgBoxHeight         FORM      "9900"
SmBoxHeight         FORM      "350"

MASK13    INIT      "Z,ZZZ,ZZZ,ZZZ"
DIM13a    DIM       13
DIM9a     DIM       13
.hold2    DIM       4500                          .MIN
hold2     DIM       7500
line1     dim       46
.line1    dim       55
LASTLINE  FORM      "9450"
.Flags
.Left
ArrangementFlag     INIT      "1"
AddressingFlag      INIT      "1"
SourceFlag          INIT      "1"
SelectionsFlag      INIT      "1"
.Right
SelectFlag1         INIT      "1"
TextFlag  INIT      "1"
SelectFlag2         INIT      "1"

UnitFlag  INIT      "1"
CleanCdeFlag        INIT      "1"
NetNameFlag         INIT      "1"
SampleFlag          INIT      "1"
DeliveryFlag        INIT      "1"
AddNotesFlag        INIT      "1"
OwnerPrintFlag      FORM      "00"
EnddetailFlag       INIT      "N"
PageNum             FORM       5
SELNOTESFLAG        INIT      "N"
.Patch1.2
PICT1   PICT
.Patch1.2
#n4       FORM      4
#n41      FORM      4
#n8       FORM      8
#n9       FORM      9
#n1       FORM      1
#n2       FORM      2
#n3       FORM      3
#result   FORM      9
#str3     DIM       3
num9      FORM      9
.CRLF     INTEGER   1,"0x07F"
N52       form      5.2
SectionOne          INIT      "N"
SectionTwo          INIT      "N"
.AdobePDFFlag       INIT      "N"
OwnerInfoFlag       INIT      "N"
FaxFlag                       INIT "N"
RECCOUNT  FORM      5
.hexeight integer   4,"4294967295"
.
.;
DIMTEXT1  DIM       46
DIMTEXT2  DIM       46
DIMTEXT3  DIM       46
DIMTEXT4  DIM       46
DIMTEXT6  DIM       4
DIMTEXT5  DIM       46
DATCARD   FILE
.begin patch 1.3
FontPL5                 font
FontPL8B               font
.end patch 1.3
.START PATCH 1.34
REntFlag  Dim       1         .'Y' if anypart of list is rental
RentRow   Form      5
.End PATCH 1.34

.START PATCH 1.1 ADDED LOGIC
.tempfile file
DimPtr    Dim       ^
DimPtr1   Dim       ^
DimPtr2   Dim       ^
FrmPtr    form      ^
FrmPtr1   form      ^
.START PATCH 1.1.3 ADDED LOGIC
FrmPtr2   form      ^
UsageFlag form      1
.END PATCH 1.1.3 ADDED LOGIC
.START PATCH 1.32 ADDED LOGIC
FrmPtr3   form      ^
.END PATCH 1.32 ADDED LOGIC
CallFlag  FORM      1
PrintFlag FORM      1
.END PATCH 1.1 ADDED LOGIC

.
BEGIN
.Begin Patch 1.3
          create    fontPL5,"Times New Roman",size=5
          create    fontPL8B,"Times New Roman",size=8,Bold
.End Patch 1.3
.Patch1.2
        CREATE      PICT1=3:13:30:50:
                    "\\nins1\e\apps\PLB\CODE\INHOUSEOVERLAY.BMP"
.patch1.2
.START PATCH 1.1.4 ADDED LOGIC
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 1.1.4 ADDED LOGIC

.START PATCH 1.1 ADDED LOGIC
          call      GetWinVer
.xxx
.begin patch 1.5
.         Call      GetPDFPath
.end patch 1.5
.xxx
          call      CreateObjects
          call      debug
.begin patch 1.43
          Trap       SPOOL1 giving error if SPOOL
.end patch 1.43
          if (CallFlag = 1)
                    pack      inpname,DimPtr
                    move      "OWNER",COMMENT
                    pack      INITS,DimPtr1
                if (PrintFlag = C0 | PrintFlag = 1)     .Laser3 = Default
                        if (osflag = c1 | Osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt win2k Xp
                                PRTOPEN DataCardPrint,"\\NINS2\laser3 Blankstock","dataprnt.lst"
                        elseif (osflag = c3 | OSflag =c4)         .win 95 98
                                PRTOPEN DataCardPrint,"Laser3 Blankstock","dataprnt.lst"
                        else   .(osflag = c0)         .Don't know prompt for printer
                                PRTOPEN DataCardPrint,"-","dataprnt.lst"
                        endif
                elseif (PrintFlag = 2)  .Laser4
                        if (osflag = c1 | osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt 2k xp
                                PRTOPEN DataCardPrint,"\\NINS2\Laser4 Legal","dataprnt.lst"
                        elseif (osflag = c3 | osflag =c4)         .win 95 98
                                PRTOPEN DataCardPrint,"Laser4 Legal","dataprnt.lst"
                        else   .(osflag = c0)         .Don't know prompt for printer
                                PRTOPEN DataCardPrint,"-","dataprnt.lst"
                        endif
                elseif (PrintFlag = 3)  .PDF
.begin patch 1.1.5
.//Patch 1.1.9 Use the Flag to check if pdf has been created
.                              Call      GetPDFPATH
.                              pack      str45 from PDFPATH,"\res\pdf995.ini"
.
.                              call      "GU$INI;WRITE_TO_INI" USING str45:
.                                        "Parameters":
.                                        "ProcessPDF":
..                                        "\\nins1\e\apps\plb\code\pdftest.bat":
.                                        "\\nins1\e\apps\winbatch\Del995flag.exe":
.                                        result
..                              if (result = C0)
..Prepare Flag file
.                                        call      getSDrive
.                                        pack      taskname from PDFPath,"\flag.dat"
.
.                                        prep      tempfile,taskname
..                                        prep      tempfile,Taskname
..                                        write     tempfile,SEQ;"flag set"
.                                        close     tempfile
.                              endif
.//Patch 1.1.9

.First check 995 autolaunch settings
.begin patch 1.31
.begin patch 1.5
.                              Call      PDF995Auto
.                              call      SetPDFFlag
.end patch 1.5

.                             move      "PDF995 Copy",Filename
.                             pack      KeyLocation,"Prepping FIle "
.                             Open      TestFile,"c:\progra~1\\pdf995\res\pdf995.ini"
.                             Prepare   TempFile,"c:\progra~1\\pdf995\res\pdf995.out"
.                             Loop
.                                       move      "PDF995 Copy",Filename
.                                       pack      KeyLocation,"Reading FIle "
.//Patch 1.1.8 Code Modification
..                                      Read      TestFile,seq;STr35
.                                       Read      TestFile,seq;Taskname
.                                       Until Over
..                                      Scan      "Autolaunch=1" in str35
.                                       Scan      "Autolaunch=1" in taskname
.                                       If equal
.                                                 move      Yes to Reset995flag
..                                                reset     str35
..                                                clear     str35
.                                                 reset     taskname
.                                                 clear     taskname                                          
..                                                Move      "Autolaunch=0" to str35
.                                                 Move      "Autolaunch=0" to taskname
.//Patch 1.1.8 Code Modification End                                            
.                                       endif
.                                       move      "PDF995 Copy",Filename
.                                       pack      KeyLocation,"Writing FIle "
.//Patch 1.1.8 Code Modification                                      
..                                      write     tempfile,Seq;str35
.                                       write     tempfile,Seq;taskname
.//Patch 1.1.8 Code Modification                  
.                             Repeat
.                             move      "PDF995 Copy",Filename
.                             pack      KeyLocation,"WEOF FIle "
.                             weof      tempfile,seq
.                             move      "PDF995 Copy",Filename
.                             pack      KeyLocation,"Closing Output FIle "
.                             close     tempfile
.                             move      "PDF995 Copy",Filename
.                             pack      KeyLocation,"Closing Input FIle "
.                             close     testfile
.                             move      "PDF995 Copy",Filename
.                             pack      KeyLocation,"Erasing Save FIle "
.                             Erase     "c:\progra~1\\pdf995\res\pdf995.Sav"
.                             move      "PDF995 Rename",Filename
.                             pack      KeyLocation,"Ren Ini to Sav "
.                             Rename    "c:\progra~1\\pdf995\res\pdf995.ini","c:\progra~1\\pdf995\res\pdf995.Sav"
.                             move      "PDF995 Rename",Filename
.                             pack      KeyLocation,"Ren out to Ini"
.                             Rename    "c:\progra~1\\pdf995\res\pdf995.out","c:\progra~1\\pdf995\res\pdf995.ini"
.begin patch 1.31
.end patch 1.1.5
                        Erase     "c:\work\pdf\datacardR.pdf"
.begin patch 1.5
                        pack    str45,"c:\work\pdf\DatacardR.pdf"
.                        PRTOPEN DataCardPrint,"PDF995","Datacard"
                              
                        PRTOPEN DataCardPrint,"PDF:",str45
.                        pack    str45,"Datacard.pdf"
.end patch 1.5
                endif
.patch1.2.1
          elseif (CallFlag = 2)
                    pack      inpname,DimPtr
                    move dimptr1 to prtfilename
                    PRTOPEN DataCardPrint,"faxfile","Faxfile.prn"

.patch1.2.1
          else

.END PATCH 1.1 ADDED LOGIC
                    CMATCH    B1 TO PROGRAM             .CHAINED FROM DSINIT?
                    IF EOS                       .NO
                              MOVE      "NDAT0002" TO PROGRAM
                              MOVE      "Names In The News" TO COMPNME
                              MOVE      "EXIT" TO PF5
                              MOVE      "datacardR.lst" TO PRTNAME
                    ENDIF
.
                    DISPLAY   *P01:05,"Options     :",Comment:
                              *P01:06,"Input File  :",INPNAME:
                              *P01:09,"Record Count:":
                              *p01:11,"Copies req. :",INITS
.
                    call      trim using inits
                    IF (inits = "")
                              move      c1 to COPY
                    else
                              move      INITS to COPY
                    ENDIF
                    RESET     COMMENT
                    SCAN      "OWNER" IN COMMENT
                    IF EQUAL
                              move      YES to OWNERINFOFLAG
                    ELSE
                              MOVE      NO to OWNERINFOFLAG
                    ENDIF
                    call trim using PRTNAME
                    if (PRTNAME = "")
                              CLOCK     TIMESTAMP,TIMESTAMP
                              PACK      PRINTNAME,LSTNUM,TIMESTAMP
                              PACK      PRTFILENAME,NTWKPATH1,PRINTNAME,".LST"
                    else
                              REP       LOWUP,PRTNAME
                              scan ".LST",PRTNAME
                              if equal
                                        movefptr prtname,n2
                                        sub c1 from n2
                                        reset prtname
                                        setlptr   prtname,n2
                              endif
                              PACK      PRINTNAME,PRTNAME
.begin patch 1.51
.                              PACK      PRTFILENAME,NTWKPATH1,PRINTNAME,".LST"
                              PACK      PRTFILENAME,"c:\work\pdf\",PRINTNAME,".pdf"
                    endif

          reset     Comment
          scan      "PDF",Comment
          if        equal
          move      c3,PrintFlag 
          endif
                                    if (PrintFlag = C0 | PrintFlag = 1)     .Laser3 = Default
                                            if (osflag = c1 | Osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt win2k Xp
                                                    PRTOPEN DataCardPrint,"\\NINS2\laser3 Blankstock","dataprnt.lst"
                                            elseif (osflag = c3 | OSflag =c4)         .win 95 98
                                                    PRTOPEN DataCardPrint,"Laser3 Blankstock","dataprnt.lst"
                                            else   .(osflag = c0)         .Don't know prompt for printer
                                                    PRTOPEN DataCardPrint,"-","dataprnt.lst"
                                            endif
                                    elseif (PrintFlag = 2)  .Laser4
                                            if (osflag = c1 | osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt 2k xp
                                                    PRTOPEN DataCardPrint,"\\NINS2\Laser4 Legal","dataprnt.lst"
                                            elseif (osflag = c3 | osflag =c4)         .win 95 98
                                                    PRTOPEN DataCardPrint,"Laser4 Legal","dataprnt.lst"
                                            else   .(osflag = c0)         .Don't know prompt for printer
                                                    PRTOPEN DataCardPrint,"-","dataprnt.lst"
                                            endif
                                    elseif (PrintFlag = 3)  .PDF
                        Erase     prtfilename
.begin patch 1.5
                        pack    str45,prtfilename
                              
                        PRTOPEN DataCardPrint,"PDF:",str45
                    endif
.                    PRTOPEN DataCardPrint,"faxfile","Faxfile.prn"
.end patch 1.51
.START PATCH 1.1 ADDED LOGIC
          endif
.END PATCH 1.1 ADDED LOGIC
          RESET     COMMENT
          SCAN      "BANNER" IN COMMENT
          IF EQUAL
          PRTPAGE DataCardPrint;*UNITS=*HIENGLISH:
                    *ORIENT=*Portrait:
                    *MarginL=1
                    MOVE "90" TO ROW
                    prtpage DataCardPrint;*p50:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"*********************************************************************************************************************";
                    add       SingleSpaced to Row
                    prtpage DataCardPrint;*p300:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"**USER:** ";
                    prtpage DataCardPrint;*font=CourierNew12,*ll,USER;
                    add       SingleSpaced to Row
                    prtpage DataCardPrint;*p300:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"**Date:** ";
                    prtpage DataCardPrint;*font=CourierNew12,*ll,TODAY;
                    add       SingleSpaced to Row
                    prtpage DataCardPrint;*p300:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"**INPUT FILE:**";
                    prtpage DataCardPrint;*font=CourierNew12,*ll,INPNAME;
                    add       SingleSpaced to Row
                    prtpage DataCardPrint;*p300:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"**COPY:**";
.                   prtpage DataCardPrint;*font=CourierNew12,*ll,INITS;
                    add       SingleSpaced to Row
                    prtpage DataCardPrint;*p50:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"*********************************************************************************************************************";
                    PRTPAGE DataCardPrint;*NEWPAGE
          ENDIF
.
          OPEN      DatCard,INPNAME
          move      c1,ndatpath

ReadRecord
.START PATCH 1.1.3 ADDED LOGIC
          if (RECCOUNT > C0)  .Not the First Page
                    if (CallFlag = 1)
                              if (UsageFlag > 0)
                                        PRTPAGE DataCardPrint;*NEWPAGE
.                                       call      PrintUsageToPFile using LSTNUM,UsageFlag,DataCardPrint
                                        call      PrintUsageToPFile using LSTNUM,UsageFlag,DataCardPrint,Company
                              endif
                    endif
          endif
.END PATCH 1.1.3 ADDED LOGIC
          call      clearvars
          read      datcard,seq;DATVARS
          goto EndDataCARD if over
          ADD       "1",RECCOUNT
          DISPLAY   *P15:9,RECCOUNT;
          if (RECCOUNT > C1)  .Not the First Page
                    PRTPAGE DataCardPrint;*NEWPAGE
          endif
          PRTPAGE DataCardPrint;*UNITS=*HIENGLISH:
                    *ORIENT=*Portrait:
                    *MarginL=1;
          CALL      FIRSTHEADER
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Arrangement
          move      #N9 to row
.Begin patch 1.2
.         prtpage   DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"****ARRANGEMENT*****";
.         add       SingleSpaced to Row
.ARRANGEMENT
          pack      NARRFLD1,"01X",LSTNUM
          move      "D.Load-NARRAIM",Location
          pack      KeyLocation,"Key: ",NARRFLD1
          call      NARRAIM
          if over
.                   prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"N/A";
          Else
                    prtpage   DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"****ARRANGEMENT*****";
                    add       SingleSpaced to Row
          endif
.End patch 1.2
          loop
                    until over
CONTINUEARRANGEMENT
                    call      DataLoadRefArrangement
                    prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,NREFDESC;
                    add       SingleSpaced to Row
                    move      "D.Load-NARRKG",Location
                    pack      KeyLocation,"Key: ",NARRFLD1
                    if (ROW   >= LASTLINE)
                              MOVE      "2" to ArrangementFlag
                              Goto      CheckPlacesRight
                    endif
                    call      NARRKG
          repeat
          MOVE      "3" to ArrangementFlag
Addressing
          add       SingleSpaced to Row
          if (ROW   >= LASTLINE)
                              Goto      CheckPlacesRight
          endif
          pack      NADDFLD1,"01X",LSTNUM
          move      "D.Load-NADDAIM",Location
          pack      KeyLocation,"Key: ",NADDFLD1
          call      NADDAIM
          if not over
                    prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"*****ADDRESSING******";
                    add       SingleSpaced to Row
          endif
          loop
                    until over
CONTINUEADDRESSING
                    call      DataLoadRefAddressing
                    count n4,nREFDESC
.Patch1.2
                    if (str25 <> "")
                              if (n4 > "11")
                                        scan b1,nrefdesc
                                        movefptr nrefdesc to n6
                                        squeeze nrefdesc,str15
                                        setlptr NREFDESC to n6
                                        reset nREFDESC
                                        call trim using nrefdesc
                                        prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,NREFDESC;
                                        prtpage DataCardPrint;*p2050:ROW,*ALIGNMENT=*RIGHT,*font=CourierNew12,*ll,STR25;
                                        add       SingleSpaced to ROW
                                        prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,str15;
                                        add       SingleSpaced to ROW
                              else
                                        prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,NREFDESC;
                                        prtpage DataCardPrint;*p2050:ROW,*ALIGNMENT=*RIGHT,*font=CourierNew12,*ll,STR25;
                                        add       SingleSpaced to ROW
                              endif
                    else
                              prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,NREFDESC;
                              prtpage DataCardPrint;*p2050:ROW,*ALIGNMENT=*RIGHT,*font=CourierNew12,*ll,STR25;
                              add       SingleSpaced to ROW
                    endif
.Patch1.2
                    move      "D.Load-NADDKG",Location
                    pack      KeyLocation,"Key: ",NADDFLD1
                    if (ROW   >= LASTLINE)
                              MOVE      "2" to AddressingFlag
                              Goto CheckPlacesRight
                    endif
.CONTINUEADDRESSING
                    call      NADDKG
          repeat
          move      "3" to AddressingFlag

SOURCE
          add       SingleSpaced to Row
          if (ROW   >= LASTLINE)
                    Goto      CheckPlacesRight
          endif
.SOURCE
          pack      NSRCFLD1,"01X",LSTNUM
          move      "D.Load-NSRCAIM",Location
          pack      KeyLocation,"Key: ",NSRCFLD1
          call      NSRCAIM
          if not over
                    prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"*******SOURCE********";
                    add       SingleSpaced to Row
.                   prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"N/A";
          endif
          loop
CONTINUESOURCE
                    until over
                    call      DataLoadRefSource
                    prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,NREFDESC;
                    add       SingleSpaced to ROW
                    move      "D.Load-NSRCKG",Location
                    pack      KeyLocation,"Key: ",NSRCFLD1
                    if (ROW   >= LASTLINE)
                              MOVE      "2" to SOURCEFlag
                              Goto CheckPlacesRight
                    endif
.CONTINUESOURCE
                    call      NSRCKG
          repeat
          move      "3" to SourceFlag
SELECTIONS
          add       SingleSpaced to Row
          if (ROW   >= LASTLINE)
                    Goto      CheckPlacesRight
          endif
.SELECT
          pack      NSLTFLD1,"01X",LSTNUM
          move      "D.Load-NSLTAIM",Location
          pack      KeyLocation,"Key: ",NSLTFLD1
          call      NSLTAIM
          if not over
                    prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"*****SELECTIONS******";
                    add       SingleSpaced to Row
          endif
          loop
                    until over
CONTINUESELECTIONS
                    call      DataLoadRefSelection
                    prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,NREFDESC;
                    prtpage DataCardPrint;*p2050:ROW,*ALIGNMENT=*right,*font=CourierNew12,*ll,STR25;
                    add       SingleSpaced to ROW
                    move      "D.Load-NSLTKG",Location
                    pack      KeyLocation,"Key: ",NSLTFLD1
                    if (ROW   >= LASTLINE)
                              MOVE      "2" to SelectionsFlag
                              Goto CheckPlacesRight
                    endif
.CONTINUESELECTIONS
                    call      NSLTKG
          repeat
          move      "3" to SelectionsFlag
          move YES to SectionTwo
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Selects
          move      "10" to ROW
          Add       "90"      to ROW
          if (NDATCONV <> "1")
                    move      c3 to SELECTFLAG1
                    move      c3 to SELECTFLAG2
                    Goto Text
          endif
          add       smboxheight to row
.patch1.2Commentout
.         move      c1 to NSELPATH
.         move      c0 to #n4
.Patch1.2CO
                    pack      NSELFLD1,"01X",LSTNUM
                    clear     NSELFLD2
                    move      "D.Load-NSELAIM",Location
                    pack      KeyLocation,"Key: ",NSELFLD1
                    call      NSELAIM
.Patch1.2
          loop
.COmmentout
.                   add       c1 to #n4
.                   move      #n4 to str4
.                   call      zfillit using str4,c0
.                   rep       zfill,str4
.                   pack      nselfld with lstnum,str4
.                   call      nselkey
.Commentout
                    until over
.START PATCH 1.1.1 REPLACED LOGIC
.                   Goto OfficeUse if (NSELSTATUS = "1")                        Skip select if for office use only
                  if (OwnerInfoFlag <> YES)
                              if (NSELSTATUS = "1" | NSELSTATUS = "2")          .1 = Special, 2 = Office Use Only
                                        Goto OfficeUse
                              endif
                    endif
.END PATCH 1.1.1 REPLACED LOGIC
                    Goto OfficeUse if (NSELINACTIVE = "1")            Skip select if inactive
.START PATCH 1.52 REPLACED LOGIC
.                    if ((NSELBASE = "BASE")|(NSELBASE = "SEC."))
                    move      No,Tertbase
                    call      Trim using NselBase
                    if        (NSelBase <> "" & NSelBase <> " " & NSelBase <> "    ")          .overkill
                    move      yes,tertbase
                    endif
                    if (NSELBASE = "BASE" | NSELBASE = "SEC." | tertbase = "Y")
.END PATCH 1.52 REPLACED LOGIC
                              selectlistview.InsertItem giving N8 using NSELINDEX
                              selectlistview.SetItemText giving N1 using N8,NSELNUM,1
                              selectlistview.SetItemText giving N1 using N8,NSELLIST,2
                    else
                              select2listview.InsertItem giving N8 using NSELINDEX
                              select2listview.SetItemText giving N1 using N8,NSELNUM,1
                              select2listview.SetItemText giving N1 using N8,NSELLIST,2
                    endif
.Begin patch 1.34   
                    call      Debug
                    if        (NSELEXC <> "2")       .if the select is not exchange only
                    MOve      Yes,REntFlag           .at least one portion of this list is rentable
                    endif
.End patch 1.34     
OfficeUse
                    move      "D.Load-NSELKG",Location
                    pack      KeyLocation,"Key: ",NSELFLD1
                    call      NSELKG
          repeat
          
          Selectlistview.GetItemCount giving #result
          sub       c1 from #result
          compare   c0,#result
          if less
                    move      "3" to SELECTFLAG1
                    Goto      Text
          else
                    prtpage   DataCardPrint;*p2150:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"Base:";
          endif
          for num9,"0",#result
                    Selectlistview.GetItemText giving str4 using num9,1
                    call      zfillit using str4,c0
                    rep       zfill,str4
                    pack      nselfld with lstnum,str4
                    call      nselkey
                    move      mask13 to dim13a
.DH testing
                    subtract  N10,n10
                    move      nselqty to n10
                    edit      n10 to dim13a
                    call      trim using dim13a
                    prtpage   DataCardPrint;*p3680:ROW,*ALIGNMENT=*RIGHT,*font=CourierNew12,*ll,DIM13A;
                    call      trim using nselsname
                    rep       lowup in NSELSNAME
                    if (NSELNOTES <> "1")
                              prtpage   DataCardPrint;*p3770:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,nselsname;
                    else
                              move YES to SELNOTESFLAG
                              prtpage   DataCardPrint;*p3770:ROW,*ALIGNMENT=*LEFT,*boldon,*font=CourierNew12,*ll,nselsname;
                              prtpage   DataCardPrint;*font=CourierNew12,*ll,"*";
                    endif
                    move      NSELPRICE to dim9a
                    CALL SelectLoadModifier
                    If (NSELEXC= "1")
                              pack str55 with "Exch/",str25
                              prtpage   DataCardPrint;*p7930:ROW,*font=CourierNew12,*ll,*ALIGNMENT=*RIGHT,str55;
                    elseif (NSELEXC= "2")                          .exchange only
.                             pack str55 with "Exch Only/",str25
.DH 28 April 2008 ??
                              pack str55 with "Exchange Only"
                              prtpage   DataCardPrint;*p7930:ROW,*font=CourierNew12,*ll,*ALIGNMENT=*RIGHT,str55;
                    else
                              prtpage   DataCardPrint;*p7930:ROW,*font=CourierNew12,*ll,*ALIGNMENT=*RIGHT,str25;
                    endif
                    add       SingleSpaced to Row
                    if (ROW   >= LASTLINE)
                              MOVE      "2" to SelectFlag1
                              Goto CheckPlacesLeft
                    endif
                    move      row to #n8
CONTINUESELECTS
          repeat
          move      "3" to SELECTFLAG1
Text
.TEXT
.begin patch 1.42
          clear     hold2
          clear     NTXT1TEXT
          clear     line1
          move      c2,Ntxt1path
          for #N2,C1,"5"
                    pack      NTXT1FLD1,LSTNUM,#N2
                    rep       zfill,ntxt1fld1
                    move      "D.Load-NTXT1KEY",Location
                    pack      KeyLocation,"Key: ",NTXT1FLD1
                    call      debug
                    call      NTXT1KEY
                    until     over
                    if        (lstnum <> NTXT1LIST)
                    Break    
                    endif
                    append    NTXT1TEXT,hold2
          repeat                  
                    reset     hold2
                    call      Trim using hold2
                    if (hold2 <> "")
                              movelptr hold2 to n7
                              loop
                                        clear     line1
                                        call      PARSITUP using line1,hold2,C1
                                        movefptr hold2 to n6
                              until (n6 >= n7)
                                        add       singlespaced to row
                              if (ROW   >= LASTLINE)
                                        MOVE      "4" to TEXTFlag
                                        Goto CheckPlacesLeft
                              endif
ContinueText1
                              prtpage   DataCardPrint;*p3030:ROW,*ALIGNMENT=*left,*font=CourierNew12,*ll,line1;
                              add       singlespaced to row
                              repeat

                              if (line1 <> "")
                              prtpage   DataCardPrint;*p3030:ROW,*ALIGNMENT=*left,*font=CourierNew12,*ll,line1;
                              add       singlespaced to row
                              endif
                    endif
.end patch 1.42
          
          clear     hold2
          clear     NTXTTEXT
          move      #n8 to row
          add       SmBoxHeight to row
          add       singlespaced to row
.         for #N1,C1,"9"
          for #N2,C1,"15"
                    pack      NTXTFLD,LSTNUM,#N2
                    rep       zfill,ntxtfld
                    move      "D.Load-NTXTKEY",Location
                    pack      KeyLocation,"Key: ",NTXTFLD
                    call      NTXTKEY
                    until over
                    append    NTXTTEXT,hold2
          repeat
          if (hold2 <> "")
                    IF (ndatconv <> "1")
                              prtpage DataCardPrint;*p2150:ROW,*ALIGNMENT=*left,*font=CourierNew12,*ll,"BASE: ";
                    ENDIF
                    reset     hold2
                    call      Trim using hold2
                    movelptr hold2 to n7
                    loop
                              clear     line1
                              call      PARSITUP using line1,hold2,C1
                              movefptr hold2 to n6
                    until (n6 >= n7)
                              if (ROW   >= LASTLINE)
                                        MOVE      "2" to TEXTFlag
                                        Goto CheckPlacesLeft
                              endif
ContinueText
                              rep       lowup in line1
                              prtpage   DataCardPrint;*p3030:ROW,*ALIGNMENT=*left,*font=CourierNew12,*ll,line1;
                              add       singlespaced to row
                    repeat
                    if (line1 <> "")
                              rep       lowup in line1
                              prtpage   DataCardPrint;*p3030:ROW,*ALIGNMENT=*left,*font=CourierNew12,*ll,line1;
                              add       singlespaced to row
                    endif
          endif
          MOVE      "3" to TEXTFlag
          if (SELECTFLAG2 = YES)
                    Goto      CheckPlacesRight
          endif
          add       singlespaced to row
          if (ROW   >= LASTLINE)
                    Goto CheckPlacesLeft
          endif
AdditionalSelects
          clear     num9
          Select2listview.GetItemCount giving #result
          sub       c1 from #result
          compare   c0,#result
          if less
                    move      "3" to SELECTFLAG2
                    Goto      EndDetail
          endif
          for num9,"0",#result
                    Select2listview.GetItemText giving str4 using num9,1
                    call      zfillit using str4,c0
                    rep       zfill,str4
                    pack      nselfld with lstnum,str4
                    call      nselkey
                    move      mask13 to dim13a
.dh testing
                    sub       n10,n10
                    move      nselqty to n10
                    edit      n10 to dim13a
                    call      trim using dim13a
                    prtpage   DataCardPrint;*p3680:ROW,*ALIGNMENT=*RIGHT,*font=CourierNew12,*ll,DIM13A;
                    call      trim using nselsname
                    rep       lowup in NSELSNAME
                    if        (NSELNOTES <> "1")
                              prtpage   DataCardPrint;*p3770:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,nselsname;
                    else
                              move YES to SELNOTESFLAG
                              prtpage   DataCardPrint;*p3770:ROW,*ALIGNMENT=*LEFT,*boldon,*font=CourierNew12,*ll,nselsname;
                              prtpage   DataCardPrint;*font=CourierNew12,*ll,"*";
                    endif
                    IF (NSELPRICE <> C0)
                              move nselprice to dim9a
                              call trim using dim9a
                              call SelectLoadModifier
                    ELSE
                              Clear STR25
                    ENDIF
                    prtpage   DataCardPrint;*p7930:ROW,*font=CourierNew12,*ll,*ALIGNMENT=*RIGHT,str25;
                    add       SingleSpaced to Row
                    if (ROW   >= LASTLINE)
                              MOVE      "2" to selectFlag2
                              Goto CheckPlacesLeft
                    endif
ContinueSelects2
          repeat
test
          move      "3" to selectFlag2
          Goto      EndDetail
          If (SectionOne = YES & SectionTwo = YES)
.begin patch 1.34             
                    Call      CheckRentMin
.end patch 1.34               
                    Goto READRECORD
          Else
                    Goto CheckPlacesLeft
          Endif

ENDDATACARD
          PRTCLOSE DataCardPrint
                              move      "3000",str4      .=30sec
                              call      Waitin using str4
.                              pause     "60"
.START PATCH 1.1 ADDED LOGIC
.begin patch 1.51
          if (CallFlag = 1 or CallFlag = 0)
.          if (CallFlag = 1)
.enf patch 1.51
                    if (PrintFlag = 3)  .PDF
.It takes some time for the file to be created, so we must check
.Allow 20 seconds

//Patch 1.1.9 Use the Flag to check if pdf has been created
.begin patch 1.40
.                    call      getSDrive
                    if        (ClntServFlag = c1)
.begin patch 1.5
.                               call     GetPDFPath
.                               pack      APIFileName from PDFPATH,"\flag.dat",hexzero
..                              pack      APIFileName,SysDRive,"\progra~1\pdf995\flag.dat",hexzero
.                              
.                              loop
.                                        call      FindFirstFile
.                              until (APIResult = 0 | APIResult = hexeight)
.                                        move      "100",str3
.                                        call      Waitin using str3
..                                        pause     "1"
.                              repeat
.                              pause     "5"
.                              pack      Filestring,SysDrive,"\progra~1\pdf995\flag.dat"
.                              erase     "c:\progra~1\pdf995\flag.dat"
.                              erase     Filestring
                    Else
.                              pack      APIFileName,"!",SysDRive,"\progra~1\pdf995\flag.dat",hexzero
.                               call     GetPDFPath
.                              pack      APIFileName,"!",PDFPATH,"\flag.dat",hexzero
.                              loop
.                                        call      FindFirstFile
.                              until (APIResult = 0 | APIResult = hexeight)
.                                        move      "100",str3
.                                        call      Waitin using str3
..                                        pause     "1"
.                              repeat
.                                        move      "500",str3
.                                        call      Waitin using str3
.                              pause     "5"
.                              pack      Filestring,"!",SysDrive,"\progra~1\pdf995\flag.dat"
.                              erase     "c:\progra~1\pdf995\flag.dat"
.                              erase     Filestring
.                              erase     "!c:\progra~1\pdf995\flag.dat"
.end patch 1.5
                    endif
.                              pack      APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
.                              loop
.                                        call      FindFirstFile
.                              until (APIResult = 0 | APIResult = hexeight)
.                                        pause     "1"
.                              repeat
.                              pause     "5"
.begin patch 1.5
.                              pack      APIFileName,PDFPATH,"\flag.dat"
.
.                              erase     APIFILENAME
.end patch 1.5
.end patch 1.40
//Patch 1.1.9
//Patch 1.1.9 Comment Out - This section is replaced with the logic above
.                             clock   timestamp,timestamp1
.                             move    timestamp1,time1
.                             loop
.                                       move      C0,N1
.                                       trap      IOTrap if IO
.                                       open      tempfile,str45
.                                       trapclr   IO
.                                       if (N1 = C0)
.                                                 close     tempfile
.                                                 break
.                                       endif
.                                       clock   timestamp,timestamp
.                                       move    timestamp,time2
.                                       sub     time1,time2,time3
.                                       if (time3 > 2000) .20 Seconds Maximum
.                                                 break
.                                       endif
.                             repeat
.//Patch 1.1.9
.begin patch 1.35
.begin patch 1.39
          move      "300",str4                   .3 seconds
          call      Waitin using str4
.          Pause     "30"
.begin patch 1.40
                    call      testclient
                    if        (ClntServFlag = c1)                         .we are using the client need to move the file for email
                    move      "1000",str4
                    call      Waitin using str4
.                    Pause     "10"
.begin patch 1.5
.                    pack      MailAttach from "c:\work\pdf\",str45              ."
                    pack      MailAttach from str45              ."
.                    pack      taskname from "!c:\work\pdf\",str45              ."
.end patch 1.5
                    pack      taskname from "!",str45              ."
                    move      "500",str3
                    call      Waitin using str3
.                    Pause     C5
.Copy the file using 10 blocks
                    FINDFILE  Taskname,FILESIZE=Size
                    DIV        "10",Size
.Initiate the copy
                    copyfile  taskname,mailattach:          
                    BlksLeft,BlockSize=Size
.*
.Loop copying all blocks
                    LOOP
  
                    WHILE     (BlksLeft)
.Next block
                    COPYNEXT   BlksLeft

                    REPEAT
  
 

.                    Pause     C5
.                    copyfile  taskname,mailattach          
                    move      "500",str3              .5 seconds
                    call      Waitin using str3
.                    Pause     C5
                    Erase     Taskname                    
                    endif
.                              pack      MailAttach from "c:\work\pdf\",str55              ."
.end patch 1.40
.                    move      "3000",str4         .30 seconds
.                    call      Waitin using str4
.                    Pause     C30
CheckFile
.begin patch 1.5
.          pack      MailAttach from "c:\WORK\pdf\",Str45                  ."
          pack      MailAttach from Str45                  ."
.end patch 1.5
          Move      MailAttach,Str55
          trap      WaitForEnd giving error if IO
          open      FileCheck,STR55,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO
.end patch 1.39


                              Move      "Here is your PDF datacard",MailSubjct
.                             move    "Here is your PDF File",SmtpSubject Subject
                              Clear     MailBody
                              append    str45,mailbody
                              append    CRLF,Mailbody
                              Reset     MailBOdy
.argh what is going on        
.begin patch 1.51
          if (CallFlag = 1)
                              call      Trim using Dimptr2
                              if        (dimptr2 = "")
                              pack      taskname from portn,"I lost track of who you are - Error",dimptr2,"file in c:\work\pdf\datacard.pdf"
                              Alert     Stop,Mess,F1,taskname
                              endif
.argh what is going on        
                              
                              pack      Mailto from DimPtr2,"@nincal.com"
                              pack      MailFrom from DimPtr2,"@nincal.com"
          elseif (CallFlag = 0)
                              pack      Mailto from user,"@nincal.com"
                              pack      MailFrom from user,"@nincal.com"
          endif
.end patch 1.51
.begin patch 1.39
.                              pack      MailAttach from "C:\WORK\pdf\",Str45                  ."
.end patch 1.39
..   Set the text message that is send with the attachments
.                             move    str45,SmtpTextMessage(1)   Array <Text message >
.                             move    "1",SmtpTextIndexLast                               Index to last entry in TextMessage array
.                             move    "NTS4",SmtpEmailServer                   Address of email serverc
.                             clear   smtpemailaddress
.                             append  DimPtr2,SmtpEmailAddress
.                             append  "@nincal.com",SmtpEmailAddress
.                             reset   smtpemailaddress
.                             move    DimPtr2,SmtpUserName                                User name
.                             move    DimPtr2,SmtpUserFullName                                User name
..   Set the destinations of the email. Max 100 (Mime spec)
.                             move    smtpemailaddress,SmtpDestinations(1,1)
.                             move    DimPtr2,SmtpDestinations(1,2)
.                             move    "1",SmtpDestIndexLast                          originators UserName
.                             move    str45,SmtpAttachments(1,1)                     Attached file name
.                             move    "C:\WORK\pdf",SmtpAttachments(1,2)           Path to attached file name
.                             move    "1",SmtpAttIndexLast                                Index to last entry - Only 1 entry
.                             move    "C:\work\eMail.Log",SmtpLogFile          Path/filename to Log all socket read/writes
.                             clear   SmtpLogFile                                         'Clear' disables the LogFile
.                             move    "1",SmtpProgress                                    Enable progress bars
.                             call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
.                             if not equal
.                                       pack    Mess,"Result Code ",SmtpResult," - ",SmtpResultText,NewLine:
.                                                 "Status Code ",SmtpStatus," - ",SmtpStatusText
.                                       move    "PDF File not found",SmtpSubject Subject
.                                       move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
.                                       call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
..                                  Alert  Stop,Mess,F1,"PL/B emailer - ERROR"
.                             else
.                                   Alert  Stop,"eMail transmitted OK",F1,"PL/B emailer"
.                             endif
.                              move      "1000",str4         .10 seconds
.                              call      Waitin using str4
.                              pause     "10"
                              Call      SendMail
.end patch 1.35
.Clean up afterwards
.                           move    "                                        ",APIFileName
.                           clear   APIFileName
.                           pack    APIFileName,"C:\WORK\",str45,hexzero
.                           call    DeleteFile
.                           if (APIResult = 0 | APIResult = hexeight)
.                           endif
.                              erase     str55
                    endif
                    close     Datcard
                    erase     inpname
.begin patch 1.1.5
.begin patch 1.31
.begin patch 1.5
.                    Call      PDF995Auto0
.end patch 1.5
.end patch 1.31
.end patch 1.1.5
.Begin patch 1.51
          if (CallFlag = 0)
          SHUTDOWN   "CLS"
          endif
.end patch 1.51
                    return
          else
.END PATCH 1.1 ADDED LOGIC
.                   pack      APIFileName,"\\nts0\d\data\fax\faxfile.prn",hexzero
                    pack      APIFileName,"c:\work\faxfile.prn",hexzero
                    call      FindFirstFile
                    if (APIResult <> 0 & APIResult <> hexeight)
.                             copyfile "\\nts0\d\data\fax\faxfile.prn",PRTFILENAME
                              pack      str45,"c:\work\faxfile.prn"
.                             pack      str45,"\\nts0\d\data\fax\faxfile.prn"
                              copyfile str45,PRTFILENAME
                              erase     str45
                    endif
          endif
          Erase     mailattach

          return if (callflag = 2)
   SHUTDOWN   "CLS"
          STOP

IOTrap
          move      C1,N1
          return

DataLoadRefAddressing
          pack      NREFFLD,"A",NADDNUM
          move      "D.Load1-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          if (NADDPRICE = C0)
                    clear     str9
          else
                    unpack    NADDPRICE,str5,str3
                    call      FormatNumeric using str5,str6
                    pack      str9,str6,str3
          endif
          pack      NMODFLD,NADDDESC
          rep       zfill,NMODFLD
          move      "D.Load2-NMODKEY",Location
          pack      KeyLocation,"Key: ",NMODFLD
          call      NMODKEY
          call      Trim using NMODDESC
          pack      str25,str9,NMODDESC
          return
DataLoadRefArrangement
          pack      NREFFLD,"R",NARRNUM
          move      "D.Load4-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          return
DataLoadRefSource
          pack      NREFFLD,"S",NSRCNUM
          move      "D.Load5-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          move      C0,N3
          move      NSRCPER,N3
          if (N3 > C0)
                    move      N3,NSRCPER
                    call      Trim using NSRCPER
                    pack      str4,NSRCPER,PRC
          endif
          return
DataLoadRefSelection
          pack      taskname,NSLTVARS
          pack      str6,"LLL",NSLTNUM
          pack      NREFFLD,"L",NSLTNUM
          move      "D.Load2-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          if (NSLTPRICE = C0)
                    clear     str9
          else
                    unpack    NSLTPRICE,str5,str3
                    call      FormatNumeric using str5,str6
                    pack      str9,str6,str3
          endif
          pack      NMODFLD,NSLTDESC
          rep       zfill,NMODFLD
          move      "D.Load3-NMODKEY",Location
          pack      KeyLocation,"Key: ",NMODFLD
          call      NMODKEY
          call      Trim using NMODDESC
          pack      str25,str9,NMODDESC
          call      FormatNumeric using NSLTQTY,str11
          PACK TASKNAME WITH NREFDESC,B8,STR25
          return

DataLoadRefNetName
          pack      taskname,NETNAME
          pack      NREFFLD,NETNAME
          move      "D.Load2-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          return
DataLoadRefSAMPLE
          pack      taskname,SAMPLE
          pack      NREFFLD,SAMPLE
          move      "D.Load2-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          return
Header
          PRTPAGE DataCardPrint;        *NEWPAGE:
                    *UNITS=*HIENGLISH:
                    *ORIENT=*Portrait:
                    *MarginL=1

FIRSTHEADER
          add       c1 to PageNum
.Patch1.2
        if (OwnerInfoFlag = YES)
                PRTPAGE DataCardPrint;*PICTRECT=*OFF,*PICT=2655:8575:2100:7600:pict1
        endif
.Patch1.2
          move      "10" to ROW
          prtpage   DataCardPrint;*pensize=15,*RECT=ROW:LgBoxHeight:1:7980
          add       SmBoxHeight to Row,n9
          prtpage   DataCardPrint;*p7330:row,*pensize=15,*line=7330:n9;
          add       SmBoxHeight to Row
          prtpage   DataCardPrint;*p0:row,*pensize=15,*line=7980:row;
          prtpage   DataCardPrint;*p2100:row,*pensize=15,*line=2100:9900;
          add       SmBoxHeight to Row
          prtpage   DataCardPrint;*p0:row,*pensize=15,*line=2100:row;
          add       SmBoxHeight to Row
          prtpage   DataCardPrint;*p0:row,*pensize=15,*line=2100:row;
          add       SmBoxHeight to Row
          prtpage   DataCardPrint;*p0:row,*pensize=15,*line=2100:row;
.START PATCH 1.1.7 ADDED LOGIC
          call trim using CLEANCDE
          if ((CLEANCDE <> "0000")&(CLEANCDE <> ""))
                    add       SmBoxHeight to Row
                    prtpage   DataCardPrint;*p0:row,*pensize=15,*line=2100:row;
          endif
.END PATCH 1.1.7 ADDED LOGIC
.begin patch 1.34
.more notes
          If        (rentFlag = Yes & (ELSTCDE = "C" or Elstcde = "P"))
.begin patch 1.53
          prtpage DataCardPrint;*p2150:9250,*ALIGNMENT=*LEFT,*font=CourierNew8,*ll,"Mailers choosing not to make their list available may be subject to a nonreciprocal fee on rentals.",*boldoff;       
.end patch 1.53
          prtpage DataCardPrint;*p2150:9350,*ALIGNMENT=*LEFT,*font=CourierNew8,*ll,"*Any quantity below minimum is subject to a flat fee plus applicable base, select and additional",*boldoff;
          prtpage DataCardPrint;*p2150:9450,*ALIGNMENT=*LEFT,*font=CourierNew8,*ll,"charges. On quantities below 5,000 a $100/flat fee is imposed, Below 1,000 the fee is $200/flat.",*boldoff;
          endif

.end patch 1.34
Notes
          prtpage DataCardPrint;*p2150:9550,*ALIGNMENT=*LEFT,*font=CourierNew8,*ll,"Full payment is required on orders not cancelled prior to the maildate. If exchange, status will",*boldoff;
          prtpage DataCardPrint;*p2150:9650,*ALIGNMENT=*LEFT,*font=CourierNew8,*ll,"remain as ordered. Orders cancelled by mailer prior to mail date are subject to a $100.00/flat",*boldoff;
          prtpage DataCardPrint;*p2150:9750,*ALIGNMENT=*LEFT,*font=CourierNew8,*ll,"cancellation fee plus running and shipping charges.",*boldoff;
.         prtpage DataCardPrint;*p2150:9700,*ALIGNMENT=*LEFT,*font=TimesNew5,*ll,*boldon,"Orders cancelled by mailer prior to mail date are subject to a $75.00 processing fee.",*boldoff;

Footer
.DMA Section
          prtpage DataCardPrint;*p5:10010,*ALIGNMENT=*LEFT,*font=CourierNew8,*ll,*boldon,"Direct",*boldoff;
          prtpage DataCardPrint;*p5:10130,*ALIGNMENT=*LEFT,*font=CourierNew8,*ll,*boldon,"Marketing",*boldoff;
          prtpage DataCardPrint;*p5:10250,*ALIGNMENT=*LEFT,*font=CourierNew8,*ll,*boldon,"Association",*boldoff;

.START PATCH 1.1.4 REPLACED LOGIC
.         prtpage DataCardPrint;*p4000:9960,*ALIGNMENT=*RIGHT,*font=TimesNew16,*ll,*boldon,"Names ",*boldoff;
.         prtpage DataCardPrint;*p4000:9960,*ALIGNMENT=*LEFT,*font=TimesNew16I,"in the News ",*boldoff;
.         prtpage DataCardPrint;*p3000:10220,*pensize=15,*line=5300:10220;
.         prtpage DataCardPrint;*p4200:10240,*font=TimesNew11,*ALIGNMENT=*CENTER,*ll,"C A L I F O R N I A   I N C .";
..NIN Contact Info
.         prtpage DataCardPrint;*p4000:10480,*ALIGNMENT=*CENTER,*font=CourierNew8,*ll,"1300 Clay Street - Oakland, CA 94612-1429 * 415-989-3350 * Fax 415-433-7796 ",*boldoff;
.         prtpage DataCardPrint;*p7950:10450,*ALIGNMENT=*RIGHT,*font=TimesNew11,*ll,PageNum,*boldoff;
..........................
.Getting the Logo to fit in this small amount of space was a bear!!  Be careful if you decide to modify.
.begin patch 1.32
.begin patch 1.38
.          if        (Company = c2)
.          prtpage   DataCardPRint;*p=3625:10010,*Overlayon:
.                    *font=fontPL8b,"Pacific Lists, Inc.":
.                    *p=3625:10210,*font=fontPL5,"1300 Clay St. 11th Floor":
.                    *p=3625:10290,"Oakland, CA 94612-1429":
.                    *p=3625:10370,"415-945-9450 ","·"," Fax 415-945-9451":
.                    *p=3625:10450,"A Division of Names in the News",*Overlayoff
.
.          Else
          prtpage   DataCardPrint;*units=*HIENGLISH,*Pictrect=*off,*PICT=9910:10695:3000:8000:NINLogo
.          Endif
.end patch 1.38
.end patch 1.32
.END PATCH 1.1.4 REPLACED LOGIC
SideHeader
          move      "10" to ROW
          Add       "90"      to ROW
          prtpage DataCardPrint;*p7950:ROW,*ALIGNMENT=*RIGHT,*font=CourierNew12,*ll,LSTNUM;
          call      trim using mlstname
          IF (OWNERINFOFLAG = YES)
                    prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,MLSTNAME;
.                   prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"INTERNAL COPY: ";
.                   prtpage DataCardPrint;*font=CourierNew12,*ll,MLSTNAME;
          ELSE
                    prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,MLSTNAME;
          ENDIF
          IF (pagenum > c1)
                    prtpage DataCardPrint;*font=CourierNew12,*ll," (Cont.)";
                    IF (NDATOFF = "1")
                              prtpage DataCardPrint;*font=CourierNew12,*ll," - Office Use Only";
                    ENDIF
          ELSE
                    if (NDATOFF = "1")
                              prtpage DataCardPrint;*font=CourierNew12,*ll," - Office Use Only ";
                    ENDIF
          ENDIF

.WithDrawn / Temporary Withdrawn????
          if (STATUS = "W")
                    prtpage DataCardPrint;*p7250:ROW,*ALIGNMENT=*RIGHT,*font=CourierNew12,*ll,"**WITHDRAWN**";
          elseif (STATUS = "T")
                    prtpage DataCardPrint;*p7250:ROW,*ALIGNMENT=*RIGHT,*font=CourierNew12,*ll,"**TEMPWITH**";
          else
.No then is it New and/or exclusive?
                    if (NLSTCDE = YES)
.begin patch 1.32
.                             if (ELSTCDE = "C")
                              if (ELSTCDE = "C" or Elstcde = "P")
.begin patch 1.37
.                                        prtpage DataCardPrint;*p7250:ROW,*ALIGNMENT=*RIGHT,*font=CourierNew12,*ll,"*NEW/EXCLUSIVE*";
.end patch 1.37
                              else
                                        prtpage DataCardPrint;*p7250:ROW,*ALIGNMENT=*RIGHT,*font=CourierNew12,*ll,"**NEW**";
                              Endif
                    else
.                             if (ELSTCDE = "C")
                              if (ELSTCDE = "C" or Elstcde = "P")
.end patch 1.32
.begin patch 1.37
.                                        prtpage DataCardPrint;*p7250:ROW,*ALIGNMENT=*RIGHT,*font=CourierNew12,*ll,"*EXCLUSIVE*";
.end patch 1.37
                              Endif
                    endif
          endif

          unpack    REVDATE,str2,yy,mm,dd
          move      MM to n2
          load      str12 with n2,"January","February","March","April","May","June","July","August","September","October","November","December"
          pack      str30,str12,b1,dd,comma,b1,str2,yy
.START PATCH 1.1.7 REPLACED LOGIC
.         add       SmBoxHeight to Row
          add       "280" to Row
          prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"Last Update:";
          add       "154" to Row
.END PATCH 1.1.7 REPLACED LOGIC
          prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,str30;
.START PATCH 1.1.7 REPLACED LOGIC
.         Call RightPadding
.         add       SmBoxHeight to Row
          call trim using CLEANCDE
          if ((CLEANCDE <> "0000")&(CLEANCDE <> ""))
                    add       "188" to Row
                    prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"Updated: ";
                    add       "158" to Row
                    if (CLEANCDE = "C002")
                              call      Trim using CLNINFO
                              prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,CLNINFO;
                    else
                              Call      DataLoadRefClean
                              call      Trim using NREFDESC
                              prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,NREFDESC;
                    endif
                    move      "1150" to Row
          else
                    Call      RightPadding
                    add       SmBoxHeight,Row
          endif
.END PATCH 1.1.7 REPLACED LOGIC
.START PATCH 1.1.1 REPLACED LOGIC
.         prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"Sex: ";
.begin patch 1.2
          call      debug
                    Clear     str25
              if              (NdatFem > 0)
                    move                NDatFem,N3
                    MOVe                N3,str3
                    append    "Women ",str25
                    append    str3,str25
                    append    "%",Str25
                    append    " ",Str25
             endif
              if              (NdatMen > 0)
                    move                NDatMen,N3
                    MOVe                N3,str3
                    append    "Men ",str25
                    append    str3,str25
                    append    "%",Str25
          endif
          reset     str25
          if        (NdatFem > 0 or NdatMen > 0)
                    prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,Str25;
                    Else
                    prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"Gender: ";
                    call trim using sex
                    prtpage DataCardPrint;*font=CourierNew12,*ll,SEX;
                              endif
.         prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"Gender: ";
.END PATCH 1.1.1 REPLACED LOGIC
.         call trim using sex
.         prtpage DataCardPrint;*font=CourierNew12,*ll,SEX;
.end patch 1.2
          add       SmBoxHeight to Row
.begin patch 1.34
          Move      Row,REntRow                .we don't know if rental yet :(
          prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"Minimum: ";

.end patch 1.34
          call trim using min
          prtpage DataCardPrint;*font=CourierNew12,*ll,MIN;
          add       SmBoxHeight to Row
          add       SmBoxHeight to Row
          move      ROW to #N9
          return
.begin patch 1.34             
CheckRentMin
          If        (rentFlag = Yes & (ELSTCDE = "C" or Elstcde = "P"))
                    If        (Pagenum = c1)
                    prtpage   DataCardPRint;*p2090:REntRow,*boldon,"*",*boldoff;
                    endif
.begin patch 1.53
          prtpage DataCardPrint;*p2150:9250,*ALIGNMENT=*LEFT,*font=CourierNew8,*ll,"Mailers choosing not to make their list available may be subject to a nonreciprocal fee on rentals.",*boldoff;       
.end patch 1.53
          prtpage DataCardPrint;*p2150:9350,*ALIGNMENT=*LEFT,*font=CourierNew8,*ll,"*Any quantity below minimum is subject to a flat fee plus applicable base, select and additional",*boldoff;
          prtpage DataCardPrint;*p2150:9450,*ALIGNMENT=*LEFT,*font=CourierNew8,*ll,"charges. On quantities below 5,000 a $100/flat fee is imposed, Below 1,000 the fee is $200/flat.",*boldoff;
          endif
          Return
.end patch 1.34               


ENDDETAIL
          add       SingleSpaced to Row
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PrintUnitData
          if (UNITDATA <> "")
                    if (ROW   >= LASTLINE)
                              Goto CheckPlacesLeft
                    endif
                    unpack unitdata,dimtext1,dimtext2,dimtext3,dimtext4
                    clear #n2
                    for #n2,"1","5"
                              clear dimtext5
                              load     dimtext5,#n2,dimtext1,dimtext2,dimtext3,dimtext4,dimtext6
                              pack str2,carr,b1
                              rep str2,DIMTEXT5
                              call trim using dimtext5
                              if (DIMTEXT5 <> "")
                                        prtpage DataCardPrint;*p3030:ROW,*ALIGNMENT=*left,*font=CourierNew12,*ll,DIMTEXT5;
                                        add singlespaced to row
                                        if (ROW   >= LASTLINE)
                                                  move "2" to UnitFlag
                                                  goto checkplacesleft
                                        endif
                              endif
PrintUnitDataContinue
                repeat
          endif
          move "3" to UnitFlag
          add singlespaced to row

PrintCleanCode
.START PATCH 1.1.7 REPLACED LOGIC - MOVED TO DIFFERENT LOCATION
.         call trim using CLEANCDE
.         if ((CLEANCDE <> "0000")&(CLEANCDE <> ""))
.                   if (ROW   >= LASTLINE)
.                             Goto CheckPlacesLeft
.                   endif
.                   prtpage DataCardPrint;*p2150:ROW,*ALIGNMENT=*left,*font=CourierNew12,*ll,"UPDATED: ";
.                   if (CLEANCDE = "C002")
.                             prtpage DataCardPrint;*p3030:ROW,*font=CourierNew12,*ll,CLNINFO;
.                             add       singlespaced to row
.                   else
.                             Call      DataLoadRefClean
.                             prtpage DataCardPrint;*p3030:ROW,*font=CourierNew12,*ll,NREFDESC;
.                             add       singlespaced to row
.                   endif
.         endif
.         move "3" to CleanCdeFlag
.END PATCH 1.1.7 REPLACED LOGIC - MOVED TO DIFFERENT LOCATION
          if (ROW   >= LASTLINE)
                    Goto CheckPlacesLeft
          endif
PrintNetName
          call trim using NETNAME
          if ((NETNAME <> "0000")&(NETNAME <> ""))
                    prtpage DataCardPrint;*p2150:ROW,*ALIGNMENT=*left,*font=CourierNew12,*ll,"NET NAMES: ";
                    if (NETNAME = "N002")
                              prtpage DataCardPrint;*p3030:ROW,*font=CourierNew12,*ll,NETINFO;
                              add       singlespaced to row
                    else
                              Call      DataLoadRefNetName
                              prtpage DataCardPrint;*p3030:ROW,*font=CourierNew12,*ll,NREFDESC;
                              add       singlespaced to row
                    endif
          endif
          move "3" to NetNameFlag
PrintSample
          call trim using SAMPLE
          if ((SAMPLE <> "0000")&(SAMPLE <> ""))
                    if (ROW   >= LASTLINE)
                              Goto CheckPlacesLeft
                    endif
                    prtpage DataCardPrint;*p2150:ROW,*ALIGNMENT=*left,*font=CourierNew12,*ll,"SAMPLE: ";
                    Call      DataLoadRefSAMPLE
                    prtpage DataCardPrint;*p3030:ROW,*font=CourierNew12,*ll,NREFDESC;
                    add       singlespaced to row
          endif
          move "3" to SampleFlag
PrintDelivery
          call trim using DELCODE
          if ((DELCODE <> "0000")&(DELCODE <> ""))
                    if (ROW   >= LASTLINE)
                              Goto CheckPlacesLeft
                    endif
                    prtpage DataCardPrint;*p2150:ROW,*ALIGNMENT=*left,*font=CourierNew12,*ll,"DELIVERY: ";
                    Call                DataLoadRefDelivery
                    prtpage DataCardPrint;*p3030:ROW,*font=CourierNew12,*ll,NREFDESC;
                    add       singlespaced to row
          endif
          move "3" to DeliveryFlag
AddNotes
          if (SelNotesFlag = YES)
                    if (ROW   >= LASTLINE)
                              Goto CheckPlacesLeft
                    endif
                    prtpage DataCardPrint;*p2150:ROW,*ALIGNMENT=*left,*font=CourierNew12,*boldon,*ll,"*See list description for additional information regarding";
                    add       singlespaced to row
ContinueAddNotes
                    prtpage DataCardPrint;*p2150:ROW,*ALIGNMENT=*left,*font=CourierNew12,*boldon,*ll," this select.",*boldoff;
                    add       singlespaced to row
          endif
          move "3" to ADDNOTESFLAG
          if (OWNERINFOFLAG = YES)
                    if (ROW   >= LASTLINE)
                              Goto CheckPlacesLeft
                    endif
                    Goto      OWNERINFO
          else
                    move YES to SectionOne
          endif
.
          If (SectionOne = YES & SectionTwo = YES)
.begin patch 1.34             
                    Call      CheckRentMin
.end patch 1.34               
                    Goto READRECORD
          Else
                    Goto CheckPlacesLeft
          Endif
.This code is touchy.  CheckPlacesRight it a checkpoint to see what sections have finished in the body of the datacard and which have not.  If a section has not been started
.it is given a 1.  if it has been started but not finished it is given a 2.  If a section has completed is it given a three.  It has a very distinct order
.so be carefull when changing!!!!!!
CheckPlacesRight
          if (SelectFlag1 = "1")
                    goto Selects
          elseif (SelectFlag1 = "2")
                    goto ContinueSelects
          endif
          if (TextFlag = "1")
                    Call RightPadding
                    goto      Text
          elseif (TextFlag = "2")
                    Call RightPadding
                    goto ContinueText
          elseif (TextFlag = "4")
                    Call RightPadding
                    goto ContinueText1
          endif
          if (SelectFlag2 = "1")
                    Call RightPadding
                    goto      AdditionalSelects
          elseif (SelectFlag2 = "2")
                    Call RightPadding
                    goto ContinueSelects2
          endif
          if (UnitFlag = "1")
                    Call RightPadding
                    Goto PrintUnitData
          elseif (UnitFlag = "2")
                    Call RightPadding
                    Goto PrintUnitDataContinue
          endif
          if (CleanCdeFlag = "1")
                    Call RightPadding
                    Goto PrintCleanCode
          endif
          if (NetNameFlag = "1")
                    Call RightPadding
                    Goto PrintNetName
          endif
          if (SampleFlag = "1")
                    Call RightPadding
                    Goto PrintSample
          endif
          if (DeliveryFlag = "1")
                    Call RightPadding
                    Goto PrintDelivery
          endif
          if (AddNotesFlag = "1")
                    Call RightPadding
                    Goto AddNotes
          elseif (AddNotesFlag = "2")
                    Call RightPadding
                    Goto ContinueAddNotes
          endif
          if (OWNERINFOFLAG = YES)
                    if (OwnerPrintFlag = c0)
                              Call RightPadding
                              Goto OwnerInfo
                    else
                              Branch OwnerPrintFlag,Own1,Own2,Own3,Own4,Own5,Own6,Own7,Own8,Own9,own10
                    endif
          endif
          move yes to sectionone
          If (SectionOne = YES & SectionTwo = YES)
.begin patch 1.34             
                    Call      CheckRentMin
.end patch 1.34               
                    GOTO READRECORD
          Else
                    Goto CheckPlacesLeft
          Endif
.This code is touchy.  CheckPlacesLeft it a checkpoint to see what sections have finished in the charges section of the datacard and which have not.  If a section has not been started
.it is given a 1.  if it has been started but not finished it is given a 2.  If a section has completed is it given a three.  It has a very distinct order
.so be carefull when changing!!!!!!
CheckPlacesLeft
          If (SectionOne <> YES | SECTIONTWO <> YES)
                    Call Header
          endif
          if (ArrangementFlag = "1")
                    goto Arrangement
          elseif (ArrangementFlag = "2")
                    call      NARRKG
                    if not over
                              move      #N9 to row
                              prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"ARRANGEMENT (Cont.)";
                              add       SingleSpaced to Row
                              goto Continuearrangement
                    else
                              Move      "3" to ArrangementFlag
                    endif
          endif
          if (AddressingFlag = "1")
                    Goto Addressing
          elseif (AddressingFlag = "2")
                    call  NADDKG
                    if not over
                              prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"*ADDRESSING(Cont.)*";
                              add       SingleSpaced to Row
                              goto ContinueAddressing
                    else
                              Move      "3" to AddressingFlag
                              Goto Source
                    endif
          endif
          if (SourceFlag = "1")
                    Goto Source
          elseif (SourceFlag = "2")
                    call      NSRCKG
                    if not over
                              prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"*****SOURCE(Cont.)****";
                              add       SingleSpaced to Row
                              goto ContinueSource
                    else
                              move      "3" to SourceFlag
                    endif
          endif
          if (SelectionsFlag = "1")
                    Goto Selections
          elseif (SelectionsFlag = "2")
                    call      NSLTKG
                    if not over
                              prtpage DataCardPrint;*p80:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"*SELECTIONS(Cont.)*";
                              add       SingleSpaced to Row
                              goto ContinueSelections
                    else
                              move      "3" to SelectionsFlag
.                             goto ENDDATACARD
.begin patch 1.34             
                    Call      CheckRentMin
.end patch 1.34               
                              GOTO READRECORD
                    endif
          endif
          Move      YES to SectionTwo
          If (SectionOne = YES& SectionTwo = YES)
.begin patch 1.34             
                    Call      CheckRentMin
.end patch 1.34               
                    Goto readrecord
          Else
                    Goto CheckPlacesRight
          Endif


OwnerInfo
          add       SingleSpaced to Row
          add       SingleSpaced to Row
          add       SingleSpaced to Row
          if (ROW   >= LastLine)
                    Goto CheckPlacesLeft
          endif
          prtpage DataCardPrint;*p4000:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"Last Updated By: ";
          prtpage DataCardPrint;*font=CourierNew12,*ll,PASSWORD;
          add       SingleSpaced to Row
          if (ROW   >= LastLine)
                    Move c1 to OwnerPrintFlag
                    Goto CheckPlacesLeft
          endif
Own1
          MOVE NEWDATE TO N8
          IF (NEWDATE = "00000000" | NEWDATE = B8 | N8 = C0)
                    prtpage DataCardPrint;*p4000:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"Put Up Date: ";
                    prtpage DataCardPrint;*font=CourierNew12,*ll,"NOT AVAILABLE";
          ELSE
                    unpack newdate,cc,yy,mm,dd
                    pack str10,mm,"/",dd,"/",yy
                    prtpage DataCardPrint;*p4000:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"Put Up Date: ";
                    prtpage DataCardPrint;*font=CourierNew12,*ll,str10;
          ENDIF
          add       SingleSpaced to Row
          if (ROW   >= LastLine)
                    Move c2 to OwnerPrintFlag
                    Goto CheckPlacesLeft
          endif

Own2
          prtpage DataCardPrint;*p4000:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"List Owner #: ";
.START PATCH 1.1.8 REPLACED LOGIC
.         bump      ownnum,2
.         move      ownnum to nownfld
          unpack    OWNNUM,str2,NOWNFLD
.END PATCH 1.1.8 REPLACED LOGIC
          call      nownkey
          prtpage DataCardPrint;*font=CourierNew12,*ll,OWNLON;
          add       SingleSpaced to Row
          if (ROW   >= LastLine)
                    Move c3 to OwnerPrintFlag
                    Goto CheckPlacesLeft
          endif
Own3
          prtpage DataCardPrint;*p4000:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,OWNLONM;
          add       SingleSpaced to Row
          if (ROW   >= LastLine)
                    Move c4 to OwnerPrintFlag
                    Goto CheckPlacesLeft
          endif
Own4
.START PATCH 1.1.6 REPLACED LOGIC
.         call trim using COMMPER
.         prtpage DataCardPrint;*p4000:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,COMMPER;
          move      COMMPER,str6
          call      trim using str6
          prtpage   DataCardPrint;*p4000:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,str6;
.END PATCH 1.1.6 REPLACED LOGIC
          prtpage DataCardPrint;*font=CourierNew12,*ll," % Commission";
          add       SingleSpaced to Row
          if (ROW   >= LastLine)
                    Move c5 to OwnerPrintFlag
                    Goto CheckPlacesLeft
          endif

Own5
          prtpage DataCardPrint;*p4000:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,OWNOCPY;
          add       SingleSpaced to Row
          if (ROW   >= LastLine)
                    Move c6 to OwnerPrintFlag
                    Goto CheckPlacesLeft
          endif
Own6
          prtpage DataCardPrint;*p4000:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,OWNLOSA;
          add       SingleSpaced to Row
          if (ROW   >= LastLine)
                    Move c7 to OwnerPrintFlag
                    Goto CheckPlacesLeft
          endif
Own7
          CALL TRIM USING OWNLOCTY
          if (OWNLOCTY <> "")
                    prtpage DataCardPrint;*p4000:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,OWNLOCTY;
                    prtpage DataCardPrint;*font=CourierNew12,*ll,",";
                    prtpage DataCardPrint;*font=CourierNew12,*ll,B1;
                    prtpage DataCardPrint;*font=CourierNew12,*ll,OWNLOS;
                    prtpage DataCardPrint;*font=CourierNew12,*ll,B1;
                    prtpage DataCardPrint;*font=CourierNew12,*ll,OWNLOZC;
          else
                    prtpage DataCardPrint;*p4000:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,OWNLOS;
                    prtpage DataCardPrint;*font=CourierNew12,*ll,B1;
                    prtpage DataCardPrint;*font=CourierNew12,*ll,OWNLOZC;
          endif
          add       SingleSpaced to Row
          if (ROW   >= LastLine)
                    Move c8 to OwnerPrintFlag
                    Goto CheckPlacesLeft
          endif
Own8
          if (ownTele <> "")
                    call      trim using owntele
                    unpack    OWNTELE,#str3,str3,str4
                    pack      str15,"(",#str3,")",b1,str3,DASH,str4
                    prtpage DataCardPrint;*p4000:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,str15;
                    add       SingleSpaced to Row
          endif
          if (ROW   >= LastLine)
                    Move c9 to OwnerPrintFlag
                    Goto CheckPlacesLeft
          endif
Own9
          call      trim using ownfax
          if (ownfax <> "")
                    unpack    OWNFAX,#str3,str3,str4
                    pack      str15,"(",#str3,")",b1,str3,DASH,str4
                    prtpage DataCardPrint;*p4000:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,str15;
                    add       SingleSpaced to Row
          endif
          if (ROW   >= LastLine)
                    Move c10 to OwnerPrintFlag
                    Goto CheckPlacesLeft
          endif
Own10
.
.START PATCH 1.1.9 REPLACED LOGIC
.         clear     nfulfld
.         pack      NFULFLD,DATFUL
.         rep       zfill,NFULFLD
.         move      C1,NFULPATH
.         move      "D.Load-NFULKEY",Location
.         pack      KeyLocation,NFULFLD
.         call      NFULKEY
.         if over
.                   clear     NFULCOMP
.         endif
.;
.         prtpage DataCardPrint;*p4000:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"CC TO: ";
.         prtpage DataCardPrint;*font=CourierNew12,*ll,NFULCOMP;
.         move YES to SectionOne
.         Goto CheckPlacesLeft
          call      TRIM using DATFUL
          if (DATFUL <> "")
                    pack      COMPFLD, DATFUL
                    rep       zfill,COMPFLD
                    move      C1,COMPPATH
                    move      "D.Load-COMPKEY",Location
                    pack      KeyLocation,COMPFLD
                    call      COMPKEY
                    if over
                              clear     COMPCOMP
                    else
                              if (COMPSVBFLG <> "T")
                                        clear     COMPCOMP
                              endif
                    endif
          else   // datful = ""
                    clear COMPCOMP
          endif
          prtpage DataCardPrint;*p4000:ROW,*ALIGNMENT=*LEFT,*font=CourierNew12,*ll,"CC TO: ";
          prtpage DataCardPrint;*font=CourierNew12,*ll,COMPCOMP;
          move YES to SectionOne
          Goto CheckPlacesLeft
.END PATCH 1.1.9 REPLACED LOGIC
SelectLoadModifier
          pack      NMODFLD,NSELDESC
          rep       zfill,NMODFLD
          move      "D.Load2-NMODKEY",Location
          pack      KeyLocation,"Key: ",NMODFLD
          call      NMODKEY
          call      Trim using NMODDESC
          if ((NSELBASE = "BASE")|(NSELBASE = "SEC."))
                    pack      str25,dim9a,NMODDESC
                    call trim using str25
          else
                    pack      str25,"+",dim9a,NMODDESC
                    call trim using str25
          endif
          return

DataLoadRefDelivery
          pack      taskname,DELCODE
          pack      NREFFLD,DELCODE
          move      "D.Load2-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          return
DataLoadRefClean
          pack      taskname,CLEANCDE
          pack      NREFFLD,CLEANCDE
          move      "D.Load2-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          return
ClearVars
.Begin patch 1.34   
          Clear     RentFlag
.End patch 1.34     
   move c1 to ArrangementFlag
   move c1 to AddressingFlag
   move c1 to SourceFlag
   move c1 to SelectionsFlag
.Right
   move c1 to SelectFlag1
   move c1 to TextFlag
   move c1 to SelectFlag2

   move c1 to UnitFlag
   move c1 to CleanCdeFlag
   move c1 to NetNameFlag
   move c1 to SampleFlag
   move c1 to DeliveryFlag
   move c1 to OwnerPrintFlag
   move c1 to EnddetailFlag
          move c1 to AddNotesFlag
   clear PageNum
   clear #n4
   clear #n41
   clear #n8
   clear #n9
   clear #n1
   clear #n2
   clear #n3
   clear #result
   clear #str3
   clear num9
   clear N52
          move NO to SectionOne
          move NO to SectionTwo
          move NO to SelNotesFlag
   SelectListView.deleteallitems
   Select2ListView.deleteallitems
.;
          clear dimtext1
          clear dimtext2
          clear dimtext3
          clear dimtext4
          clear dimtext5
          clear dimtext6
          return

RightPadding
          move      "10" to ROW
          Add       "90" to ROW
          add       smboxheight to row
          return

.START PATCH 1.1 ADDED LOGIC
CreateObjects
          create    CourierNew8,"Courier New",size=7
          create    TimesNew7,"Times New Roman",size=7
          create    TimesNew11,"Times New Roman",size=11
          create    TimesNew16,"Times New Roman",size=16
          create    TimesNew16I,"Times New Roman",size=16,Italic
          create    CourierNew12,"Courier New",size=11
          CREATE    White=*WHITE
          CREATE    BLACK=*BLACK
          CREATE    selectlistview=1:50:1:50,SORTHEADER=1,SORTORDER=1
          Selectlistview.INSERTCOLUMN  USING "Index",30,0
          Selectlistview.INSERTCOLUMN  USING "Select Num",30,1
          Selectlistview.INSERTCOLUMN  USING "ListNum",100,2
          ACTIVATE selectlistview
.
          CREATE    select2listview=1:50:50:200,SORTHEADER=1,SORTORDER=1
          Select2listview.INSERTCOLUMN  USING "Index",30,0
          Select2listview.INSERTCOLUMN  USING "Select Num",30,1
          Select2listview.INSERTCOLUMN  USING "ListNum",100,2
          ACTIVATE select2listview
          setprop   SelectListView,visible=c0
          setprop   Select2ListView,visible=c0
          return

.START PATCH 1.1.3 REPLACED LOGIC
.CreateDataCard Routine DimPtr,DimPtr1,DimPtr2,FrmPtr,FrmPtr1
..DimPtr  = FileName
..DimPtr1 = Initials
..DimPtr2 = LogIn Name
..FrmPtr  = Printer Choice
..FrmPtr1 = Owner Info Choice
.begin patch 1.32
.CreateDataCard Routine DimPtr,DimPtr1,DimPtr2,FrmPtr,FrmPtr1,FrmPtr2
CreateDataCard Routine DimPtr,DimPtr1,DimPtr2,FrmPtr,FrmPtr1,FrmPtr2,FrmPtr3
.DimPtr  = FileName
.DimPtr1 = Initials
.DimPtr2 = LogIn Name
.FrmPtr  = Printer Choice
.FrmPtr1 = Owner Info Choice
.FrmPtr2 = Usage Choice - '0'=No Usage Print, '1'=Usage Print, '2'=Usage Print w/o Duplexing
.FrmPtr3 = Company
.END PATCH 1.1.3 REPLACED LOGIC
          if        (FrmPtr3 = c2)
                    move      FrmPtr3,Company
          endif     
.end patch 1.32   added frmptr3
          call      Trim using DimPtr
          if (DimPtr <> "")
                    pack      APIFileName,DimPtr,hexzero
                    call      FindFirstFile
                    if (APIResult <> 0 & APIResult <> hexeight)
                              move      C0,RECCOUNT
                              move      FrmPtr,PrintFlag
                              move      C1,CallFlag
                              call      Trim using DimPtr2
                              call      RemoveChar using DimPtr2,B1
                              if (FrmPtr1 = 1)
                                        move      YES,OWNERINFOFLAG
                              else
                                        move      NO,OWNERINFOFLAG
                              endif
.START PATCH 1.1.3 ADDED LOGIC
                              if (PrintFlag = 3 & FrmPtr2 > 0)
.Force 'No Duplex' if PDF
                                        move      C1,UsageFlag
                              else
                                        move      FrmPtr2,UsageFlag
                              endif
.END PATCH 1.1.3 ADDED LOGIC
.                             move      c1 to NDATCONV
                              call      Begin
                              destroy   TimesNew7
                              destroy   CourierNew8
                              destroy   TimesNew11
                              destroy   TimesNew16
                              destroy   TimesNew16I
                              destroy   CourierNew12
                              destroy   White
                              destroy   BLACK
                              destroy   selectlistview
                              destroy   select2listview
                              erase     DimPtr
                    endif
          endif
          return
.END PATCH 1.1 ADDED LOGIC
.Patch1.2.1
CreateFaxFileDataCard Routine DimPtr,DimPtr1
.DimPtr  = FileName
.DimPtr1 = DatacardReturnPrintName
          call      Trim using DimPtr
          if (DimPtr <> "")
                    pack      APIFileName,DimPtr,hexzero
                    call      FindFirstFile
                    move c2 to callflag
                    move c0 to reccount
                    call      Begin
                    destroy   TimesNew7
                    destroy   CourierNew8
                    destroy   TimesNew11
                    destroy   TimesNew16
                    destroy   TimesNew16I
                    destroy   CourierNew12
                    destroy   White
                    destroy   BLACK
                    destroy   selectlistview
                    destroy   select2listview
                    close datcard
                    erase     DimPtr
          endif
          return
.Patch1.2.1
.begin patch 1.39
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    move      "3000",str4
                    call      waitin using str4
.                    pause     "30"
                    noreturn
                   if        (trapcount > 60)   . 5 min are you kidding me. clearly not waiting 5 min
                    Pack       MailSubjct,"Datacard print - ",str55
.                    Move      "CReques@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    "mailTo = ",mailbody
                    append    mailto,mailbody
                    append    CRLF,MailBOdy
                    append    "maiLFrom = ",mailbody
                    append    maiLFrom,mailbody
                    
                    append    CRLF,MailBOdy
                    append    str45,MailBody
                    append    CRLF,MailBOdy
                    append    str55,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Clear     Mailto
                    Pack      MailTO,"CReques@nincal.com"
                    Move      B1,Mailattach
                    call      SendMail
                    return
                    
                    endif
          
                    goto      checkfile
.end patch 1.39
.begin patch 1.43
Spool1
         TRAPCLR   SPOOL
         Trap       SPOOL1 giving error if SPOOL
         SCAN      "S10" IN ERROR
         if equal
                    pack      taskname from "I Cannot find that printer Please select one."
                    Alert     caution,taskname,result

                    PRTOPEN DataCardPrint,"","dataprnt.lst"
                    return
          Else
                    Reset Error
                    pack      taskname from "printing failed Please inform Information Services. ",CRLF,Error
                    Alert     caution,taskname,result
                    SHUTDOWN   "CLS"
                    STOP

         endif
.end patch 1.43
...................................................................
          include   nselio.inc
          include   nrefio.inc
          include   nmodio.inc
          include   naddio.inc
          include   narrio.inc
          include   ncatio.inc
          include   NSLTio.inc
          include   nsrcio.inc
          include   ntxtio.inc
          include   ndatio.inc
.START PATCH 1.1.9 REPLACED LOGIC
.         include   nfulio.inc
          include compio.inc
          include   cntio.inc
          include   ncntio.inc
.START PATCH 1.1.9 REPLACED LOGIC
.begin patch 1.42
         include    ntxt1io.inc
.end patch 1.42
          include   nownio.inc
          INCLUDE   COMLOGIC.INC