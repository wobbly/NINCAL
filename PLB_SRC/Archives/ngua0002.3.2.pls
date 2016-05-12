PC       EQU       0
         INC       COMMON.INC
         INCLUDE   CONS.INC
         INCLUDE   NORDDD.INC
         INCLUDE   NOWNDD.INC
;Patch2.9
                              include   compdd.inc
                              include   cntdd.inc
.         INCLUDE   NMLRDD.INC
;Patch2.9
         include   hp.inc
.begin release 2.4
         include   nguadd.inc  
.end release 2.4
.START PATCH 2.8 ADDED LOGIC
          INCLUDE   NSEL2DD.INC
          include   nmoddd.inc
.END PATCH 2.8 ADDED LOGIC
release  init      "3.2"        DLH     turnoff  Pacific Lists
Reldate   Init      "25 January 2010"
.release  init      "3.1"        DLH     22May2007  Pacific Lists
.Reldate   Init      "22 May 2007"
.release  init      "3.0"        DMB    02SEP2004  New Logo - Prt Page rewrite
.release  init      "2.9"        DMB    26MAY2004 Mailer Conversion   
.release  init      "2.8"           ASH 28JAN2004 DATACARD CONVERSION
.release  init      "2.7"           ASH 14April2003 ALLOW USER ALLOCATED FAX NUMBER
.                                                 ADDED SOME LANGUAGE TO FORM
.release  init      "2.6"           DLH 04March2003 print only rental qty
;release  init      "2.5"           ASH 02OCT2000 NEW SERVER ADDED
.release  init      "2.4"           DLH 09Sep00 write lr and date to new NINGuar file
.release  init      "2.3"           DLH 10Feb00 LETTERHEAD MAcro replacement
.release  init      "2.2"           ASH 30DEC98 NINORD Y2K, File expansion
.release  init      "2.1"           ASH NINMLR Y2K
.release  init      "2.0"           DLH Blankstock Letter head 18dec96
.release  init      "1.8"           DLH 25Jun96 print owner fax number
.release  init      "1.7"           JD  24may95 print to laser printer
.release  init      "1.6"          DLH JAN95 added qty/ppm
.RELEASE  INIT      "1.5"          DLH 13MAR92
.CREATED                           08DEC1988
.
KEY      DIM       6
THIRTY   FORM      "30"
FORTY5   FORM      "45"
SIXTY    FORM      "60"
NINETY   FORM      "90"
NWORK1   FORM      1
.Start Patch #2.1 - MCOMP expansion
.STRING   DIM       35
STRING   DIM       45
.End Patch #2.1 - MCOMP expansion
STRING2  DIM       50
.COMMA    INIT      ","
DAYS     FORM      2
GUAR     DIM       1
CON      INIT      "000"
TYPIST   DIM       3
ATTNTO   DIM       25
.START PATCH 2.7 ADDED LOGIC
ATTNFAX    DIM         11
.END PATCH 2.7 ADDED LOGIC
LR       INIT      "LR ##"
SPACE    INIT      " "
.Start Patch #2.2 - increase vars to reflect increased OQTY
.QTYOUT   DIM       9         *USED FOR ORDER PRINT
.QTYNUM   FORM      7         *USED FOR ORDER PRINT, QTY FORMATING.
.QTYMSK   INIT      "Z,ZZ9,999"    *USED FOR ORDER PRINT
QTYOUT   DIM       11         *USED FOR ORDER PRINT
QTYNUM   FORM      9         *USED FOR ORDER PRINT, QTY FORMATING.
QTYMSK   INIT      "ZZZ,ZZ9,999"    *USED FOR ORDER PRINT
.End Patch #2.2 - increase vars to reflect increased OQTY
ARCD     DIM       3                   AREA CODE
EXCH     DIM       3                   EXCHANGE
TELE     DIM       4                   TELEPHONE#
LBRAK    INIT      "("
RBRAK    INIT      ")"
FAX      INIT      "Fax: "
FAXLINE  DIM       25
.begin patch 3.1
FontO7              font
FontO18B  font

          create    fontO7,"Times New Roman",size=7
          create    fontO18B,"Times New Roman",size=18,Bold
.end patch 3.1


.Patch 1.26 - Variables Added
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
spoolfle DIM       40                   .order  SPOOL FILEs
Tempfle DIM       40                   .order  SPOOL FILEs
PrintDoc  pfile
TimesNew11          font  
          create    TimesNew11,"Times New Roman",size=11
                    move "180" to eightlpi
          move "300" to Column1
          move "700" to Column2
CopyVar2 dim        5000 
NUM       FORM      10
.Patch 1.26
input    file
GUARFILE IFILE     KEYLEN=6
.
         OPEN      GUARFILE,"GUARANT"
         MOVE      C1 TO NORDPATH
.begin release 2.4
        clock   timestamp,timestamp
        unpack  timestamp,CC,YY,MM,DD
.end release 2.4
.
         MOVE      "NGUA0002" TO PROGRAM
          if        (Company = c2)
          move      "Pacific Lists, Inc.",Compnme
          Else
          MOVE      "Names in the News" TO COMPNME
          endif
         MOVE      "PRINT GUARANTEES" TO STITLE
.START PATCH 2.5 REPLACED LOGIC
.         open      input,"\\nts0\d\data\guarant"
.         CALL      PAINT
.         SPLOPEN   "\\nts0\d\DATA\NGUAR2.LST"

         PACK      STR35,NTWKPATH1,"guarant"
         PACK      STR45,NTWKPATH1,"NGUAR2.LST"
         open      input,STR35
         CALL      PAINT
.Patch 3.0 Commented Out
.         SPLOPEN   STR45
.Patch 3.0 Commented Out
.Patch 3.0 Logic Added
                                                            PRTOPEN PrintDoc,"faxfile","Faxfile.prn"
                                                            PRTPAGE PrintDoc;*UNITS=*HIENGLISH:
                                                                                                                        *ORIENT=*Portrait:
                                                                                                                        *MarginL=1;
.Patch 3.0 Logic Added
.END PATCH 2.5 REPLACED LOGIC
LOOP     filepi    1;input
.START PATCH 2.7 REPLACED LOGIC
.         READ      input,seq;key,GUAR,TYPIST,ATTNTO
.For Testing
          call      debug
.                                       move "553562" to KEY
.                                       move "553562" to KEY
.                                       move "1" to GUAR
.                                       move "DLH" to TYPIST
.                                       move "David Herrick" to ATTNTO
.                                       move "4154337796" to ATTNFAX
.FOr Testing
.COmmentted out for testing

        READ input,seq;key,GUAR,TYPIST,ATTNTO,ATTNFAX
.FOr Testing
.END PATCH 2.7 REPLACED LOGIC
         GOTO      FINI IF  OVER
           add c1 to num         
           
         CMATCH    B1 TO KEY
         GOTO      LOOP IF EOS 
.Patch 3.0 Logic Added
                              if (num > c1)
                            prtpage PrintDoc;*NEWPAGE:
                                        *UNITS=*HIENGLISH:
                   *ORIENT=*PORTRAIT;
                              endif
.Patch 3.0 Logic Added
        MOVE      KEY TO NORDFLD
         CALL      NORDKEY
         GOTO      PRINT IF OVER
         PACK      MKEY FROM OMLRNUM,CON
         CALL      NMLRKEY
         CALL      NMLR IF OVER
         MOVE      OLON TO NOWNFLD
         CALL      NOWNKEY
         CALL      NOWNER IF OVER
         CMATCH    " " TO ATTNTO
         CALL      ATTN IF NOT EOS
         CALL      FAX
PRINT    MOVE      GUAR TO NWORK1
         ENDSET    MCOMP
         DISPLAY   *P1:23,*EL,"END1",MCOMP
END1     CMATCH    " ",MCOMP
         GOTO      MORE IF EQUAL
         LENSET    MCOMP
         RESET     MCOMP
         GOTO      END2
MORE     BUMP      MCOMP BY -1
         GOTO      END1
END2     ENDSET    O1DES
         DISPLAY   *P1:23,*EL,"END2",O1DES
END2A    CMATCH    " ",O1DES
         GOTO      MORE1 IF EQUAL
         LENSET    O1DES
         RESET     O1DES
         GOTO      GO
MORE1    BUMP      O1DES BY -1
         GOTO      END2A
.
ATTN     MOVE      ATTNTO TO OWNLONM
         RETURN    
.START PATCH 2.7 REPLACED LOGIC
.fax      clear     faxline
.         cmatch    " " to OWNFAX
.         RETURN    IF EQUAL
.         RETURN    IF EOS 
.         UNPACK    OWNFAX INTO ARCD,EXCH,TELE
.         PACK      FAXLINE FROM FAX,LBRAK,ARCD,RBRAK,B1,EXCH,DASH,TELE
.         RETURN
fax
          clear     faxline
          call      Trim using ATTNFAX
          if (ATTNFAX <> "")
                    count     howmany,ATTNFAX
                    if (howmany = 11)
                              unpack    ATTNFAX,str1,ARCD,EXCH,TELE
                    elseif (howmany = 10)
                              unpack    ATTNFAX,ARCD,EXCH,TELE
                    elseif (howmany = 7)
                              unpack    ATTNFAX,EXCH,TELE
                              move      "510",ARCD
                    else
                              goto fax2
                    endif
          else
fax2
                    cmatch    " ",OWNFAX
                    RETURN IF EQUAL
                    RETURN IF EOS 
                    UNPACK    OWNFAX,ARCD,EXCH,TELE
          endif
          PACK      FAXLINE,FAX,LBRAK,ARCD,RBRAK,B1,EXCH,DASH,TELE
          RETURN
.END PATCH 2.7 REPLACED LOGIC         
.
GO       PACK      STRING FROM MCOMP
.START PATCH 2.7 REPLACED LOGIC
.         PACK      STRING2 FROM O1DES,SLASH,COMMA,SPACE,LR,KEY
         PACK      STRING2 FROM O1DES
.END PATCH 2.7 REPLACED LOGIC
         PACK      QTYOUT FROM QTYMSK
         MOVE      OQTY TO QTYNUM
;begin patch 2.6
               move           c0 to n9
               move           oexqty to n9
               sub            n9 from qtynum
;end patch 2.6
         EDIT      QTYNUM TO QTYOUT
.START PATCH 2.8 REPLACED LOGIC
.         UNPACK    OPPM INTO STR3,STR2
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
                    unpack    OPPM,str3,str2
                    pack      str6,str3,".",str2
                    rep       zfill,str6
                    move      str6,NSEL2PRICE
                    move      "/m",NMODDESC
          else
                    pack      NMODFLD,NSEL2DESC
                    rep       zfill,NMODFLD
                    move      "NMODKEY",Location
                    pack      KeyLocation,"Key: ",NMODFLD
                    call      NMODKEY
                    if over
                              move      "/m",NMODDESC
                    endif
          endif
          call      Trim using NSEL2NAME
          call      Trim using NMODDESC
          unpack    NSEL2PRICE,STR5,STR3
          call      FormatNumeric using str5,str6
          pack      taskname,str6,str3,NMODDESC
.END PATCH 2.8 REPLACED LOGIC
.begin patch 2.
.         PRINT     hpbtray,hpltrhd,*N,*N,*N,*N,*N,*N,*6,OWNLONM:
.Patch 3.0 Commented Out
..         PRINT     hpbtray;
..         call      PortraitLTRHead
.START PATCH 2.7 REPLACED LOGIC
.         print     *N,HPTMSRMN,hpfixed,*N,*N,*N,*N,*N,*6,OWNLONM:
.                   *N,*6,OWNOCPY,B2,FAXLINE:
.                   *N,*6,OWNLOSA:
.                   *N,*6,OWNLOCTY," ",OWNLOS,"  ",OWNLOZC:
.                   *N:
.                   *N,*6,"RE: MAILER - ",STRING,*FLUSH;
..         print     *N,HPTMSRMN,hpfixed,*N,*N,*N,*N,*N,*6,"Attention:  ",OWNLONM:
..                   *N,*6,OWNOCPY,B2,FAXLINE:
..                   *N,*6,OWNLOSA:
..                   *N,*6,OWNLOCTY," ",OWNLOS,"  ",OWNLOZC:
..                   *N:
..                   *N,*6,"RE: MAILER - ",STRING,*FLUSH;
.END PATCH 2.7 REPLACED LOGIC
.Start patch #2.1 - remmed and replaced to match current version in PLB_SRC
.         PRINT     *10,*rptchar "_":47:
.                   *N,*10,"LIST - ",STRING2,*FLUSH;
..       PRINT     *10,*rptchar "_":37:
..                 *N,*10,"LIST - ",STRING2,*FLUSH;
.End patch #2.1 - remmed and replaced to match current version in PLB_SRC
.START PATCH 2.7 REPLACED LOGIC
.         PRINT     *10,*rptchar "_":60:
.                   *N,*10,o2des:
.                   *n,*10,"Quantity: ",qtyOUT," @",str3,period,str2,"/m",*n:
.                   *N,*6,"Please be advised that Names in the News/Ca, ";
.START PATCH 2.8 REPLACED LOGIC
.         PRINT     *10,*rptchar "_":60:
.                   *N,*10,o2des:
.                   *n,*10,"Quantity: ",qtyOUT," @",str3,period,str2,"/m":
.                   *N,*10,LR,KEY,SPACE,SPACE,SPACE,SPACE,"Mail Date:  ",OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY,*n:
.                   *N,*6,"Please be advised that Names in the News/Ca, ";
..        PRINT     *10,*rptchar "_":60:
..                  *N,*10,NSEL2NAME:
..                  *n,*10,"Quantity: ",qtyOUT," @",taskname:
..                  *N,*10,LR,KEY,SPACE,SPACE,SPACE,SPACE,"Mail Date:  ",OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY,*n:
..                  *N,*6,"Please be advised that Names in the News/Ca, ";
.END PATCH 2.8 REPLACED LOGIC
.END PATCH 2.7 REPLACED LOGIC 
..        PRINT     "Inc. guarantees payment ":
..                   *N,*6,"on behalf of our client, on the above order";
.Patch 3.0 Commented Out
.Patch 3.0 Logic Added
.begin patch 3.2
.begin patch 3.1
.          call      debug
..testing
..         move      "P",compexcl
.          IF        (CompExcl = "P")
.          prtpage   PrintDoc;*p=1:25,*font=fontO18b,"Pacific Lists, Inc.":
.                    *p=451:343,*font=fontO7,"1300 Clay St. 11th Floor":
.                    *p=451:443,"Oakland, CA 94612-1492":
.                    *p=317:543,"415-945-9450 ","·"," Fax 415-945-9451":
.                    *p=317:643,"A Division of Names in the News"
.          Else
.         prtpage   Laser;*Pictrect=*off,*PICT=0:800:5350:10350:NINLogo
.                                                 prtpage   PrintDoc;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:1000:300:8300:NINLogo
                                                  prtpage   PrintDoc;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:1000:300:8300:NINLogo
.          endif
.end patch 3.1
.end patch 3.2
                                                  move "300" to row
                                                  add eightlpi to ROW
                                                  add eightlpi to ROW
                                                  add eightlpi to ROW
                                                  add eightlpi to ROW
                                                  Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Attention: ";
                                                  Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,OWNLONM;
                                                  add eightlpi to ROW
                                                  Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,OWNOCPY;
                                                  Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,B2;
                                                  Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,FAXLINE;
                                                  add eightlpi to ROW
                                                  Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,OWNLOSA;
                                                  add eightlpi to ROW
                                                  call trim using OWNLOCTY
                                                  Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,OWNLOCTY;
                                                  Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,COMMA;
                                                  Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,B1;
                                                  Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,OWNLOS;
                                                  Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,B1;
                                                  Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,OWNLOZC;
                                                  add eightlpi to ROW
                                                  add eightlpi to ROW
                                                  add eightlpi to ROW
                                                  Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"RE:"; 
                                                  Prtpage PrintDoc;*pcolumn2:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,*ULON,"MAILER -":
                                                            *ALIGNMENT=*LEFT,*font=TimesNew11,*ll,*ULON,STRING,*ULOFF;
                                                  add eightlpi to ROW
                                                  Prtpage PrintDoc;*pcolumn2:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,*ULON,"LIST -":
                                                            *ALIGNMENT=*LEFT,*font=TimesNew11,*ll,*ULON,STRING2,*ULOFF;
                                                  add eightlpi to ROW
                                                  Prtpage PrintDoc;*pcolumn2:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,NSEL2NAME;
                                                  add eightlpi to ROW
                                                  Prtpage PrintDoc;*pcolumn2:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Quantity: ";
                                                  Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,QTYOUT;
                                                  Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll," @";
                                                  Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,taskname;
                                                  add eightlpi to ROW
                                                  Prtpage PrintDoc;*pcolumn2:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,LR;
                                                  Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,KEY;
                                                  Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,B5;
                                                  Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Mail Date:  ";
                                                  pack str10 with OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
                                                  Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,str10;
                                                  add eightlpi to ROW
                                                  add eightlpi to ROW
.                                                  IF        (CompExcl = "P")
.                                                  Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please be advised that Pacific Lists, Inc. guarantees payment ";
.                                                  Else
                                                  Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please be advised that Names in the News guarantees payment ";
.                                                  Endif
                                                  add eightlpi to ROW
                                                  Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"on behalf of our client, on the above order, ";
.Patch 3.0 Logic Added
         BRANCH    NWORK1 OF THIRTY,FORTY5,SIXTY,NINETY:
                   NONE,NONE,THIRTY,FORTY5,SIXTY
NONE     PRINT     ".",*N
         GOTO      PRNTMORE
THIRTY   MOVE      THIRTY TO DAYS
         GOTO      PRNTMORE
FORTY5   MOVE      FORTY5 TO DAYS
         GOTO      PRNTMORE
SIXTY    MOVE      SIXTY TO DAYS
         GOTO      PRNTMORE
NINETY   MOVE      NINETY TO DAYS
         GOTO      PRNTMORE
PRNTMORE 
.Patch 3.0 Commented Out
.                             PRINT     ", no later than ",DAYS," days ":
.                   *N,*6,"after the mail date.":
.                   *N,*N,*N,*N,*N:
.                   *6,"________________________";
.         PRINT     "________________________":
.                   *N,*6,"signature":
.                   *N,*N,*N,*N,*N:
.                   *6,"_____________________________":
.                   *N,*10,"date":
.                   *N,*N,*N,*N,*6,TYPIST,*N,*N,*N,*N,*N,*N,*N:
.                   *N,*N,*FLUSH
.Patch 3.0 Commented Out
.Patch 3.0 Logic Added
                              Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"no later than ";
                              Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,DAYS;
                              Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll," days ";
                              add eightlpi to ROW
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"after the mail date.";
                              add eightlpi to ROW
                              add eightlpi to ROW
                              add eightlpi to ROW
                              add eightlpi to ROW
                              add eightlpi to ROW
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"________________________________________________";
                              add eightlpi to ROW
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Signature";
                              add eightlpi to ROW
                              add eightlpi to ROW
                              add eightlpi to ROW
                              add eightlpi to ROW
                              add eightlpi to ROW
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"_____________________________";
                              add eightlpi to ROW
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Date";
                              add eightlpi to ROW
                              add eightlpi to ROW
                              add eightlpi to ROW
                              add eightlpi to ROW
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,TYPIST;
.Patch 3.0 Logic Added
.FOr Testing COmmented Out
         FILEPI    2;GUARFILE
         read      guarfile,key;;
         DELETE    GUARFILE,KEY
.begin release 2.4
         pack      nguafld from key
         call      nguatst
         if        not over
         call     nguadel
         else
         pack     guadate from cc,yy,mm,dd
         pack     guaLR from key
         call     nguawrt
         endif
.For TEsting COmmented Out
.end release 2.4
.For testing
.                             prtclose PrintDoc
.                             Rename "\\nts0\d\data\fax\faxfile.prn",Str45
.fort testing 
         GOTO      LOOP
FINI   
.Patch 3.0 Commented Out
.         print     033,"&l1H"
.        splclose
.Patch 3.0 Commented Out
.Patch 3.0 Logic Added
.Patch 3.1 bug fix
          if (num <> c0)
                              prtclose PrintDoc
                              Rename "c:\work\faxfile.prn",Str45
          endif
.Patch 3.0 Logic Added
        close     guarfile
.         EXECUTE   "F:\PUBLIC\NPRINT g:\DATA\nguar2.LST Q=DESKJET NT NB f=BLANKSTOCK S=NS1"
         shutdown  "CLS"
         STOP
NOWNER   MOVE      "****************" TO OWNOCPY
         RETURN
NMLR   MOVE      "****************" TO MCOMP
         RETURN
         INCLUDE   NORDIO.inc
;Patch2.9
                              include   compio.inc
                              include   cntio.inc
.         INCLUDE   NMLRIO.inc
;Patch2.9
         INCLUDE   NOWNIO.inc
         include   hpio.inc
.begin release 2.4
         include   nguaio.inc
.end release 2.4
.START PATCH 2.8 ADDED LOGIC
          INCLUDE   NSEL2IO.INC
          include   nmodio.inc
.END PATCH 2.8 ADDED LOGIC
         INCLUDE   COMLOGIC.inc
