PC       EQU       0
         INC       COMMON.INC
         INCLUDE   CONS.INC
         INCLUDE   NORDDD.INC
         INCLUDE   NOWNDD.INC
                              include   compdd.inc
                              include   cntdd.inc
         include   hp.inc
         include   nguadd.inc  
          INCLUDE   NSEL2DD.INC
          include   nmoddd.inc
release  init      "1.0"        DLH     print outside Guarantee
Reldate   Init      "05 NOvember 2012"
.
KEY      DIM       6
THIRTY   FORM      "30"
FORTY5   FORM      "45"
SIXTY    FORM      "60"
NINETY   FORM      "90"
NWORK1   FORM      1
STRING   DIM       45
STRING2  DIM       50
DAYS     FORM      2
GUAR     DIM       1
CON      INIT      "000"
TYPIST   DIM       3
ATTNTO   DIM       25
ATTNFAX    DIM         11
LR       INIT      "LR ##"
SPACE    INIT      " "
QTYOUT   DIM       11         *USED FOR ORDER PRINT
QTYNUM   FORM      9         *USED FOR ORDER PRINT, QTY FORMATING.
QTYMSK   INIT      "ZZZ,ZZ9,999"    *USED FOR ORDER PRINT
ARCD     DIM       3                   AREA CODE
EXCH     DIM       3                   EXCHANGE
TELE     DIM       4                   TELEPHONE#
LBRAK    INIT      "("
RBRAK    INIT      ")"
FAX      INIT      "Fax: "
FAXLINE  DIM       25
FontO7              font
FontO18B  font

PrtFlag   Form       1
DimPtr    Dim       ^
DimPtr1   Dim       ^
DimPtr2   Dim       ^
DimPtr3   Dim       ^
DimPtr4   Dim       ^
DimPtr5   Dim       ^


NINLogo   PICT
spoolfle DIM       40                   .order  SPOOL FILEs
Tempfle DIM       40                   .order  SPOOL FILEs
PrintDoc  pfile
TimesNew11          font  
CopyVar2 dim        5000 
NUM       FORM      10
input    file
GUARFILE IFILE     KEYLEN=6
.
         OPEN      GUARFILE,"GUARANT"
         MOVE      C1 TO NORDPATH
PrintGua  Routine  DimPTr,DimPtr1,DimPTr2,Dimptr3,DimPtr4,DimPtr5
          create    fontO7,"Times New Roman",size=7
          create    fontO18B,"Times New Roman",size=18,Bold
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
          create    TimesNew11,"Times New Roman",size=11
                    move "180" to eightlpi
          move "300" to Column1
          move "700" to Column2
          MOVE      C1 TO NORDPATH
          call      trim using DimPtr
          packkey   Nordfld from Dimptr             .LR#
          rep       Zfill in Nordfld
          Move      Nordfld,Key
          call      Trim using DimPtr1              .Guaranty type        
          move      DimPtr1,Guar
          call      Trim using DimPtr2            .Inits
          move      DimPtr2,Typist
          call      Trim using DimPtr3
          move      DimPtr3,PrtFlag
          call      Trim using DimPtr4
          move      DimPtr4,ATTNTO
          call      Trim using DimPtr5
          move      DimPtr5,ATTNFAX
          


        clock   timestamp,timestamp
        unpack  timestamp,CC,YY,MM,DD
.
         MOVE      "NGUA0003" TO PROGRAM
          MOVE      "Names in the News" TO COMPNME
         MOVE      "PRINT GUARANTEES" TO STITLE
         CALL      PAINT

          Call      PrintPrep
          PRTPAGE   PrintDoc;*UNITS=*HIENGLISH:
                                                                                                                        *ORIENT=*Portrait:
                                                                                                                        *MarginL=1;
          call      debug
.                                       move "553562" to KEY
.                                       move "553562" to KEY
.                                       move "1" to GUAR
.                                       move "DLH" to TYPIST
.                                       move "David Herrick" to ATTNTO
.                                       move "4154337796" to ATTNFAX
.FOr Testing
.COmmentted out for testing
                              if (num > c1)
                            prtpage PrintDoc;*NEWPAGE:
                                        *UNITS=*HIENGLISH:
                   *ORIENT=*PORTRAIT;
                              endif
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
.
GO       PACK      STRING FROM MCOMP
         PACK      STRING2 FROM O1DES
         PACK      QTYOUT FROM QTYMSK
         MOVE      OQTY TO QTYNUM
               move           c0 to n9
               move           oexqty to n9
               sub            n9 from qtynum
         EDIT      QTYNUM TO QTYOUT

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
                                                  prtpage   PrintDoc;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:1000:300:8300:NINLogo

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
                                                  Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please be advised that Names in the News guarantees payment ";
                                                  add eightlpi to ROW
                                                  Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"on behalf of our client, on the above order, ";
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
         packkey      nguafld from nordfld
         call      nguatst
         if        not over
         call     nguadel
         else
         pack     guadate from cc,yy,mm,dd
         pack     guaLR from key
         call     nguawrt
         endif
FINI   
                            prtpage PrintDoc;*NEWPAGE
                              prtclose PrintDoc
                    move      "500",str3
                    call      waitin using str3                              
.                              Rename "c:\work\faxfile.prn",Str45
          REturn
NOWNER   MOVE      "****************" TO OWNOCPY
         RETURN
NMLR   MOVE      "****************" TO MCOMP
         RETURN
.......................................................................................................................
PrintPrep
          call      GetWinVer
                    if (PRTFLAG = "2")  .Sales
                              DISPLAY   *P15:07,PRTNAME,b1,prtflag," Laser3"

                              if (OSFLAG >= c6)
                                        PRTOPEN   PrintDoc,"\\nins2\Laser3 Blankstock","Guaranty"
.                              Elseif    (OSFLAG = "7" or OSFLAG = "9")        .,vista,win7
.                                        PRTOPEN   PrintDoc,"","Guaranty"
                              else   .(osflag = c0)         .Don't know prompt for printer
                                        PRTOPEN   PrintDoc,"","Guaranty"
                              endif
                    elseif (PRTFLAG = "3")                            .Acctng
                              DISPLAY   *P15:07,PRTNAME,b1,prtflag," Laser2"
                              if (OSFLAG >= c6)
                                        PRTOPEN   PrintDoc,"\\nins2\Laser2","Guaranty"
.                              Elseif    (OSFLAG = "7" or OSFLAG = "9")        .,vista,win7
.                                        PRTOPEN   PrintDoc,"","Guaranty"
                              else   .(osflag = c0)         .Don't know prompt for printer
                                        PRTOPEN   PrintDoc,"","Guaranty"
                              endif
                  elseif (PRTFlag = 5)     .Susan
                              DISPLAY   *P15:07,PRTNAME,b1,prtflag," Susan's Printer"
                              if (OSFLAG >= c6)
                                  PRTOPEN PrintDoc,"@KYOCERA FS1030D","Guaranty"
.                          elseif (osflag = c6)         .XP
.                                  PRTOPEN PrintDoc,"@KYOCERA FS1030D","Guaranty"
.                          elseif (osflag = c1)         .win 95 98
.                                  PRTOPEN PrintDoc,"@KYOCERAS","Guaranty"
                          else   .(osflag = c0)         .Don't know prompt for printer
                                  PRTOPEN PrintDoc,"","Guaranty"
                          endif
                  elseif (prtflag = 7)     .DH
                              DISPLAY   *P15:07,PRTNAME,b1,prtflag," David's Printer"
                                  PRTOPEN PrintDoc,"","Guaranty"
                    else
                              DISPLAY   *P15:07,PRTNAME,b1,prtflag," Laser8","Guaranty"
                              if (OSFLAG >= c6)
.                                        PRTOPEN   PrintDoc,"\\NINs2\Laser8","Guaranty"
.                              elseif (OSFLAG = "3" or OSFLAG = "4")             .95/98
                                        PRTOPEN   PrintDoc,"\\nins2\Laser8","Guaranty"
.                              Elseif    (OSFLAG = "7" or OSFLAG = "9")        .,vista,win7
.                                        PRTOPEN   PrintDoc,"","Guaranty"
                              else   .(osflag = c0)         .Don't know prompt for printer
                                        PRTOPEN   PrintDoc,"-","Guaranty"
                              endif
                    endif

          Return
.............................................................................................................


         INCLUDE   NORDIO.inc
                              include   compio.inc
                              include   cntio.inc
         INCLUDE   NOWNIO.inc
         include   hpio.inc
         include   nguaio.inc
          INCLUDE   NSEL2IO.INC
          include   nmodio.inc
         INCLUDE   COMLOGIC.inc
