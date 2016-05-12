...........................................................................
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORD2DD.inc
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
         INCLUDE   NOWNDD.inc
         INCLUDE   NSHPDD.inc
         INCLUDE   NDATDD.inc
         include   nspedd.inc
         include   nrtndd.inc
         INCLUDE   NMRGDD.inc
         INCLUDE   NMOADD.inc
         INCLUDE   NMOBDD.inc
         include   media.inc
         include   shipping.inc
         include   hp.inc
         include   nofrdd.inc
          INCLUDE   NSELDD.INC
          INCLUDE   NSEL2DD.INC
          INCLUDE   NTXTDD.INC
          INCLUDE   NSEL3DD.INC
          INCLUDE   NADDDD.INC
          INCLUDE   NSLTDD.INC
          INCLUDE   NREFDD.INC
          INCLUDE   NMODDD.INC
          include   nspe3dd.inc
.Begin patch 3.0
          include   NCntdd.inc
          include   Nusedd.inc
          include   prtpagedd.inc
.end patch 3.0
          
Release  init      "3.01"        DLH    convert to Sunbelt Internal PDF facility and some print cleanup
reldate   Init      "2013 April 18"
.Release  init      "3.00"        DLH    convert to prtpage. See archives for previous changes
.reldate   Init      "8 February 2013"
.Release  init      "2.92"        DLH    change to print directly to printer
.reldate   Init      "9 August 2011"
.Release  init      "2.91"        DLH    use Data Manager
.reldate   Init      "3 February 2011"
.Release  init      "2.9"        DLH    Cleanup LM vs Brokerage
.reldate   Init      "23 April 2009"
.Release  init      "2.86"        ASH             19JUN2007 PLI Inclusion
.Release  init      "2.85"        JD    23APR2007 Moved "Exchange" status print to catch exch/rent marked orders.
.Release  init      "2.84"       DMB    12OCT2006 Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.release  init      "2.83"      DLH     18Sep2006 IF shipped take regardless of date
.                                          IF date is due take even if not shipped
.release  init      "2.82"      DLH     12JUL2006 Turn off Epsilon Discount
.release  init         "2.81" DMS       22JUN2006 FULFILLMENT CONVERSION
.release  init      "2.80 "     JD      20JAN2006 Added check for Variance flag.(listmgmt only)
.;release  init      "2.76"      JD      03JUN2005 changed input name to NINORDU.SRT.
.;release  init      "2.75"      JD      09JUn2004 Refix required trim of datacard var Commper
.;RELEASE  INIT      "2.73.3"    ASH     27MAY2004 MAILER CONVERSION
.                                       ASH                           Corrected logic around NINSEL3
.RELEASE  INIT      "2.73.2"            29APR04             DB FIXES
.RELEASE  INIT      "2.73.1"  ASH       12APR04             DATACARD CONVERSION
.RELEASE  INIT      "2.73"              ASH       29jan04   DATACARD CONVERSION
.RELEASE  INIT      "2.72"              JD        06AUG2002           added lw robbins 12/m mailers.
.release  init      "2.71"              ASH       05FEB2002           NINFUL CONVERSION
.release  init      "2.7"               DLH       24OCT2001           new date logig around no return dates
.release  init      "2.6"               ASH       02OCT2000           NEW SERVER ADDED
.release  init      "2.5"               ASH       01JUN2000 Added Media types
.release  init      "2.4"               DLH       08Oct99   additional rtn date checks
.release  init      "2.3"               ASH       26May99   NINSPEC.DAT conversion
.RELEASE  INIT      "2.2"               ASH       07MAY99   REPLACED OODES{NINORD.DAT} --> OFDESC{NINOFR.DAT}
.Release  Init      "2.1"               DLH       01Apr99   new Shipping.inc
.RELEASE  INIT      "2.0"               ASH       08JAN99   NINORD Y2K, File expansion
.Release  init      "1.9"               DLH       28Sep98   added code to handle pending orders
.                                                           See norddd.inc patch 5
.release   init     "1.8"       DLH     09JUl98   use datacard commission on brokerage rental estimates.
.release  init       "1.7"      DLH     10Jun98   add nspedd & io set locking flags off for mailer,datacard, & spec inst reads.
.release  init       "1.6"      DLH     27Mar98   Turned off read only
.                                                           open as program is only one using file.
.release  init       "1.5"      DLH     07Feb97   new data card format
.release  init       "1.4"      JD      02feb96   only proces 2/1/96 forward
.release  init       "1.3"      JD      21jun95   check broker file if lstmgmt
.release  init       "1.2"      DLH     27Feb95             new batch bill code
.Release   init      "1.1"      JD      11jan95   added nbrkdd. print brcomp
.                                                           print of net name.
.Release   init      "1.0"      DLH     15may94   initial release from nord013b
. .............................................................................
.
.
. WORK VARIABLES
.
THOUS    FORM      "1000"
HUND     FORM      "100"
PDATE    DIM       8
JOBBR    FORM      1               BRANCH FOR JOB TYPE SEPERATE,TOTAL
CHANGE   FORM      7.2
SAMEMLR  DIM       7
TOTALDOL FORM      7.2
YR       DIM       2
GROSS    FORM      7.2
TOTAL    FORM      9.2
AR       FORM      9.2
CODENUM  FORM      2
EXCLPRT  DIM       4
.
PROGNAME DIM       8
CHKJUL   FORM      5
UNBILAMT FORM      9.2
FORM92   FORM      9.2
FORM52   FORM      5.2
.begin patch 3.0
.CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
.MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
.NUM10    FORM      10             NUMERIC WORK FIELD FOR CONVERSION.
.MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
.end patch 3.0
.
UNBILINC FORM      9.2
UNLNC30  FORM      9.2
UNLNC60  FORM      9.2
UNLNC90  FORM      9.2
UNLNC90P FORM      9.2
UNBILTOT FORM      12.2
LSTMTOT  FORM      9.2             TOTAL LIST MANAGEMENT DUE 30 DAYS
BRKEXTOT FORM      9.2             TOTAL BROKERAGE EXCHANGE DUE 30 DAYS
BRKRNTOT FORM      9.2             TOTAL BROKERAGE RENTAL DUE 30 DAYS
WRIT     FORM      1
printd   form     4
.
KEY      DIM       28
. .............................................................................
.
.
. PROGRAM VARIABLES
. .................
.
ANS      DIM       1
DATE     DIM       8
TIME     DIM       8
febdat   form      5
MDATE    FORM      5
JUNDATE  FORM      6
chkdate  form      6
chkdate2 dim       6
QTYCHK   FORM      9
CHKMM    FORM      2                  HOLDS MONTH PARAMETER
TENDOLL  FORM      "99999"
NINEDOLL FORM      "199999"
SEVDOLL  FORM      "300000"
JUNEDAT  INIT      "060191"
..............................................................................
CHKYR    FORM      2                  HOLDS YEAR PARAMETER
PASS     FORM      3                  FOR JOBBR = 1 PASS = 1 = batch
.                                     FOR JOBBR = 1 PASS = 2 = Broker orders
.                                     FOR JOBBR = 1 pass = 3 - Non broker.
.                                     FOR JOBBR = 2 NOT  USED.
FERROR   DIM       25                 ERROR MESSAGE DISPLAY FIELD.;
CHKMLR   DIM       4                  USED TO ELIMINATE DUP MAILER DISP.
COMPMLR  DIM       4
LINES    FORM      2
PAGE     FORM      5
result1  form      3
remain   form      3.1
PBREAK   FORM      "57"
SHIPPED  DIM       9
EXCHANGE DIM       9
COMSLCT  DIM       9
COUNTO   FORM      5                  NUMBER OF ORDERS READ.
COUNTO1  FORM      5                  NUMBER OF ORDERS CALCULATED.
COUNTI   FORM      5                  NUMBER OF INVOICES READ
COUNTI1  FORM      5                  NUMBER OF INVOICES CALCULATED
TOTALU   FORM      9.2
TDMCSW   DIM       1                  TRIPLEX INDICATOR Y=TRIPLEX, N=NOT
SPLITSW  DIM       1                  RENT/EXCHANGE SPLIT = 'Y'
SW30     DIM       1                  DUE FOR BILLING IN 30 DAYS = "Y"
SYSJDATE FORM      5
MRGE     DIM       8
AGEFLAG  FORM      1                  CHECK FOR MAILDATE AGE.
.
SPCL     DIM       2               .SPECIAL INSTRUCTION KEY
SPCL1    DIM       2           .SPECIAL INSTRUCTION CODE
SPCL2    DIM       2           .SPECIAL INSTRUCTION CODE
SPCL3    DIM       2           .SPECIAL INSTRUCTION CODE
SPCL4    DIM       2           .SPECIAL INSTRUCTION CODE
SPCL5    DIM       2           .SPECIAL INSTRUCTION CODE
SPCL6    DIM       2           .SPECIAL INSTRUCTION CODE
V1       FORM      1
WORK47   DIM       47
WK247    DIM       47
WORK06   DIM       6
holdstr  dim     758
line1    dim     127
line2    dim     127
line3    dim     127
line4    dim     127
line5    dim     127
line6    dim     127
line7    dim     127
CARR     INIT     0x7f
carrfill dim      2
lwrobb12 init     "2749-1822-0704-0974-1451-0127-0638"
TEXT1    DIM       47
VARFLAG  FORM      1         Var Flag 1 yes.
DIFF     FORM      10
TENPER   FORM      10
vqty     form      9
NFULCOMP  DIM       55
.
.Begin patch 3.0
PDFFLAG   Form      1
PPMPRT1   DIM       9
font8    font
        create  font8,"Times New Roman",size=8
font8i    font
        create  font8i,"Times New Roman",size=8,italic        
PrintFlag Form      1
FileCheck FIle
trapcount form      4
str9E     Dim       9
.end patch 3.0

. .............................................................................
. MAINLINE
. .............................................................................
         TRAP      ABORT IF F5
         MOVE      "EXIT" TO PF5
         MOVE      "NORD013D" TO PROGRAM
         MOVE      "UNBILLED REPORT" TO STITLE
         MOVE      "DATE" TO PF3
         MOVE      "Names in the News" TO COMPNME
.begin patch 3.0
         move      c1 to PDFFlag
         RESET     COMMENT
         SCAN      "PDF" IN COMMENT
         CALL      OPTPDF IF EQUAL
.end patch 3.0

.
          if        (inpname = "")
         MOVE      "NINORDU.SRT|10.10.30.103:502" TO NORDNAME
          else
          pack     Nordname from inpname,"|10.10.30.103:502"
          endif
         CLOCK     DATE TO DATE
         MOVE      DATE TO PDATE
         MOVE      PDATE TO TODAY
         UNPACK    PDATE INTO MM,ANS,DD,ANS,YY
         XIF
         CLOCK     TIME TO TIME
         CALL      PAINT
         CALL      FUNCDISP
         GOTO      TRAPS
.begin patch 3.0
OPTPDF   MOVE      C2 TO PDFFlag
          Move      "UnBill",prtname
         RETURN
.end patch 3.0

NOTHING  RETURN
KEYDATE
         KEYIN     *P10:12,"ENTER DATE ",*ZF,*JR,*EL,*+,MM,*DV,SLASH,DD:
                   *DV,SLASH,YY
         PACK      DATE FROM MM,DD,YY
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         MOVE      "99/99/99" TO PDATE
         EDIT      DATE TO PDATE
         CALL      PAINT
         CALL      FUNCDISP
         RETURN
TRAPS    TRAP      IO GIVING ERROR NORESET IF IO
         TRAP      KEYDATE IF F3
         DISPLAY   *P1:24,*EL,"OPENING FILES";
         TRAP      RANGE GIVING ERROR NORESET IF RANGE
         TRAP      FORMAT GIVING ERROR NORESET IF FORMAT
         TRAP      PARITY GIVING ERROR NORESET IF PARITY
         MOVE      "ninordu  Sorted Input FILE " TO FERROR
         open      nordfile,nordname
         MOVE      "MASTER  ON ACCOUNT FILE " TO FERROR
         move      c1 to nordflag
         SCAN      "VAR" IN COMMENT
         IF        EQUAL
         MOVE      C1 TO VARFLAG
                              endif
         PACK      STR35,NTWKPATH1,"unbill.LST"
.Begin patch 3.0
.Find Printer Aread
          move      portn to ncntfld1
          rep       zfill in ncntfld1
          Move      c3,NCNTPATH
          call      ncntkey
          if        over
                    move      c2 to cntprint
          endif
          move      CntPrint,PrintFlag

          call      GetWinVer
          if        (PDFFLAG = 2)
.begin Patch 3.01
                 
.          Call      GetPDFPath
.          Call      PDF995Auto
.          call      SetPDFFlag
.          PRTOPEN   Laser,"PDF995",prtname
.use build in pdf pring facility
          pack      str55 from "c:\work\pdf\",prtname,".pdf"
          PRTOPEN   Laser,"pdf:",str55
.end Patch 3.01
          pack    str45,prtname

          else
          
                    if (PrintFlag = C0 | PrintFlag = 1)     .Laser2 
                              if (osflag = c1 | Osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt win2k Xp
                                          PRTOPEN Laser,"\\NINS2\laser2",prtname
                                  elseif (osflag = c3 | OSflag =c4)         .win 95 98
                                          PRTOPEN Laser,"Laser2",prtname
                                  else   .(osflag = c0)         .Don't know prompt for printer
                                          PRTOPEN Laser,"-",prtname
                                  endif
                          elseif (PrintFlag = 2)  .Laser3
                                  if (osflag = c1 | osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt 2k xp
                                          PRTOPEN Laser,"\\NINS2\laser3 Blankstock",prtname
                                  elseif (osflag = c3 | osflag =c4)         .win 95 98
                                          PRTOPEN Laser,"Laser3 Blankstock",prtname
                                  else   .(osflag = c0)         .Don't know prompt for printer
                                          PRTOPEN Laser,"-",prtname
                                  endif
                          elseif (PrintFlag = 5)  .Susan
                                    if (osflag = c2)         .nt
                                            PRTOPEN Laser,"Kyocera FS-1030D",Str45
                                    elseif (osflag = c6 | osflag = c8 | osflag = c9)         .XP
                                            PRTOPEN Laser,"Kyocera FS-1030D",Str45
                                    elseif (osflag = c1 | osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .win 95 98
                                            PRTOPEN Laser,"KYOCERAS",Str45
                                    else   .(osflag = c0)         .Don't know prompt for printer
                                            PRTOPEN Laser,"@",Str45
                                    endif
                          elseif (PrintFlag = 7)  .david
                                    if (osflag = c2)         .nt +
                                            PRTOPEN Laser,"Kyocera FS-C5030N KX",Str45
                                    elseif (osflag = c6 | osflag = c8 | osflag = c9)         .XP
                                            PRTOPEN Laser,"Kyocera FS-C5030N KX",Str45
                                    elseif (osflag = c1 | osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .win 95 98
                                            PRTOPEN Laser,"KYOCERAM",Str45
                                    else   .(osflag = c0)         .Don't know prompt for printer
                                            PRTOPEN Laser,"@",Str45
                                    endif
                    endif
          endif

.         Call           GetWinVer
.               if             (osflag = c1 or Osflag = C5 or OsFlag = C6)         .nt or win2000 or Windows XP
.               splopen        "\\NINs2\Laser8","R"
.               Elseif         (osflag = c3 or OsFlag = C4)         .win 95 98
.               splopen        "\\NINs2\Laser8","R"
.               Elseif         (osflag = c0)         .Don't know prompt for printer
.               elseif (osflag = "9" )         .win 7
.               splopen        "\\NINs2\Laser8","R"
.               else     
.               splopen        "","R"
.               endif
.         print     hptop,hpdupl,HPtmsr17,*f
.          PRTPAGE   Laser;*NEWPAGE:
          PRTPAGE   Laser;*UNITS=*HIENGLISH:
                    *ORIENT=*PORTRAIT,*Font=Font8,*DUPLEX=2


.end patch 3.0
............................................................................................................

         move      c3 to ndatlock
         move      c3 to nmlrlock
         move      c3 to nspelock
............................................................................................................
.
         MOVE      "                    " TO FERROR
         KEYIN    *P1:24,*EL,"FILES OPEN 30 SEC'S TO CHANGE DATE",*T30,ANS;
         TRAP      NOTHING IF F3
         MOVE      B1 TO PF3
         CALL      FUNCDISP
         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         CALL      CVTJUL
         MOVE      JULDAYS TO SYSJDATE
         TRAP      ABORT IF F5
..............................................................................
.for testing only.............................................................
..............................................................................
.
START    MOVE      C0 TO LINES

         MOVE      C0 TO TOTALDOL
         MOVE      "D" TO ANS
         KEYIN     *P28:09,"(D)efault or (A)ll ",*T30,*RV,ANS
         REP       "D1A2" IN ANS
         MOVE      ANS TO JOBBR
         BRANCH    JOBBR OF START1,START1         >1 OR 2 ?
         GOTO      START                            NO!
.
START1
                              compare   c1 to varflag
                              if        equal
         MOVE      C2 TO PASS
                              else
                              move      c1 to pass
                              endif
.         move      c2 to pass
.patch2.80
         SUB       TOTALU FROM TOTALU
         SUB       TOTAL FROM TOTAL
         MOVE      C0 TO UNLNC90P
         MOVE      C0 TO UNLNC90
         MOVE      C0 TO UNLNC60
         MOVE      C0 TO UNLNC30
         SUB       AR FROM AR
         SUB       UNBILINC FROM UNBILINC
         SUB       COUNTO FROM COUNTO
         SUB       COUNTO1 FROM COUNTO1
         SUB       COUNTI FROM COUNTI
         SUB       COUNTI1 FROM COUNTI1
         Sub        Printd from Printd
.
GETREC
         CALL      NORD2SEQ
         GOTO      EXIT IF OVER
            If      (olrn = "627233")
            call    Debug
            endif
          IF (olrn = "539853")
TEST
                    RESET OLRN
          ENDIF
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
          move b15 to str15
          move b25 to nmoddesc

          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if not over
                    move      NSEL2NAME,O2DES

                              pack      NMODFLD,NSEL2DESC
                              rep       zfill,NMODFLD
                              move      "NMODKEY",Location
                              pack      KeyLocation,"Key: ",NMODFLD
                              call      NMODKEY
                              if over
                                        move      "/M",NMODDESC
                              else
                                        call      Trim using NMODDESC
                                        if (nmoddesc = "")
                                                  move      "/M",NMODDESC
                                        endif
                              endif
.
                    if (NSEL2SPRICE > C0)
                              unpack    NSEL2SPRICE,str5,str3
                              call      FormatNumeric using str5,str6
                              pack      str9,str6,str3
                              pack      str15," @ ",str9,NMODDESC
                              call trim using str15
                    endif

          else
                    move      "/M",NMODDESC
                    unpack    OPPM,str3,str2
                    pack      str6,str3,".",str2
                    rep       zfill,str6
                    move      str6,NSEL2PRICE
          endif
.
.         call      rotdial
         ADD       C1 TO COUNTO
         DISPLAY   *P1:24,*EL,COUNTO;
         CMATCH    "B" TO OSTAT    *BILLED?
         GOTO      GETREC IF EQUAL       .YES
.
         CMATCH    "p" TO OSTAT       Pending order ?
         GOTO      getrec IF EQUAL     YES, skip.
         CMATCH    "l" TO OSTAT       Pending order ?
         GOTO      getrec IF EQUAL     YES, skip.
         CMATCH    "z" TO OSTAT       cancelled LCR order ?
         GOTO      Getrec IF EQUAL     YES, skip.
.note cancodes also updated to skip cancelled pending orders.
.
         RESET     CANCODES               .RESET FORM POINTER.
         SCAN      OSTAT IN CANCODES       .CANCELLED?
         GOTO      GETREC IF EQUAL
         MOVE      OLON TO NOWNFLD
         REP       ZFILL IN NOWNFLD
         MOVE      C1 TO NOWNPATH
         CALL      NOWNKEY
          call      Trim using OFULLFIL
          if (OFULLFIL <> "")
                    pack      COMPFLD,OFULLFIL
                    call      zfillit using COMPFLD
                    move      C1,COMPPATH
                    move      "GETREC-COMPKEY",Location
                    pack      KeyLocation,COMPFLD
                    call      COMPKEY
                    if over
                              clear     COMPFLD
                              clear     NFULCOMP
                    else
                              move      COMPCOMP,NFULCOMP
                    endif
          else      // OFULLFIL = ""
                    clear     COMPFLD
                    clear     NFULCOMP
          endif
         PACK      STR2 FROM OSALES10,OSALES
         REP       ZFILL IN STR2
.
         MOVE      NO TO LSTMSW
          MOVE      C1 TO Nbrkpath
         PACK      nbrkfld FROM Obrknum,obrkcnt
         REP       ZFILL,nbrkfld
         clear     brsales
         CALL      nbrkkey
         move      c0 to n2
         move      brsales to n2
         compare   c6 to n2
         if        equal
         move       yes to lstmsw
         CLEAR      MCODE         .CLEAR VAR.
         ELSE
         MATCH     "00" TO STR2
         IF        EQUAL
         RESET     RUNCODES
         SCAN      OLNUM IN RUNCODES
         IF        NOT EQUAL
         MOVE      YES TO LSTMSW
         ENDIF
         ENDIF
         ENDIF
.
           IF       (str2 = "27" | str2 = "28" | str2 = "02" | str2 = "19")
           move   yes to lstmsw
           endif
.          
         MOVE      NO TO OVER
PREPOWN  MOVE      OLON TO NOWNFLD
         CALL      NOWNKEY
          call      Trim using OFULLFIL
          if (OFULLFIL <> "")
                    pack      COMPFLD,OFULLFIL
                    call      zfillit using COMPFLD
                    move      C1,COMPPATH
                    move      "PREPOWN-COMPKEY",Location
                    pack      KeyLocation,COMPFLD
                    call      COMPKEY
                    if over
                              clear     COMPFLD
                              clear     NFULCOMP
                    else
                              move      COMPCOMP,NFULCOMP
                    endif
          else      // FULLFIL = ""
                    clear     COMPFLD
                    clear     NFULCOMP
          endif
         CALL      MLRREAD
         BRANCH    PASS OF pass1,pass2,pass3
pass1
         move      c1 to pass
         DISPLAY   *P01:05,"PASS ",pass
         match     yes to lstmsw
         goto      getrec if equal
         cmatch    "B" to mcode
         goto      pass1a if equal
         cmatch    "A" to mcode
         goto      pass1a if equal
         goto      getrec
pass1a   MOVE      ORTNDTEM TO MM
         MOVE      ORTNDTED TO DD
         MOVE      ORTNDTEY TO YY
         clear     chkdate2
         pack      chkdate2 from ortndtem,ortndted,ortndtey
         move      c0 to chkdate
         move      chkdate2 to chkdate
              if            (chkdate = "0" or chkdate2 = "")
                            if            (ortnnum = "0001")
                            goto          contin                   .take it now it is a reuse
                            else
                        call          usemd
                            endif
              endif

         CALL      CVTJUL
         GOTO      CHKDAYS
pass2
         move      c2 to pass
         DISPLAY   *P01:05,"PASS ",pass
          cmatch    yes to lstmsw
         goto      getrec if not equal
         clear     mcode
         MOVE      ORTNDTEM TO MM                           .if batch
         MOVE      ORTNDTED TO DD                           .dlh/jd 01jun94
         MOVE      ORTNDTEY TO YY
                              compare   c1 to varflag
                              if        equal
                              move      c0 to vqty
                              move      oqty to vqty
                              call      readship
                              scan      "*SHIPPED*",SHIPPED
                                        If        Equal
                                        move      vqty to oqty
                                        MOVE      C0 TO N10
                                        Else
                                        GOto      Getrec
                                        Endif
          MOVE      oQTY TO N10
         DIV       C10 INTO N10
         MOVE      N10 TO TENPER
         MOVE      SQUANT TO N10
         MOVE      oQTY TO DIFF
         SUB       N10 FROM DIFF
         COMPARE   C0 TO DIFF
         CALL      NEG IF LESS
         COMPARE   DIFF TO TENPER
         IF         NOT GREATER
                              goto      okvar
                              elseIf    (Jobbr = c1)
                              goto      getrec               else we want all
                              endif
                              endif
Okvar
         clear     chkdate2
         pack      chkdate2 from ortndtem,ortndted,ortndtey
         move      c0 to chkdate
         move      chkdate2 to chkdate
              if            (chkdate = "0" or chkdate2 = "")
                            if            (ortnnum = "0001")
                            goto          contin                   .take it now it is a reuse
                            else
                        call          usemd
                            endif
              endif


         CALL      CVTJUL
         goto       chkdays
pass3
                              compare   c1 to varflag
                              goto      exit if equal
         cmatch    yes to lstmsw
         goto      getrec if equal
         cmatch    "B" to mcode
         goto      getrec if equal
         cmatch    "A" to mcode
         goto      getrec if equal
         MOVE      ORTNDTEM TO MM
         MOVE      ORTNDTED TO DD
         MOVE      ORTNDTEY TO YY
         clear     chkdate2
         pack      chkdate2 from ortndtem,ortndted,ortndtey
         move      c0 to chkdate
         move      chkdate2 to chkdate
              if            (chkdate = "0" or chkdate2 = "")
                            if            (ortnnum = "0001")
                            goto          contin                   .take it now it is a reuse
                            else
                        call          usemd
                            endif
              endif


         CALL      CVTJUL
         GOTO      CHKDAYS
.
CHKDAYS  MOVE      SYSJDATE TO CHKJUL
         ADD       C7 TO CHKJUL              PLUS 7 DAYS
         SUB       JULDAYS FROM CHKJUL
         COMPARE    CHKJUL TO C0                DATE GREATER THAN 7 DAYS AFTER
          If        Not Less
          scan      "*SHIPPED*" TO SHIPPED
                    If Not Equal
                              If        (Jobbr = c1)   .default job Else ALL
                              goto      Getrec
                              endif
                    endif
          Endif
         GOTO      CONTIN                        NO,PROCESS
.
USEMD    MOVE      OMDTEM TO MM
         MOVE      OMDTED TO DD
         MOVE      OMDTEY TO YY
         return
.
CONTIN
         CALL      DETAIL
         GOTO      GETREC               .ADDITIONAL CRITERIA FAILED GET NEXT RE
.
DETAIL
         MOVE      C0 TO FORM92
         MOVE      C0 TO UNBILAMT
         MOVE      C0 TO UNBILINC
         MOVE      C0 TO FORM52
         MOVE      C0 TO AR
         CALL      MLRREAD
         CALL      READSHIP            .ORDER SHIPPED????
         CALL      READMRG
         CALL      GETCARD
         CALL      READRTN
.
         SUB       FORM92 FROM FORM92
         SUB       FORM52 FROM FORM52
         MOVE      OQTY TO FORM92
         DIV       THOUS INTO FORM92
          MOVE      NSEL2PRICE,FORM52
         MULT      FORM52 BY FORM92
         MOVE      FORM92 TO UNBILINC
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES             EXCHANGE ?
         GOTO      OKEX IF EQUAL
         GOTO      RENT
.OKEX - CHECK FOR SPLIT RENTAL/EXCHANGE.
OKEX
         MOVE      C0 TO FORM92
         MOVE      OEXQTY TO FORM92
         COMPARE   C0 TO FORM92            PURE EXCHANGE ?
         IF        EQUAL                 YES.
         MOVE      C0 TO QTYCHK
         MOVE      NO TO SPLITSW
         MOVE      OQTY TO QTYCHK
         MOVE      QTYCHK TO FORM92
         CMATCH    YES TO LSTMSW
                   IF        EQUAL
                   MOVE      C0 TO UNBILINC
                   GOTO      OK
                   ELSE
                   MOVE      C0 TO UNBILINC
                   GOTO      OK
                   ENDIF
         ELSE
         MOVE      YES TO SPLITSW
         CMATCH    YES TO LSTMSW
                   IF        EQUAL
                   MOVE      C0 TO UNBILAMT
                   GOTO      RENTPART
                   ELSE
                   MOVE      C0 TO QTYCHK
                   MOVE      OEXQTY TO QTYCHK
                   MOVE      QTYCHK TO FORM92
                   GOTO      GETPRICE
                   ENDIF
         ENDIF
.
GETPRICE UNPACK    JUNEDAT INTO MM,DD,YY
         CALL      CVTJUL           .CONVERT JUNE 1ST'S DATE TO JULIAN
         MOVE      JULDAYS TO JUNDATE    .SAVE RESULT
         MOVE      OODTEM TO MM
         MOVE      OODTED TO DD
         MOVE      OODTEY TO YY
         CALL      CVTJUL           .CONVERT TODAY'S  DATE TO JULIAN
         MOVE      JULDAYS TO MDATE    .SAVE RESULT
         COMPARE   JUNDATE TO MDATE
         IF        NOT GREATER
         MOVE      C8 TO FORM52
         GOTO      CALCE
         ENDIF
         COMPARE   QTYCHK TO TENDOLL
         IF        NOT LESS
         MOVE      C10 TO FORM52
         GOTO      CALCE
         ENDIF
         COMPARE   QTYCHK TO NINEDOLL
         IF        NOT LESS
         MOVE      C9 TO FORM52
         GOTO      CALCE
         ENDIF
         COMPARE   SEVDOLL TO QTYCHK
         IF        NOT LESS
         MOVE      C7 TO FORM52
         ENDIF
         MOVE      C8 TO FORM52
CALCE
        reset      lwrobb12
        match      "0638" to obrknum             .lwrobb
        if         equal
        scan       omlrnum in lwrobb12
        if         equal
        move       "12" to form52
        endif
        endif
         DIVIDE    THOUS INTO FORM92
         MULTIPLY  FORM52 BY FORM92
         MOVE      FORM92 TO UNBILAMT
         CMATCH    YES TO SPLITSW
         IF        EQUAL
         GOTO      RENTPART
         ELSE
         MOVE      FORM92 TO UNBILINC
         GOTO      OK
         ENDIF

RENTPART MOVE      C0 TO FORM92          SPLIT RENT/EXCHANGE
         MOVE      C0 TO N9
         MOVE      OQTY TO FORM92
         MOVE      OEXQTY TO N9
         SUBTRACT  N9 FROM FORM92           GET RENTAL PORTION
         MULT      ".001" BY FORM92
         MOVE      "65.00" TO FORM52          .ESTIMATED $.   (USE DATACARD?)
         MULT      FORM52 BY FORM92
         CMATCH    YES TO LSTMSW
         IF        EQUAL
         MULT      ".1" BY FORM92
         ELSE
.         match      "0192" to obrknum
         match      "0**0" to obrknum           .cheating need to cleanup later
         IF         EQUAL
.         display    *p1:24,*el,"EPSILON!!!!!!!!!"
         match      "20" to commper
           if         equal
           MULT      ".1" BY FORM92
           goto      ok
           ENDIF
         match      "30" to commper
            if         equal
            MULT      ".2" BY FORM92
            goto      ok
            endif
         ENDIF
.oddball use 10% commission
         MULT      ".1" BY FORM92
         goto      ok
         endif
.
        CMATCH    YES TO LSTMSW
         IF        EQUAL
         MULT      ".1" BY FORM92
         ELSE
         move       c0 to n32      .DLH USE Datacard info 09Jul98
         move       commper to n32
         mult       ".01" by n32
         mult       n32,form92
         ENDIF
         ADD       FORM92 TO UNBILAMT
         MOVE      UNBILAMT TO UNBILINC
         GOTO      OK
.
GETCARD  MOVE      OLNUM TO NDATFLD
         CLEAR     EXCLPRT
         MOVE      C1 TO NDATPATH
         CALL      NDATKEY
         RETURN    IF OVER
         CMATCH    "C" TO ELSTCDE
         IF        EQUAL
         MOVE      "EXCL" TO EXCLPRT
         else
         cmatch    "P" to elstcde
         if        equal
         move      "PLIX" to EXCLPRT
         ENDIF
         endif
               call           Trim using Commper

          if (NDATCONV = "1")
                              pack      NSELFLD1,"01X",LSTNUM
.begin patch 3.0
.                              pack      NSELFLD2,"021XBASE"   .how long has this been wrong?
                              pack      NSELFLD2,"02XBASE"
                              Clear     Nselfld3
.end patch 3.0
                              move      "NSELAIM",Location
                              pack      KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD2
                              call      NSELAIM
                              if not over
                                        if (NSELEXC <> "2")
                                                  move      C0,FORM52
                                                  move      NSELPRICE,FORM52
                                        endif
                              else
                                        goto DataCheckText
                              endif
          else
DataCheckText
                    pack      NTXTFLD,LSTNUM,"1"
                    move      "NTXTKEY",Location
                    pack      KeyLocation,"Key: ",NTXTFLD
                    call      NTXTKEY
                    if not over
                              move      NTXTTEXT,text1
                              SCAN      "EXCHANGE ONLY" IN TEXT1
                              RETURN IF EQUAL                 NO USABLE $ RETURN
                              RESET     TEXT1
                              SCAN      "$" IN TEXT1
                              RETURN IF NOT EQUAL        NO USABLE $ RETURN
                              BUMP      TEXT1 BY 1
                              PACK      STR2 FROM TEXT1
                              MOVE      STR2 TO FORM52
                              SCAN      "$" IN TEXT1        .DO WE HAVE CORRECT PRICE?
                              RETURN IF NOT EQUAL            .YES.
                              CLEAR     STR2
                              BUMP      TEXT1 BY 1
                              PACK      STR2 FROM TEXT1       .NO, NOW WE DO!
                              MOVE      STR2 TO FORM52
                    else
                              clear     text1
                    endif
          endif
         RETURN
RENT     CMATCH    YES TO LSTMSW             LIST MANAGEMENT?
         IF        EQUAL
         MULT      ".1" BY UNBILINC            YES
         ELSE

        display     *p1:24,"brk",obrknum

         match      "0**0" to obrknum           .cheating need to cleanup later
           IF         EQUAL
           display    *p1:24,*el,"EPSILON!!!!!!!!!"
           match      "20" to commper
            if         equal
            MULT      ".1" BY UNbilinc
            goto      ok
            ENDIF

           match      "30" to commper
           if         equal
           MULT      ".2" BY UNbilinc
           display    *p1:24,*el,"Unbilinc ",Unbilinc,b1,commper
           goto      ok
           ENDIF
.oddball use 10% commission
          MULT      ".1" BY Unbilinc
          goto      ok
          endif

         move       c0 to n32      .DLH USE Datacard info 09Jul98
         move       commper to n32
         mult       ".01" by n32
         mult       n32,unbilinc
         ENDIF
..
OK
          match     "009406",OFULLFIL
          if equal
                    goto TDMCYES1
          else
                    goto USEMD1
          endif
TDMCYES1
         clear     chkdate2
         pack      chkdate2 from ortndtem,ortndted,ortndtey
         move      c0 to chkdate
         move      chkdate2 to chkdate
         compare   c0 to chkdate
         call       USEMD1 IF EQUAL
         MATCH     b6 TO chkdate2
         call       USEMD1 IF EQUAL
         call       USEMD1 IF Eos
         MOVE      ORTNDTEM TO MM
         MOVE      ORTNDTED TO DD
         MOVE      ORTNDTEY TO YY
         CALL      CVTJUL
         GOTO      CHKDAY1
USEMD1   MOVE      OMDTEM TO MM
         MOVE      OMDTED TO DD
         MOVE      OMDTEY TO YY
         CALL      CVTJUL
.
CHKDAY1  MOVE      SYSJDATE TO CHKJUL
.
         SUBTRACT  CHKJUL FROM JULDAYS
         MOVE      NO TO SW30
.
         COMPARE   C31 TO JULDAYS
         GOTO      BILL30 IF LESS          .SHOULD BE BILLED THIS MONTH.
.
         COMPARE   "61" TO JULDAYS         .SHOULD BE BILLED BY END NEXT MON.
         GOTO      BILL60 IF LESS
.
         COMPARE   "91" TO JULDAYS         .
         GOTO      BILL90 IF LESS
         ADD       UNBILINC TO UNLNC90P
         GOTO      OK1
BILL30   ADD       UNBILINC TO UNLNC30
         MOVE      YES TO SW30
         GOTO      OK1
.
BILL60   ADD       UNBILINC TO UNLNC60
         GOTO      OK1
.
BILL90   ADD       UNBILINC TO UNLNC90
         GOTO      OK1
.
OK1      MOVE      UNBILINC TO FORM92
         ADD       FORM92 TO TOTALU
         ADD       FORM92 TO TOTAL
         ADD       C1 TO COUNTO1
         MOVE      FORM92 TO AR
.
CHECK1   MATCH     YES TO SW30               DUE IN 30?
         IF        EQUAL                             YES
         MATCH     YES TO LSTMSW            LIST MANAGEMENT?
         IF        EQUAL
         ADD       UNBILINC TO LSTMTOT       YES
         ELSE
         RESET     EXCODES                        NO
         SCAN      OELCODE IN EXCODES           OK ITS BROKERAGE, EXCHANGE?
         IF        EQUAL
         ADD       UNBILINC TO BRKEXTOT          YES.
         ELSE
         ADD       UNBILINC TO BRKRNTOT          NO ITS RENTAL.
         ENDIF
         ENDIF
         ENDIF
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES
         GOTO      CHECKOK IF EQUAL
         CLEAR     EXCHANGE
         GOTO      DISSCOM
CHECKOK
         MOVE      "EXCHANGE" TO EXCHANGE
DISSCOM
         CLEAR     COMSLCT
         CMATCH    "C",OCOMSLCT
         IF        EQUAL
         MOVE      "COMSELECT" TO COMSLCT
         ENDIF
         CMATCH    "L",OCOMSLCT
         IF        EQUAL
         MOVE      "LIFESTYLE" TO COMSLCT
         ENDIF
         CMATCH    "I",OCOMSLCT
         IF        EQUAL
         MOVE      "IC SYSTEMS" TO COMSLCT
         ENDIF
         NORETURN
         
.check for maildate age if >= 2wks flag it.   dlh 20may93.
         MOVE      OMDTEM TO MM
         MOVE      OMDTED TO DD
         MOVE      OMDTEY TO YY
         CALL      CVTJUL
         MOVE      C0 TO AGEFLAG
         MOVE      SYSJDATE TO CHKJUL
.begin patch 2.83    if maildate is past at all flag
          if        (Juldays <=SysJDate)
          move      c1 to AgeFlag
          endif
.   if maildate is past at all flag
         GOTO      PRINT
.
MLRREAD  MOVE      C1 TO NMLRPATH
         PACK      MKEY FROM OMLRNUM,OCOBN
         REP       ZFILL,MKEY
         CALL      NMLRKEY
         CMATCH    "B" TO MCODE        .BATCH BILL ?
         IF         NOT EQUAL         .NO
         cmatch    "A" to mcode
         IF         NOT EQUAL         .NO
         CLEAR      MCODE         .CLEAR VAR.
         ENDIF
         endif
         RETURN
.
.
READSHIP CLEAR     SHIPPED
         MOVE      OLRN TO NSHPFLD
         CALL      NSHPKEY
         RETURN    IF OVER
         MOVE      "*SHIPPED*" TO SHIPPED
         MOVE      C0 TO N9
         MOVE      SQUANT TO N9
         COMPARE   C0 TO N9
         IF        NOT EQUAL
         MOVE      SQUANT TO OQTY
         ENDIF
         RETURN
READRTN  MATCH     ORTNNUM TO NRTNFLD
         IF        NOT EQUAL
         MOVE      ORTNNUM TO NRTNFLD
         CALL      NRTNKEY
         ENDIF
         RETURN
READMRG
         CLEAR     MRGE
         MOVE      OLRN TO NMRGFLD
         rep       zfill in nmrgfld
         MOVE      C1 TO NMRGPATH
         CALL      NMRGKEY
         RETURN    IF OVER
         MOVE      "*MERGED*" TO MRGE
         RETURN
.
BALAN
.MONEY ON ACCOUNT ?     .ADD IN THE FUTURE??????
         MOVE      MKEY TO CHKMLR
         RESET     MKEY TO 4
         APPEND    Z3 TO MKEY
         RESET     MKEY
         MOVE      MKEY TO NMObFLD
         move       c0,Balance
         Move       c1,Nmobpath
         CALL      NMOBKEY
         
         COMPARE   C0 TO BALANCE
         GOTO      BALAN1 IF EQUAL
         GOTO      BALAN1 IF NOT LESS
         MULT      SEQ BY BALANCE
         ADD       BALANCE TO TOTALDOL
BALAN1   MOVE      NO TO OVER
         CALL      NMOBKS
         GOTO      PRINT IF OVER
         CMATCH    YES TO OVER
         GOTO      PRINT IF EQUAL
         MOVE      MKEY TO COMPMLR
         MATCH     CHKMLR TO COMPMLR
         GOTO      BALAN1 IF NOT EQUAL
         MULT      SEQ BY BALANCE
         ADD       BALANCE TO TOTALDOL
          Goto      Print
.
............................................................
.
.begin patch 3.0
.CVT      ENDSET    CVTFLD                        CHECK LAST BYTE.
.         RESET     MPCHARS
.         SCAN      CVTFLD IN MPCHARS             IS IT A MINUSOVRPNCH?
.         GOTO      CVTMP IF EQUAL                YES.
.         RESET     CVTFLD                        NO.
.         TYPE      CVTFLD                        CHECK NUMERIC VALIDITY.
.         RETURN    IF EQUAL                      ITS OK.
.FORMERR  DISPLAY   *P1:23,*EL,*B,"FORMAT ERROR READING LR: ",OLRN
.         NORETURN                                POP THE STACK.
.         GOTO      GETREC                       GO BACK TO READ.
.CVTMP    REPLACE   MPCHANGE IN CVTFLD            CHANGE MP TO NUMBER.
.         RESET     CVTFLD
.         TYPE      CVTFLD                        VALID NUMERIC?
.         GOTO      FORMERR IF NOT EQUAL          NO.
.         MOVE      CVTFLD TO NUM10               MOVE INTO NUMERIC.
.         MULTIPLY  SEQ BY NUM10               CHANGE TO MINUS.
.         MOVE      NUM10  TO CVTFLD              MOVE BACK TO DIM.
.         RETURN
.end patch 3.0
.
.......................................................................
HEADER   ADD       C1 TO PAGE
          Move      "250",row
         branch    pass of hd1,hd2,hd3
hd1     
.begin patch 3.0
          if        (page > c1)
          Prtpage   Laser;*Newpage
          endif
          PrtPage   Laser;*p=1:row,"CONFIDENTIAL",*p=4250:row,*alignment=*center,"N I N   U N B I L L E D   'Batch'   O R D E R S   ",*alignment=*Left,*p=7250:row,"DATE:":
                    *alignment=*right,*p=8160:row,TODAY,*alignment=*left
          add       sixlpi,row
          PrtPAge   Laser;*alignment=*Left,*p=7250:row,"PAGE:":
                    *alignment=*right,*p=8160:row,PAGE,*ALIGNMENT=*LEFT          
          add       sixlpi,row
.          PRINT     *F,*l,*l,*1,"CONFIDENTIAL",*16,PROGRAM,*37,"N I N   U N B I L L":
.                   " E D   'Batch'   O R D E R S   A S   O F",b2,"DATE:",PDATE:
.                   *121,"PAGE ## ",PAGE
         goto      hdx
hd2      
          if        (page <> c1)
          Prtpage   Laser;*Newpage
          endif
          PrtPage   Laser;*p=1:row,"CONFIDENTIAL",*p=4250:row,*alignment=*center,"N I N   U N B I L L E D   'Broker'   O R D E R S   ",*alignment=*Left,*p=7250:row,"DATE:":
                    *alignment=*right,*p=8160:row,TODAY,*alignment=*left
          add       sixlpi,row
          PrtPAge   Laser;*alignment=*Left,*p=7250:row,"PAGE:":
                    *alignment=*right,*p=8160:row,PAGE,*ALIGNMENT=*LEFT          
          add       sixlpi,row
.          PRINT     *F,*l,*l,*1,"CONFIDENTIAL",*16,PROGRAM,*37,"N I N   U N B I L L":
.                   " E D   'Broker'   O R D E R S   A S   O F",b2,"DATE:",PDATE:
.                   *121,"PAGE ## ",PAGE
         goto      hdx
hd3     
          if        (page <> c1)
          Prtpage   Laser;*Newpage
          endif
          PrtPage   Laser;*p=1:row,"CONFIDENTIAL",*p=4250:row,*alignment=*center,"N I N   U N B I L L E D   O R D E R S   ",*alignment=*Left,*p=7250:row,"DATE:":
                    *alignment=*right,*p=8160:row,TODAY,*alignment=*left
          add       sixlpi,row
          PrtPAge   Laser;*alignment=*Left,*p=7250:row,"PAGE:":
                    *alignment=*right,*p=8160:row,PAGE,*ALIGNMENT=*LEFT          
          add       sixlpi,row
.          PRINT     *F,*l,*l,*1,"CONFIDENTIAL",*16,PROGRAM,*37,"N I N   U N B I L L":
.                   " E D   O R D E R S   A S   O F",*89,"DATE:",PDATE:
.                   *121,"PAGE ## ",PAGE
         goto      hdx
hdx      
          PrtPage   Laser;*p=1:row,"ORDER",*p=2000:row,"MAILER NAME",*p=5125:row,"LIST",*p6750:row,"MAIL",*p=7375:row,"RETURN"
          add       sixlpi,row
          PrtPage   Laser;*p=1:row,"DATE",*p=625:row,"MLR##",*p=1500:row,"LR",*p=2000:row,"OFFER",*p6750:row,"DATE",*p=7375:row,"DATE"
          add       sixlpi,row
          prtpage   laser;*p=1:row,*RPTCHAR "-":264
          add       SixLPI,row
.          print     *L,*1,"ORDER",*34,"MAILER NAME":
.                   *87,"LIST":
.                   *114,"MAIL",*124,"RETURN":
.                   *L,*1,"DATE",*11,"MLR##",*26,"LR":
.                   *34,"OFFER",*81,"##":
.                   *87,"OWNER NAME",*114,"DATE",*124:
.                   "DATE":
.                   *n,*1,*RPTCHAR,DASH:132
.         MOVE      C7 TO LINES
.end patch 3.0
         RETURN
.......................................................................
PRINT
.begin patch 3.0
          if        (row >= 10000)
          call      header
          endif
          if        (PAGE = c0)
          call      header
          endif

.         COMPARE   PBREAK TO LINES
.         CALL      HEADER IF NOT LESS
.         COMPARE   C0 TO LINES
.         CALL      HEADER IF EQUAL
         CLEAR     BRCOMP
         CLEAR     NBRKFLD
         PACK      NBRKFLD FROM OBRKNUM,OBRKCNT
         CALL      NBRKKEY
         if         over
         move      mccto to brcomp
         endif
.
          add        c1 to printd
          Call      Trim using RtCOmp
          IF        (rtcomp <> "")
          call      Debug
          PrtPage   Laser;*p=5125:row,"RTN-TO: ",rtcomp
.          PRint     *86,"RTN-TO: ",rtcomp
.          Add       C1,Lines
          add       sixlpi,row
          endif
          if        (row >= 10000)
          call      header
          call      Overrun
          endif
.          If        (lines >= Pbreak)
.          call      Header
.          call      Overrun
.          endif
.end patch 3.0
.begin patch 3.0
          PrtPage   Laser;*p=1:row,"MP##",Omlrky,*p=2000:row,brcomp,*p=4750:row,olon,*p=5125:row,ownocpy:
                   *p=7375:row,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY
          if        (ageflag = c1)
          PrtPage   Laser;*p6750:row,*BOLDON,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY,*BOLDOff
          else
          PrtPage   Laser;*p6750:row,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
          endif
          add       sixlpi,row

.        PRINT     *1,"MP##",Omlrky,*34,brcomp,*81,OLON,*86,OWNOCPY,*113,OMDTEM,SLASH:
.                   OMDTED,SLASH,OMDTEC,OMDTEY,*124,ORTNDTEM,SLASH,ORTNDTED:
.                   SLASH,ORTNDTEC,ORTNDTEY;
.          Add       C1,Lines
.         COMPARE   C1 TO AGEFLAG
.         IF        EQUAL
.         PRINT     *112,hpbon,STAR,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY,STAR,hpboff;
.         ENDIF
         call             Trim using NFULCOMP
          if        (row >= 10000)
          call      header
          call      Overrun
          endif
.          If        (lines >= Pbreak)
.          call      Header
.          call      Overrun
.          endif
.end patch 3.0
.          if (OCompID = "P" | OCompID2 = "P")
.                    move      " (PLI)",str6
.          else
.                    clear     str6
.          endif
          IF (ELSTCDE = "C")
.begin patch 3.0
          PrtPage   Laser;*p=1:row,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY:
                    *p=625:row,OMLRNUM,SLASH,OCOBN,*p=1130:row,*boldon,mcode,*boldoff:
                    *p=1500:row,Olrn,*p=2000:row,ordcname,*Boldon,Nfulcomp,*Boldoff
          add       sixlpi,row
.                    Print     *L,*1,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY:
.                              *12,OMLRNUM,SLASH,OCOBN,B1,*21,hpbon,MCODE,hpboff,*23,OLRN,str6:
.                              *34,ORDcNAME,*86,*ll,hpbon,NFULCOMP,hpboff;
          Else
          PrtPage   Laser;*p=1:row,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY:
                    *p=625:row,OMLRNUM,SLASH,OCOBN,*p=1130:row,*boldon,mcode,*boldoff:
                    *p=1500:row,Olrn,*p=2000:row,ordcname,Nfulcomp
          add       sixlpi,row
.                    Print     *L,*1,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY:
.                              *12,OMLRNUM,SLASH,OCOBN,B1,*21,hpbon,MCODE,hpboff,*23,OLRN,str6:
.                              *34,ORDcNAME,*86,*ll,NFULCOMP;
          EndIF
.          Add       C1,Lines
                    if        (row >= 10000)
                    call      header
                    call      Overrun
                    endif
.          If        (lines >= Pbreak)
.          call      Header
.          call      Overrun
.          endif
          Clear     ppmprt1
          pack      ppmprt1 from Form52,nmoddesc

          PrtPage   Laser;*p=10:row,*Boldon,Shipped,*Boldoff:
                    b3,MRGE,*p=2000:row,OFDESC,*p=4765:row,*Boldon,exclprt,*Boldoff,*p=5700:row,*Alignment=*right,Ppmprt1:
                    *Alignment=*Left,b1,*boldon,exchange,*Boldoff,*Alignment=*right,*p=7750:row,UNBILINC,*Alignment=*Left
          add       sixlpi,row
.         PRINT     *L,*12,HpBon,SHIPPED,HpBoff,*23,MRGE,*34,OFDESC:
.                   *81,HPBON,ExclPRt,HPBOFF,*86,FORM52,NMODDESC,HPBON,EXCHANGE,HPBOFF,*119,UNBILINC
.          Add       c1,lines
         ADD       UNBILINC TO UNBILTOT
                    if        (row >= 10000)
                    call      header
                    call      Overrun
                    endif
.         IFZ       PC
.          If        (lines >= Pbreak)
.          call      Header
.          call      Overrun
.          endif
.          PRINT     *1,"LIST - ",O1DES,B1,O2DES,B1,HPBON,str15,HPBOFF,b1,COMSLCT,*109,"QUANTITY ";
          add       sixlpi,row
.          Add       c1,lines
.
         MOVE      "ZZZZZZZZZ" TO STR9
         MOVE      C0 TO N9
         MOVE      OQTY TO N9
         EDIT      N9 TO STR9
.         PRINT     STR9;
         MOVE      "ZZZZZZZZZ" TO STR9e
         MOVE      C0 TO N9
         MOVE      OEXQTY TO N9
         EDIT      N9 TO STR9e
.         PRINT     "/",STR9e
.DLH 2013 04 02
.          PrtPage   Laser;*p=1:row,"LIST - ",O1DES,B1,O2DES,B1,*BoldON,str15,*BoldOFF,b1,COMSLCT,*p=6500:row,"QTY: ",STR9,"/",str9e
          PrtPage   Laser;*p=1:row,"LIST - ",O1DES,B1,NSEL2NAME,B1,*BoldON,str15,*BoldOFF,b1,COMSLCT,*p=6500:row,"QTY: ",STR9,"/",str9e
          add       sixlpi,row
.end patch 3.0
         clear     media
         CMATCH    B1,OFOCODE
         GOTO      DISMED2 IF NOT EQUAL
         GOTO      ship
DISMED2  MOVE      C0 TO N2
         TYPE      OFOCODE
         GOTO      MED10 IF NOT EQUAL
         MOVE      OFOCODE TO N2
         GOTO      DIS27
MED10    REP       "A0B1C2D3E4F5G6H7I8J9" IN OFOCODE
         TYPE      OFOCODE
         GOTO      MED20 IF NOT EQUAL
         MOVE      OFOCODE TO N2
         ADD       C10 TO N2
         GOTO      DIS27
MED20    REP       "K0L1M2N3O4P5Q6R7S8T9" IN OFOCODE
         TYPE      OFOCODE
         GOTO      MED30 IF NOT EQUAL
         MOVE      OFOCODE TO N2
         ADD       "20" TO N2
         GOTO      DIS27
MED30    REP       "U0V1X2Y3Z4" IN OFOCODE
         MOVE      OFOCODE TO N2
         ADD       "30" TO N2
DIS27    MOVE      MED0 TO MEDIA
         LOAD      MEDIA FROM N2 OF MED1,MED2,MED3,MED4,MED5:
                   MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
                   MED15,MED16,MED17,MED18,MED19,MED20,MED21,MED22:
                   MED23,MED24,MED25,MED26,MED27,MED28,MED29
ship     move      c0 to n2
         clear     shipdesc
         match    b2 to oshp
         goto      shipblnk if equal
         MOVE      OSHP TO N2
         move      ship0 to shipdesc
         load      shipdesc from N2 OF SHIP1,SHIP2,SHIP3,SHIP4,SHIP5,SHIP6:
                            SHIP7,SHIP8,SHIP9,ship10
         goto     shipprt
SHIPBLNK move     "NOTHING" to shipdesc
.
shipprt
                    if        (row >= 10000)
                    call      header
                    call      Overrun
                    endif
.          If        (Lines >= Pbreak)
.         CALL      HEADER
.         call      overrun
.         endif
          call      Trim using Media
          PrtPage   Laser;*p=1:row,"Ship: ",MEDIA,*p=3500:row,"Via: ",shipdesc
.          print     *1,"Ship: ",MEDIA,*60,"Via: ",shipdesc
          add       sixlpi,row
.         add        c1 to lines
         move       c0 to n2
         move       onetper to n2
         compare    c0 to n2
         if         not equal
           cmatch    no to onetfm
            if        equal
                    if        (row >= 10000)
                    call      header
                    call      Overrun
                    endif
.                    If        (Lines >= Pbreak)
.                    CALL      HEADER
.                    call      overrun
.                    endif
          PrtPage   Laser;*p=235:row,"Per List Owner - Gross Billing No Deductionss"
.          print     *4,"Per List Owner - Gross Billing":
.                      " No Deductionss",*l
.         add        c1 to lines
          add       sixlpi,row
            endif
          cmatch     "F" to onetfm
           if         not equal
                    if        (row >= 10000)
                    call      header
                    call      Overrun
                    endif
.                    If        (Lines >= Pbreak)
.                    CALL      HEADER
.                    call      overrun
.                    endif
          PrtPage   Laser;*p=235:row,"Mailer Guarantees ",onetper,"% payment on Gross Names Shipped"
          add       sixlpi,row
          PrtPage   Laser;*p=235:row,"& will pay $",onetrc,"/m running charge on: unused names."
          add       sixlpi,row
.          print      *4,"Mailer Guarantees ",onetper:
.                      "% payment on Gross Names Shipped":
.                      *n,*4,"& will pay $",onetrc,"/m running charge on":
.                      " unused names."
.         add        c2 to lines
          else
                    if        (row >= 10000)
                    call      header
                    call      Overrun
                    endif
.          If        (Lines >= Pbreak)
.                    CALL      HEADER
.                    call      overrun
.                    endif
          PrtPage   Laser;*p=235:row,onetper,"% Volume Discount"
          add       sixlpi,row
.          print      *4,onetper,"% Volume Discount":
.                      *n
.         add        c1 to lines
           endif
         else
         endif
          clear     line1
          pack      NSEL3FLD1,"01X1",OLRN
          move      "NSEL3AIM",Location
          pack      KeyLocation,"Key: ",NSEL3FLD1
          call      NSEL3AIM
          loop
                    until over
                    clear     taskname
                    if (NSEL3CODE = "A")
                              pack      NADDFLD,OLNUM,NSEL3NUM
                              move      "NADDKEY",Location
                              pack      KeyLocation,"Key: ",NADDFLD
                              call      NADDKEY
                              if not over
                                        pack      NREFFLD,"A",NADDNUM
                                        move      "NREFKEY",Location
                                        pack      KeyLocation,"Key: ",NREFFLD
                                        call      NREFKEY
                                        call      Trim using NREFDESC
                                        if (NSEL3PRICE = 0)
                                                  pack      taskname,NREFDESC
                                        else
                                                  pack      NMODFLD,NADDDESC
                                                  rep       zfill,NMODFLD
                                                  move      "NMODKEY",Location
                                                  pack      KeyLocation,"Key: ",NMODFLD
                                                  call      NMODKEY
                                                  call      Trim using NMODDESC
                                                  unpack    NSEL3PRICE,str5,str3
                                                  call      FormatNumeric using str5,str6
                                                  pack      str9,str6,str3
                                                  pack      taskname,NREFDESC,AT,str9,NMODDESC
                                        endif
                              endif
                    elseif (NSEL3CODE = "L")
                              pack      NSLTFLD,OLNUM,NSEL3NUM
                              move      "NSLTKEY",Location
                              pack      KeyLocation,"Key: ",NSLTFLD
                              call      NSLTKEY
                              if not over
                                        pack      NREFFLD,"L",NSLTNUM
                                        move      "NREFKEY-2",Location
                                        pack      KeyLocation,"Key: ",NREFFLD
                                        call      NREFKEY
                                        call      Trim using NREFDESC
                                        if (NSEL3PRICE = 0)
                                                  pack      taskname,NREFDESC
                                        else
                                                  pack      NMODFLD,NSLTDESC
                                                  rep       zfill,NMODFLD
                                                  move      "NMODKEY-2",Location
                                                  pack      KeyLocation,"Key: ",NMODFLD
                                                  call      NMODKEY
                                                  call      Trim using NMODDESC
                                                  unpack    NSEL3PRICE,str5,str3
                                                  call      FormatNumeric using str5,str6
                                                  pack      str9,str6,str3
                                                  pack      taskname,NREFDESC,AT,str9,NMODDESC
                                        endif
                              endif
                    endif
                    movelptr line1,result
                    movelptr taskname,howmany
                    add       result,howmany,N10
                    if (N10 > 124)
                              reset     line1
                              call      SPCLNSTO
                              clear     line1
                    elseif (taskname <> "")
                              append    taskname,line1
                              append    ", ",line1
                    endif
                    move      "NSEL3KG",Location
                    call      NSEL3KG
          repeat
          if (line1 <> "")
                    reset     line1
                    call      SPCLNSTO
                    clear     line1
          endif
         move      olrn to nspefld
         call      nspekey
...................................................................................
         if             over
                              pack      NSPE3FLD,OLRN
                              move      C3,NSPE3LOCK
                              move      "O.LoadScreens-NSPE3KEY",Location
                              pack      KeyLocation,"Key: ",NSPE3FLD
                              call      NSPE3KEY
                              pack    holdstr,desc005
                          pack      carrfill,CARR,B1
                          rep       carrfill,holdstr
                          call      TRIM using holdstr
                          goto      parser
           endif

         pack      holdstr,DESC001,DESC002
         pack      carrfill,CARR,B1
         rep       carrfill,holdstr
         call      TRIM using holdstr
parser
         call      PARSITUP using line1,holdstr,C1
         call      PARSITUP using line2,holdstr,C1
         call      PARSITUP using line3,holdstr,C1
         call      PARSITUP using line4,holdstr,C1
         call      PARSITUP using line5,holdstr,C1
         call      PARSITUP using line6,holdstr,C1
         call      PARSITUP using line7,holdstr,C1
         CALL      SPCLNSTO                           SPEC INSTRUC ROUTINE
         Clear     Line1
         MOVE      line2,line1
         CALL      SPCLNSTO
         Clear     Line1
         MOVE      line3,line1
         CALL      SPCLNSTO
         Clear     Line1
         MOVE      line4,line1
         CALL      SPCLNSTO
         Clear     Line1
         MOVE      line5,line1
         CALL      SPCLNSTO
         Clear     Line1
         MOVE      line6,line1
         CALL      SPCLNSTO
         Clear     Line1
         MOVE      line7,line1
         CALL      SPCLNSTO
         Clear     Line1
         Clear     Line2
         Clear     Line3
         Clear     Line4
         Clear     Line5
         Clear     Line6
         Clear     Line7
         GOTO      ADDLNE
.
. ROUTINE FOR SPECIAL INSTRUCTION PRINT
.
SPCLNSTO
.line1 will be cleared in PARSITUP if necessary.  Calling Trim will reset past values!!!
         if (line1 <> "")
                    if        (row >= 10000)
                    call      header
                    call      Overrun
                    endif
.                    If        (Lines >= Pbreak)
.                    CALL      HEADER
.                    cALL      Overrun
.                    endif
          PrtPage   Laser;*p=235:row,"**  ",line1
          add       sixlpi,row
.                   PRINT     *4,"**",*7,line1
.                   add       c1 to lines
         endif
         return
         XIF
         IFNZ      PC
.          If        (Lines >= Pbreak)
.                    CALL      HEADER
.                    cALL      Overrun
.                    endif
                    if        (row >= 10000)
                    call      header
                    call      Overrun
                    endif
.        PRINT     *7,"LIST - ",O1DES,B1,O2DES,B1,HPBON,str15,HPBOFF,b1,COMSLCT,*109,"QUANTITY ";
         MOVE      "ZZZZZZZZZ" TO STR9
         MOVE      C0 TO N9
         MOVE      OQTY TO N9
         EDIT      N9 TO STR9
.         PRINT     STR9;
         MOVE      "ZZZZZZZZZ" TO STR9
         MOVE      C0 TO N9
         MOVE      OEXQTY TO N9
         EDIT      N9 TO STR9
         PrtPage   Laser;*p=7500:row,*alignment=*Left,"/",STR9
          add       sixlpi,row
         PrtPAge    Laser;*p=1:row,*rptchar "-":264 
          add       sixlpi,row
.         PRINT     "/",STR9:
.                   *L,*1,"------------------------------":
.                   "------------------------------":
.                   "------------------------------":
.                   "------------------------------":
.                   "------------"
         XIF
addlne
                    if        (row >= 10000)
                    call      header
                    goto      getrec
                    endif

.          If        (Lines >= Pbreak)
.                    CALL      HEADER
.                    goto      getrec
.                    endif

         PrtPAge    Laser;*p=1:row,*rptchar "-":264 
          add       sixlpi,row
.         print      *1,*RPTCHAR,DASH:132
.         add        c1,lines

         GOTO      GETREC
.
overrun   PrtPage   Laser;*p5:row,"(continued from previous page)"
          add       sixlpi,row
.          print     *2,"(continued from previous page)"
.         add       c1 to lines
         return
..............................................................................
.  EXIT
..............................................................................
.
NEG      MULT      SEQ BY DIFF
         RETURN

EXIT     BRANCH    PASS OF EXIT1A,EXIT1B,exit1c
EXIT1A
          add       sixlpi,row
          if        (row >= 10000)
          call      header
          endif
.         COMPARE   c0 TO LINES
.         CALL      HEADER IF equal
.         COMPARE   PBREAK TO LINES
.         CALL      HEADER IF NOT LESS
          PrtPage   Laser;*p=1:row,"TOTAL UNBILLED INCOME - ",UNBILTOT
          add       sixlpi,row
          PrtPage   Laser;*p=1:row,"THIS REPORT DOES NOT INCLUDE ANY UNBILLED ORDERS WITH A RETURN DATE GREATER THAN *7* DAYS AFTER THE":
                   " REPORT DATE."
          add       sixlpi,row
          PrtPage   Laser;*p=1:row,"IF THE RETURN DATE IS BLANK THEN THE MAIL-DATE IS USED."
          add       sixlpi,row
.          PRINT     *L,*1,"TOTAL UNBILLED INCOME - ",UNBILTOT:
.                   *L,*1,"THIS REPORT DOES NOT INCLUDE ANY UNBILLED ORDERS":
.                   " WITH A RETURN DATE GREATER THAN *7* DAYS AFTER TO THE":
.                   " REPORT DATE.",*L:
.                   *1,"IF THE RETURN DATE IS BLANK THEN THE MAIL-DATE IS USED."
          PrtPage   Laser;*p=1:row,"START TIME ",TIME
.         PRINT     *L,"START TIME ",TIME
         CLOCK     TIME TO TIME
          PrtPage   Laser;*p=2000:row,COUNTO,"- ORDERS EXAMINED",printd,"-  Orders Printed        END TIME   ",TIME
.          PRINT     *L,"END TIME   ",TIME," ",COUNTO,"- ORDERS EXAMINED",printd,"-  Orders Printed"
         SUB       COUNTO FROM COUNTO
          Sub       Printd,Printd
         move      page to remain
         div       c2 into remain
         move      c0 to result1
         add       remain to result1
         compare   result1 to remain
.         display   *p1:24,remain,b1,result1,*w5
         if        not equal
          PrtPage   Laser;*newpage
.         print     *f
         endif
         MOVE      C0 TO UNBILTOT
         MOVE      C0 TO LINES
         MOVE      C0 TO PAGE
         MOVE      "ORDER FILE " TO FERROR
         CLOSE     NORDFILE
         MOVE      C0 TO NORDFLAG
         move      c2 to pass
.DH
          MOve      c2,Varflag
.end DH
         DISPLAY   *P01:05,"PASS ",pass
         GOTO      GETREC
EXIT1B   
          add       sixlpi,row
          if        (row >= 10000)
          call      header
          endif
.          COMPARE   PBREAK TO LINES
.         CALL      HEADER IF NOT LESS
.         COMPARE   c0 TO LINES
.         CALL      HEADER IF equal
          PrtPage   Laser;*p=1:row,"TOTAL UNBILLED INCOME - ",UNBILTOT
          add       sixlpi,row
          PrtPage   Laser;*p=1:row,"THIS REPORT DOES NOT INCLUDE ANY UNBILLED ORDERS WITH A RETURN DATE GREATER THAN THE":
                   " REPORT DATE."
          add       sixlpi,row
          PrtPage   Laser;*p=1:row,"IF THE RETURN DATE IS BLANK THEN THE MAIL-DATE IS USED."
          add       sixlpi,row
          PrtPage   Laser;*p=1:row,"START TIME ",TIME
         CLOCK     TIME TO TIME
          PrtPage   Laser;*p=2000:row,COUNTO,"- ORDERS EXAMINED",printd,"-  Orders Printed        END TIME   ",TIME

.         PRINT     *L,*1,"TOTAL UNBILLED INCOME - ",UNBILTOT:
.                   *L,*1,"THIS REPORT DOES NOT INCLUDE ANY UNBILLED ORDERS":
.                   " WITH A Return Date 7 DAYS GREATER THAN THE REPORT DATE.":
.                 *l,*1,"Unless we have shipping information":         
.                   *l,*1,"IF THE RETURN DATE IS BLANK THEN THE MAIL-DATE IS USED."
.         PRINT     *L,"START TIME ",TIME
.         CLOCK     TIME TO TIME
.         PRINT     *L,"END TIME   ",TIME," ",COUNTO,"- ORDERS EXAMINED",printd,"-  Orders Printed"
         move      page to remain
         div       c2 into remain
         move      c0 to result1
         add       remain to result1
         compare   result1 to remain
         if        not equal
          PrtPage   Laser;*newpage
.         print     *f
         endif
                              compare   c1 to varflag
                              goto      exit3 if equal
          Sub       Printd,Printd
         SUB       COUNTO FROM COUNTO
         MOVE      C0 TO UNBILTOT
         MOVE      C0 TO LINES
         MOVE      C0 TO PAGE
         MOVE      "ORDER FILE " TO FERROR
         CLOSE     NORDFILE
         MOVE      C0 TO NORDFLAG
         move      c3 to pass
         DISPLAY   *P01:05,"PASS ",pass
         GOTO      GETREC
EXIT1c
          add       sixlpi,row
          if        (row >= 10000)
          call      header
          endif
.         COMPARE   c0 TO LINES
.         CALL      HEADER IF equal
.         COMPARE   PBREAK TO LINES
.         CALL      HEADER IF NOT LESS
          PrtPage   Laser;*p=1:row,"TOTAL UNBILLED INCOME - ",UNBILTOT
          add       sixlpi,row
          PrtPage   Laser;*p=1:row,"THIS REPORT DOES NOT INCLUDE ANY UNBILLED ORDERS WITH A RETURN DATE GREATER THAN *7* DAYS AFTER THE":
                   " REPORT DATE."
          add       sixlpi,row
          PrtPage   Laser;*p=1:row,"IF THE RETURN DATE IS BLANK THEN THE MAIL-DATE IS USED."
          add       sixlpi,row
          PrtPage   Laser;*p=1:row,"START TIME ",TIME
         CLOCK     TIME TO TIME
          PrtPage   Laser;*p=2000:row,COUNTO,"- ORDERS EXAMINED",printd,"-  Orders Printed        END TIME   ",TIME,*Newpage
.         PRINT     *L,*1,"TOTAL UNBILLED INCOME - ",UNBILTOT:
.                   *L,*1,"THIS REPORT DOES NOT INCLUDE ANY UNBILLED ORDERS":
.                   " WITH A RETURN DATE GREATER THAN *7* DAYS AFTER THE":
.                   " REPORT DATE.",*L:
.                   *1,"IF THE RETURN DATE IS BLANK THEN THE MAIL-DATE IS USED."
.         PRINT     *L,"START TIME ",TIME
.         CLOCK     TIME TO TIME
.         PRINT     *L,"END TIME   ",TIME," ",COUNTO,"- ORDERS EXAMINED",printd,"-  Orders Printed"
.         PRINT     *F
         goto      exit3
EXIT3    BEEP
          PrtClose  Laser
.         SPLCLOSE
          call      Trim using User
          if        (User = "")
          move      "GemmaSpranza",User
          endif
          if        (pdfflag = c2)
.begin Patch 3.01
.          Call      PDF995Auto0
.end Patch 3.01
          move    "Here is your Unbilled Report in PDF",MailSubjct
          pack    MailBody,"Input File:  ",INPNAME
          pack      MailBOdy,"Input File:  ",INPNAME
          Pack      MailTO from User,"@nincal.com"
          Pack      MailFrom from User,"@nincal.com"
          Pack      MailAttach from "c:\work\PDF\",prtname,".pdf"
          Move      c0,TrapCount                   .reset
       
CheckFile

            trap      WaitForEnd giving error if IO
            open      FileCheck,MailAttach,Exclusive          
            Close     FIleCHeck
            call      SendMail
            Pause   "2"
          Erase     MailAttach
          endif

         release
         shutdown  "cls"
ABORT    TRAPCLR   F5
          add       Sixlpi,Row
          Prtpage   Laser;*p1:row,*Boldon,"*****JOB ABORTED BY OPERATOR*****",*Boldoff
.         PRINT     *L,"*****JOB ABORTED BY OPERATOR"
         GOTO      EXIT3
..............................................................................
.  ERROR SUBROUTINES
..............................................................................
.
IO
         TRAPCLR   IO
         NORETURN
         DISPLAY   *P1:23,*EL,FERROR," NOT ON LINE",*B,*B,*B:
                   *P1:24,*EL,"ERROR = ",ERROR
         DISPLAY   *P1:24,*EL,"IO ERROR INFORM COMPUTER PERSONNEL !!!";
         BEEP
          add       Sixlpi,Row
          Prtpage   Laser;*p1:row,*Boldon,"*** JOB ABORTED - I/O ERROR",*Boldoff
.         PRINT     *L,"*** JOB ABORTED - I/O ERROR"
         KEYIN     *P70:24,*EOFF,ANS;
         CMATCH    "Q",ANS
         GOTO      EXIT IF EQUAL
         GOTO      IO
RANGE
         TRAPCLR   RANGE
         NORETURN
         DISPLAY   *P1:24,*EL,"RANGE ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
          add       Sixlpi,Row
          Prtpage   Laser;*p1:row,*Boldon,"*** JOB ABORTED - RANGE ERROR",*Boldoff
.         PRINT     *L,"*** JOB ABORTED - RANGE ERROR"
         KEYIN     *P70:24,*EOFF,ANS;
         CMATCH    "Q",ANS
         GOTO      EXIT IF EQUAL
         GOTO      RANGE
FORMAT
         TRAPCLR   FORMAT
         NORETURN
         DISPLAY   *P1:24,*EL,"FORMAT ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
          add       Sixlpi,Row
          Prtpage   Laser;*p1:row,*Boldon,"*** JOB ABORTED - FORMAT ERROR",*Boldoff
.         PRINT     *L,"*** JOB ABORTED - FORMAT ERROR"
         KEYIN     *P70:24,*EOFF,ANS;
         CMATCH    "Q",ANS
         GOTO      EXIT IF EQUAL
         GOTO      FORMAT
PARITY
         TRAPCLR   PARITY
         NORETURN
         DISPLAY   *P1:24,*EL,"PARITY ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
          add       Sixlpi,Row
          Prtpage   Laser;*p1:row,*Boldon,"*** JOB ABORTED - PARITY ERROR",*Boldoff
.         PRINT     *L,"*** JOB ABORTED - PARITY ERROR"
         KEYIN     *P70:24,*EOFF,ANS;
         CMATCH    "Q",ANS
         GOTO      EXIT IF EQUAL
         GOTO      PARITY
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
.                   if        (trapcount > 60)   . 5 min are you kidding me
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Nord0003 - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
                    Move      "dherric@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
                    Move      "dherric@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    mailattach,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return
                    endif
                    goto      checkfile




         INCLUDE   NDATIO.inc
          INCLUDE   COMPIO.inc
          INCLUDE   CNTIO.inc
         INCLUDE   NSHPIO.inc
         INCLUDE   NORD2IO.inc
         INCLUDE   NOWNIO.inc
         INCLUDE   NMRGIO.inc
         INCLUDE   NMOAIO.inc
         INCLUDE   NMOBIO.inc
         include   nrtnio.inc
         include   nspeio.inc
         include   nofrio.inc
          INCLUDE   NSELIO.INC
          INCLUDE   NSEL2IO.INC
          INCLUDE   NTXTIO.INC
          INCLUDE   NSEL3IO.INC
          INCLUDE   NADDIO.INC
          INCLUDE   NSLTIO.INC
          INCLUDE   NREFIO.INC
          INCLUDE   NMODIO.INC
          include   nspe3io.inc
.Begin patch 3.0
          include   NCntio.inc
          include   Nuseio.inc
.end patch 3.0
        INCLUDE   COMLOGIC.inc
