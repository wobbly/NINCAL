PC       EQU       0
         INC       COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   CONSacct.inc
         INCLUDE   NORDDD.inc
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
         INCLUDE   NBILDD.inc
         INCLUDE   NJSTDD.inc
         include   ndatdd.inc
         INCLUDE   NOWNDD.inc
         INCLUDE   NAcdDD.inc
         INCLUDE   NADJDD.inc
         INCLUDE    ninvdd.inc
         include    Ninvacddd.inc
         include   ndat3dd.inc
         INCLUDE   NPAYDD.INC
         include   nmrgdd.inc
         include   nshpdd.inc
          INclude   Nadjres.inc
          INCLUDE   NSEL2DD.INC
         INCLUDE   HP.INC
release   init    "4.23"       DLH REmove PLI
RElDate   Init      "8 May 2010"
.release   init    "4.22"       DLH use New line instead of Carriage Return Word 2007 treats CR as new paragragh
.                                       we want Sift+enter newline
.RElDate   Init      "18 May 2009"
.release   init      "4.22"    DLH if MIA fmrptr1 causes error
.reldate   Init      "10 November 08"
.release  init      "4.21"    DLH Fix short pay logo
.reldate  Init      "10 November 08"
.release  init      "4.2"     DLH MOve adj codes to INclude
.reldate  Init      "23 September 08"
.release  init      "4.1"     DLH 1AUG2007 pli
.release  init      "4.0"     DLH 1AUG2007 pli
.release init    "3.9"        DLH 30Jul2007 code 29
.release init    "3.84"       DLH 6Jun2007 Oslspern update
.release init    "3.83"       ASH 16MAY2006 RECONCILIATION OF LANGUAGE BETWEEN:  NCSH002A, NORD002L, NINV002L, NADJ002L, NORD0024, NORD024B
.*** SEE Nadj002l.3.9.pls for prior documenation
.begin patch 4.21
FrmPtr1   Form      ^
.end patch 4.21
.end patch 4.22

GreyFIll       Color
NoFIll         Color                   .White
colornum form       24
.end patch 4.1
COMNT    DIM       25
COMNT1   DIM       21
REASON   FORM      1
ADJDATE  DIM       8
INVDATE  DIM       8
DATE     DIM       8
LOCAL    INIT      "LOCAL"
HOTFLAG  FORM       1                 "1=daily print, 2=hot print"
PRTFLAG  DIM       1
MAILDATE DIM       10
apflag   form      1
ARMASK   DIM       15
AP1MASK  DIM       15
AP2MASK  DIM       15
M$GROSS  DIM       15
PSMASK   DIM       9
STMASK   DIM       10
CTYMASK  DIM       10
LRMASK   DIM       11
TYPIST   DIM       2             TYPIST INITIALS
amendno  form      2
shipsw    dim       1
mrgsw     dim       1
.
.ADj CHARGE DESCRIPTION.
.
nadjtext  DIM       35
adjdesc1  DIM       35
adjdesc2  DIM       35
adjdesc3  DIM       35
adjdesc4  DIM       35
adjdesc5  DIM       35
adjdesc6  DIM       35
adjdesc7  DIM       35
adjdesc8  DIM       35
adjdesc9  DIM       35
. TOTAL ADj CHARGE
.
adj$1    DIM       15
adj$2    DIM       15
adj$3    DIM       15
adj$4    DIM       15
adj$5    DIM       15
adj$6    DIM       15
adj$7    DIM       15
adj$8    DIM       15
adj$9    DIM       15
MASK9    INIT      "ZZZ,ZZZ,ZZZ"
M$AddQTY    DIM       11
M$QTY    DIM       11
M$QTYx   DIM       11
MASK72   INIT      "Z,ZZZ,ZZZ.ZZ-"
MASK62   INIT      "ZZZ,ZZZ.ZZ-"
MASK52   INIT      "ZZ,ZZZ.ZZ-"
MASK42   INIT      "Z,ZZZ.ZZ-"
MASK7    INIT      "Z,ZZZ,ZZZ"
M$AR     DIM       15
M$PPM    DIM       6
M$PPMx   DIM       6
M$AP1    DIM       13
SHORT    DIM       59      *NET/SHORT INSTRUCTIONS.
GUARPRT  DIM       34      *GUARANTY PRINT LINE.
FORMFLAG FORM      1      1=MAILER, 2=OWNER/manager
FORM2    FORM      2
ELEVEN   FORM      "11"
FIFTY1   FORM      "51"
formar1  form      9.2
.
HUNDRD   FORM      "100"
FORM52   FORM      5.2
FORM42   FORM      4.2
FORM62   FORM      6.2
FORMAP1  FORM      9.2
TIME     DIM       8
COUNT    FORM      5
CAREOF   DIM       3
PAYTOO   DIM       25
PAYCHK   FORM      1
PAYKEY   DIM        5
amendnum  dim       2
.
NADJUST  FILE
adjnumber dim       2
HOLD      DIM       500
HOLD2     DIM       304
OLDBRKNEWCOMP external "COMP001A;OldBrktoNewComp"
TCell     Automation
TCell2    Automation
Table1    Automation
TRange    Automation
Range     Automation
CRRet     Init    0x0D
.begin patch 4.22
CRNewLn   Init      0x0A
.end patch 4.22           all occurances of CRRet replaced in body of code
CRTab     Init      0x09
TWidth    form      1.2
wdTexture15Percent integer 4,"0x96"
BordStart form      9
BordEnd   form      9
PgStart   form      9
.Variant objects used to talk to outside applications
.See PL/B help in order to understand use of Variant objects.
.
.Booleans
.PL/B does not have a Boolean datatype, so we have to create our own.
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant

AutPtr    Automation          ^
DimPtr    Dim                 ^
MSWordFlag form               1                   .0=user printer, 1 = use word object via OLE
NINLogo   PICT
FirstPage form      5
Laser               pfile
.
Font08              font
Font09I             font
Font09B             font
Font09BI  font
Font010             font
Font014B  font
Font014BI font
Font07dot5I         font
sevenfive form      "7.5"
.Begin Patch 4.0
Font07                 font
FontO7              font
FontO18B  font
.end patch 4.0
PERCENT  FORM      4.2
CALCmPER FORM      7.4
TEMP     FORM      8.4
VerticalPos         Form           5
.
netcomnt1 dim      70
netcomnt2 dim      70
netcomnt3 dim      70
.
.begin patch 4.21
.PrintMSWordInvoice Routine AutPtr,DimPtr
.begin patch 4.22
.PrintMSWordInvoice Routine AutPtr,DimPtr,FrmPTr1  
.         Move      FrmPtr1,Company
.end patch 4.21
.end patch 4.22
Begin
          call      GetWinVer
          create    fontO7,"Times New Roman",size=7
          create    fontO18B,"Times New Roman",size=18,Bold
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.Create fonts to be used
          create    Font08,"Times New Roman",size=8
          create    Font09I,"Times New Roman",size=9,Italic
          create    Font09B,"Times New Roman",size=9,Bold
          create    Font09BI,"Times New Roman",size=9,Bold,Italic
          create    Font010,"Times New Roman",size=10
          create    Font014B,"Times New Roman",size=14,Bold
          create    Font014BI,"Times New Roman",size=14,Bold,Italic
          create    Font07dot5I,"Times New Roman",size=sevenfive,Italic
.Begin Patch 4.0
          create    font07,"Times New Roman",size=7
.         create    fontO7,"Times New Roman",size=7
.         create    fontO18B,"Times New Roman",size=18,Bold
.End Patch 4.0

          move      C0,MSWordFlag
          call      Trim using INPNAME
          if (INPNAME = "")   .Called from NCSH002A - USE OLE LOGIC
                    pack      INPNAME,DimPtr
                    move      C1,MSWordFlag
                  create  OTRUE,VarType=VT_BOOL,VarValue=1
                  create  OFALSE,VarType=VT_BOOL,VarValue=0
          endif
.MAIN
         MOVE      "Names in the News" TO COMPNME
         MOVE      "PRINT DAILY ADJUSTMENTS" TO STITLE
         CALL      PAINT
         MOVE      C1 TO NORDPATH
         move      c1 to nbrkpath       .set access to isi.
.
         move      c1 to NINVOUTFLAG             .new compute.inc  this old code falling thru and printing detail. No No.
         move      c1 to NINVFRMFLAG              .mailer/remit copies some LO & internal charges remain hidden
         CLOCK     TIME TO TIME
         DISPLAY   *P20:04,"START TIME : ",TIME
         CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO MM,DD,YY
         PACK      DATE FROM MM,SLASH,DD,SLASH,YY
         XIF
         IFZ       PC
         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         XIF
         MATCH     "NINPADJ2" TO INPNAME        *DAILY PRINT?
         IF        EQUAL                        *YES
                 MOVE      C1 TO HOTFLAG        .NOT HOT
.Daily Print no longer done!!!
                    shutdown "cls"
         ELSE
                 MOVE      C2 TO HOTFLAG        .HOT
                 unpack    INPNAME TO NjstfLD,typist
                 MOVE      "HOT PRINT ADJUSTMENTS" TO STITLE
                 rep       zfill in njstfld
.dlh fix for missing jd code?
                 unpack    njstfld into str6,adjnumber
.end dlh fix for missing jd code?

         ENDIF
         move      c1 to formflag
         MOVE      DATE TO TODAY
         CALL      PAINT
         MOVE      "Exit" TO PF5
         TRAP      END IF F5
         CALL      FUNCDISP
         DISPLAY   *P01:06,"Input File  : ":
                   *P01:07,"Print File  : ":
                   *P01:08,"Input Count : ":
                   *P01:09,"Today's Date : "
.
INPGET
         BRANCH    hotflag OF OPENINP,READLIVE
openinp  TRAP      FILENG IF IO
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         DISPLAY   *P15:06,INPNAME
         CLOSE     TESTFILE
         OPEN      NADJUST,INPNAME
          GOTO      PRTGET
FILENG   NORETURN
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *P15:06,INPNAME
         GOTO      INPGET
.
READLIVE
         DISPLAY   *P15:06,INPNAME
         REP       ZFILL IN NjstFLD
         call      njstkey
.
PRTGET
         BRANCH    hotflag OF DLYPRT,HOTPRT
DLYPRT   MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MOVE      C1 TO PRTFLAG
         MATCH     LOCAL TO PRTNAME
         GOTO      TESTQUES IF EQUAL
         MOVE      C2 TO PRTFLAG
.          if (osflag = c1 | osflag = c5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt
.                    PRTOPEN Laser,"\\NINS2\laser2","FAXFILE.PRN"
.          elseif (osflag = C3 | osflag = C4)
           if         (osflag >= c6)
                    PRTOPEN Laser,"\\NINS2\Laser2","FAXFILE.PRN"
          else   .(osflag = c0)         .Don't know prompt for printer
                    PRTOPEN Laser,"-","FAXFILE.PRN"
          endif
         GOTO      TESTQUES
.
HOTPRT
          if (MSWordFlag = C0)
.                    if (osflag = c1 | osflag = c5 | osflag = c6)         .nt
.                              PRTOPEN Laser,"\\NINS2\laser2","FAXFILE.PRN"
.                    elseif (osflag = C3 | osflag = C4)
           if         (osflag >= c6)
                              PRTOPEN Laser,"\\NINS2\Laser2","FAXFILE.PRN"
                    else   .(osflag = c0)         .Don't know prompt for printer
                              PRTOPEN Laser,"-","FAXFILE.PRN"
                    endif
.else we are using word object
          endif
          GOTO       PROCESS
.
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
.
TESTQUES
.
        goto       testprt
READADJ
         FILEPI    1;nadjust
         READ      nadjust,SEQ;jstvars,typist
         goto      done if over          .02mar93 dlh
         move      jstsubno to amendnum
.
PROCESS  ADD       C1 TO COUNT
         DISPLAY   *P16:08,COUNT,b2,jstreasn
         MOVE      JSTCRCT TO REASON
.begin patch xxx
.         BRANCH    REASON OF SALES,CORRECT,SHORT
         BRANCH    REASON OF SALES,CORRECT,SHORT,CLoseAr
.begin patch xxx
         CLEAR     COMNT
         GOTO      REST
SALES    MOVE      "*SALES BOOK CORRECTION*" TO COMNT
         GOTO      REST
CORRECT  MOVE      "***CORRECTED INVOICE***" TO COMNT
         GOTO      REST
SHORT    MOVE      "***  SHORT PAYMENT  ***" TO COMNT
.begin patch xxx
         GOTO      REST
CloseAR  MOVE      "****   Close A/R   ****" TO COMNT
.begin patch xxx

REST     MOVE      JSTLR TO NORDFLD
         REP       ZFILL IN NORDFLD
         CALL      NORDKEY
         PACK      MKEY FROM OMLRNUM,z3
                              pack                compfld3 from mkey
                              call                COMPKEY3
                              if (CNCTINACTIVE = "T")
                                        clear     cnctfname
                              endif
         PACK      NBILFLD FROM MKEY,JSTBILTO
         UNPACK    JSTDATE INTO cc,YY,MM,DD
         PACK      ADJDATE FROM MM,SLASH,DD,SLASH,YY
         UNPACK    JSTINVDT INTO cc,YY,MM,DD
         PACK      INVDATE FROM MM,SLASH,DD,SLASH,YY
           PACK      STR2 FROM OSALES10,OSALES
           REP       ZFILL IN STR2
           MOVE      NO TO LSTMSW
.begin patch 4.00
.         if        (str2 = "06" or str2 = "02" or Str2 = "19")
          if        (str2 = "06" or str2 = "02" or Str2 = "19" or Str2 = "27" or STr2 = "28")
          Move      Yes,LstmSW
.           MATCH     "06" TO STR2
.           IF        EQUAL
.                    MOVE      YES TO LSTMSW            *LIST MANAGEMENT.
.           ELSE
.              MATCH     "19" TO STR2
.              IF        EQUAL
.                    MOVE      YES TO LSTMSW
.              endif
           endif
         MOVE      OLNUM TO NDATFLD
         MOVE      C1 TO NDATPATH
         CALL      NDATKEY

         MOVE      JSTLR TO NINVFLD
         REP       ZFILL IN NINVFLD
         MOVE      C1 TO NINVPATH
         CALL      NINVKEY
         IF        OVER
         DISPLAY   *P10:15,"NO INV FOUND!!!",*W2
         ENDIF

         MOVE      YES TO SUBPPSW
         MOVE      NordFLD to nmrgfld
         REP       ZFILL IN NMRGFLD
         MOVE      NordFLD to nshpfld
         REP       ZFILL IN NshpFLD
         move      c0 to nmrgrqty
         move      c0 to nmrgiqty
         move      c0 to nmrgnet
         move      no to mrgsw
         CALL      NMRGKEY
         if        not over
         move      yes to mrgsw
         endif
         move      no to shipsw
         CALL      NshpKEY
         if        not over
         move      yes to shipsw
         endif

         call      wipecvars
                    call      Ninvacdrecclear
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
;
          call      compute
.
         move      c0 to formar1
         move      formar to formar1
.
                              pack                compfld3 from mkey
            call              COMPKEY3
                              if (CNCTINACTIVE = "T")
                                        clear     cnctfname
                              endif
         move      "01" to mm
         move      "01" to dd
         move      "95" to yy
         call      cvtjul
         move      juldays to str5
         move      invdtem to mm
         move      invdted to dd
         move      invdtey to yy
         call      cvtjul
         move      str5 to n5
         sub       juldays from n5
         goto      readbill if not less
         CLEAR     BRCOMP
         CLEAR     BRaddr
         CLEAR     BRcity
         CLEAR     BRstate
         CLEAR     BRzip
         CLEAR     NBRKFLD
         move      b3 to careof
         PACK      NBRKFLD FROM IBRKNUM,IBRKCNT
         CMATCH    B1 TO NBRKFLD
         goto      GOON IF EOS
                              call      OLDBRKNEWCOMP using NBRKFLD,HOLD
                              goto      goon if (hold = "")
         pack      mkey from mlrn,z3
                              pack                compfld3 from mkey
          call                COMPKEY3
                              if (CNCTINACTIVE = "T")
                                        clear     cnctfname
                              endif
         cmatch    yes to mbildrct            .bill direct?  11/2 dlh
         if         equal            .yes
         goto      goon
         endif
                              MOVE COMPCOMP to CNCTFNAME
                              reset hold to 7
                              move hold to COMPCOMP
                              reset hold to 62
                              move hold to COMPADDR
                              reset hold to 132
                              move hold to COMPCITY
                              reset hold to 162
                              move hold to COMPSTATE
                              reset hold to 164
                              move hold to COMPZIP
                              reset hold
         move     "C/O" to careof
GOON     MOVE      NINVFLD TO NADJFLD
         CALL      NADJKEY
         MOVE      MASK92 TO ARMASK
         MOVE      C0 TO CMPT92
          add      asrecadj to cmpt92
         add       CMPT92 to formar
         EDIT      FORMar TO ARMASK
         MOVE      MASK92 TO AP1MASK
         MOVE      C0 TO CMPT92
          move      aspayad1 to cmpt92
         move       ap to formap1
         ADD       CMPT92 FROM FORMAP1
         MOVE      FORMAP1 TO CMPT92
         EDIT      CMPT92 TO AP1MASK
         MOVE      MASK92 TO AP2MASK
         MOVE      C0 TO CMPT92
         move       aspayad2 to cmpt92
          move      ap2 to formap2
         ADD       CMPT92 FROM FORMAP2
         MOVE      FORMAP2 TO CMPT92
         EDIT      CMPT92 TO AP2MASK
         MOVE      MASK42 TO PSMASK
         edit      jstpost to psmask
         MOVE      MASK52 TO STMASK
          edit     jststax to stmask
         edit      jstctax to ctymask
          edit     jstlrinc to lrmask
         PACK      MAILDATE FROM OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
         CLEAR     COMNT1
         CMATCH    "D" TO JSTCD
         GOTO      DB IF EQUAL
         GOTO      CR IF NOT EQUAL
DB       BRANCH    REASON OF PRINT
         MOVE      "ADDITIONAL AMOUNT DUE" TO COMNT1
         GOTO      PRINT
CR       BRANCH    REASON OF PRINT
         MOVE      "CREDIT DUE" TO COMNT1
.
PRINT    CALL      OWNPREP
         COMPARE   C0 TO FORMAP2
         IF        EQUAL
            compare   c0 to formap1
            if        equal
            move      c1 to apflag
            else
            move      c2 to apflag
            endif
         GOTO      det
         ELSE
         MOVE      AP2MASK,AP1MASK
         move      c2 to apflag
         ENDIF
det
         MOVE      QTYbild TO n9
         MOVE      MASK9 TO M$QTY
         EDIT      n9 TO M$QTY

           MOVE      PPM TO CMPT92
           MOVE      MASK32 TO M$PPM
           MOVE      CMPT92 TO FORM32
           EDIT      FORM32 TO M$PPM

.
         MOVE      MASK92 TO M$GROSS
         EDIT      GROSS TO M$GROSS
.
         MOVE      MASK92 TO M$AR
         EDIT      FORMAR1 TO M$AR
.
         call      charges
.
FORMS    branch    formflag of prtmlr,prtown

prtmlr
          if (MSWordFlag = 0)
                    call      prtinvfrm
.begin patch 4.1
                    call      PrtmlrboxGui
.                   call      prtinvfrm2
.end patch 4.1
          endif
         compare   c2 to hotflag
         if        equal
         move      adjnumber to jstsubno
         else
         move      amendnum to jstsubno
         endif
          packkey   NSEL2FLD,"1",LRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
          endif
          if (MSWordFlag = 0)
.............................................................................
                    call      Trim using COMPCITY
                    Prtpage   Laser;*font=Font09B,*p=1500:2212,adjdate:
                              *p=5000:2212,invnum,dash,jstsubno:
                              *p=6750:2212,OMLRPON:
                              *p=1500:2400,Mlrn,"/",Cobn,"-",Billtn:
                              *p=1500:2525,CNCTFNAME:
                              *p=1150:2650,CareOf:
                              *p=1500:2650,COMPCOMP:
                              *p=6750:2712,LRN:
                              *p=1500:2775,COMPADDR:
                              *p=1500:2900,*ll,COMPCITY,", ",COMPSTATE," ",COMPZIP
                    Prtpage   Laser;*p=6750:3212,OMDTEM,"/",OMDTED,"/",OMDTEC,OMDTEY:
                              *p=1500:3602,*font=Font010,comnt,*font=Font09B:
                              *p=1500:3800,OODES:
                              *p=1500:4050,OMLRKY:
                              *p=1500:4300,O1DES:
                              *p=1500:4440,NSEL2NAME:
                              *p=4250:4440,jstreasn:
                              *p=2500:4900,*ALIGNMENT=*right,*overlayoff,M$Qty:
                              *p=5000:4900,"@":
                              *p=5475:4900,M$PPM:
                              *p=7500:4900,M$GRoss,*overlayon,*ALIGNMENT=*Left
.
        if         (onetfm <> "F" & onetFM <> "M")
        move       c1 to netflag
        endif

                              if (netflag = 2 or netflag = 3)
                   move      c0 to calcmper
                MOVE      NMRGNET TO CMPT94
             MOVE      NMRGIQTY TO N7
          DIVIDE    N7 INTO CMPT94
                   MULT      "100" BY CMPT94
                MOVE      C0 TO PERCENT
             ADD       CMPT94 TO PERCENT
                          move      c0 to temp
             move      irnetper to temp
          compare   c3 to netflag            .flat???
                   if        equal
                       move      irnetper to percent         .yes print stated flat rate
             else                              .no print calced rate
                             compare   temp to percent           .use bigger %
                             if        less
                                 move      temp to percent
                       endif
                   endif
                   cmatch    no to lstmsw
                if        equal               .its a flat discounted brokerage client
                              Prtpage   Laser;*p=4585:4412,*ALIGNMENT=*right,Irnetper,*ALIGNMENT=*Left,"% Net Arrangement, Run charge @",inetrc,slash,"M"
                                        else
                              Prtpage   Laser;*p=4585:3602,"% Net Arrangement, Run charge @",inetrc,slash,"M":
                                        *p=4585:3727,Irnetper,*ALIGNMENT=*Left,"No Deducts, No CV required."
                                        endif
                              endif



          else
.MS Word Version
.
        clear      netcomnt1
          clear      netcomnt2
        clear      netcomnt3
.
        if         (onetfm <> "F" & onetFM <> "M")
        move       c1 to netflag
        endif

                              if (netflag = 2 or netflag = 3)
                   move      c0 to calcmper
                MOVE      NMRGNET TO CMPT94
             MOVE      NMRGIQTY TO N7
          DIVIDE    N7 INTO CMPT94
                   MULT      "100" BY CMPT94
                MOVE      C0 TO PERCENT
             ADD       CMPT94 TO PERCENT
                          move      c0 to temp
             move      irnetper to temp
          compare   c3 to netflag            .flat???
                   if        equal
                       move      irnetper to percent         .yes print stated flat rate
             else                              .no print calced rate
                             compare   temp to percent           .use bigger %
                             if        less
                                 move      temp to percent
                       endif
                   endif
                   cmatch    no to lstmsw
                if        equal               .its a flat discounted brokerage client
                           pack      netcomnt1 from Irnetper,"% Net Name Arrangement, Run charge @",inetrc,slash,"M"
                                        else
                          pack      netcomnt2 from Irnetper,"% Net Arrangement, Run charge @",inetrc,slash,"M"
                          pack      netcomnt3 from "No Deducts, No CV required."
                                        endif
                              endif

.Note:  Setting a property will apply that property for all subsequent text!!
.         The End of the Range is always implied to be the last text that was entered.
                    AutPtr.Sections.Add
                    getprop   AutPtr.Sections(2),*Range=Range
                    getprop   Range,*Start=PgStart

          If        (company = c2)
                    AutPtr.Shapes.AddPicture using *Filename="\\nins1\e\netutils\pacificlists.jpg",*Anchor=Range
          else
                    AutPtr.Shapes.AddPicture using *Filename="\\nins1\e\netutils\Logocolornotag.jpg",*Anchor=Range
          endif

.                    AutPtr.Shapes.AddPicture using *Filename="\\nins1\e\netutils\Logocolornotag.jpg",*Anchor=Range

                    AutPtr.Shapes(2).ScaleHeight using ".65",OFALSE
                    AutPtr.Shapes(2).ScaleWidth using ".65",OFALSE
.Need to set up the RelativeVerticalPosition in order for Top property to behave properly
.I have set the RelativeVerticalPosition to "wdRelativeVerticalPositionPage"=1
                    setprop   AutPtr.Shapes(2),*RelativeVerticalPosition=1,*Top="30"
                    setprop Range.Font,*Name="Times New Roman"                            .Default Font Name
                    setprop Range.Font,*Size=14                                           .Default Font Size
                    setprop AutPtr.Sections(2).PageSetup,*TopMargin=30                    .Default Top Margin
                    setprop AutPtr.Sections(2).PageSetup,*BottomMargin=0                  .Default Bottom Margin
                    setprop AutPtr.Sections(2).PageSetup,*LeftMargin=30                   .Default Left Margin
                    setprop AutPtr.Sections(2).PageSetup,*RightMargin=35                  .Default Right Margin
.testing DH
                    Setprop Range.ParagraphFormat,*LineSpacingRule="0"  ...single space
.testing DH
                    Setprop Range.ParagraphFormat,*LineUnitBefore="0"
.testing DH

.                   Range.InsertAfter using "Names"
.                   setprop Range,*Bold=OTRUE
.                   setprop Range.Font,*Size=20
..
.                   Range.InsertAfter using " in the News"
.                   add       C6,PgStart,howmany
.                   setprop   Range,*Start=howmany
.                   getprop   Range,*End=result
.                   setprop Range,*Bold=OFALSE
.                   setprop Range,*Italic=OTRUE
.                   setprop   Range,*Start=PgStart
.                   setprop Range,*Underline=OTRUE
..
.                   Range.InsertAfter using CRNewLn
.                   Range.InsertAfter using "C A L I F O R N I A   I N C"
.                   setprop   Range,*Start=result
.                   getprop   Range,*End=result
.                   setprop Range.Font,*Size=14
.                   setprop Range,*Underline=OFALSE
.                   setprop Range,*Italic=OFALSE
...............................
                    Range.InsertAfter using CRNewLn
                    Range.InsertAfter using CRNewLn
                    Range.InsertAfter using CRNewLn
                    Range.InsertAfter using CRNewLn
                    getprop   Range,*End=result
                    Range.InsertAfter using CRNewLn
                    getprop   Range,*End=BordStart
                    Range.InsertAfter using "Invoice"
                    getprop   Range,*End=BordEnd
                    setprop   Range,*Start=result
                    setprop Range,*Bold=OTRUE
                    setprop Range,*Italic=OTRUE
.
                    Range.InsertAfter using CRNewLn
                    getprop   Range,*End=result
                    setprop   Range,*Start=result
                    Range.InsertAfter using CRNewLn
.Table Number 1
                    getprop   Range,*End=result
                    setprop   Range,*Start=result
                    getprop   Range,*End=N9
                    Range.InsertAfter using CRNewLn
.Make sure CR is in proper format
                    setprop   Range,*Start=result
                    setprop   Range.Font,*Size=10
                    setprop Range,*Bold=OFALSE
                    setprop Range,*Italic=OFALSE
                    setprop   Range,*Start=result
.
                    AutPtr.Range giving TRange using result,N9
                    AutPtr.Tables.Add giving Table1 using TRange,9,8
                    setprop   Table1.Range.Font,*Size=10
                    setprop Table1.Range,*Bold=OFALSE
                    setprop Table1.Range,*Italic=OFALSE
.
                    Table1.Cell giving TCell using 2,2
                    TCell.Range.InsertAfter using "Date:"
                    setprop   TCell.Range,*Italic=OTRUE
                    Table1.Cell giving TCell using 2,4
                    TCell.Range.InsertAfter using adjdate
                    Table1.Cell giving TCell using 2,5
                    TCell.Range.InsertAfter using "Invoice ##"
                    setprop   TCell.Range,*Italic=OTRUE
                    Table1.Cell giving TCell using 2,6
                    TCell.Range.InsertAfter using invnum
                    TCell.Range.InsertAfter using dash
                    TCell.Range.InsertAfter using jstsubno
                    Table1.Cell giving TCell using 2,7
                    TCell.Range.InsertAfter using "Mailer's P.O. ##"
                    setprop   TCell.Range,*Italic=OTRUE
                    Table1.Cell giving TCell using 2,8
                    TCell.Range.InsertAfter using OMLRPON
.
                    Table1.Cell giving TCell using 3,2
                    TCell.Range.InsertAfter using "Client ##"
                    setprop   TCell.Range,*Italic=OTRUE
                    Table1.Cell giving TCell using 3,4
                    TCell.Range.InsertAfter using MLRN
                    TCell.Range.InsertAfter using "/"
                    TCell.Range.InsertAfter using COBN
                    TCell.Range.InsertAfter using "-"
                    TCell.Range.InsertAfter using BILLTN
.
                    Table1.Cell giving TCell using 4,4
                    TCell.Range.InsertAfter using CNCTFNAME
.
                    Table1.Cell giving TCell using 5,3
                    TCell.Range.InsertAfter using careof
                    Table1.Cell giving TCell using 5,4
                    TCell.Range.InsertAfter using COMPCOMP
                    Table1.Cell giving TCell using 5,7
                    TCell.Range.InsertAfter using "NIN LR ##"
                    setprop   TCell.Range,*Italic=OTRUE
                    Table1.Cell giving TCell using 5,8
                    TCell.Range.InsertAfter using lrn
.
                    Table1.Cell giving TCell using 6,4
                    TCell.Range.InsertAfter using COMPADDR
.
                    call      Trim using COMPCITY
                    Table1.Cell giving TCell using 7,4
                    TCell.Range.InsertAfter using COMPCITY
                    TCell.Range.InsertAfter using ", "
                    TCell.Range.InsertAfter using COMPSTATE
                    TCell.Range.InsertAfter using " "
                    TCell.Range.InsertAfter using COMPZIP
.
                    Table1.Cell giving TCell using 8,7
                    TCell.Range.InsertAfter using "Mail Date:"
                    setprop   TCell.Range,*Italic=OTRUE
                    Table1.Cell giving TCell using 8,8
                    TCell.Range.InsertAfter using OMDTEM
                    TCell.Range.InsertAfter using "/"
                    TCell.Range.InsertAfter using OMDTED
                    TCell.Range.InsertAfter using "/"
                    TCell.Range.InsertAfter using OMDTEC
                    TCell.Range.InsertAfter using OMDTEY
.Format Table 1
                    move      ".47",TWidth
                    setprop   Table1.Columns(1),*Width=TWidth
                    move      ".62",TWidth
                    setprop   Table1.Columns(2),*Width=TWidth
                    move      ".42",TWidth
                    setprop   Table1.Columns(3),*Width=TWidth
                    move      "1.5",TWidth
                    setprop   Table1.Columns(4),*Width=TWidth
                    move      "1",TWidth
                    setprop   Table1.Columns(5),*Width=TWidth
                    move      "1.5",TWidth
                    setprop   Table1.Columns(6),*Width=TWidth
                    move      "1.2",TWidth
                    setprop   Table1.Columns(7),*Width=TWidth
                    move      "1.17",TWidth
                    setprop   Table1.Columns(8),*Width=TWidth
.
                    setprop   Table1.Borders,*Enable=OFALSE           .Make sure no other Borders are applied
                    setprop   Table1.Borders,*OutsideLineStyle=1      .1 = Single Line
.Set up Alignment in first table.
.Note:  Table1 is a temporary object, and ends up linked to third Table.  If more are added, I will need to change following code.
.THIS MUST BE DONE BEFORE CELL MERGING!!!!!!!
.0 = Align Left
.1 = Align Center
.2 = Align Right
                    getprop   Table1.Rows,*Count=result
                    for N9,C1,result
                              Table1.Cell giving TCell using N9,2
                              setprop   TCell.Range.Paragraphs(1),*Alignment=0
                    repeat
                    for N9,C1,result
                              Table1.Cell giving TCell using N9,3
                              setprop   TCell.Range.Paragraphs(1),*Alignment=0
                    repeat
                    for N9,C1,result
                              Table1.Cell giving TCell using N9,4
                              setprop   TCell.Range.Paragraphs(1),*Alignment=0
                    repeat
                    for N9,C1,result
                              Table1.Cell giving TCell using N9,5
                              setprop   TCell.Range.Paragraphs(1),*Alignment=0
                    repeat
                    for N9,C1,result
                              Table1.Cell giving TCell using N9,6
                              setprop   TCell.Range.Paragraphs(1),*Alignment=0
                    repeat
                    for N9,C1,result
                              Table1.Cell giving TCell using N9,7
                              setprop   TCell.Range.Paragraphs(1),*Alignment=0
                    repeat
                    for N9,C1,result
                              Table1.Cell giving TCell using N9,8
                              setprop   TCell.Range.Paragraphs(1),*Alignment=0
                    repeat
.
                    Table1.Cell giving TCell using 4,4
                    Table1.Cell giving TCell2 using 4,8
                    TCell.Merge using TCell2
.
                    Table1.Cell giving TCell using 5,4
                    Table1.Cell giving TCell2 using 5,6
                    TCell.Merge using TCell2
.
                    Table1.Cell giving TCell using 6,4
                    Table1.Cell giving TCell2 using 6,8
                    TCell.Merge using TCell2
.
                    Table1.Cell giving TCell using 7,4
                    Table1.Cell giving TCell2 using 7,8
                    TCell.Merge using TCell2
.Table Number 2
                    getprop   Range,*End=result
                    setprop   Range,*Start=result
                    getprop   Range,*End=N9
                    Range.InsertAfter using CRNewLn
.Make sure CR is in proper format
                    setprop   Range,*Start=result
                    setprop   Range.Font,*Size=10
                    setprop Range,*Bold=OFALSE
                    setprop Range,*Italic=OFALSE
                    setprop   Range,*Start=result
.
                    AutPtr.Range giving TRange using result,N9
                    AutPtr.Tables.Add giving Table1 using TRange,10,3
                    setprop   Table1.Range.Font,*Size=10
                    setprop Table1.Range,*Bold=OFALSE
                    setprop Table1.Range,*Italic=OFALSE
.
                    Table1.Cell giving TCell using 1,3
                clear   taskname
                    pack      taskname,comnt,crtab,crtab,crtab,netcomnt1,netcomnt2
                    TCell.Range.InsertAfter using taskname
                    Table1.Cell giving TCell using 2,3
                    clear   taskname
                    pack      taskname,crtab,crtab,crtab,crtab,crtab,crtab,netcomnt3
                    TCell.Range.InsertAfter using taskname

.
                    Table1.Cell giving TCell using 3,2
                    TCell.Range.InsertAfter using "Mailer's Offer"
                    setprop   TCell.Range,*Italic=OTRUE
                    Table1.Cell giving TCell using 3,3
                    TCell.Range.InsertAfter using OODES
.
                    Table1.Cell giving TCell using 5,2
                    TCell.Range.InsertAfter using "Key:"
                    setprop   TCell.Range,*Italic=OTRUE
                    Table1.Cell giving TCell using 5,3
                    TCell.Range.InsertAfter using OMLRKY
.
                    Table1.Cell giving TCell using 7,2
                    TCell.Range.InsertAfter using "List:"
                    setprop   TCell.Range,*Italic=OTRUE
                    Table1.Cell giving TCell using 7,3
                    TCell.Range.InsertAfter using O1DES
.
                    Table1.Cell giving TCell using 8,3
                    TCell.Range.InsertAfter using NSEL2NAME
.NOTE:  NEXT CODE IS FOR REFERENCE.  JSTREASN USED TO BE PRINTED, BUT IT NO LONGER APPEARS ON THE FORM.
.HPT725 ENDS UP BEING OFF THE RIGHT MARGIN IN OLD PRINT OUT!!
.                             *L,*L,hpt025,hpdtch85,hpitalic,"List:",hpdtch10,hpuprght:
.                             hpt125,O1DES:
.                             *l,hpt125,o2DES,hpt725,jstreasn:
.Format Table 2
                    move      ".47",TWidth
                    setprop   Table1.Columns(1),*Width=TWidth
                    move      "1.03",TWidth
                    setprop   Table1.Columns(2),*Width=TWidth
                    move      "6.37",TWidth
                    setprop   Table1.Columns(3),*Width=TWidth
.
                    setprop   Table1.Borders,*Enable=OFALSE           .Make sure no other Borders are applied
                    setprop   Table1.Borders,*OutsideLineStyle=1      .1 = Single Line
.Set up Alignment in second table.
.Note:  Table1 is a temporary object, and ends up linked to third Table.  If more are added, I will need to change following code.
.THIS MUST BE DONE BEFORE CELL MERGING!!!!!!!
.0 = Align Left
.1 = Align Center
.2 = Align Right
                    getprop   Table1.Rows,*Count=result
                    for N9,C1,result
                              Table1.Cell giving TCell using N9,2
                              setprop   TCell.Range.Paragraphs(1),*Alignment=0
                    repeat
                    for N9,C1,result
                              Table1.Cell giving TCell using N9,3
                              setprop   TCell.Range.Paragraphs(1),*Alignment=0
                    repeat
.Table Number 3
                    getprop   Range,*End=result
                    setprop   Range,*Start=result
                    getprop   Range,*End=N9
                    Range.InsertAfter using CRNewLn
                    AutPtr.Range giving TRange using result,N9
                    AutPtr.Tables.Add giving Table1 using TRange,32,9
                    setprop   Table1.Range.Font,*Size=10
                    setprop Table1.Range,*Bold=OFALSE
                    setprop Table1.Range,*Italic=OFALSE
.
                    Table1.Cell giving TCell using 1,2
                    TCell.Range.InsertAfter using "Original Billing"
                    setprop   TCell.Range,*Bold=OTRUE
                    setprop   TCell.Range,*Underline=OTRUE
....
                    move      C0,N9
                    move      irexqty,N9
                    compare   C0,N9
                    if not equal
                              move      MASK9,M$QTYX
                              edit      N9,M$QTYx
                              move      MASK32,M$PPMX
                              move      iexppm,form32
                              edit      FORM32,M$PPMX
                    endif
....
                    Table1.Cell giving TCell using 2,2
                    TCell.Range.InsertAfter using "Quantity Addressed"
                    setprop   TCell.Range,*Italic=OTRUE
                    Table1.Cell giving TCell using 2,3
                    TCell.Range.InsertAfter using M$QTY
                    Table1.Cell giving TCell using 2,4
                    TCell.Range.InsertAfter using "$ Per M"
                    setprop   TCell.Range,*Italic=OTRUE
                    Table1.Cell giving TCell using 2,5
                    TCell.Range.InsertAfter using "@"
                    Table1.Cell giving TCell using 2,6
                    TCell.Range.InsertAfter using M$PPM
                    Table1.Cell giving TCell using 2,7
                    TCell.Range.InsertAfter using "Amount Due"
                    setprop   TCell.Range,*Italic=OTRUE
                    Table1.Cell giving TCell using 2,8
                    TCell.Range.InsertAfter using M$GRoss
          endif
        move       c0 to n9
        move       irexqty to n9
        compare    c0 to n9
        if         not equal
         MOVE      MASK9 TO M$QTYX
         EDIT      n9 TO M$QTYx
         MOVE      MASK32 TO M$PPMX
          move      iexppm to form32
         EDIT      FORM32 TO M$PPMX
                    if (MSWordFlag = 0)
                              Prtpage   Laser;*p=2500:5025,*ALIGNMENT=*right,*overlayoff,M$QtyX:
                                        *p=5000:5025,"@":
                                        *p=5475:5025,M$PPMX,*overlayon,*ALIGNMENT=*Left
                    endif
        endif
...................................
          packkey   NSEL2FLD,"1",LRN
          move      "NSEL2KEY-2",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
          endif
..................................................
          if (MSWordFlag = 0)
                    move      "5025",verticalpos
          else
                    move      C3,howmany
          endif
          add       sixlpi,row
          FOR AcdRecCount,"1","15"
                    MOve      NInvAcdRec(AcdRecCount).NinvAcdNumRec,NinvAcdNum
                    MOve      NInvAcdRec(AcdRecCount).NinvAcdCodeRec,NinvAcdCode
                    MOve      NInvAcdRec(AcdRecCount).NinvAcdRateRec,NinvAcdRate
                    MOve      NInvAcdRec(AcdRecCount).NInvAcdPercRec,NInvAcdPerc
                    MOve      NInvAcdRec(AcdRecCount).NINVAcdANINCDRec,NINVAcdANINCD
                    MOve      NInvAcdRec(AcdRecCount).NINvAcdqtyRec,str9
                    MOve      NInvAcdRec(AcdRecCount).NINvAcdTotalRec,str15
                    MOve      NInvAcdRec(AcdRecCount).NinvAcdAextcdRec,NinvAcdAextcd
                    MOve      NInvAcdRec(AcdRecCount).NinvAcdRateTRec,NinvAcdRateT
                    MOve      NInvAcdRec(AcdRecCount).NINvAcdDescRec,nacdtext
                    move      NINVAcdANINCD to str3
                    call      trim using str3
                    move      c0 to anincd
                    move      str3 to Anincd
                    If (NinvacdNum = "")
                              Break
                    endif
.
.                             if (ninvoutflag = 1 & Ninvfrmflag = c3 & NInvAcdCode = "041")
                    if (NInvAcdCode = "041")
                              Goto DontPrintDet
                    endif                           *
.
.                             if (ninvoutflag = 1 & Ninvfrmflag = c3 & NInvAcdCode = "085")
                    if (NInvAcdCode = "085")
                              Goto DontPrintDet
                    endif                           *
.                             if (ninvoutflag = 1 & NinvFrmflag = c1 & (ANINCD = c1 or ANINCD = c3 or ANINCD = c4 ))
                    if (ANINCD = c1 or ANINCD = c3 or ANINCD = c4 )
                              Goto DontPrintDet               ;mailer copies do not print codes 1,3, or 4
                    endif                           *
.                             if (ninvoutflag = 1 & Elstcde = "C" & NinvFrmflag = c3 & NInvAcdCode = "003")
                    if (Elstcde = "C" & NinvFrmflag = c3 & NInvAcdCode = "003")
                              Goto DontPrintDet
                    endif                           *
                    clear     str10
                    If (NinvAcdRate > 0)
                              move      mask72 to str12
                              edit      NinvAcdRate to str12
                              Move      "@" to Str1
                    else
                              Clear     str1
                              clear     str12
                    endif
                    move      "ZZZ.9999" to str8
                    edit      NInvAcdPerc to str8
                    cLEAR     m$ADDQTY
                    If (str9 <> "")
                              MOVE      MASK9 TO M$AddQTY
                              MOVE      str9 TO N9
                              Edit      N9 to M$AddQTY
                    ENDIF
                    add       "150" to VerticalPos
                    if (MSWordFlag = 0)
                              prtpage   Laser;*p800:VerticalPos,*font=Font09B,nacdtext,*p5000:VerticalPos,str1:
                                        *p=4500:VerticalPos,*ALIGNMENT=*right,M$AddQty,*ALIGNMENT=*Left:
.                                       *p=5475:VerticalPos,*ALIGNMENT=*right,NInvAcdRate,*ALIGNMENT=*Left:
                                        *p=5475:VerticalPos,*ALIGNMENT=*right,str12,*ALIGNMENT=*Left:
                                        *p=7500:VerticalPos,*ALIGNMENT=*right,str15,*ALIGNMENT=*Left
                    else
                              add       C1,howmany
.
                              call      Trim using nacdtext
                              Table1.Cell giving TCell using howmany,2
                              TCell.Range.InsertAfter using nacdtext
                              setprop   TCell.Range,*Bold=OTRUE
.
                              call      Trim using M$AddQty
                              Table1.Cell giving TCell using howmany,4
                              TCell.Range.InsertAfter using M$AddQty
                              setprop   TCell.Range,*Bold=OTRUE
.
                              Table1.Cell giving TCell using howmany,5
                              TCell.Range.InsertAfter using str1
                              setprop   TCell.Range,*Bold=OTRUE
.
                              call      Trim using str12
                              Table1.Cell giving TCell using howmany,6
                              TCell.Range.InsertAfter using str12
                              setprop   TCell.Range,*Bold=OTRUE
.
                              call      Trim using str15
                              Table1.Cell giving TCell using howmany,8
                              TCell.Range.InsertAfter using str15
                              setprop   TCell.Range,*Bold=OTRUE
                    endif
DontPrintDet
          repeat
          if (MSWordFlag = 0)
                    move      "7500",row
                    sub       sixlpi,row
                    Prtpage   Laser;*p=5000:row,"Original Total Due:":
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,M$AR,*ALIGNMENT=*left,*overlayon
                    move      "7500",row
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc1:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$1,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc2:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$2,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc3:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$3,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc4:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$4,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc5:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$5,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc6:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$6,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc7:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$7,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc8:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$8,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc9:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$9,*ALIGNMENT=*left,*overlayon

                    prtpage   Laser;*p=7500:9916,*ALIGNMENT=*right,*overlayoff,ARmask,*ALIGNMENT=*left,*overlayon:
                              *p=7600:9916,Typist
                    goto wipevars
          else
.MS Word Version
....................................................
                    if (howmany > 13)
                              add       C2,howmany
                    else
                              move      "15",howmany
                    endif
.Totals
                    Table1.Cell giving TCell using howmany,7
                    TCell.Range.InsertAfter using "Original Total Due"
                    setprop   TCell.Range.Font,*Size=11
                    Table1.Cell giving TCell using howmany,8
                    TCell.Range.InsertAfter using M$AR
.Next Section of Table 3
                    add       C1,howmany
                    Table1.Cell giving TCell using howmany,2
                    TCell.Range.InsertAfter using "Adjustments to Billing"
                    setprop   TCell.Range,*Bold=OTRUE
                    setprop   TCell.Range,*Underline=OTRUE
.
                    add       C1,howmany
                    Table1.Cell giving TCell using howmany,2
                    TCell.Range.InsertAfter using "     "
                    TCell.Range.InsertAfter using adjdesc1
                    Table1.Cell giving TCell using howmany,8
                    TCell.Range.InsertAfter using adj$1
.
                    add       C1,howmany
                    Table1.Cell giving TCell using howmany,2
                    TCell.Range.InsertAfter using "     "
                    TCell.Range.InsertAfter using adjdesc2
                    Table1.Cell giving TCell using howmany,8
                    TCell.Range.InsertAfter using adj$2
.
                    add       C1,howmany
                    Table1.Cell giving TCell using howmany,2
                    TCell.Range.InsertAfter using "     "
                    TCell.Range.InsertAfter using adjdesc3
                    Table1.Cell giving TCell using howmany,8
                    TCell.Range.InsertAfter using adj$3
.
                    add       C1,howmany
                    Table1.Cell giving TCell using howmany,2
                    TCell.Range.InsertAfter using "     "
                    TCell.Range.InsertAfter using adjdesc4
                    Table1.Cell giving TCell using howmany,8
                    TCell.Range.InsertAfter using adj$4
.
                    add       C1,howmany
                    Table1.Cell giving TCell using howmany,2
                    TCell.Range.InsertAfter using "     "
                    TCell.Range.InsertAfter using adjdesc5
                    Table1.Cell giving TCell using howmany,8
                    TCell.Range.InsertAfter using adj$5
.
                    add       C1,howmany
                    Table1.Cell giving TCell using howmany,2
                    TCell.Range.InsertAfter using "     "
                    TCell.Range.InsertAfter using adjdesc6
                    Table1.Cell giving TCell using howmany,8
                    TCell.Range.InsertAfter using adj$6
.
                    add       C1,howmany
                    Table1.Cell giving TCell using howmany,2
                    TCell.Range.InsertAfter using "     "
                    TCell.Range.InsertAfter using adjdesc7
                    Table1.Cell giving TCell using howmany,8
                    TCell.Range.InsertAfter using adj$7
.
                    add       C1,howmany
                    Table1.Cell giving TCell using howmany,2
                    TCell.Range.InsertAfter using "     "
                    TCell.Range.InsertAfter using adjdesc8
                    Table1.Cell giving TCell using howmany,8
                    TCell.Range.InsertAfter using adj$8
.
                    add       C1,howmany
                    Table1.Cell giving TCell using howmany,2
                    TCell.Range.InsertAfter using "     "
                    TCell.Range.InsertAfter using adjdesc9
                    Table1.Cell giving TCell using howmany,8
                    TCell.Range.InsertAfter using adj$9
.Grand Totals
                    add       C5,howmany
                    Table1.Cell giving TCell using howmany,7
                    TCell.Range.InsertAfter using "Total Due"
                    Table1.Cell giving TCell using howmany,8
                    TCell.Range.InsertAfter using ARmask
                    setprop   TCell.Range.Shading,*Texture=wdTexture15Percent
.
                    add       C2,howmany
                    Table1.Cell giving TCell using howmany,2
                    TCell.Range.InsertAfter using "Payment due upon receipt.  Please return copy with payment."
                    setprop   TCell.Range,*Bold=OTRUE
                    setprop   TCell.Range,*Italic=OTRUE
                    Table1.Cell giving TCell using howmany,6
                    TCell.Range.InsertAfter using "Member Direct Marketing Association"
                    setprop   TCell.Range,*Italic=OTRUE
                    Table1.Cell giving TCell using howmany,9
                    TCell.Range.InsertAfter using TYPIST
.END PATCH 3.81 REPLACED LOGIC
.Format Table 3
                    move      ".47",TWidth
                    setprop   Table1.Columns(1),*Width=TWidth
                    move      "2.09",TWidth
                    setprop   Table1.Columns(2),*Width=TWidth
                    move      ".84",TWidth
                    setprop   Table1.Columns(3),*Width=TWidth
                    move      ".72",TWidth
                    setprop   Table1.Columns(4),*Width=TWidth
                    move      ".25",TWidth
                    setprop   Table1.Columns(5),*Width=TWidth
                    move      ".67",TWidth
                    setprop   Table1.Columns(6),*Width=TWidth
                    move      "1.45",TWidth
                    setprop   Table1.Columns(7),*Width=TWidth
                    move      ".9",TWidth
                    setprop   Table1.Columns(8),*Width=TWidth
                    move      ".50",TWidth
                    setprop   Table1.Columns(9),*Width=TWidth
.
                    setprop   Table1.Borders,*Enable=OFALSE           .Make sure no other Borders are applied
                    setprop   Table1.Borders,*OutsideLineStyle=1      .1 = Single Line
.Set up Alignment in last table.
.Note:  Table1 is a temporary object, and ends up linked to third Table.  If more are added, I will need to change following code.
.THIS MUST BE DONE BEFORE CELL MERGING!!!!!!!
.0 = Align Left
.1 = Align Center
.2 = Align Right
                    getprop   Table1.Rows,*Count=result
                    sub       C1,result
                    for N9,C1,result
                              Table1.Cell giving TCell using N9,2
                              setprop   TCell.Range.Paragraphs(1),*Alignment=0
                    repeat
                    for N9,C1,result
                              Table1.Cell giving TCell using N9,3
                              setprop   TCell.Range.Paragraphs(1),*Alignment=0
                    repeat
                    Table1.Cell giving TCell using 2,3
                    setprop   TCell.Range.Paragraphs(1),*Alignment=2
                    for N9,C1,result
                              Table1.Cell giving TCell using N9,4
                              setprop   TCell.Range.Paragraphs(1),*Alignment=1
                    repeat
                    for N9,C1,result
                              Table1.Cell giving TCell using N9,6
                              setprop   TCell.Range.Paragraphs(1),*Alignment=2
                    repeat
                    for N9,C1,result
                              Table1.Cell giving TCell using N9,7
                              setprop   TCell.Range.Paragraphs(1),*Alignment=1
                    repeat
                    for N9,C1,result
                              Table1.Cell giving TCell using N9,8
                              setprop   TCell.Range.Paragraphs(1),*Alignment=2
                    repeat
.
                    for N8,"4","15"
                              Table1.Cell giving TCell using N8,2
                              Table1.Cell giving TCell2 using N8,3
                              TCell.Merge using TCell2
                    repeat
                    for N8,"17","29"
                              Table1.Cell giving TCell using N8,2
                              Table1.Cell giving TCell2 using N8,3
                              TCell.Merge using TCell2
                    repeat
                    Table1.Cell giving TCell using 32,6
                    Table1.Cell giving TCell2 using 32,8
                    TCell.Merge using TCell2
                    Table1.Cell giving TCell using 32,2
                    Table1.Cell giving TCell2 using 32,4
                    TCell.Merge using TCell2
.
.Reset the Range
                    setprop   Range,*Start=PgStart
.Format Rows which need to be centered
.0 = Align Left
.1 = Align Center
.2 = Align Right
.**I did not set up any Paragraphs, so each line is interpreted as its' own Paragraph.
.Each Cell in each Table is also, inherently, its own Paragraph!!
.                   setprop   Range.Paragraphs(1),*Alignment=1
.                   setprop   Range.Paragraphs(2),*Alignment=1
.                   setprop   Range.Paragraphs(3),*Alignment=1
.                   setprop   Range.Paragraphs(4),*Alignment=1
.                   getprop   Range.Paragraphs,*Count=result
.                   setprop   Range.Paragraphs(result),*Alignment=1
                    setprop   Range.Paragraphs(6),*Alignment=1
.Place Border around "Invoice" language
                    sub       C1,BordEnd
                    sub       C1,BordStart
.If I do not make sure the Range is specifically only around the text, then the Border
.will appear for the whole line.  Border collection will assume I am speaking of a Paragraph.
                    setprop   Range,*Start=BordStart
                    setprop   Range,*End=BordEnd
                    setprop   Range.Borders,*OutsideLineStyle=1       .1 = Single Line
          endif
        move       c0 to n9
        move       irexqty to n9
        compare    c0 to n9
        if         not equal
         MOVE      MASK9 TO M$QTYX
         EDIT      n9 TO M$QTYx
.
         move      c0 to CMPT92
         EDIT      iexppm TO M$PPMX
          if (MSWordFlag = 0)
                    Prtpage   Laser;*p=2500:5025,*ALIGNMENT=*right,*overlayoff,M$QtyX:
                              *p=5000:5025,"@":
                              *p=5475:5025,M$PPMX,*overlayon,*ALIGNMENT=*Left
          else
.MS Word Version
          endif
        endif
.................
          if (MSWordFlag = 0)
                    move      "5025",row
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,ADDESC1:
                              *p=5000:row,*ALIGNMENT=*right,*overlayoff,AT1:
                              *p=5475:row,ADD$RT1:
                              *p=7500:row,ADD$1,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,ADDESC2:
                              *p=5000:row,*ALIGNMENT=*right,*overlayoff,AT2:
                              *p=5475:row,ADD$RT2:
                              *p=7500:row,ADD$2,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,ADDESC3:
                              *p=5000:row,*ALIGNMENT=*right,*overlayoff,AT3:
                              *p=5475:row,ADD$RT3:
                              *p=7500:row,ADD$3,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,ADDESC4:
                              *p=5000:row,*ALIGNMENT=*right,*overlayoff,AT4:
                              *p=5475:row,ADD$RT4:
                              *p=7500:row,ADD$4,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,ADDESC5:
                              *p=5000:row,*ALIGNMENT=*right,*overlayoff,AT5:
                              *p=5475:row,ADD$RT5:
                              *p=7500:row,ADD$5,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,ADDESC6:
                              *p=5000:row,*ALIGNMENT=*right,*overlayoff,AT6:
                              *p=5475:row,ADD$RT6:
                              *p=7500:row,ADD$6,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,ADDESC7:
                              *p=5000:row,*ALIGNMENT=*right,*overlayoff,AT7:
                              *p=5475:row,ADD$RT7:
                              *p=7500:row,ADD$7,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,ADDESC8:
                              *p=5000:row,*ALIGNMENT=*right,*overlayoff,AT8:
                              *p=5475:row,ADD$RT8:
                              *p=7500:row,ADD$8,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,ADDESC9:
                              *p=5000:row,*ALIGNMENT=*right,*overlayoff,AT9:
                              *p=5475:row,ADD$RT9:
                              *p=7500:row,ADD$9,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,ADDESC10:
                              *p=5000:row,*ALIGNMENT=*right,*overlayoff,AT10:
                              *p=5475:row,ADD$RT10:
                              *p=7500:row,ADD$10,*ALIGNMENT=*left,*overlayon
                    move      "7500",row
                    sub       sixlpi,row
                    Prtpage   Laser;*p=5000:row,"Original Total Due:":
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,M$AR,*ALIGNMENT=*left,*overlayon
                    move      "7500",row
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc1:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$1,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc2:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$2,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc3:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$3,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc4:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$4,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc5:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$5,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc6:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$6,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc7:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$7,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc8:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$8,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc9:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$9,*ALIGNMENT=*left,*overlayon
.
                    prtpage   Laser;*p=7500:9916,*ALIGNMENT=*right,*overlayoff,ARmask,*ALIGNMENT=*left,*overlayon:
                              *p=7600:9916,Typist
                    GOTO WIPEVARS
          else
.MS Word Version
.All taken care of before this point!!
                    call      wipevar1
          endif
prtown
        CLEAR     ADDESC1
         CLEAR     ADDESC2
         CLEAR     ADDESC3
         CLEAR     ADDESC4
         CLEAR     ADDESC5
         CLEAR     ADDESC6
         CLEAR     ADDESC7
         CLEAR     ADDESC8
         CLEAR     ADDESC9
         CLEAR     ADDESC10
         CLEAR     ADD$1
         CLEAR     ADD$2
         CLEAR     ADD$3
         CLEAR     ADD$4
         CLEAR     ADD$5
         CLEAR     ADD$6
         CLEAR     ADD$7
         CLEAR     ADD$8
         CLEAR     ADD$9
         CLEAR     ADD$10
         CLEAR     ADD$RTE
         CLEAR     ADD$RT1
         CLEAR     ADD$RT2
         CLEAR     ADD$RT3
         CLEAR     ADD$RT4
         CLEAR     ADD$RT5
         CLEAR     ADD$RT6
         CLEAR     ADD$RT7
         CLEAR     ADD$RT8
         CLEAR     ADD$RT9
         CLEAR     ADD$RT10
         CLEAR     SHORT
         CLEAR     GUARPRT
         CLEAR     AT1
         CLEAR     AT2
         CLEAR     AT3
         CLEAR     AT4
         CLEAR     AT5
         CLEAR     AT6
         CLEAR     AT7
         CLEAR     AT8
         CLEAR     AT9
         CLEAR     AT10
         move      c3 to NINVFRMFLAG              .mailer/remit copies some LO & internal charges remain hidden
                              move      "C" to elstcde                 force so Betsy Crones/non exclusive don't print Commission.
                              call      compute
         branch    apflag of wipevars
         PACK      MKEY FROM MLRN,z3
                              pack                compfld3 from mkey
          call                COMPKEY3
                              if (CNCTINACTIVE = "T")
                                        clear     cnctfname
                              endif
.....................
          packkey   NSEL2FLD,"1",LRN
          move      "NSEL2KEY-3",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
          endif
          if (MSWordFlag = 0)
............................................................................
                    call      prtinvfrm
.begin patch 4.1
                    call      PrtOwnerboxGui
                    
.                   call      prtinvfrm3
.end patch 4.1
                    call      Trim using ownlocty
                    Prtpage   Laser;*font=Font09B,*p=1500:2212,adjdate:
                              *p=5000:2212,invnum,dash,jstsubno:
                              *p=6750:2212,OMLRPON:
                              *p=1500:2400,ownlon:
                              *p=1500:2525,ownlonm:
                              *p=1500:2650,ownocpy:
                              *p=6750:2712,LRN:
                              *p=1500:2775,ownlosa:
                              *p=1500:2900,*ll,ownlocty,", ",ownlos," ",ownlozc:
                              *p=1500:3150,COMPCOMP
                    Prtpage   Laser;*p=6750:3212,OMDTEM,"/",OMDTED,"/",OMDTEC,OMDTEY:
                              *p=1500:3602,*font=Font010,comnt,*font=Font09B:
                              *p=1500:3800,OODES:
                              *p=1500:4050,OMLRKY:
                              *p=1500:4300,O1DES:
                              *p=1500:4440,NSEL2NAME:
                              *p=4250:4440,jstreasn:
                              *p=2500:4900,*ALIGNMENT=*right,*overlayoff,M$Qty:
                              *p=5000:4900,"@":
                              *p=5475:4900,M$PPM:
                              *p=7500:4900,M$GRoss,*overlayon,*ALIGNMENT=*Left
          else
.MS Word Version
          endif
        move       c0 to n9
        move       irexqty to n9
        compare    c0 to n9
        if         not equal
         MOVE      MASK9 TO M$QTYX
         EDIT      n9 TO M$QTYx
          move      iexppm to form32
         EDIT      FORM32 TO M$PPMX
          if (MSWordFlag = 0)
                    Prtpage   Laser;*p=2500:5025,*ALIGNMENT=*right,*overlayon,M$QtyX:
                              *p=5025:4900,"@":
                              *p=5475:4900,M$PPMX,*overlayoff,*ALIGNMENT=*Left
          else
.MS Word Version
          endif
        endif
.....................
          if (MSWordFlag = 0)
                    move      "5025",row
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,ADDESC1:
                              *p=5000:row,*ALIGNMENT=*right,*overlayoff,AT1:
                              *p=5475:row,ADD$RT1:
                              *p=7500:row,ADD$1,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,ADDESC2:
                              *p=5000:row,*ALIGNMENT=*right,*overlayoff,AT2:
                              *p=5475:row,ADD$RT2:
                              *p=7500:row,ADD$2,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,ADDESC3:
                              *p=5000:row,*ALIGNMENT=*right,*overlayoff,AT3:
                              *p=5475:row,ADD$RT3:
                              *p=7500:row,ADD$3,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,ADDESC4:
                              *p=5000:row,*ALIGNMENT=*right,*overlayoff,AT4:
                              *p=5475:row,ADD$RT4:
                              *p=7500:row,ADD$4,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,ADDESC5:
                              *p=5000:row,*ALIGNMENT=*right,*overlayoff,AT5:
                              *p=5475:row,ADD$RT5:
                              *p=7500:row,ADD$5,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,ADDESC6:
                              *p=5000:row,*ALIGNMENT=*right,*overlayoff,AT6:
                              *p=5475:row,ADD$RT6:
                              *p=7500:row,ADD$6,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,ADDESC7:
                              *p=5000:row,*ALIGNMENT=*right,*overlayoff,AT7:
                              *p=5475:row,ADD$RT7:
                              *p=7500:row,ADD$7,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,ADDESC8:
                              *p=5000:row,*ALIGNMENT=*right,*overlayoff,AT8:
                              *p=5475:row,ADD$RT8:
                              *p=7500:row,ADD$8,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,ADDESC9:
                              *p=5000:row,*ALIGNMENT=*right,*overlayoff,AT9:
                              *p=5475:row,ADD$RT9:
                              *p=7500:row,ADD$9,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,ADDESC10:
                              *p=5000:row,*ALIGNMENT=*right,*overlayoff,AT10:
                              *p=5475:row,ADD$RT10:
                              *p=7500:row,ADD$10,*ALIGNMENT=*left,*overlayon
                    move      "7500",row
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc1:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$1,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc2:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$2,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc3:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$3,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc4:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$4,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc5:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$5,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc6:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$6,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc7:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$7,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc8:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$8,*ALIGNMENT=*left,*overlayon
                    add       sixlpi,row
                    Prtpage   Laser;*p=800:row,adjdesc9:
                              *p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$9,*ALIGNMENT=*left,*overlayon
.
                    prtpage   Laser;*p=7500:9916,*ALIGNMENT=*right,*overlayoff,ARmask,*ALIGNMENT=*left,*overlayon:
                              *p=7600:9916,Typist
                    return
          else
.MS Word Version
          endif

WIPEVARS
         branch    hotflag of wipevar1,done
wipevar1 CLEAR     ADjDESC1
         CLEAR     adjdESC2
         CLEAR     adjdESC3
         CLEAR     adjdESC4
         CLEAR     adjdESC5
         CLEAR     adjdESC6
         CLEAR     adjdESC7
         CLEAR     adjdESC8
         CLEAR     adjdESC9
         CLEAR     adj$1
         CLEAR     adj$2
         CLEAR     adj$3
         CLEAR     adj$4
         CLEAR     adj$5
         CLEAR     adj$6
         CLEAR     adj$7
         CLEAR     adj$8
         CLEAR     adj$9
         CLEAR     ADDESC1
         CLEAR     ADDESC2
         CLEAR     ADDESC3
         CLEAR     ADDESC4
         CLEAR     ADDESC5
         CLEAR     ADDESC6
         CLEAR     ADDESC7
         CLEAR     ADDESC8
         CLEAR     ADDESC9
         CLEAR     ADDESC10
         CLEAR     ADD$1
         CLEAR     ADD$2
         CLEAR     ADD$3
         CLEAR     ADD$4
         CLEAR     ADD$5
         CLEAR     ADD$6
         CLEAR     ADD$7
         CLEAR     ADD$8
         CLEAR     ADD$9
         CLEAR     ADD$10
         CLEAR     ADD$RTE
         CLEAR     ADD$RT1
         CLEAR     ADD$RT2
         CLEAR     ADD$RT3
         CLEAR     ADD$RT4
         CLEAR     ADD$RT5
         CLEAR     ADD$RT6
         CLEAR     ADD$RT7
         CLEAR     ADD$RT8
         CLEAR     ADD$RT9
         CLEAR     ADD$RT10
         CLEAR     SHORT
         CLEAR     GUARPRT
         CLEAR     AT1
         CLEAR     AT2
         CLEAR     AT3
         CLEAR     AT4
         CLEAR     AT5
         CLEAR     AT6
         CLEAR     AT7
         CLEAR     AT8
         CLEAR     AT9
         CLEAR     AT10
         clear     amendnum
          if (MSWordFlag <> 0)
.MS Word Version
                    return
          endif
         BRANCH    HOTFLAG TO READADJ,DONE
         GOTO      readadj
.
testprt
         GOTO      readadj
.
READBILL
         bump      cobn by 2
         move      cobn to billtn
         reset     cobn
         PACK      NBILFLD FROM MLRN,COBN,BILLTN
         CALL      NBILKEY
         if        not over
                   MOVE      B3 TO CAREOF
                   MOVE      BILNAME TO CNCTFNAME
                move      bilcomp to COMPCOMP
             move    BILaddr to COMPADDR
          move    BILcity to COMPCITY
                   move    BILState to COMPSTATE
                move    BILzip to COMPZIP
         endif
         goto      goon
.
.
READMLR
         RETURN
.
..
OWNPREP  MOVE      OLON TO NOWNFLD
         REP       ZFILL IN NOWNFLD
         MOVE      PAYTN TO STR1
         MOVE      PAYTN TO PAYCHK
         PACK      PAYKEY FROM NOWNFLD,STR1
         REP       ZFILL IN PAYKEY
         MOVE      PAYKEY TO NPAYFLD
         REP       ZFILL IN NPAYFLD
         CLEAR     PCOMP                   *PCBUS DOES NOT CLEAR ON OVER.
         CLEAR      PNAME
         CLEAR      PSTREET
         CLEAR      PCITY
         CLEAR      PSTATE
         CLEAR      PZIP
         CALL      NPAYKEY
         CALL      NOWNKEY
         CMATCH    B1 TO PCOMP
         IF        NOT EOS
         COMPARE   C0 TO PAYCHK
         IF        EQUAL
         MOVE      PCOMP TO OWNOCPY
         MOVE      PNAME TO OWNLONM
         MOVE      PSTREET TO OWNLOSA
         MOVE      PCITY TO OWNLOCTY
         MOVE      PSTATE TO OWNLOS
         MOVE      PZIP TO OWNLOZC
         ENDIF
         ENDIF
         RETURN
.
charges  move      c0 to amendno
         move      c0 to index
.
nextadj  add      c1 to amendno
         add      c1 to index
         pack      njstfld from invnum,amendno
         rep       zfill in njstfld
         display   *p1:23,*el,njstfld
         call      njstkey
         goto      lstchrg if over
         move      c0 to CMPT92
         ADD       jstar TO CMPT92
         add       CMPT92 to formar
.
OUTadj1
         move     c0 to n2
         match    "99" to jstreasn
         if       equal
         move     adjres99 to nadjtext
         goto     chrgbr
         endif
         move     jstreasn to n2
         clear   nadjtext
         load     nadjtext from n2 of adjres1,adjres2,adjres3,adjres4,adjres5:
                 adjres6,adjres7,adjres8,adjres9,adjres10,adjres11,adjres12:
                 adjres13,adjres14,adjres15,adjres16,adjres17,adjres18,adjres19:
                 adjres20,adjres21,adjres22,adjres23,adjres24,adjres25,adjres26:
                 adjres27,adjres28,adjres29,adjres30,adjres31,adjres32,adjres33:
                 adjres34,adjres35,adjres36,adjres37,adjres38
.                 adjres34,adjres35,adjres36,adjres37
.
chrgbr
         BRANCH    INDEX OF CHARG1,CHARG2,CHARG3,CHARG4,CHARG5,CHARG6:
                   CHARG7,CHARG8,CHARG9
         GOTO      lstCHRG
.
CHARG1
         MOVE      MASK92 TO adj$1
         EDIT      CMPT92 TO adj$1
         MOVE      nadjtext TO adjdesc1
         GOTO      nextadj
CHARG2
         MOVE      MASK92 TO adj$2
         EDIT      CMPT92 TO adj$2
         MOVE      nadjtext TO adjdesc2
         GOTO      nextadj
CHARG3
         MOVE      MASK92 TO adj$3
         EDIT      CMPT92 TO adj$3
         MOVE      nadjtext TO adjdesc3
         GOTO      nextadj
CHARG4
         MOVE      MASK92 TO adj$4
         EDIT      CMPT92 TO adj$4
         MOVE      nadjtext TO adjdesc4
         GOTO      nextadj
CHARG5
         MOVE      MASK92 TO adj$5
         EDIT      CMPT92 TO adj$5
         MOVE      nadjtext TO adjdesc5
         GOTO      nextadj
CHARG6
         MOVE      MASK92 TO adj$6
         EDIT      CMPT92 TO adj$6
         MOVE      nadjtext TO adjdesc6
         GOTO      nextadj
CHARG7
         MOVE      MASK92 TO adj$7
         EDIT      CMPT92 TO adj$7
         MOVE      nadjtext TO adjdesc7
         GOTO      nextadj
CHARG8
         MOVE      MASK92 TO adj$8
         EDIT      CMPT92 TO adj$8
         MOVE      nadjtext TO adjdesc8
         GOTO      nextadj
CHARG9
         MOVE      MASK92 TO adj$9
         EDIT      CMPT92 TO adj$9
         MOVE      nadjtext TO adjdesc9
         GOTO      nextadj
.
lstchrg
         match      "27" to jstreasn
         goto       readadj if equal
         match      "14" to jstreasn
         goto       readadj if equal
         return
.............................................................................
.*......................................................................
..
DONE
          if (MSWordFlag = 1)
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.word.exe will still be running.
                    destroy   Range
                    clear     INPNAME
                    return
          endif
          if (HOTFLAG = C2)        .HOT
                    if (LSTNUM = "013260")
                              call      PrtOwn
                    endif
                    prtclose Laser
                    shutdown  "cls"
          endif
          prtclose Laser
         BRANCH    FORMFLAG TO MLREOJ
         SPLCLOSE
         CLOCK     TIME TO TIME
         DISPLAY   *P20:07,"DONE TIME : ",TIME
         shutdown  "cls"
         STOP
MLREOJ   move       C2 TO FORMFLAG
         display   *p10:14,"OWNer COPY"
         COMPARE   C1 TO HOTFLAG
         IF        EQUAL
         CLOSE     nadjust
.......................................................................
        if (osflag >= c6)
                PRTOPEN Laser,"\\NINS2\laser2","FAXFILE.PRN"
.          elseif (osflag = C3 | osflag = C4)
.                PRTOPEN Laser,"Laser2","FAXFILE.PRN"
        else   .(osflag = c0)         .Don't know prompt for printer
                PRTOPEN Laser,"-","FAXFILE.PRN"
        endif
.
         move       C0 TO COUNT
         goto      readadj
         ELSE
         move       C0 TO COUNT
         GOTO       PRTOWN
         ENDIF
................................................................
prtinvfrm
..................................................................
          if (FirstPage > 0)
                    prtpage   Laser;*NewPage
          endif
.begin patch 4.1
.begin patch 4.0
          prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon
.         IF        (OCompID = "P"and formflag = c1 or Ocompid2 = "P" and Formflag = c2)
.         call      debug
.         prtpage   Laser;*p=93:25,*font=fontO18b,"Pacific Lists, Inc.":
.                   *p=500:343,*font=font07,"1300 Clay St. 11th Floor":
.                   *p=400:443,"Oakland, CA 94612-1429":
.                   *p=335:543,"415-945-9450 ","·"," Fax 415-945-9451":
.                   *p=335:643,"A Division of Names in the News"
.         Else
.         prtpage               Laser;*Pictrect=*off,*PICT=0:1000:375:3375:NINLogo
.         endif
.end patch 4.1
.                              *Pictrect=*off,*PICT=0:1000:375:3375:NINLogo:
.         prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
.                   *Pictrect=*off,*PICT=0:1300:500:8100:NINLogo:
          PRtPage   Laser;*RECT=1115:1340:3750:4500:
                    *p=3785:1125,*font=Font014BI,"Invoice":
                    *p=500:1962,*font=Font08,*line=7580:1962:                      ;top Hori line
                    *p=500:1962,*line=500:3462:                          ;left side (top)
                    *p=7580:1962,*line=7580:3462:                        ;right side (top)
                    *p=500:3522,*line=7580:3522:                      ;Middle Hori line
                    *p=500:3522,*line=500:4522:                          ;left side (Middle)
                    *p=7580:3522,*line=7580:4522:                        ;right side (Middle)
                    *RECT=4582:10332:500:7580:                           ;bottom section drawn as rectangle
                    *p5625:10150,*font=Font09i,"Member Direct Marketing Association"
.end patch 4.0
          prtpage   Laser;*p750:2212,*font=Font07Dot5I,"Date:":
                    *p4250:2212,"Invoice##:":
                    *p5750:2212,"Mailer's P.O.:":
                    *p5750:2712,"NIN LR ##:":
                    *p5750:3212,"Mail Date:":
                    *p750:3800,"Mailer's Offer:":
                    *p750:4050,"Key:":
                    *p750:4300,"List:":
                    *p750:4712,*font=Font09B,*ULOn,"Original Billing",*font=Font07Dot5I,*ULOff:
                    *p750:4900,"Quantity Addressed:":
                    *p=4250:4900,"$ Per M":
                    *p=5750:4900,"Amount Due:":
                    *p750:7500,*font=Font09B,*ULOn,"Adjustments to Billing",*font=Font07Dot5I,*ULOff:
                    *font=Font07Dot5I
          add       C1,FirstPage
          return

.prtinvfrm2
..Client Version Info
.         prtpage   Laser;*p=3650:1625,*font=Font014b,"Client Copy":
.                   *p750:2400,*font=Font07Dot5I,"Client##:":
.                   *p515:10150,*font=Font09Bi,"Payment due upon receipt. Please return copy with payment.":
.                   *p=5750:9916,*font=Font09B,"Total Due:"
.         return
.prtinvfrm3
..List Owner Version Info
.         Prtpage   Laser;*p=2750:1650,*font=Font07Dot5I,"Amount Due: ",ownocpy:
.                   *p=5100:1800,ap1mask:
.                   *p750:2400,*font=Font07Dot5I,"Owner##:":
.                   *p750:3150,"Client:"
.         return
.begin patch 4.1
PRtNINLogo
              prtpage               Laser;*Pictrect=*off,*PICT=0:1000:375:3375:NINLogo
          return
PRtPLILogo
.begin patch 4.23
          call      PrtNinLogo
          return
.end patch 4.23
          prtpage   Laser;*p=93:25,*font=fontO18b,"Pacific Lists, Inc.":
                    *p=500:343,*font=font07,"180 Grand Ave. Suite 1365":
                    *p=400:443,"Oakland, CA 94612-3716":
                    *p=335:543,"415-945-9450 ","·"," Fax 415-945-9451":
                    *p=335:643,"A Division of Names in the News"
          return

..prtmlrboxGui Routine Laser
prtmlrboxGui
          clear     str2
          pack      str2 from osales10,osales
.begin patch 4.21
.         IF        (Ocompid = "P")
.         call      PrtPLILOgo
.         Elseif    (Ocompid2 = "P" & (str2 = "27" | str2 = "28"))
.         call      PrtPLILOgo
.         else
.         call      PrtNINLOgo
.         endif

          If        (MsWordFlag = c1)     .short pay call
                    If        (company = C2)
                    call      PrtPLILOgo
                    Else
                    call      PrtNINLOgo
                    endif               
          Else
                    IF        (Ocompid = "P")
                    call      PrtPLILOgo
                    Elseif    (Ocompid2 = "P" & (str2 = "27" | str2 = "28"))
                    call      PrtPLILOgo
                    else
                    call      PrtNINLOgo
                    endif
          Endif                         
.end patch 4.21
          prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
                        *p=3650:1625,*font=Font014b,"Client Copy":
                    *p750:2400,*font=Font07Dot5I,"Client##:":
                    *p515:10150,*font=Font09Bi,"Payment due upon receipt. Please return copy with payment.":
                    *p=5750:9916,*font=Font09B,"Total Due:"
               return
..prtownerboxGui Routine Laser
prtownerboxGui
          IF        (Ocompid = "P" & OCompid2  <> "N")
          call      PrtPLILOgo
          ElseIF    (Ocompid2 = "P")
          call      PrtPLILOgo
          else
          call      PrtNINLOgo
          endif
.               getitem       GreyFill,0,colornum
               prtpage         Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
                               *Fill=*On:
                               *Bgcolor=Colornum:
                              *RECT=1527:1902:2652:5652:
                              *Fill=*Off
               getitem        NoFill,0,colornum
               prtpage        Laser;*Bgcolor=Colornum:
                    *p=2750:1650,*font=Font07Dot5I,"Amount Due: ",ownocpy:
                    *p=5100:1800,ap1mask:
                    *p750:2400,*font=Font07Dot5I,"Owner##:":
                    *p750:3150,"Client:"
          REturn
.end patch 4.1
.begin patch 4.22
PrintMSWordInvoice Routine AutPtr,DimPtr,FrmPTr1  
          Move      FrmPtr1,Company
          Goto      Begin
.end patch 4.21
.end patch 4.22
..............................................................................
.
         INCLUDE   NORDIO.inc
          INCLUDE   COMPIO.inc
          INCLUDE   CNTIO.inc
         INCLUDE   NBILIO.inc
         INCLUDE   NOWNIO.inc
        INCLUDE   NAcdio.inc
         include   nadjio.inc
         INCLUDE   NJSTIO.inc
         INCLUDE   NPAYIO.INC
         include    compute.inc
         INCLUDE    ninvio.inc
         include    Ninvacdio.inc
         include   nmrgio.inc
         include   nshpio.inc
         include   ndatio.inc
         include   ndat3io.inc
          INCLUDE   NSEL2IO.INC
        INCLUDE   COMLOGIC.inc

