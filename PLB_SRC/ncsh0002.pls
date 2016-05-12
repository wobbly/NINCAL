............................................................................
.
. PROGRAM    : NCSH002
. DATE       : 04/19/88
. AUTHOR     : E.W. LAKE
. DESCRIPTION: PRODUCES NIN CASH RECEIPTS REGISTER.
.
............................................................................
.
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   NMOADD.inc
         include   hp.inc
.
         INCLUDE   CONS.inc
         INCLUDE   CONSACCT.inc
+
.begin patch 2.9
         INCLUDE   NCSHDD.inc
.START PATCH 3.4 ADDED LOGIC
              INCLUDE         NCTRDD.INC
.END PATCH 3.4 ADDED LOGIC
         INCLUDE   NADJDD.inc
.end patch 2.9
+
         INCLUDE   NADJCLDD.inc
+
         INCLUDE   NORDDD.inc
+
.START PATCH 3.8 REPLACED LOGIC
.         INCLUDE   NMLRDD.inc
              INCLUDE         COMPDD.inc
              INCLUDE         CNTDD.inc
.END PATCH 3.8 REPLACED LOGIC
         INCLUDE   NDAT3DD.INC
         include   nescdd.inc
.begin patch 2.8
.begin patch 3.92
.         INCLUDE   NINVDD.inc
              INCLUDE         ninvdd.inc
              Include         NinvAcddd.inc
.end patch 3.92
         include   nacddd.inc
         include   ndatdd.inc
         include   nshpdd.inc
.end patch 2.8
         inc       nmrgdd.inc
         INCLUDE   NOWNDD.INC
.
Release   Init      "4.5"     DLH  use spool file print option, prtplay once to pdf once to laser2
reldate   Init      "2014 April 22"
.Release   Init      "4.4"     DLH  Sunbelt PDF output
.reldate   Init      "05 April 2013"
.Release   Init      "4.3"     DLH  convert to PRTPAGE & PDF output
.reldate   Init      "17 January 2013"
.Release   Init      "4.2"     DLH  correction see code
.reldate   Init      "29 November 2010"
.Release   Init      "4.1"     DLH reas23
.reldate   Init      "14 October 2010"
.Release   Init      "4.0"     DLH turn off intercomp
.reldate   Init      "12 November 2009"
.release   init     "3.96"          DLH 2008January28 Clarify Inter comp transfers regarding escrows
.release   init     "3.95"          DLH 2007December18 Clarify Inter comp transfers
.release   init     "3.94"          DLH 2007December03 New reason code and fixed missing codes 15-21
.release   init     "3.93"          DLH 2007July11 Pacific Lists COnversion
.release   init     "3.92"          DLH 2005March02 Invoice COnversion
.release   init     "3.91"          ASH 11JAN2005 Mailer/Broker field CONVERSION:  DAT25N,ESCROW
.release   init     "3.9"          ASH 05AUG2004 LOGO CONVERSION
.release   init     "3.8"          ASH 27MAY2004 MAILER CONVERSION
.release   init     "3.7"          DMB 10JUL2002 Added Vars to Calculate Code P and all internal AR
.release   init     "3.6"          DMB 25JUN2002 Added Code to UPdate AR Total to controls.dat
.release   init     "3.5"         ASH17JUN2002 CONVERSION OF CONTROLS.DAT, NINCHK.DAT, DAT25N.DAT
.release   init     "3.4"         02MAY2002 ASH CHANGE DATE RECEIVED FROM DETAIL RECORD TO CONTROL RECORD
.release   init     "3.3"          6Mar2002 DLH More, more ,more flagging of Neg A/P to make acct pay attn
.release   init     "3.2"         25Feb2002 DLH More, more ,more flagging of "Differences" to make acct pay attn
.release   init     "3.1"         22Nov2000 DLH New exteral codes 'd' & "N"
.release   init     "3.0"         14OCt99 DLH cleanup
.release   init     "2.9"         26Apr99 DLH NINadj & nadjust Y2k
.release   init     "2.8"         26Apr99 DLH NININV Y2k
.release   init     "2.7"         18NOV98 JD changed mcomp printing.
.release   init     "2.6"        27oct98 ASH NINMOA Y2K, File expansion
.release   init     "2.5"        24Sep98 ASH NINMLR Y2K File expansion
.release   init     "2.4"        07aug96 DLH add option to print on dot matrix
.RELEASE   INIT     "2.3"        01jan95 flag escrow clients
.RELEASE   INIT     "2.2"       15NOV94 DLH NEW COMPUTE, CONSACCT.INC
.RELEASE  INIT      "2.1"       01NOV94 DLH DO NOT ADD 'O' & 'Q' TO AR.
.release  init       "2.0"      oct94 DLH/JD print related moa transactions
.
.RELEASE  INIT       "1.8"      21SEP94 DLH PRINT CHECK TOTAL AT EOJ.
.RELEASE  INIT      "1.7"       14MAR94 PRINT TO LASER. 
.RELEASE  INIT      "1.6"      24SEP93 DLH UPDATE INV IBRKNUM & IBRKCNT 
.
.release  init      "1.5"     change in mlr check break.
.
.RELEASE  INIT      "1.4"     DLH 20MAY93 SUBTOTAL BY MLR CHECK NUMBER.
.
.RELEASE  INIT      "1.3"    JD 3/11/92.
.                            MODIFIED NADJCLIO.inc, ADDED AP TOTALS, CHANGED
.                            PROGRAM VARIABLES FROM LAKE GROUP TO NINCA.
.RELEASE  INIT      "1.2"    DLH 1/8/92.
.RELEASE  INIT      "1.1"    E.W. LAKE  07/14/88
.                           PCBUS CONVERSION
.
.RELEASE INIT      "1.0"    E.W. LAKE   04/19/88
.                           INITIAL RELEASE
.
............................................................................
.
.needed for printing to PDF for non native fonts and things like Copyright to appear correctly in the pdf
PDF_FLAGS_WIN_ANSI_ENCODING EQU 128
PRTFLAG  FORM      1
LOCAL    INIT      "LOCAL"
PAGE     FORM      5
PRTLINES FORM      2
PAYAMT   FORM      10.2
MLRITOT  FORM      10.2       .Mlr total in (internal)
MLRETOT  FORM      10.2       .Mlr total in (external)
MLRMATOT  FORM     8.2       .Mlr total MOA applied
MLRAP    FORM      10.2
CHKITOT  FORM      10.2       .check total in (internal)
CHKETOT  FORM      10.2       .check total in (external)
CHKAP    FORM      10.2
FINAP    FORM      10.2
TOTAP2   FORM      10.2
.begin patch 3.93
TOTAP3   FORM      10.2
.end patch 3.93
GRDITOT  FORM      10.2        .grand total internal rec'd
GRDETOT  FORM      10.2        .grand total external rec'd
GRDMATOT FORM      8.2        .grand total MOA applied
TotAP1    FORM      10.2
GRDDIFF  FORM      10.2
PREAMT   FORM      10.2
GRDMLR   FORM      10.2
DIFF     FORM      10.2
DATEMASK INIT      "XX/XX/XX"
CSHDATE  DIM       8
SYSDATE  DIM       8
ARAMT    FORM      10.2
INVCHK1  DIM       6
APAMT    FORM      7.2
AP2AMT   FORM      9.2
.begin patch 3.93
AP3AMT   FORM      9.2
XnincAMT   FORM      9.2
Trans     Form      10.2
TotTrans  Form      10.2
TotCheck  Form      10.2
TotXninc  Form      10.2
HoldAP1   form      10.2           .$ of AP1 removed from total TotAP1 and added to transfer
.begin patch 3.95
SubTAp3Trans        Form      10.2             .track seperately there may be code I ap3's that drop in a seperage bucket
.end patch 3.95

MT$Ap1    Dim       17        .ap1
MT$Ap3    Dim       17        
MT$Xninc            DIM       17        
MT$Trans            DIM       17        
MT$Checks           DIM       17        
MASK92b   INIT      "Z,ZZZ,ZZZ,ZZZ.99-"
.end patch 3.93
DATEPRT1 DIM       8
DATEPRT2 DIM       8
.START PATCH 3.91 REPLACED LOGIC
.HOLDMLR  INIT      "    "
.holdbrk  dim       4
.HOLDCHK  DIM       6
HOLDMLR  INIT      "      "
holdbrk  dim       6
HOLDCHK  DIM       12
.END PATCH 3.91 REPLACED LOGIC
.begin patch 3.3
NegAPFlag     dim       1             .for detail
NegAPFlagEOJ  dim       1             .for end of job
NegARFlag     dim       1             .for detail
NegARFlagEOJ  dim       1             .for end of job
.end patch 3.3
.START PATCH 3.91 REPLACED LOGIC
.PRTCHK   DIM       6
PRTCHK   DIM       12
.END PATCH 3.91 REPLACED LOGIC
CASH     INIT      "CSH"
EXT      INIT      "EXT"
CNTNUMB  DIM       3
.
FORM2    FORM      2
FORM22   FORM      2.2
FORM7    FORM      7
FORM52   FORM      5.2
FORM11   FORM      11
ACOUNT    FORM      5
CO       FORM      1
AMNTRECD DIM       14
TRANDTE   DIM       8
.begin patch 3.2
DiffFlag       Init "N"
.end patch 3.2
.Start Patch #2.6 - extended var to hold century
.DINVDTE  DIM       8
DINVDTE  DIM       10
.End Patch #2.6 - extended var to hold century
DETAIL$  FORM      8.2
DOLLAR   INIT      "$$,$$$,$$$.99-"
MOAFLAG  FORM      2
escFLAG  init      "N"
lasrflag init      "T"            generally true unless a/p clerk has req
eojflag  init      "N"
.begin patch 2.9
shipsw   dim        1
mrgsw    dim        1
.end patch 2.9
.START PATCH 3.4 ADDED LOGIC
ControlFlag form              "0"
.END PATCH 3.4 ADDED LOGIC
.Patch3.6
HCNUM     DIM        3          Hold vars for control update for AR check
HCNUMDATE DIM        8          Hold vars for control update for AR check
.subPatch3.6
.Patch3.7
CODEPAR     FORM     10.2       Var for Code P Transactions-Prepaid listowner rec'd check to pay AR
CROSSCHKAR  FORM     10.2       Var for CrossCheckTotal for AR
.subPatch3.7
.begin patch 3.93
CmpPrtNme Dim       18
.end patch 3.93
.END PATCH 3.9 ADDED LOGIC
.begin patch 4.3
          Include   PrtPagedd.inc
FileCheck FIle
trapcount form      4
.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font
font6   font
font7   font
        create  font1,"Times New Roman",size=9
        create  font2,"Times New Roman",size=9,bold
        create  font3,"Times New Roman",size=9,italic
        create  font4,"Times New Roman",size=10
        create  font5,"Times New Roman",size=10,bold
        create  font6,"Times New Roman",size=10,italic
        create  font7,"Times New Roman",size=7
font8    font
        create  font8,"Times New Roman",size=8
font8i    font
        create  font8i,"Times New Roman",size=8,italic        
.end patch 4.3
.begin  patch 4.5
spoolfle    dim     45
.end  patch 4.5

+........................................................................
.
         MOVE      "NCSH0002 " TO PROGRAM
         MOVE      "Cash Receipts" TO STITLE
.         MOVE      "NINCSH" TO INPNAME
         MOVE      "Names In The News" TO COMPNME
.         MOVE      LOCAL TO PRTNAME
         CLEAR     HOLDCHK
         match     "NOLASER" in comment
         if        equal
         move      "F" to lasrflag
         endif
         MOVE      "NINCSH" TO NCSHNAME
         MOVE      C1 TO NCSHPATH
         MOVE      C4 TO Nmoapath
         MOVE      C1 TO NOWNPATH
         CLOCK     DATE TO SYSDATE
         MOVE      SYSDATE TO TODAY
         IFNZ      PC
         UNPACK    SYSDATE INTO MM,DD,YY
         PACK      SYSDATE FROM MM,SLASH,DD,SLASH,YY
         XIF
         CALL      PAINT
         TRAP      END IF F5
         MOVE      "Exit" TO PF5
         CALL      FUNCDISP
         MOVE      c59 TO PRTLINES
         Move       "10501",row
.
         DISPLAY   *P01:06,"Input File  :":
                   *P01:07,"Print File  :":
                   *P01:09,"Record Count:"
.
INPGET   DISPLAY   *P15:06,INPNAME
         TRAP      INPNG IF IO
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         DISPLAY   *P15:06,INPNAME
         MOVE      INPNAME TO NCSHNAME
         GOTO      PRTGET
INPNG    NORETURN
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *P15:06,INPNAME
         GOTO      INPGET
.
PRTGET   MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MOVE      C1 TO PRTFLAG
         MATCH     LOCAL TO PRTNAME
         GOTO      START IF EQUAL
         MOVE      C2 TO PRTFLAG
         PACK      PRTFILE WITH PDRIVE,PRTNAME
.begin patch 4.3
.         SPLOPEN   PRTFILE
.         SPLOPEN   "\\nins2\laser2","R"
          call      GetWinVer                 
.begin patch 4.4
.          Call      GetPDFPath
.          Call      PDF995Auto
.          call      SetPDFFlag
.          Prtopen   Laser,"",prtname                       .testing
.          PRTOPEN   Laser,"PDF995",prtname
          pack      str55 from "c:\work\pdf\",prtname,".pdf"
.begin  patch 4.5
           pack       spoolfle,"\\nins1\e\data\",prtname,".spl"
           PRTOPEN   Laser,"PDF:",str55,Flags=2,Spoolfile=spoolfle
.end  patch 4.5
          pack    str45,prtname
.end patch 4.4
.end patch 4.3
         DISPLAY   *P15:07,PRTNAME
         GOTO      START
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
.
+........................................................................
.
START    MOVE      C0 TO PAYAMT
         MOVE      C0 TO ARAMT
         MOVE      C0 TO APAMT
         MOVE      C0 TO AP2AMT
.begin patch 3.93
         MOVE      C0 TO AP3AMT
         MOVE      C0 TO XNincAMT
.end patch 3.93
         MOVE      C1 TO MOAFLAG
.begin patch 3.3
               move           no to NegAPFlag
               move           no to NegARFlag
.end patch 3.3
         CALL      NCSHSEQ
         GOTO      EOJ IF OVER
.START PATCH 3.4 ADDED LOGIC
              if (ControlFlag = C0)
.START PATCH 3.5 REPLACED LOGIC
.Start Patch3.6
               move                CNUM to HCNUM
               move                CNUMDATE to HCNUMDATE
.subPatch3.6
.              move           CNUM,NCTRFLD
               pack           NCTRFLD,CNUM,CNUMDATE
.END PATCH 3.5 REPLACED LOGIC
               rep            zfill,NCTRFLD
                      move    "START-NCTRKEY",Location
              pack    KeyLocation,"Key: ",NCTRFLD
                      call    NCTRKEY
               move           C1,ControlFlag
              endif
.END PATCH 3.4 ADDED LOGIC
.begin patch 3.93 added logic
          if        (NCTRCOMP = "P")
          Move      "Pacific Lists Inc",CmpPrtNme
          else
          MOVe      "Names in the News",CmpPRtNme
          endif
.end patch 3.93 added logic
         move      cnum to cntnumb
         clear     nmoanme4
         append    "nmoa" to nmoanme4
         append    cnum to nmoanme4
         reset     nmoanme4
         ADD       C1 TO N6
         DISPLAY   *P15:09,N6
.
         MATCH     NCSHCHK TO HOLDCHK
         CALL      NEWCHK IF NOT EQUAL
.
         MATCH     CMLR TO HOLDMLR
         CALL      NEWMLR IF NOT EQUAL
.
.begin patch 2.9
.         MOVE      CAMOUNT TO CVTFLD
.         CALL      CVT
.         MOVE      NUM102 TO PAYAMT
         move       camount to payamt
.end patch 2.9

         MOVE      DATEMASK TO DATEPRT2
.CMO,CDY,CYR pulled from DAT25N using NCSHDD.INC
.With NININV.DAT conversion CSHDATE might want to be formatted: CCYYMMDD
         PACK      CSHDATE FROM CMO,CDY,CYR
         EDIT      CSHDATE  TO DATEPRT2
.
              match         "596346" to CLR
              call          Debug if equal
              
.begin patch 3.39
          If        (CEXTCD = b1 or CEXTCD = "I")
.         CMATCH    B1 TO CEXTCD
         GOTO      INTERNAL
.         GOTO      INTERNAL IF EQUAL
          endif
.end patch 3.39
.
         CMATCH    "O" TO CEXTCD                really internal using MOA
         IF        EQUAL
         MOVE      C2 TO MOAFLAG
         GOTO      INTERNAL 
         ENDIF
.
.begin patch 3.1
.         CMATCH    "D" TO CEXTCD                 applying to MOA
.         IF        EQUAL
         if         (cextcd = "D" or cextcd = "d" or cextcd = "N")
.end patch 3.1
         MOVE      C3 TO MOAFLAG
         ENDIF
.
         CMATCH    "Q" TO CEXTCD                really internal using MOA
         IF        EQUAL                        & an external 'P' combined
         MOVE      C2 TO MOAFLAG
         GOTO      INTERNAL 
         ENDIF
.
. EXTERNAL ENTRY.
. IF IT HAS AN UNPAID LR, UPDATE THE INVOICE RECORD.
.
EXTERNAL MATCH     B6 TO CLR
         GOTO      EXTNOLR IF EQUAL
         MOVE      CLR TO NORDFLD                  .dlh 01feb95
         MOVE      C1    TO NORDPATH
         CALL      NORDKEY
         MOVE      CLR TO NINVFLD
         MOVE      C1 TO NINVPATH
         CALL      NINVKEY
         GOTO      EXTNOLR IF OVER
         CMATCH    "P" TO STATB
         GOTO      EXTNOLR IF EQUAL
.begin patch 3.39
.statb status should have caught but lets double check
          Cmatch    "I",CEXTCD                    .Inter company transaction?
          GOTO                Internal IF EQUAL   .yep
.                   GOTO                EXTNOLR IF EQUAL    .yep
.end patch 3.39

         CMATCH    "A" TO CEXTCD                   .no money in - prepaying LO
         goto       chkonly if equal
         PACK      INVCHK1 WITH EXT,CEXTCD,CNUM
         MOVE      CAMOUNT  TO MLRPAYR
.         MOVE      CSHDATE TO MLRPAYD
.START PATCH 3.4 REPLACED LOGIC
.         PACK      MLRPAYD FROM cc,cyr,CMO,CDY
         PACK      MLRPAYD,NCTRDATE
.END PATCH 3.4 REPLACED LOGIC
.START PATCH 3.91 REPLACED LOGIC
.         move      ncshchk to imlrchk
         unpack     ncshchk,str6,imlrchk
.END PATCH 3.91 REPLACED LOGIC
.begin patch 3.0
         goto      chkonly1                         . 14OCt99 DLH skip re-re-read of order and ivoice it is wiping info.
chkonly  MOVE      CLR TO NORDFLD
         MOVE      C1 TO NORDPATH
         CALL      NORDKEY
         MOVE      CLR TO NINVFLD
         MOVE      C1 TO NINVPATH
         CALL      NINVKEY
chkonly1 MOVE      OLON TO LON            add owner ##
.end patch 3.0
         move      obrknum to ibrknum     add brk #3
         move      obrkcnt to ibrkcnt

         CALL      NINVUPD
EXTNOLR
.START PATCH 3.91 REPLACED LOGIC
.         match     b4 to cmlr
         match     b6 to cmlr
.END PATCH 3.91 REPLACED LOGIC
         goto      det1 if equal
         goto      det1 if eos
         type      obrknum
         if        not equal
         move      "0000" to obrknum
         endif
         PACK      NMOAFLD4 FROM obrknum,omlrnum
         MOVE      NO TO OVER
         CLEAR     TRANSNUM
         CALL      NMOAKEY
CHKAGAIN GOTO      DET1 if over
         match     clr to LRNUM
         IF        EQUAL
         MATCH     CNUM TO CONTROL
         IF        NOT EQUAL
         CLEAR     TRANSNUM
         CALL      NMOAKS
         goto      det1 if over
         GOTO      CHKAGAIN
         ENDIF
         else
         CLEAR     TRANSNUM
         call      nmoaks
         goto      det1 if over
         goto      chkagain
         ENDIF
DET1     
.begin patch 4.3
.               COMPARE        c59 TO PRTLINES
.               CALL           HEADING IF NOT LESS
                    if        (row >= 10000)
                    call      heading
                    endif
.end patch 4.3
.begin patch 4.3
          add       sixlpi,row
.          prtpage   Laser;*p=1:row,CMLR,*p=220:row,CLR,*p=7375:row,*alignment=*right,dateprt2,*p=8125:row,CEXtcd,*alignment=*left
          prtpage   Laser;*p=1:row,CMLR,*p=2500:row,*alignment=*right,CLR,*p=7375:row,*alignment=*right,dateprt2,*p=8125:row,CEXtcd,*alignment=*left
.         PRINT     *01,CMLR:
.                      *36,CLR:
.                      *100,DATEPRT2;
         match      "Q" to cextcd
         IF         NOT EQUAL
          prtpage   Laser;*p=8000:row,*alignment=*right,PayAmt,*alignment=*left
.         PRINT       *109,PAYAMT;
         ELSE
          prtpage   Laser;*p=8000:row,*alignment=*right,C0,*p=8125:row,CEXtcd,*alignment=*left
.         PRINT       *109,C0;
         ENDIF
.         PRINT       *122,CEXTCD
.end patch 4.3
        type       TRANSNUM
        GOTO       NOMOA IF not EQUAL
         MOVE      DOLLAR TO AMNTRECD
         MULT      "-1" BY ONAMOUNT
         MOVE      ONAMOUNT TO DETAIL$
         EDIT      DETAIL$ TO AMNTRECD
.Start Patch #2.6 - remmed and replaced lines
.         UNPACK    INVDATE INTO MM,DD,YY
.         PACK      DINVDTE FROM MM,SLASH,DD,SLASH,YY
         UNPACK    INVDATE INTO str2,YY,MM,DD
         PACK      DINVDTE FROM MM,SLASH,DD,SLASH,str2,YY
.End Patch #2.6 - remmed and replaced lines
         CLEAR     TRANDTE
         TYPE      TRANDATE
          CLEAR     RDESC
         COMPARE   "99" TO REASON
         CALL      REASON IF EQUAL
         LOAD      RDESC FROM REASON OF REAS1,REAS2,REAS3,REAS4,REAS5:
.begin patch 3.94
.                   REAS6,REAS7,REAS8,REAS9,REAS10,REAS11,REAS12,REAS13,REAS14
                   REAS6,REAS7,REAS8,REAS9,REAS10,REAS11,REAS12,REAS13,REAS14:
                   REAS15,REAS16,REAS17,REAS18,REAS19,REAS20,REAS21,REAS22,REAS23
.end patch 3.94
.begin patch 4.3
          add       Sixlpi,row
          prtpage   Laser;*p=1:row,trandte,*p=1000:row,Control,*p=1412:row,"MOA ENTRY##",*p=2118:row,Transnum:
                    *p=2647:row,CHECKNUM,*p=4529:row,DINVDTE,*p=4941:row,AMNTRECD,*Boldon,b1,MoaComp,*BOLDoff
.         PRINT     *1,TRANDTE,*17,CONTROL,*24,"MOA ENTRY##",*36,transnum:
.                   *45,CHECKNUM,*77,DINVDTE,*FLUSH;
.        PRINT     *84,AMNTRECD,HPBon,b1,MoaComp,HpBoff
.end patch 4.3
        ADD        C1 TO PRTLINES
        goto       det1x
NOMOA   CALL       PRECOMP
        CMATCH     YES TO PREPAYSW
        IF         EQUAL
.begin patch 4.3
.        cmatch     true to lasrflag
.        if         equal      
          add       sixlpi,row
          prtpage   laser;*p=1:row,*BOLDON,*ULON,"ASSOCIATED MOA ENTRY NOT FOUND!!!!",*BOLDOFF,*ULOFF
.         PRINT     *1,HPBON,HPITALIC,"ASSOCIATED MOA ENTRY NOT FOUND!!!!",HPBOFF,HPUPRGHT
.         else
.         PRINT     *1,p24BON,p24ITAL,"ASSOCIATED MOA ENTRY NOT FOUND!!!!",p24BOFF,P24UPRT
.         endif
.end patch 4.3
         ADD       C1 TO PRTLINES
        ENDIF
det1x    BRANCH    MOAFLAG OF DET1XA,DET1XB,det1xa
DET1XA   ADD       PAYAMT TO MLRETOT
         ADD       PAYAMT TO CHKETOT
         GOTO      DET1XC
.
DET1XB   ADD       PAYAMT TO GRDMATOT
.
DET1XC   MATCH     "P" TO CEXTCD
         IF        EQUAL
         ADD       PAYAMT TO PREAMT
.;Patch3.7
         ADD       PAYAMT TO CODEPAR
.;subpatch3.7
         ENDIF
         MATCH     "Q" TO CEXTCD
         IF        EQUAL
         ADD       PAYAMT TO PREAMT
         ENDIF
         ADD       C1 TO PRTLINES
         clear     transnum
         GOTO      START
.
. INTERNAL ENTRY.
. MUST HAVE A VALID UNPAID INVOICE.
.
INTERNAL MOVE      CLR TO NINVFLD
         MOVE      C1 TO NINVPATH
         CALL      NINVKEY
         GOTO      NOINV IF OVER

         CMATCH    "P" TO STATB
         GOTO      INVPAID IF EQUAL

.         MOVE      AR TO CVTFLD
.         CALL      CVT
.         MOVE      NUM102 TO ARAMT
         move      ar to aramt
.         MOVE      AP1 TO CVTFLD
.         move      c0 to num102
.         CALL      CVT
.         MOVE      NUM102 TO APAMT
         move      ap1 to apamt
.         MOVE      AP2 TO CVTFLD
         move      c0 to num102
.         CALL      CVT
         MOVE      ap2 TO AP2AMT
.begin patch 3.93         
         MOVE      ap3 TO AP3AMT
         MOVE      Xninc TO XnincAMT
.end patch 3.93         
.         CMATCH    "*" TO ADJC          ADJUSTED?
.         GOTO      InTB IF NOT EQUAL        NO.
.DLH 28nov94 - don't depend on flag - check file anyway.
         MOVE      LRN TO NADJFLD
         rep       zfill in nadjfld
         CALL      NADJKEY
         GOTO      INTB IF OVER
         CALL      NADJCALC               APPLIES ADJUSTMENTS TO INV VARIABLES.
.
. UPDATE THE INVOICE RECORD.
. PUT THE CASH CONTROL IN THE CHECK NUMBER FIELD
. UPDATE THE MLR AMT RECEIVED & MLR DATE RECEIVED FIELDS.
.
INTB     PACK      INVCHK1 WITH CASH,CNUM
         MOVE      CAMOUNT  TO MLRPAYR
.         MOVE      CSHDATE TO MLRPAYD
.START PATCH 3.4 REPLACED LOGIC
.         PACK      MLRPAYD FROM cc,cyr,CMO,CDY
         PACK      MLRPAYD,NCTRDATE
.END PATCH 3.4 REPLACED LOGIC
.START PATCH 3.91 REPLACED LOGIC
.         move      ncshchk to imlrchk
         unpack               ncshchk,str6,imlrchk
.END PATCH 3.91 REPLACED LOGIC
         cmatch    "Q" to cextcd          .previously cancelled paid lo?
         if        not equal              .no
         cmatch    "P" to cextcd          .previously paid lo?
           if        not equal              .no
           MOVE      INVCHK1 TO CHKN1       .reflect control in process.
           endif
         endif
         MOVE      CLR TO NORDFLD
         MOVE      C1    TO NORDPATH
         CALL      NORDKEY
.         MOVE      CLR TO NINVFLD          . No need to do again 11/19/99 jd
.         MOVE      C1 TO NINVPATH
.         CALL      NINVKEY
         MOVE      OLON TO LON
         CALL      NINVUPD
.
         type      obrknum
         if        not equal
         move      "0000" to obrknum
         endif
         PACK      NMOAFLD4 FROM obrknum,omlrnum
.         MOVE      NO TO OVER
.begin  patch 3.3
.begin patch 4.2
.correction 11/29/2010 as long as adjusted AR is not negative we should be fine
.         compare   c0 to AR
.         if        less
.         move      yes to NegARFlag
.         endif
.end patch 4.2
         compare   c0 to ARamt
         if        less
         move      yes to NegARFlag
         endif
.
         compare   c0 to ap1
         if        less
         move      yes to NegAPFlag
         endif
         compare   c0 to apamt
         if        less
         move      yes to NegAPFlag
         endif
.
         compare   c0 to ap2
         if        less
         move      yes to NegAPFlag
         endif
.begin patch 3.93
          if        (AP3amt < C0)
          move      yes to NegAPFlag
          endif
.end patch 3.93


.end  patch 3.3
         CLEAR     TRANSNUM
         CALL      NMOAKEY
         GOTO      DET2a if over
chkagan2 match     clr to LRNUM
         IF        EQUAL
         MATCH     CNUM TO CONTROL
         IF        NOT EQUAL
         CLEAR     TRANSNUM
         call     rotdial
         CLEAR     TRANSNUM
         CALL      NMOAKS
         goto      det2a if over
         GOTO      CHKAGAN2
         ENDIF
         else
         CLEAR     TRANSNUM
         call      nmoaks
         goto      det2a if over
         GOTO      CHKAGAN2
         ENDIF
det2a    MOVE      DATEMASK TO DATEPRT1
.Start Patch #2.6 - remmed and replaced line
.         PACK      INVDATE FROM INVDTEM,INVDTED,INVDTEY
.Century hardcoded - TEMPORARY!!!!!
         PACK      INVDATE FROM CC,INVDTEY,INVDTEM,INVDTED
.LINE WILL BE FOLLOWING WHEN NININV IS CONVERTED!!!!
.         PACK      INVDATE FROM INVDTEC,INVDTEY,INVDTEM,INVDTED
.End Patch #2.6 - remmed and replaced line
.Start Patch #2.6 - remmed and replaced line
.         EDIT      INVDATE  TO DATEPRT1
         pack      str6,INVDTEM,INVDTED,INVDTEY
         EDIT      str6  TO DATEPRT1
.End Patch #2.6 - remmed and replaced line         
.????? c0 compare don't know why there commented out DLH 20Mar95
.         COMPARE   C0 TO ARAMT             .0 DUE /PREPAID?
.         IF        NOT EQUAL
.begin patch 3.3
               IF             (NegARFlag = "Y")
               call           NegAR
               endif
.end patch 3.3
.begin patch 3.3
          IF        (Cextcd <> "I")              .if its a code I don't check we know they don't match
          COMPARE   PAYAMT TO ARAMT
          GOTO      INTDIFF IF NOT EQUAL
          Endif
.end patch 3.3

.         ENDIF
.        
DET2     
.begin patch 4.3
.               COMPARE        c59 TO PRTLINES
.               CALL           HEADING IF NOT LESS
                    if        (row >= 10000)
                    call      heading
                    endif
.end patch 4.3
.begin patch 3.3
.begin patch PLI
.begin patch 4.0 turn off Intercomp
.         if        ((AP3amt <> 0 or XNincamt <> 0) & CEXTCD  <> "I")      We have an IntraCorp transaction/transfer
.
.                             if        (NegAPFlag = "Y")
.                   move           yes to NegAPFlagEOJ                  .set flag for totals
.                             PRINT        *01,CMLR:
.                                          *36,CLR:
.                                          *45,INVNUM:
.                                          *54,hpbon,"NO CHECK":
.                                          *64,APAMT,hpboff:
.                                          *77,DATEPRT2;
.                                   move           No to NegAPFlag
.                             ADD       C1 TO PRTLINES
.                                   else
.                             PRINT        *01,CMLR:
.                                          *36,CLR:
.                                          *45,INVNUM:
.                                          *54,hpbon,"NO CHECK",HpBoff:
.                                          *64,APAMT:
.                                          *77,DATEPRT2;
.                             ADD       C1 TO PRTLINES
.                                   endif
.
.                   ADD       C1 TO PRTLINES
..begin patch 3.95            
..                  SUB       apamt,TotAP1     Testing JD/DH turned back on 1/4/08
.                   SUB       apamt,TotAP1
..end patch 3.95              
.                   Add       apamt,HoldAp1
.                   sub       Trans,Trans
.                   add       Apamt,Trans
.                   add       Xnincamt,Trans
.                   Add       AP3amt,Trans
.                   add       Trans,TotTrans
..begin patch 3.95            
.                   Add       AP3amt,SubTAp3Trans
..end patch 3.95              
.
.                   add       XnincAmt,TotXninc
.                   PRINT       *N,*01,HPBOn,"Inter company transfer,",*54,Trans,*LL," = ",AP3amt," + ",xnincamt," + ",Apamt,HPBoff
          
.         Else
                    if        (NegAPFlag = "Y")
                    move           yes to NegAPFlagEOJ                  .set flag for totals
.begin patch 4.3
                    add       Sixlpi,row
                    PRtpage   Laser;*p1:row,CMLR,*p=2500:row,CLR,*p=3500:row,INVNUM:
                              *p4875:row,*alignment=*Right,Dateprt1:
                              *p5500:row,*alignment=*Right,*BOLDON,APAMT,*BoldOFF:
                              *p6125:row,*alignment=*Right,DATEPRT2,*alignment=*left
.                    PRINT        *01,CMLR:
.                                 *36,CLR:
.                                 *45,INVNUM:
.                                 *54,DATEPRT1:
.                                 *64,hpbon,APAMT,hpboff:
.                                 *77,DATEPRT2;
                          move           No to NegAPFlag
                    ADD       C1 TO PRTLINES
                          else
                    add       sixlpi,row
                    PRtpage   Laser;*p1:row,CMLR,*p=2500:row,CLR,*p=3500:row,INVNUM:
                              *p4875:row,*alignment=*Right,Dateprt1:
                              *p5500:row,*alignment=*Right,APAMT:
                              *p6125:row,*alignment=*Right,DATEPRT2,*alignment=*left
.                    PRINT        *01,CMLR:
.                                 *36,CLR:
.                                 *45,INVNUM:
.                                 *54,DATEPRT1:
.                                 *64,APAMT:
.                                 *77,DATEPRT2;
                    ADD       C1 TO PRTLINES
.end patch 4.3
                          endif
.         endif                               
.end patch 4.0 turn off Intercomp
.begin patch 3.3
         match      "Q" to cextcd
         IF         not EQUAL
.begin patch 4.3
........          add       sixlpi,row
          prtpage   Laser;*p=6750:row,*alignment=*Right,Payamt,*p=8125:row,CEXtcd,*alignment=*left
.         PRINT       *86,PAYAMT;
         ELSE
          prtpage   Laser;*p=6750:row,*alignment=*Right,c0,*p=8125:row,CEXtcd,*alignment=*left
.         PRINT       *86,C0;
         ENDIF
.         PRINT       *122,CEXTCD
.end patch 4.3
         type      TRANSNUM
         GOTO       NOMOA2 IF not EQUAL
         MOVE      DOLLAR TO AMNTRECD
         MULT      "-1" BY ONAMOUNT
         MOVE      ONAMOUNT TO DETAIL$
         EDIT      DETAIL$ TO AMNTRECD
.Start Patch #2.6 - remmed and replaced lines
.         UNPACK    INVDATE INTO MM,DD,YY
.         PACK      DINVDTE FROM MM,SLASH,DD,SLASH,YY
         UNPACK    INVDATE INTO str2,YY,MM,DD
         PACK      DINVDTE FROM MM,SLASH,DD,SLASH,str2,YY
.End Patch #2.6 - remmed and replaced lines
          CLEAR     RDESC
         COMPARE   "99" TO REASON
         CALL      REASON IF EQUAL
         LOAD      RDESC FROM REASON OF REAS1,REAS2,REAS3,REAS4,REAS5:
.begin patch 3.94
.                   REAS6,REAS7,REAS8,REAS9,REAS10,REAS11,REAS12,REAS13,REAS14
                   REAS6,REAS7,REAS8,REAS9,REAS10,REAS11,REAS12,REAS13,REAS14:
                   REAS15,REAS16,REAS17,REAS18,REAS19,REAS20,REAS21,REAS22,Reas23
.end patch 3.94
.begin patch 4.3
          add       sixlpi,row
          prtpage   Laser;*p=1:row,trandte,*p=1000:row,Control,*p=1412:row,"MOA ENTRY##",*p=2118:row,Transnum:
                    *p=2647:row,CHECKNUM,*p=4529:row,DINVDTE,*p=4941:row,AMNTRECD,*Boldon,b1,MoaComp,*BOLDoff
.         PRINT     *1,TRANDTE,*17,CONTROL,*24,"MOA ENTRY##",*36,transnum:
.                   *45,CHECKNUM,*77,DINVDTE,*FLUSH;
.         PRINT     *84,AMNTRECD
.end patch 4.3
         ADD      C1 TO PRTLINES
         goto      det2x
NOMOA2   CALL      PRECOMP
         CMATCH     YES TO PREPAYSW
         IF         EQUAL
.begin patch 4.3
          add       sixlpi,row
          prtpage   Laser;*p=1:row,*BOLDON,*ULON,"ASSOCIATED MOA ENTRY NOT FOUND!!!!",*BOLDOFF,*ULOFF
.        cmatch     true to lasrflag
.        if         equal      
.         PRINT     *1,HPBON,HPITALIC,"ASSOCIATED MOA ENTRY NOT FOUND!!!!",HPBOFF,HPUPRGHT
.         else
.         PRINT     *1,p24BON,p24ITAL,"ASSOCIATED MOA ENTRY NOT FOUND!!!!",p24BOFF,P24UPRT
.         endif
.end patch 4.3
         ADD       C1 TO PRTLINES
         ENDIF
det2x    BRANCH    MOAFLAG OF DET2XA,DET2XB,det2xc
DET2XA   ADD       PAYAMT TO MLRITOT
         ADD       PAYAMT TO CHKITOT
         GOTO      DET2Xd
DET2XB   ADD       PAYAMT TO mlrMATOT
         GOTO      DET2Xd
DET2Xc   ADD       PAYAMT TO MLRITOT
         ADD       PAYAMT TO CHKITOT
         GOTO      DET2Xe
DET2Xd   ADD       APAMT  TO MLRAP
         ADD       AP2AMT TO TOTAP2
.begin patch 3.93         
         ADD       AP3AMT TO TOTAP3
.end patch 3.93         
         ADD       APAMT  TO CHKAP
det2xe   ADD       C1 TO PRTLINES
         clear     transnum
         GOTO      START
.
.begin patch 3.3
NegAR
.begin patch 4.3
.               COMPARE        c59 TO PRTLINES
.               CALL           HEADING IF NOT LESS
                    if        (row >= 10000)
                    call      heading
                    endif
.end patch 4.3
               Move           Yes to NegARFlagEOJ               .set flag for end of report
.begin patch 4.3
          add       Sixlpi,row
          PRtpage   Laser;*p1:row,*BOLDON,CMLR,*p=2500:row,CLR,*p=3500:row,INVNUM:
                    *p3675:row,*alignment=*Right,Dateprt1:
                    *p4325:row,*alignment=*Right,APAMT:
                    *p5025:row,*alignment=*Right,DATEPRT2,*alignment=*left:
                    *p5025:row,*alignment=*Left,PayAmt,b2,"NEG AR",*BOLDOFF
.         PRINT        *01,HPBON,CMLR:
.                      *36,CLR:
.                      *45,INVNUM:
.                      *54,DATEPRT1:
.                      *64,APAMT:
.                      *77,DATEPRT2:
.                      *86,PAYAMT:
.                      *117,"NEG AR",HpBoff
.end patch 4.3
               add            c1 to prtlines
               return
.end patch 3.3
INTDIFF  MOVE      PAYAMT TO DIFF
         SUBTRACT  ARAMT  FROM  DIFF
.begin patch 4.3
.               COMPARE        c59 TO PRTLINES
.               CALL           HEADING IF NOT LESS
                    if        (row >= 10000)
                    call      heading
                    endif
.end patch 4.3
.begin patch 3.2
               Move           Yes to DiffFlag               .set flag for end of report
.begin patch 4.3
          add       Sixlpi,row
          PRtpage   Laser;*p1:row,*BOLDON,CMLR,*p=2500:row,CLR,*p=3500:row,INVNUM:
                    *p3675:row,*alignment=*Right,Dateprt1:
                    *p4325:row,*alignment=*Right,APAMT:
                    *p5025:row,*alignment=*Right,DATEPRT2,*alignment=*left:
                    *p5025:row,*alignment=*Left,PayAmt,b2,"DIFF: ",Diff,*BOLDOFF
.         PRINT        *01,HPBON,CMLR:
.                      *36,CLR:
.                      *45,INVNUM:
.                      *54,DATEPRT1:
.                      *64,APAMT:
.                      *77,DATEPRT2:
.                      *86,PAYAMT:
..                      *117,"DIFF:",DIFF
.                      *117,"DIFF:",DIFF,HpBoff
.end patch 4.3
.end patch 3.2
         type      TRANSNUM
        GOTO       NOMOA3 IF not EQUAL
         MOVE      DOLLAR TO AMNTRECD
         MULT      "-1" BY ONAMOUNT
         MOVE      ONAMOUNT TO DETAIL$
         EDIT      DETAIL$ TO AMNTRECD
.Start Patch #2.6 - remmed and replaced lines
.         UNPACK    INVDATE INTO MM,DD,YY
.         PACK      DINVDTE FROM MM,SLASH,DD,SLASH,YY
         UNPACK    INVDATE INTO str2,YY,MM,DD
         PACK      DINVDTE FROM MM,SLASH,DD,SLASH,str2,YY
.End Patch #2.6 - remmed and replaced lines
          CLEAR     RDESC
         COMPARE   "99" TO REASON
         CALL      REASON IF EQUAL
         LOAD      RDESC FROM REASON OF REAS1,REAS2,REAS3,REAS4,REAS5:
.begin patch 3.94
.                   REAS6,REAS7,REAS8,REAS9,REAS10,REAS11,REAS12,REAS13,REAS14
                   REAS6,REAS7,REAS8,REAS9,REAS10,REAS11,REAS12,REAS13,REAS14:
                   REAS15,REAS16,REAS17,REAS18,REAS19,REAS20,REAS21,REAS22,REas23
.end patch 3.94
.begin patch 4.3
          add       Sixlpi,row
          prtpage   Laser;*p=1:row,trandte,*p=1000:row,Control,*p=1412:row,"MOA ENTRY##",*p=2118:row,Transnum:
                    *p=2647:row,CHECKNUM,*p=4529:row,DINVDTE,*p=4941:row,AMNTRECD

.         PRINT     *1,TRANDTE,*17,CONTROL,*24,"MOA ENTRY##",*36,transnum:
.                   *45,CHECKNUM,*77,DINVDTE,*FLUSH;
.         PRINT     *84,AMNTRECd
.end patch 4.3
         ADD       C1 TO PRTLINES
         GOTO      det3x
NOMOA3   CALL      PRECOMP
        CMATCH     YES TO PREPAYSW
        IF         EQUAL
.begin patch 4.3
          add       Sixlpi,row
          prtpage   Laser;*p=1:row,*BOLDON,*ULON,"ASSOCIATED MOA ENTRY NOT FOUND!!!!",*BOLDOFF,*ULOFF
.        cmatch     true to lasrflag
.        if         equal      
.         PRINT     *1,HPBON,HPITALIC,"ASSOCIATED MOA ENTRY NOT FOUND!!!!",HPBOFF,HPUPRGHT
.         else
.         PRINT     *1,p24BON,p24ITAL,"ASSOCIATED MOA ENTRY NOT FOUND!!!!",p24BOFF,P24UPRT
.         endif
.end patch 4.3
         ADD       C1 TO PRTLINES
        ENDIF
det3x    BRANCH    MOAFLAG OF DET3XA,DET3XB,det3xc
DET3XA   ADD       PAYAMT TO MLRITOT
         ADD       PAYAMT TO CHKITOT
         GOTO      DET3Xd
DET3XB   ADD       PAYAMT TO mlrMATOT
         GOTO      DET3Xd
DET3Xc   ADD       PAYAMT TO MLRITOT
         ADD       PAYAMT TO CHKITOT
         GOTO      DET3Xe
DET3Xd   ADD       APAMT     TO MLRAP
         ADD       APAMT     TO CHKAP
         ADD       AP2AMT TO TOTAP2
det3xe   ADD       C1 TO PRTLINES
         clear     transnum
         GOTO      START
.
.begin patch 4.3
NoINv     if        (row >= 10000)
          call      heading
          endif
.NOINV    COMPARE   c59 TO PRTLINES
.         CALL      HEADING IF NOT LESS
.START PATCH 3.91 REPLACED LOGIC
.         PRINT        *01,CMLR:
.                      *07,MCOMP:
.                      *36,CLR:
.                      *117,"NO INVOICE"
.begin patch 4.3
          add       Sixlpi,row
          PrtPage   laser;*p=1:row,CMLR,*p335:row,compcomp,*p=2500:row,*alignment=*Right,Clr,*Alignment=*left,*p=2750:row,"No Invoice"
.         PRINT        *01,CMLR:
.                      *07,COMPCOMP:
.                      *36,CLR:
.                      *117,"NO INVOICE"
.end patch 4.3
.END PATCH 3.91 REPLACED LOGIC
         ADD       C1 TO PRTLINES
         GOTO      START
.begin patch 4.3
Invpaid
          if        (row >= 10000)
          call      heading
          endif
.INVPAID  COMPARE   c59 TO PRTLINES
.         CALL      HEADING IF NOT LESS
.START PATCH 3.91 REPLACED LOGIC
.         PRINT        *01,CMLR:
.                      *07,MCOMP:
.                      *36,CLR:
.                      *117,"INVOICE PAID"
.begin patch 4.3
          add       Sixlpi,row
          PrtPage   laser;*p=1:row,CMLR,*p335:row,compcomp,*p=2500:row,*alignment=*Right,Clr,*Alignment=*left,*p=2750:row,"Invoice Paid"
.         PRINT        *01,CMLR:
.                      *07,COMPCOMP:
.                      *36,CLR:
.                      *117,"INVOICE PAID"
.end patch 4.3
.END PATCH 3.91 REPLACED LOGIC
         ADD       C1 TO PRTLINES
         GOTO      START
.
NEWMLR
.begin patch 3.96   
         move      no to escflag
.begin patch 4.0 turn off Intercomp
.         if        ((AP3amt <> 0 or XNincamt <> 0) & CEXTCD  <> "I")      We have an IntraCorp transaction/transfer
.so don't even check for escrow - can't happen until 2nd pass
.         Else
.end patch 4.0 turn off Intercomp
.end patch 3.96     
         pack      nescfld from holdbrk,holdmlr
         rep       zfill in nescfld
         call      nesckey
         if        not over
         move      yes to escflag
         else    
         move      no to escflag
         endif
.begin patch 4.0 turn off Intercomp
.begin patch 3.96   
.         Endif
.end patch 3.96     
.end patch 4.0 turn off Intercomp

         MOVE      CLR TO NORDFLD
         MOVE      C1    TO NORDPATH
         CALL      NORDKEY
         type      obrknum
         if        not equal
         move      "0000" to obrknum
         endif
.START PATCH 3.91 REPLACED LOGIC - TEMPORARY LOGIC
.         move      obrknum to holdbrk
              pack            COMPFLD4,obrknum
              move            "NEWMLRB-COMPKEY2",Location
              pack            KeyLocation,"Key: ",COMPFLD4
              call            COMPKEY2
              move            COMPNUM to holdbrk
.END PATCH 3.91 REPLACED LOGIC - TEMPORARY LOGIC
         MOVE      CMLR TO HOLDMLR
.START PATCH 3.91 REPLACED LOGIC
..Start Patch #2.5 - remmed and replaced line
..         PACK      MCOMP WITH B10,B10,B5
.         PACK      MCOMP WITH B10,B10,B10,B10,B5
..End Patch #2.5 - remmed and replaced line
.         PACK      MKEY WITH CMLR,Z3
.         CALL      NMLRKEY
............................
.Following is temporary
.         PACK      MCOMP WITH B10,B10,B10,B10,B5
.
         PACK      COMPCOMP WITH B10,B10,B10,B10,B5
         PACK      COMPFLD,CMLR
               move      "NEWMLR-COMPKEY",Location
         pack    KeyLocation,"Key: ",COMPFLD
         CALL      COMPKEY
.END PATCH 3.91 REPLACED LOGIC
         COMPARE   C0 TO PAGE
         RETURN    IF EQUAL
MLRTOT   call      newchk
.begin patch 4.3
.         COMPARE   c59 TO PRTLINES
.         CALL      HEADING IF NOT LESS
          if        (row >= 10000)
          call      heading
          endif
          add       Sixlpi,row
.         PRINT     *N;
         match     yes to escflag
         if        equal
          add       sixlpi,row
          prtpage   Laser;*p=1:row,"<ESCROW> <ESCROW> <ESCROW>",*p=2882:row,"CLIENT TOTAL:"
.         print    *1,"<ESCROW> <ESCROW> <ESCROW>",*49,"CLIENT TOTAL:";
         ELSE
          add       sixlpi,row
          prtpage   Laser;*p=2882:row,"CLIENT TOTAL:"
.         PRINT    *49,"CLIENT TOTAL:";
         ENDIF
          prtpage   Laser;*p=5500:row,*alignment=*right,mlrap,*p=6750:row,MLRITOT,*p=8000:row,MLRETOT,*alignment=*left
.         PRINT         *63,MLRAP:
.                      *86,MLRITOT:
.                      *109,MLRETOT;
         compare   c0 to mlrmatot
         if        equal
          add       sixlpi,row
.         print     *N
         else
         mult     seq by mlrmatot
         MOVE      mlrmatot TO DETAIL$
         move     dollar to amntrecd
         EDIT      DETAIL$ TO AMNTRECD
.         edit     mlrmatot to amntrecd
.        cmatch     true to lasrflag
.        if         equal      
          add       sixlpi,row
          Prtpage   Laser;*p=2352:row,*BOLDOn,"Client total from MOA:",*p=5353:row,amntrecd,*BOLDOFF
.         print     *N,*40,hpbon,"Client total from MOA:",*91,amntrecd:
.                   Hpboff,*n
.         else
.         print     *N,*40,p24bon,"Client total from MOA:",*91,amntrecd:
.                   p24boff,*n
.         endif
.end patch 4.3
         add       c1 to prtlines
         endif
         ADD       C3 TO PRTLINES
         cmatch    yes to eojflag
         if        not equal
.START PATCH 3.91 REPLACED LOGIC
.         PRINT     *01,CMLR:
.                   *07,MCOMP
.begin patch 4.3
          add       Sixlpi,row
          PrtPage   laser;*p=1:row,CMLR,*p335:row,compcomp
.         PRINT     *01,CMLR:
.                   *07,COMPCOMP
.end patch 4.3
.END PATCH 3.91 REPLACED LOGIC
         add       c1 to prtlines
         endif
        ADD       MLRITOT TO GRDITOT
         ADD       MLRmaTOT TO GRDmaTOT
         ADD       MLRETOT TO GRDETOT
.begin patch 4.0 turn off Intercomp
.begin patch 3.95             
.         if        ((AP3amt <> 0 or XNincamt <> 0) & CEXTCD  <> "I")      We have an IntraCorp transaction/transfer
.         Else
         ADD       MLRAP   TO TotAP1
.                   endif
.end patch 3.95               
.end patch 4.0 turn off Intercomp

         MOVE      C0 TO MLRITOT
         MOVE      C0 TO MLRETOT
         MOVE      C0 TO MLRmaTOT
         MOVE      C0 TO MLRAP
         move      c0 to chketot       *added 9/29/93.
         MOVE      NO TO ESCFLAG
         RETURN
.
NEWCHK   MOVE      HOLDCHK TO PRTCHK
         MOVE      NCSHCHK TO HOLDCHK
         COMPARE   C0 TO PAGE 
         RETURN    IF EQUAL

.
CHKTOT   CMATCH    B1 TO HOLDCHK
         IF        EOS
         MOVE      C0 TO CHKITOT
         MOVE      C0 TO CHKETOT
         MOVE      C0 TO CHKAP
         RETURN
         ENDIF
         compare   c0 to chkitot
         return    if equal
.begin patch 4.3
.         COMPARE   c59 TO PRTLINES
.         CALL      HEADING IF NOT LESS
          if        (row >= 10000)
          call      heading
          endif
          add       sixlpi,row
          add       sixlpi,row
          prtpage   Laser;*p2352:row,"CHECK TOTAL ## ",prtchk,*p=5500:row,*alignment=*right,chkap,*p=6750:row,chkITOT,*p=8000:row,chkETOT,*alignment=*left
          add       sixlpi,row

.         PRINT     *N,*39,"CHECK TOTAL ## ",PRTCHK,*63,CHKAP:
.                   *86,CHKITOT:
.                   *109,CHKETOT:
.                   *N
.end patch 4.3
         ADD       C3 TO PRTLINES
         MOVE      C0 TO CHKITOT
         MOVE      C0 TO CHKETOT
         MOVE      C0 TO CHKAP
         RETURN    
.
EOJ      CALL     CHKTOT                        .DLH 09/21/94
         move     yes to eojflag
         CALL     MLRTOT
         MOVE     GRDITOT TO GRDDIFF
         ADD      GRDETOT TO GRDDIFF
         MOVE      TotAP1 TO FINAP                       .ap1
         ADD       TOTAP2 TO FINAP                        .ap2                                                                                        
.begin patch 3.93         
.begin patch 3.95             
.         ADD       TOTAP3 TO FINAP                         .ap3
.end patch 3.95               
.end patch 3.93         
         MOVE     GRDITOT TO GRDMLR
.;Patch3.7
         MOVE      GRDITOT TO CROSSCHKAR
         ADD       CODEPAR TO CROSSCHKAR
.;subpatch3.7
         ADD       PREAMT TO GRDMLR
         mult     seq by grdmatot
.begin patch 4.3
.         COMPARE   c59 TO PRTLINES
.         CALL      HEADING IF NOT LESS
          if        (row >= 9500)
          call      heading
          endif
.end patch 4.3

.begin patch 3.93         
         MOVE      MASK92b,MT$Trans
         EDIT      TOTTrans,MT$Trans
         MOVE      MASK92b,MT$Xninc
         EDIT      TOTXninc,MT$Xninc
         MOVE      MASK92b,MT$ap1
         EDIT      Holdap1,MT$ap1
         MOVE      MASK92b,MT$AP3
         EDIT      TotAP3,MT$AP3
         
.end patch 3.93         
.begin patch 4.3
          add       sixlpi,row
          add       sixlpi,row
          prtpage   Laser;*p2352:row,*alignment=*right,"Totals MOA ",*p=6750:row,grdmatot
          add       sixlpi,row
          prtpage   Laser;*p2352:row,*Boldon,"Deposit Total ",*alignment=*right,*p=8000:row,*ULON,grddiff,*alignment=*left,*BOLDOFF,*ULOFF
          add       sixlpi,row
          prtpage   Laser;*p2352:row,"Totals AP1 ",*p=5500:row,*alignment=*right,TotAP1,*p=6750:row,grdITOT,*p=8000:row,grdETOT
                    if        (row >= 10000)
                    call      heading
                    endif

          add       sixlpi,row
          prtpage   Laser;*p325:row,"Records Processed ",n6,*p2352:row,*alignment=*right,"Totals AP2 ",*p=5500:row,*alignment=*right,TotAP2,*p=6750:row,PREAMT,*alignment=*left
          add       sixlpi,row
          prtpage   Laser;*p2352:row,*alignment=*right,"Totals AP3 ",*p=5500:row,*alignment=*right,TotAP3,*alignment=*left,*p=7125:row,*alignment=*left
                    
.         PRINT     *N,*48,"Totals MOA :",*86,grdmatot:
.                   *n,*48,"TOTALS  AP1:":
.                      *63,TotAP1:
.                      *86,GRDITOT:
.                      *104,"+":
.                      *109,GRDETOT:
.                      *121,"=":
.                      *122,GRDDIFF:
.                   *L,*05,"Records Processed ",n6:
.                   *56,"AP2:",*63,TOTAP2,*86,PREAMT:
.end patch 4.3
.begin patch 3.93         
.                   *L,*56,"AP3:",*63,TOTAP3,*124,"---------":
.begin patch 3.95             
.                   *L,*56,"AP3:",*63,TOTAP3,*124,"---------";
          MOVe      mask92b,MT$AP3
          edit      SubTAp3Trans,MT$AP3
.begin patch 4.3
          add       Sixlpi,Row
          PRtpage   Laser;*p=1:row,*boldon,"TOTAL InterComp Transfer's ",*boldoff,*alignment=*right,*p=5500:row,MT$Trans,*alignment=*Left
                    if        (row >= 10000)
                    call      heading
                    endif
          add       Sixlpi,Row
                    if        (row >= 10000)
                    call      heading
                    endif
          PRtpage   Laser;*p=2352:row,*alignment=*right," = ",*p=5500:row,MT$ap3:
                    *alignment=*Left," + ",*p=6750:row,*alignment=*Right,MT$xninc,*alignment=*Left," + ",*p=8000:row:
                    *alignment=*Right,MT$ap1,*BOLDOFF,*alignment=*Left
.                    Print     *L,*1,Hpbon,"TOTAL InterComp Transfer's ",*63,MT$Trans:
.                             *L,*1,Hpbon,"TOTAL InterComp Transfer's ",*63,MT$Trans:
.end patch 3.95               
.                    *N,*63," = ",MT$ap3," + ",MT$xninc," + ",MT$ap1,HPBoff;
.end patch 4.3
                              
.end patch 3.93         
.begin patch 4.3
          add       Sixlpi,Row
          PRtpage   Laser;*p=2352:row,*Alignment=*right,"TOTAL    AP 1 & 2:",*p=5500:row,FINAP,*p=6750:row,GRDMLR,*alignment=*Left:
                    *p=6800:row," - GRAND TOTAL"

.          PRint     *L,*48,"TOTAL    AP 1 & 2:",*63,FINAP,*86,GRDMLR," - GRAND TOTAL"
.end patch 4.3
.         Calc      TotCheck=(finap-totap3+TotTrans)
.Dh test 12/13/07
.         Calc      TotCheck=(TotAP1+totap2+totap3+TotTrans)
          Calc      TotCheck=(TotAP1+totap2+TotTrans)
          MOVE                MASK92b,MT$Checks
          edit      Totcheck,MT$Checks
.begin patch 4.3
          add       Sixlpi,Row
          PRtpage   Laser;*p=2352:row,*Alignment=*right,"G Total Payables:",*p=5500:row,MT$Checks
.          Print     *48,"G Total Payables:",*63,MT$Checks
.end patch 4.3
.begin  patch 3.2
               If             (DiffFlag = "Y")
.begin patch 4.3
.               COMPARE        c59 TO PRTLINES
.               CALL           HEADING IF NOT LESS
                    if        (row >= 10000)
                    call      heading
                    endif
.end patch 4.3
.begin patch 4.3
          add       Sixlpi,Row
          PRtpage   Laser;*p=1:row,*BoldON,"*****THERE ARE ERROR(S) in this control!!!!!"
          add       Sixlpi,Row
          PRtpage   Laser;*p=1:row,*BoldON,"*****THERE ARE ERROR(S) in this control!!!!!"
          add       Sixlpi,Row
          PRtpage   Laser;*p=1:row,*BoldON,"*****THERE ARE ERROR(S) in this control!!!!!",*BOLDOFF
.               print          *l,*1,hpbon,"*****THERE ARE ERROR(S) in this control!!!!!":
.                              *l,*1,"**********THERE ARE ERROR(S) in this control!!!!!":
.                              *l,*1,"***************THERE ARE ERROR(S) in this control!!!!!",hpBoff
.end patch 4.3
               endif
.end  patch 3.2
.begin  patch 3.3
               If             (NegAPFlagEOJ = "Y")
.begin patch 4.3
.               COMPARE        c59 TO PRTLINES
.               CALL           HEADING IF NOT LESS
                    if        (row >= 10000)
                    call      heading
                    endif
.end patch 4.3
.begin patch 4.3
          add       Sixlpi,Row
          PRtpage   Laser;*p=1:row,*BoldON,"*****THERE IS NEGATIVE A/P(S) in this control!!!!!"
          add       Sixlpi,Row
          PRtpage   Laser;*p=1:row,*BoldON,"*****THERE IS NEGATIVE A/P(S) in this control!!!!!"
          add       Sixlpi,Row
          PRtpage   Laser;*p=1:row,*BoldON,"*****THERE IS NEGATIVE A/P(S) in this control!!!!!",*BOLDOFF
.               print          *l,*1,hpbon,"*****THERE IS NEGATIVE A/P(S) in this control!!!!!":
.                              *l,*1,"*****THERE IS NEGATIVE A/P(S) in this control!!!!!":
.                              *l,*1,"*****THERE IS NEGATIVE A/P(S) in this control!!!!!",hpBoff
.end patch 4.3
               endif
.end  patch 3.3
.begin  patch 3.3
               If             (NegARFlagEOJ = "Y")
.begin patch 4.3
.               COMPARE        c59 TO PRTLINES
.               CALL           HEADING IF NOT LESS
                    if        (row >= 10000)
                    call      heading
                    endif
.end patch 4.3
.begin patch 4.3
          add       Sixlpi,Row
          PRtpage   Laser;*p=1:row,*BoldON,"*****THERE IS NEGATIVE A/R(S) in this control!!!!!"
          add       Sixlpi,Row
          PRtpage   Laser;*p=1:row,*BoldON,"*****THERE IS NEGATIVE A/R(S) in this control!!!!!"
          add       Sixlpi,Row
          PRtpage   Laser;*p=1:row,*BoldON,"*****THERE IS NEGATIVE A/R(S) in this control!!!!!",*BOLDOFF
.               print          *l,*1,hpbon,"*****THERE IS NEGATIVE A/R(S) in this control!!!!!":
.                              *l,*1,"*****THERE IS NEGATIVE A/R(S) in this control!!!!!":
.                              *l,*1,"*****THERE IS NEGATIVE A/R(S) in this control!!!!!",hpBoff
.end patch 4.3
               endif
.end  patch 3.3
.Patch3.6
.               if ((NegARFlagEOJ = "Y")|(NegAPFlagEOJ = "Y")|(DiffFlag = "Y"))
               if ((NegARFlagEOJ = "Y")|(DiffFlag = "Y"))
               else
.START PATCH 3.5 REPLACED LOGIC
.                     move    CNUM,NCTRFLD
                            pack    NCTRFLD,HCNUM,HCNUMDATE
.END PATCH 3.5 REPLACED LOGIC
                      rep     zfill,NCTRFLD
                      move    "START-NCTRKEY",Location
                      pack    KeyLocation,"Key: ",NCTRFLD
                      call    NCTRKEY
                      if not over 
.Patch3.7
                              move CROSSCHKAR to NCTRAMT2
.                             move GRDMLR to NCTRAMT2
.                             move GRDItot to NCTRAMT2
.endpatch3.7
          call    NCTRUPD
                            endif  
               endif

.EndPatch3.6

.begin patch 4.3
.         BRANCH    PRTFLAG TO END
.         cmatch    true to lasrflag
.         if        not equal
.         print     *f
.         endif
.         SPLCLOSE
.         CALL      REMVTOF
          prtclose  Laser
.begin patch 4.4
.          call      GetPDfPAth
.end patch 4.4
.begin  patch 4.5
           PRTPLAY Spoolfle,"pdf:",PDFNAME=str55
           PRTPLAY Spoolfle,"\\nins2\laser2"
.end  patch 4.5

          pack      MailAttach from "c:\work\pdf\",prtname,".pdf"  
          move      Mailattach,Str55
          Move      MailAttach,Str45
CheckFile
          trap      WaitForEnd giving error if IO
          open      FileCheck,STR55,Exclusive     
          Close     FIleCHeck



          move    "Here is your PDF File",MailSubjct
          Clear     MailBody
          append    "CASH  Report",Mailbody
          append    CRLF,Mailbody
          reset     Mailbody
          pack      Mailto from user,"@nincal.com"
          pack      MailFrom from user,"@nincal.com"
          call      SendMail
.Clean up afterwards
          pack      str55,"\\nins1\e\data\",prtname,".pdf"
          pause c7
          copyfile  mailattach,str55
          pause c7
          erase     mailattach
.begin patch 4.4
.          Call      PDF995Auto0
.end patch 4.4
          
.end patch 4.3
.begin  patch 4.5
          erase     spoolfle
.end  patch 4.5
         shutdown  "cls"
         GOTO      END
+........................................................................
.
HEADING  ADD       C1 TO PAGE
         compare    c1 to page
         if         equal
.begin patch 4.3
          PRTPAGE   Laser;*UNITS=*HIENGLISH:
                    *ORIENT=*PORTRAIT,*Font=Font8:
                    *DUPLEX=2

.         cmatch     true to lasrflag
.         if         equal
.         print      hp17ptch,HPTOP,hpdupl,*f
.         else
.         print      p2417cpi
.         endif
.end patch 4.3
         endif
.START PATCH 3.9 REPLACED LOGIC
.         PRINT     *F,*n,*N,"CONFIDENTIAL":
.                      *55,"NAMES IN THE NEWS, INC.":
.                      *119,"DATE: ",SYSDATE:
.                   *N,*55,"CASH RECEIPTS REGISTER":
.                      *119,"PAGE:    ",PAGE:
.                   *N:
.                   *N,*62,"CONTROL ",cntnumb:
.                   *N:
.                   *N,*54,"---[ LIST OWNER ]---":
.                      *77,"----[ INTERNAL ]----":
.                      *100,"----[ EXTERNAL ]----":
.                   *N,*55,"INVOICE":
.                      *67,"INVOICE":
.                      *78,"PAYMENT":
.                      *90,"PAYMENT":
.                      *101,"PAYMENT":
.                      *113,"PAYMENT":
.                   *N,*01,"MLR##":
.                      *07,"CLIENT":
.                      *39,"LR##":
.                      *47,"INV##":
.                      *58,"DATE":
.                      *68,"AMOUNT":
.                      *81,"DATE":
.                      *91,"AMOUNT":
.                      *104,"DATE":
.                      *114,"AMOUNT":
.                   *N,*01,"----":
.                      *07,"-------------------------":
.                      *36,"------":
.                      *45,"------":
.                      *54,"--------":
.                      *64,"----------":
.                      *77,"--------":
.                      *87,"----------":
.                      *100,"--------":
.                      *110,"----------":
.                      *122,"-"
.begin patch 4.3
          Move      "250",row
          if        (Page > 1)
          prtpage   Laser;*newpage
          endif
          prtpage   Laser;*p1:row,"CONFIDENTIAL",*p4250:row,*alignment=*Center,CmpPrtNme,*alignment=*Left,*p=7325:row,"DATE:":
                    *alignment=*Right,*p=8125:row,sysdate,*alignment=*Left
          add       Sixlpi,row
          prtpage   Laser;*p4250:row,*alignment=*Center,"CASH RECEIPTS REGISTER",*alignment=*Left,*p=7325:row,"PAGE:":
                    *alignment=*Right,*p=8125:row,PAGE,*alignment=*Left
          add       Sixlpi,row
          clear     str12
          pack      str12 from "CONTROL ",cntnumb
          prtpage   Laser;*p4250:row,*alignment=*Center,Str12,*alignment=*Left
          add       Sixlpi,row
          add       Sixlpi,row
          prtpage   Laser;*p5500:row,*alignment=*Right,"----[ LIST OWNER ]----"
          prtpage   Laser;*p6750:row,*alignment=*Right,"------[ INTERNAL ]------"
          prtpage   Laser;*p8000:row,*alignment=*Right,"------[ EXTERNAL ]------"
          add       Sixlpi,row
          prtpage   Laser;*p4875:row,*alignment=*Right,"INVOICE"
          prtpage   Laser;*p5500:row,*alignment=*Right,"INVOICE"
          prtpage   Laser;*p6125:row,*alignment=*Right,"PAYMENT"
          prtpage   Laser;*p6750:row,*alignment=*Right,"PAYMENT"
          prtpage   Laser;*p7375:row,*alignment=*Right,"PAYMENT"
          prtpage   Laser;*p8000:row,*alignment=*Right,"PAYMENT",*alignment=*Left
          add       Sixlpi,row
          prtpage   Laser;*p1:row,"MLR##",*p=375:row,"CLIENT",*p=2500:row,"LR##",*p=3500:row,"INV##":
                    *p4875:row,*alignment=*Right,"DATE":
                    *p5500:row,*alignment=*Right,"AMOUNT":
                    *p6125:row,*alignment=*Right,"DATE":
                    *p6750:row,*alignment=*Right,"AMOUNT":
                    *p7375:row,*alignment=*Right,"DATE":
                    *p8000:row,*alignment=*Right,"AMOUNT",*alignment=*Left
          add       Sixlpi,row
          add       Sixlpi,row
.          PRINT     *F,*n,*N,"CONFIDENTIAL":
..begin patch 3.93
..                      *55,"NAMES IN THE NEWS":
.                      *55,CmpPrtNme:
..end patch 3.93
.                      *119,"DATE: ",SYSDATE:
.                   *N,*55,"CASH RECEIPTS REGISTER":
.                      *119,"PAGE:    ",PAGE:
.                   *N:
.                   *N,*62,"CONTROL ",cntnumb:
.                   *N:
.                   *N,*54,"---[ LIST OWNER ]---":
.                      *77,"----[ INTERNAL ]----":
.                      *100,"----[ EXTERNAL ]----":
.                   *N,*55,"INVOICE":
.                      *67,"INVOICE":
.                      *78,"PAYMENT":
.                      *90,"PAYMENT":
.                      *101,"PAYMENT":
.                      *113,"PAYMENT":
.                   *N,*01,"MLR##":
.                      *07,"CLIENT":
.                      *39,"LR##":
.                      *47,"INV##":
.                      *58,"DATE":
.                      *68,"AMOUNT":
.                      *81,"DATE":
.                      *91,"AMOUNT":
.                      *104,"DATE":
.                      *114,"AMOUNT":
.                   *N,*01,"----":
.                      *07,"-------------------------":
.                      *36,"------":
.                      *45,"------":
.                      *54,"--------":
.                      *64,"----------":
.                      *77,"--------":
.                      *87,"----------":
.                      *100,"--------":
.                      *110,"----------":
.                      *122,"-"
          if        (Page > 1)
          add       Sixlpi,Row
          PrtPage   laser;*p=1:row,CMLR,*p335:row,compcomp
          endif
.end patch 4.3
.END PATCH 3.9 REPLACED LOGIC
         MOVE      C11 TO PRTLINES
         compare    c1 to n6
         if         equal
.START PATCH 3.91 REPLACED LOGIC
.         PRINT        *01,CMLR:
.                      *07,MCOMP
.begin patch 4.3
          PrtPage   laser;*p=1:row,CMLR,*p335:row,compcomp
.         PRINT        *01,CMLR:
.                      *07,COMPCOMP
.end patch 4.3
.END PATCH 3.91 REPLACED LOGIC
         add       c1 to prtlines
         endif
         RETURN
REASON   MOVE      "ENTRY CORRECTION" TO RDESC
         RETURN
PRECOMP clear      prepaysw
          match      b6 to clr
         return     if equal
         PACK     MKEY FROM MLRN,COBN
         CALL     NMLRKEY
         MOVE     OLON TO NOWNFLD
         CALL     NOWNKEY
.begin patch 2.8
         move      c1 to ndatpath
         move      olnum to ndatfld
         call      ndatkey
         call      wipecvars
.end patch 2.8
         MOVE      clr to nmrgfld
         REP       ZFILL IN NMRGFLD
         move      c0 to nmrgrqty
         move      c0 to nmrgiqty
         move      c0 to nmrgnet
.begin patch 2.9
         move      no to mrgsw
         move      no to shipsw
.end patch 2.9

         CALL      NMRGKEY
.begin patch 2.9
         if       not over
         move     yes to mrgsw
         endif
         MOVE      clr to nshpfld
         REP       ZFILL IN NshpFLD
         CALL      NshpKEY
         if       not over
         move     yes to shipsw
         endif
.end patch 2.9
               call           Ninvacdrecclear
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
         CALL     COMPUTE
         RETURN

+........................................................................
.begin patch 4.3
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    move      "3000",str4
                    call      waitin using str4
.                    pause     "30"
                    noreturn
                   if        (trapcount > 60)   . 5 min are you kidding me. clearly not waiting 5 min
                    Pack       MailSubjct,"CASH pdf - ",mailattach
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
.end patch 4.3
.
         INCLUDE   NCSHIO.inc
.;begin patch 3.92
.;         INCLUDE   NINVIO.inc
.;         INCLUDE   COMPUTE.inc
              INCLUDE         ninvio.inc
              Include         NInvAcdio.inc
              INCLUDE         compute.inc
.;end patch 3.92
+
         INCLUDE   NADJIO.inc
.
         INCLUDE   NADJCLIO.inc
+
         INCLUDE   NORDIO.inc
+
.START PATCH 3.8 REPLACED LOGIC
.         INCLUDE   NMLRio.inc
              INCLUDE         COMPio.inc
              INCLUDE         CNTio.inc
.END PATCH 3.8 REPLACED LOGIC
+
.begin patch 2.8
         include   nacdio.inc
         include   ndatio.inc
         include   nshpio.inc
.end patch 2.8
         INCLUDE   NMOAIO.INC
         INCLUDE   NDAT3IO.INC
         INCLUDE   NOWNIO.INC
         INCLUDE   NESCIO.INC
         include   nmrgio.inc
.START PATCH 3.4 ADDED LOGIC
              INCLUDE         NCTRIO.INC
.END PATCH 3.4 ADDED LOGIC
 
        INCLUDE   COMLOGIC.inc
