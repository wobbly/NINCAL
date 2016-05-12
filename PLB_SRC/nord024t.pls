..
.. PURPOSE - READS INPUT FILE (NINORD) AND SPOOLS
..           RECORDS FOR TRANSMISSION TO target analsys/PIDI/BlackBaud xML FORMAT.
..
PC       EQU       0
         INCLUDE    COMMON.inc
         INCLUDE   CONS.inc
                                        include   compdd.inc
                                        include   cntdd.inc
         INCLUDE   NCNTDD.inc
         INCLUDE   NORDDD.inc
         INCLUDE   NRTNDD.inc
         INCLUDE   NOWNDD.inc
         INCLUDE   NCRCDD.inc
         INCLUDE   NSPIDD.inc
         INCLUDE   MEDIA.inc
         INCLUDE   SHIPPING.inc
         INCLUDE   NOFRDD.INC
         INCLUDE   NSPEDD.INC
         INCLUDE   HP.INC
        INCLUDE WINAPI.INC
          INCLUDE   NSEL2DD.INC
          INCLUDE   NSEL3DD.INC
          INCLUDE   NADDDD.INC
          INCLUDE   NSLTDD.INC
          INCLUDE   NREFDD.INC
          INCLUDE   NMODDD.INC
..
release  init      "3.11"         DLH added OMlrLstCd
Reldate   Init      "2016 March 16"
.Release   INit      "3.10"    DLH change mailer PO to our LR for brokerage orders
.Reldate   Init      "2013 September 30"
.Release   INit      "3.01"    DLH add Mailer PO
.Reldate   Init      "2013 September 12"
.Release   INit      "3.00"    DLH Breakout MSF HIGH DOllar into seperate xml, see archives for older code versions
.Reldate   Init      "2013 August 8"
.Release          Init               "2.79"               DLH
.reldate   Init      "2013 July 10"                          more code for infogroup email
.Release          Init               "2.78"               DMB            12OCT2006  Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.Release         Init               "2.77"               DMB            15SEP2006  Changed directory where xml files are created.
.Release         Init               "2.76"               DMS            22JUN2006  FULFILLMENT CONVERSION
.release  init      "2.74"        ASH   10JUL06  Adjusted logic for determining exchanges
.Release        Init           "2.73"           JD             08jul2003  write continuation lr info occode = 1
.
. OTHER  VARIABLES.
. ....................
DimPtr    DIM       ^
SPCL     DIM       2               *SPECIAL INSTRUCTION KEY
SPCL1    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL2    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL3    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL4    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL5    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL6    DIM       2           *SPECIAL INSTRUCTION CODE
.
REVTXT   INIT      "Revised: "
CANTXT   INIT      "**CANCELLED** : "
BILDTXT  INIT      "**Billed Order**"
REVDATA  DIM       30
BILDDATA DIM       16
.
. FILES.
. ......
targORD   FILE               .TARG ORDER INPUT FILE.
BADORD   FILE
NAMFILE  FILE      FIX=185      .Added 2 bytes to file
WEEKFILE FILE      FIX=146      .Added 4 bytes to file
SAVEFILE IFILE
.
tdmcstat dim       1
THREE    FORM      "3"
FOUR     FORM      "4"
FIVE     FORM      "5"
SIX      FORM      "6"
SEVEN    FORM      "7"
EIGHT    FORM      "8"
ANS      DIM       1
FILE     FORM      2         BRANCHING CONSTANT FOR I/O TRAPS
DATE     DIM       8         'MM/DD/YY'.
DAY      DIM       2
OFFEROUT DIM       11       FOR OUTPUT OF OFFER, SUPPRESSED IF NO OFFER SELCTD.
COUNT    FORM      5
SPCOUNT  FORM      5
ORIG099  DIM       1
ANY099   DIM       1
WORK06   DIM       6
WORK47   DIM       47
WK247    DIM       47
Nfield52 form      5.2
InstructionCounter            form           2
str100         Dim            100
NFIELD23 FORM      3.2                  (NUMERIC WORK FIELD)
NFIELD4  FORM      4
MLRKEY   DIM       7
V1       FORM      2
MAIL1    DIM       25                       INFOR-
MAIL2    DIM       25                       MATION
MAIL3    DIM       25                       READ
EXCHANGE DIM       15         *USED FOR ORDER PRINT
TEST     DIM       15         *USED FOR ORDER PRINT
F3       DIM       3         *USED FOR ORDER PRINT
F2       DIM       2         *USED FOR ORDER PRINT
ENTIRE   DIM       1        *USED FOR ORDER PRINT
SAMPLE   DIM       26       *USED FOR ORDER PRINT
SPCL7    DIM       2         *ORDER FILLER
SPCL8    DIM       2         *
SPCL9    DIM       2         *
CORTN    DIM       3         *USED FOR ORDER PRINT
CONT     DIM       23        *USED FOR ORDER PRINT
CONT1    DIM       20        *USED FOR ORDER PRINT
.
CONTDTE  DIM       10        *USED FOR ORDER PRINT
CONTQTY  DIM       11        *USED FOR ORDER PRINT
QTYMSK   INIT      "ZZZ,ZZ9,999"    *USED FOR ORDER PRINT
QTYMSK2  INIT      "Z,ZZ9,999"    *USED FOR ORDER PRINT
QTYOUT   DIM       11        *USED FOR ORDER PRINT
QTYNUM   FORM      9         *USED FOR ORDER PRINT, QTY FORMATING.
QTYNUM2  FORM      7         *USED FOR ORDER PRINT, QTY FORMATING.
qtyout2  dim       9         * just for tdmc lolfile
MEDMEMO  DIM       25        *USED FOR ORDER PRINT, ON MAG TAPE.
COMSLCT  DIM       25        *USED FOR ORDER PRINT, COMSELECT ORDERS.
REPRT    DIM       15        *USED FOR ORDER PRINT, REPRINTED ORDERS.
.                            *AND CANCELLED ORDERS, REPRINT IMPLIED.
PRTPO    DIM       7         *USED FOR ORDER LABEL PRINT
LBOFR    DIM       25        *USED FOR ORDER LABEL PRINT.
LROUT    DIM       6         *USED FOR ORDER & LABEL PRINT.
LRMASK   INIT      "ZZZZZ9"
LRNUM    FORM      6
COUNTR   FORM      4
BUFFER   FORM      "4"
RTNSTRNG DIM       6
RTNCHEK  FORM      4
PRICECK  DIM       5
DESC     DIM       12
DESC2     DIM      3
TDMCSW   INIT      "N"
MEDTYPE  DIM        1
save     dim        47
prtlines form       2
c33      form       "33"
c66      form       "66"
lastlr   dim        6
lastlabl dim        6
salenumb dim        2
fullCNT      DIM       34
cnt          dim       20
intrnet  dim        46                .print contact's internet address
BEGIN    FORM      2
LAST     FORM      2
loltype  dim       1
loldes   form      1
lolcodes init       "DXLRQ"
dim45b   dim       45
compm    dim       25
holdstr  dim      758
line     dim      55
line1    dim      55
line2    dim      55
line3    dim      55
line4    dim      55
line5    dim      55
line6    dim      55
line7    dim      55
line8    dim      55
line9    dim      55
line10   dim      55
line11   dim      55
line12   dim      55
line13   dim      55
line14   dim      55
line15   dim      55
CARR     INIT     0x7f
carrfill dim      2
font2   font
columnA form    9
columnB form    9
columnC form    9
row1    form    9
row2    form    9
XMLFile        File
XMLFileName        Dim             255
XMLRealFileName    Dim             255
PackData DataList
startfp   form      9
endfp     form      9
leftparan init      "("
str100a   Dim       100

Taskname1 dim 255
.begin patch 3.00
pass      form      1                   1st pass regular file, second pass high dollar list # 074891
.end patch 3.00


. .............................................................................
* PROGRAM MAIN.
* *************
.
. OPEN FILES.
. ...........
         MOVE      "NORD024t" TO PROGRAM
         MOVE      "Names in the News" TO COMPNME
         MOVE      "Target SPOOL PROGRAM" TO STITLE
         CALL       PAINT
.begin patch 3.00
          move      c1,pass
.end patch 3.00
         create    font2,"Arial",size=9
         MOVE      C0,HowMany
.
         MOVE      "750",column1
         MOVE      "1950",column2
         MOVE      "1550",column3
         MOVE      "4500",column4
         MOVE      "5700",column5
         MOVE      "5300",column6
         MOVE      "650",row1
         MOVE      "650",row2
          call      GetWinVer
         TRAP      IO IF IO
.begin patch 3.0
Pass2
.end patch 3.0
         DISPLAY   *P1:24,"OPENING FILES"
         MOVE      C1 TO FILE
         IFNZ      PC
         OPEN      targORD,"\\nins1\e\data\nprint.ful",EXCLUSIVE
         XIF
         IFZ      PC
         OPEN      targORD,"\\nins1\e\data\nprint.ful",EXCLUSIVE
         ADD       C1 TO FILE
.Create work var
          create    PackData=1:1:1:1
         ADD       C1 TO FILE
CLOCK    CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO MM,DD,YY
         PACK      DATE FROM MM,SLASH,DD,SLASH,YY
         XIF
         IFZ       PC
         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         XIF
          move      DATE,str6
. ....................................................
. DISS - DISPLAY PROGRAM NAME.
. ............................
DISS     MOVE      "NORD024T" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "Create TaG/pidi xml file" TO STITLE
         CALL       PAINT
         MOVE       "ABORT" TO PF5
         CALL       FUNCDISP
         TRAP       ABORT IF F5
.
+ *****************************************************************************
. ORD - ORDER SECTION.
. ....................
ORD
READO    MOVE      SEVEN TO FILE
         filepi    1;targord
         READ      targORD,SEQ;ordvars
         GOTO      EOJ IF OVER
         ADD       C1 TO COUNTR
         DISPLAY   *P10:12,*EL,"COUNT READ ",COUNTR,b1,olrn
         CMATCH    "x" TO OSTAT       Cancelled Pending order ?
         GOTO      reado IF EQUAL     YES, Do not write, leave in file.
         reset     cancodes
         cmatch    "z" to ostat      .cancelled LCR skip
         goto      reado if equal
         scan      ostat in cancodes
         goto      reado  if equal
         cmatch    "R" in ostat
         goto      reado if equal
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
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
                    move      "/M",NMODDESC
          else
                    pack      NMODFLD,NSEL2DESC
                    rep       zfill,NMODFLD
                    move      "NMODKEY",Location
                    pack      KeyLocation,"Key: ",NMODFLD
                    call      NMODKEY
                    if over
                              move      "/M",NMODDESC
                    endif
          endif
.END PATCH 2.74A ADDED LOGIC
         pack      salenumb from osales10,osales
         CMATCH    " " TO OLRN
         GOTO      READO IF EOS
         GOTO      READO IF EQUAL
         MOVE      C0 TO QTYNUM
         MOVE      C0 TO QTYNUM2
         MOVE      OQTY TO QTYNUM
         MOVE      OQTY TO QTYNUM2
         COMPARE   C0 TO QTYNUM
         GOTO      READO IF EQUAL
         match     "0001",ortnnum
         goto      reado if equal
         PACK      MKEY FROM OMLRNUM,OCOBN
         CALL      NMLRKEY
         PACK      QTYOUT FROM QTYMSK
         EDIT      QTYNUM TO QTYOUT
         PACK      QTYOUT2 FROM QTYMSK2
         EDIT      QTYNUM2 TO QTYOUT2
         ADD       C1 TO COUNT
         move      OLRN,NSPEFLD
         move      "READO-NSPEKEY",Location
         call      NSPEKEY
.
. PRINT VARIABLES FROM ACCESSED RECORD
.
.
         MOVE      " " TO EXCHANGE
         MOVE      " " TO ENTIRE
         MOVE      " " TO TEST
         MOVE      " " TO CONT
         MOVE      " " TO CONT1
         MOVE      " " TO REPRT
         PACK      MKEY FROM OMLRNUM,OCOBN
         CALL      NMLRKEY
         CALL      NOMLR IF OVER
         MOVE      OLON TO NOWNFLD
         MOVE      "10" TO FILE
         REP       " 0" IN NOWNFLD
         CALL      NOWNKEY

checkful
     call Trim using OFULLFIL
          if (OFULLFIL <> "")
                    pack      COMPFLD,OFULLFIL
                    call      zfillit using COMPFLD
                    move      C1,COMPPATH
                    move      "READO-COMPKEY",Location
                    pack      KeyLocation,COMPFLD
                    call      COMPKEY
                    if over
                              clear     COMPFLD
                              clear     COMPCOMP
                    else
                                        match "009411" to COMPFLD
                                        goto  good1 if equal
                    endif

          else
                    clear     COMPFLD
                    clear     COMPCOMP
          endif
      goto  reado
good1
.begin patch 3.00
          if        (pass = c1 & olnum = "074891")
          goto      reado                                   .its High Dollar & pass 1 is regular file
          elseif   (pass = c2 & olnum <> "074891")
          goto      reado                                   .its regular file & pass 2 is High Dollar
          endif
.end patch 3.00
         CLEAR     REVDATA
         MOVE      OLRN TO NCRCFLD
         CALL      NCRCKEY
         IF        NOT OVER
NCRCLOOP CMATCH    "C" TO NCRCCODE
         IF        EQUAL
         PACK      REVDATA FROM CANTXT,NCRCMM,SLASH,NCRCDD,SLASH,ncrccc,NCRCYY
         ELSE
         PACK      REVDATA FROM REVTXT,NCRCMM,SLASH,NCRCDD,SLASH,ncrccc,NCRCYY
         ENDIF
         CALL      NCRCKS
         GOTO      NCRCEXIT IF OVER
         MATCH     NCRCFLD TO NCRCKEY
         GOTO      NCRCEXIT IF NOT EQUAL
         GOTO      NCRCLOOP
         ENDIF
NCRCEXIT 
.begin patch 3.0
.          MOVE      "0" TO NFIELD4
.         MOVE      ORTNNUM TO NFIELD4
.         BRANCH    NFIELD4 OF NOTDMC
.end patch 3.0
         ADD       C1 TO SPCOUNT
        DISPLAY   *P14:20,*EL,"Target ORDERS SPOOLED : ",SPCOUNT
         compare   c1 to spcount
         if        not equal
         call      firstord
         endif
         move      c0 to prtlines
         move      olrn to lastlr
         CMATCH    " " TO OFOCODE
         GOTO      MEDIAEX IF EQUAL          *NO MEDIA SELECT
         GOTO      MEDIAEX IF EOS            * NO MEDIA SELECT
         MOVE      C0 TO NFIELD23
         TYPE      OFOCODE
         MOVE      OFOCODE TO SAVE         *SAVE VARIABLE
         GOTO      MED10 IF NOT EQUAL
         MOVE      OFOCODE TO NFIELD23
         GOTO      DIS27
MED10    REP       "A0B1C2D3E4F5G6H7I8J9" IN OFOCODE
         TYPE      OFOCODE
         GOTO      MED20 IF NOT EQUAL
         MOVE      OFOCODE TO NFIELD23
         ADD       C10 TO NFIELD23
         GOTO      DIS27
MED20    REP       "K0L1M2N3O4P5Q6R7S8T9" IN OFOCODE
         TYPE      OFOCODE
         GOTO      MED30 IF NOT EQUAL
         MOVE      OFOCODE TO NFIELD23
         ADD       "20" TO NFIELD23
         GOTO      DIS27
MED30    REP       "U0V1X2Y3Z4" IN OFOCODE
         MOVE      OFOCODE TO NFIELD23
         ADD       "30" TO NFIELD23
DIS27    MOVE      MED0 TO MEDIA
         LOAD      MEDIA FROM NFIELD23 OF MED1,MED2,MED3,MED4,MED5:
                   MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
                   MED15,MED16,MED17,MED18,MED19,MED20,MED21,MED22:
                   MED23,MED24,MED25,MED26,MED27,MED28,MED29
         CLEAR     MEDTYPE
         MOVE      YES TO MEDTYPE
         LOAD      MEDTYPE FROM NFIELD23 OF NO,NO,NO,NO,NO,NO,YES,YES,NO:
                   YES,YES,YES,YES,YES,YES,YES,YES,NO,YES,NO,NO,NO,YES,NO,NO,NO,NO,NO
MEDIAEX  CLEAR     SAMPLE
         CALL      SAMPLE
         CLEAR     RTCNTCT
         CLEAR     COMSLCT
         CLEAR     RTCOMP
         clear     str45
         clear     dim45b
         CLEAR     RTCITY
         CLEAR     RTSTATE
         CLEAR     RTZIP
         CLEAR     CORTN
         CLEAR     CONTDTE
         CLEAR     CONT
         CLEAR     CONTQTY
         MATCH     "C" TO OCOMSLCT            *COMSELECT ORDER?
         CALL      COMSLCT IF EQUAL           *YES
         MATCH     "L" TO OCOMSLCT            *LIFESTYLE OVERLAY?
         CALL      LIFESTYL IF EQUAL          *YES
         MATCH     "I" TO OCOMSLCT            *IC SYSTEMS OVERLAY?
         CALL      ICSYSTEM IF EQUAL          *YES
         PACK      QTYOUT FROM QTYMSK
         MOVE      OQTY TO QTYNUM
         EDIT      QTYNUM TO QTYOUT
         PACK      QTYOUT2 FROM QTYMSK2
         move      oqty to qtynum2
         EDIT      QTYNUM2 TO QTYOUT2
         MATCH     "1" TO OCCODE
         CALL      CONTIN IF EQUAL            *CONTINUATION ORDER.
         MATCH     "2" TO OCCODE
         CALL      CONTIN1 IF EQUAL           *CONTINUATION ORDER/NO OMIT.
         BUMP      OODNUM BY 4
         MOVE      OODNUM TO OFFEROUT
         MOVE      ORTNNUM TO NRTNFLD
         CALL      NRTNKEY
         MATCH     "2531" TO ORTNNUM
         IF        NOT EQUAL
         MATCH     RTCOMP TO MCOMP
         CALL      CHNGRET IF NOT EQUAL
         ENDIF
.
.ON THESE MAILERS THE OFFER DESC. MUST BE USED ON THE RETURN-TO CONTACT
.LINE    677-CMS, 210-COPLON, 53-ANACAPA, 702-MAZEL, 965-ORAM.
.
         MATCH     "0677" TO OMLRNUM       *USE MLR.OFR DESC ON RET-TO?
         CALL      USEOFR IF EQUAL         *YES.
         MATCH     "0210" TO OMLRNUM       *USE MLR.OFR DESC ON RET-TO?
         CALL      USEOFR IF EQUAL         *YES.
         MATCH     "0053" TO OMLRNUM       *USE MLR.OFR DESC ON RET-TO?
         CALL      USEOFR IF EQUAL         *YES.
         MATCH     "0702" TO OMLRNUM       *USE MLR.OFR DESC ON RET-TO?
         CALL      USEOFR IF EQUAL         *YES.
         MATCH     "0965" TO OMLRNUM       *USE MLR.OFR DESC ON RET-TO?
         CALL      USEOFR IF EQUAL         *YES.
         CMATCH    "R" TO ANS               *REPRINT ?
         CALL      REPRT IF EQUAL             *YES.
         CMATCH    "X" TO OSTAT               *CANCELLED ORDER?
         CALL      CANCLLED IF EQUAL
TEST     MOVE      "0" TO NFIELD23           *CLEAR FIELD
         MOVE      OTOCODE TO NFIELD23
         BRANCH    NFIELD23 TO TESTYES,TESTYES
         GOTO      EXCHANGE
TESTYES  MOVE      "X" TO TEST
EXCHANGE
           MOVE        C0,NFIELD23
           MOVE      OELCODE TO NFIELD23
         BRANCH    NFIELD23 TO ENTRENT,EXCHANG1,ENTIRE
         GOTO      OPRINT1            *NO CODE!!!!
SAMPLE   move     C0,nfield23
         MOVE      OSCODE TO NFIELD23
         BRANCH    NFIELD23 OF SAM1,SAM2,SAM3
         MOVE      "  " TO SAMPLE
         RETURN
SAM1     MOVE      "SAMPLE ENCLOSED" TO SAMPLE
         RETURN
SAM2     MOVE      "SAMPLE TO FOLLOW" TO SAMPLE
         RETURN
SAM3     MOVE      "SAMPLE PREVIOUSLY CLEARED" TO SAMPLE
         RETURN
ENTRENT  MOVE      "X" TO ENTIRE
         MOVE      "        " TO EXCHANGE
         GOTO      OPRINT1
EXCHANG1
         MATCH     "         " TO OEXQTY
         GOTO      OPRINT1 IF NOT EQUAL
         MOVE      "EXCHANGE" TO EXCHANGE
         GOTO      OPRINT1
ENTIRE
         MATCH     "         " TO OEXQTY
         GOTO      OPRINT1 IF NOT EQUAL
         MOVE      "EXCHANGE" TO EXCHANGE
         MOVE      "X" TO ENTIRE
OPRINT1
         PACK      LROUT FROM LRMASK
         MOVE      OLRN TO LRNUM
         EDIT      LRNUM TO LROUT
         move       c0 to n2
         move       onetper to n2
         compare    c0 to n2
         MATCH     "         " TO OEXQTY
         GOTO      OPRINT2 IF EOS
         GOTO      OPRINT2 IF EQUAL
         GOTO      OPRINT3
OPRINT2  MATCH     "EXCHANGE" TO EXCHANGE
         GOTO      REALPPM IF NOT EQUAL
         PACK      PRICECK FROM F3,PERIOD,F2
         MATCH     "   .00" TO PRICECK
         GOTO      REALPPM IF NOT EQUAL
         GOTO      OPRINT3
REALPPM
OPRINT3
         MATCH     "  " TO OSHP
         GOTO      NOSHIP IF EQUAL
         GOTO      NOSHIP IF EOS
         move      C0 to nfield23
         MOVE      OSHP TO NFIELD23
         MOVE      SHIP0 TO SHIPdesc
         LOAD      SHIPdesc FROM NFIELD23 OF SHIP1,SHIP2,SHIP3,SHIP4,SHIP5:
                   SHIP6,SHIP7,SHIP8,SHIP9
         GOTO      OPRINT4
NOSHIP   CLEAR     SHIPdesc
OPRINT4
DISREGO
         MOVE      OLRN TO NSPEFLD
         REP       ZFILL,NSPEFLD
         MOVE      "DISREGO-NSPEKEY",Location
         CALL      NSPEKEY
         MOVE      OLRN TO NSPEFLD
         REP       ZFILL,NSPEFLD
         MOVE      "DISREGO-NSPEKEY",Location
         CALL      NSPEKEY
         call      TRIM using DESC002
         call      PARSITUP using line1,DESC002,C1
         call      PARSITUP using line2,DESC002,C1
         call      PARSITUP using line3,DESC002,C1
         call      PARSITUP using line4,DESC002,C1
         call      PARSITUP using line5,DESC002,C1
         call      PARSITUP using line6,DESC002,C1
         call      PARSITUP using line7,DESC002,C1
         call      PARSITUP using line8,DESC002,C1
         call      PARSITUP using line9,DESC002,C1
         call      PARSITUP using line10,DESC002,C1
         call      PARSITUP using line11,DESC002,C1
         call      PARSITUP using line12,DESC002,C1
         call      PARSITUP using line13,DESC002,C1
         call      PARSITUP using line14,DESC002,C1
         MOVE      C0 TO V1
         MOVE      line1,line
         ADD       C1,V1
         CALL      SPCLNSTO                           SPEC INSTRUC ROUTINE
         MOVE      line2,line
         ADD       C1,V1
         CALL      SPCLNSTO
         MOVE      line3,line
         ADD       C1,V1
         CALL      SPCLNSTO
         MOVE      line4,line
         ADD       C1,V1
         CALL      SPCLNSTO
         MOVE      line5,line
         ADD       C1,V1
         CALL      SPCLNSTO
         MOVE      line6,line
         ADD       C1,V1
         CALL      SPCLNSTO
         MOVE      line7,line
         ADD       C1,V1
         CALL      SPCLNSTO
         MOVE      line8,line
         ADD       C1,V1
         CALL      SPCLNSTO
         MOVE      line9,line
         ADD       C1,V1
         CALL      SPCLNSTO
         MOVE      line10,line
         ADD       C1,V1
         CALL      SPCLNSTO
         MOVE      line11,line
         ADD       C1,V1
         CALL      SPCLNSTO
         MOVE      line12,line
         ADD       C1,V1
         CALL      SPCLNSTO
         GOTO      TYPIST
SPCLNSTO
         return
REUSE
         RETURN
TYPIST
        clear   intrnet
        pack    NCNTFLD,OCOCODE
        move    "NCNTKEY",Location
        pack    KeyLocation,"Key: ",NCNTFLD
        call    NCNTKEY
        if not over
                scan    "BILLING",CNTNAME
                goto cntexit if equal
                    move      CNTNAME,str35
                    call      RemoveChar using str35,B1
                    pack      intrnet,str35,"@NINCAL.COM"
cntexit         reset   CNTNAME
        endif
REG
writelol
              goto          doxml
PreReplacit
               Move           "&" to str1
               move           "&amp;" to str6                        .";must be 1st one
               call           ReplaceIt Using str100,str1,str6
               Move           "<" to str1
               move           "&lt;" to str6
               call           ReplaceIt Using str100,str1,str6
               Move           ">" to str1
               move           "&gt;" to str6
               call           ReplaceIt Using str100,str1,str6
               Move           "#"" to str1                                      ."
               move           "&quot;" to str6
               call           ReplaceIt Using str100,str1,str6
               Move           "'" to str1
               move           "&apos;" to str6
               call           ReplaceIt Using str100,str1,str6
               Return

DOXML
                    move      ORTNNUM,NRTNFLD
                    move      "XML-NRTNKEY",Location
                    pack      KeyLocation,"Key: ",NRTNFLD
                    call      NRTNKEY
.
                    clear     MEDIA
                    cmatch    " ",OFOCODE
                    goto      MEDIAEXXML IF EQUAL           *NO MEDIA SELECT
                    goto      MEDIAEXXML IF EOS             * NO MEDIA SELECT
                    move      C0,NFIELD23
                    type      OFOCODE
                    goto      MED10XML IF NOT EQUAL
                    move      OFOCODE,NFIELD23
                    goto      DIS27XML
MED10XML
                    rep       "A0B1C2D3E4F5G6H7I8J9",OFOCODE
                    type      OFOCODE
                    goto      MED20XML IF NOT EQUAL
                    move      OFOCODE,NFIELD23
                    add       C10,NFIELD23
                    goto      DIS27XML
MED20XML
                    rep       "K0L1M2N3O4P5Q6R7S8T9",OFOCODE
                    type      OFOCODE
                    goto      MED30XML IF NOT EQUAL
                    move      OFOCODE,NFIELD23
                    add       "20",NFIELD23
                    goto      DIS27XML
MED30XML
                    rep       "U0V1X2Y3Z4",OFOCODE
                    move      OFOCODE,NFIELD23
                    add       "30",NFIELD23
DIS27XML
                    move      MED0,MEDIA
                    load      MEDIA,NFIELD23,MED1,MED2,MED3,MED4,MED5:
                              MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
                              MED15,MED16,MED17,MED18,MED19,MED20,MED21,MED22:
                              MED23,MED24,MED25,MED26,MED27,MED28,MED29
MEDIAEXXML
                    clear     SHIPDESC
                    call      Trim using OSHP
                    if (OSHP <> "")
                              move      C0,nfield23
                              move      OSHP,NFIELD23
                              move      SHIP0,SHIPDESC
                              load      SHIPDESC FROM NFIELD23 OF SHIP1,SHIP2,SHIP3,SHIP4,SHIP5:
                                        SHIP6,SHIP7,SHIP8,SHIP9
                    endif
                    clear     EXCHANGE
                    move      C0,NFIELD23
                    move      OELCODE,NFIELD23
                    if (NFIELD23 = 2 | NFIELD23 = 3)
                              move      "EXCHANGE",EXCHANGE
                    endif
                    pack      NCNTFLD,OCOCODE
                    move      "XML-NCNTKEY",Location
                    pack      KeyLocation,"Key: ",NCNTFLD
                    call      NCNTKEY
                    call      CreateXMLFile
                    call      LoadXMLFile
.                   call      CloseXMLFile
.         endif
      goto  reado
CreateXMLFile
          clock     timestamp,timestamp
          clear     str45
          unpack    timestamp,str8,str6
.begin patch 3.00
.          pack      Taskname,"\\nins1\e\STORAGE\EXPORT\009411\Orders\tar.",str8,".t",str6,".Pretouch"
          if        (pass = c1)
          pack      Taskname,"\\nins1\e\STORAGE\EXPORT\009411\Orders\tar.",str8,".t",str6,".Pretouch"
          else
          pack      Taskname,"\\nins1\e\STORAGE\EXPORT\009411\Orders\tar1.",str8,".t",str6,".Pretouch"
          endif
.end patch 3.00

          prepare   XMLFile,taskname
          write     Xmlfile,seq;"<nin>"
          weof      xmlfile,seq
          close     xmlfile
          clear     XmlRealFileName
.begin patch 3.00
.          pack      XmlRealFileName,"\\nins1\e\STORAGE\EXPORT\009411\Orders\tar.",str8,".t",str6,".xml"
          if        (pass = c1)
          pack      XmlRealFileName,"\\nins1\e\STORAGE\EXPORT\009411\Orders\tar.",str8,".t",str6,".xml"
          else
          pack      XmlRealFileName,"\\nins1\e\STORAGE\EXPORT\009411\Orders\tar1.",str8,".t",str6,".xml"
          endif
.end patch 3.00

          clear     XmLFIleName
.begin patch 3.00
.          pack      XmlFileName,"\\nins1\e\STORAGE\EXPORT\009411\Orders\tar.",str8,".t",str6,"."
          if        (pass = c1)
          pack      XmlFileName,"\\nins1\e\STORAGE\EXPORT\009411\Orders\tar.",str8,".t",str6,"."
          else      
          pack      XmlFileName,"\\nins1\e\STORAGE\EXPORT\009411\Orders\tar1.",str8,".t",str6,"."
          endif
.end patch 3.00
          prepare   XMLFile,XmlRealFileName
          write     Xmlfile,seq;"<nin>"
          return

LoadXMLFile
          write     Xmlfile,seq;"<lforder>"
          write     Xmlfile,seq;"<lrnum>",OLRN,"</lrnum>"
          write     Xmlfile,seq;"<orderdate>",OODTEC,OODTEY,OODTEM,OODTED,"</orderdate>"
          clear     str100
          move      MCOMP,str100
          call      PreReplacit
          write     Xmlfile,seq;"<mailername>",str100,"</mailername>"
.begin patch 3.01
          clear     str100
.begin patch 3.10
          pack      str2 from osales10,osales
          rep       zfill,str2
          if        (str2 <> "06" & str2 <> "27")              .not list management
          move      Olrn,str100
          else
          move      omlrpon,str100
.          move      omlrpon,str100
          endif
.end patch 3.10
          call      PreReplacit
          write     Xmlfile,seq;"<mailerpo>",str100,"</mailerpo>"        
.end patch 3.01
          clear     str100
          move      O1DES,str100
          call      PreReplacit
          write     Xmlfile,seq;"<listname>",str100,"</listname>"
          clear     str100
          move      NSEL2NAME,str100
          call      PreReplacit
          call      SelectRep
checkw
          write     Xmlfile,seq;"<selection>"
          write     Xmlfile,seq;"<type>"
          write     Xmlfile,seq;str100
          write     Xmlfile,seq;"</type>"
          write     Xmlfile,seq;"<months>"
          write     Xmlfile,seq;str25
          write     Xmlfile,seq;"</months>"
          write     Xmlfile,seq;"<amount>"
          write     Xmlfile,seq;NSEL2PRICE
          write     Xmlfile,seq;"</amount>"
          write     Xmlfile,seq;"</selection>"
          write     Xmlfile,seq;"<qty>",OQTY,"</qty>"
          clear     str100
          move      OMLRKY,str100
          call      PreReplacit
          write     Xmlfile,seq;"<keycode>",str100,"</keycode>"
          if (MEDIA = "" | MEDIA = " ")
                    move      "******",MEDIA
          endif
          clear     str100
          move      MEDIA,str100
          call      PreReplacit
          write     Xmlfile,seq;"<media>",str100,"</media>"
          if (ORTNNUM = "0001")
                    write     Xmlfile,seq;"<shipcompany>","Reuse Order!!!","</shipcompany>"
          else
                    clear     str100
                    move  dim45b,str100
                    call      PreReplacit
                    write     Xmlfile,seq;"<shipcontact>",str100,"</shipcontact>"
                    clear     str100
                    move      RTCOMP,str100
                    call      PreReplacit
                    write     Xmlfile,seq;"<shipcompany>",str100,"</shipcompany>"
                    clear     str100
                    move      RTADDR,str100
                    call      PreReplacit
                    write     Xmlfile,seq;"<shipaddr>",str100,"</shipaddr>"
..... patch for infogroup email
//Again this is hardwired for triplex i believe due to the lenghth and def of fields in ninrtn file 
                              If (RTNUM = "5318")
                              write     Xmlfile,seq;"<shipemail>","incoming.files@infogroup.com","</shipemail>" 
                              endif   
                    write     Xmlfile,seq;"<shipcity>",RTCITY,"</shipcity>"
                    write     Xmlfile,seq;"<shipstate>",RTSTATE,"</shipstate>"
                    write     Xmlfile,seq;"<shipzip>",RTZIP,"</shipzip>"
          endif
          write     Xmlfile,seq;"<maildate>",OMDTEC,OMDTEY,OMDTEM,OMDTED,"</maildate>"
          write     Xmlfile,seq;"<shipdate>",ORTNDTEC,ORTNDTEY,ORTNDTEM,ORTNDTED,"</shipdate>"
          write     Xmlfile,seq;"<shipvia>",SHIPDESC,"</shipvia>"
          move      C0,nfield52
          write     Xmlfile,seq;"<oppm>",NSEL2PRICE,"</oppm>"
          write     Xmlfile,seq;"<selprice>",NSEL2SPRICE,"</selprice>"
          if (OELCODE = "2" | OELCODE = "3")
                    write     Xmlfile,seq;"<exchange>","EXCHANGE","</exchange>"
          endif
.begin code 3.11
           call       trim using OMlrLstCd
           if         (OMlrLStCd <> "")
                    write     Xmlfile,seq;"<MlrListCode>",OMlrLstCd,"</MlrListCode>"
          endif          
.end code 3.11
          write     Xmlfile,seq;"<specialselections>"
          PackData.GetCount giving N10
          if (N10 > C0)
                    move      C0,N1
                    for result,"1",N10
                              getitem   PackData,result,NREFDESC
                              move      NREFDESC,str100
                              if (str100 <> "")
                                        call      PreReplacit
                              endif
                              write     Xmlfile,seq;str100
                    repeat
          endif
          write     Xmlfile,seq;"</specialselections>"
          write     Xmlfile,seq;"<specialinstructions>"
          for InstructionCOunter,"1","14"
                    clear     str100
                    load      str100,InstructionCounter,Line1,Line2,Line3,Line4,Line5,Line6,Line7:
                              Line8,line9,line10,line11,line12,line13,line14
                    if (str100 <> "")
                              call      PreReplacit
                    endif
                    write     Xmlfile,seq;str100
          repeat
          write     Xmlfile,seq;"</specialinstructions>"
          if (occode = "1")         continuation omit
                    write     Xmlfile,seq;"<opu>"
                    write     Xmlfile,seq;"<opulr>",OLRNCO,"</opulr>"
                    write     Xmlfile,seq;"<opudate>",OODTECOC,OODTECOY,OODTECOM,OODTECOD,"</opudate>"
                    write     Xmlfile,seq;"<opuqty>",OQTYCO,"</opuqty>"
                    write     Xmlfile,seq;"</opu>"
          endif
          write     Xmlfile,seq;"<contact>",CNTNAME,"</contact>"
          if (OELCODE = "1" | OELCODE = "3")
                    write     Xmlfile,seq;"<entirelist>","TRUE"
                    write     Xmlfile,seq;"</entirelist>"
          endif
endxml
          write     Xmlfile,seq;"</lforder>"
          write     Xmlfile,seq;"</nin>"
          weof      XMLFile,seq
          close     XMLfile
          clear     Taskname
          pack      taskname,XMLFileName,"Pretouch"
          clear     str45
          clear     taskname1
          pack      taskname1,XMLFileName,"Touch"
   pause     "3"
          rename    Taskname,Taskname1
          erase   Taskname1
          return

CloseXMLFile
          return

MERGY
PRNTLABL
NOPO
         RETURN
CHNGRET
         clear     dim45b
         move      mcomp  to dim45b
         MOVE      "C/O" TO CORTN
         RETURN
.
. USEOFR - PRINT OFFER DESC AS RETURN-TO CONTACT.
USEOFR
         clear      dim45b
         move       OFDESC to dim45b
         MOVE      "C/O" TO CORTN
         RETURN
.
. CONTIN - CONTINUATION ORDER, INCLUDE EXTRA INFORMATION.
CONTIN
         MOVE      "X" TO CONT
         PACK      CONTDTE FROM OODTECOM,SLASH,OODTECOD,SLASH,OODTECOC,OODTECOY
         MOVE      QTYMSK TO CONTQTY
         MOVE      OQTYCO TO QTYNUM
         EDIT      QTYNUM TO CONTQTY
         RETURN
. CONTIN1 - CONTINUATION ORDER, NO OMIT.
CONTIN1
         MOVE      "X" TO CONT1
         RETURN
.
. REPRT - REPRINT ORDER, PRINT AT TOP.
REPRT
.         MOVE      "*** REPRINT ***" TO REPRT
         RETURN
.
. CANCLLED - CANCELLED ORDER, PRINT AT TOP.
CANCLLED
.         MOVE      "**CANCELLED**" TO REPRT
         RETURN
.
. MEDMEMO - MAG TAPE ORDER, INCLUDE ADDITION INFO.
.MEDMEMO  MOVE      "INCLUDE LAYOUT & DUMP." TO MEDMEMO
.MEDMEMO  MOVE      MED0 TO MEDIA
.         RETURN
.
.
. COMSLCT - COMSELECT ORDER.
COMSLCT  MOVE      "**CC: CONSUMER DIRECT" TO COMSLCT
         RETURN
LIFESTYL MOVE      "CC:LIFESTYLE SELECTOR" TO COMSLCT
         RETURN
ICSYSTEM  MOVE      "**CC: IC SYSTEMS **" TO COMSLCT
         RETURN
NOTDMC
         DISPLAY   *P1:24,*EL,*HON,"NON-TRIPLEX ORDER",*HOFF,*B,*B,*B,*B,*B;
         GOTO      READO
.
NOORD
         DISPLAY     *W2,*R,*P1:24,*EL,"****SPOOLING STOPED!!!!!!":
                   *P20:24,*R,*HON,*EL,"NO RECORD FOUND FOR LR##",OLRN
        KEYIN     *P78:24,*HOFF,STR1;
        GOTO      READO

FIRSTORD
          return
NOMLR
         MOVE      "NO SUCH MAILER" TO MCOMP
         RETURN
.
                                        include   compio.inc
                                        include   cntio.inc
         INCLUDE   NOWNIO.inc
         INCLUDE   NORDIO.inc
         INCLUDE   NCRCIO.inc
         INCLUDE   NRTNIO.inc
         INCLUDE   NSPIIO.inc
         INCLUDE   NOFRIO.INC
         INCLUDE   NSPEIO.INC
         INCLUDE   COMLOGIC.inc
ABORT    DISPLAY   *P1:24,*HON,*BLINKON,*RED,"JOB ABORTED",*B,*B,*B,*W5,*B
EOJ
          
eoj4
         IFNZ      PC
         FLUSH     NAMFILE
         XIF
         CLOSE     targord
         SPLCLOSE
         MOVE      "-1" TO SEQ
.begin patch 3.0
          if        (pass = c1)
          move      c2,pass
          goto      Pass2
          endif
.end patch 3.0
         STOP
IO
         TRAPCLR    IO
         NORETURN
         TRAP      IO IF IO
         BRANCH    FILE OF ONE,TWO,THREE,FOUR,FIVE:
                   SIX,SEVEN,EIGHT,NINE,TEN,TWELVE
ZERO
         DISPLAY   *P1:24,"UNDEFINED IO ERROR",*W2;
         GOTO      IOEXIT
ONE
         DISPLAY   *P1:24,"TDMCORD.OUT FILE ERROR",*W2;
         GOTO      IOEXIT
TWO
         DISPLAY   *P1:24,"TRIPLEX1.OUT FILE ERROR",*W2;
         GOTO      IOEXIT
THREE
         DISPLAY   *P1:24,"BADORD FILE ERROR",*W2;
         GOTO      IOEXIT
FOUR
         DISPLAY   *P1:24," FILE ERROR",*W2;
         GOTO      IOEXIT
FIVE
         DISPLAY   *P1:24," FILE ERROR",*W2;
         GOTO      IOEXIT
SIX
         DISPLAY   *P1:24," FILE ERROR",*W2;
         GOTO      IOEXIT
SEVEN
         GOTO      IOEXIT
EIGHT
         DISPLAY   *P1:24," LABEL SPOOL FILE ERROR",*W2;
         KEYIN     *R,*P1:24,"DO YOU WANT ME TO CREATE FILE ",ANS;
         CMATCH    "Y" TO ANS
         GOTO      PREPLABL IF EQUAL
         CMATCH    "N" TO ANS
         GOTO      EIGHT IF NOT EQUAL
         GOTO      IOEXIT
NINE
         DISPLAY   *P1:24,"BAD ORDER FILE ERROR",*W2;
         GOTO      IOEXIT
TEN      DISPLAY   *P1:24,*EL,"ORDER SPOOL FILE ERROR",*W2
         GOTO      IOEXIT
ELEVEN   DISPLAY   *P1:24,*EL,"ORDER FILE ERROR",*W2
         GOTO      IOEXIT
TWELVE   DISPLAY   *P1:24,*EL,"NINSPEC FILE ERROR",*W2
         GOTO      IOEXIT
IOEXIT
         KEYIN     *P60:24,*B,ANS;
         CMATCH    "Q" TO ANS
         GOTO      IOEXIT1 IF EQUAL
         BRANCH    FILE OF ONE,TWO,THREE,FOUR,FIVE,SIX,SEVEN,EIGHT
         GOTO      ZERO
IOEXIT1  TRAPCLR   IO
         SHUTDOWN  "ALERT"
PREPLABL
          call      CleanFaxfile
         GOTO      CLOCK
.START PATCH 2.64 ADDED LOGIC
CleanFaxfile
          return
.END PATCH 2.64 ADDED LOGIC

.START PATCH 09/22/2004 ADDED LOGIC
SelectRep
          pack      str25,B55
          scan      leftparan,str100
          if equal
                    bump      str100
                    movefptr str100,startfp
                    move      C0,endfp
                    loop
                              move      str100,str1
                              type      str1
                              if not equal
.START PATCH 09/22/2004 REPLACED LOGIC
.                                       break
                                        if (str1 <> DASH)
                                                  break
                                        endif
.END PATCH 09/22/2004 REPLACED LOGIC
                              endif
                              movefptr str100,endfp
                              bump      str100
                    repeat
                    if (endfp >= startfp)
                              reset     str100,startfp
                              setlptr   str100,endfp
                              move      str100,str25
                              reset     str100
                              setlptr   str100
                              pack      str35,"(",str25
                              call      ReplaceIt using str100,str35,leftparan
                              pack      str100,str100,B55,B55
.
                              move      str100,str100a
                              rep       lowup,str100a


                              move      "MOS",str6
                              scan      str6,str100a
                              if equal
                                        movefptr str100a,result
                                        add       C2,result,howmany
                                        reset     str100,result
                                        setlptr   str100,howmany
                                        move      str100,str6
                                        reset     str100
                                        setlptr   str100
                                        call      ReplaceIt using str100,str6,B1
                                        pack      str100,str100,B55,B55
                              else
                                        move      "MONTHS",str6
                                        scan      str6,str100a
                                        if equal
                                                  movefptr str100a,result
                                                  add       C5,result,howmany
                                                  reset     str100,result
                                                  setlptr   str100,howmany
                                                  move      str100,str6
                                                  reset     str100
                                                  setlptr   str100
                                                  call      ReplaceIt using str100,str6,B1
                                                  pack      str100,str100,B55,B55
                                        else
                                                  move      "MONTH",str6
                                                  scan      str6,str100a
                                                  if equal
                                                            movefptr str100a,result
                                                            add       C4,result,howmany
                                                            reset     str100,result
                                                            setlptr   str100,howmany
                                                            move      str100,str6
                                                            reset     str100
                                                            setlptr   str100
                                                            call      ReplaceIt using str100,str6,B1
                                                            pack      str100,str100,B55,B55
                                                  endif
                                        endif
                              endif
                    endif
                    reset     str100
                    setlptr   str100
                    move      "( ",str2
                    call      ReplaceIt using str100,str2,leftparan
                    pack      str100,str100,B55,B55
                    move      "( ",str2
                    call      ReplaceIt using str100,str2,leftparan
                    pack      str100,str100,B55,B55
                    move      "(/",str2
                    call      ReplaceIt using str100,str2,leftparan
                    pack      str100,str100,B55,B55
                    move      "( ",str2
                    call      ReplaceIt using str100,str2,leftparan
                    pack      str100,str100,B55,B55
                    move      "( ",str2
                    call      ReplaceIt using str100,str2,leftparan
                    pack      str100,str100,B55,B55
                    move      "~",str1
                    move      "()",str2
                    call      ReplaceIt using str100,str2,B55
                    pack      str100,str100,B55,B55
                    move      "( )",str3
                    call      ReplaceIt using str100,str3,B55
                    pack      str100,str100,B55,B55
                    reset     str100
                    setlptr   str100
          endif
          return

         INCLUDE   NCNTIO.inc
          INCLUDE   NSEL2IO.INC
          INCLUDE   NSEL3IO.INC
          INCLUDE   NADDIO.INC
          INCLUDE   NSLTIO.INC
          INCLUDE   NREFIO.INC
          INCLUDE   NMODIO.INC
