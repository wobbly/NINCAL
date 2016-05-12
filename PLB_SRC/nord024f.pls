..
.. PURPOSE - READS INPUT FILE (NINORD) AND SPOOLS
..           RECORDS FOR TRANSMISSION TO FRontline xML FORMAT.
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
.         INCLUDE   NSPIDD.inc
         INCLUDE   MEDIA.inc
         INCLUDE   SHIPPING.inc
         INCLUDE   NOFRDD.INC
         INCLUDE   NSPEDD.INC
         INCLUDE   HP.INC
        INCLUDE WINAPI.INC
.         INCLUDE   NFULDD.INC
          INCLUDE   NSEL2DD.INC
          INCLUDE   NSEL3DD.INC
          INCLUDE   NADDDD.INC
          INCLUDE   NSLTDD.INC
          INCLUDE   NREFDD.INC
          INCLUDE   NMODDD.INC
..
release  init      "2.78"         DLH added OMlrLstCd
Reldate   Init      "2016 March 16"
.Release  init      "2.77"       DLH   add mailer PO and return-to email (infogroup) use datamanager
.reldate   Init     "2013 June 5" 
.Release  init      "2.76"       DLH   Change TDMC address
.reldate   Init     "09 December 2010" 
.Release  init      "2.75"       DLH   Add Division to XML defs
.reldate   Init     "25 October 2010" 
.Release  init      "2.74.2"       DMB   12OCT06  Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.release  init      "2.74.1"       DMB   16SEP06  Changed Location of xml files
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
FulOrd   FILE               .Fulfillment ORDER INPUT FILE.
BADORD   FILE
NAMFILE  FILE      FIX=185      .Added 2 bytes to file
WEEKFILE FILE      FIX=146      .Added 4 bytes to file
SAVEFILE IFILE
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
.MAILCC   DIM       17                       MAILER
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
XMLFileName    Dim            255
PackData DataList
startfp   form      9
endfp     form      9
leftparan init      "("
str100a   Dim       100

Hits      Form    9

          create    PackData=1:1:1:1
          move      C1,NORDPATH
          
          
.begin patch 2,77
.        OPEN      FulOrd,"\\nins1\e\data\nprint.ful",EXCLUSIVE
        OPEN      FulOrd,"\\nins1\e\data\nprint.ful|10.10.30.103:502",EXCLUSIVE
.end patch 2,77
.       OPEN      FulOrd,"c:\work\nprint.ful",EXCLUSIVE
.         erase     "c:\work\test.xml"

CreateXMLFile
          clock     timestamp,timestamp
          clear     str45
          unpack    timestamp,str8,str6
          clear     XmLFIleName
.Patch 2.74.1 File relocation 
.         pack      XmlFileName,"\\nins1\e\STORAGE\XML\Frontline\FRNTLINE.",str8,".t",str6,".xml"
.begin patch 2,77
.          pack      XmlFileName,"\\nins1\E\STORAGE\EXPORT\009410\Orders\FRNTLINE.",str8,".t",str6,".xml"
          pack      XmlFileName,"\\nins1\E\STORAGE\EXPORT\009410\Orders\FRNTLINE.",str8,".t",str6,".xml|10.10.30.103:502"
.end patch 2,77
.Patch 2.74.1 File relocation 
          prepare   Xmlfile,Xmlfilename      
          
.         write     Xmlfile,seq;"<nin>" 
          
//
ReadO
          Move      SEVEN TO FILE
        Filepi    1;FulORD
        Read      FulOrd,SEQ;ordvars
        Goto      EOJ IF OVER
        Add       C1 TO COUNTR
        DISPLAY   *P10:12,*EL,"COUNT READ ",COUNTR,b1,olrn
        CMatch    "x" TO OSTAT       Cancelled Pending order ?
        Goto      ReadO IF Equal     YES, Do not write, leave in file.
.note cancodes also updated to skip cancelled pending orders.
        Reset     cancodes
        CMatch    "z" to ostat      .cancelled LCR skip
        Goto      ReadO if equal
        Scan      OSTAT in cancodes
        Goto      ReadO  if equal
        Cmatch    "R" in ostat
        Goto      ReadO if equal
        pack      salenumb from osales10,osales
        CMATCH    " " TO OLRN
        GOTO      ReadO IF EOS
        GOTO      ReadO IF EQUAL
        MOVE      C0 TO QTYNUM
        MOVE      C0 TO QTYNUM2
        MOVE      OQTY TO QTYNUM
        MOVE      OQTY TO QTYNUM2
        COMPARE   C0 TO QTYNUM
        GOTO      ReadO IF EQUAL
        match     "0001",ortnnum
        goto      ReadO if equal
        PACK      MKEY FROM OMLRNUM,OCOBN
        CALL      NMLRKEY
        ADD       C1 TO COUNT
        PACK      MKEY FROM OMLRNUM,OCOBN
        CALL      NMLRKEY
        CALL      NOMLR IF OVER
        MOVE      OLON TO NOWNFLD
        MOVE      "10" TO FILE
        REP       " 0" IN NOWNFLD
        Call      NOWNKEY
Checkful
.Start Patch 1.34 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.        
.        Call       Trim using OWNCTN
        Call        Trim using OFULLFIL
.End Patch 1.34 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.                  
//        If (OWNCTN <> "")
//                  Pack      NFULFLD,OWNctn
//                  Rep       zfill,NFULFLD
//                  Move      C1,NFULPATH
//                  Move      "READO-NFULKEY",Location
//                  Pack      KeyLocation,NFULFLD
//                  Call      NFULKEY
//                  If (Nfulnum = "0030")
//                            Add c1 to hits
//Only write to file if we have a valid record                        
//                            If (Hits = c1)
//                                      write     Xmlfile,seq;"<nin>"                                         
//                            Endif
//                            Call      LoadXML
//               Endif
//        Else
//                  Clear     NFULFLD
//                  Clear     NFULCOMP
//        Endif
//
.Start Patch 1.34 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.   
.         if (OWNCTN <> "")
.                   pack      COMPFLD6,OWNctn
.                   rep       zfill,COMPFLD6
.                   move      C1,COMPPATH
.                   move      "READO-COMPKEY6",Location
.                   pack      KeyLocation,COMPFLD6
.                   call      COMPKEY6
.                   If over
.                             Clear     COMPFLD6
.                             Clear     COMPCOMP            
.                   Else
.                             If (COMPSVBFLG = "T")
.                                       If (COMPFLD6 = "0030")
.                                                 Add c1 to hits
//Only write to file if we have a valid record                        
.                                                 If (Hits = c1)
.                                                           write     Xmlfile,seq;"<nin>"                                         
.                                                 Endif
.                                                 Call      LoadXML
.                                       Else
.                                                 Clear     COMPFLD6
.                                                 Clear     COMPCOMP                                
.                                       Endif                         
.                             
.                             Else
.                                       Clear     COMPFLD6
.                                       Clear     COMPCOMP
.                             Endif               
.                   Endif
.         Else
.                   Clear     COMPFLD6
.                   Clear     COMPCOMP
.         Endif
.End Patch 1.34 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.     
.Start Patch 1.34 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.               
          if (OFULLFIL <> "")
                    pack      COMPFLD,OFULLFIL
                    call      zfillit using COMPFLD
                    move      C1,COMPPATH
                    move      "READO-COMPKEY",Location
                    pack      KeyLocation,COMPFLD
                    call      COMPKEY
                    If over
                              Clear     COMPFLD
                              Clear     COMPCOMP            
                    Else
                              If (COMPFLD = "009410")
                                        Add c1 to hits
//Only write to file if we have a valid record                        
                                        If (Hits = c1)
                                                  write     Xmlfile,seq;"<nin>"                                         
                                        Endif
                                        Call      LoadXML
                              Else
                                        Clear     COMPFLD
                                        Clear     COMPCOMP                                
                              Endif                         
                    Endif
          Else
                    Clear     COMPFLD
                    Clear     COMPCOMP
          Endif
.End Patch 1.34 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.       
        Goto    ReadO                              

LoadXML
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
         pack      salenumb from osales10,osales
         MOVE      C0 TO QTYNUM
         MOVE      C0 TO QTYNUM2
         MOVE      OQTY TO QTYNUM
         MOVE      OQTY TO QTYNUM2
         PACK      QTYOUT FROM QTYMSK
         EDIT      QTYNUM TO QTYOUT
         PACK      QTYOUT2 FROM QTYMSK2
         EDIT      QTYNUM2 TO QTYOUT2
         ADD       C1 TO COUNT
         move      OLRN,NSPEFLD
         move      "NSPEKEY",Location
         call      NSPEKEY
         MOVE      " " TO EXCHANGE
         MOVE      " " TO ENTIRE
         MOVE      " " TO TEST
         MOVE      " " TO CONT
         MOVE      " " TO CONT1
         MOVE      " " TO REPRT
         PACK      MKEY FROM OMLRNUM,OCOBN
         CALL      NMLRKEY
         MOVE      OLON TO NOWNFLD
         MOVE      "10" TO FILE
         REP       " 0" IN NOWNFLD
         CALL      NOWNKEY
.
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
NCRCEXIT MOVE      "0" TO NFIELD4
         MOVE      ORTNNUM TO NFIELD4
         BRANCH    NFIELD4 OF NOTDMC
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
.;
.;ON THESE MAILERS THE OFFER DESC. MUST BE USED ON THE RETURN-TO CONTACT
.;LINE    677-CMS, 210-COPLON, 53-ANACAPA, 702-MAZEL, 965-ORAM.
.;
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
.....Following logic removed as decision was made to print DESC001 (XSTAT) in last two lines....
.         call      PARSITUP using line15,DESC002,C1
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
               move           "&amp;" to str6                        ;must be 1st one
               call           ReplaceIt Using str100,str1,str6
               Move           "<" to str1
               move           "&lt;" to str6
               call           ReplaceIt Using str100,str1,str6
               Move           ">" to str1
               move           "&gt;" to str6
               call           ReplaceIt Using str100,str1,str6
               Move           "#"" to str1
               move           "&quot;" to str6
               call           ReplaceIt Using str100,str1,str6
               Move           "'" to str1
               move           "&apos;" to str6
               call           ReplaceIt Using str100,str1,str6
               call Trim using str100
               Return
."
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
                    call      LoadXMLFile
      return

LoadXMLFile
          write     Xmlfile,seq;"<lforder>"
          write     Xmlfile,seq;"<lrnum>",OLRN,"</lrnum>"
.         write     Xmlfile,seq;"<orderdate>",OODTEM,DASH,OODTED,DASH,OODTEC,OODTEY,"</orderdate>"
          write     Xmlfile,seq;"<orderdate>",OODTEC,OODTEY,DASH,OODTEM,DASH,OODTED,"</orderdate>"
.begin patch 2.77
          write     Xmlfile,seq;"<mailerpo>",OMLRPON,"</mailerpo>"
.end patch 2.77
          clear     str100
          move      MCOMP,str100
          call      PreReplacit
          write     Xmlfile,seq;"<mailername>",str100,"</mailername>"
          clear     str100
          move      O1DES,str100
          call      PreReplacit
          write     Xmlfile,seq;"<listnum>",OLNUM,"</listnum>"
          write     Xmlfile,seq;"<listname>",str100,"</listname>"
          clear     str100
          move      NSEL2NAME,str100
          call      PreReplacit
.         call      SelectRep
.
          write     Xmlfile,seq;"<selection>",str100,"</selection>"
.         write     Xmlfile,seq;"<selection>"
.         write     Xmlfile,seq;"<type>"
.         call      Trim using str100
.         write     Xmlfile,seq;str100
.         write     Xmlfile,seq;"</type>"
.         write     Xmlfile,seq;"<months>"
.         write     Xmlfile,seq;str25
.         write     Xmlfile,seq;"</months>"
.         write     Xmlfile,seq;"<amount>"
.         write     Xmlfile,seq;NSEL2PRICE
.;        write     Xmlfile,seq;b1,str9
.         write     Xmlfile,seq;"</amount>"
.         write     Xmlfile,seq;"</selection>"
          write     Xmlfile,seq;"<qty>",OQTY,"</qty>"
          clear     str100
          move      OMLRKY,str100
          call      PreReplacit
          write     Xmlfile,seq;"<keycode>",str100,"</keycode>"
          write     Xmlfile,seq;"<division>FDG</division>"
.NEW FIELD
          move      OMLRPON,str100
          call      PreReplacit
          write     Xmlfile,seq;"<ponum>",str100,"</ponum>"
.
          if (MEDIA = "" | MEDIA = " ")
                    move      "******",MEDIA
          endif
          clear     str100
          move      MEDIA,str100
          call      PreReplacit
          write     Xmlfile,seq;"<media>",str100,"</media>"
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
          write     Xmlfile,seq;"<shipcity>",RTCITY,"</shipcity>"
          write     Xmlfile,seq;"<shipstate>",RTSTATE,"</shipstate>"
          write     Xmlfile,seq;"<shipzip>",RTZIP,"</shipzip>"
.NEW FIELD
          clear     str100
//Again this is hardwired for triplex i believe due to the lenghth and def of fields in ninrtn file 
        If (RTNUM = "5318")

.begin patch 2.76
                    write     Xmlfile,seq;"<shipemail>","incoming.files@infogroup.com","</shipemail>" 
.                    write     Xmlfile,seq;"<shipemail>","incoming.files@donnelley.infousa.com","</shipemail>" 
.end patch 2.76
          Else
                    write     Xmlfile,seq;"<shipemail>",str100,"</shipemail>"
          Endif     
.
.         write     Xmlfile,seq;"<maildate>",OMDTEM,DASH,OMDTED,DASH,OMDTEC,OMDTEY,"</maildate>"
          write     Xmlfile,seq;"<maildate>",OMDTEC,OMDTEY,DASH,OMDTEM,DASH,OMDTED,"</maildate>"
.         write     Xmlfile,seq;"<shipdate>",ORTNDTEM,DASH,ORTNDTED,DASH,ORTNDTEC,ORTNDTEY,"</shipdate>"
          write     Xmlfile,seq;"<shipdate>",ORTNDTEC,ORTNDTEY,DASH,ORTNDTEM,DASH,ORTNDTED,"</shipdate>"
          write     Xmlfile,seq;"<shipvia>",SHIPDESC,"</shipvia>"
          move      C0,nfield52
          write     Xmlfile,seq;"<oppm>",NSEL2PRICE,"</oppm>"
          write     Xmlfile,seq;"<selprice>",NSEL2SPRICE,"</selprice>"
.begin code 2.78
           call       trim using OMlrLstCd
           if         (OMlrLStCd <> "")
                    write     Xmlfile,seq;"<MlrListCode>",OMlrLstCd,"</MlrListCode>"
          endif          
.end code 2.78

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
          CALL DEBUG          
          write     Xmlfile,seq;"</specialinstructions>"
          if (occode = "1")         continuation omit
                    write     Xmlfile,seq;"<opu>"
                    write     Xmlfile,seq;"<opulr>",OLRNCO,"</opulr>"
                    write     Xmlfile,seq;"<opudate>",OODTECOC,OODTECOY,DASH,OODTECOM,DASH,OODTECOD,"</opudate>"
                    write     Xmlfile,seq;"<opuqty>",OQTYCO,"</opuqty>"
                    write     Xmlfile,seq;"</opu>"
          endif
          write     Xmlfile,seq;"<contact>",CNTNAME,"</contact>"
          if (OELCODE = "1" | OELCODE = "3")
                    write     Xmlfile,seq;"<entirelist>","TRUE"
                    write     Xmlfile,seq;"</entirelist>"
          else
                    write     Xmlfile,seq;"<entirelist>","</entirelist>"
          endif
          if (OELCODE = "2" | OELCODE = "3")
                    write     Xmlfile,seq;"<orderterms>","Exchange","</orderterms>"
          else
                    write     Xmlfile,seq;"<orderterms>","</orderterms>"        
          endif
          write     Xmlfile,seq;"<requesttype>","New","</requesttype>"

endxml
          write     Xmlfile,seq;"</lforder>"
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
//USEOFR - PRINT OFFER DESC AS RETURN-TO CONTACT.
USEOFR
         clear      dim45b
         move       OFDESC to dim45b
         MOVE      "C/O" TO CORTN
         RETURN
//CONTIN - CONTINUATION ORDER, INCLUDE EXTRA INFORMATION.
CONTIN
         MOVE      "X" TO CONT
//
         PACK      CONTDTE FROM OODTECOM,SLASH,OODTECOD,SLASH,OODTECOC,OODTECOY
         MOVE      QTYMSK TO CONTQTY
         MOVE      OQTYCO TO QTYNUM
         EDIT      QTYNUM TO CONTQTY
         RETURN
// CONTIN1 - CONTINUATION ORDER, NO OMIT.
CONTIN1
         MOVE      "X" TO CONT1
         RETURN
//REPRT - REPRINT ORDER, PRINT AT TOP.
REPRT
         RETURN
// CANCLLED - CANCELLED ORDER, PRINT AT TOP.
CANCLLED
         RETURN
// COMSLCT - COMSELECT ORDER.
COMSLCT  MOVE      "**CC: CONSUMER DIRECT" TO COMSLCT
         RETURN
LIFESTYL MOVE      "CC:LIFESTYLE SELECTOR" TO COMSLCT
         RETURN
ICSYSTEM  MOVE      "**CC: IC SYSTEMS **" TO COMSLCT
         RETURN
NOTDMC
         return
NOORD
         return

FIRSTORD
.         call      CreateXMLFile
          return
NOMLR
         MOVE      "NO SUCH MAILER" TO MCOMP
         RETURN
           INCLUDE   COMPIO.inc
           INCLUDE   CNTIO.inc
         INCLUDE   NOWNIO.inc
         INCLUDE   NORDIO.inc
         INCLUDE   NCRCIO.inc
         INCLUDE   NRTNIO.inc
.         INCLUDE   NSPIIO.inc
         INCLUDE   NOFRIO.INC
         INCLUDE   NSPEIO.INC
         INCLUDE   COMLOGIC.inc
ABORT    DISPLAY   *P1:24,*HON,*BLINKON,*RED,"JOB ABORTED",*B,*B,*B,*W5,*B
EOJ
eoj4
         IFNZ      PC
                   FLUSH     NAMFILE
         XIF
         
          If (Hits > c0)

                    Write XmlFile,seq;"</nin>"  
                    Weof  XmlFile,seq
          Endif                
           Close              XMLfile    
         Close                FulORD
         SPLCLOSE
         MOVE      "-1" TO SEQ
          shutdown


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
CleanFaxfile
          return

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
                                        if (str1 <> DASH)
                                                  break
                                        endif
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
        INCLUDE NCNTIO.inc
.         INCLUDE   NFULIO.INC
          INCLUDE   NSEL2IO.INC
          INCLUDE   NSEL3IO.INC
          INCLUDE   NADDIO.INC
          INCLUDE   NSLTIO.INC
          INCLUDE   NREFIO.INC
          INCLUDE   NMODIO.INC