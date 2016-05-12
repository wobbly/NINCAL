*NCRC0002 - CORRECTION SELECT & SUMMARY PRINT PROGRAM
;.............................................................................
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NCRCDD.inc
.START PATCH 1.82 REPLACED LOGIC
.         INCLUDE   NMLRDD.inc
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
.END PATCH 1.82 REPLACED LOGIC
         INCLUDE   NDATDD.inc
         INCLUDE   NORDDD.inc
         INCLUDE   NSPEDD.inc
         INCLUDE   NUSEDD.inc
         INCLUDE   HP.inc

release  init      "1.82"            27MAY2004 ASH MAILER CONVERSION
.release  init      "1.81"            12Jul2002 DLH Update Osflag logic to use GetWinVer
;release  init      "1.8"            30Mar2001 DLH Replace if exist c:\windows logic
;release  init      "1.7"            04OCT2000 ASH NEW SERVER ADDED
;release  init      "1.6"            06DECJD added code for nt/win95-98 copy.
;release  init      "1.5"           11MAY99 ASH NINSPE Conversion/expansion
;RELEASE  INIT      "1.4"           10FEB99 ASH NINORD Y2K, File expansion
;release  init      "1.3"           07DEC98 JD cleaned up print for Mcomp new size
;release  init      "1.2"          07apr98 DLH year 2000 compliance new keys
;RELEASE  INIT      "1.2"          03nov95 jd clear special inst vars..
;RELEASE  INIT      "1.1"         31AUG92 DLH  PRINT LIST NUMBER.
;
;RELEASE  INIT      "1.0"
LINES    FORM      2
PAGE     FORM      "000"
DATE     DIM       8
DATEBR   FORM      1
TYPBR    FORM      1
;
WRKFILE  IFILE     KEYLEN=11,DUP
;OUTSP    IFILE     KEYLEN=6,VAR=288
WRKNAME  DIM       8
WRKNAM1  INIT      "CRCWK   "
WRKFLD   DIM       11
;
TYPPICK  DIM       30        chosen TYPISTS
TYPOMIT  DIM       15        chosen TYPIST omissions
OMITBRCH FORM      1         if TYPIST omits were requested,0=yes,1=no
TYPMASK  INIT      "XXX-XXX-XXX-XXX-XXX-XXX-XXX-XXX-XXX-XXX"
TYPS     DIM       39        above mask breaks up 'TYPISTS' field for scan
REVPICK  DIM       112       chosen dates
REVPICK2 DIM       112       chosen dates only when mm/dd/yy is used
REVCOUNT FORM      2         screen line counter
YEAR     DIM       4         keyed in year and year from datmst
MO       DIM       2         keyed in month and month from datmst
MOYR     DIM       6         month & year 
DAY      DIM       2         keyed in day and day from datmst
REVBRCH  FORM      1         used to determine if/how dates should
;                             be checked. 0=no,1=yy,2=mmyy,3=mmddyy
H1       FORM      2         horizontal screen coordinate
H2       FORM      2                     "
H3       FORM      2                     "
H4       FORM      2                     "
H5       FORM      2                     "
;
LODATE   FORM      5         julian low order date   (yyjjj)
HIDATE   FORM      5         julian high order date  (yyjjj)
RECJUL   FORM      5         julian order date from NINORD3
;
PICK     DIM       1         KEYED IN PICK OFF CHOICE.
NUMPICK  FORM      1         NUMERIC PICK OFF CHOICE
ANS      DIM       1
WORK06   DIM       6
PRSPCL1 DIM       47                    LASER/FAX PRINT OF SPECIAL INST.
PRSPCL2 DIM       47                    LASER/FAX PRINT OF SPECIAL INST.
PRSPCL3 DIM       47                    LASER/FAX PRINT OF SPECIAL INST.
PRSPCL4 DIM       47                    LASER/FAX PRINT OF SPECIAL INST.
PRSPCL5 DIM       47                    LASER/FAX PRINT OF SPECIAL INST.
PRSPCL6 DIM       47                    LASER/FAX PRINT OF SPECIAL INST.
SPCL     DIM       2           *SPECIAL INSTRUCTION CODE
SPCL1    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL2    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL3    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL4    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL5    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL6    DIM       2           *SPECIAL INSTRUCTION CODE
;START PATCH 1.5 - ADDED VARS
holdstr  dim      758
line1    dim      146
line2    dim      146
line3    dim      146
line4    dim      146
line5    dim      146
CARR     INIT     0x7f
carrfill dim      2
;begin patch 1.81
;begin patch 1.8
;osflag   form     1    1=win9x 2=winnt win2k
;end patch 1.8
;end patch 1.81

;END PATCH 1.5 - ADDED VARS
         MOVE      "NCRC0002" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "CORRECTION SUMMARY" TO STITLE
         CALL       PAINT
         MOVE       "EXIT" TO PF5
         MOVE      "PRINT ONLY" TO PF4
         CALL       FUNCDISP
         TRAP       STOP IF F5
         MOVE      C1 TO NORDPATH
         MOVE      C1 TO NDATPATH
         CLOCK     DATE TO DATE
         MOVE      C2 TO Ncrcpath
;START PATCH 1.7 REPLACED LOGIC
.         SPLOPEN   "\\nts0\d\DATA\NCRC2.LST"
         PACK      STR35,NTWKPATH1,"NCRC2.LST"
         SPLOPEN   STR35
;END PATCH 1.7 REPLACED LOGIC
         PACK      WRKNAME FROM WRKNAM1,PORTN
;.begin patch 1.81
; begin patch 1.8
;        getinfo  system,str6
;        unpack   str6 into str1,str2
;        unpack   str2 into str1
;        move     c0 to osflag
                    call                GetWinVer
;..0 = unknown
;..1 = Windows NT
;..2 = WIN32s Windows 3.1x (obsolete)
;..3 = Window 95
;..4 = Window 98
;..5 = Windows 2000
;..8 = Windows CE
;        if       (str1 = "3" or str1 = "4")
;        move     c1 to osflag
;        endif
;        if       (str1 = "1" or str1 = "5")
;        move     c2 to osflag
;        endif
;.end patch 1.8
;.end patch 1.81
;
;
CHOICE   TRAP      PRINTO IF F4
         KEYIN    *P15:6,"How would you like to choose the records you want?:":
                   *P27:9,"by (T)ypist initials?":
                   *P30:10,"(R)evision Date?":
                   *P30:11,"(F)inished?":
                   *p30:12,"(P)rint existing file":
                   *P19:13,"Choose one: ",PICK;
         CMATCH    "F",PICK
         GOTO      CHOICE IF EOS
         STOP      IF EQUAL
;
         MOVE      PICK,ANS
         REPLACE   "T1R2F3P4",ANS
         MOVE      C0 TO NUMPICK
         MOVE      ANS,NUMPICK
         COMPARE   C0 TO NUMPICK
         GOTO      CHOICE IF EQUAL
         COMPARE   "5" TO NUMPICK
         GOTO      CHOICE IF NOT LESS
         MOVE      C0 TO N6
         PACK      WRKNAME FROM WRKNAM1,PORTN
         branch    numpick of prepfile,prepfile,choice,printo
prepfile PREPARE   WRKFILE,WRKNAME,WRKNAME,"11","18"
         BRANCH    NUMPICK OF TYPPICK,DATEPICK,choice,printo
;ADD ALL
;  TYPIST SELECTION ROUTINE
;           (UP TO FIFTEEN)
;
TYPPICK  DISPLAY   *P1:4,*EF,*P20:4," T Y P I S T   P I C K - O F F ":
            *P1:23,"Please enter the typist initials you would like to":
                   " choose (hit '*' if finished)":
                   *P55:24,"Writing to ",WRKNAME;
;
TYP      CLEAR     TYPPICK
         MOVE      "7",V
TYP1     KEYIN     *P21:V,*EL,"Typist Initials:",*JL,NUSEFLD;
         CMATCH    "*",NUSEFLD
         GOTO      TYP1 IF EOS
         GOTO      FINTYP IF EQUAL
         CALL      NUSEKEY
         MOVE      NUSEUSER TO STR24
         GOTO      TYP1OK IF NOT OVER
         DISPLAY   *P40:V,"Typist Initials not on file.",*W2;
         GOTO      TYP1
TYP1OK   KEYIN     *P38:V,"- ",*DV,STR24,*P61:V,"OK?",ANS;
         CMATCH    "N",ANS
         GOTO      TYP1 IF EQUAL
         GOTO      TYP1OK IF EOS
         CMATCH    "Y",ANS
         GOTO      TYP1OK IF NOT EQUAL
         SETLPTR    NUSEFLD TO 3
         APPEND    NUSEFLD,TYPPICK
TYP2     ADD      C1,V
TYP2B    KEYIN     *P31:V,*EL,*DV,"or",":",*JL,NUSEFLD
         CMATCH    "*",NUSEFLD
         GOTO      FINTYP IF EQUAL
         GOTO      TYP2B IF EOS
         MOVE      C2 TO NUSEPATH
         CALL      NUSEKEY
         GOTO      TYP2OK IF NOT OVER
         DISPLAY   *P40:V,"Typist Initials not on file.",*W2;
         GOTO      TYP2B
TYP2OK   KEYIN     *P38:V,"- ",*DV,STR24,*P61:V,"OK?",ANS;
         CMATCH    "N",ANS
         GOTO      TYP2B IF EQUAL
         GOTO      TYP2OK IF EOS
         CMATCH    "Y",ANS
         GOTO      TYP2OK IF NOT EQUAL
         SETLPTR    NUSEFLD TO 3
         APPEND    NUSEFLD,TYPPICK
         COMPARE   "21",V
         GOTO      TYP2 IF NOT EQUAL
;
FINTYP   MOVEFPTR  TYPPICK TO N3
         COMPARE   C0 TO N3                    ANY TYPEGORIES ENTRD?
         GOTO      CHOICE IF EQUAL               NO.
         GOTO      DATESEC
;
;  REVISE DATE SELECTION ROUTINE
;   CAN SELECT BY YEAR, MONTH/YEAR OR MONTH/DAY/YEAR.
;   OTHER PICK-OFFS WILL GO TO DATESEC TO SEE IF THE USER WANTS A
;    SECONDARY DATE PICK-OFF.
;
DATEPICK DISPLAY   *P1:4,*EF,*P17:4,"***R E V I S E   D A T E   P I C K":
                   " - O F F***";
         GOTO      DATECHCE
DATESEC  MOVE      C0 TO REVBRCH
         KEYIN    *P1:24,*EL,*P15:24,"Do you want a secondary search by ":
                   "DATE? ",ANS;
         CMATCH    "Y",ANS
         GOTO      DATESEC IF EOS
         GOTO      DATESEC2 IF EQUAL
         CMATCH    "N",ANS
         GOTO      DATESEC IF NOT EQUAL
         GOTO      INPUT
DATESEC2 DISPLAY   *P8:5,"***S E C O N D A R Y   R E V I S E   D A T E   ":
                   "P I C K - O F F***";
DATECHCE KEYIN    *P1:11,*EF,"How would you like to choose the revise dates:":
                   *P49:11,"1. by YEAR?":
                   *P49:12,"2. by MONTH/YEAR?":
                   *P49:13,"3. by MONTH/DAY/YEAR?":
                   *P71:13,"Choose: ",REVBRCH;
         MOVE     C1 TO REVCOUNT
         MOVE      "14",V
         BRANCH    REVBRCH OF YRSEL,MOSEL,DYSEL
         GOTO      DATECHCE
;
; SELECT BY YEAR... 4 POSITION YEAR FIELDS ARE APPENDED TO REVPICK.
;
YRSEL    CLEAR     REVPICK
         MOVE     C1 TO N1
YRROW    BRANCH    N1 OF YRROW1,YRROW2,YRROW3,YRROW4,ENDYR
YRROW1   MOVE      "03" TO H1
         MOVE      "16" TO H2
         GOTO      YRPICK
YRROW2   MOVE      "21" TO H1
         MOVE      "35" TO H2
         GOTO      YRPICK
YRROW3   MOVE      "40" TO H1
         MOVE      "54" TO H2
         GOTO      YRPICK
YRROW4   MOVE      "59" TO H1
         MOVE      "73" TO H2
YRPICK   KEYIN     *PH1:V,*DV,REVCOUNT,") yyyy",*ZF,*JR,YEAR;
         MATCH     "0*",YEAR
         GOTO      ENDYR IF EQUAL
         GOTO      YRPICK IF EOS
         TYPE      YEAR
         GOTO      YRPICK IF NOT EQUAL
YROK     KEYIN     *PH2:V,"OK?",ANS;
         CMATCH    "N",ANS
         GOTO      YRPICK IF EQUAL
         GOTO      YROK IF EOS
         CMATCH    "Y",ANS
         GOTO      YROK IF NOT EQUAL
         ADD      C1,REVCOUNT
         APPEND    YEAR,REVPICK
         ADD      C1,V
         COMPARE   "21",V
         GOTO      YRPICK IF NOT EQUAL
         MOVE      "14",V
         ADD      C1 TO N1
         GOTO      YRROW
ENDYR    MOVEFPTR  REVPICK TO N3
         COMPARE   C0 TO N3
         GOTO      CHOICE IF EQUAL
         GOTO      INPUT
;
; SELECT BY MONTH/YEAR... 6 POSITION MONTH/YEAR FIELDS ARE APPENDED
;                         TO REVPICK.
;
MOSEL    CLEAR     REVPICK
         MOVE     C1 TO N1
MOROW    BRANCH    N1 OF MOROW1,MOROW2,MOROW3,MOROW4,ENDMO
MOROW1   MOVE      "03" TO H1
         MOVE      "07" TO H2
         MOVE      "10" TO H3
         MOVE      "16" TO H4
         GOTO      MOPICK
MOROW2   MOVE      "21" TO H1
         MOVE      "25" TO H2
         MOVE      "28" TO H3
         MOVE      "35" TO H4
         GOTO      MOPICK
MOROW3   MOVE      "40" TO H1
         MOVE      "44" TO H2
         MOVE      "47" TO H3
         MOVE      "54" TO H4
         GOTO      MOPICK
MOROW4   MOVE      "59" TO H1
         MOVE      "63" TO H2
         MOVE      "66" TO H3
         MOVE      "73" TO H4
MOPICK   KEYIN     *PH1:V,*DV,REVCOUNT,")   /":
                   *PH2:V,*+,*ZF,*JR,MO,*PH3:V,*ZF,*JR,*-,YEAR;
         MATCH     "0*",MO
         GOTO      ENDMO IF EQUAL
         GOTO      MOPICK IF EOS
         TYPE      MO
         GOTO      MOPICK IF NOT EQUAL
         TYPE      YEAR
         GOTO      MOPICK IF EOS
         GOTO      MOPICK IF NOT EQUAL
MOOK     KEYIN     *PH4:V,"OK?",ANS;
         CMATCH    "N",ANS
         GOTO      MOPICK IF EQUAL
         GOTO      MOOK IF EOS
         CMATCH    "Y",ANS
         GOTO      MOOK IF NOT EQUAL
         ADD      C1,REVCOUNT
         APPEND    MO,REVPICK
         APPEND    YEAR,REVPICK
         ADD      C1,V
         COMPARE   "21",V
         GOTO      MOPICK IF NOT EQUAL
         MOVE      "14",V
         ADD      C1 TO N1
         GOTO      MOROW
ENDMO    MOVEFPTR  REVPICK TO N3
         COMPARE   C0 TO N3
         GOTO      CHOICE IF EQUAL
         GOTO      INPUT
;
; SELECT BY MONTH/DAY/YEAR...  10 POSITION MO/DY/YR FIELDS ARE APPENDED
;                              TO REVPICK & REVPICK2 IF NEEDED.
;
DYSEL    CLEAR     REVPICK
         CLEAR     REVPICK2
         MOVE     C1 TO N1
DYROW    BRANCH    N1 OF DYROW1,DYROW2,DYROW3,DYROW4,ENDDY
DYROW1   MOVE      "03" TO H1
         MOVE      "07" TO H2
         MOVE      "10" TO H3
         MOVE      "13" TO H4
         MOVE      "16" TO H5
         GOTO      DYPICK
DYROW2   MOVE      "21" TO H1
         MOVE      "25" TO H2
         MOVE      "28" TO H3
         MOVE      "31" TO H4
         MOVE      "35" TO H5
         GOTO      DYPICK
DYROW3   MOVE      "40" TO H1
         MOVE      "44" TO H2
         MOVE      "48" TO H3
         MOVE      "50" TO H4
         MOVE      "54" TO H5
         GOTO      DYPICK
DYROW4   MOVE      "59" TO H1
         MOVE      "63" TO H2
         MOVE      "66" TO H3
         MOVE      "69" TO H4
         MOVE      "73" TO H5
DYPICK   KEYIN     *PH1:V,*DV,REVCOUNT,")   /  /":
                   *PH2:V,*+,*ZF,*JR,MO,*PH3:V,*ZF,*JR:
                   DAY,*PH4:V,*ZF,*JR,*-,YEAR;
         MATCH     "0*",MO
         GOTO      ENDDY IF EQUAL
         GOTO      DYPICK IF EOS
         TYPE      MO
         GOTO      DYPICK IF NOT EQUAL
         TYPE      DAY
         GOTO      DYPICK IF NOT EQUAL
         TYPE      YEAR
         GOTO      DYPICK IF NOT EQUAL
DYOK     KEYIN     *PH5:V,"OK?",ANS;
         CMATCH    "N",ANS
         GOTO      DYPICK IF EQUAL
         GOTO      DYOK IF EOS
         CMATCH    "Y",ANS
         GOTO      DYOK IF NOT EQUAL
         PACK      DATE WITH MO,DAY,YEAR
         BRANCH    N1 TO DYAPP,DYAPP,DYAPP2,DYAPP2
DYAPP    APPEND    DATE TO REVPICK
         GOTO      DYCOUNT
DYAPP2   APPEND    DATE TO REVPICK2
DYCOUNT  ADD      C1,REVCOUNT
         ADD      C1,V
         COMPARE   "21",V
         GOTO      DYPICK IF NOT EQUAL
         MOVE      "14",V
         ADD      C1 TO N1
         GOTO      DYROW
ENDDY    MOVEFPTR  REVPICK TO N3
         COMPARE   C0 TO N3
         GOTO      CHOICE IF EQUAL
;
INPUT    branch    numpick to inputs,inputks
inputs   CALL      NCRCSEQ
         GOTO      PRINT IF OVER
         ADD       C1 TO N5
         DISPLAY   *P1:23,"RECORDS READ: ",N5;
         BRANCH    NUMPICK TO SETTYP,SETDATE
inputks  compare   c0 to n5
         if        equal
         clear     str8
         reset     revpick
;         DISPLAY   *P1:22,revpick,*w2
         append    revpick into str8      .get 1st date
         reset     str8
         unpack    str8 into mm,dd,str2,yy
         pack      ncrcfld2 from str2,yy,mm,dd
         call      ncrctst
         endif
         CALL      NCRCks
         GOTO      PRINT IF OVER
         ADD       C1 TO N5
         DISPLAY   *P1:23,"RECORDS READ KS: ",ncrcfld2,N5;
         BRANCH    NUMPICK TO SETTYP,SETDATE
;
SETTYP   RESET     TYPPICK
;
NEXTTYP  MOVE      TYPPICK,NUSEFLD                   MOVE TO 3 POS
         SCAN      NUSEFLD IN TYPS                   IS IT IN THE RECORD?
         GOTO      ENDTYP IF EQUAL               YES.
         BUMP      TYPPICK BY 3                  NO. MORE TO CHECK?
         GOTO      INPUT IF EOS                   NO.
         GOTO      NEXTTYP                            YES.
*
;
ENDTYP   BRANCH    REVBRCH TO SETYR,SETMO,SETDY  DATE CHECK?
         GOTO      WRTPREP
;
; BY DATE RETRIEVAL...
;
SETDATE  BRANCH    REVBRCH TO SETYR,SETMO,SETDY  MAJOR PICK-OFF BY DATE.
         GOTO      WRTPREP
*
; BY YEAR RETRIEVAL...
;
SETYR    RESET     REVPICK                       GET FP READY FOR SCAN.
         pack      str4 from ncrccc,ncrcyy
         SCAN      str4 IN REVPICK               IS IT IN THE STRING?
         GOTO      WRTPREP IF EQUAL             YES.
         GOTO      INPUT                      NO.
*
; BY MONTH RETRIEVAL...
;
SETMO    RESET     REVPICK                       GET FP READY FOR SCAN.
         PACK      MOYR WITH NCRCMM,ncrccc,NCRCYY             SET UP SEARCH STRING.
         SCAN      MOYR IN REVPICK               IS IT IN THE STRING?
         GOTO      WRTPREP IF EQUAL             YES.
         GOTO      INPUT                      NO.
*
; BY DAY RETRIEVAL...
;
SETDY    RESET     REVPICK                       GET FP READY FOR SCAN.
         RESET     REVPICK2                               "
         PACK      REVDATE FROM NCRCMM,NCRCDD,ncrccc,NCRCYY
;         DISPLAY   *P1:20,revdate,b1,revpick,*w2
         SCAN      REVDATE IN REVPICK            IS IT IN 1ST STRING?
         GOTO      WRTPREP IF EQUAL             YES.
         SCAN      REVDATE IN REVPICK2           NO. HOWBOUT 2ND STRING?
         GOTO      WRTPREP IF EQUAL                 YES.
         GOTO      INPUT                          NO.
*
;
WRTPREP  PACK      WRKFLD FROM NCRCTYP,ncrccc,NCRCYY,NCRCMM,NCRCDD
         CALL      WRTWRK
         ADD       C1 TO N6
         DISPLAY   *P25:23,"RECORDS SELECTED ",N6;
         GOTO      INPUT
;.............................................................................
WRTWRK   WRITE     WRKFILE,WRKFLD;ncrcvars 
         RETURN
PRINT    
         CLOSE     WRKFILE,eofsize
         pause     "2"
PRINTO   OPEN      WRKFILE,WRKNAME
         PRINT     HPLAND,hpptch,hplin8,hptop
         MOVE      C0 TO N6
         MOVE      C0 TO N5
PINPUT   READKS    WRKFILE;NCRCvars
         GOTO      EOJ2 IF OVER
         ADD       C1 TO N6
         DISPLAY   *P12:14,"RECORDS PRINTED ",N6
BREAK    COMPARE   C1 TO N6
         IF        EQUAL
         CALL      HD0
         MOVE      NCRCTYP TO B3
         ENDIF
         MATCH     B3 TO NCRCTYP
         IF        NOT EQUAL
         CALL      TL0
;         MOVE      C1 TO N5           .1ST RECORD FOR NEW TYPIST
         MOVE      C0 TO PAGE
         MOVE      NCRCTYP TO B3
         CALL      HD0
         ENDIF
;START PATCH 1.5 - REPLACED LOGIC
;         clear     PRSPCL1
;         clear     PRSPCL2
;         clear     PRSPCL3
;         clear     PRSPCL4
;         clear     PRSPCL5
;         clear     PRSPCL6
;         CLEAR     DESC0L1
;         CLEAR     DESC0L2
;         CLEAR     DESC991
;         CLEAR     DESC992
;         CLEAR     DESC981
;         CLEAR     DESC982
;         MOVE      NCRCKEY TO NordFLD
;         CALL      Nordkey
;         MOVE      NCRCKEY TO NSPEFLD
;         CALL      NSPEKEY
;.START PATCH #1.4 - REPLACED LOGIC
;.         UNPACK    OSPI INTO SPCL1,SPCL2,SPCL3,SPCL4,SPCL5,SPCL6
;         UNPACK    OSPI INTO STR1,SPCL1,STR1,SPCL2,STR1,SPCL3,STR1,SPCL4,STR1,SPCL5,STR1,SPCL6
;.END PATCH #1.4 - REPLACED LOGIC
;         MOVE      SPCL1 TO SPCL
;         CALL      SPCLNSTO                           SPEC INSTRUC ROUTINE
;         MOVE      SPCL2 TO SPCL
;         CALL      SPCLNSTO
;         MOVE      SPCL3 TO SPCL
;         CALL      SPCLNSTO
;         MOVE      SPCL4 TO SPCL
;         CALL      SPCLNSTO
;         MOVE      SPCL5 TO SPCL
;         CALL      SPCLNSTO
;         MOVE      SPCL6 TO SPCL
;         CALL      SPCLNSTO
;         goto      printdet
;.
;. ROUTINE FOR SPECIAL INSTRUCTION PRINT
;.
;SPCLNSTO MATCH     "00" TO SPCL
;         GOTO      PRTSPL0O IF EQUAL
;         MATCH     "99",SPCL
;         GOTO      PRTSPL9O IF EQUAL
;         MATCH     "98" TO SPCL
;         GOTO      PRTSPL98 IF EQUAL
;PRTSPL0O MOVE      "00",SPCL
;         move      desc0l1 to  prspcl1
;         move      desc0l2 to  prspcl2
;         RETURN
;.
;PRTSPL9O MOVE      "99",SPCL
;         move      desc991 to  prspcl3
;         move      desc992 to  prspcl4
;         RETURN
;.
;PRTSPL98 MOVE      "98",SPCL
;         move      desc981 to  prspcl5
;         move      desc982 to  prspcl6
;         RETURN
;............................................
         clear     holdstr
         clear     line1
         clear     line2
         clear     line3
         clear     line4
         clear     line5
         MOVE      NCRCKEY TO NordFLD
         CALL      Nordkey
         MOVE      NCRCKEY TO NSPEFLD
         MOVE      "BREAK-NSPEKEY",Location
         CALL      NSPEKEY
         pack      holdstr,DESC001,DESC002
         call      Trim using holdstr
         GOTO      PRINTDET
;END PATCH 1.5 - REPLACED LOGIC
;
printdet
;         MOVE      NCRCKEY TO NSPEFLD
;         CALL      NSPEKEY
         MOVE      OLNUM TO NDATFLD
         CALL      NDATKEY
        PACK      MKEY FROM OMLRNUM,OCOBN
       CALL      NMLRKEY
;START PATCH 1.5 - REPLACED LOGIC
;DTL0     COMPARE   "48" TO LINES
;         CALL      HD0 IF NOT LESS
;         PRINT     *L,*1,NCRCKEY,*10,MCOMP,*58,NCRCMM,SLASH,NCRCDD,SLASH:
;                   ncrccc,NCRCYY,*70,OLNUM,B1,MLSTNAME,*142,OMLRKY,*156,OMLRPON:
;                   *L,*1,prspcl1,*70,prspcl3,*120,prspcl5:
;                   *L,*1,prspcl2,*70,prspcl4,*120,prspcl6
;         ADD       C3 TO LINES
;
;         ADD       C1 TO N5
;         GOTO      PINPUT
;TL0      COMPARE   "48" TO LINES
DTL0     COMPARE   "57" TO LINES
         CALL      HD0 IF NOT LESS
;Throw away carriage return characters.  Do not need them for this situation.
         append    carr,carrfill
         append    " ",carrfill
         reset     carrfill
         rep       carrfill,holdstr
         call      PARSITUP using line1,holdstr,C1
         call      Trim using line1
         call      PARSITUP using line2,holdstr,C1
         call      Trim using line2
         call      PARSITUP using line3,holdstr,C1
         call      Trim using line3
         call      PARSITUP using line4,holdstr,C1
         call      Trim using line4
         call      PARSITUP using line5,holdstr,C1
         call      Trim using line5
         PRINT     *L,*1,NCRCKEY,*10,MCOMP,*58,NCRCMM,SLASH,NCRCDD,SLASH:
                   ncrccc,NCRCYY,*70,OLNUM,B1,MLSTNAME,*142,OMLRKY,*156,OMLRPON
         ADD       C2 TO LINES
         if (line1 <> "")
                   PRINT        *1,line1
                   add          C1,LINES
         endif
         if (line2 <> "")
                   PRINT        *1,line2
                   add          C1,LINES
         endif
         if (line3 <> "")
                   PRINT        *1,line3
                   add          C1,LINES
         endif
         if (line4 <> "")
                   PRINT        *1,line4
                   add          C1,LINES
         endif
         if (line5 <> "")
                   PRINT        *1,line5
                   add          C1,LINES
         endif
         ADD       C1 TO N5
         GOTO      PINPUT
TL0      COMPARE   "57" TO LINES
;END PATCH 1.5 - REPLACED LOGIC
         CALL      HD0 IF NOT LESS
         PRINT     *L,*1,"TOTAL CORRECTIONS FOR: ",B3,B2,N5
         MOVE      C0 TO N5
         RETURN
HD0      ADD       C1 TO PAGE
         PRINT     *F,*L,*L,*55,HPBON,"CORRECTION SUMMARY",HPBOFF:
                   *125,DATE,*156,"PAGE: ",page:
                   *L,*1,HPLN3:
                   *L,*1,"ORDER REVISIONS BY : ",NCRCTYP:
                   *L,*1,"LR##",*10,"MAILER",*60,"DATE",*70,"LIST":
                   B2,"LIST DESCRIPTION",*142,"MP##",*156,"MLR PO##"
         MOVE      C7 TO LINES
         RETURN
;
EOJ2     CALL      TL0
         PRINT     HPPORT,hp10ptch,HPRESET
;
STOP      SPLCLOSE
;         execute    "c:\command.com /c copy \\nins1\d\DATA\NCRC2.LST \\NINS2\laser8"
;begin patch 1.8
;         path      exist,"c:\windows"
;         if        over
;begin patch 1.81
;        if        (osflag = c2)
                    If                  (OsFlag = C1 | OsFlag = C5)         .NT
;end patch 1.81
;end patch 1.8
;START PATCH 1.7 REPLACED LOGIC
;         Execute   "c:\winnt\system32\cmd.exe /c copy \\nts0\d\DATA\ncrc2.LST \\nts0\laser8 "
;         else
;         EXECUTE   "c:\command.com /c copy \\nts0\d\DATA\ncrc2.LST \\nts0\ncrc2.lst "
         PACK      TASKNAME,"c:\winnt\system32\cmd.exe /c copy ",NTWKPATH1,"ncrc2.LST \\NINS2\laser8 "
         Execute   TASKNAME
;begin patch 1.81
;         else
                    Elseif              (OsFlag = C3 | OSFlag = C4) WIn9X
;end patch 1.81
                    
         PACK      TASKNAME,"c:\command.com /c copy ",NTWKPATH1,"ncrc2.LST \\NINS2\laser8 "
         EXECUTE   TASKNAME
;begin patch 1.81
                    Elseif              (OsFlag = C6)          ;windows XP
         PACK      TASKNAME,"c:\windows\system32\cmd.exe /c copy ",NTWKPATH1,"ncrc2.LST \\NINS2\laser8 "
         EXECUTE   TASKNAME
;end patch 1.81
;END PATCH 1.7 REPLACED LOGIC
         endif
        STOP
;       
         INCLUDE   NCRCIO.inc
.START PATCH 1.82 REPLACED LOGIC
.         INCLUDE   NMLRIO.inc
          INCLUDE   COMPIO.inc
          INCLUDE   CNTIO.inc
.END PATCH 1.82 REPLACED LOGIC
         INCLUDE   NDATIO.inc
         INCLUDE   NORDIO.inc
         INCLUDE   NSPEIO.inc
         INCLUDE   NUSEIO.inc
         INCLUDE   COMLOGIC.inc

