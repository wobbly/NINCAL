PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         include   hp.inc
         INCLUDE   NDATDD.inc
;Patch1.9
                              include compdd.inc
                              include   cntdd.inc
.         INCLUDE   NMLRDD.inc
;Patch1.9
          include   nmdldd.inc
          INCLUDE   NXRFDD.inc
          include   Ntxtdd.inc
          include   Nseldd.inc
          include   NModdd.inc
.       
. NIN MAILDATE LIST INSTRUCTION FILE VARIABLES.
.

Release   Init      "2.1"     DLH - someone had un commented several old print statements causing spool errors
.                                       added check for OS at the same time
REldate   Init      "22 July 2009"

.Release   Init      "2.0"     DLH REwrite
.REldate   Init      "17 June 2008"
.RELEASE  INIT      "1.9"       DMB 26MAY2004 Mailer Conversion
.RELEASE  INIT      "1.8"       ASH 02OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "1.7"       DLH  8Jan98 Added print of EXClusivety
.RELEASE  INIT     "1.6"       jd   1May97  added parse.text1
.RELEASE  INIT     "1.5"       jd   12May94    FIXED SPOOLING.
.RELEASE  INIT     "1.4"       DLH   06May94   LASER
.RELEASE  INIT     "1.3"       DLH   20APR93   ALLOW USE OF MAILER IF NO
.                              LIST. SEE NSCH0001.DBS
.RELEASE  INIT     "1.2"       DLH   28FEB92   ADD LISTMLR NDATXX INCLUDES.
.
.RELEASE  INIT      "1.1"
LIST     DIM       6           KEY
DESC1    DIM       70
DESC2    DIM       68
SHEDCODE DIM       1
LCRCODE  DIM       1
PLANBR   DIM       3
EXCL     DIM       4
DESC3    DIM       70
DESC4    DIM       70
callbr   dim       3
. FILES.
. ......
.NINMDLST IFILE     KEYLEN=6,FIXED=292
.558 + 35 + ????
OUTPUT    IFILE     KEYLEN=35,COMP,var=600,DUPLICATES
OUTPUT2   IFILE     KEYLEN=38,COMP,,var=600,DUPLICATES
OUTPUT3   IFILE     KEYLEN=38,COMP,var=600,DUPLICATES
.
.output file 3 description

.LIST     DIM       6           001-006
.olstname dim      35           007-041
.UNIV     DIM      11           042-052
.PRICE    DIM       6           053-058
.DESC1    DIM       70          059-128
.DESC2    DIM       68          129-196
.DESC3    DIM       70          197-266
.DESC4    DIM       70          267-336
.SHEDCODE DIM       1           337-337
.LCRCODE  DIM       1           338-338
.PLANNER  DIM       3           339-341
.CALLER   DIM       3           342-344
.STATUS   DIM       1           345-345  'W' FOR WITHDRAWN.
.ELSTCDE  DIM       1           346-346  EXCLUSIVE CODE (N, C or  ).
.
.
. OTHER PROGRAM VARIABLES.
. ........................
TEXT1    DIM       47         556-602  FREE TEXT.  **NOTE: EACH LINE OF TEXT
DIM13a    DIM       13
dim9a     dim       13
TOTUNIV  FORM      10
MASK13    INIT      "Z,ZZZ,ZZZ,ZZZ"
ANS      DIM       1
PAGE     FORM      4
ONE      FORM      "1"
THREE    FORM      "3"
ZERO     FORM      "0"
UNIV     DIM       11
UNIVMASK INIT      "ZZZ,ZZZ,ZZ9"
PRICE    DIM       6
FORM9    FORM      9
FIVE     FORM      "5"
LINES    FORM      3
TIPE     FORM      1
NUM      FORM      1             BRANCHING CONSTANT
COUNT    FORM      5             NUMBER OF RECORDS PROCESSED
TOTLINE  FORM      "65"          NUMBER OF LINES PER PAGE
FILE     FORM      1             BRANCHING CONSTANT FOR IO ERRORS.
DATE     DIM       8             FOR OUTPUT
COPY     FORM      2
KEY38    DIM       38
SHEDINFO DIM       10            IF SHEDCODE = 'N', PRINT "NO BOOKING"
LCRINFO  DIM       10            IF LCRCODE = 'N', PRINT "DON'T LCR".
EXONLY   INIT      "EXCHANGE ONLY"
EXCH     INIT      "EXCH"
EX       INIT      "EX"
DOLLAR   INIT      "$"
W        INIT      "W"
STAT     DIM       7
T        INIT      "T"
REPTYPE  FORM      1
.....................................
Laser     pfile
Header1   form    9
font8    font
Row1      Form      9
row2      FOrm      9
EXTERNALFLAG        dIM       1
. MAIN
. .............................................................................
.
          GOTO      START
ExtPRint  Routine
.          IF ENTERERED AS ROUTINE SET FLAG
          MOVE      YES,EXTERNALFLAG
START    TRAP      EXIT IF F5
         MOVE      "EXIT" TO PF5
         MOVE      "NMDL0002" TO PROGRAM
         MOVE      "Names In The News" TO COMPNME
         MOVE      "Booking INSTRUCTIONS Print" TO STITLE
         MOVE      C1 TO NXRFPATH         .SET ACCESS TO LIST KEY.
         MOVE      ONE TO FILE
         TRAP      IO IF IO
        create  font8,"Times New Roman",size=8
        move    "200",column
          move    "2250",column1
          MOve      "3000",Header1
        move    "7000",column2
        move    "7500",column3

         CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO MM,DD,YY
         PACK      DATE FROM MM,SLASH,DD,SLASH,YY
         XIF
         MOVE      DATE TO TODAY
         CALL      PAINT
         CALL      FUNCDISP
         ADD       ONE TO FILE
         MOVE      C1 TO NDATPATH          .SET ACCESS TO ISAM KEY.
MERGE    ADD       ONE TO FILE
NEWFILE
         move      "L" to ans
         KEYIN     *P20:07,"BY (L)IST NAME/(P)LANNER/(C)ALLER",*uc,*t60,*rv,ANS
         REP       "L1P2C3" IN ANS
         MOVE      ANS TO REPTYPE
         BRANCH    REPTYPE OF PEP1,PEP2,PEP3
PEP1    close     output
        PACK   STR35,NTWKPATH1,"MDLSTOUT"
        PACK   STR45,NTWKPATH1,"MDLSTOUT"
        PREPARE   OUTPUT,STR35,STR45,"35","600"
         ADD       ONE TO TIPE
         add       c1 to file
         GOTO      READ
PEP2     close     output2
         PACK   STR35,NTWKPATH1,"MDLST2"
         PACK   STR45,NTWKPATH1,"MDLST2"
         PREPARE   OUTPUT2,STR35,STR45,"38","600"
         add       c1 to file
         GOTO      READ
PEP3     close     output3
         PACK   STR35,NTWKPATH1,"MDLST3"
         PACK   STR45,NTWKPATH1,"MDLST3"
         PREPARE   OUTPUT3,STR35,STR45,"38","600"
         add       c1 to file
READ
          call      NmdlSeq
          GOTO      PREPPRNT IF OVER
          SCAN      "M" IN Mdlkey
          IF        EQUAL
          BUMP      MdlKey 1
          pack      MKEY with MDLKey,z3
          RESET     MdlKey
          MOVE      C0 TO FORM9
          EDIT      FORM9 TO UNIV
          CLEAR     PRICE
          move      zero  to univ
          rep       zfill in mkey
          CALL      NMLRKEY
                    IF        OVER
                    CALL      NONAME 
                    ELSE
                    MOVE      MCOMP TO OLSTNAME
                    ENDIF

          GOTO      WRITE
          ELSE
          RESET     MDLKey
          MOVE      MDLKey TO NDATFLD
          CALL      NDATKEY
          GOto      WRite
NONAME    MOVE      "   LIST DESCRIPTION MISSING" TO OLSTNAME
          RETURN
WRITE    BRANCH    REPTYPE OF WRITE1,WRITE2,WRITE3
WRITE1
.         COMPARE   ONE TO TIPE
.         GOTO      WRITE2 IF NOT EQUAL
.         WRITE     OUTPUT,OLSTNAME;*+,LIST,OLSTNAME,UNIV,PRICE:
.                   DESC1,DESC2,DESC3,DESC4:
.                   SHEDCODE,LCRCODE,PLANNER,CALLER,STATUS,elstcde
          Write     Output,Olstname;MDLVARS,Olstname,Univ,PRice,status,Elstcde
         ADD       ONE TO COUNT
         DISPLAY   *P10:12,*EL,*HON,"NUMBER OF RECORDS WRITTEN = ",COUNT;
         GOTO      READ
WRITE2
         PACK      KEY38 FROM MDLPLAN,OLSTNAME
          Write     Output2,Key38;MDLVARS,Olstname,Univ,PRice,status,Elstcde
.         WRITE     OUTPUT2,KEY38;*+,LIST,OLSTNAME,UNIV,PRICE:
.                   DESC1,DESC2,DESC3,DESC4:
.                   SHEDCODE,LCRCODE,PLANNER,CALLER,STATUS,elstcde
         ADD       ONE TO COUNT
         DISPLAY   *P10:12,*EL,*HON,"NUMBER OF RECORDS WRITTEN = ",COUNT;
         GOTO      READ
WRITE3
         PACK      KEY38 FROM MDLCALL,OLSTNAME
          Write     Output3,Key38;MDLVARS,Olstname,Univ,PRice,status,Elstcde
.         WRITE     OUTPUT3,KEY38;*+,LIST,OLSTNAME,UNIV,PRICE:
.                   DESC1,DESC2,DESC3,DESC4:
.                   SHEDCODE,LCRCODE,PLANNER,CALLER,STATUS,elstcde
         ADD       ONE TO COUNT
         DISPLAY   *P10:12,*EL,*HON,"NUMBER OF RECORDS WRITTEN = ",COUNT;
         GOTO       READ
PREPPRNT
.         branch     reptype to sopen,sopen,sopen
.         KEYIN     *P20:07,"BY (L)IST NAME/(P)LANNER/(C)ALLER",*uc,ANS
.         REP       "L1P2C3" IN ANS
.         MOVE      ANS TO REPTYPE
.         goto      out
.sopen
.         add       c1 to file
.         close     ninmdlst
.         OPEN      NINMDLST,"NINMDLST",SHARE
.         move      c4 to file
out
.START PATCH 1.8 REPLACED LOGIC
.         SPLOPEN    "g:\data\MDLSTOUT.lst"
         PACK   STR35,NTWKPATH1,"MDLSTOUT.lst"
          call      GetWinVer
          Call      PDF995Auto
          call      SetPDFFlag
                              PRTOPEN Laser,"PDF995","MDLSTOUT.lst"

.                    if (osflag = c1 | osflag = C5 | osflag = c6)         .nt 2k xp
.                              PRTOPEN Laser,"\\NINs2\Laser3 Blankstock","MDLSTOUT.lst"
.                    elseif (osflag = c3 | osflag =c4)         .win 95 98
.                              PRTOPEN Laser,"Laser3 Blankstock","MDLSTOUT.lst"
.                    else   .(osflag = c0)         .Don't know prompt for printer
.                              PRTOPEN Laser,"-","MDLSTOUT.lst"
.                    endif

         BRANCH    REPTYPE OF OUT1,OUT2,OUT3
OUT1
.        COMPARE   ONE TO TIPE
.         GOTO      OUT2 IF NOT EQUAL
.         FLUSH     OUTPUT
         TRAP      OUT1A IF IO
         CLOSE     OUTPUT
         trapclr   io
out1a    OPEN      OUTPUT,"MDLSTOUT",read
         GOTO      PRINT
OUT2
         TRAP      OUT2A IF IO
.         FLUSH     OUTPUT2
         CLOSE     OUTPUT2
OUT2A    OPEN      OUTPUT2,"MDLST2",EXCLUSIVE
         GOTO      PRINT
OUT3
         TRAP      OUT3A IF IO
.         FLUSH     OUTPUT2
         CLOSE     OUTPUT3
OUT3A    OPEN      OUTPUT3,"MDLST3",READ
         GOTO      PRINT

PRINT    MOVE      "00000" TO COUNT
MORECOPY CALL      HEADER
         CLEAR     OLSTNAME
         MOVE      " " TO OLSTNAME
READOUT
         CLEAR     SHEDINFO
         CLEAR     LCRINFO
         
         BRANCH    REPTYPE OF READ1,READ2,READ3
READ1
.    COMPARE   ONE TO TIPE
.          GOTO      READ2 IF NOT EQUAL
.         READKS    OUTPUT;list,OLSTNAME,UNIV,PRICE:
.                   DESC1,DESC2,DESC3,DESC4,SHEDCODE,LCRCODE:
.                   PLANNER,CALLER,STATUS,elstcde
          ReadKS    Output;MDLVARS,Olstname,Univ,PRice,status,Elstcde
         GOTO      TOTAL IF OVER
         MOVE      mdlkey TO NXRFFLD
         CALL      NXRFKEY
         if        over
         move      b6 to nxrfmlr
         endif
         ADD       ONE TO COUNT
         GOTO      CODEE
READ2
          ReadKS    Output2;MDLVARS,Olstname,Univ,PRice,status,Elstcde
.         READKS    OUTPUT2;LIST,OLSTNAME,UNIV,PRICE:
.                   DESC1,DESC2,DESC3,DESC4,SHEDCODE,LCRCODE:
.                   PLANNER,CALLER,STATUS,elstcde
         GOTO      TOTAL IF OVER
         MOVE      mdlkey TO NXRFFLD
         CALL      NXRFKEY
         if        over
         move      b6 to nxrfmlr
         endif
         ADD       ONE TO COUNT
         MATCH     MDLPLAN TO PLANBR
         CALL      PLANBR IF NOT EQUAL
         GOTO      CODEE
READ3
.         READKS    OUTPUT3;caller,OLSTNAME,UNIV,PRICE:
          ReadKS    Output3;MDLVARS,Olstname,Univ,PRice,status,Elstcde
.         READKS    OUTPUT3;list,OLSTNAME,UNIV,PRICE:
.                   DESC1,DESC2,DESC3,DESC4,SHEDCODE,LCRCODE:
.                   PLANNER,CALLER,STATUS,elstcde
         GOTO      TOTAL IF OVER
         MOVE      mdlkey TO NXRFFLD
         CALL      NXRFKEY
         if        over
         move      b6 to nxrfmlr
         endif
         ADD       ONE TO COUNT
         MATCH     MDLcall TO callBR
         CALL      callBR IF NOT EQUAL
         GOTO      CODEE
.
CODEE    
         MOVE      MDLKey TO NDATFLD
         CALL      NDATKEY
         CALL      NONAME IF OVER
         ENDIF
         MOVE      UNIVMASK TO UNIV
         MOVE      ZERO TO FORM9
         MOVE      UNIVERSE TO FORM9
         EDIT      FORM9 TO UNIV
          clear     TEXT1
          if (NDATCONV = "1")
                    move      C1,NDATPATH
                    pack      NDATFLD,LSTNUM
                    move      "NDATKEY",Location
                    pack      KeyLocation,NDATFLD
                    call      NDATKEY
.                   if (NDATEXCH <> "1")
                              pack      NSELFLD1,"01X",LSTNUM
                              pack      NSELFLD2,"02XBASE"
                              move      "NSELAIM",Location
                              pack      KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD2
                              call      NSELAIM
                              if not over
.Patch3.6
                                        clear dim13a
                                        clear n10
                                        move      mask13 to dim13a
                                        move      nselqty to n10
                   ADD   N10 TO TOTUNIV
                                        edit      n10 to dim13a
                                        call trim using dim13a
                                        call trim using nselsname
                                        clear str30
                                        packkey str30 with nselsname
                                        uppercase  str30
                                        move nselprice to dim9a
                                        call trim using dim9a
                                        CALL SelectLoadModifier
.                                       call trim using
                                        if (NSELEXC <> "2")
.patch3.6
                                                  pack      TEXT1,dim13a,B1,str30,"$",str25
.                                                 pack      TEXT1,NSELQTY,B1,NSELSNAME,B1,"$",NSELPRICE
                                        else
                                                  pack      TEXT1,dim13a,B1,str30,"EXCH ONLY"
.                                                 pack      TEXT1,NSELQTY,B1,NSELSNAME,B1,"EXCHANGE ONLY"
                                        endif
.patch3.6
                              else
                                        goto DataCheckText
                              endif
.                   endif
          else
DataCheckText
                    pack      NTXTFLD,LSTNUM,"1"
                    move      "NTXTKEY",Location
                    pack      KeyLocation,"Key: ",NTXTFLD
                    call      NTXTKEY
                    if not over
                              call      PARSITUP using text1,ntxttext,C1
.                             move      NTXTTEXT,text1
                    endif
          endif
.         parse     textdata into text1 using " ~09",noskip,blankfill
         SCAN      EXONLY IN TEXT1
         GOTO      EXONLY IF EQUAL
         SCAN      EXCH IN TEXT1
         GOTO      EXRENT IF EQUAL
         SCAN      DOLLAR IN TEXT1
         GOTO      QUES IF NOT EQUAL
         MOVE      TEXT1 TO PRICE
         GOTO      CodeF
EXONLY   MOVE      EXCH TO PRICE
         GOTO      CodeF
EXRENT   SCAN      DOLLAR IN TEXT1
         PACK      PRICE FROM EX,SLASH,TEXT1
         GOTO      CodeF
QUES     MOVE      "????" TO PRICE

CodeF


.         CMATCH    "N" TO SHEDCODE
.         CALL      NOSHED IF EQUAL
         CMATCH    "N" TO mdlLCRCD
         CALL      NOLCR IF EQUAL
.         COMPARE   TOTLINE TO LINES
.         CALL      HEADER IF NOT LESS
          COMPARE  "9900" to ROW
          call      header if not less
         ADD       C4 TO LINES
         CLEAR     STAT
         CMATCH    W TO STATUS
         CALL      WITH IF EQUAL
         CMATCH    T TO STATUS
         CALL      TEMP IF EQUAL
         Clear     EXCL
         Cmatch    "C" to Elstcde
         call      EXCL if Equal
          prtpage Laser;*pcolumn:row,*ALIGNMENT=*LEFT,*font=font8,*ll,Olstname;                 
          prtpage Laser;*pcolumn2:row,*ALIGNMENT=*LEFT,*font=font8,*ll,mdlPLAN;                 
          prtpage Laser;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=font8,*ll,mdlCALL;                 
          prtpage Laser;*pcolumn:row1,*ALIGNMENT=*LEFT,*font=font8,*ll,Mdlkey;                  
          prtpage Laser;*pcolumn2:row1,*ALIGNMENT=*LEFT,*font=font8,*ll,Stat;                   
          prtpage Laser;*pcolumn3:row1,*ALIGNMENT=*LEFT,*font=font8,*ll,PRice;                  
          prtpage Laser;*pcolumn:row2,*ALIGNMENT=*LEFT,*font=font8,*ll,NXRFmlr;                 
          prtpage Laser;*pcolumn3:row2,*ALIGNMENT=*LEFT,*font=font8,*ll,LCRInfo;                          

          if (mdltext <> "")

                    reset     mdltext
                    call      Trim using mdltext
                    movelptr mdltext to n7
                    loop
                              clear     DESC1
                              call      PARSITUP using Desc1,mdltext,C1
                              movefptr mdltext to n6
                    until (n6 >= n7)
                              if (ROW   >= 9900)
                              Call      Header
                              endif
                              if        (desc1 <> "")
                              prtpage   Laser;*pColumn1:ROW,*ALIGNMENT=*left,*ll,DESC1;
                              add     eightlpi,row          
                              endif
                    Repeat
          
          Endif               
          if        (row < row2)
                    Loop
                    UNtil     (Row > Row2+270)
                    add     eightlpi,row          
                    Repeat
          Else                          
          add     eightlpi,row          
          add     eightlpi,row          
          endif
          Move      Row,Row1
          add       "135",row1
          Move      Row1,Row2
          add       "135",row2

.         PRINT     *L,*N,LIST,*8,OLSTNAME,*43,DESC1,*115,PLANNER,*124,CALLER:
.                   *126,STAT,*126,STAT:
.                   *L,*1,"MLR## ",NXRFMLR,B1,SHEDINFO,*22,LCRINFO:
.                   *43,DESC2,*112,UNIV," ",PRICE," ",Hpbon,excl,Hpboff;
.         MATCH    B10 TO DESC3
.         goto     desc4 if equal
.         GOTO      DESC4 IF EOS
.         PRINT     *N,*43,DESC3;
.         add       c1 to lines
.DESC4    MATCH    B10 TO DESC4
.         goto     readout if equal
.         GOTO      READOUT IF EOS
.         PRINT     *N,*43,DESC4;
.         add       c1 to lines
         GOTO      READOUT
WITH     MOVE      "WITHDRN" TO STAT
         RETURN
TEMP     MOVE      "TEMPWTH" TO STAT
         RETURN
EXCL     Move      "Excl" to EXCL
         return         
NOSHED   MOVE      "NO BOOKING" TO SHEDINFO
         RETURN
NOLCR    MOVE      "DON'T LCR" TO LCRINFO
         RETURN
PLANBR
         MOVE      MDLPLAN TO PLANBR
         COMPARE   ONE TO COUNT
         CALL      HEADER IF NOT EQUAL
         RETURN
callBR
         MOVE      MDLcall TO callBR
         COMPARE   ONE TO COUNT
         CALL      HEADER IF NOT EQUAL
         RETURN
.
HEADER   MOVE      C6 TO LINES
          Move      "200",row
         ADD       ONE TO PAGE
         compare   c1 to page
         if        equal
.         PRINT     hptmsr17,hpdupl,hptop,*F                .compressed
          prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*Duplex=2
          Else
          prtpage   Laser;*NEWPAGE
         endif

.         PRINT     *F,*N,*15,"* * *   M A S T E R    M A I L D A T E    ":
.                   "S C H E D U L I N G    I N S T R U C T I O N S   ":
.                   "* * *",*116,"PAGE : ",PAGE,*L:
.                   *116,"DATE : ",DATE:
.                   *L,*1,"LIST##",*8,"LIST NAME",*43,"INSTRUCTIONS":
.                   *112,"PLANNER",*122,"CALLER":
.                   *L,*1,"------",*8,"--------------------";
.         PRINT     "---------";
.         PRINT     "-----",*43,"-------------------------------------";
.         PRINT     "-----------------------------":
.                   *112,"------",*122,"------":
.                   *N
          clear     row
          move      "200",row
          prtpage Laser;*pColumn:row,*ALIGNMENT=*Left,*font=font8,"Confidential";
          clock   timestamp,str8
          unpack  str8,str2,yy,mm,dd
          clear   str10
          pack    str10,mm,slash,dd,slash,str2,yy
          prtpage Laser;*p7050:row,*ALIGNMENT=*LEFT,*font=font8,"Date:",str10;        
          if        (reptype = c1)
          prtpage Laser;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,"Master Booking Instructions By List Name";      
          Elseif    (reptype = c2)
          prtpage Laser;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,"Master Booking Instructions By Planner";        
          Elseif    (reptype = c3)
          prtpage Laser;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,"Master Booking Instructions by Caller";         
          endif
          add     eightlpi,row        
          add     eightlpi,row        
          prtpage Laser;*p7400:row,*ALIGNMENT=*Left,*font=font8,*ll,"Page:",Page;                 
          add     eightlpi,row        
          add     eightlpi,row  
          prtpage Laser;*pcolumn:row,*ALIGNMENT=*LEFT,*font=font8,*ll,"List Name ";                       
          prtpage Laser;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=font8,*ll,"List Instructions ";                        
          prtpage Laser;*pcolumn2:row,*ALIGNMENT=*LEFT,*font=font8,*ll,"Planner";                         
          prtpage Laser;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=font8,*ll,"Caller";                          
        add     eightlpi,row  
          prtpage Laser;*pcolumn:row,*ALIGNMENT=*LEFT,*font=font8,*ll,"List Number ";                     
        add     eightlpi,row          
          prtpage Laser;*pcolumn:row,*ALIGNMENT=*LEFT,*font=font8,*ll,"Mailer Number ";                   
        add     eightlpi,row          
        add     eightlpi,row          
        add     eightlpi,row          
        add     eightlpi,row          
          Move      Row,Row1
          add       "135",row1
          Move      Row1,Row2
          add       "135",row2
         RETURN

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

IO       TRAPCLR   IO
         NORETURN
IO1      BRANCH    FILE OF ONE,TWO,THREE,FOUR
         DISPLAY   *P1:24,*EL,*B,*HON,"UNKOWN I/O ERROR";
         GOTO      IOEXIT
ONE      DISPLAY   *P1:24,*EL,*B,*HON,"ARCCLOCK FILE ERROR";
         GOTO      IOEXIT
TWO      DISPLAY   *P1:24,*EL,*B,*HON,"NINDAT FILE ERROR";
         GOTO      IOEXIT
THREE    DISPLAY   *P1:24,*EL,*B,*HON,"NINMDLST FILE ERROR";
         GOTO      IOEXIT
FOUR     DISPLAY   *P1:24,*EL,*B,*HON,"OUTPUT FILE ERROR";
         GOTO      IOEXIT
IOEXIT   KEYIN     *P77:24,*EOFF,ANS;
         CMATCH    "Q" TO ANS
         STOP      IF EQUAL
         GOTO      IO1
TOTAL   
.COMPARE   TOTLINE TO LINES
.         CALL      HEADER IF NOT LESS
          COMPARE  "9900" to ROW
          call      header if not less

.         PRINT     *L,*N,"TOTAL RECORDS : ",COUNT
          prtpage Laser;*pColumn1:row,*ALIGNMENT=*Left,*boldon,*ll,"Total Records : ",Count; 
.          SUB       ONE FROM COPY
.         COMPARE   ZERO TO COPY
.         GOTO      EXIT IF EQUAL
         GOTO      EXIT
         DISPLAY   *P1:22,*EL,"NUMBER COPIES LEFT TO PRINT ",COPY;
         MOVE      "0000" TO PAGE
         MOVE      "00000" TO COUNT
.         CLOSE     OUTPUT
         GOTO      MORECOPY
EXIT     BRANCH    REPTYPE TO CLOS1,CLOS2,CLOS3
CLOS1    COMPARE   ONE TO TIPE
         GOTO      CLOS2 IF NOT EQUAL
         CLOSE     OUTPUT
         GOTO      SPL
CLOS2    CLOSE     OUTPUT2
         GOTO      SPL
CLOS3    CLOSE     OUTPUT3
.
SPL      
.         .print     hpreset
.         SPLCLOSE
          prtclose  Laser
         release
.         shutdown  "cls"
          alert     PLAIN,"Your report is c:\work\pdf\mdlstout.pdf",result
          IF        (EXTERNALFLAG = YES)
          RETURN
          ELSE
          STOP
          ENDIF
         INCLUDE   NDATIO.inc
;Patch1.9
                              include compio.inc
                              include   cntio.inc
.         INCLUDE   NMLRIO.INC
;Patch1.9
          include   Ntxtio.inc
          include   NMoDIO.inc
          include   NSelIO.inc
          include   NMDLIO.inc
         INCLUDE   NXRFIO.inc
         INCLUDE   COMLOGIC.inc

