
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
         INCLUDE   NOWNDD.inc
          include    compdd.inc
          include    cntdd.inc
          include    ndatdd.inc
RELEASE  INIT          "1.00"       DLH   More Eom auto
Reldate   Init      "20 Sept 2010"
NUM      FORM      1
NUM1     FORM      1
FIRST    DIM       1
RECREAD  FORM      7
ReadCnt   Form      7
WRITREC  FORM      7
SELECT   FORM      2
RETURN   FORM      4
INFILE   FILE      VAR=498
OUTFILE  FILE      VAR=498
FILEIN   DIM       8
RUNFLAG  FORM       1
INPUT    DIM       30
enddate      form   5
.
          CMATCH    B1 TO PROGRAM       .CHAINED FROM DSINIT WITH INFO?
        IF        EOS                 .NO
                  MOVE      "NORDEom3" TO PROGRAM
                  MOVE      "Names In The News" TO COMPNME
                  KEYIN     *P10:12,*EL,"INPUT FILE NAME : ",INPUT
        endif
        DISPLAY *P12:11,*EL,"RECORDS READ = "
        MOVE    "Comslect monthly DISKIN" TO STITLE
        CALL    PAINT
        MOVE    "EXIT" TO PF5
        CALL    FUNCDISP
        TRAP    EOJ IF F5
        unpack      today into mm,str1,dd,str1,yy
          Move      c1,Nordpath
        Move        C3,Nordlock    
        move        c0,recread
.
.
PROCESS
        PACK    STR35,NTWKPATH1,"DISKIN80"
        PREPARE OUTFILE,STR35,exclusive
        Move        "999999",nordfld  
        call      Nordkey

INPUT
          call      NordKP
        GOTO    EOJ IF OVER
        ADD     "1",READcnt
        DISPLAY *P28:11,b1,READcnt,b3,olrn
          if        (Readcnt > 100000)
          goto      Eoj
          endif
          CMATCH    "C",OCOMSLCT
          goto      Input if not equal
          call      debug
          if        (mm <> oodtem or yy <> oodtey)
          goto      Input
          endif
WRIT
        WRITE   OUTFILE,SEQ;ORDVARS
        ADD     "1" TO WRITREC
        DISPLAY *P12:12,*EF,"RECORDS WRITTEN = ",WRITREC
        GOTO    INPUT
.
EOJ
         WEOF      OUTFILE,SEQ
         CLOSE     OUTFILE
         CLOSE     INFILE
         shutdown  "cls"
         STOP
         
          include   Nordio.inc
          include    compio.inc
          include      cntio.inc
        INCLUDE   COMLOGIC.inc
        include   ndatio.inc
         INCLUDE   NOWNIO.inc

