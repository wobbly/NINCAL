
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
RECREAD  FORM      3
WRITREC  FORM      3
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
                  MOVE      "NORDEom1" TO PROGRAM
                  MOVE      "Names In The News" TO COMPNME
                  KEYIN     *P10:12,*EL,"INPUT FILE NAME : ",INPUT
        endif
        MOVE    "CACFB monthly DISKIN" TO STITLE
        CALL    PAINT
        MOVE    "EXIT" TO PF5
        CALL    FUNCDISP
        TRAP    EOJ IF F5
        unpack      today into mm,str1,dd,str1,yy
        call        cvtjul
        move        Juldays to enddate
        
.
.
PROCESS
        PACK    STR35,NTWKPATH1,"DISKIN78"
        PREPARE OUTFILE,STR35
          Packkey      Nordfld1,"01X006005"
          call      NordAim

INPUT
          call      NordKG
        GOTO    EOJ IF OVER
        ADD     "1",RECREAD
        DISPLAY *P12:11,*EL,"RECORDS READ = ",RECREAD
          move      Oodtem,mm
          move      OOdted,dd
          move      OOdtey,yy
          call      cvtjul
          if        (juldays < enddate-795 or juldays > enddate)
          goto      Input
          endif
WRIT
        WRITE   OUTFILE,SEQ;ORDVARS
        ADD     "1" TO WRITREC
        DISPLAY *P12:12,*EL,"RECORDS WRITTEN = ",WRITREC
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
