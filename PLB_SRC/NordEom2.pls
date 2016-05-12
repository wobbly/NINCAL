
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
RECREAD  FORM      5
WRITREC  FORM      5
SELECT   FORM      2
RETURN   FORM      4
INFILE   FILE      VAR=498
OUTFILE  FILE      VAR=498
FILEIN   DIM       8
RUNFLAG  FORM       1
INPUT    DIM       30
enddate      form   5
BCNAME   DIM       45             BROKER NAME
.
          CMATCH    B1 TO PROGRAM       .CHAINED FROM DSINIT WITH INFO?
        IF        EOS                 .NO
                  MOVE      "NORDEom2" TO PROGRAM
                  MOVE      "Names In The News" TO COMPNME
                  KEYIN     *P10:12,*EL,"INPUT FILE NAME : ",INPUT
        endif
        DISPLAY *P12:11,*EL,"RECORDS READ = ",RECREAD
        MOVE    "AWF monthly DISKIN" TO STITLE
        CALL    PAINT
        MOVE    "EXIT" TO PF5
        CALL    FUNCDISP
        TRAP    EOJ IF F5
        unpack      today into mm,str1,dd,str1,yy
        
.
.
PROCESS
        PACK    STR35,NTWKPATH1,"DISKIN79"
        PREPARE OUTFILE,STR35
          Packkey      Nordfld2 from "02X002312"
          call      NordAim

INPUT
          call      NordKG
        GOTO    EOJ IF OVER
        ADD     "1",RECREAD
        DISPLAY *P29:11,*EL,RECREAD
          if        (mm <> oodtem or yy <> oodtey)
          goto      Input
          endif
WRIT
         PACK      MKEY FROM OMLRNUM,OCOBN
         CALL      NMLRKEY
         MOVE      MCOMP TO BCNAME      
         clear     nbrkfld
         move      c1 to nbrkpath
         pack      nbrkfld from obrknum,z3
         cmatch    b1 to nbrkfld
         if        not eos
            call      nbrkkey
            if        not over
                          move      brcomp to mname
            endif 
         endif
         MOVE      MCOMP TO BCNAME      *was remmed for patch #4.3

        WRITE   OUTFILE,SEQ;ORDVARS,mcomp,bcname
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
