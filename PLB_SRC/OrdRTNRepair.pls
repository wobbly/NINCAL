PC         EQU         0
         INCLUDE   COMMON.inc
           INCLUDE   CONS.inc
RELEASE  INIT      "1.0"         DLH   Read Diskin replace return-to as specified
          INclude   Norddd.inc
FILE     FILE      fixed=696
DATA     DIM       1
CODE     DIM       1
LR       DIM       6
INAME    DIM       35
CompID    DIm       1                mailer excluiive bytte
CompID2   Dim       1              LIst exclusive byte
OK       DIM       5
ORDPRINT ifile      keylen=6,fixed=696
ORDPRNTA afile      fixed=696
         TRAP      STOP IF F5
           MOVE      "Names In The News" TO COMPNME
           MOVE        "Replace Return to" TO PROGRAM
           MOVE      "END" TO PF5
           CALL       PAINT
           CALL       FUNCDISP
          move      c1,nordpath
COSMO
         KEYIN     *P01:01,*EL,*EOFF,"ENTER PASSWORD",OK
          rep       Lowup,ok
         MATCH     "COSMO",OK
         STOP      IF NOT EQUAL
         KEYIN     *P1:4,*EF,"ENTER INPUT FILE: ",INAME
         call       trim using iname
         pack       str55 from "\\nins1\e\data\",iname                 ."
          Open      File,str55,read         
Lookit         
         Loop       
         read       File,seq;ordvars
         until      over
         packkey   Nordfld,OLRN
          call      Nordkey
          if        Not over
                    if        (ORTNNUM = "4556")                .double check
                    move      "2945",ORTNNUM
                    call      Nordupd
                    DISPLAY   *P1:24,"Updated";
                    endif
          
          endif

          repeat
          
         DISPLAY   *P1:24,"Updated";
         
          Stop











STOP     STOP
          INclude   NordIO.inc
           INCLUDE   COMLOGIC.inc

