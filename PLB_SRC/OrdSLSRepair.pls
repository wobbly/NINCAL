PC         EQU         0
         INCLUDE   COMMON.inc
           INCLUDE   CONS.inc
RELEASE  INIT      "1.0"         DLH   Fix order salesperson using value from company file and diskin of
.                                orders to be fixed
          INclude   Norddd.inc
          INclude   Compdd.inc
          INclude   Cntdd.inc        
FILE     FILE      fixed=696
DATA     DIM       1
CODE     DIM       1
LR       DIM       6
INAME    DIM       35
CompID    DIm       1                mailer excluisve bytte
CompID2   Dim       1              LIst exclusive byte
OK       DIM       5
         TRAP      STOP IF F5
           MOVE      "Names In The News" TO COMPNME
           MOVE        "Fix Sales person" TO PROGRAM
           MOVE      "END" TO PF5
           CALL       PAINT
           CALL       FUNCDISP
.         DISPLAY    *P01:23,*ES
.         DISPLAY   *P01:23,*EL,"HIT F3 TO END"
COSMO
         KEYIN     *P01:01,*EL,*EOFF,"ENTER PASSWORD",OK
          rep       Lowup,ok
         MATCH     "COSMO",OK
         STOP      IF NOT EQUAL
         KEYIN     *P1:4,*EF,"ENTER INPUT FILE: ",INAME
         call       trim using iname
         pack       str55 from "\\nins1\e\data\",iname                ."
          move      c1,nordpath
         OPEN      FILE,str55
         
Lookit         
         Loop       
         read       File,seq;ordvars
         until      over
          packkey   Nordfld,OLRN

          call      Nordkey
                    if not over
                    pack      mkey from Omlrnum,ocobn
                    call      Nmlrkey
                    call      debug
                    unpack    compcontact into Osales10,osales
                    call      Nordupd
                    else                    
                    endif
.          endif
          repeat
          
         DISPLAY   *P1:24,"Updated";
         
          Stop











STOP     STOP
          INclude   CompIO.inc
          INclude   Cntio.inc        
          INclude   NordIO.inc
           INCLUDE   COMLOGIC.inc

