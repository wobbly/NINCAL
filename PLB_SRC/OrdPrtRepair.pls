* ****************************************************************************
* TO MARK AN UNBILLED REC BILLED CHANGE SECOND BYTE OF NININV OR CMPINV FROM
* 0 TO B
* X TO Q
* TO MARK AN UNPAID PAID
* 0 TO P
* ****************************************************************************
* TO REINSTATE AN ORDER CHANGE SECOND BYTE OF NINORD OR CMPORD FROM
* X TO 0
* Q TO B
* *****************************************************************************
* *****************************************************************************
PC         EQU         0
         INCLUDE   COMMON.inc
           INCLUDE   CONS.inc
RELEASE  INIT      "1.0"         DLH   dump record that was HOTPRINTED back into file for processing
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
           MOVE        "Add recs to order print" TO PROGRAM
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
         pack       str55 from "\\nins1\e\data\",iname
.         pack       str55 from "\\nins1\e\data\ninvoice.dave"
          move      C3,nordlock     
          move      c1,nordpath
         OPEN      FILE,str55
         
          open      ORDPRINT,"NINPRINT.isi|NINS1:502"
          move      "OPEN-ninprint.aam",Location
          open      ordprnta,"ninprint.aam|NINS1:502"
Lookit         
         Loop       
         read       File,seq;ordvars
.         read       File,seq;str10,str1,str6
         until      over
.          packkey   Nordfld from str6
.          call      Nordkey
.          if        over
.          stop
.          endif
.         if         ((ostat = "0" | Ostat = "B") & ORCOde = "S")
.          call      debug
          packkey   Nordfld,OLRN

          read      ORDPRINT,nordFLD;;
                    if over
                    write     ORDPRINT,nordFLD;oRDVARS
                    insert    ORDPRNTA
                    TRAPCLR   IO
                    else                    
                    endif
.          endif
          repeat
          
         DISPLAY   *P1:24,"WRITTEN";
         
          Stop











STOP     STOP
          INclude   NordIO.inc
           INCLUDE   COMLOGIC.inc

