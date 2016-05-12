PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NCSHDD.INC
         INCLUDE   NCTRDD.INC 

PAYAMT    FORM      10.2
GRDAR     FORM      12.2
CHKFILE   FILE
READFLAG  FORM       1
HCNUM     FORM       3
HCNUMDATE FORM       8     

Release   INIT       "1.0"    Initial Release of Program to Check Control.dat AR total with Cash AR Total
.File must be sorted and copied first in checks.wbt
          MOVE      "NINCSH" TO NCSHNAME
          open      ncshfile,ncshname
          MOVE      C1 TO NCSHPATH
          prepare   CHKFILE,"\\nins1\e\data\ARCHECK.DAT"
          CALL      PAINT
          DISPLAY   *P01:10,"Reading Cash File"
          CLEAR      n8
start
         Loop
         
                    CALL      NCSHSEQ
         Until Over
                    add c1 to n8
                    DISPLAY   *P01:15,"Records Read  : ",N8


                    if (Readflag = c0)
                              move CNUM to HCNUM
                              move CNUMDATE to HCNUMDATE
                              pack    NCTRFLD,HCNUM,HCNUMDATE
                              rep     zfill,NCTRFLD
                              move    "START-NCTRKEY",Location
                              pack    KeyLocation,"Key: ",NCTRFLD
                              call    NCTRKEY
                    if not over
                                       move c1 to readflag
                    endif
                    endif
                    MOVE      C0 TO PAYAMT
                    MOVE      camount to payamt
                    Add       payamt to GRDAR
         Repeat
CHECK
         compare GRDAR to NCTRAMT2
         if equal
                 erase   "\\nins1\e\data\ARCHECK.DAT"
                 DISPLAY   *P01:20,"COMPARE SUCCESSFUL" 
                 PAUSE    C3
         else
.If I don't put something in there close will delete file if first IO instruction

                 DISPLAY  *P01:20,"VALUES NOT EQUAL CORRECT AND RERUN CASH/EDIT" 
                 PAUSE    C3 
                 WRITE    CHKFILE,SEQ;HCNUM,HCNUMDATE,b3,"CASH: ",GRDAR,b3,"Control: ",NCTRAMT2,b3,"AR DOES NOT MATCH!!"
                 close    CHKFILE
         endif  


         INCLUDE     NCTRIO.INC
         INCLUDE     NCSHIO.INC
         INCLUDE     COMLOGIC.inc
