PC       EQU       0        
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC
         
RELEASE  INIT      "Pre"
         MOVE      "MARIO" TO PROGRAM
         CLOCK     DATE TO TODAY      
         CALL      PAINT
LOOPER   KEYIN     *P10:10,*EL,STR1
         CMATCH    YES TO STR1
         GOTO      DOIT IF EQUAL
         CMATCH    STAR TO STR1
         STOP      IF EQUAL
         GOTO      LOOPER
DOIT     EXECUTE   "C:\PROGRA~1\WINBATCH\SYSTEM\WINBATCH C:\PROGRA~1\WINBATCH\SAMPLES\FAX.WBT"
         GOTO      LOOPER    
         INCLUDE   COMLOGIC.INC
