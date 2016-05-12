..............................................................................
.turned off 18feb93    dlh.
..............................
.
.I.   OPEN THE SYSTEM LOG FILE
.
.         CLOCK     PORT TO PORTINFO
.         UNPACK    PORTINFO INTO TASK,ANS,NODE,ANS,PORTSCRN,ANS,ATTRIB
         BRANCH    SECURITY OF OPEN,OPEN,OPEN,OPEN,OPEN
OPEN     TRAP      NOLOG GIVING ERROR IF IO
.;         goto      logopen
.;         return
         OPEN      LOGFILE,"LOGFILE|NINS1:502"
         move                     c1 to logflag
         TRAPCLR   IO
         GOTO      LOGOPEN
         RETURN
*..............................................................................
.  LOG FILE IS MISSING
. 
NOLOG    DISPLAY   *P20:1,*EL,"LOGFILE","/TEXT is missing!":
                   *P20:3,*EL,ERROR:
                   *P20:2,*EL,"  The port number is ",LOGPORT,*W
         TRAPCLR   IO
         NORETURN
HANG
.;        goto       open
         DISPLAY    *P5:1,"MISSING LOGFILE",*EL
.;         IFZ       PC
.;         PREPARE   LOGFILE,"E:\DATA\TEXT\LOGFILE"
.;         XIF
.;         IFNZ      PC
         PREPARE   LOGFILE,"\\nins1\e\data\text\LOGFILE.dat"
.;         XIF
         FILEPI    4;LOGFILE
         WRITE     LOGFILE,SEQ;HEADER,HEADER1,"  ",TODAY," ",TIME
         WRITE     LOGFILE,SEQ;HEADER2,HEADER3
         WEOF      LOGFILE,SEQ
         IFNZ      PC
         FLUSH     LOGFILE
         XIF
         CLOSE     LOGFILE
         GOTO      OPEN
*..............................................................................
LOGWRITE  branch logflag of writeok
          call      open
writeok
          CLOCK     TIME TO TIME
         CLOCK     PORT TO PORTINFO
         UNPACK    PORTINFO INTO TASK,ANS,NODE,ANS,PORTSCRN,ANS,ATTRIB
         PACK      LOGPORT FROM NODE,TASK
.;         MATCH     "       " TO LOGPORT
.;         CALL      FINDPORT IF EQUAL
.;         CALL      FINDPORT IF EOS
LOGWRT1  TRAP      LOG GIVING ERROR IF IO                ADDED TO PREVENT DA
         CLEAR     SUBDIRCT
;          return
         FILEPI    2;LOGFILE
         WRITE     LOGFILE,SEQEOF;SUBDIRCT,LOGPORT:
                                  "    ",LOGTYPE:
                                  "  ",TODAY:
                                  "  ",TIME:
                                  "  ",LOGINFO:
                                  b1,user
         WEOF      LOGFILE,SEQEOF
         TRAPCLR   IO
         RETURN
. .............................................................................
LOG 
       goto       logwrite
         TRAPCLR   IO
         DISPLAY   *P1:24,*EL,*HON,"LOG FILE ERROR: ",ERROR,*HOFF,*W
         TRAP      NOLOG GIVING ERROR IF IO
         OPEN      LOGFILE,"LOGFILE"
         TRAPCLR   IO
         GOTO      LOGWRITE
*..............................................................................
LOGCLOSE 
.;return
.;         IFNZ      PC
.;         FLUSH     LOGFILE
.;         XIF
         CLOSE     LOGFILE
         RETURN
*..............................................................................
LOGOPEN  TRAPCLR   PARITY
LOGEXIT
*..............................................................................
