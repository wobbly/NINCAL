
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORD2DD.inc
         INCLUDE   NOWNDD.inc
.START PATCH 1.43 REPLACED LOGIC
..START PATCH 1.41 ADDED LOGIC
.         INCLUDE   NFULDD.INC
..END PATCH 1.41 ADDED LOGIC
          include    compdd.inc
          include    cntdd.inc
.END PATCH 1.43 REPLACED LOGIC
          include    ndatdd.inc
Release   init      "1.51"              DLH add filter for LM orders
Reldate   INit      "16 Aug 12"          
.Release   init      "1.50"              DLH add MMI
.Reldate   INit      "28 Apr 11"          
.RELEASE  INIT         "1.44"       JD   11OCT2006 FULFILLMENT CONVERSION/Datacard
.RELEASE  INIT         "1.43"      DMS  21JUN2006 FULFILLMENT CONVERSION
.RELEASE  INIT      "1.42"      DMB  31MAY2005 Added code suspect that code was accidentally modified .  Fixed if statement
.RELEASE  INIT      "1.41"      ASH 04FEB2002 NINFUL CONVERSION
.RELEASE  INIT      "1.4"      ASH 02OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "1.3"      ASH 15JAN99 NINORD Y2K, File expansion
.RELEASE  INIT      "1.2"      DLH 18MAR92
NUM      FORM      1
NUM1     FORM      1
FIRST    DIM       1
RECREAD  FORM      3
WRITREC  FORM      3
.Start patch #1.3 - increased var
.SELECT   FORM      1
SELECT   FORM      2
.End patch #1.3 - increased var
RETURN   FORM      4
.Start patch #1.3 - increased var
.INFILE   FILE      VAR=328
.OUTFILE  FILE      VAR=328
INFILE   FILE      VAR=498
OUTFILE  FILE      VAR=498
.eND patch #1.3 - increased var
FILEIN   DIM       8
RUNFLAG  FORM       1
INPUT    DIM       30
.
          CMATCH    B1 TO PROGRAM       .CHAINED FROM DSINIT WITH INFO?
        IF        EOS                 .NO
                  MOVE      "NORD0017" TO PROGRAM
                  MOVE      "Names In The News" TO COMPNME
                  KEYIN     *P10:12,*EL,"INPUT FILE NAME : ",INPUT
        endif
.begin patch 1.5
.        MOVE    "EPSILON WEEKLY DISKIN" TO STITLE
        MOVE    "WEEKLY DISKIN" TO STITLE
.end patch 1.5
        CALL    PAINT
        MOVE    "EXIT" TO PF5
        CALL    FUNCDISP
        TRAP    EOJ IF F5
.
INPGET  
          TRAP    INPNG GIVING ERROR IF IO
        OPEN    TESTFILE,INPNAME
        TRAPCLR IO
        DISPLAY *P15:06,INPNAME
        MOVE    INPNAME TO input
        close   testfile
        GOTO    process
INPNG
          NORETURN
        TRAPCLR IO
        KEYIN   *P01:24,*EL,"The Input file is not on-line. ":
                   *DV,ERROR:   
                *P15:06,INPNAME
        GOTO    INPGET
.
PROCESS
        RESET   COMMENT
        SCAN    "LIST" IN COMMENT
        CALL    OPTLIST EQUAL
        RESET   COMMENT
        SCAN    "RTN" IN COMMENT
        CALL    OPTRTN IF EQUAL
.begin patch 1.5
        RESET   COMMENT
        SCAN    "FULL" IN COMMENT
        CALL    OPTFull IF EQUAL
.end patch 1.5
.begin patch 1.51
        RESET   COMMENT
        SCAN    "LM" IN COMMENT
        CALL    OPTLM IF EQUAL
.end patch 1.51
        open    infile,inpname
.START PATCH 1.4 REPLACED LOGIC
.         PREPARE   OUTFILE,"g:\DATA\DISKIN77"
.begin patch 1.51
          if        (runflag = c4)
        PACK    STR35,NTWKPATH1,"DISKIN68"
          else        
        PACK    STR35,NTWKPATH1,"DISKIN77"
          endif
.end patch 1.51
        PREPARE OUTFILE,STR35
.END PATCH 1.4 REPLACED LOGIC
        RESET   COMMENT
        SCAN    "LIST" IN COMMENT
        CALL    OPTLIST EQUAL
        RESET   COMMENT
        SCAN    "RTN" IN COMMENT
        CALL    OPTRTN IF EQUAL
.begin patch 1.5
        RESET   COMMENT
        SCAN    "FULL" IN COMMENT
        CALL    OPTFull IF EQUAL
.end patch 1.5
.begin patch 1.51
        RESET   COMMENT
        SCAN    "LM" IN COMMENT
        CALL    OPTLM IF EQUAL
.end patch 1.51
KEYIN 
          DISPLAY *P15:12,*EL,"1) FOR TDMC / 2) EPSILON"
        MOVE    C2 TO NUM
.begin patch 1.5
        Branch      num of INput,INput,input,input
.        COMPARE "1" TO NUM
.        GOTO    INPUT IF EQUAL
.        COMPARE "2" TO NUM
.        GOTO    INPUT IF EQUAL
.end patch 1.5
        GOTO    KEYIN
INPUT
          READ    INFILE,SEQ;ORD2VARS
        GOTO    EOJ IF OVER
        ADD     "1",RECREAD
        DISPLAY *P12:11,*EL,"RECORDS READ = ",RECREAD
.         MATCH     YES TO FIRST
.         GOTO      BRANCH2 IF NOT EQUAL
.begin patch 1.5
.        BRANCH  NUM OF TDMC,EPSIL
..BRANCH2
..         BRANCH    NUM OF TDMC,EPSIL2
.TDMC
..         MOVE      NO TO FIRST
.. .        MOVE      ORTNNUM TO RETURN
..         COMPARE   "0040" TO RETURN
..         GOTO      INPUT IF NOT EQUAL
..         GOTO      WRIT IF EQUAL
..
.EPSIL
..         KEYIN     *P17:12,*EL,"1) BY LIST / 2) BY MLR",NUM1
.EPSIL2
.         BRANCH  RUNFLAG OF RTN,LIST
         BRANCH  RUNFLAG OF RTN,LIST,FullFil,LM
.end patch 1.5
 
RTN
          MOVE    ORTNNUM TO RETURN
        COMPARE "1102" TO RETURN
        GOTO    CHESH IF EQUAL
        COMPARE "3516" TO RETURN
        GOTO    CHESH IF EQUAL
.         COMPARE   "0048" TO RETURN
.         GOTO      COVE IF EQUAL
        GOTO    INPUT
.
.COVE     MATCH    "0396" TO OMLRNUM
.         GOTO      INPUT IF NOT EQUAL
.
CHESH
        MOVE    OFOCODE TO SELECT
        BRANCH  SELECT OF INPUT,INPUT,WRIT,INPUT,INPUT,WRIT,WRIT,WRIT,WRIT
WRIT
        WRITE   OUTFILE,SEQ;ORD2VARS
        ADD     "1" TO WRITREC
        DISPLAY *P12:12,*EL,"RECORDS WRITTEN = ",WRITREC
        GOTO    INPUT
.
LIST    
        MATCH   "1547" TO OLON
        GOTO    WRIT IF EQUAL
        MATCH   "2191" TO OLON
        GOTO    WRIT IF EQUAL
        if      (olon = "1547" |olon = "2191"|olon = "4827"|olon= "4903"|olon= "4911"|olon="5366"|olon="5636")
                    MOVE      OLON TO NOWNFLD
          CALL      NOWNKEY
.START PATCH 1.41 ADDED LOGIC
.                   SCAN      "EPSILON" IN OWNCTN
.                   GOTO      WRIT IF EQUAL
                    call      Trim using OWNCTN
.START PATCH 1.43 REPLACED LOGIC
.                   if (OWNCTN <> "")
.                             pack      NFULFLD,OWNCTN
.                             rep       zfill,NFULFLD
.                             move      C1,NFULPATH
.                             move      "LIST-NFULKEY",Location
.                             pack      KeyLocation,NFULFLD
.                             call      NFULKEY
.                   else
.                             clear     NFULFLD
.                             clear     NFULCOMP
.                   endif
.                   if (NFULFLD = "0008")
.                             goto WRIT
.                   else
.                             scan      "EPSILON",NFULCOMP
.
.                   if (OWNCTN <> "")
.                             pack      COMPFLD6,OWNCTN
.                             rep       zfill,COMPFLD6
.                             move      C1,COMPPATH
.                             move      "LIST-COMPKEY6",Location
.                             pack      KeyLocation,COMPFLD6
.                             call      COMPKEY6
.                             if over             
.                                       clear     COMPFLD6
.                                       clear     COMPCOMP
.                             else
.                                       if (COMPSVBFLG <> "T")
.                                                 clear     COMPFLD6
.                                                 clear     COMPCOMP
.                                       endif
.                             endif
.                   else      // OWNCTN = ""
.                             clear     COMPFLD6
.                             clear     COMPCOMP
.                   endif

.begin patch "1.44"
                    if (DATFUL <> "")
                              pack      COMPFLD, DATFUL
                              rep       zfill, COMPFLD
                              move           C1,COMPPATH
                              move      "Verify-COMPKEY",Location
                              pack      KeyLocation,COMPFLD
                              call      COMPKEY
                              if over
                              clear     COMPCOMP
                              clear   COMPOLDSVB
                              else
                                        if (COMPSVBFLG <> "T")
                                        clear     COMPCOMP
                              clear   COMPOLDSVB
                                        endif
                              endif
                    else      // datful = ""
                              clear     COMPCOMP
                              clear   COMPOLDSVB
                    endif
.                   if (COMPFLD6 = "0008")
                    if (COMPOLDSVB = "0008")
                              goto WRIT
                    else
                              scan      "EPSILON",COMPCOMP
.end patch "1.44"
.END PATCH 1.43 REPLACED LOGIC
                              if equal 
                                        goto WRIT
                              endif
                    endif
.>Patch 1.42 Added Missing Endif                  
          endif
.>Patch 1.42 End patch                            
.END PATCH 1.41 ADDED LOGIC
                   GOTO      INPUT
.begin patch 1.5
FullFil
          MATCH   "009428" TO OFullfil
          GOTO    WRIT IF EQUAL
          goto      Input
.end patch 1.5
.begin patch 1.51
LM
          pack      str2 from Osales10,Osales
          if        (str2 = "06" or str2 = "27" or str2 = "30")
          GOTO    WRIT
          endif
          goto      Input
.end patch 1.51
OPTLIST   
          MOVE      C2 TO RUNFLAG
        RETURN
OPTRTN   
          MOVE      C1 TO RUNFLAG
        RETURN
.begin patch 1.5
OPTFULL
          MOVE      C3 TO RUNFLAG
        RETURN
.end patch 1.5
.begin patch 1.51
OPTLM
          MOVE      C4 TO RUNFLAG
        RETURN
.end patch 1.51
EOJ
         WEOF      OUTFILE,SEQ
         CLOSE     OUTFILE
         CLOSE     INFILE
         shutdown  "cls"
         STOP
.START PATCH 1.43 REPLACED LOGIC
..START PATCH 1.41 ADDED LOGIC
.         INCLUDE   NFULIO.INC
..END PATCH 1.41 ADDED LOGIC 
          include    compio.inc
          include      cntio.inc
.END PATCH 1.43 REPLACED LOGIC
        INCLUDE   COMLOGIC.inc
        include   ndatio.inc
         INCLUDE   NOWNIO.inc

