pC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         include   nowndd.inc
         include   norddd.inc
.START PATCH 1.3 REPLACED LOGIC
..START PATCH 1.1 ADDED LOGIC
.         INCLUDE   NFULDD.INC
..END PATCH 1.1 ADDED LOGIC
           include compdd.inc
           include cntdd.inc
.END PATCH 1.3 REPLACED LOGIC
.
RELEASE  INIT          "1.4"       DMB  12OCT2006 Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.RELEASE  INIT         "1.3"       DMS  22JUN2006 FULFILLMENT CONVERSION
.RELEASE  INIT      "1.2"       DMB  30MAY2005 Fixed bug that occured during compile
.RELEASE  INIT      "1.1"       ASH 05FEB2002 NINFUL CONVERSION
.RELEASE  INIT      "1.0"       JD27mar97 
TDMCORD  IFILE     KEYLEN=6,VAR=7
INFILE   FILE
tdmcout2 file
TDMCOUT  IFILE     KEYLEN=6,VAR=7
KEY      DIM       6
ONE      FORM      "1"
READCNT  FORM      4
LR       DIM       6
.
         MOVE      "NORD0035" TO PROGRAM
         MOVE      "Names in the News" TO COMPNME
         MOVE      "Missing TDMC Records" TO STITLE
         CALL      PAINT
         OPEN      INFILE,"ninprint.cop"
         OPEN      TDMCOUT,"tdmcord2"
         open      tdmcout2,"FAILED"
read
         READ      INFILE,SEQ;ordvars
         GOTO      EOJ IF OVER
         ADD       ONE TO READCNT
         DISPLAY   *P10:12,"NUMBER OF ORDERS READ : ",READCNT
         move      olon to nownfld
         call      nownkey
.START PATCH 1.1 REPLACED LOGIC
.         RESET     OWNCTN
.         SCAN      "TDMC" IN OWNCTN          *TRIPLEX CCTO?
.         if        equal
.         goto      writdmc
.         RESET     OWNCTN
.         SCAN      "TRIPLEX" IN OWNCTN          *TRIPLEX CCTO?
.         if        equal
.         goto      writdmc
.         MATCH     "0040",ORTNNUM
.         IF        EQUAL
.         RESET     RUNCODES
.         SCAN      OLNUM IN RUNCODES
.         IF        NOT EQUAL
.         GOTO      WRITDMC
.         endif
.         endif
..........
.Start Patch 1.4 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.         
.         call      Trim using OWNCTN
          call      Trim using OFULLFIL
.End Patch 1.4 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.           
.START PATCH 1.3 REPLACED LOGIC
.         if (OWNCTN <> "")
.                   pack      NFULFLD,OWNCTN
.                   rep       zfill,NFULFLD
.                   move      C1,NFULPATH
.                   move      "READ-NFULKEY",Location
.                   pack      KeyLocation,NFULFLD
.                   call      NFULKEY
.         else
.                   clear     NFULFLD
.                   clear     NFULCOMP
.         endif
.         if (NFULFLD = "0026")
.                   goto writdmc
.         else
.                   scan      "TDMC",NFULCOMP
.                   if equal
.                             goto writdmc
.                   else
.                             reset     NFULCOMP
.                             scan      "TRIPLEX",NFULCOMP
.Start Patch 1.4 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.    
.         if (OWNCTN <> "")
.                   pack      COMPFLD6,OWNCTN
.                   rep       zfill,COMPFLD6
.                   move      C1,COMPPATH
.                   move      "READ-COMPKEY6",Location
.                   pack      KeyLocation,COMPFLD6
.                   call      COMPKEY6
.                   if over
.                             clear     COMPFLD6
.                             clear     COMPCOMP
.                   else
.                             if (COMPSVBFLG <> "T")
.                                       clear     COMPFLD6
.                                       clear     COMPCOMP
.                             endif
.                   endif
.         else      // OWNCTN = ""
.                   clear     COMPFLD6
.                   clear     COMPCOMP
.         endif
.         if (COMPFLD6 = "0026")
.                   goto writdmc
.         else
.                   scan      "TDMC",COMPCOMP
.                   if equal
.                             goto writdmc
.                   else
.                             reset     COMPCOMP
.                             scan      "TRIPLEX",COMPCOMP
.END PATCH 1.3 REPLACED LOGIC
.                             if equal
.                                       goto writdmc
.                             endif
.                   endif
.         endif
.End Patch 1.4 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.                
.Start Patch 1.4 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.    
          if (OFULLFIL <> "")
                    pack      COMPFLD,OFULLFIL
                    call      zfillit using COMPFLD
                    move      C1,COMPPATH
                    move      "READ-COMPKEY",Location
                    pack      KeyLocation,COMPFLD
                    call      COMPKEY
                    if over
                              clear     COMPFLD
                              clear     COMPCOMP
                    endif
          else      // OFULLFIL = ""
                    clear     COMPFLD
                    clear     COMPCOMP
          endif
          if (OFULLFIL = "009406")
                    goto writdmc
          endif
.End Patch 1.4 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.                
          match     "0040",ORTNNUM
          if equal
                    reset     RUNCODES
                    scan      OLNUM,RUNCODES
                    if not equal
                              goto WRITDMC
                    endif
          endif
.END PATCH 1.1 REPLACED LOGIC
         goto      read
         
WRItDMC  FILEPI    1;TDMCOUT
         read      tdmcout,lr;;
         goto      read if not over
.>Patch 1.2 Code Modified         
         FILEPI    1;TDMCOUT2
.          WRITE     TDMCOUT2,LR;LR,STR1
           WRITE     TDMCOUT2,SEQ;LR,STR1
.>Patch 1.2 End Patch
         GOTO      READ
EOJ 
         CLOSE     INFILE
         STOP
         include   nordio.inc
         include   nownio.inc
.START PATCH 1.3 REPLACED LOGIC
..START PATCH 1.1 ADDED LOGIC
.         INCLUDE   NFULIO.INC
..END PATCH 1.1 ADDED LOGIC 
          include compio.inc
          include cntio.inc
.END PATCH 1.3 REPLACED LOGIC
        INCLUDE   COMLOGIC.inc

