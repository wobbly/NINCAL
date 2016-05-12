...............................................................................
..
.. NINVACDIO INCLUSION
...............................................................................
.
. ENTRY POINT : NINVACDKEY
. REQUIRED    : 'NINVACDFLD'
. RETURNED    : INVOICE ACD RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.
NINVACDKEY
               BRANCH   ninvACDFlag TO ninvACD1
               CALL      NINVOPEN
NINVAcd1       FILEPI    1;NINVACDFILE
               READ      NINVACDFILE,NINVACDFLD;INVACDVARS
               return
...............................................................................
.
. ENTRY POINT : NINVTST
. REQUIRED    : NINVFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NInvAcdTST     Branch         NinvAcdFlag to NinvAcd2
               CALL      NInvAcdOPEN
NInvAcd2       FILEPI    1;NInvAcdFILE
               READ      NInvAcdFILE,NInvAcdFLD;str1;
               RETURN
...............................................................................
.
. ENTRY POINT : NInvAcdKS
. REQUIRED    : ''
. RETURNED    : INVOICE RECORD
. DESCRIPTION : KEY SEQUENTIAL INVOICE FILE READ
.
NInvAcdKS      Branch         NinvAcdFlag to NINvAcd3   
               CALL      NInvAcdOPEN
NInvAcd3       FILEPI    1;NInvAcdFILE
               READKS    NInvAcdFILE;INVAcdVARS
.
               RETURN
...............................................................................
.
. ENTRY POINT : NInvAcdSEQ
. REQUIRED    :
. RETURNED    : INVOICE ACD RECORD
. DESCRIPTION : SEQUENTIAL INVOICE FILE READ
.
NInvAcdSEQ     BRANCH    NInvAcdFLAG TO NInvAcd4
               CALL      NInvAcdOPEN
NINVACD4       FILEPI    1;NInvAcdFILE
               READ      NInvAcdFILE,SEQ;INVACDVARS
               RETURN
...............................................................................
.
. ENTRY POINT : NInvAcdWRT
. REQUIRED    : 'NInvAcdFLD', 
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT. 
.
NInvAcdWRT     bRANCH         NinvAcdFlag to NinvAcd5
               CALL      NInvAcdOPEN
NInvAcd5       FILEPI    1;NInvAcdFILE
               WRITE     NInvAcdFILE,NInvAcdFLD;INVACDVARS
.
               RETURN
.
...............................................................................
.
. ENTRY POINT : NInvAcdUPD
. REQUIRED    : A PREVIOUS KEY READ,
. RETURNED    :
. DESCRIPTION : KEY UPDATE INVOICE ACDFILE
.
NInvAcdUPD     Branch         NInvacdflag to NinvAcd6
               CALL      NInvAcdOPEN
NInvAcd6       FILEPI    1;NInvAcdFILE
               UPDATE    NInvAcdFILE;INVACDVARS
               RETURN
...............................................................................
.
. ENTRY POINT : NInvAcdDEL
. REQUIRED    : 'NInvAcdFLD'
. RETURNED    :
. DESCRIPTION : PATH=1 DELETES TeXT & ALL KEYS. 
.
NInvAcdDEL     BRANCH    NInvAcdFlag TO NInvAcd7
.
               CALL      NInvAcdOPEN

NInvAcd7       CALL      NInvAcdtst                            ;ensure valid position in file
               RETURN    IF OVER
               FILEPI    1;NInvAcdFILE
               DELETE    NInvAcdFILE,NInvAcdFLD
               RETURN
.
...............................................................................
.
. ENTRY POINT : NInvAcdOPEN
. REQUIRED    : 
. RETURNED    : 'NInvAcdFLAG' SET TO 1 if opened
. DESCRIPTION : OPEN NIN INVOICE ACD FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NInvAcdOPEN  
               TRAP      NInvAcdGONE IF IO
               BRANCH    NInvAcdFLAG OF NInvAcdCLOS,NInvAcdCLOS
               GOTO      NInvAcdOPN1
NInvAcdCLOS    CLOSE     NInvAcdFILE
NInvAcdOPN1    OPEN      NInvAcdFILE,NInvAcdNAME
               TRAPCLR   IO
               TRAP           IOmssg GIVING ERROR IF IO
               MOVE      c1 TO NInvAcdFLAG
               RETURN
................................................................................
NInvAcdGONE    MOVE      NInvAcdNAME TO FILENAME
               CALL      FILEGONE
.
................................................................................
NInvAcdRecClear
              FOR           AcdRecCount,"1","15"
                              Clear  NInvAcdRec(AcdRecCount).NinvAcdNumRec
                              Clear  NInvAcdRec(AcdRecCount).NinvAcdCodeRec
                              MOve   c0 to NInvAcdRec(AcdRecCount).NinvAcdRateRec
                              MOve   c0 to NInvAcdRec(AcdRecCount).NInvAcdPercRec
                              MOve   c0 to NInvAcdRec(AcdRecCount).NInvAcdQtyRec
                              Clear  NInvAcdRec(AcdRecCount).NINVAcdANINCDRec
                              Clear  NInvAcdRec(AcdRecCount).NINvAcdAextcdRec
                              Clear  NInvAcdRec(AcdRecCount).NinvAcdRateTRec
                              Clear  NInvAcdRec(AcdRecCount).NINvAcdTotalRec
                              Clear  NInvAcdRec(AcdRecCount).NINvAcdDescRec
               repeat
               Return

................................................................................
.NInvAcdRecLoad - load up all ACD records
.Requires - prepositioning in the Isam file so readks can be used
.         - NinvAcdRec empty see clear routine above
.         - and valid Invnum value from invoice read
NInvAcdRecLoad
               FOR           AcdRecCount,"1","15"
               call           NInvAcdKS
                              If over
                              Break
                              endif
                              IF             (invnum = NInvAcdINV)
                              MOve           NinvAcdNum to NInvAcdRec(AcdRecCount).NinvAcdNumRec
                              MOve           NinvAcdCode to NInvAcdRec(AcdRecCount).NinvAcdCodeRec
                              MOve           NinvAcdRate to NInvAcdRec(AcdRecCount).NinvAcdRateRec
                              MOve           NInvAcdPerc to NInvAcdRec(AcdRecCount).NInvAcdPercRec
                              MOve           NINVAcdANINCD to NInvAcdRec(AcdRecCount).NINVAcdANINCDRec
.begin patch 1.3
                              MOve           NINVAcdQty to NInvAcdRec(AcdRecCount).NINVAcdQtyRec
			if	(AdjCalcFlag = YES)                   .calling from adjustment program with change in qty in N9
.if adjustment calc we forced new qty to qtybild
.DO NOT RESAVE RECORD WITH THESE Values in the table
                              	MOve           QtyBild to NInvAcdRec(AcdRecCount).NINVAcdQtyRec
                              	Else
                              	MOve           NINVAcdQty to NInvAcdRec(AcdRecCount).NINVAcdQtyRec
			endif
.end patch 1.3
                              MOve           NinvAcdAextcd to NInvAcdRec(AcdRecCount).NinvAcdAextcdRec
                              Move           NINvAcdRateT to NInvAcdRec(AcdRecCount).NinvAcdRateTRec
                              else
                              break
                              endif
               repeat
NInvAcdRecLoadExit
               return
................................................................................

