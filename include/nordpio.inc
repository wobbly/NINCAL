..............................................................................
.
. nordpIO INCLUSION
. NIN RECORD NUMBER IO INCLUSION
.
. FILE NAME : NINPRINTL
.patch 1.1 03May2001 DLH filelist - temporarily removed 15MAY01 by ASH
.
..............................................................................
.
nordPKEY BRANCH    nordPpath TO nordP1A,nordPerr
nordP1A  BRANCH    nordPFLAG OF nordP1B
         CALL      nordPOPEN
nordP1B  TRAP      IOMssg Giving Error if IO
         branch    nordPlock to nordP1bL,nordP1bR,nordP1bN
nordP1bL FILEPI    1;nordPFILE
         READ      nordPFILE,nordPFLD;ordvars
         TRAPCLR   IO
         RETURN
nordP1bR  
         READLK    NORDPFILE,NORDPFLD;ORDVARS
         TRAPCLR   IO
         RETURN
nordP1bN
         READ      NORDPFILE,NORDPFLD;ORDVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : nordPTST
. REQUIRED    : nordPFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
nordPTST branch    nordPpath to nordP2a,nordP2c
nordP2A  BRANCH    nordPFLAG TO nordP2B
         CALL      nordPOPEN
nordP2B  TRAP      IOMssg Giving Error if IO
         FILEPI    1;nordPFILE
         READ      nordPFILE,nordPFLD;;
         TRAPCLR   IO
         RETURN
nordP2c  BRANCH    NORDPFLG2 TO NORDP2d
         CALL      NORDPOPN2
NORDP2d  TRAP      IOMssg Giving Error if IO
         FILEPI    1;NORDPFLE
         READ      NORDPFLE,NORDPFLD1,NORDPFLD2,NORDPFLD3,NORDPFLD4;;
         TRAPCLR   IO
..............................................................................
.
. ENTRY POINT : nordPTST2
. REQUIRED    : nordPFLD
. RETURNED    :
. DESCRIPTION : TEST KEY for OLRN
.
nordPTST2
	 BRANCH    nordPpath TO nordP1A2,nordPerr
nordP1A2
	 BRANCH    nordPFLAG OF nordP1B2
         CALL      nordPOPEN
nordP1B2
	 TRAP      IOMssg Giving Error if IO
         branch    nordPlock to nordP1bL2,nordP1bR2,nordP1bN2
nordP1bL2
	 FILEPI    1;nordPFILE
         READ      nordPFILE,nordPFLD;;
         TRAPCLR   IO
         RETURN
nordP1bR2  
         READLK    NORDPFILE,NORDPFLD;;
         TRAPCLR   IO
         RETURN
nordP1bN2
         READ      NORDPFILE,NORDPFLD;;
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : nordPKS
. REQUIRED    :
. RETURNED    : RECORD NUMBER RECORD
. DESCRIPTION : KEY SEQUENTIAL RECORD NUMBER FILE READ
.
nordPKS  branch    nordPpath to nordP3A,nordPerr
nordP3A  BRANCH    nordPFLAG TO nordP3B
         CALL      nordPOPEN
nordP3B  TRAP      IOMssg Giving Error if IO
         branch    nordPlock to nordP3bL,nordP3bR,nordP3bN
nordP3bL
         FILEPI    1;nordPFILE
         READKS    nordPFILE;ordvars
         TRAPCLR   IO
         RETURN
nordP3bR
         READKSLK  NORDPFILE;ORDVARS
         TRAPCLR   IO
         RETURN
nordP3bN
         READKS    NORDPFILE;ORDVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : nordPSEQ
. REQUIRED    :
. RETURNED    : RECORD NUMBER RECORD
. DESCRIPTION : SEQUENTIAL RECORD NUMBER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
nordPSEQ BRANCH    nordPFLAG TO nordP4
         CALL      nordPOPEN
nordP4   TRAP      IOMssg Giving Error if IO
         branch    nordPlock to nordP4L,NordP4R,nordP4N
NordP4L  FILEPI    1;nordPFILE
         READ      nordPFILE,SEQ;ordvars
         TRAPCLR   IO
         RETURN
NordP4R
         READLK    NORDPFILE,SEQ;ORDVARS
         TRAPCLR   IO
         RETURN
NordP4N
         READ      NORDPFILE,SEQ;ORDVARS
         TRAPCLR   IO
         RETURN

..............................................................................
.
. ENTRY POINT : nordPWRT
. REQUIRED    : 'nordPFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
nordPWRT MOVE      C1 TO NORDPPATH
         BRANCH    nordPFLAG TO nordP5
         CALL      nordPOPEN
nordP5   TRAP      IOMssg Giving Error if IO
.begin patch 1.1
         FILEPI    1;nordPFILE
.         FILEPI    1;PendFList
         WRITE     nordPFILE,nordPFLD;ordvars
.         WRITE     PendFList;ordvars
         CALL      NORDPWRT2
         TRAPCLR   IO
         RETURN
.
NORDPWRT2
         BRANCH    NORDPFLG2 TO NORDP5B
         CALL      NORDPOPN2
NORDP5B  FILEPI    1;NORDPFLE
         INSERT    NORDPFLE
         RETURN
.end patch 1.1
..............................................................................
.
. ENTRY POINT : nordPUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
nordPUPD BRANCH    nordPFLAG TO nordP6
         CALL      nordPOPEN
nordP6   TRAP      IOMssg Giving Error if IO
.begin patch 1.1
         UPDATE    nordPFILE;ordvars
         FILEPI    1;nordPFILE
.         FILEPI    1;PendFlist
.         UPDATE    PendFList;ordvars
.end patch 1.1
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : nordPDEL
. REQUIRED    : 'nordPFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
.nordPDEL BRANCH    nordPFLAG TO nordP7
.         CALL      nordPOPEN
.nordP7   TRAP      IOMssg Giving Error if IO 
.         FILEPI    1;nordPFILE
.         DELETE    nordPFILE,nordPFLD
.         TRAPCLR   IO
.         RETURN

nordPDEL BRANCH    NORDPFLG2 TO NORDP7
         CALL      NORDPOPN2
nordP7   BRANCH    nordPFLAG TO nordP7A
         CALL      nordPOPEN
nordP7A  TRAP      IOMssg Giving Error if IO
         READ      nordPFILE,nordPFLD;;
         IF OVER
                   TRAPCLR      IO
                   RETURN
         ENDIF
.         PACK      NORDPFLD1,"01X",OCO2CODE
.         PACK      NORDPFLD2,"02X",OLNUM
.         PACK      NORDPFLD3,"03X",OCOCODE
.         PACK      NORDPFLD4,"04X",OHIST
         FILEPI    1;NORDPFLE
         READ      NORDPFLE,NORDPFLD1,NORDPFLD2,NORDPFLD3,NORDPFLD4;str6,str6
         IF OVER
                   TRAPCLR      IO
                   RETURN
         ENDIF
         IF (NORDPFLD <> str6)
                   LOOP
                                READKG  NORDPFLE;str6,str6
                                UNTIL   OVER
                                UNTIL (NORDPFLD = str6)
                   REPEAT
         ENDIF
         IF (NORDPFLD = str6)
.begin patch 1.1
                   FILEPI    1;NORDPFLE
                   DELETE    NORDPFLE
                   FILEPI    1;NORDPFILE
                   DELETEK   NORDPFILE,NORDPFLD
.                    FILEPI    1;PendFlist
.                    DELETE    PendFList
         ELSE
.???????????????
                   FILEPI    1;NORDPFILE
                  DELETE    NORDPFILE,NORDPFLD
.                    FILEPI    1;PendFlist
.                    DELETE    PendFList
.end patch 1.1
         ENDIF
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NORDPAIM
. REQUIRED    : NORDPFLD1,NORDPFLD2,NORDPFLD3,NORDPFLD4
. RETURNED    : ORDER RECORD
. DESCRIPTION : AIM ORDER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NORDPAIM BRANCH    NORDPFLG2 TO NORDP8
         CALL      NORDPOPN2
NORDP8   TRAP      IOMssg Giving Error if IO
         Branch    NordPlock to NordP8L,NORDP8R,NOrdP8N
NordP8L  FILEPI    1;NORDPFLE
         READ      NORDPFLE,NORDPFLD1,NORDPFLD2,NORDPFLD3,NORDPFLD4;ORDVARS
         TRAPCLR   IO
         RETURN
NordP8R
         READLK    NORDPFLE,NORDPFLD1,NORDPFLD2,NORDPFLD3,NORDPFLD4;ORDVARS
         TRAPCLR   IO
         RETURN
NordP8N
         READ      NORDPFLE,NORDPFLD1,NORDPFLD2,NORDPFLD3,NORDPFLD4;ORDVARS
         TRAPCLR   IO
         RETURN

..............................................................................
.
. ENTRY POINT : NORDPKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : ORDER RECORD
. DESCRIPTION : AIM KEY GENERIC ORDER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NORDPKG  BRANCH    NORDPFLG2 TO NORDP9
         CALL      NORDPOPN2
NORDP9   TRAP      IOMssg Giving Error if IO
         Branch    NordPlock to NordP9L,NordP9R,NordP9N
NordP9L  FILEPI    1;NORDPFLE
         READKG    NORDPFLE;ORDVARS
         TRAPCLR   IO
         RETURN
NordP9R
         READKGLK    NORDPFLE;ORDVARS
         TRAPCLR   IO
         RETURN
NordP9N
         READKG    NORDPFLE;ORDVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
.         IFZ      PC
. ENTRY POINT : NORDPKGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : ORDER RECORD
. DESCRIPTION : Aim KEY GENERIC PRIOR ORDER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NORDPKGP BRANCH    NORDPFLG2 TO NORDP10
         CALL      NORDPOPN2
NORDP10  TRAP      IOMssg Giving Error if IO
         branch    NordPlock to NordP10L,NordP10R,NordP10N
NordP10L FILEPI    1;NORDPFLE
         READKGP   NORDPFLE;ORDVARS
         TRAPCLR   IO
         RETURN
NordP10R
         READKGPLK   NORDPFLE;ORDVARS
         TRAPCLR     IO
         RETURN
NordP10N
         READKGP   NORDPFLE;ORDVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NORDPAIMA
. REQUIRED    : NORDPFLD1,NORDPFLD2,NORDPFLD3,NORDPFLD4
. RETURNED    : ORDER RECORD OLRN
. DESCRIPTION : AIM ORDER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NORDPAIMA
         BRANCH    NORDPFLG2 TO NORDP11
         CALL      NORDPOPN2
NORDP11  TRAP      IOMssg Giving Error if IO
         Branch    NordPlock to NordP11L,NORDP11R,NOrdP11N
NordP11L FILEPI    1;NORDPFLE
         READ      NORDPFLE,NORDPFLD1,NORDPFLD2,NORDPFLD3,NORDPFLD4;str6,str6
         TRAPCLR   IO
         RETURN
NordP11R
         READLK    NORDPFLE,NORDPFLD1,NORDPFLD2,NORDPFLD3,NORDPFLD4;str6,str6
         TRAPCLR   IO
         RETURN
NordP11N
         READ      NORDPFLE,NORDPFLD1,NORDPFLD2,NORDPFLD3,NORDPFLD4;str6,str6
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NORDPKGA
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : ORDER RECORD OLRN
. DESCRIPTION : AIM KEY GENERIC ORDER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NORDPKGA BRANCH    NORDPFLG2 TO NORDP12
         CALL      NORDPOPN2
NORDP12  TRAP      IOMssg Giving Error if IO
         Branch    NordPlock to NordP12L,NordP12R,NordP12N
NordP12L FILEPI    1;NORDPFLE
         READKG    NORDPFLE;str6,str6
         TRAPCLR   IO
         RETURN
NordP12R
         READKGLK    NORDPFLE;str6,str6
         TRAPCLR   IO
         RETURN
NordP12N
         READKG    NORDPFLE;str6,str6
         TRAPCLR   IO
         RETURN
...............................................................................
.
. ENTRY POINT : nordPOPEN
. REQUIRED    : 'nordPFLAG'
. RETURNED    : 'nordPFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN RECORD NUMBER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
nordPOPEN 
         TRAP      nordPGONE IF IO
         branch    nordPlock to nordPop1,nordPop1R,nordPop1
nordPop1
.begin patch 1.1
         OPEN      nordPFILE,nordPNAME
.         OPEN      PendFList
         TRAPCLR   IO
.         MOVE      C1 TO NORDPFLG2
         MOVE      C1 TO NORDPFLaG
.end patch 1.1
         RETURN
nordPop1R
         OPEN      NORDPFILE,NORDPNAME,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NORDPFLAG
         RETURN
.
nordPGONE MOVE      nordPNAME TO FILENAME
          CALL      FILEGONE
...............................................................................
.
. ENTRY POINT : NORDPOPN2
. REQUIRED    :
. RETURNED    : 'NORDPFLG2' SET TO '1', IF OPENNED.
. DESCRIPTION : OPEN NIN ORDER FILE, AIMDEXED
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NORDPOPN2 
         TRAP      nordPGNE2 IF IO
         branch    nordPlock to nordPop2,nordPop2r,nordPop2
nordPop2
.begin patch 1.1
         OPEN      NORDPFLE,NORDPNME2
.         OPEN      PendFList
         TRAPCLR   IO
         MOVE      C1 TO NORDPFLG2
.         MOVE      C1 TO NORDPFLaG
.end patch 1.1
         RETURN
nordPop2R
         OPEN      NORDPFLE,NORDPNME2,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NORDPFLG2
         RETURN
nordPGNE2
         MOVE     nordPNME2 TO FILENAME
         CALL     FILEGONE
nordPerr move     "invalid path" to filename
         call     filegone
         trapclr  io
         return
.
..............................................................................

