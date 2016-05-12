..............................................................................
.
. NCSHIO INCLUSION
. NIN CASH FILE I/O ROUTINES
.
.
. FILE NAME : DAT25Nx
. REC LENGTH: 93 FIXED
. INDEX     : SEQ
. AIM KEY   : 23-29
.             33-34
.
.PATCH 1.2 - REARRANGED IO LOGIC IN REFERENCE TO FLAGS
.PATCH 1.1 - ADDED LOGIC FOR FILELIST
..............................................................................
.
. ENTRY POINT : NcshKEY
. REQUIRED    : 'NcshPATH', 'NcshFLD3'
. RETURNED    : cash RENORD
. DESCRIPTION : EXACT ISAM KEY READ
.
ncshkey
         branch    ncshflg3 to ncsh1
.START PATCH 1.1 REPLACED LOGIC
.         CALL      NcshoPN3
         CALL      NcshoPEN
.END PATCH 1.1 REPLACED LOGIC
Ncsh1    trap      IOMssg giving Error if IO
         branch    NCSHLOCK to NCSH1L,NCSH1R,NCSH1N
NCSH1L   FILEPI    1;NcshFiL3
         READ      Ncshfil3,NcshFLD3;cashvars
         trapclr   IO
         RETURN
NCSH1R   READLK    Ncshfil3,NcshFLD3;cashvars
         trapclr   IO
         RETURN
NCSH1N   READ      Ncshfil3,NcshFLD3;cashvars
         trapclr   IO
         RETURN
.START PATCH 1.1 ADDED LOGIC
..............................................................................
.
. ENTRY POINT : NcshTST
. REQUIRED    : 'NcshPATH', 'NcshFLD3'
. RETURNED    : cash RENORD
. DESCRIPTION : EXACT ISAM KEY READ
.
.START PATCH 1/12/05 ASH REPLACED LOGIC
.NCSHTST
.         branch    ncshflg3 to ncsh2
.         CALL      NcshoPEN
.NCSH2    trap      IOMssg giving Error if IO
.         branch    NCSHLOCK to NCSH2L,NCSH2R,NCSH2N
.NCSH2L   FILEPI    1;NcshFiL3
.         READ      Ncshfil3,NcshFLD3;str2,*7,str6,*36,str1,str10
.         trapclr   IO
.         RETURN
.NCSH2R   READLK    Ncshfil3,NcshFLD3;str2,*7,str6,*36,str1,str10
.         trapclr   IO
.         RETURN
.NCSH2N   READ      Ncshfil3,NcshFLD3;str2,*7,str6,*36,str1,str10
.         trapclr   IO
.         RETURN
.......................................
NCSHTST
         branch    ncshflg3 to ncsh2
         CALL      NcshoPEN
NCSH2    trap      IOMssg giving Error if IO
         branch    NCSHLOCK to NCSH2L,NCSH2R,NCSH2N
NCSH2L   FILEPI    1;NcshFiL3
         READ      Ncshfil3,NcshFLD3;str2,*9,str6,*38,str1,str16
         trapclr   IO
         RETURN
NCSH2R   READLK    Ncshfil3,NcshFLD3;str2,*9,str6,*38,str1,str16
         trapclr   IO
         RETURN
NCSH2N   READ      Ncshfil3,NcshFLD3;str2,*9,str6,*38,str1,str16
         trapclr   IO
         RETURN
.END PATCH 1/12/05 ASH REPLACED LOGIC
.END PATCH 1.1 ADDED LOGIC
..............................................................................         
.
. ENTRY POINT : Ncshks
. REQUIRED    : 
. RETURNED    : cash RENORD
. DESCRIPTION : KEY SEQUENTIAL cash FILE READ
.
NcshKS 
         branch    ncshflg3 to ncsh3
         CALL      NcshOPEN
Ncsh3    trap      IOMssg giving Error if IO
         branch    NCSHLOCK to NCSH3L,NCSH3R,NCSH3N
NCSH3L   FILEPI    1;NcshFIL3
         READKS    NcshFIL3;cashvarS
         trapclr   IO
         RETURN
NCSH3R   READKSLK  NcshFIL3;cashvarS
         trapclr   IO
         RETURN
NCSH3N   READKS    NcshFIL3;cashvarS
         trapclr   IO
         RETURN
.START PATCH 1.1 ADDED LOGIC
..............................................................................         
.
. ENTRY POINT : NcshksA
. REQUIRED    : 'NcshATH'
. RETURNED    : cash RENORD
. DESCRIPTION : KEY SEQUENTIAL cash FILE READ
.
.START PATCH 1/12/05 ASH REPLACED LOGIC
.NcshKSA 
.         branch    ncshflg3 to ncsh8
.         CALL      NcshOPEN
.Ncsh8    trap      IOMssg giving Error if IO
.         branch    NCSHLOCK to NCSH8L,NCSH8R,NCSH8N
.NCSH8L   FILEPI    1;NcshFIL3
.         READKS    NcshFIL3;str2,*7,str6,*36,str1,str10
.         trapclr   IO
.         RETURN
.NCSH8R   READKSLK  NcshFIL3;str2,*7,str6,*36,str1,str10
.         trapclr   IO
.         RETURN
.NCSH8N   READKS    NcshFIL3;str2,*7,str6,*36,str1,str10
.         trapclr   IO
.         RETURN
.......................................
NcshKSA 
         branch    ncshflg3 to ncsh8
         CALL      NcshOPEN
Ncsh8    trap      IOMssg giving Error if IO
         branch    NCSHLOCK to NCSH8L,NCSH8R,NCSH8N
NCSH8L   FILEPI    1;NcshFIL3
         READKS    NcshFIL3;str2,*9,str6,*38,str1,str16
         trapclr   IO
         RETURN
NCSH8R   READKSLK  NcshFIL3;str2,*9,str6,*38,str1,str16
         trapclr   IO
         RETURN
NCSH8N   READKS    NcshFIL3;str2,*9,str6,*38,str1,str16
         trapclr   IO
         RETURN
.END PATCH 1/12/05 ASH REPLACED LOGIC
.END PATCH 1.1 ADDED LOGIC
..............................................................................
.
. ENTRY POINT : NCSHSEQ
. REQUIRED    :
. RETURNED    : CASH RECORD
. DESCRIPTION : SEQUENTIAL CASH FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCSHSEQ  COMPARE   NCSHPATH TO NCSHFLAG
         GOTO      NCSH4 IF EQUAL
         CALL      NCSHOPEN
NCSH4    trap      IOMssg giving Error if IO
         branch    NCSHLOCK to NCSH4L,NCSH4R,NCSH4N
NCSH4L   FILEPI    1;NCSHFILE
         READ      NCSHFILE,SEQ;Cashvars                        
         trapclr   IO
         RETURN
NCSH4R   READLK    NCSHFILE,SEQ;Cashvars                        
         trapclr   IO
         RETURN
NCSH4N   READ      NCSHFILE,SEQ;Cashvars                        
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCSHWRT
. REQUIRED    : 
. RETURNED    :
. DESCRIPTION : AIM WRITE 
.
.START PATCH 1.1 REPLACED LOGIC

.NCSHWRT  COMPARE   NCSHPATH TO NCSHFLG2
.         GOTO      NCSH5 IF EQUAL
.         CALL      NCSHOPEN
.NCSH5    trap      IOMssg giving Error if IO
.         FILEPI    1;NCSHFIL2
.         WRITE     NCSHFIL2;cashvars
.         trapclr   IO
.NcshWRT2 BRANCH    NcshFLG3 TO Ncsh5b
.         CALL      NcshOPN3
.Ncsh5b   trap      IOMssg giving Error if IO
.         FILEPI    1;NcshFil3
.         INSERT    NcshFIL3,NcshfLD3
.         trapclr   IO
.         RETURN

.START PATCH 1.2 REPLACED LOGIC
.NCSHWRT COMPARE   NCSHPATH TO NCSHFLG2
.        GOTO      NCSH5 IF EQUAL
NCSHWRT
	branch    NCSHFLG2 to NCSH5
.END PATCH 1.2 REPLACED LOGIC
        CALL      NCSHOPEN
NCSH5   trap      IOMssg giving Error if IO
        FILEPI    1;NCSHLIST
        WRITE     NCSHLIST;CASHVARS
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NCSHUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE CASH FILE
.
.START PATCH 1.2 REPLACED LOGIC
.NCSHUPD COMPARE   NCSHPATH TO NCSHFLG2
.        GOTO      NCSH6 IF EQUAL
NCSHUPD
	branch    NCSHFLG2 to NCSH6
.END PATCH 1.2 REPLACED LOGIC
        CALL      NCSHOPEN
NCSH6   TRAP      IOMssg Giving Error if IO
        FILEPI    1;NCSHLIST
        UPDATE    NCSHLIST;CASHVARS
        TRAPCLR   IO
        RETURN
.END PATCH 1.1 REPLACED LOGIC
.START PATCH 1.1 REPLACED LOGIC
...............................................................................
..
.. ENTRY POINT : NCSHDEL
.. REQUIRED    : PREVIOUS VALID AIM READ
.. RETURNED    :
.. DESCRIPTION :  AIM KEY TXT/AID DELETE
..
.NCSHDEL  COMPARE   NCSHPATH TO NCSHFLG2         
.         GOTO      NCSH7 IF EQUAL
.         CALL      NCSHOPEN
.NCSH7    trap      IOMssg giving Error if IO
.         FILEPI    1;NCSHFIL2
.         DELETE    NCSHFIL2
.         trapclr   IO
.         RETURN
.START PATCH 1.2 REPLACED LOGIC
.NCSHDEL COMPARE   NCSHPATH TO NCSHFLG2
.        GOTO      NCSH7 IF EQUAL
NCSHDEL
	branch    NCSHFLG2 to NCSH7
.END PATCH 1.2 REPLACED LOGIC
         CALL      NCSHOPEn
NCSH7    trap      IOMssg giving Error if IO
         FILEPI    1;NCSHLIST
         DELETE    NCSHLIST
         trapclr   IO
         RETURN
.END PATCH 1.1 REPLACED LOGIC
...............................................................................
..............................................................................
.
. ENTRY POINT : NCSHAIM
. REQUIRED    : CSHFLD
. RETURNED    : CASH RECORD
. DESCRIPTION : AIM CASH FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
.START PATCH 1.2 REPLACED LOGIC
.NCSHAIM COMPARE   NCSHPATH TO NCSHFLG2
.        GOTO      NCSH9 IF EQUAL
NCSHAIM
	branch    NCSHFLG2 to NCSH9
.END PATCH 1.2 REPLACED LOGIC
         CALL      NCSHOPEN
NCSH9    trap      IOMssg giving Error if IO
         branch    NCSHLOCK to NCSH9L,NCSH9R,NCSH9N
NCSH9L   FILEPI    1;NCSHFIL2
.START PATCH REPLACED LOGIC
.         READ      NCSHFIL2,NCSHFLD,NCSHFLD2;cashvars
         READ      NCSHFIL2,NCSHFLD1,NCSHFLD,NCSHFLD2,NCSHFLD4,NCSHFLD5;cashvars
.END PATCH REPLACED LOGIC
         trapclr   IO
         RETURN
NCSH9R   
.START PATCH REPLACED LOGIC
.         READLK    NCSHFIL2,NCSHFLD,NCSHFLD2;cashvars
         READLK    NCSHFIL2,NCSHFLD1,NCSHFLD,NCSHFLD2,NCSHFLD4,NCSHFLD5;cashvars
.END PATCH REPLACED LOGIC
         trapclr   IO
         RETURN
NCSH9N
.START PATCH REPLACED LOGIC
.         READ      NCSHFIL2,NCSHFLD,NCSHFLD2;cashvars
         READ      NCSHFIL2,NCSHFLD1,NCSHFLD,NCSHFLD2,NCSHFLD4,NCSHFLD5;cashvars
.END PATCH REPLACED LOGIC
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCSHKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : CASH RECORD
. DESCRIPTION : AIM KEY GENEREIC CASH FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
.START PATCH 1.2 REPLACED LOGIC
.NCSHKG COMPARE   NCSHPATH TO NCSHFLG2
.        GOTO      NCSH10 IF EQUAL
NCSHKG
	branch    NCSHFLG2 to NCSH10
.END PATCH 1.2 REPLACED LOGIC
         CALL      NCSHOPEN
NCSH10   trap      IOMssg giving Error if IO
         branch    NCSHLOCK to NCSH10L,NCSH10R,NCSH10N
NCSH10L  FILEPI    1;NCSHFIL2
         READKG    NCSHFIL2;cashvars
         trapclr   IO
         RETURN
NCSH10R  READKGLK  NCSHFIL2;cashvars
         trapclr   IO
         RETURN
NCSH10N  READKG    NCSHFIL2;cashvars
         trapclr   IO
         RETURN
..............................................................................
.
         IFZ      PC
. ENTRY POINT : NCSHKGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : CASH RECORD
. DESCRIPTION : KEY GENERIC PRIOR CASH FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
.START PATCH 1.2 REPLACED LOGIC
.NCSHKGP COMPARE   NCSHPATH TO NCSHFLG2
.        GOTO      NCSH11 IF EQUAL
NCSHKGP
	branch    NCSHFLG2 to NCSH11
.END PATCH 1.2 REPLACED LOGIC
         CALL      NCSHOPEN
NCSH11   trap      IOMssg giving Error if IO
         branch    NCSHLOCK to NCSH11L,NCSH11R,NCSH11N
NCSH11L  FILEPI    1;NCSHFIL2
         READKGP   NCSHFIL2;cashvars
         trapclr   IO
         RETURN
NCSH11R  READKGPLK NCSHFIL2;cashvars
         trapclr   IO
         RETURN
NCSH11N  READKGP   NCSHFIL2;cashvars
         trapclr   IO
         RETURN
         XIF
..............................................................................
.
. ENTRY POINT : NCSHAIMA
. REQUIRED    : CSHFLD
. RETURNED    : CONTROL DATE LOADED INTO STR8
. DESCRIPTION : AIM CASH FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
.START PATCH 1.2 REPLACED LOGIC
.NCSHAIMA COMPARE   NCSHPATH TO NCSHFLG2
.        GOTO      NCSH12 IF EQUAL
NCSHAIMA
	branch    NCSHFLG2 to NCSH12
.END PATCH 1.2 REPLACED LOGIC
         CALL      NCSHOPEN
NCSH12   trap      IOMssg giving Error if IO
         branch    NCSHLOCK to NCSH12L,NCSH12R,NCSH12N
NCSH12L  FILEPI    1;NCSHFIL2
         READ      NCSHFIL2,NCSHFLD1,NCSHFLD,NCSHFLD2,NCSHFLD4,NCSHFLD5;;
         trapclr   IO
         RETURN
NCSH12R   
         READLK    NCSHFIL2,NCSHFLD1,NCSHFLD,NCSHFLD2,NCSHFLD4,NCSHFLD5;;
         trapclr   IO
         RETURN
NCSH12N
         READ      NCSHFIL2,NCSHFLD1,NCSHFLD,NCSHFLD2,NCSHFLD4,NCSHFLD5;;
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCSHKGA
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    :	CONTROL DATE LOADED INTO STR8
. DESCRIPTION : AIM KEY GENEREIC CASH FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
.START PATCH 1.2 REPLACED LOGIC
.NCSHKGA COMPARE   NCSHPATH TO NCSHFLG2
.        GOTO      NCSH13 IF EQUAL
NCSHKGA
	branch    NCSHFLG2 to NCSH13
.END PATCH 1.2 REPLACED LOGIC
         CALL      NCSHOPEN
NCSH13   trap      IOMssg giving Error if IO
         branch    NCSHLOCK to NCSH13L,NCSH13R,NCSH13N
NCSH13L  FILEPI    1;NCSHFIL2
         READKG    NCSHFIL2;;
         trapclr   IO
         RETURN
NCSH13R  READKGLK  NCSHFIL2;;
         trapclr   IO
         RETURN
NCSH13N  READKG    NCSHFIL2;;
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCSHOPEN
. REQUIRED    : 'NCSHFLAG'
. RETURNED    : 'NCSHFLAG' SET TO '1' IF SEQ FILE OPENNED
.               'NCSHFLG2' SET TO 'S' IF AIM FILE OPENNED
. DESCRIPTION : OPEN NIN CASH FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
.START PATCH 1.1 REPLACED LOGIC
.NCSHOPEN BRANCH    NCSHPATH OF NCSHOPN1,NCSHOPN2,ncshopn3
.         MOVE      "WHAT CASH" TO NCSHNAME
.         GOTO      NCSHGONE
.NCSHOPN1 TRAP      NCSHGONE IF IO
.         OPEN      NCSHFILE,NCSHNAME
.         TRAPCLR   IO
.         MOVE      C1 TO NCSHFLAG
.         RETURN
.NCSHOPN2 TRAP      NCSHGONE IF IO
.         OPEN      NCSHFIL2,NCSHNAME
.         TRAPCLR   IO
.         MOVE      C2 TO NCSHFLG2
.         RETURN
NCSHOPEN
        TRAP      NCSHGONE IF IO
        BRANCH    NCSHLOCK TO NCSHOPNL,NCSHOPNR

NCSHOPNL
        OPEN      NCSHLIST
        OPEN      NCSHFILE,NCSHNAME
        TRAPCLR   IO
        MOVE      C1 TO NCSHFLAG
.START PATCH 1.2 REPLACED LOGIC 
.       MOVE      C2 TO NCSHFLG2
       MOVE      C1 TO NCSHFLG2
.END PATCH 1.2 REPLACED LOGIC
        MOVE      C1 TO NCSHFLG3
        RETURN
NCSHOPNR
        OPEN      NCSHLIST,LOCKMANUAL,SINGLE
        OPEN      NCSHFILE,NCSHNAME
        TRAPCLR   IO
        MOVE      C1 TO NCSHFLAG
.START PATCH 1.2 REPLACED LOGIC 
.       MOVE      C2 TO NCSHFLG2
       MOVE      C1 TO NCSHFLG2
.END PATCH 1.2 REPLACED LOGIC
        MOVE      C1 TO NCSHFLG3
        RETURN
.END PATCH 1.1 REPLACED LOGIC
.START PATCH 1.1 REPLACED LOGIC
.
...............................................................................
..
.. ENTRY POINT : NcshOPN3
.. REQUIRED    :
.. RETURNED    : 'NcshFLG3' SET TO '1', IF OPENNED.
.. DESCRIPTION : OPEN NIN cash FILE, 
..               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
..
.NcshOPN3 TRAP      Ncshgone IF IO
.         OPEN      NcshFiL3,Ncshname
.         TRAPCLR   IO
.         MOVE      C1 TO NcshFLG3
.         RETURN
..
..         TRAPCLR   IO
..         RETURN
.END PATCH 1.1 REPLACED LOGIC

NCSHGONE MOVE      NCSHNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................

