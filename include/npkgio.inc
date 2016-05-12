;----------------------------------------------------------------
;
;   NPKGIO.INC - 
;
;   Copyright(c) Names in the News Ca., Inc. 2000
;
;   Author: David Herrick
;   Created: 1/28/2002 7:43:18 AM
;   Last change: DLH 1/28/2002 7:43:18 AM
;----------------------------------------------------------------

..............................................................................
.
. NPKGIO INCLUSION
. NINCAL PACKAGE FILE DEFINITION
.
. FILE NAME : NINPKG.DAT
.
. REC LENGTH:	793 FIXED
. INDEX KEY :	001-010	Mailer/Package Number
. AAMKEY:	001-004	Mailer
.		005-010	Package Number
.		011-160	Package Name
.		161-185	Package ID
.
.PATCH 1.1	ASH	25FEB2002	ADDED FIELDS FOR MASTER PACKAGE LOGIC
...............................................................................
.NPKGKEY - Isam key read
.
. ENTRY POINT : NPKGKEY
. REQUIRED    : 'NPKGFLD'
. RETURNED    : PACKAGE RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPKGKEY  BRANCH    NPKGFLAG TO NPKG1
         CALL      NPKGOPEN
NPKG1    trap      IOMssg giving Error if IO
	 FILEPI    1;NPKGFILE
         READ      NPKGFILE,NPKGFLD;NPKGVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPKGTST
. REQUIRED    : 'NPKGFLD'
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NPKGTST  BRANCH    NPKGFLAG TO NPKG2
         CALL      NPKGOPEN
NPKG2    trap      IOMssg giving Error if IO
	 FILEPI    1;NPKGFILE
         READ      NPKGFILE,NPKGFLD;;
	 trapclr   IO
         RETURN
.
.START PATCH 1.1 ADDED LOGIC
..............................................................................
.
. ENTRY POINT : NPKGTSTA
. REQUIRED    : 'NPKGFLD'
. RETURNED    : str1 - which holds value of NPKGMaster
. DESCRIPTION : TEST KEY
.
NPKGTSTA BRANCH    NPKGFLAG TO NPKG2A
         CALL      NPKGOPEN
NPKG2A   trap      IOMssg giving Error if IO
	 FILEPI    1;NPKGFILE
         READ      NPKGFILE,NPKGFLD;*694,str1
	 trapclr   IO
         RETURN
.END PATCH 1.1 ADDED LOGIC
..............................................................................
.
. ENTRY POINT : NPKGKS
. REQUIRED    :
. RETURNED    : PACKAGE RECORD
. DESCRIPTION : KEY SEQUENTIAL PACKAGE FILE READ
.
NPKGKS   BRANCH    NPKGFLAG TO NPKG3
         CALL      NPKGOPEN
NPKG3    trap      IOMssg giving Error if IO
	 FILEPI    1;NPKGFILE
         READKS    NPKGFILE;NPKGVARS
	 trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPKGSEQ
. REQUIRED    :
. RETURNED    : PACKAGE RECORD
. DESCRIPTION : SEQUENTIAL PACKAGE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPKGSEQ  BRANCH    NPKGFLAG TO NPKG4
         CALL      NPKGOPEN
NPKG4    trap      IOMssg giving Error if IO
	 FILEPI    1;NPKGFILE
         READ      NPKGFLE3,SEQ;NPKGVARS
	 trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPKGWRT
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NPKGWRT
         BRANCH    NPKGFLAG OF NPKG5
         CALL      NPKGOPEN
NPKG5    trap      IOMssg giving Error if IO
	 FILEPI    1;NPKGLIST
         WRITE     NPKGLIST;NPKGVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPKGUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE PACKAGE FILE
.
NPKGUPD  BRANCH    NPKGFLAG TO NPKG6
         CALL      NPKGOPEN
NPKG6    trap      IOMssg giving Error if IO
	 FILEPI    1;NPKGLIST
         UPDATE    NPKGLIST;NPKGVARS
	 trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPKGDEL
. REQUIRED    : VALID FILE READ
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NPKGDEL  BRANCH    NPKGFLAG TO NPKG7
         CALL      NPKGOPEN
NPKG7    trap      IOMssg giving Error if IO
	 FILEPI    1;NPKGFILE
         DELETE    NPKGLIST
	 trapclr   IO
         RETURN

..............................................................................
.
. ENTRY POINT : NPKGAIM
. REQUIRED    : NPKGFLD1,NPKGFLD2,NPKGFLD3,NPKGFLD4,NPKGFLD5
. RETURNED    : PACKAGE RECORD
. DESCRIPTION : AIM PACKAGE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPKGAIM  COMPARE   NPKGPATH TO NPKGFLAG
         GOTO      NPKG9 IF EQUAL
         CALL      NPKGOPEN
NPKG9    trap      IOMssg giving Error if IO
         branch    NPKGLOCK to NPKG9L,NPKG9R,NPKG9N
NPKG9L   FILEPI    1;NPKGFLE2
         READ      NPKGFLE2,NPKGFLD1,NPKGFLD2,NPKGFLD3,NPKGFLD4,NPKGFLD5;NPKGVARS
         trapclr   IO
         RETURN
NPKG9R   
         READLK    NPKGFLE2,NPKGFLD1,NPKGFLD2,NPKGFLD3,NPKGFLD4,NPKGFLD5;NPKGVARS
         trapclr   IO
         RETURN
NPKG9N
         READ      NPKGFLE2,NPKGFLD1,NPKGFLD2,NPKGFLD3,NPKGFLD4,NPKGFLD5;NPKGVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPKGKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : PACKAGE RECORD
. DESCRIPTION : AIM KEY GENEREIC PACKAGE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPKGKG   COMPARE   NPKGPATH TO NPKGFLAG
         GOTO      NPKG10 IF EQUAL
         CALL      NPKGOPEN
NPKG10   trap      IOMssg giving Error if IO
         branch    NPKGLOCK to NPKG10L,NPKG10R,NPKG10N
NPKG10L  FILEPI    1;NPKGFLE2
         READKG    NPKGFLE2;NPKGVARS
         trapclr   IO
         RETURN
NPKG10R  READKGLK  NPKGFLE2;NPKGVARS
         trapclr   IO
         RETURN
NPKG10N  READKG    NPKGFLE2;NPKGVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPKGKGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : PACKAGE RECORD
. DESCRIPTION : KEY GENERIC PRIOR PACKAGE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPKGKGP  COMPARE   NPKGPATH TO NPKGFLAG
         GOTO      NPKG11 IF EQUAL
         CALL      NPKGOPEN
NPKG11   trap      IOMssg giving Error if IO
         branch    NPKGLOCK to NPKG11L,NPKG11R,NPKG11N
NPKG11L  FILEPI    1;NPKGFLE2
         READKGP   NPKGFLE2;NPKGVARS
         trapclr   IO
         RETURN
NPKG11R  READKGPLK NPKGFLE2;NPKGVARS
         trapclr   IO
         RETURN
NPKG11N  READKGP   NPKGFLE2;NPKGVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPKGAIMT
. REQUIRED    : NPKGFLD1,NPKGFLD2,NPKGFLD3,NPKGFLD4,NPKGFLD5
. RETURNED    : 
. DESCRIPTION : AIM PACKAGE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPKGAIMT COMPARE   NPKGPATH TO NPKGFLAG
         GOTO      NPKG12 IF EQUAL
         CALL      NPKGOPEN
NPKG12   trap      IOMssg giving Error if IO
         branch    NPKGLOCK to NPKG12L,NPKG12R,NPKG12N
NPKG12L  FILEPI    1;NPKGFLE2
         READ      NPKGFLE2,NPKGFLD1,NPKGFLD2,NPKGFLD3,NPKGFLD4,NPKGFLD5;;
         trapclr   IO
         RETURN
NPKG12R   
         READLK    NPKGFLE2,NPKGFLD1,NPKGFLD2,NPKGFLD3,NPKGFLD4,NPKGFLD5;;
         trapclr   IO
         RETURN
NPKG12N
         READ      NPKGFLE2,NPKGFLD1,NPKGFLD2,NPKGFLD3,NPKGFLD4,NPKGFLD5;;
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPKGOPEN
. REQUIRED    : 'NPKGFLAG'
. RETURNED    : 'NPKGFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN PACKAGE FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NPKGOPEN TRAP      NPKGGONE giving Error IF IO
	 BRANCH    NPKGLOCK TO NPKGOPNL,NPKGOPNR

NPKGOPNL OPEN 	   NPKGLIST
         OPEN      NPKGFLE3,NPKGNME3
         TRAPCLR   IO
         MOVE      C1 TO NPKGFLAG
         RETURN
NPKGOPNR
        OPEN      NPKGLIST,LOCKMANUAL,SINGLE
        OPEN      NPKGFLE3,NPKGNME3
        TRAPCLR   IO
        MOVE      C1 TO NPKGFLAG
        RETURN
.
NPKGGONE MOVE      NPKGNAME TO FILENAME
         CALL      FILEGONE
..............................................................................
.END OF INCLUDE

