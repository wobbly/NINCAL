..............................................................................
.
. NSPEIO INCLUSION
. NIN ORDER 00/99/98 FILE DEFINITION
.
. FILE NAME : NINSPEC
. REC LENGTH: 288 COMP
. INDEX KEY : 1-2 
. Last Update Added logic to thwart I44 errors for defunct Key values - forcing setting of OVER flag - ASH 5/20/04
. Last Update Added trap IO trap logic ASH May 11,1999
. Last update aDD nspelock DLH 10Jun98
..............................................................................
.
. ENTRY POINT : NSPEKEY
. REQUIRED    : 'NSPEFLD'
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSPEKEY
. Last Update Added logic to thwart I44 errors for defunct Key values - forcing setting of OVER flag - ASH 5/20/04
.START PATCH 5/20/2004 ADDED LOGIC - ASH
	 call      Trim using NSPEFLD
	 if (NSPEFLD = "")
		   flagrestore	"0001"
		   return
	 endif
.END PATCH 5/20/2004 ADDED LOGIC - ASH
         BRANCH    NSPEFLAG TO NSPE1
         CALL      NSPEOPEN
         
NSPE1    trap      IOMssg giving Error if IO
         branch    nspelock of nspe1L,Nspe1R,Nspe1N
.default FIlepi's active
Nspe1L   FILEPI    1;NSPEFILE
         READ      NSPEFILE,NSPEFLD;NSPEVARS
         trapclr   IO
         RETURN
.with record locks . note file open must have had flag set
Nspe1R
         READLK    NSPEFILE,NSPEFLD;NSPEVARS
         trapclr  IO
         RETURN
.No locks
Nspe1N
         READ      NSPEFILE,NSPEFLD;NSPEVARS
         trapclr  IO
         RETURN
..............................................................................
.
. ENTRY POINT : NSPETST
. REQUIRED    : MKEY
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NSPETST  BRANCH    NSPEFLAG TO NSPE2
         CALL      NSPEOPEN
NSPE2    trap      IOMssg giving Error if IO
.         FILEPI    1;NSPEFILE
         READ      NSPEFILE,NSPEFLD;STR1
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NSPEKS
. REQUIRED    :
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : KEY SEQUENTIAL SPECIAL INSTRUCTION FILE READ
.
NSPEKS   BRANCH    NSPEFLAG TO NSPE3
         CALL      NSPEOPEN
NSPE3    trap      IOMssg giving Error if IO
         BRANCH    NSPELOCK OF NSPE3L,NSPE3R,NSPE3N

NSPE3L   FILEPI    1;NSPEFILE
         READKS    NSPEFILE;NSPEVARS
         trapclr   IO
         RETURN
NSPE3R
         READKSLK  NSPEFILE;NSPEVARS
         trapclr   IO
         RETURN
NSPE3N
         READKS    NSPEFILE;NSPEVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NSPESEQ
. REQUIRED    :
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : SEQUENTIAL SPECIAL INSTRUCTION FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSPESEQ  BRANCH    NSPEFLAG TO NSPE4
         CALL      NSPEOPEN
NSPE4    trap      IOMssg giving Error if IO
         BRANCH    NSPELOCK OF NSPE4L,NSPE4R,NSPE4N
NSPE4L   FILEPI    1;NSPEFILE
         READ      NSPEFILE,SEQ;NSPEVARS
         trapclr   IO
         RETURN
NSPE4R
         READLK    NSPEFILE,SEQ;NSPEVARS
         trapclr   IO
         RETURN
NSPE4N
         READ      NSPEFILE,SEQ;NSPEVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NSPEWRT
. REQUIRED    : 'MKEY'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NSPEWRT  BRANCH    NSPEFLAG OF NSPE5
         CALL      NSPEOPEN
NSPE5    trap      IOMssg giving Error if IO
         BRANCH    NSPELOCK TO NSPE5L,NSPE5R
NSPE5L   FILEPI    1;NSPEFILE
         WRITE     NSPEFILE,NSPEFLD;*+,NSPEVARS
         trapclr   IO
         RETURN
NSPE5R
         WRITE     NSPEFILE,NSPEFLD;*+,NSPEVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NSPEDEL
. REQUIRED    : 'MKEY'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NSPEDEL  BRANCH    NSPEFLAG TO NSPE7
         CALL      NSPEOPEn
NSPE7    trap      IOMssg giving Error if IO
         FILEPI    1;NSPEFILE
         DELETE    NSPEFILE,NSPEFLD
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NSPEOPEN
. REQUIRED    : 'NSPEFLAG'
. RETURNED    : 'NSPEFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN SPECIAL INSTRUCTION FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NSPEOPEN TRAP      NSPEGONE IF IO
         BRANCH    NSPELOCK TO NSPEOPNL,NSPEOPNR

NSPEOPNL OPEN      NSPEFILE,NSPENAME
         TRAPCLR   IO
         MOVE      C1 TO NSPEFLAG
         RETURN
NSPEOPNR OPEN      NSPEFILE,NSPENAME,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NSPEFLAG
         RETURN

.
NSPEGONE MOVE      NSPENAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................

