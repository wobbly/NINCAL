* NRTXIO/INC.
* *****************************************************************************
* NAMES IN THE NEWS MASTER RETURN-TO TAX FILE.
* *****************************************************************************
.
.    FILE:      NINRTAX
.  LENGTH:      34
.COMPRESS:      NONE
.    TYPE:      ISAM,FIXED
.     KEY:      NRTXFLD
...............................................................................
.
NRTXKEY  BRANCH    NRTXFLAG TO NRTX1
         CALL      NRTXOPEN
NRTX1    FILEPI    1;NRTXFILE
         READ      NRTXFILE,NRTXFLD;RTXNUM:    1-4   RETURN-TO NUMBER    
                                RTXCD:       5-9   TAX AREA CODE
                                RTXPERC:    10-13  TAX PERCENTAGE
                                RTXCODE:    14-14  CODE
                                RTXEXMPT    15-34  EXEMPTION NUMBER
                           RETURN
..............................................................................
.
. ENTRY POINT : NMLRTST
. REQUIRED    : NRTXFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NRTXTST  BRANCH    NRTXFLAG TO NRTX2
         CALL      NRTXOPEN
NRTX2    FILEPI    1;NRTXFILE
         READ      NRTXFILE,NRTXFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : NMLRWRT
. REQUIRED    : NRTXFLD
. RETURNED    :
. DESCRIPTION : KEY WRITE
.
NRTXWRT  BRANCH    NRTXFLAG TO NRTX5
         CALL      NRTXOPEN
NRTX5    FILEPI    1;NRTXFILE
         WRITE     NRTXFILE,NRTXFLD;RTXNUM:    1-4   RETURN-TO NUMBER    
                                RTXCD:       5-9   TAX AREA CODE
                                RTXPERC:    10-13  TAX PERCENTAGE
                                RTXCODE:    14-14  CODE
                                RTXEXMPT    15-34  EXEMPTION NUMBER
         RETURN
..............................................................................
.
. ENTRY POINT : NMLRUPD
. REQUIRED    : PREVIOUS VALID ISAM READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE
.
NRTXUPD  BRANCH    NRTXFLAG TO NRTX6
         CALL      NRTXOPEN
NRTX6    FILEPI    1;NRTXFILE
        UPDATE     NRTXFILE;RTXNUM:    1-4   RETURN-TO NUMBER
                                RTXCD:       5-9   TAX AREA CODE
                                RTXPERC:    10-13  TAX PERCENTAGE
                                RTXCODE:    14-14  CODE
                                RTXEXMPT    15-34  EXEMPTION NUMBER
         RETURN
..............................................................................
.
. ENTRY POINT : NMLRDEL
. REQUIRED    : NRTXFLD
. RETURNED    :
. DESCRIPTION : KEY RECORD DELETE
.
NRTXDEL  BRANCH    NRTXFLAG TO NRTX7
         CALL      NRTXOPEN
NRTX7    FILEPI    1;NRTXFILE
         DELETE     NRTXFILE,NRTXFLD
         RETURN
..............................................................................
.
. ENTRY POINT : NRTXOPEN
. REQUIRED    : 'NRTXFLAG'
. RETURNED    : 'NRTXFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN RETURN-TO TAX FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NRTXOPEN TRAP      NRTXGONE IF IO
         OPEN      NRTXFILE,NRTXNAME
         TRAPCLR   IO
         MOVE      C1 TO NRTXFLAG
         RETURN
NRTXGONE MOVE      NRTXNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................
