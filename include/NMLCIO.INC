*******************************************************************************
*  NMLCIO/INC  - I-O INCLUDE FOR MLRCATS FILE.                            *
*******************************************************************************
*  CREATED -  03/07/91                                                        *
*******************************************************************************
.
NMLCKEY  BRANCH    NMLCFLAG TO NMLC1
	 CALL      NMLCOPEN
NMLC1	 FILEPI    1;NMLCFILE
         READ      NMLCFILE,NMLCFLD;NMLCNUM:
 		   NMLCCAT
         RETURN
..............................................................................
NMLCTST  BRANCH    NMLCFLAG TO NMLC2
	 CALL      NMLCOPEN
NMLC2	 FILEPI    1;NMLCFILE
         READ      NMLCFILE,NMLCFLD;STR1;
         RETURN
..............................................................................
NMLCKS   BRANCH    NMLCFLAG TO NMLC3
	 CALL      NMLCOPEN
NMLC3	 FILEPI	   1;NMLCFILE
	 READ      NMLCFILE,SEQ;NMLCNUM:
		   NMLCCAT
	 RETURN
.
..............................................................................
.
NMLCWRT  BRANCH    NMLCFLAG TO NMLC5
	 CALL      NMLCOPEN
NMLC5	 FILEPI    1;NMLCFILE
         WRITE      NMLCFILE,NMLCFLD;NMLCNUM:
 		   NMLCCAT
         RETURN
..............................................................................
.
NMLCDEL  BRANCH    NMLCFLAG TO NMLC7
	 CALL      NMLCOPEN
NMLC7	 FILEPI    1;NMLCFILE
         DELETE   NMLCFILE,NMLCFLD
         RETURN
..........................................................................
NMLCOPEN TRAP      NMLCGONE IF IO
	 OPEN      NMLCFILE,NMLCNAME
	 TRAPCLR   IO
	 MOVE      C1 TO NMLCFLAG
	 RETURN
.............................................................................
NMLCGONE MOVE      NMLCNAME TO FILENAME
	 CALL      FILEGONE
..............................................................................
.