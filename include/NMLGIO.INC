..............................................................................
NMLGKEY  BRANCH    NMLGFLAG TO NMLG1
	 CALL      NMLGOPEN
NMLG1 	 FILEPI	   1;NMLGFILE
	 READ	   NMLGFILE,NMLGFLD;NMLGKEY:
		   NMLGFIL:
	           NMLGCAT
	 RETURN
..............................................................................
NMLGTST  BRANCH    NMLGFLAG TO NMLG2
	 CALL      NMLGOPEN
NMLG2 	 FILEPI	   1;NMLGFILE
	 READ	   NMLGFILE,NMLGFLD;STR1
	 RETURN
..............................................................................
NMLGKS   BRANCH    NMLGFLAG TO NMLG3
	 CALL      NMLGOPEN
NMLG3 	 FILEPI	   1;NMLGFILE
	 READKS	   NMLGFILE;NMLGKEY:
		   NMLGFIL:
	           NMLGCAT
	 RETURN
..............................................................................
NMLGSEQ  BRANCH    NMLGFLAG TO NMLG4
	 CALL      NMLGOPEN
NMLG4 	 FILEPI	   1;NMLGFILE
	 READ	   NMLGFILE,SEQ;NMLGKEY:
		   NMLGFIL:
	           NMLGCAT
	 RETURN
..............................................................................
NMLGWRT  BRANCH    NMLGFLAG TO NMLG5
	 CALL      NMLGOPEN
NMLG5 	 FILEPI	   1;NMLGFILE
	 WRITE	   NMLGFILE,NMLGFLD;NMLGKEY:
		   NMLGFIL:
	           NMLGCAT
	 RETURN
..............................................................................
NMLGDEL  BRANCH    NMLGFLAG TO NMLG7
	 CALL      NMLGOPEN
NMLG7 	 FILEPI	   1;NMLGFILE
	 DELETE	   NMLGFILE,NMLGFLD
	 RETURN
..............................................................................
NMLGOPEN TRAP      NMLGGONE IF IO
	 OPEN      NMLGFILE,NMLGNAME
	 TRAPCLR   IO
	 MOVE	   C1 TO NMLGFLAG
	 RETURN
..............................................................................
NMLGGONE MOVE      NMLGNAME TO FILENAME
	 CALL      FILEGONE
..............................................................................
