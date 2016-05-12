;----------------------------------------------------------------
;
;   DTPio.inc - 
;
;   Copyright(c) Names in the News Ca., Inc. 2004
;
;   Author: David Herrick
;   Created: 1/28/2004 
;----------------------------------------------------------------

..............................................................................
.
. DTPIO INCLUSION
. NIN EOM Days To Pay IO Inclusion
.
. FILE NAME : DaysToPay
. REC LENGTH: 40 VAR
. INDEX KEY : 1-7 (BRoker, Mailer)
.
..............................................................................
.
. ENTRY POINT : DTPKEY
. REQUIRED    : 'DTPFLD'
. RETURNED    : DTP RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
DTPKEY  BRANCH    DTPFLAG TO DTP1
         CALL      DTPOPEN
DTP1    FILEPI    1;DTPFILE
         READ      DTPFILE,DTPFLD;DTPBroker:   
				DTPMailer:  
				DTP3yrcount:
				DTP3yrDays:  
				DTP1yrcount:
				DTP1yrDays:
                                                            DtpFill
          RETURN
..............................................................................
.
. ENTRY POINT : DTPTST
. REQUIRED    : DTPFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
DTPTST  BRANCH    DTPFLAG TO DTP2
         CALL      DTPOPEN
DTP2    FILEPI    1;DTPFILE
         READ      DTPFILE,DTPFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : DTPKS
. REQUIRED    :
. RETURNED    : DATACARD REFERANCE RECORD
. DESCRIPTION : KEY SEQUENTIAL Days to Pay File READ
.
DTPKS   BRANCH    DTPFLAG TO DTP3
         CALL      DTPOPEN
DTP3    FILEPI    1;DTPFILE
         READKS    DTPFILE;DTPBroker:   1-1      REFERENCE CODE TYPE
				DTPMailer:  
				DTP3yrcount:
				DTP3yrDays:  
				DTP1yrcount:
				DTP1yrDays:
                                                            DtpFill  
         RETURN
..............................................................................
.
. ENTRY POINT : DTPSEQ
. REQUIRED    :
. RETURNED    : DATACARD REFERANCE RECORD
. DESCRIPTION : SEQUENTIAL Days to Pay File READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
DTPSEQ  BRANCH    DTPFLAG TO DTP4
         CALL      DTPOPEN
DTP4    FILEPI    1;DTPFILE
         READ      DTPFILE,SEQ;DTPBroker:   1-1      REFERENCE CODE TYPE
				DTPMailer:  
				DTP3yrcount:
				DTP3yrDays:  
				DTP1yrcount:
				DTP1yrDays:
                                                            DtpFill  
         RETURN
.............................................................................
.
. ENTRY POINT : DTPWRT
. REQUIRED    : 'DTPFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
DTPWRT  BRANCH    DTPFLAG TO DTP5
         CALL      DTPOPEN
DTP5    FILEPI    1;DTPFILE
         WRITE     DTPFILE,DTPFLD;DTPBroker:   1-1      REFERENCE CODE TYPE
				DTPMailer:  
				DTP3yrcount:
				DTP3yrDays:  
				DTP1yrcount:
				DTP1yrDays:
                                                            DtpFill
         RETURN
..............................................................................
.
. ENTRY POINT : DTPDEL
. REQUIRED    : 'DTPFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
DTPDEL  BRANCH    DTPFLAG TO DTP7
         CALL      DTPOPEN
DTP7    FILEPI    1;DTPFILE
         DELETE    DTPFILE,DTPFLD
         RETURN
...............................................................................
.
. ENTRY POINT : DTPOPEN
. REQUIRED    : 'DTPFLAG'
. RETURNED    : 'DTPFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN Days to Pay File
.               NOTE this routine does not call filegone
;              programmer must test for file presence.
.
DTPOPEN TRAP      DTPGONE IF IO
         OPEN      DTPFILE,DTPNAME
         TRAPCLR   IO
         MOVE      C1 TO DTPFLAG
         RETURN
.
DTPGONE MOVE      DTPNAME TO FILENAME
	noreturn
	RETURN
.
..............................................................................
