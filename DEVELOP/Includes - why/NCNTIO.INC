..............................................................................
.
. NCNTIO INCLUSION
. NIN CONTACT NUMBER IO INCLUSION
.
. FILE NAME : NINCNT
.
..............................................................................
.
. ENTRY POINT : NCNTKEY
. REQUIRED    : NCNTFLD
. RETURNED    : NCNTVARS
. DESCRIPTION : KEY REad
.
.
NCNTKEY  BRANCH    NCNTPATH TO NCNT1A,NCNT1END,NCNT1C,Ncnt1E
NCNT1A   COMPARE   C1,NCNTFLAG
         goto       NCNT1B if equal
         call       NCNTOPEN 
NCNT1B   trap      IOMssg giving Error if IO
.         FILEPI    1;NCNTFILE
         READ      NCNTFILE,NCNTFLD;CNTVARS
         goto NCNT1END
NCNT1C   COMPARE   C1,NCNTFLG1
         goto       NCNT1D if equal
         call      NCNTOPEN 
NCNT1D   trap      IOMssg giving Error if IO
.         FILEPI    1;NCNTFIL1
         READ      NCNTFIL1,NCNTFLD1;CNTVARS
NCNT1END
         trapclr   IO
         RETURN
NCNT1E   COMPARE   C1,NCNTFLAG
         goto       NCNT1F if equal
         call       NCNTOPEN 
NCNT1F   trap      IOMssg giving Error if IO
         FILEPI    1;NCNTFIL4
         READ      NCNTFIL4,NCNTFLD4;CNTVARS
         goto NCNT1END

.NCNTKEY  BRANCH    NCNTFLAG OF NCNT1
.         CALL      NCNTOPEN
.NCNT1    trap      IOMssg giving Error if IO
.         FILEPI    1;NCNTFILE
.         READ      NCNTFILE,NCNTFLD;CNTVARS
.         trapclr   IO
.         RETURN
..............................................................................
.
. ENTRY POINT : NCNTTST
. REQUIRED    : NCNTFLD.NCNTFLD1
. RETURNED    :
. DESCRIPTION : TEST KEY
.NCNTPATH        FORM    1        .1=cntnum,2=aim(left over),3=cntport,4=salesnumber

.
NCNTTST  BRANCH    NCNTPATH TO NCNT2A,NCNT2END,NCNT2C,ncnt2E
NCNT2A   COMPARE   C1,NCNTFLAG
         goto NCNT2B if equal
         call      NCNTOPEN 
NCNT2B   trap      IOMssg giving Error if IO
.         FILEPI    1;NCNTFILE
         READ      NCNTFILE,NCNTFLD;;
         goto NCNT2END
NCNT2C   COMPARE   C1,NCNTFLG1
         goto NCNT2D if equal
         call      NCNTOPEN 
NCNT2D   trap      IOMssg giving Error if IO
.         FILEPI    1;NCNTFIL1
         READ      NCNTFIL1,NCNTFLD1;;
NCNT2END
         trapclr   IO
         RETURN
NCNT2e   COMPARE   C1,NCNTFLAG
         goto      NCNT2f if equal
         call      NCNTOPEN 
NCNT2f   trap      IOMssg giving Error if IO
         FILEPI    1;NCNTFIL4
         READ      NCNTFIL4,NCNTFLD4;;
         goto NCNT2END

..............................................................................
.
. ENTRY POINT : NCNTKS
. REQUIRED    :
. RETURNED    : RECORD NUMBER RECORD
. DESCRIPTION : KEY SEQUENTIAL RECORD NUMBER FILE READ
.
NCNTKS   BRANCH    NCNTPATH TO NCNT3A,NCNT3END,NCNT3C,ncnt3E
NCNT3A   COMPARE   C1,NCNTFLAG
         goto NCNT3B if equal
         call      NCNTOPEN 
NCNT3B   trap      IOMssg giving Error if IO
.         FILEPI    1;NCNTFILE
         READKS    NCNTFILE;CNTVARS
         goto NCNT3END
NCNT3C   COMPARE   C1,NCNTFLG1
         goto NCNT3D if equal
         call      NCNTOPEN 
NCNT3D   trap      IOMssg giving Error if IO
.         FILEPI    1;NCNTFIL1
         READKS    NCNTFIL1;CNTVARS
NCNT3END
         trapclr   IO
         RETURN
NCNT3e   COMPARE   C1,NCNTFLAG
         goto 	NCNT3f if equal
         call      NCNTOPEN 
NCNT3f   trap      IOMssg giving Error if IO
         FILEPI    1;NCNTFIL4
         READKS    NCNTFIL4;CNTVARS
         goto NCNT3END
..............................................................................
.
. ENTRY POINT : NCNTSEQ
. REQUIRED    :
. RETURNED    : RECORD NUMBER RECORD
. DESCRIPTION : SEQUENTIAL RECORD NUMBER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCNTSEQ  BRANCH    NCNTPATH TO NCNT4A,NCNT4END,NCNT4C,Ncnt4E
NCNT4A   COMPARE   C1,NCNTFLAG
         goto NCNT4B if equal
         call      NCNTOPEN 
NCNT4B   trap      IOMssg giving Error if IO
.         FILEPI    1;NCNTFILE
         READ      NCNTFILE,SEQ;CNTVARS
         goto NCNT4END
NCNT4C   COMPARE   C1,NCNTFLG1
         goto NCNT4D if equal
         call      NCNTOPEN 
NCNT4D   trap      IOMssg giving Error if IO
.         FILEPI    1;NCNTFIL1
         READ      NCNTFIL1,SEQ;CNTVARS
NCNT4END
         trapclr   IO
         RETURN
NCNT4E   COMPARE   C1,NCNTFLaG
         goto 	NCNT4F if equal
         call      NCNTOPEN 
NCNT4F   trap      IOMssg giving Error if IO
         FILEPI    1;NCNTFIL4
         READ      NCNTFIL4,SEQ;CNTVARS
         goto NCNT4END

..............................................................................
.
. ENTRY POINT : NCNTSEQ0
. REQUIRED    :
. RETURNED    : RECORD NUMBER RECORD
. DESCRIPTION : SEQUENTIAL RECORD NUMBER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCNTSEQ0 BRANCH    NCNTPATH TO NCNT4XA,NCNT4XEND,NCNT4XC
NCNT4XA  COMPARE   C1,NCNTFLAG
         goto NCNT4XB if equal
         call      NCNTOPEN 
NCNT4XB  trap      IOMssg giving Error if IO
.         FILEPI    1;NCNTFILE
         READ      NCNTFILE,C0;CNTVARS
         goto NCNT4END
NCNT4XC  COMPARE   C1,NCNTFLG1
         goto NCNT4XD if equal
         call      NCNTOPEN 
NCNT4XD  trap      IOMssg giving Error if IO
.         FILEPI    1;NCNTFIL1
         READ      NCNTFIL1,C0;CNTVARS
NCNT4XEND
         trapclr   IO
         RETURN

.NCNTSEQ0 BRANCH    NCNTFLAG TO NCNT4A
.         CALL      NCNTOPEN
.NCNT4A   trap      IOMssg giving Error if IO
.         FILEPI    1;NCNTFILE
.         READ      NCNTFILE,C0;CNTVARS
.         trapclr   IO
.         RETURN
..............................................................................
.
. ENTRY POINT : NCNTWRT
. REQUIRED    : 'NCNTFLD','NCNTFLD1'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NCNTWRT  MOVE      C1 TO NCNTPATH
NCNTWRT1 BRANCH    NCNTFLAG TO NCNT5
         CALL      NCNTOPEN
NCNT5    trap      IOMssg giving Error if IO
         FILEPI    1;NCNTFList
         WRITE     NCNTFList;CNTVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCNTUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
NCNTUPD
.BRANCH    NCNTPATH TO NCNT6A,NCNT6END,NCNT6C
NCNT6A   COMPARE   C1,NCNTFLAG
         goto NCNT6B if equal
         call      NCNTOPEN 
NCNT6B   trap      IOMssg giving Error if IO
         FILEPI    1;NCNTFList
         UPDATE    NCNTFList;CNTVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCNTDEL
. REQUIRED    : 'NCNTFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NCNTDEL
.BRANCH    NCNTPATH TO NCNT7A,NCNT7END,NCNT7C
NCNT7A   COMPARE   C1,NCNTFLAG
         goto NCNT7B if equal
         call      NCNTOPEN 
NCNT7B   trap      IOMssg giving Error if IO
.         FILEPI    1;NCNTFILE
.         DELETE    NCNTFILE,NCNTFLD
         FILEPI    1;NCNTFList
         DELETE    NCNTFList
         trapclr   IO
         RETURN
.NCNT7C   COMPARE   C1,NCNTFLG1
.         goto NCNT7D if equal
.         call      NCNTOPEN 
.NCNT7D   trap      IOMssg giving Error if IO
.         FILEPI    1;NCNTFIL1
.         DELETE    NCNTFIL1,NCNTFLD1
.NCNT7END
.         trapclr   IO
.         RETURN

.NCNTDEL  BRANCH    NCNTFLAG TO NCNT7
.         CALL      NCNTOPEN
.NCNT7    trap      IOMssg giving Error if IO
.         FILEPI    1;NCNTFILE
.         DELETE    NCNTFILE,NCNTFLD
.         trapclr   IO
.         RETURN
..............................................................................
. ENTRY POINT : NCNTAIM
. REQUIRED    : NCNTFLD2
. RETURNED    : CONTACT RECORD
. DESCRIPTION : AIM CONTACT FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCNTAIM  BRANCH    NCNTPATH TO NCNT8A,NCNT8a,NCNT8a

NCNT8A	 BRANCH    NCNTFLaG TO NCNT8B
         CALL      NCNTOPeN
NCNT8B   trap      IOMssg giving Error if IO
NCNT8L   FILEPI    1;NCNTFIL2
         READ      NCNTFIL2,NCNTFLD2;CNTVARS
         trapclr   IO
         RETURN
.NCNT8R   
.         READLK    NCNTFIL2,NCNTFLD2;CNTVARS
.         trapclr   IO
.         RETURN         
.NCNT8N   
.         READ      NCNTFIL2,NCNTFLD2;CNTVARS
.         trapclr   IO
.         RETURN




..............................................................................
.
. ENTRY POINT : NCNTKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : MAILER RECORD
. DESCRIPTION : AIM KEY GENEREIC MAILER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCNTKG   
NCNT10A	 BRANCH    NCNTFLaG TO NCNT10B
         CALL      NCNTOPeN
NCNT10B  trap      IOMssg giving Error if IO
         BRANCH    NCNTLOCK TO NCNT10L,NCNT10R,NCNT10N
NCNT10L  FILEPI    1;NCNTFIL2
         READKG    NCNTFIL2;CNTVARS
         trapclr   IO
         RETURN
NCNT10R  
         READKGLK  NCNTFIL2;CNTVARS
         trapclr   IO
         RETURN         
NCNT10N  
         READKG    NCNTFIL2;CNTVARS
         trapclr   IO
         RETURN

...............................................................................
.
. ENTRY POINT : NCNTOPEN
. REQUIRED    : 'NCNTFLAG','NCNTFLG1','NCNTFLG2'
. RETURNED    : 'NCNTFLAG','NCNTFLG1','NCNTFLG2' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN RECORD NUMBER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NCNTOPEN BRANCH    NCNTPATH OF NCNTOPN1,NCNTOPN2,NCNTOPN3
NCNTOPN1 MOVE      NCNTNME1,NCNTNAME
         TRAP      NCNTGONE IF IO
         BRANCH    NCNTLOCK TO NCNTOP,NCNTOPR,NCNTOP
         
NCNTOP    OPEN      NCNTFLIST

.         OPEN      NCNTFILE,NCNTNAME
         TRAPCLR   IO
         MOVE      C1 TO NCNTFLAG
         RETURN
NCNTOPR  OPEN      NCNTFLIST,LOCKMANUAL,SINGLE
.OPEN      NCNTFILE,NCNTNAME,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NCNTFLAG
         RETURN         
.
NCNTOPN2 MOVE      NCNTNME1,NCNTNAME
         TRAP      NCNTGONE IF IO
         BRANCH    NCNTLOCK TO NCNTOP2,NCNTOP2R,NCNTOP2
         
NCNTOP2             OPEN      NCNTFLIST
.OPEN      NCNTFIL2,NCNTNAME
         TRAPCLR   IO
         MOVE      C1 TO NCNTFLG2
         RETURN
NCNTOP2R   OPEN      NCNTFLIST,LOCKMANUAL,SINGLE
.OPEN      NCNTFIL2,NCNTNAME,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NCNTFLG2
         RETURN         
.
NCNTOPN3 MOVE      NCNTNME2,NCNTNAME
         TRAP      NCNTGONE IF IO
         BRANCH    NCNTLOCK TO NCNTOP3,NCNTOP3R,NCNTOP3
         
NCNTOP3             OPEN      NCNTFLIST
.  OPEN      NCNTFIL1,NCNTNAME
         TRAPCLR   IO
         MOVE      C1 TO NCNTFLG1
         RETURN
NCNTOP3R   OPEN      NCNTFLIST,LOCKMANUAL,SINGLE
. OPEN      NCNTFIL1,NCNTNAME,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NCNTFLG1
         RETURN   
.
NCNTGONE MOVE      NCNTNAME TO FILENAME
          CALL      FILEGONE
.
