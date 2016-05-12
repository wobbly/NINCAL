..............................................................................
.
. NDATIO INCLUSION
. NIN DATACARD FILE I/O ROUTINES
.
. FILE NAME : NINDAT
. REC LENGTH: 600 FIXED
. INDEX KEY : (1)  2-7       LIST#
. AIMDEX KEYS:(1) 58-132     LIST NAME
.             (2) 64-138     MLSTNAME
.             (3) 14-19      DATFUL
.               (4)  1-1           STATUS
.               (5) 32-32          NEW BYTE
.               (6) 33-33      EXCLUSIVE BYTE

.
.28 AUgust 2012 DLH - new file Nindat5.isi
. patch 1.3 29JUNE2006 DMS - Add search, as per 6/12/2006 CTF Meeting
. patch 1.2 21JUNE2006 ASH - Added aamdex for Fulfillment Number
.APRIL 22, 2004 ASH ADDED NDATBUSY & NDATRELEASE
.
..............................................................................
.
. ENTRY POINT : NDATKEY
. REQUIRED    : 'NDATPATH', 'NDATFLD'
. RETURNED    : DATACARD
. DESCRIPTION : EXACT ISAM KEY READ
.
NDATKEY
          BRANCH    NDATPATH,NDAT1,NDAT1X,NDAT1B,Ndat1D
NDAT1
          BRANCH    NDATFLAG,NDAT1A
          CALL      NDATOPEN
NDAT1A
          BRANCH    NDATLOCK,NDAT1Al,NDAT1AR,NDAT1AN
.with file locks
NDAT1Al
          trap      IOMssg giving Error if IO
          FILEPI    1;NDATFILE
          READ      NDATFILE,NDATFLD;DATVARS
          GOTO NDAT1x
.with record locks . note file open must have had flag set
NDAT1AR
          trap      IOMssg giving Error if IO
          READLK    NDATFILE,NDATFLD;DATVARS
          GOTO NDAT1X
.with NO locks
NDAT1AN
          trap      IOMssg giving Error if IO
          READ      NDATFILE,NDATFLD;DATVARS
          GOTO NDAT1X
NDAT1B
          BRANCH    NDATFLAG,NDAT1C
          CALL      NDATOPEN
NDAT1C
          BRANCH    NDATLOCK,NDAT1Cl,NDAT1CR,NDAT1CN
NDAT1CL
          trap      IOMssg giving Error if IO
          FILEPI    1;NDATFLE3
          READ      NDATFLE3,NDATFLD3;DATVARS
          GOTO NDAT1X
.with record locks . note file open must have had flag set
NDAT1CR
          trap      IOMssg giving Error if IO
          READLK    NDATFLE3,NDATFLD3;DATVARS
          GOTO NDAT1X
.with NO locks
NDAT1CN
          trap      IOMssg giving Error if IO
          READ      NDATFLE3,NDATFLD3;DATVARS
          GOTO NDAT1X
NDAT1D
          IF        (NDATFLAG <> 1)
          CALL      NDATOPEN
          endif
          BRANCH    NDATLOCK,NDAT1Dl,NDAT1DR,NDAT1DN
.with file locks
NDAT1Dl
          trap      IOMssg giving Error if IO
          FILEPI    1;NDATFLE4
          READ      NDATFLE4,NDATFLD;DATVARS
          GOTO NDAT1x
.with record locks . note file open must have had flag set
NDAT1DR
          trap      IOMssg giving Error if IO
          READLK    NDATFLE4,NDATFLD;DATVARS
          GOTO NDAT1X
.with NO locks
NDAT1DN
          trap      IOMssg giving Error if IO
          READ      NDATFLE4,NDATFLD;DATVARS
          GOTO NDAT1X

NDAT1X
          trapclr   IO
          RETURN
..............................................................................
.
. ENTRY POINT : NDATTST
. REQUIRED    : NDATFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NDATTST
          BRANCH    NDATPATH,NDAT2,NDAT2X,NDAT2B
NDAT2
          BRANCH    NDATFLAG,NDAT2A
          CALL      NDATOPEN
NDAT2A
          trap      IOMssg giving Error if IO
          FILEPI    1;NDATFILE
          READ      NDATFILE,NDATFLD;;
          GOTO NDAT2X
NDAT2B
          BRANCH    NDATFLAG,NDAT2C
          CALL      NDATOPEN
NDAT2C
          trap      IOMssg giving Error if IO
          FILEPI    1;NDATFLE3
          READ      NDATFLE3,NDATFLD3;;
NDAT2X
          trapclr   IO
          RETURN
..............................................................................
.
. ENTRY POINT : NDATKS
. REQUIRED    : 'NDATPATH'
. RETURNED    : DATACARD
. DESCRIPTION : KEY SEQUENTIAL DATACARD FILE READ
.
NDATKS
          BRANCH    NDATPATH,NDAT3A,NDAT3X,NDAT3B,Ndat3D
NDAT3
          BRANCH    NDATFLAG,NDAT3A
          CALL      NDATOPEN
NDAT3A
          BRANCH    NDATLOCK,NDAT3Al,NDAT3AR,NDAT3AL
NDAT3Al
          trap      IOMssg giving Error if IO
          FILEPI    1;NDATFILE
          READKS    NDATFILE;DATVARS
          GOTO NDAT3X
NDAT3AR
          trap      IOMssg giving Error if IO
          READKSLK NDATFILE;DATVARS
          GOTO NDAT3X
NDAT3AN
          trap      IOMssg giving Error if IO
          READKS    NDATFILE;DATVARS
          GOTO NDAT3X
NDAT3B
          BRANCH    NDATFLAG,NDAT3C
          CALL      NDATOPEN
NDAT3C
          BRANCH    NDATLOCK,NDAT3CL,NDAT3CR,NDAT3CN
NDAT3CL
          trap      IOMssg giving Error if IO
          FILEPI    1;NDATFLE3
          READKS    NDATFLE3;DATVARS
          GOTO NDAT3X
NDAT3CR
          trap      IOMssg giving Error if IO
          READKSLK NDATFLE3;DATVARS
          GOTO NDAT3X
NDAT3CN
          trap      IOMssg giving Error if IO
          READKS    NDATFLE3;DATVARS
          GOTO NDAT3X
.read only managed files
NDAT3D
          BRANCH    NDATLOCK,NDAT3DL,NDAT3DR,NDAT3DN
NDAT3DL
          trap      IOMssg giving Error if IO
          FILEPI    1;NDATFLE4
          READKS    NDATFLE4;DATVARS
          GOTO NDAT3X
NDAT3DR
          trap      IOMssg giving Error if IO
          READKSLK NDATFLE4;DATVARS
          GOTO NDAT3X
NDAT3DN
          trap      IOMssg giving Error if IO
          READKS    NDATFLE4;DATVARS
          GOTO NDAT3X
NDAT3X
          trapclr   IO
          RETURN
..............................................................................
.
. ENTRY POINT : NDATSEQ
. REQUIRED    :
. RETURNED    : DATACARD
. DESCRIPTION : SEQUENTIAL DATACARD FILE READ
.
NDATSEQ
          BRANCH    NDATFLAG,NDAT4
          CALL      NDATOPEN
NDAT4
          trap      IOMssg giving Error if IO
          FILEPI    1;NDATFILE
          READ      NDATFILE,SEQ;DATVARS
          trapclr   IO
          RETURN
..............................................................................
.
. ENTRY POINT : NDATWRT
. REQUIRED    : 'NDATFLD', 'NDATPATH=1'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT., AIM INSERTS.
.
NDATWRT
          BRANCH    NDATFLAG,NDAT5
          CALL      NDATOPEN
NDAT5
          trap      IOMssg giving Error if IO
          FILEPI    1;NDATFLIST
          WRITE     NDATFLIST;DATVARS
          trapclr   IO
          RETURN
..............................................................................
.
. ENTRY POINT : NDATUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD
.
NDATUPD
          BRANCH    NDATFLAG,NDAT6
          CALL      NDATOPEN
NDAT6
          trap      IOMssg giving Error if IO
          FILEPI    1;NDATFLIST
          UPDATE    NDATFLIST;DATVARS
          trapclr   IO
          RETURN
..............................................................................
.
. ENTRY POINT : NDATDEL
. REQUIRED    : 'NDATPATH', 'NDATFLD'
. RETURNED    :
. DESCRIPTION : PATH=1 DELETES TXT & ISAM KEYS.
.
NDATDEL
          BRANCH    NDATFLAG,NDAT7
          CALL      NDATOPEN
NDAT7
          trap      IOMssg giving Error if IO
          FILEPI    1;NDATFLIST
          DELETE    NDATFLIST
          trapclr   IO
          RETURN
..............................................................................
.
. ENTRY POINT : NDATKP
. REQUIRED    : 'NDATPATH'
. RETURNED    : DATACARD DATa
. DESCRIPTION : KEY SEQUENTIAL prior DATACARD FILE READ
.
NDATKP
          BRANCH    NDATPATH,NDAT8,NDAT8X,NDAT8B,Ndat8D
NDAT8
          BRANCH    NDATFLAG,NDAT8A
          CALL      NDATOPEN
NDAT8A
          BRANCH    NDATLOCK,NDAT8AL,NDAT8AR,NDAT8AN
NDAT8AL
          trap      IOMssg giving Error if IO
          FILEPI    1;NDATFILE
          READKP    NDATFILE;DATVARS
          GOTO NDAT8X
NDAT8AR
          trap      IOMssg giving Error if IO
          READKPLK NDATFILE;DATVARS
          GOTO NDAT8X
NDAT8AN
          trap      IOMssg giving Error if IO
          READKP    NDATFILE;DATVARS
          GOTO NDAT8X
NDAT8B
          BRANCH    NDATFLAG,NDAT8C
          CALL      NDATOPEN
NDAT8C
          BRANCH    NDATLOCK,NDAT8CL,NDAT8CR,NDAT8CN
NDAT8CL
          trap      IOMssg giving Error if IO
          FILEPI    1;NDATFLE3
          READKP    NDATFLE3;DATVARS
          GOTO NDAT8X
NDAT8CR
          trap      IOMssg giving Error if IO
          READKPlk NDATFLE3;DATVARS
          GOTO NDAT8X
NDAT8CN
          trap      IOMssg giving Error if IO
          READKP    NDATFLE3;DATVARS
          GOTO NDAT8X
NDAT8D
          BRANCH    NDATLOCK,NDAT8DL,NDAT8DR,NDAT8DN
NDAT8DL
          trap      IOMssg giving Error if IO
          FILEPI    1;NDATFLE4
          READKP    NDATFLE4;DATVARS
          GOTO NDAT8X
NDAT8DR
          trap      IOMssg giving Error if IO
          READKPlk NDATFLE4;DATVARS
          GOTO NDAT8X
NDAT8DN
          trap      IOMssg giving Error if IO
          READKP    NDATFLE4;DATVARS
          GOTO NDAT8X
NDAT8X
          trapclr   IO
          RETURN
..............................................................................
.
. ENTRY POINT : NDATAIM
. REQUIRED    : NDATFLD1,NDATFLD2,NDATFLD4,NDATFLD5,NDATFLD6,NDATFLD7
. RETURNED    : DATACARD RECORD
. DESCRIPTION : AIM DATACARD FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NDATAIM
          BRANCH    NDATFLAG,NDAT9
          CALL      NDATOPEN
NDAT9
          BRANCH    NDATLOCK,NDAT9l,NDAT9R,NDAT9N
NDAT9L
          trap      IOMssg giving Error if IO
          FILEPI    1;NDATFLE2
.START PATCH 1.3 REPLACED LOGIC
..START PATCH 1.2 REPLACED LOGIC
..        READ      NDATFLE2,NDATFLD2;DATVARS
.         READ      NDATFLE2,NDATFLD1,NDATFLD2,NDATFLD4;DATVARS
.          READ      NDATFLE2,NDATFLD1,NDATFLD2,NDATFLD4,NDATFLD5,NDATFLD6,NDATFLD7;DATVARS
          READ      NDATFLE2,NDATFLD1,NDATFLD2,NDATFLD4,NDATFLD5,NDATFLD6,NDATFLD7,NDATFLD8;DATVARS
..END PATCH 1.2 REPLACED LOGIC
.END PATCH 1.3 REPLACED LOGIC
          trapclr   IO
          RETURN
NDAT9r
          trap      IOMssg giving Error if IO
.START PATCH 1.3 REPLACED LOGIC
..START PATCH 1.2 REPLACED LOGIC
..        READLK    NDATFLE2,NDATFLD2;DATVARS
.         READLK    NDATFLE2,NDATFLD1,NDATFLD2,NDATFLD4;DATVARS
          READLK    NDATFLE2,NDATFLD1,NDATFLD2,NDATFLD4,NDATFLD5,NDATFLD6,NDATFLD7,NDATFLD8;DATVARS
..END PATCH 1.2 REPLACED LOGIC
.END PATCH 1.3 REPLACED LOGIC
          trapclr   IO
          RETURN
NDAT9N
          trap      IOMssg giving Error if IO
.START PATCH 1.3 REPLACED LOGIC
..START PATCH 1.2 REPLACED LOGIC
..        READ      NDATFLE2,NDATFLD2;DATVARS
.         READ      NDATFLE2,NDATFLD1,NDATFLD2,NDATFLD4;DATVARS
          READ      NDATFLE2,NDATFLD1,NDATFLD2,NDATFLD4,NDATFLD5,NDATFLD6,NDATFLD7,NDATFLD8;DATVARS
..END PATCH 1.2 REPLACED LOGIC
.END PATCH 1.3 REPLACED LOGIC
          trapclr   IO
          RETURN
..............................................................................
.
. ENTRY POINT : NDATKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : DATACARD RECORD
. DESCRIPTION : AIM KEY GENEREIC DATACARD FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NDATKG
          BRANCH    NDATFLAG,NDAT10
          CALL      NDATOPEN
NDAT10
          BRANCH    NDATLOCK,NDAT10L,NDAT10R,NDAT10N
NDAT10L
          trap      IOMssg giving Error if IO
          FILEPI    1;NDATFLE2
          READKG    NDATFLE2;DATVARS
          trapclr   IO
          RETURN
NDAT10R
          trap      IOMssg giving Error if IO
          READKGLK NDATFLE2;DATVARS
          trapclr   IO
          RETURN
NDAT10N
          trap      IOMssg giving Error if IO
          READKG    NDATFLE2;DATVARS
          trapclr   IO
          RETURN
..............................................................................
.
.         IFZ      PC
. ENTRY POINT : NDATKGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : DATACARD RECORD
. DESCRIPTION : KEY GENERIC PRIOR DATACARD FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NDATKGP
          BRANCH    NDATFLAG,NDAT11
          CALL      NDATOPEN
NDAT11
          BRANCH    NDATLOCK,NDAT11L,NDAT11R,NDAT11N
NDAT11L
          trap      IOMssg giving Error if IO
          FILEPI    1;NDATFLE2
          READKGP   NDATFLE2;DATVARS
          trapclr   IO
          RETURN
NDAT11R
          trap      IOMssg giving Error if IO
          READKGPLK NDATFLE2;DATVARS
          trapclr   IO
          RETURN
NDAT11N
          trap      IOMssg giving Error if IO
          READKGP   NDATFLE2;DATVARS
          trapclr   IO
          RETURN
.         XIF
..............................................................................
.
. ENTRY POINT : NDATBUSY
. REQUIRED    : NDATFLD
. RETURNED    : DATACARD RECORD WITH NDATBUSY SET TO "*"
. DESCRIPTION : CALLS NDATTST THEN UPDATES BUSY BYTE
.
NDATBUSY MOVE      C1,NDATPATH
         BRANCH    NDATFLAG TO NDAT12
         CALL      NDATOPEN
NDAT12   CALL      NDATTST
         if over
                   move   "NDATBUSY",Location
                   clear  Error
                   append NDATFLD,Error
                   append " - Not Found in File!",Error
                   reset  Error
                   goto   IOMssg
         endif
         MOVE      STAR,NDATBUSY
         TRAP      IOMssg Giving Error if IO
         FILEPI    1;NDATFLIST
         UPDATAB   NDATFLIST;*518,NDATBUSY
         TRAPCLR   IO
         RETURN
.
..............................................................................
.
. ENTRY POINT : NDATRELEASE
. REQUIRED    : NDATFLD
. RETURNED    : DATACARD RECORD WITH NDATBUSY SET TO " "
. DESCRIPTION : CALLS NDATTST THEN RELEASES BUSY BYTE
.
NDATRELEASE
         MOVE      C1,NDATPATH
         BRANCH    NDATFLAG TO NDAT13
         CALL      NDATOPEN
NDAT13   CALL      NDATTST
         MOVE      " ",NDATBUSY
         TRAP      IOMssg Giving Error if IO
         FILEPI    1;NDATFLIST
         UPDATAB   NDATFLIST;*518,NDATBUSY
         TRAPCLR   IO
         RETURN
...............................................................................
.
. ENTRY POINT : NDATOPEN
. REQUIRED    : 'NDATPATH', 'NDATFLAG'
. RETURNED    : 'NDATFLAG' SET TO THE PATH NUMBER: 'NDATPATH', IF OPENED.
. DESCRIPTION : OPEN NIN DATACARD FILE, DEPENDENT ON PATH NUMBER.
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NDATOPEN
          LOAD      NDATNAME USING NDATPATH FROM NDATNME1,NDATNME2
          TRAP      NDATGONE IF IO
          BRANCH    NDATLOCK,NDATOPENl,NDATOPENr
NDATOPENl
          OPEN      NDATFLIST
          TRAPCLR   IO
          MOVE      C1,NDATFLAG
          RETURN
NDATOPENr
          OPEN      NDATFLIST,LOCKMANUAL,SINGLE
          TRAPCLR   IO
          MOVE      C1,NDATFLAG
          RETURN
NDATGONE
          MOVE      NDATNAME,FILENAME
          CALL      FILEGONE
