.DLH last change 28 sep 2008 add email patch 1.3
.    Last change:  DB 06 JUN 2004    ;patch 1.2
.    Last change:  DB 28 MAY 2004    1:39 pm ;Patch1.1
.    Last change:  DB 23 SEP 2003    1:39 pm ;Patch1.0
.    Last change:  AH   19 Aug 1998    1:39 pm
..............................................................................
.
. NBRKIO INCLUSION
. NIN BROKER FILE I/O ROUTINES
.
. REVISED 03AUG93 ADDED SALES PERSIN
.
. FILE NAME : NINBRK
. REC LENGTH: 151 FIXED
. INDEX KEY : 1-4,5-7 (4 POSITION BRK#, 3 POSITION CONTACT#)
.
. ASH 17JUL98  ADDED LOGIC FOR FILE/RECORD LOCKING WITH Y2K UPDATE
.              Added logic to trap for IO errors using routine
.              found in comlogic.inc with variables set in
.              driver program
#str4     dim       4
..............................................................................
.
. ENTRY POINT : NBRKKEY
. REQUIRED    : 'NBRKFLD'
. RETURNED    : BROKER RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NBRKKEY
.  BRANCH    NBRKFLAG TO NBRK1
.         CALL      NBRKOPEN
.          call     compkey
.dbadd
                              move      c2 to compconvflag
                pack          cnctfld4 from nbrkfld
                              pack      compfld4 from cnctfld4
                              if (compfld4  = "    ")
                                        setflag over
                                        return
                              endif
                              clear     NBRKVARS
                              CALL      cnctkey2

.          pack     compfld4 from cnctcnt
                              call      compkey2
         return
.NBRK1    TRAP      IOMssg Giving Error if IO
.         BRANCH    NBRKLOCK TO NBRK1L,NBRK1R,NBRK1N

...............................................................................
.dbpatch for newcompany file -turned on brkopen
.OK to delete if all reads fine
.NBRKKEYORIG  
.                             BRANCH    NBRKFLAG TO NBRK1L
.                             CALL      NBRKOPEN
.         OPEN      NBRKFILE,NBRKNAME
.         TRAPCLR   IO
.         MOVE      C1 TO NBRKFLAG
.                             TRAP      IOMssg Giving Error if IO
.         BRANCH    NBRKLOCK TO NBRK1L,NBRK1R,NBRK1N
.endpatch
.NBRK1L - file locks
NBRK1L   FILEPI    1;NBRKFILE
         READ      NBRKFILE,NBRKFLD;NBRKVARS      1-4    BROKER NUMBER. **KEY**
         TRAPCLR   IO
         RETURN

.NBRK1R - record locks
NBRK1R 
         READLK    NBRKFILE,NBRKFLD;NBRKVARS      1-4    BROKER NUMBER. **KEY**
         TRAPCLR   IO
         RETURN         

.NBRK1N - no locks
NBRK1N   
         READ      NBRKFILE,NBRKFLD;NBRKVARS      1-4    BROKER NUMBER. **KEY**
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NBRKTST
. REQUIRED    : NBRKFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NBRKTST  BRANCH    NBRKFLAG TO NBRK2
         CALL      NBRKOPEN
NBRK2    TRAP      IOMssg Giving Error if IO
         FILEPI    1;NBRKFILE
         READ      NBRKFILE,NBRKFLD;STR1
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NBRKKS
. REQUIRED    :
. RETURNED    : BROKER RECORD
. DESCRIPTION : KEY SEQUENTIAL BROKER FILE READ
.
NBRKKS   BRANCH    NBRKFLAG TO NBRK3
         CALL      NBRKOPEN
NBRK3    TRAP      IOMssg Giving Error if IO
         BRANCH    NBRKLOCK to NBRK3L,NBRK3R,NBRK3N

.NBRK3L - file locks
NBRK3L   FILEPI    1;NBRKFILE
         READKS    NBRKFILE;NBRKVARS
                              TRAPCLR   IO
         RETURN

.NBRK3R - record locks
NBRK3R 
         READKSLK  NBRKFILE;NBRKVARS 
         TRAPCLR   IO
         RETURN         

.NBRK3N - no locks
NBRK3N   
         READKS    NBRKFILE;NBRKVARS 
          TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NBRKSEQ
. REQUIRED    :
. RETURNED    : BROKER RECORD
. DESCRIPTION : SEQUENTIAL BROKER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NBRKSEQ  BRANCH    NBRKPATH TO NBRK4A,NBRKGONE,NBRK4C
NBRK4A   BRANCH    NBRKFLAG TO NBRK4B
         CALL      NBRKOPEN
NBRK4B   TRAP      IOMssg Giving Error if IO
         BRANCH    NBRKLOCK TO NBRK4BL,NBRK4BR,NBRK4BN

.NBRK4BL - file locks
NBRK4BL  FILEPI    1;NBRKFILE
         READ      NBRKFILE,SEQ;NBRKVARS
         TRAPCLR   IO
         RETURN

.NBRK4BR - record locks
NBRK4BR 
         READLK    NBRKFILE,SEQ;NBRKVARS 
         TRAPCLR   IO
         RETURN         

.NBRK4BN - no locks
NBRK4BN   
         READ      NBRKFILE,SEQ;NBRKVARS 
         TRAPCLR   IO
         RETURN

NBRK4C   BRANCH    NBRKFLG3 TO NBRK4D
         CALL      NBRKOPEN
NBRK4D   TRAP      IOMssg Giving Error if IO
         BRANCH    NBRKLOCK TO NBRK4DL,NBRK4DR,NBRK4DN

.NBRK4DL - file locks
NBRK4DL  FILEPI    1;NBRKFLE3
         READ      NBRKFLE3,SEQ;NBRKVARS
         TRAPCLR   IO
         RETURN

.NBRK4DR - record locks
NBRK4DR 
         READLK    NBRKFLE3,SEQ;NBRKVARS 
         TRAPCLR   IO
         RETURN         

.NBRK4DN - no locks
NBRK4DN   
         READ      NBRKFLE3,SEQ;NBRKVARS 
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NBRKWRT
. REQUIRED    : 'NBRKFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NBRKWRT  MOVE      C1 TO NBRKPATH
NBRKWRT1 COMPARE   NBRKPATH TO NBRKFLAG
         GOTO      NBRK5 IF EQUAL
         CALL      NBRKOPEN
NBRK5    TRAP      IOMssg Giving Error if IO
         FILEPI    1;NBRKFILE
         WRITE     NBRKFILE,NBRKFLD;NBRKVARS                               BRKCNT:       5-7    BROKER CONTACT NUMBER. 
         TRAPCLR   IO
         CALL      NBRKWRT2
         RETURN
.......................
NBRKWRT2 BRANCH    NBRKFLG2 TO NBRK5B
         CALL      NBRKOPN2
NBRK5B   TRAP      IOMssg Giving Error if IO
         FILEPI    1;NBRKFLE2
         INSERT    NBRKFLE2
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NBRKUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE BROKER FILE
.
NBRKUPD  BRANCH    NBRKFLAG TO NBRK6
         CALL      NBRKOPEN
NBRK6    TRAP      IOMssg Giving Error if IO
         FILEPI    1;NBRKFILE
         UPDATE    NBRKFILE;NBRKVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NBRKDEL
. REQUIRED    : 'NBRKFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NBRKDEL  BRANCH    NBRKFLAG TO NBRK7
         CALL      NBRKOPEN
NBRK7    TRAP      IOMssg Giving Error if IO
         FILEPI    1;NBRKFILE
         DELETE    NBRKFILE,NBRKFLD
         TRAPCLR   IO
         RETURN
...............................................................................
..............................................................................
.
. ENTRY POINT : NBRKAIM   
. REQUIRED    : BRKAKEY
. RETURNED    : BROKER RECORD
. DESCRIPTION : AIM BROKER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NBRKAIM  BRANCH    NBRKFLG2 TO NBRK9
         CALL      NBRKOPN2
NBRK9    TRAP      IOMssg Giving Error if IO
         BRANCH    NBRKLOCK TO NBRK9L,NBRK9R,NBRK9N

.NBRK9L - file locks
NBRK9L   FILEPI    1;NBRKFLE2
         READ      NBRKFLE2,NBRKFLD2;NBRKVARS
         TRAPCLR   IO
         RETURN

.NBRK9R - record locks
NBRK9R 
         READLK    NBRKFLE2,NBRKFLD2;NBRKVARS 
         TRAPCLR   IO
         RETURN         

.NBRK9N - no locks
NBRK9N   
         READ      NBRKFLE2,NBRKFLD2;NBRKVARS 
         TRAPCLR   IO
         RETURN
         
..............................................................................
.
. ENTRY POINT : NBRKKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : BROKER RECORD
. DESCRIPTION : AIM KEY GENEREIC BROKER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NBRKKG   BRANCH    NBRKFLG2 TO NBRK10
         CALL      NBRKOPN2
NBRK10   TRAP      IOMssg Giving Error if IO
         BRANCH    NBRKLOCK TO NBRK10L,NBRK10R,NBRK10N

.NBRK10L - file locks
NBRK10L  FILEPI    1;NBRKFLE2
         READKG    NBRKFLE2;NBRKVARS                               BRKCNT:       5-7    BROKER CONTACT NUMBER. 
         TRAPCLR   IO
         RETURN

.NBRK10R - record locks
NBRK10R 
         READKGLK  NBRKFLE2;NBRKVARS 
         TRAPCLR   IO
         RETURN         

.NBRK10N - no locks
NBRK10N   
         READKG    NBRKFLE2;NBRKVARS 
         TRAPCLR   IO
         RETURN
..............................................................................
. 
. ENTRY POINT : NBRKKGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : BROKER RECORD
. DESCRIPTION : KEY GENERIC PRIOR BROKER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NBRKKGP  BRANCH    NBRKFLG2 TO NBRK11
         CALL      NBRKOPN2
NBRK11   TRAP      IOMssg Giving Error if IO
         BRANCH    NBRKLOCK TO NBRK11L,NBRK11R,NBRK11N

.NBRK11L - file locks
NBRK11L  FILEPI    1;NBRKFLE2
         READKGP   NBRKFLE2;NBRKVARS
         TRAPCLR   IO
         RETURN
   
.NBRK11R - record locks
NBRK11R 
         READKGPLK NBRKFLE2;NBRKVARS 
         TRAPCLR   IO
         RETURN         

.NBRK11N - no locks
NBRK11N   
         READKGP   NBRKFLE2;NBRKVARS 
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NBRKOPEN
. REQUIRED    : 'NBRKFLAG'
. RETURNED    : 'NBRKFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN BROKER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NBRKOPEN BRANCH    NBRKPATH OF NBRKOP,NBRKOP,NBRKOP
.NBRKOPEN BRANCH    NBRKPATH OF NBRKOPN1,NBRKOPN2,NBRKOPN3
NBRKOPN1 TRAP      NBRKGONE IF IO
         BRANCH    NBRKLOCK TO NBRKOP,NBRKOPR,NBRKOP
         
NBRKOP
.         OPEN      NBRKFILE,NBRKNAME
.         TRAPCLR   IO
.         MOVE      C1 TO NBRKFLAG
          TRAP      COMPGONE IF IO
          OPEN      COMPFLIST
          TRAPCLR   IO
          MOVE      C1,COMPFLAG
         RETURN

NBRKOPR  OPEN      NBRKFILE,NBRKNAME,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NBRKFLAG
         RETURN
.
NBRKOPN2 TRAP      NBRKGONE IF IO
         BRANCH    NBRKLOCK TO NBRKOP2,NBRKOP2R,NBRKOP2

NBRKOP2  OPEN      NBRKFLE2,NBRKNAME
         TRAPCLR   IO
         MOVE      C1 TO NBRKFLG2
         RETURN

NBRKOP2R OPEN      NBRKFLE2,NBRKNAME,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NBRKFLG2
         RETURN         
         
.
NBRKOPN3 TRAP      NBRKGONE IF IO
         BRANCH    NBRKLOCK TO NBRKOP3,NBRKOP3R,NBRKOP3
         
NBRKOP3  OPEN      NBRKFLE3,NBRKNAME
         TRAPCLR   IO
         MOVE      C1 TO NBRKFLG3
         RETURN

NBRKOP3R OPEN      NBRKFLE3,NBRKNAME,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NBRKFLG3
         RETURN     
         
.
NBRKGONE MOVE      NBRKNAME TO FILENAME
         CALL      FILEGONE
.
MVBRVARS
.ASH 22MAR2005 - Added this patch back in
.patch1.1
.          if (COMPBRKFLG = "T" | COMPCLRFLG = "T")
          if (COMPBRKFLG = "T" | COMPCLRFLG = "T" | CompDntCare = "T")
.patch1.1
                    move      compoldbrk,brknum
                    unpack    cnctcnt,#str4,brkcnt
.patch1.2
                    if (brkcnt = "")
                              move z3 to brkcnt
                    endif
.patch1.2
                    move      cnctfname to brcntct
                    move      compcomp to brcomp
                    move      compaddr to braddr
                    move      compaddr2 to br2addr
                    move      compcity to brcity
                    move      compstate to brstate
                    move      compzip to brzip
                    move      compcntry to brcoun
.PATCH1.0
                    IF (CNCTDATE2 = "        " OR CNCTDATE2 = "")     ;No Contact Rev DATE
                              IF (CNCTDATE = "        " OR CNCTDATE = "")       ;No Comtact Creation Date
                                        move      compruser to brname                                                   ;Use Company Rev User
                                        move      comprdte to brrevdat                                                     ;Use Company Rev Date
                              ELSE
                                        move      CNCTUSER to brname                                                    ;No Cnct Rev then use cnct creation user\date
                                        move      CNCTDATE to brrevdat                                                            ;No Cnct Rev then use cnct creation user\date
                              ENDIF
                    ELSE
                                        move      CNCTUSER2 to brname                                                    ;User cnct Rev user/date
                                        move      CNCTDATE2 to brrevdat
                    ENDIF
.                   move      compuser to brname
.                   move      comprdte to brrevdat
.PATCH1.0
                    move      compcredit to brcredit
.PATCH
.If Company is inactive then all contacts should show inactive
.if company is ACTIVE then refer to contacts for ACTIVITY STATUS
.                   move      cnctinactive to brinactive
                    IF        (COMPINACTIVE = "T")
                              MOVE      COMPINACTIVE to BRINACTIVE
                    ELSE
                              MOVE      CNCTINACTIVE to BRINACTIVE
                    ENDIF
                    move      compfaxflag2 to brfaxoflag
.;
.Patch Add for Broker Tele Check - If Company phone and no cnctphone use compphone else use cnctphone
.move     cnctphone to brtele
.patch1.0
                    IF (CNCTPHONE = B10 OR CNCTPHONE = "0000000000" OR CNCTPHONE = "")
.                   IF (CNCTPHONE = B10 OR CNCTPHONE = "0000000000")
.patch1.0
                              MOVE COMPPHONE to BRTELE
                    ELSE
                              MOVE CNCTPHONE to BRTELE
                    ENDIF
.;
.Patch Add for Broker Fax Check - If Company fax and no cnctfax use compfax else use cnctfax
.                   move      cnctfax to brfax
.patch1.0
.                   IF (CNCTFAX = B10 OR CNCTFAX = "0000000000")
                    IF (CNCTFAX = B10 OR CNCTFAX = "0000000000" OR CNCTFAX = "")
.patch1.0
                              MOVE COMPFAX to BRFAX
                    ELSE
                              MOVE CNCTFAX to BRFAX
                    ENDIF
.Patch1.0
                    IF (CNCTSALES = "  " OR CNCTSALES = "00" OR CNCTSALES = "")
                              move      compcontact to brsales
                    ELSE
                              MOVE      CNCTSALES to BRSALES
                    ENDIF
.Patch1.0
.begin patch 1.3
                    IF (CNCTEmail = "  "  OR CNCTSALES = "")
                              move      compEmail to brEmail
                    ELSE
                              MOVE      CNCTEmail to BREmail
                    ENDIF
.end patch 1.3

                    packkey   brknum,brknum
                    packkey   brkcnt,brkcnt
                    packkey   brcntct,brcntct
                    packkey   brcomp,brcomp
                    packkey   braddr,braddr
                    packkey   br2addr,br2addr
                    packkey   brcity,brcity
                    packkey   brstate,brstate
                    packkey   brzip,brzip
                    packkey   brcoun,brcoun
                    packkey   brname,brname
                    packkey   brrevdat,brrevdat
                    packkey   brcredit,brcredit
                    packkey   brinactive,brinactive
                    packkey   brfaxoflag,brfaxoflag
                    packkey   brtele,brtele
                    packkey   brfax,brfax
                    packkey   brsales,brsales
.begin patch1.3
                    packkey   brEmail,brEmail
.end patch1.3
.patch1.1
          else
                    setflag over
          endif
.patch1.1
          return
.         include   \\nts0\c\library\include\compio.inc
.         include   \\nts0\c\library\include\cntio.inc.
..............................................................................
