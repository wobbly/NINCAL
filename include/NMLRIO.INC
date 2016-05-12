.         05/19/03 1.0 Company Conversion Patch
.----------------------------------------------------------------
.
.   nmlrio.inc -
.
.   Copyright(c) Names in the News Ca., Inc. 2000
.
.   Author: David L Herrick
.   Created: 6/12/2002 6:02:31 AM
.   Last change: DLH 6/12/2002 6:02:31 AM
.
. Release 1.2     ASH  06JAN05 Modifed logic around Mailer Contact Names
.----------------------------------------------------------------
.Patch to allow for old association broker num
#convbrk  dim       7
CnctOldBrkKey1 external "COMP001c;CnctOldBrkKey"
#str9               dim       9
#str3     DIM 3
.
..............................................................................
.
. NMLRIO INCLUSION
. NIN MAILER FILE I/O ROUTINES
. REVISED
.
.            11Sep98 ASH added IO trap logic
.            30Mar98 DLH add option file lock, record lock, no lock
.            28Mar98 DLH new record structure see nmlrdd.inc
.            24JUL91 TO NINCA'S VARIABLE NAMES.
.
. FILE NAME : NINMLR
. REC LENGTH: 177 FIXED
. INDEX KEY : 3-9 (4 POSITION MLR#, 3 POSITION CONTACT#)
.
..............................................................................
.
. ENTRY POINT : NMLRKEY
. REQUIRED    : 'NMLRFLD'
. RETURNED    : MAILER RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NMLRKEY
.                                       BRANCH    NMLRFLAG TO NMLR1
.         CALL      NMLROPEN
.NMLR1
.                             TRAP      IOMssg Giving Error if IO
.        branch    nmlrlock to nmlr1L,nmlr1R,nmlr1N
.Nmlr1L - file locks
.NMLR1L   FILEPI    1;NMLRFILE
.         READ      NMLRFILE,MKEY;Mlrvars
.         TRAPCLR   IO
.         RETURN
.Nmlr1R - Record locks
.NMLR1R
.         READLK    NMLRFILE,MKEY;Mlrvars
.         TRAPCLR   IO
.         RETURN
.Nmlr1N - NO locks
.NMLR1N
.         READ      NMLRFILE,MKEY;Mlrvars
.         TRAPCLR   IO
.Patch1.0
                              move c3 to compconvflag
.               pack                    cnctfld4 from mkey
                              clear               MLRVARS
.                             CALL                cnctkey2
                              pack                compfld3 from mkey
.patch1.1
                              if (compfld3  = "    ")
                                        setflag over
                                        return
                              endif
.patch1.1
         call                 compkey3
.Patch1.0
         RETURN
..............................................................................
.
. ENTRY POINT : NMLRTST
. REQUIRED    : MKEY
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NMLRTST
.Patch1.0
                              move c3 to compconvflag
                              clear               MLRVARS
                              pack                COMPFLD3 from mkey
.patch1.1
                              if (compfld3  = "    ")
                                        setflag over
                                        return
                              endif
.patch1.1
         call                 COMPTST3
.Patch1.0
.                             BRANCH    NMLRFLAG TO NMLR2
.         CALL      NMLROPEN

.NMLR2
.                             TRAP      IOMssg Giving Error if IO
.         FILEPI    1;NMLRFILE
.         READ      NMLRFILE,MKEY;STR1
.         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NMLRKS
. REQUIRED    :
. RETURNED    : MAILER RECORD
. DESCRIPTION : KEY SEQUENTIAL MAILER FILE READ
.
NMLRKS
                    move c3 to compconvflag
                    CALL COMPKS3
.                   BRANCH    NMLRFLAG TO NMLR3
.        CALL      NMLROPEN
.MLR3    TRAP      IOMssg Giving Error if IO
.        branch    nmlrlock to nmlr3L,Nmlr3R,nmlr3N

.Nmlr3L   FILEPI    1;NMLRFILE
.         READKS    NMLRFILE;Mlrvars
.         TRAPCLR   IO
.         RETURN

.Nmlr3R
.         READKSLK  NMLRFILE;Mlrvars
.         TRAPCLR   IO
.         RETURN

.Nmlr3N
.         READKS    NMLRFILE;Mlrvars
.         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NMLRSEQ
. REQUIRED    :
. RETURNED    : MAILER RECORD
. DESCRIPTION : SEQUENTIAL MAILER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NMLRSEQ
.                              BRANCH    NMLRPATH TO NMLR4A,NMLRGONE,NMLR4C
                              move c3 to compconvflag
                              call  COMPSEQ
.NMLR4A   BRANCH    NMLRFLAG TO NMLR4B
.         CALL      NMLROPEN

.NMLR4B   TRAP      IOMssg Giving Error if IO
.         BRANCH    NMLRLOCK TO NMLR4BL,NMLR4BR,NMLR4BN

.NMLR4BL  FILEPI    1;NMLRFILE
.         READ      NMLRFILE,SEQ;MLRVARS
.         TRAPCLR   IO
.         RETURN
.NMLR4Br
.         READLK    NMLRFILE,SEQ;MLRVARS
.         TRAPCLR   IO
.         RETURN
.NMLR4BN
.         READ      NMLRFILE,SEQ;MLRVARS
.         TRAPCLR   IO
.         RETURN
.
.NMLR4C   BRANCH    NMLRFLG3 TO NMLR4D
.         CALL      NMLROPEN
.
.NMLR4D   TRAP      IOMssg Giving Error if IO
.         BRANCH    NMLRLOCK TO NMLR4DL,NMLR4DR,NMLR4DN
.
.NMLR4DL  FILEPI    1;NMLRFLE3
.         READ      NMLRFLE3,SEQ;MLRVARS
.         TRAPCLR   IO
.         RETURN
.NMLR4DR
.         READLK    NMLRFLE3,SEQ;MLRVARS
.         TRAPCLR   IO
.         RETURN
.NMLR4DN
.         READ      NMLRFLE3,SEQ;MLRVARS
.         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NMLRWRT
. REQUIRED    : 'MKEY'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NMLRWRT  MOVE      C1 TO NMLRPATH
NMLRWRT1 COMPARE   NMLRPATH TO NMLRFLAG
         GOTO      NMLR5 IF EQUAL
         CALL      NMLROPEN
NMLR5    TRAP      IOMssg Giving Error if IO
         FILEPI    1;NMLRFILE
         WRITE     NMLRFILE,MKEY;MLRVARS
         TRAPCLR   IO
         call      nmlrwrt2
         RETURN
.......................
NMLRWRT2 BRANCH    NMLRFLG2 TO NMLR5B
         CALL      NMLROPN2
NMLR5B   TRAP      IOMssg Giving Error if IO
         FILEPI    1;NMLRFLE2
         INSERT    NMLRFLE2
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NMLRUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE MAILER FILE
.
NMLRUPD  BRANCH    NMLRFLAG TO NMLR6
         CALL      NMLROPEN
NMLR6    TRAP      IOMssg Giving Error if IO
         FILEPI    1;NMLRFILE
         UPDATE    NMLRFILE;MLRVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NMLRDEL
. REQUIRED    : 'MKEY'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NMLRDEL  BRANCH    NMLRFLAG TO NMLR7
         CALL      NMLROPEN
NMLR7    TRAP      IOMssg Giving Error if IO
         FILEPI    1;NMLRFILE
         DELETE    NMLRFILE,MKEY
         TRAPCLR   IO
         RETURN
...............................................................................
..............................................................................
.
. ENTRY POINT : NMLRAIM
. REQUIRED    : MLRAKEY
. RETURNED    : MAILER RECORD
. DESCRIPTION : AIM MAILER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NMLRAIM
                              move c3 to compconvflag
                              MOVE "02XT" to COMPFLD7
                              CLEAR COMPFLD8
                              CLEAR COMPFLD9
                              CLEAR COMPFLD10
                              unpack    MLRAKEY,#str3
                              bump MLRAKEY,3
                              pack COMPFLD2,#str3,MLRAKEY
                              call COMPAIM
.                             BRANCH    NMLRFLG2 TO NMLR9
.         CALL      NMLROPN2
.NMLR9    TRAP      IOMssg Giving Error if IO
.         BRANCH    NMLRLOCK TO NMLR9L,NMLR9R,NMLR9N

.NMLR9L   FILEPI    1;NMLRFLE2
.         READ      NMLRFLE2,MLRAKEY;MLRVARS
.         TRAPCLR   IO
.         RETURN
.NMLR9R
.         READLK    NMLRFLE2,MLRAKEY;MLRVARS
.         TRAPCLR   IO
.         RETURN
.NMLR9N
.         READ      NMLRFLE2,MLRAKEY;MLRVARS
.         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NMLRKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : MAILER RECORD
. DESCRIPTION : AIM KEY GENEREIC MAILER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NMLRKG
                              move c3 to compconvflag
                              call COMPKG
.                             BRANCH    NMLRFLG2 TO NMLR10
.         CALL      NMLROPN2
.NMLR10   TRAP      IOMssg Giving Error if IO
.         BRANCH    NMLRLOCK TO NMLR10L,NMLR10R,NMLR10N
.
.NMLR10L  FILEPI    1;NMLRFLE2
.         READKG    NMLRFLE2;MLRVARS
.         TRAPCLR   IO
.         RETURN
.NMLR10R
.         READKGLK  NMLRFLE2;MLRVARS
.         TRAPCLR   IO
.         RETURN
.NMLR10N
.         READKG    NMLRFLE2;MLRVARS
.         TRAPCLR   IO
         RETURN
..............................................................................
.
.         IFZ      PC
. ENTRY POINT : NMLRKGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : MAILER RECORD
. DESCRIPTION : KEY GENERIC PRIOR MAILER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NMLRKGP
                              move c3 to compconvflag
                              call COMPKGP
.BRANCH    NMLRFLG2 TO NMLR11
.         CALL      NMLROPN2
.NMLR11   TRAP      IOMssg Giving Error if IO
.         BRANCH    NMLRLOCK TO NMLR11L,NMLR11R,NMLR11N
.NMLR11L  FILEPI    1;NMLRFLE2
.         READKGP   NMLRFLE2;MLRVARS
.         TRAPCLR   IO
.         RETURN
.NMLR11R
.         READKGPLK NMLRFLE2;MLRVARS
.         TRAPCLR   IO
.         RETURN
.NMLR11N
.         READKGP   NMLRFLE2;MLRVARS
.         TRAPCLR   IO
.         RETURN
.         XIF
..............................................................................
.
. ENTRY POINT : NMLROPEN
. REQUIRED    : 'NMLRFLAG'
. RETURNED    : 'NMLRFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MAILER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NMLROPEN BRANCH    NMLRPATH OF NMLROPN1,NMLROPN2,NMLROPN3
NMLROPN1 TRAP      NMLRGONE IF IO
         BRANCH    NMLRLOCK TO NMLROP,NMLROPR,NMLROP

NMLROP   OPEN      NMLRFILE,NMLRNAME
         TRAPCLR   IO
         MOVE      C1 TO NMLRFLAG
         RETURN

NMLROPR  OPEN      NMLRFILE,NMLRNAME,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NMLRFLAG
         RETURN
.
NMLROPN2
         TRAP      NMLRGONE IF IO
         BRANCH    NMLRLOCK TO NMLROP2,NMLROP2R,NMLROP2

NMLROP2  OPEN      NMLRFLE2,NMLRNAME
         TRAPCLR   IO
         MOVE      C1 TO NMLRFLG2
         RETURN
NMLROP2R OPEN      NMLRFLE2,NMLRNAME,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NMLRFLG2
         RETURN
.
NMLROPN3 TRAP      NMLRGONE IF IO
         BRANCH    NMLRLOCK TO NMLROP3,NMLROP3R,NMLROP3
NMLROP3  OPEN      NMLRFLE3,NMLRNAME
         TRAPCLR   IO
         MOVE      C1 TO NMLRFLG3
         RETURN
NMLROP3R OPEN      NMLRFLE3,NMLRNAME,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NMLRFLG3
         RETURN
.
NMLRGONE MOVE      NMLRNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................
MOVEMLRVARS
.ASH 22MAR2005 - Added this patch back in
.patch1.1
.          if (COMPMLRFLG = "T")
          if (COMPMLRFLG = "T" | CompDntCare = "T")
.patch1.1
                    move      compoldmlr,mnum
.                   unpack    cnctcnt,#str4,brkcnt
                    CALL TRIM USING CNCTFNAME
                    IF (CNCTFNAME <> "")
.START PATCH 1.2 REPLACED LOGIC
.                             move      cnctfname to mname
                              if (CNCTINACTIVE <> "T")
                                        move      cnctfname to mname
                              else
                                        CLEAR     MNAME
                              endif
.END PATCH 1.2 REPLACED LOGIC
                    ELSE
                              CLEAR MNAME
                    ENDIF
                    move      compcomp to mcomp
                    move      compaddr to maddr
                    move      compaddr2 to m2addr
                    move      compcity to mcity
                    move      compstate to mstate
                    move      compzip to mzip
                    move      compcntry to mcoun
                    IF (CNCTDATE2 = "        " OR CNCTDATE2 = "")     ;No Contact Rev DATE
                              IF (CNCTDATE = "        " OR CNCTDATE = "")       ;No Comtact Creation Date
                                        move      compruser to mpass                                                    ;Use Company Rev User
                                        move      comprdte to mrevdate                                                     ;Use Company Rev Date
                              ELSE
                                        move      CNCTUSER to mpass                                                     ;No Cnct Rev then use cnct creation user\date
                                        move      CNCTDATE to mrevdate                                                            ;No Cnct Rev then use cnct creation user\date
                              ENDIF
                    ELSE
                                        move      CNCTUSER2 to mpass                                                     ;User cnct Rev user/date
                                        move      CNCTDATE2 to mrevdate
                    ENDIF
                    move      compcredit to mstat
                    move      compfaxflag1 to mfaxoflag
.;
.Patch Add for Broker Tele Check - If Company phone and no cnctphone use compphone else use cnctphone
.move     cnctphone to brtele
                    call trim using cnctphone
                    IF (CNCTPHONE = B10 OR CNCTPHONE = "0000000000" OR CNCTPHONE = "")
.                   IF (CNCTPHONE = B10 OR CNCTPHONE = "0000000000")
                              MOVE COMPPHONE to MTELE
                    ELSE
                              MOVE CNCTPHONE to MTELE
                    ENDIF
.Patch Add for Broker Fax Check - If Company fax and no cnctfax use compfax else use cnctfax
                    call trim using cnctfax
                    IF (CNCTFAX = B10 OR CNCTFAX = "0000000000" OR CNCTFAX = "")
                              MOVE COMPFAX to MFAX
                    ELSE
                              MOVE CNCTFAX to MFAX
                    ENDIF
                    IF (CNCTSALES = "  " OR CNCTSALES = "00" OR CNCTSALES = "")
                              move      compcontact to mslsper
                    ELSE
                              MOVE      CNCTSALES to mslsper
                    ENDIF
                    if (COMPEXCHANGE = "F")
                              move "R" to MRCODE
                    else
                              move " " to MRCODE
                    endif
                    if (COMPREGCDE = "T")
                              move "Y" to mCOPIES
                    else
                              Move      " "       to mCOPIES
                    endif
.                   Move      CompbillCde to MCODE
                    if (CompBillCde = " ")
                              move "M" to MCODE
                    else
                              move      CompBillCde to MCODE
                    endif
                    if (COMPBDRCTFLG = "T")
                              move "Y" to  MBildrct
                    else
                              Move      " "       to MBildrct
                    endif
                    call trim using compbroker
                    clear mbrknum
                    clear #convbrk
                    if (compbroker <> "")
                              pack #str9 with compbroker,compbroker1
                              call CnctOldBrkKey1 using #str9,#CONVBRK
                              move #convbrk,mbrknum
                    else
                              call trim using compconsult
                              if (compconsult <> "")
                                        pack #str9 with compconsult,compconsult1
                                        call CnctOldBrkKey1 using #str9,#CONVBRK
                                        move #convbrk,mbrknum
                              endif
          endif
                    packkey   mnum,mnum
                    packkey   mcomp,mcomp
                    packkey   maddr,maddr
                    packkey   m2addr,m2addr
                    packkey   mcity,mcity
                    packkey   mstate,mstate
                    packkey   mzip,mzip
                    packkey   mcoun,mcoun
                    packkey   mname,mname
                    packkey   mrevdate,mrevdate
                    packkey   mstat,mstat
                    packkey   mfaxoflag,mfaxoflag
                    packkey   mtele,mtele
                    packkey   mfax,mfax
                    packkey   mslsper,mslsper
                    packkey   mpass,mpass
                    packkey   mrevdate,mrevdate
                    packkey   mrcode,mrcode
                    packkey   mcode,mcode
                    packkey   mcopies,mcopies
                    packkey  MBildrct,MBildrct
                    packkey  MBrkNum,MBrknum
.patch1.1
          else
                    setflag   over
          endif
.patch1.1
MOVEMLRVARSX
          return
