           include    nmlrio.inc
           include  nbrkio.inc
;..............................................................................
;.
;. COMPIO INCLUSION
;. NIN Master Company  FILE I/O ROUTINES
;.
;       Patch 1.3  20JUNE2006  ASH - Added New AAMFile for Associated Service Bureaus
;       Patch 1.2  05Apr21  ASH - Added New AAMFile for Associated Consultant/Brokers
.          Patch 1.1  07MAR05  ASH - Added logic started by DB(undocumented) to loop through Mailer Contacts until Active Contact is found
;..............................................................................
;.
;. ENTRY POINT : COMPKEY
;. REQUIRED    : 'COMPFLD'
;. RETURNED    : Company RECORD
;. DESCRIPTION : EXACT ISAM KEY READ
;.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;.
COMPKEY
           BRANCH     COMPFLAG,COMP1,COMP2,COMP3
           CALL       COMPOPEN
COMP1
           TRAP       IOMssg Giving Error if IO
           BRANCH     COMPLOCK,COMP1L,COMP1R,COMP1N
COMP1L
           FILEPI     1;COMPFILE
           READ       COMPFILE,COMPFLD;COMPVARS
   TRAPCLR   IO
           RETURN
COMP1R
           READLK     COMPFILE,COMPFLD;COMPVARS
   TRAPCLR   IO
           RETURN
COMP1N
           READ       COMPFILE,COMPFLD;COMPVARS
   TRAPCLR   IO
           RETURN
;Broker
;..............................................................................
;.
;. ENTRY POINT : COMPKEY2
;. REQUIRED    : 'COMPFLD4'
;. RETURNED    : Company RECORD
;. DESCRIPTION : EXACT ISAM KEY READ
;.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
COMPKEY2
           BRANCH     COMPFLAG,COMP2
           CALL       COMPOPEN
COMP2
           TRAP       IOMssg Giving Error if IO
           BRANCH     COMPLOCK,COMP2L,COMP2R,COMP2N
COMP2L
           FILEPI     1;COMPFILE4
           READ       COMPFILE4,COMPFLD4;COMPVARS
           if over
                      clear      NBRKVARS
                      setflag over
           else
                      CALL    mvbrvars
           endif
   TRAPCLR   IO
           RETURN
COMP2R
           READLK     COMPFILE4,COMPFLD4;COMPVARS
   TRAPCLR   IO
           RETURN
COMP2N
           READ       COMPFILE4,COMPFLD4;COMPVARS
   TRAPCLR   IO
           RETURN
;Mailer
;..............................................................................
;.
;. ENTRY POINT : COMPKEY3
;. REQUIRED    : 'COMPFLD3'
;. RETURNED    : Company RECORD
;. DESCRIPTION : EXACT ISAM KEY READ
;.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
COMPKEY3
           BRANCH     COMPFLAG,COMP3
           CALL       COMPOPEN
COMP3
           TRAP       IOMssg Giving Error if IO
           BRANCH     COMPLOCK,COMP3L,COMP3R,COMP3N
COMP3L
           FILEPI     1;COMPFILE3
           READ       COMPFILE3,COMPFLD3;COMPVARS
           if over
                      clear      MLRVARS
                      setflag over
           else
                      packkey cnctfld2 to "01X",compnum
                      call       CNCTAIM
                      loop
                      until over
                      until (CNCTTYPE = "1" & CNCTINACTIVE <> "T")
                                 call       CNCTKG
                      repeat
                      CALL    MOVEMLRVARS
           endif
   TRAPCLR   IO
           RETURN
COMP3R
           READLK     COMPFILE3,COMPFLD3;COMPVARS
           if over
                      clear      MLRVARS
                      setflag over
           else
                      packkey cnctfld2 to "01X",compnum
                      call       CNCTAIM
                      loop
                      until over
                      until (CNCTTYPE = "1" & CNCTINACTIVE <> "T")
                                 call       CNCTKG
                      repeat
                      CALL    MOVEMLRVARS
           endif
           RETURN
COMP3N
           READ       COMPFILE3,COMPFLD3;COMPVARS
           if over
                      clear      MLRVARS
                      setflag over
           else
                      packkey cnctfld2 to "01X",compnum
                      call       CNCTAIM
                      loop
                      until over
                      until (CNCTTYPE = "1" & CNCTINACTIVE <> "T")
                                 call       CNCTKG
                      repeat
                      CALL    MOVEMLRVARS
           endif
           RETURN

.START PATCH 1.3 ADDED LOGIC
.Fufillment
..............................................................................
.
. ENTRY POINT : COMPKEY6 - Naming Scheme relates to naming in COMPDD.INC
. REQUIRED    : 'COMPFLD6'
. RETURNED    : Company RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
COMPKEY6
           BRANCH     COMPFLAG,COMP3F
           CALL       COMPOPEN
COMP3F
           TRAP       IOMssg Giving Error if IO
           READ       COMPFILE6,COMPFLD6;COMPVARS
           if not over
                      packkey cnctfld2 to "01X",compnum
                      call       CNCTAIM
                      loop
                                 until over
                                 until (CNCTTYPE = "4" & CNCTINACTIVE <> "T")
                                 call       CNCTKG
                      repeat
                      //As Contact calls will affect OVER flag, we need to hand set it here - for calling programs
                      setflag    not over
           endif
           TRAPCLR   IO
           RETURN
.END PATCH 1.3 ADDED LOGIC
;..............................................................................
;.
;. ENTRY POINT : COMPTST
;. REQUIRED    : COMPFLD
;. RETURNED    :
;. DESCRIPTION : TEST KEY
;.
COMPTST
           BRANCH     COMPFLAG,COMP2T
           CALL       COMPOPEN
COMP2T
           TRAP       IOMssg Giving Error if IO
           BRANCH     COMPLOCK,COMP2LT,COMP2RT,COMP2NT
COMP2LT
           FILEPI     1;COMPFILE
           READ       COMPFILE,COMPFLD;;
   TRAPCLR   IO
           RETURN
COMP2RT
           READLK     COMPFILE,COMPFLD;;
   TRAPCLR   IO
           RETURN
COMP2NT
           READ       COMPFILE,COMPFLD;;
   TRAPCLR   IO
           RETURN
;patchdb
;. ENTRY POINT : COMPTST2
;. REQUIRED    : COMPFLD4
;. RETURNED    :
;. DESCRIPTION : Broker TEST KEY
;.
.Broker
COMPTST2
           TRAP       IOMssg Giving Error if IO
           BRANCH     COMPFLAG,COMP2T2
           CALL       COMPOPEN
COMP2T2
           BRANCH     COMPLOCK,COMP2LT2,COMP2RT2,COMP2NT2
COMP2LT2
           FILEPI     1;COMPFILE4
           READ       COMPFILE4,COMPFLD4;;
   TRAPCLR   IO
           RETURN
COMP2RT2
           READLK     COMPFILE4,COMPFLD4;;
   TRAPCLR   IO
           RETURN
COMP2NT2
           READ       COMPFILE4,COMPFLD4;;
   TRAPCLR   IO
           RETURN
;patchdb
;Patch1.0
;------------------------------------------------------------------------------
;. ENTRY POINT : COMPTST3
;. REQUIRED    : COMPFLD3
;. RETURNED    :
;. DESCRIPTION : Mailer TEST KEY
;Mailer
COMPTST3
           TRAP       IOMssg Giving Error if IO
           BRANCH     COMPFLAG,COMP2T3
           CALL       COMPOPEN
COMP2T3
           TRAP       IOMssg Giving Error if IO
           BRANCH     COMPLOCK,COMP2LT3,COMP2RT3,COMP2NT3
COMP2LT3
           FILEPI     1;COMPFILE3
           READ       COMPFILE3,COMPFLD3;;
   TRAPCLR   IO
           RETURN
COMP2RT3
           READLK     COMPFILE3,COMPFLD3;;
   TRAPCLR   IO
           RETURN
COMP2NT3
           READ       COMPFILE3,COMPFLD3;;
   TRAPCLR   IO
           RETURN
;patch1.0

.START PATCH 1.3 ADDED LOGIC
..............................................................................
. ENTRY POINT : COMPTST6
. REQUIRED    : COMPFLD6
. RETURNED    :
. DESCRIPTION : Fulfillment TEST KEY
.Fulfillment
COMPTST6
           TRAP       IOMssg Giving Error if IO
           BRANCH     COMPFLAG,COMP2F3
           CALL       COMPOPEN
COMP2F3
           TRAP       IOMssg Giving Error if IO
           READ       COMPFILE6,COMPFLD6;;
           TRAPCLR    IO
           RETURN
.END PATCH 1.3 ADDED LOGIC

;..............................................................................
;.
;. ENTRY POINT : COMPKS
;. REQUIRED    :
;. RETURNED    : Company RECORD
;. DESCRIPTION : KEY SEQUENTIAL Company FILE READ
;.
COMPKS
           BRANCH     COMPFLAG,COMP3A
           CALL       COMPOPEN
COMP3A
           TRAP       IOMssg Giving Error if IO
           BRANCH     COMPLOCK,COMP3AL,COMP3AR,COMP3AN
COMP3AL
           FILEPI     1;COMPFILE
           READKS     COMPFILE;COMPVARS
   TRAPCLR   IO
           RETURN
COMP3AR
           READKSLK   COMPFILE;COMPVARS
   TRAPCLR   IO
           RETURN
COMP3AN
           READKS     COMPFILE;COMPVARS
   TRAPCLR   IO
           RETURN
;Patch1.0
;..............................................................................
;.
;. ENTRY POINT : COMPKS2
;. REQUIRED    :
;. RETURNED    : Broker/Company RECORD
;. DESCRIPTION : KEY SEQUENTIAL Company FILE READ
;.
COMPKS2
           BRANCH     COMPFLAG,COMP3A2
           CALL       COMPOPEN
COMP3A2
           TRAP       IOMssg Giving Error if IO
           BRANCH     COMPLOCK,COMP3AL2,COMP3AR2,COMP3AN2
COMP3AL2
           FILEPI     1;COMPFILE4
           READKS     COMPFILE4;COMPVARS
           if over
                      clear      NBRKVARS
                      setflag over
           else
                      CALL    mvbrvars
           endif
   TRAPCLR   IO
           RETURN
COMP3AR2
           READKSLK   COMPFILE4;COMPVARS
           if over
                      clear      NBRKVARS
                      setflag over
           else
                      CALL    mvbrvars
           endif
   TRAPCLR   IO
           RETURN
COMP3AN2
           READKS     COMPFILE4;COMPVARS
           if over
                      clear      NBRKVARS
                      setflag over
           else
                      CALL    mvbrvars
           endif
   TRAPCLR   IO
           RETURN
;..............................................................................
;.
;. ENTRY POINT : COMPKS3
;. REQUIRED    :
;. RETURNED    : Mailer/Company RECORD
;. DESCRIPTION : KEY SEQUENTIAL Company FILE READ
;.
COMPKS3
           BRANCH     COMPFLAG,COMP3A3
           CALL       COMPOPEN
COMP3A3
           TRAP       IOMssg Giving Error if IO
           BRANCH     COMPLOCK,COMP3AL3,COMP3AR3,COMP3AN3
COMP3AL3
           FILEPI     1;COMPFILE3
           READKS     COMPFILE3;COMPVARS
           if over
                                 clear      MLRVARS
                                 setflag over
                      else
;;
                                 if (compconvflag = 3)
                                            packkey cnctfld2 to "01X",compnum
                                            call       CNCTAIM
                                            loop
                                            until over
                                            until (CNCTTYPE = "1" & CNCTINACTIVE <> "T")
                                                       call       CNCTKG
                                            repeat
;
                                            CALL    MOVEMLRVARS
                      endif
           endif
   TRAPCLR   IO
           RETURN
COMP3AR3
           READKSLK   COMPFILE3;COMPVARS
           if over
                      clear      MLRVARS
                      setflag over
           else
                      if (compconvflag = 3)
;if we allow multiple contacts for mailer this needs to change.
                                 packkey cnctfld2 to "01X",compnum
                                 call       CNCTAIM
                                 loop
                                 until over
                                 until (CNCTTYPE = "1" & CNCTINACTIVE <> "T")
                                            call       CNCTKG
                                 repeat
                                 CALL    MOVEMLRVARS
                      endif
           endif
   TRAPCLR   IO
           RETURN
COMP3AN3
           READKS     COMPFILE3;COMPVARS
           if over
                      clear      MLRVARS
                      setflag over
           else
                                 if (compconvflag = 3)
;if we allow multiple contacts for mailer this needs to change.
                                            packkey cnctfld2 to "01X",compnum
                                            call       CNCTAIM
                                            loop
                                            until over
                                            until (CNCTTYPE = "1" & CNCTINACTIVE <> "T")
                                                       call       CNCTKG
                                            repeat
                                            CALL    MOVEMLRVARS
                                 endif
           endif
   TRAPCLR   IO
           RETURN
;Patch1.0

.START PATCH 1.3 ADDED LOGIC
..............................................................................
.
. ENTRY POINT : COMPKS6
. REQUIRED    :
. RETURNED    : Fulfillment/Company RECORD
. DESCRIPTION : KEY SEQUENTIAL Company FILE READ
.
COMPKS6
           BRANCH     COMPFLAG,COMP3AF3
           CALL       COMPOPEN
COMP3AF3
           TRAP       IOMssg Giving Error if IO
           READKS     COMPFILE6;COMPVARS
           if not over
                      packkey cnctfld2 to "01X",compnum
                      call       CNCTAIM
                      loop
                                 until over
                                 until (CNCTTYPE = "4" & CNCTINACTIVE <> "T")
                                 call       CNCTKG
                      repeat
                      //As Contact calls will affect OVER flag, we need to hand set it here - for calling programs
                      setflag    not over
           endif
           TRAPCLR   IO
           RETURN
.END PATCH 1.3 ADDED LOGIC

;..............................................................................
;Patch1.0
;..............................................................................
;. ENTRY POINT : COMPSEQ
;. REQUIRED    :
;. RETURNED    : Company RECORD
;. DESCRIPTION : SEQUENTIAL Company FILE READ
;.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
;.
COMPSEQ
.          BRANCH     COMPCONVFLAG,COMPKS,COMPKS2,COMPKS3
           BRANCH     COMPFLGSEQ,COMP4
           CALL       COMPOPEN2
COMP4
           TRAP       IOMssg Giving Error if IO
           BRANCH     COMPLOCK,COMP4L,COMP4R,COMP4N
COMP4L
           FILEPI     1;COMPFILE7
           READ       COMPFILE7,SEQ;COMPVARS
           if over
                      if (compconvflag = 2)
                                 clear NBRKVARS
                      elseif (compconvflag = 3)
                                 clear      MLRVARS
                      endif
                      setflag over
           else
.START PATCH 1.1 REPLACED LOGIC
.                     if (compconvflag = 2)
.                                call MVBRVARS
.                     elseif (compconvflag = 3)
.                                call       MOVEMLRVARS
.                     endif
                      if (compconvflag = 2)
                                 call MVBRVARS
                      elseif (compconvflag = 3)
                                 packkey cnctfld2 to "01X",compnum
                                 call       CNCTAIM
                                 loop
                                 until over
                                 until (CNCTTYPE = "1" & CNCTINACTIVE <> "T")
                                            call       CNCTKG
                                 repeat
                                 call       MOVEMLRVARS
                      endif
.END PATCH 1.1 REPLACED LOGIC
           endif
   TRAPCLR   IO
           RETURN
COMP4r
           READLK     COMPFILE7,SEQ;COMPVARS
           if over
                      if (compconvflag = 2)
                                 clear NBRKVARS
                      elseif (compconvflag = 3)
                                 clear      MLRVARS
                      endif
                      setflag over
           else
.START PATCH 1.1 REPLACED LOGIC
.                     if (compconvflag = 2)
.                                call MVBRVARS
.                     elseif (compconvflag = 3)
.                                call       MOVEMLRVARS
.                     endif
                      if (compconvflag = 2)
                                 call MVBRVARS
                      elseif (compconvflag = 3)
                                 packkey cnctfld2 to "01X",compnum
                                 call       CNCTAIM
                                 loop
                                 until over
                                 until (CNCTTYPE = "1" & CNCTINACTIVE <> "T")
                                            call       CNCTKG
                                 repeat
                                 call       MOVEMLRVARS
                      endif
.END PATCH 1.1 REPLACED LOGIC
           endif
   TRAPCLR   IO
           RETURN
COMP4N
           READ       COMPFILE7,SEQ;COMPVARS
           if over
                      if (compconvflag = 2)
                                 clear NBRKVARS
                      elseif (compconvflag = 3)
                                 clear      MLRVARS
                      endif
                      setflag over
           else
.START PATCH 1.1 REPLACED LOGIC
.                     if (compconvflag = 2)
.                                call MVBRVARS
.                     elseif (compconvflag = 3)
.                                call       MOVEMLRVARS
.                     endif
                      if (compconvflag = 2)
                                 call MVBRVARS
                      elseif (compconvflag = 3)
                                 packkey cnctfld2 to "01X",compnum
                                 call       CNCTAIM
                                 loop
                                 until over
                                 until (CNCTTYPE = "1" & CNCTINACTIVE <> "T")
                                            call       CNCTKG
                                 repeat
                                 call       MOVEMLRVARS
                      endif
.END PATCH 1.1 REPLACED LOGIC
           endif
   TRAPCLR   IO
           RETURN
;..............................................................................
;.
;. ENTRY POINT : COMPWRT
;. REQUIRED    : 'COMPFLD'
;. RETURNED    :
;. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
;.
COMPWRT
           BRANCH     COMPFLAG,COMP5
           CALL       COMPOPEN
COMP5
           TRAP       IOMssg Giving Error if IO
           FILEPI     1;COMPFLIST
           WRITE      COMPFLIST;COMPVARS
   TRAPCLR   IO
           RETURN
;..............................................................................
;.
;. ENTRY POINT : COMPUPD
;. REQUIRED    : A PREVIOUS KEY READ
;. RETURNED    :
;. DESCRIPTION : KEY UPDATE Company FILE
;.
COMPUPD
           BRANCH     COMPFLAG,COMP6
           CALL       COMPOPEN
COMP6
           TRAP       IOMssg Giving Error if IO
           FILEPI     1;COMPFLIST
           UPDATE     COMPFLIST;COMPVARS
   TRAPCLR   IO
           RETURN
;..............................................................................
;.
;. ENTRY POINT : COMPDEL
;. REQUIRED    : 'COMPFLD'
;. RETURNED    :
;. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
;.
COMPDEL
           BRANCH     COMPFLAG,COMP7
           CALL       COMPOPEN
COMP7
           TRAP       IOMssg Giving Error if IO
           FILEPI     1;COMPFLIST
           DELETE     COMPFLIST
           RETURN
;.............................................................................
;
; ENTRY POINT : COMPAIM
; REQUIRED    : COMPFLD2
; RETURNED    : COMPANY RECORD
; DESCRIPTION : AIM COMPANY FILE READ
;               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;
.COMPAIM
.          BRANCH     COMPFLAG,COMP9
.          CALL       COMPOPEN
.COMP9
.          TRAP       IOMssg Giving Error if IO
.          BRANCH     COMPLOCK,COMP9L,COMP9R,COMP9N
.COMP9L
.          FILEPI     1;COMPFILE2
.          READ       COMPFILE2,COMPFld2;COMPVARS
.          TRAPCLR    IO
.          RETURN
.COMP9R
.          READLK     COMPFILE2,COMPFld2;COMPVARS
.          TRAPCLR    IO
.          RETURN
.COMP9N
.          READ       COMPFILE2,COMPFld2;COMPVARS
.          TRAPCLR    IO
.          RETURN
;.............................................................................
;
; ENTRY POINT : COMPAIM
; REQUIRED    : COMPFLD2,COMPFLD7,COMPFLD8,COMPFLD9,COMPFLD10
; RETURNED    : COMPANY RECORD
; DESCRIPTION : AIM COMPANY FILE READ  with flags for mailer,broker,owner,reurnto
;               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;
COMPAIM
           BRANCH     COMPFLAG,COMP9
           CALL       COMPOPEN
COMP9
           TRAP       IOMssg Giving Error if IO
           BRANCH     COMPLOCK,COMP9L,COMP9R,COMP9N
COMP9L
           FILEPI     1;COMPFILE2
           READ       COMPFILE2,COMPFLD2,COMPFLD7,COMPFLD8,COMPFLD9,COMPFLD10,COMPFLD13;COMPVARS
           if over
                      if (compconvflag = 2)
                                 clear NBRKVARS
                      elseif (compconvflag = 3)
                                 clear      MLRVARS
                      endif
                      setflag over
           else
.START PATCH 1.1 REPLACED LOGIC
.                     if (compconvflag = 2)
.                                call MVBRVARS
.                     elseif (compconvflag = 3)
.                                MOVE z3 TO MCONTCT
.                                call       MOVEMLRVARS
.                     endif
                      if (compconvflag = 2)
                                 call MVBRVARS
                      elseif (compconvflag = 3)
                                 packkey cnctfld2 to "01X",compnum
                                 call       CNCTAIM
                                 loop
                                 until over
                                 until (CNCTTYPE = "1" & CNCTINACTIVE <> "T")
                                            call       CNCTKG
                                 repeat
                                 call       MOVEMLRVARS
                      endif
.END PATCH 1.1 REPLACED LOGIC
           endif
           TRAPCLR    IO
           RETURN
COMP9R
           READLK     compfile2,COMPFLD2,COMPFLD7,COMPFLD8,COMPFLD9,COMPFLD10,COMPFLD13;COMPVARS
           if over
                      if (compconvflag = 2)
                                 clear NBRKVARS
                      elseif (compconvflag = 3)
                                 clear      MLRVARS
                      endif
                      setflag over
           else
.START PATCH 1.1 REPLACED LOGIC
.                     if (compconvflag = 2)
.                                call MVBRVARS
.                     elseif (compconvflag = 3)
..START PATCH 11/9/2004 ADDED LOGIC
.                                MOVE z3 TO MCONTCT
..END PATCH 11/9/2004 ADDED LOGIC
.                                call       MOVEMLRVARS
.                     endif
                      if (compconvflag = 2)
                                 call MVBRVARS
                      elseif (compconvflag = 3)
                                 packkey cnctfld2 to "01X",compnum
                                 call       CNCTAIM
                                 loop
                                 until over
                                 until (CNCTTYPE = "1" & CNCTINACTIVE <> "T")
                                            call       CNCTKG
                                 repeat
                                 call       MOVEMLRVARS
                      endif
.END PATCH 1.1 REPLACED LOGIC
           endif
           TRAPCLR    IO
           RETURN
COMP9N
           READ       compfile2,COMPFLD2,COMPFLD7,COMPFLD8,COMPFLD9,COMPFLD10,COMPFLD13;COMPVARS
           if over
                      if (compconvflag = 2)
                                 clear NBRKVARS
                      elseif (compconvflag = 3)
                                 clear      MLRVARS
                      endif
                      setflag over
           else
.START PATCH 1.1 REPLACED LOGIC
.                     if (compconvflag = 2)
.                                call MVBRVARS
.                     elseif (compconvflag = 3)
..START PATCH 11/9/2004 ADDED LOGIC
.                                MOVE z3 TO MCONTCT
..END PATCH 11/9/2004 ADDED LOGIC
.                                call       MOVEMLRVARS
.                     endif
                      if (compconvflag = 2)
                                 call MVBRVARS
                      elseif (compconvflag = 3)
                                 packkey cnctfld2 to "01X",compnum
                                 call       CNCTAIM
                                 loop
                                 until over
                                 until (CNCTTYPE = "1" & CNCTINACTIVE <> "T")
                                            call       CNCTKG
                                 repeat
                                 call       MOVEMLRVARS
                      endif
.END PATCH 1.1 REPLACED LOGIC
           endif
           TRAPCLR    IO
           RETURN
;.............................................................................
;
; ENTRY POINT : COMPKG
; REQUIRED    : VALID PREVIOUS AIM READ
; RETURNED    : COMPANY RECORD
; DESCRIPTION : AIM KEY GENEREIC COMPANY FILE READ
;               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;
.COMPKG
.          BRANCH     COMPFLAG,COMP10
.          CALL       COMPOPEN
.COMP10
.          TRAP       IOMssg Giving Error if IO
.          BRANCH     COMPLOCK,COMP10L,COMP10R,COMP10N
.COMP10L
.          FILEPI     1;COMPFILE2
.          READKG     COMPFILE2;COMPVARS
.          TRAPCLR    IO
.          RETURN
.COMP10R
.          READKGLK COMPFILE2;COMPVARS
.          TRAPCLR    IO
.          RETURN
.COMP10N
.          READKG     COMPFILE2;COMPVARS
.          TRAPCLR    IO
.          RETURN
;.............................................................................
;
; ENTRY POINT : COMPKG
; REQUIRED    : VALID PREVIOUS AIM READ OF COMPANY AND FLAG
; RETURNED    : COMPANY RECORD
; DESCRIPTION : AIM KEY GENEREIC COMPANY FILE READ
;               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;
COMPKG
           BRANCH     COMPFLAG,COMP10
           CALL       COMPOPEN
COMP10
           TRAP       IOMssg Giving Error if IO
           BRANCH     COMPLOCK,COMP10L,COMP10R,COMP10N
COMP10L
           FILEPI     1;compfile2
           READKG     compfile2;COMPVARS
           if over
                      if (compconvflag = 2)
                                 clear NBRKVARS
                      elseif (compconvflag = 3)
                                 clear      MLRVARS
                      endif
                      setflag over
           else
                      if (compconvflag = 2)
                                 call MVBRVARS
                      elseif (compconvflag = 3)
                                 packkey cnctfld2 to "01X",compnum
                                 call       CNCTAIM
                                 loop
                                 until over
                                 until (CNCTTYPE = "1" & CNCTINACTIVE <> "T")
                                            call       CNCTKG
                                 repeat
                                 call       MOVEMLRVARS
                      endif
           endif
           TRAPCLR    IO
           RETURN
COMP10R
           READKGLK compfile2;COMPVARS
           if over
                      if (compconvflag = 2)
                                 clear NBRKVARS
                      elseif (compconvflag = 3)
                                 clear      MLRVARS
                      endif
                      setflag over
           else
                      if (compconvflag = 2)
                                 call MVBRVARS
                      elseif (compconvflag = 3)
                                 packkey cnctfld2 to "01X",compnum
                                 call       CNCTAIM
                                 loop
                                 until over
                                 until (CNCTTYPE = "1" & CNCTINACTIVE <> "T")
                                            call       CNCTKG
                                 repeat
                                 call       MOVEMLRVARS
                      endif
           endif
           TRAPCLR    IO
           RETURN
COMP10N
           READKG     compfile2;COMPVARS
           if over
                      if (compconvflag = 2)
                                 clear NBRKVARS
                      elseif (compconvflag = 3)
                                 clear      MLRVARS
                      endif
                      setflag over
           else
.START PATCH 1.1 REPLACED LOGIC
.                     if (compconvflag = 2)
.                                call MVBRVARS
.                     elseif (compconvflag = 3)
.                                call       MOVEMLRVARS
.                     endif
                      if (compconvflag = 2)
                                 call MVBRVARS
                      elseif (compconvflag = 3)
                                 packkey cnctfld2 to "01X",compnum
                                 call       CNCTAIM
                                 loop
                                 until over
                                 until (CNCTTYPE = "1" & CNCTINACTIVE <> "T")
                                            call       CNCTKG
                                 repeat
                                 call       MOVEMLRVARS
                      endif
.END PATCH 1.1 REPLACED LOGIC
           endif
           TRAPCLR    IO
           RETURN


;.............................................................................
;
; ENTRY POINT : COMPKGP
; REQUIRED    : PREVIOUS VALID AIM READ
; RETURNED    : COMPANY RECORD
; DESCRIPTION : KEY GENERIC PRIOR COMPANY FILE READ
;               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;
.COMPKGP
.          BRANCH     COMPFLAG,COMP11
.          CALL       COMPOPEN
.COMP11
.          TRAP       IOMssg Giving Error if IO
.          BRANCH     COMPLOCK TO COMP11L,COMP11R,COMP11N
.COMP11L
.          FILEPI     1;COMPFILE2
.          READKGP    COMPFILE2;COMPVARS
.          TRAPCLR    IO
.          RETURN
.COMP11R
.          READKGPLK COMPFILE2;COMPVARS
.          TRAPCLR    IO
.          RETURN
.COMP11N
.          READKGP    COMPFILE2;COMPVARS
.          TRAPCLR    IO
.          RETURN
;.............................................................................
;.............................................................................
;
; ENTRY POINT : COMPKGP
; REQUIRED    : PREVIOUS VALID AIM READ OF COMPANY AND FLAG
; RETURNED    : COMPANY RECORD
; DESCRIPTION : KEY GENERIC PRIOR COMPANY FILE READ
;               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;
COMPKGP
           BRANCH     COMPFLAG,COMP11
           CALL       COMPOPEN
COMP11
           TRAP       IOMssg Giving Error if IO
           BRANCH     COMPLOCK TO COMP11L,COMP11R,COMP11N
COMP11L
           FILEPI     1;COMPFILE2
           READKGP    compfile2;COMPVARS
           if over
                      if (compconvflag = 2)
                                 clear NBRKVARS
                      elseif (compconvflag = 3)
                                 clear      MLRVARS
                      endif
                      setflag over
           else
.START PATCH 1.1 REPLACED LOGIC
.                     if (compconvflag = 2)
.                                call MVBRVARS
.                     elseif (compconvflag = 3)
.                                call       MOVEMLRVARS
.                     endif
                      if (compconvflag = 2)
                                 call MVBRVARS
                      elseif (compconvflag = 3)
                                 packkey cnctfld2 to "01X",compnum
                                 call       CNCTAIM
                                 loop
                                 until over
                                 until (CNCTTYPE = "1" & CNCTINACTIVE <> "T")
                                            call       CNCTKG
                                 repeat
                                 call       MOVEMLRVARS
                      endif
.END PATCH 1.1 REPLACED LOGIC
           endif
           TRAPCLR    IO
           RETURN
COMP11R
           READKGPLK compfile2;COMPVARS
           if over
                      if (compconvflag = 2)
                                 clear NBRKVARS
                      elseif (compconvflag = 3)
                                 clear      MLRVARS
                      endif
                      setflag over
           else
.START PATCH 1.1 REPLACED LOGIC
.                     if (compconvflag = 2)
.                                call MVBRVARS
.                     elseif (compconvflag = 3)
.                                call       MOVEMLRVARS
.                     endif
                      if (compconvflag = 2)
                                 call MVBRVARS
                      elseif (compconvflag = 3)
                                 packkey cnctfld2 to "01X",compnum
                                 call       CNCTAIM
                                 loop
                                 until over
                                 until (CNCTTYPE = "1" & CNCTINACTIVE <> "T")
                                            call       CNCTKG
                                 repeat
                                 call       MOVEMLRVARS
                      endif
.END PATCH 1.1 REPLACED LOGIC
           endif
           TRAPCLR    IO
           RETURN
COMP11N
           READKGP    compfile2;COMPVARS
           if over
                      if (compconvflag = 2)
                                 clear NBRKVARS
                      elseif (compconvflag = 3)
                                 clear      MLRVARS
                      endif
                      setflag over
           else
.START PATCH 1.1 REPLACED LOGIC
.                     if (compconvflag = 2)
.                                call MVBRVARS
.                     elseif (compconvflag = 3)
.                                call       MOVEMLRVARS
.                     endif
                      if (compconvflag = 2)
                                 call MVBRVARS
                      elseif (compconvflag = 3)
                                 packkey cnctfld2 to "01X",compnum
                                 call       CNCTAIM
                                 loop
                                 until over
                                 until (CNCTTYPE = "1" & CNCTINACTIVE <> "T")
                                            call       CNCTKG
                                 repeat
                                 call       MOVEMLRVARS
                      endif
.END PATCH 1.1 REPLACED LOGIC
           endif
           TRAPCLR    IO
           RETURN
;.............................................................................
;.............................................................................
;
; ENTRY POINT : COMPAIM2
; REQUIRED    : COMPFLD11, COMPFLD12
; RETURNED    : COMPANY RECORD
; DESCRIPTION : AIM COMPANY FILE READ USING ASSOCIATED CONSULTANT/BROKER - APPLIES TO MAILER CLIENTS ONLY!!
;               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;
COMPAIM2
           BRANCH     COMPFLAG,COMP12
           CALL       COMPOPEN
COMP12
           TRAP       IOMssg Giving Error if IO
           BRANCH     COMPLOCK,COMP12L,COMP12R,COMP12N
COMP12L
           FILEPI     1;COMPFILE8
           READ       COMPFILE8,COMPFld11,COMPFld12;COMPVARS
           TRAPCLR    IO
           RETURN
COMP12R
           READLK     COMPFILE8,COMPFld11,COMPFld12;COMPVARS
           TRAPCLR    IO
           RETURN
COMP12N
           READ       COMPFILE8,COMPFld11,COMPFld12;COMPVARS
           TRAPCLR    IO
           RETURN
;.............................................................................
;
; ENTRY POINT : COMPKG2
; REQUIRED    : VALID PREVIOUS AIM READ USING ASSOCIATED CONSULTANT/BROKER
; RETURNED    : COMPANY RECORD
; DESCRIPTION : AIM KEY GENEREIC COMPANY FILE READ
;               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;
COMPKG2
           BRANCH     COMPFLAG,COMP13
           CALL       COMPOPEN
COMP13
           TRAP       IOMssg Giving Error if IO
           BRANCH     COMPLOCK,COMP13L,COMP13R,COMP13N
COMP13L
           FILEPI     1;COMPFILE8
           READKG     COMPFILE8;COMPVARS
           TRAPCLR    IO
           RETURN
COMP13R
           READKGLK COMPFILE8;COMPVARS
           TRAPCLR    IO
           RETURN
COMP13N
           READKG     COMPFILE8;COMPVARS
           TRAPCLR    IO
           RETURN
;..............................................................................
;.
;. ENTRY POINT : COMPOPEN
;. REQUIRED    : 'COMPFLAG' 'COMPPATH'
;. RETURNED    : 'COMPFLAG' SET TO '1' IF OPENNED
;. DESCRIPTION : OPEN NIN Company FILE
;.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
;.
COMPOPEN
           TRAP       COMPGONE IF IO
           OPEN       COMPFLIST
           TRAPCLR    IO
           MOVE       C1,COMPFLAG
           RETURN
COMPOPEN2
           TRAP       COMPGONE IF IO
           OPEN       COMPFILE7,COMPNAME
           TRAPCLR    IO
           MOVE       C1,COMPFLGSEQ
           RETURN
;..............................................................................
COMPGONE
           MOVE       COMPNAME,FILENAME
           CALL       FILEGONE
;..............................................................................