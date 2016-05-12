.Compute 
          INCLUDE COMMON.INC  
          INCLUDE CONS.INC
          INCLUDE   NSHPDD.INC
          INCLUDE   NMRGDD.INC
          INCLUDE NORDDD.INC
          INCLUDE COMPDD.INC  
          INCLUDE CNTDD.INC   
        INCLUDE CONSACCT.inc
        INCLUDE nacddd.inc
        INCLUDE NOWNDD.INC
        INCLUDE NDAT3DD.INC
        INCLUDE NDATDD.INC
          INCLUDE   NADJDD.INC        
          Include   NInvACddd.inc
          INCLUDE   ninvdd.inc
          INCLUDE   NJSTDD.INC        
          
          
DimPtr1  Dim ^     .OLRN
DimPtr2  Dim ^     .OSTAT
DimPtr3  Dim ^     .CHECK OR INV FLAG
DimPtr4  Dim ^     .FLAG FOR CODE 14 
FormPtr1 FORM ^     .AP1
FormPtr2 FORM ^        .JULIAN DATE OF CHECK DATE OR INV DATE
FormPtr3 FORM ^     .OPEN PAYABLES

NUM102    FORM      10.2
NUM1021   FORM      10.2
NUM1022   FORM      10.2
HoldAP1   FORM      10.2
MRGSW   DIM     1
SHIPSW  DIM     1


Release   init "1.1"            .DLH remove ancient smtp code
REldate   INit      "04Oct2012"
.Release init "1.0"

GetIncome Routine DimPtr1,DimPtr2,DimPtr3,DimPtr4,FormPtr1,FormPtr2,FormPtr3

                    clear   DimPtr4
                    MOVE    DimPtr1 TO NINVFLD
                    MOVE    DimPtr1 TO NORDFLD              
                    move      c1 to nordpath
                    CALL    nordkey     
                    If        (DimPtr2 = "B" | DimPtr2="Q")
                              move      c1 to ninvpath
                              CALL      NINVKEY
                              if Not OVER
.JDpatch
                                        CMATCH    "0" TO STATB                ;Open and still have to check for "14" Adjustment
.For invoice accrued basis
                                        If Equal
.Compute
                                                  if (ap2 <= c0)
                                                            MOVE      YES TO SUBPPSW
                                                            MOVE      LRN to nmrgfld
                                                            REP       ZFILL IN NMRGFLD
                                                            move      c0 to nmrgrqty
                                                            move      c0 to nmrgiqty
                                                            move    c0 to nmrgnet
                                                            move    no to mrgsw
                                                  move    no to shipsw
                                                            CALL    NMRGKEY
                                                            if      not over
                                                                      move      yes to mrgsw
                                                            endif
                                                            MOVE      NordFLD to nshpfld
                                                            REP       ZFILL IN NshpFLD
                                                            CALL      NshpKEY
                                                            if        not over
                                                                      move      yes to shipsw
                                                                      endif
                                                                      call      wipecvars
                                                                      move      c1 to ndatpath
                                                                      move      olnum to ndatfld
                                                                      call      ndatkey
                                                                      move      lrn to nshpfld
                                                                      call      nshpkey
                                                                  call           NInvAcdRecClear
                                                                  CLEAR          NInvAcdfld
                                                                  packkey           NInvAcdFld from Invnum
.               call           NInvAcdRecClear
                                                                  call           NinvAcdTst
                                                                  Call           NInvAcdRecLoad
                                                                      CALL      COMPUTE
                                                                      move      ap to ap1
                                                                      move    ap1 to formptr1
.;;;;;;;;;C   - may have to use this hold for master adjustment reconcile
                                                                      clear     holdap1
                                                                      move      ap to holdap1
.;;;;;;;;;C
                                                            else
                                                                      move ap2 to ap1
                                                                      move    ap1 to formptr1                                                                   
.;;;;;;;;;C   - may have to use this hold for master adjustment reconcile
                                                                      clear     holdap1
                                                                      move      ap2 to holdap1
.;;;;;;;;;C
                                                            endif
.Compute
                                                            move      "01" to n2
                                                            move      n2 to str2
                                                            rep       zfill in str2
                                                            CLEAR     NJSTFLD
                                                            PACK      NJSTFLD FROM INVNUM,str2
                                                            rep       zfill in njstfld
                                                            clear     num1022
.For Invoice Accrual Basis
                                                            CALL      NJSTKEY
                                                            if Not Over
.COmpute
                                                                      if (ap2 <= c0)
.;;;;;;;;;C                                                                               add jstap1 to ap1
.master adj check
                                                                                add       jstap1 to num1022
.master adj check
                                                                      else
.;;;;;;;;;C                                                                               add jstap2 to ap1
.master adj check
                                                                                add       jstap2 to num1022
.master adj check
                                                                      endif
                                                            else
                                                                      if (DIMPTR3 = "C")
                                                                                add ap1 to FormPtr3
                                                                                CALL      NJSTKEY
                                                                                RETURN    if OVER
                                                                      else
                                                                                goto Buckets

                                                                      endif
                                                            endif
.COmpute
.;;;;;;;;;Invoice Accrual
.For Invoice Accrual Basis

                                                            MATCH     "14" TO JSTREASN
                                                            IF      EQUAL
.Flag for Code 14 to send back to program if open
                                                                      move YES to DimPtr4
.
                                                                      move jstap1 to ap1
                                                                      mult seq to ap1
                                                                      move    ap1 to formptr1                                                         
                                                                      if (ap1 < c0)
                                                                                mult seq by ap1
                                                                                move    ap1 to formptr1                                                                             
                                                                      endif
                                                                      add ap1 to FormPtr3
.Grab Adj Date - 
                                                                      unpack JSTDATE,str2,YY,MM,DD
                                                                      move      MM to n2
                                                                      Goto      ManualBuckets
                                                            ENDIF
.Patch 1.7.1 Logic Added
                                                            clear n2
                                                            for n2,"2","9"
.                                                 loop
.                                                           add       c1 to n2
.Patch 1.7.1 Logic Added
                                                                      move      n2 to str2
                                                                      rep       zfill in str2
                                                                      CLEAR     NJSTFLD
                                                                      PACK      NJSTFLD FROM INVNUM,str2
                                                                      rep       zfill in njstfld
                                                                      CALL      NJSTKEY
.Patch 1.7.1 Comment Out
.                                                 until over
.Patch 1.7.1 Comment Out
.For Invoice Accrual 
                                                                      if (ap2 <= c0)
.master adj check
                                                                                add       jstap1 to num1022
.master adj check
                                                                      else
.master adj check
                                                                                add       jstap2 to num1022
.master adj check
                                                                      endif
.For INvoice Accrual

                                                                    MATCH     "14" TO JSTREASN
                                                                      IF      EQUAL
.Flag for Code 14 to send back to program if open
                                                                      move YES to DimPtr4
.                                                                     
                                                                                move jstap1 to ap1
                                                                                mult seq to ap1
                                                                                move    ap1 to formptr1                                                                             
                                                                                if (ap1 < c0)
                                                                                          mult seq by ap1
                                                                                          move    ap1 to formptr1                                                                                       
                                                                                endif
                                                                                add ap1 to FormPtr3

.Grab Adj Date - 
                                                                                unpack JSTDATE,str2,YY,MM,DD
                                                                                move      MM to n2
                                                                                Goto      ManualBuckets
                                                                      ENDIF
                                                            repeat
.For invoice accrual
.;;;;;;;;;C
                                                            if (ap2 <= c0)
.;;;;;;;;;C
                                                                      move holdap1 to ap1
                                                                      move    ap1 to formptr1
                                                                      MOVE      LRN TO NADJFLD
                                                                      REP       ZFILL IN NADJFLD
                                                                      CALL      NADJKEY
                                                                      add       aspayad1 to ap1
                                                                      move    ap1 to formptr1                                                                   
                                                            else
                                                                      move   holdap1 to ap1
                                                                      move    ap1 to formptr1                                                                   
                                                                      MOVE      LRN TO NADJFLD
                                                                      REP       ZFILL IN NADJFLD
                                                                      CALL      NADJKEY
                                                                      add       aspayad2 to ap1
                                                                      move    ap1 to formptr1                                                                   
.;;;;;;;;;C                                                         
                                                            endif
.;;;;;;;;;C

                                                            return if (ap1 < 0)
                                                            add ap1 to FormPtr3
                                                            return if (DIMPTR3 = "C")
                                                            goto      Buckets
                                                  endif

.for invoice accrual
                                                  CMATCH    "P" TO STATB                ;PAID
                                                  If Equal
.;;;;;;;;;;;;
.Compute
                                                            if (ap2 <= c0)
                                                                      MOVE      YES TO SUBPPSW
                                                            MOVE      LRN to nmrgfld
                                                            REP       ZFILL IN NMRGFLD
                                                            move      c0 to nmrgrqty
                                                            move      c0 to nmrgiqty
                                                            move      c0 to nmrgnet
                                                            move      no to mrgsw
                                                            move      no to shipsw
                                                            CALL      NMRGKEY
                                                            if        not over
                                                                      move      yes to mrgsw
                                                            endif
                                                            MOVE      NordFLD to nshpfld
                                                            REP       ZFILL IN NshpFLD
                                                            CALL      NshpKEY
                                                            if        not over
                                                                      move      yes to shipsw
                                                            endif
                                                            call      wipecvars
                                                            move      c1 to ndatpath
                                                            move      olnum to ndatfld
                                                            call      ndatkey
                                                            move      lrn to nshpfld
                                                            call      nshpkey
                                                                  call      NInvAcdRecClear
                                                                  CLEAR     NInvAcdfld
                                                                  packkey   NInvAcdFld from Invnum
                                                                  call      NinvAcdTst
                                                                      Call      NInvAcdRecLoad
                                                            CALL      COMPUTE
                                                                      move      ap to ap1
                                                                      move    ap1 to formptr1                                                                   
.;;;;;;;;;C   - may have to use this hold for master adjustment reconcile
                                                                      clear     holdap1
                                                                      move      ap to holdap1
.;;;;;;;;;C
                                                             else
                                                                      move ap2 to ap1
                                                                      move    ap1 to formptr1                                                                   
                                                             endif
.Compute
.;;;;;;;;;;;;;
                                                            move      "01" to n2
                                                            move      n2 to str2
                                                            rep       zfill in str2
                                                            CLEAR               NUM1022
                                                            CLEAR     NJSTFLD
                                                            PACK      NJSTFLD FROM INVNUM,str2
                                                            rep       zfill in njstfld
                                                            CALL      NJSTKEY
                                                            if not over
.Compute
                                                                      if (ap2 <= c0)
.....
                                                                                if (JSTREASN <> "14")    ..add for exception test
.....
                                                                                          add jstap1 to ap1
                                                                                          move    ap1 to formptr1                                                                                       
.....
                                                                                endif                      .add for exception test
.master adj check
                                                                                add       jstap1 to num1022
.master adj check
.....
                                                                      else
                                                                                add jstap2 to ap1
                                                                                move    ap1 to formptr1                                                                             
.master adj check
                                                                                add       jstap2 to num1022
.master adj check
                                                                      endif
.Compute
                                                            
.Comment out for exception Test
.Patch 1.7.1 Logic Added
                                                                      clear n2
                                                                      for n2,"2","9"
.                                                           loop
.                                                                     add       c1 to n2
.Patch 1.7.1 Logic Added
                                                                                move      n2 to str2
                                                                                rep       zfill in str2
                                                                                CLEAR     NJSTFLD
                                                                                PACK      NJSTFLD FROM INVNUM,str2
                                                                                rep       zfill in njstfld
                                                                                CALL      NJSTKEY
.Patch 1.7.1 Comment Out
.                                                           until over
.Patch 1.7.1 Comment Out
.compute
                                                                                if (ap2 <= c0)
.....
                                                                                          if (JSTREASN <> "14")               ..add for exception test
.....
                                                                                                    add jstap1 to ap1
                                                                                                    move    ap1 to formptr1                                                                                                 
.....
                                                                                          endif                                .add for exception test
.master adj check
                                                                                          add       jstap1 to num1022
.master adj check
.....
                                                                                else
                                                                                          add jstap2 to ap1
                                                                                          move    ap1 to formptr1                                                                                       
.master adj check
                                                                                          add       jstap2 to num1022
.master adj check
                                                                                endif
                                                            repeat

.....Code Added for exception test of the infamous "14"
.;;;;;;;;;;;;;
                                                            move      "01" to n2
                                                            move      n2 to str2
                                                            rep       zfill in str2
                                                            CLEAR     NJSTFLD
                                                            PACK      NJSTFLD FROM INVNUM,str2
                                                            rep       zfill in njstfld
                                                            CALL      NJSTKEY
                                                            if not over
.Compute
.Added code for exception Test
                                                                      if (ap2 <= c0)
                                                                                clear num102
                                                                              MATCH     "14" TO JSTREASN
                                                                              IF      EQUAL
                                                                                          move jstap1 to num102
...........
                                                                                ENDIF
..........
.Patch 1.7.1 Logic Added
                                                                                clear n2
                                                                                for n2,"2","9"
.                                                                     loop
.                                                                               add       c1 to n2
.Patch 1.7.1 Logic Added
                                                                                          move      n2 to str2
                                                                                          rep       zfill in str2
                                                                                          CLEAR     NJSTFLD
                                                                                          PACK      NJSTFLD FROM INVNUM,str2
                                                                                          rep       zfill in njstfld
                                                                                          CALL      NJSTKEY
.Patch 1.7.1 Comment Out
.                                                                     until over
.Patch 1.7.1 Comment Out
                                                                                          if (JSTREASN = "14")
                                                                                                    add jstap1 to num102
                                                                                          endif
                                                                                repeat
                                                                                mult seq to num102
                                                                                if (num102 < c0)
                                                                                          mult seq by num102
                                                                                endif
.;;;;;;;;;C
                                                                                move    holdap1 to ap1
                                                                                move    ap1 to formptr1                                                                             
                                                                                MOVE      LRN TO NADJFLD
                                                                                REP       ZFILL IN NADJFLD
                                                                                CALL      NADJKEY
                                                                                add       aspayad1 to ap1
                                                                                move    ap1 to formptr1                                                                             
.;;;;;;;;;C
                                                                                sub       num102 from ap1,num1021
                                                                                if (num1021 <> c0)
                                                                                          add num102,ap1
                                                                                          move    ap1 to formptr1                                                                                       
.;;;;;;;;;C
                                                                                else
                                                                                          move holdap1 to ap1
                                                                                          move    ap1 to formptr1                                                                                       
                                                                                          MOVE      LRN TO NADJFLD
                                                                                          REP       ZFILL IN NADJFLD
                                                                                          CALL      NADJKEY
                                                                                          add       aspayad1 to ap1
                                                                                          move    ap1 to formptr1                                                                                       
.;;;;;;;;;C
                                                                                endif
.;;;;;;;;;C
.                                                                     else
..
                                                                                if (num102 = c0)
                                                                                          move holdap1 to ap1
                                                                                          move    ap1 to formptr1                                                                                       
                                                                                          MOVE      LRN TO NADJFLD
                                                                                          REP       ZFILL IN NADJFLD
                                                                                          CALL      NADJKEY
                                                                                          add       aspayad1 to ap1
                                                                                          move    ap1 to formptr1                                                                                       
                                                                                endif
.;;;;;;;;;C
.;;;;;;;;;C
                                                                      else
                                                                                move holdap1 to ap1
                                                                                move    ap1 to formptr1                                                                             
                                                                                MOVE      LRN TO NADJFLD
                                                                                REP       ZFILL IN NADJFLD
                                                                                CALL      NADJKEY
                                                                                add       aspayad2 to ap1
                                                                                move    ap1 to formptr1                                                                             
.;;;;;;;;;C
                                                                      endif
                                                            endif
                                                  endif
......................................
Buckets
                                                  move      CHK1DTEM to N2
                                                  move      CHK1DTEM to MM
                                                  move      CHK1DTED to DD
                                                  move      CHK1DTEY to YY
ManualBuckets
.patchfor by invoice date
                                                  if (DIMPTR3 = "I")
                                                            move      INVDTEM to N2
                                                            move      INVDTEM to MM
                                                            move      INVDTED to DD
                                                            move      INVDTEY to YY
                                                  endif
                                                  call      CVTJUL
.;;Master Adjustment Check
                                                  MOVE      LRN TO NADJFLD
                                                  REP       ZFILL IN NADJFLD
                                                  CALL      NADJKEY
                                                  if (AP2 <= C0)
                                                            COMPARE   ASPAYAD1,NUM1022
                                                            if NOT EQUAL
                                                                      pack taskname,"The Master AP1 Adj: ",ASPAYAD1," Does not Equal the detail Adj: ",NUM1022," For LR: ",LRN
                                                                      call emailtrouble
                                                            endif
                                                  else
                                                            COMPARE   ASPAYAD2,NUM1022
                                                            if NOT EQUAL
                                                                      pack taskname,"The Master AP2 Adj: ",ASPAYAD2," Does not Equal the detail Adj: ",NUM1022," For LR: ",LRN
                                                                      call emailtrouble
                                                            endif
                                                  endif
.;
.endpatch
.do not allow for neg
                                                  return if (ap1 < 0)


                              ENDIF
                    ENDIF
          ENDIF
                              
                              RETURN
EmailTrouble
                    MOVE      "Houston We May have a problem" to MailSubjct
                    Move      taskname,mailbody
.                    move    "Houston We May have a problem",SmtpSubject Subject
.;.   Set the text message that is send with the attachments
.                    move    Taskname,SmtpTextMessage(1)   Array <Text message >
.                    move    "1",SmtpTextIndexLast                               Index to last entry in TextMessage array
.                    move    "NTS4",SmtpEmailServer                   Address of email serverc
.                    clear   smtpemailaddress
.                    append  "dbaca",SmtpEmailAddress
.                    append  "@nincal.com",SmtpEmailAddress
.                    reset   smtpemailaddress
.                    move    "dbaca",SmtpUserName                                User name
.;   Set the destinations of the email. Max 100 (Mime spec)
.                    move    smtpemailaddress,SmtpDestinations(1,1)
.                    move    "dbaca",SmtpDestinations(1,2)
.                    move    "1",SmtpDestIndexLast                          originators UserName
.                    move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
.                    clear   SmtpLogFile                                         'Clear' disables the LogFile
.                    move    "1",SmtpProgress                                    Enable progress bars
.                    call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
.                    if not equal
.                            pack    Mess,"Result Code ",SmtpResult," - ",SmtpResultText,NewLine:
.                                "Status Code ",SmtpStatus," - ",SmtpStatusText
.                            move    "Error Sending Message",SmtpSubject Subject
.                            move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
.                            call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
                    move      "creques@nincal.com",Mailto
                    move      "creques@nincal.com",MailFrom
                    call      sendMail
.                    endif
                    return


.compute
        INCLUDE NDATIO.INC
          INCLUDE   NSHPIO.INC
          INCLUDE   NMRGIO.INC
          INCLUDE   NDAT3IO.INC
        INCLUDE nacdIO.inc
          INCLUDE   compute.inc
          INCLUDE   ninvio.inc
          Include   NInvACdio.inc
          INCLUDE   NJSTIO.INC
          INCLUDE   NADJIO.INC                                                  
          include nordio.inc  
          INCLUDE COMLOGIC.INC