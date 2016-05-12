...............................................................................
.  
.PURPOSE - creates to sub files of inactive ACCOUNTS 

.
...............................................................................
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NMOADD.inc
         INCLUDE   NMOBDD.inc
.
release  init      "1.1"         ASH  27Oct98 NINMOA Y2K,File expansion
.RELEASE  INIT      "1.0"         DLH  13nov96
olddet1   file      fixed=119   .pass two detail recs inactive > 6mos < 12mos
olddet2   file      fixed=119   .pass two detail recs inactive > 12mos
ACCOUNTS FILE      FIXED=21
RECNUM   FILE
...............................................................................
DETCOUNT FORM      5
BALCOUNT FORM      5
DETsaved FORM      5
BALsaved FORM      5
DATE     DIM       8
NULL     DIM       7
CHKDATE  DIM       4
SYSDATE  form      5
SAVEKEY  DIM       8
SAMEMLR  DIM       4
SAMEBRK  DIM       4
SAME     DIM       8
DATEMASK DIM       8
CHANGE   FORM      7.2
inbal    form      4
dteflag  form      1
first    dim       1
.
...............................................................................
.         OPEN      ACCOUNT1,"ONACCNTBALAN"
         IFNZ       PC
         OPEN      ACCOUNTS,"NINMOB"
         XIF
         IFZ        PC
         OPEN      ACCOUNTS,"NINMOB"
         move      c2 to nmoapath
         call      nmoaopen
         XIF
...............................................................................
         CLOCK     DATE TO DATE
         MOVE      "NONA0010" TO PROGRAM
         MOVE      "Names in the News" TO compnme
         MOVE      "Find INACTIVE MOA" TO STITLE
         move      "abort" to pf5
DATE     
         MOVE      DATE TO TODAY
         MOVE      DATE TO DATEMASK
         UNPACK    DATE INTO MM,SLASH,DD,SLASH,YY
         move      "01" to dd
         CALL      PAINT
         call      cvtjul
         move      juldays to sysdate
.         PACK      SYSDATE FROM MM,YY
         KEYIN     *P10:10,"DATE OK ",*DV,DATEMASK," ",STR1
         CMATCH    YES TO STR1
         GOTO      DATEOK IF EQUAL
         KEYIN     *P18:10,*+,MM,"/",DD,"/",YY
         move      "01" to dd
         PACK      DATE FROM MM,SLASH,DD,SLASH,YY
         call      cvtjul
         move      juldays to sysdate
         GOTO      DATE
DATEOK   CLEAR     NULL
.
         MOVE      C2 TO NMOBPATH
         prepare   olddet1,"\\nins1\d\data\olddet1",exclusive
         prepare   olddet2,"\\nins1\d\data\olddet2",exclusive
         CALL      PAINT
         call      funcdisp
         trap      done if f5
...............................................................................
MENU     CLEAR     NMOAFLD
         clear     nmoafld4
         clear     nmobbrk
         clear     mlr
         READ      ACCOUNTS,SEQ;MLR,MCNT,BALANCE,nmobbrk
         GOTO      DONE IF OVER
         add       c1 to inbal
         display   *p10:12,"master records in ",inbal
         COMPARE   C0 TO BALANCE     *NO MONEY ON ACCOUNT?
         GOTO      MENU IF NOT EQUAL     *HAVE BALANCE
         PACK      NMOAFLD4 FROM NMOBBRK,MLR    *NO BALANCE, ACTIVE?
         REP       " 0" IN NMOAFLD4
         MOVE      NMOAFLD4 TO SAVEKEY
...............................................................................
         MOVE      C4 TO NMOAPATH
         CALL      NMOAKEY    *GET DETAIL REC.
         GOTO      write IF OVER
         CALL      CHKDATE    *CHECK DATE
.
LOOP 
         CALL      NMOAKS      *GET NEXT DETAIL REC.
         GOTO      write IF over     
         pack      same from NMOABRK,mlr
         rep       zfill in same
         match     same to nmoafld4
         goto      write if not equal
         CALL      CHKDATE         *YES, CHECK DATE.
         GOTO      LOOP      *DATE IN DIFFERENT MONTH, CHECK NEXT REC,.
.
...............................................................................
.CHKDATE  - TRANSACTION FROM CURRENT MONTH?
CHKDATE  
.Start Patch #1.1 - remmed and replaced line
.         UNPACK    trandate INTO MM,DD,YY
         UNPACK    trandate INTO STR2,YY,MM,DD
.End Patch #1.1 - remmed and replaced line
         move      c1 to dd
         call      cvtjul
.12months old?
         move      sysdate to n5
         sub       "183" from n5        

         if        (juldays > n5)         .more current than six months ago
         goto      next
         endif         
.                                         *ie we are running job late (in beginning of next month)         
         RETURN
.
...............................................................................
.NEXT - RECORD FOUND FOR CURRENT year. BREAK CYCLE AND START ON NEXT MLR.
NEXT     NORETURN
         GOTO      MENU
.
...............................................................................
.write
.
write   MOVE      SAVEKEY TO NMOAFLD4
.Start Patch #1.1 - remmed and replaced line
.         UNPACK    trandate INTO MM,DD,YY
         UNPACK    trandate INTO STR2,YY,MM,DD
.End Patch #1.1 - remmed and replaced line
        call      cvtjul
        move       sysdate to n6
        sub        "183" from n6
        if         (juldays >n6)
        move       c1 to dteflag
        else  
        move       c2 to dteflag
        endif
wrtLOOP  MOVE      C4 TO NMOAPATH
         CALL      NMOAKEY
         if        over
         GOTO      wrtexit
         else
         move      yes to first
wrtnxt   cmatch    yes to first
         if        equal
         goto      wrt1
         endif
         CALL      NMOAKS      *GET NEXT DETAIL REC.
         GOTO      wrtexit IF over     
         pack      same from NMOABRK,mlr
         rep       zfill in same
         match     same to nmoafld4
         goto      wrtexit if not equal
         endif
wrt1     ADD       C1 TO detsaved
         branch    dteflag of wrtold1,wrtold2
wrtold2         
         write     olddet2,seq;moavars
         goto      wrtnext
wrtold1         
         write     olddet1,seq;moavars
wrtnext
         ADD       C1 TO DETCOUNT
         PACK       NMOAFLD  FROM MLR,MCNT
         DISPLAY   *P1:14,*EL,*P15:14,"DETAIL RECORDS written : ",DETsaved
         move      no to first
         goto      wrtnxt
wrtexit  MOVE      SAVEKEY TO NMOAFLD4
         DISPLAY   *P1:14,*EL,*P15:14,"DETAIL RECORDS written : ",DETsaved
         move      no to first
         GOTO      MENU
...............................................................................
.
DONE    
         weof      olddet1,seqeof
         weof      olddet2,seqeof
         close     olddet1
         close     olddet2
         stop
         INCLUDE   NMOAIO.inc
         INCLUDE   NMOBIO.inc
         INCLUDE   COMLOGIC.inc

