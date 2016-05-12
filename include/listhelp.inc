;    Last change:  DH    8 Oct 1999    7:49 am
.............................................................................
.LISTHELP.INC - LIST HELP AIM SEARCH FOR LIST NUMBER BY NAME.
.
. REQUIRES:   NDATDD.INC
.             NDATIO.INC
.               CONS.INC
.             comlogic.inc     10jul95 DLH
.
.ENTER WITH - TOPWIND,BOTWIN,LWIND,RWIND -  TO DEFINE WINDOW.
.EXIT  WITH - LSTNUM,OLSTNAME, NEED TO MOVE TO OTHER VARIABLES AND RETURN AT 
.                           EXIT PARAGRAPH.
.
.PATCH # 1.7 - ASH 19NOV99 - ADDED LOGIC TO HIGHLIGHT EXCLUSIVE LISTS WHEN SEARCHING.
. DLH 04Mar98 - Make sure *uc turned back off
. DLH 30Jul97 - TRAP F5 to exit. Programmers responsability to reset function if nec.
. ----------------------------------------------------------------------------------
. dlh 03dec96 - change str30 to str35? why str30 in the 1st place?
. DLH 29Aug95 - add display of revdate
. DLH 27Jul95 - ADD search continue option
. DLH 30jan95 - change default free float? to yes.
.............................................................................
LISTHELP CLEAR     STR2
         trap      lstatrap if F5
         MOVE      TOPWIND TO STR2
         TYPE      STR2    *VALID NUMBER?
         CALL      LSETWIN IF NOT EQUAL
         CALL      LSETWIN IF EOS
         COMPARE   C0 TO TOPWIND
         CALL      LSETWIN IF NOT GREATER
         DISPLAY   *SETSWALL  TOPWIND:BOTWIND:LWIND:RWIND;
         MOVE      NO TO WSW         *DEFAULT EXCLUDE WITHDRAWNS
         move      yes to str1
         branch    lhcontin to lhelp,lcontin
lhelp    move      yes to str1
         KEYIN     *P1:1,*ES:
                   *P1:6,*EL,"I'LL HELP FIND THE LIST ##",*R:
                   *R,*P1:6:
                   *R,*P1:6," FREE FLOAT SEARCH ? ,":
                   *R,*P1:6,*T60,*rv,*uc,STR1,*lc:
                   *R,*P1:6,"INCLUDE WITHDRAWNS ? N",*P22:6,*RV,*uc,WSW,*lc
         CMATCH    YES TO STR1
         GOTO      FREFLOT1 IF EQUAL
         MOVE      "02L" TO AKEY1
KEYLIST  KEYIN     *P1:1,*ES,"TYPE IN LIST NAME : ":
                       *P1:2,*DV,OLSTNAME:
                   *P1:2,*T60,*edit,OLSTNAME
         MOVE      OLSTNAME TO str35
         CMATCH    "*" TO OLSTNAME
         GOTO      LSTAEXT1 IF EQUAL
         KEYIN     *P1:2,*DV,OLSTNAME,*P1:3,"OK ? ",*T254,*uc,STR1,*lc:
                   *P1:3,*EL;
         CMATCH    NO TO STR1
         GOTO      KEYLIST IF EQUAL
         call      trim  using olstname
         count     n2,olstname
         if        (n2 < 3)
         goto      ntenough
         endif
         CMATCH    B1 TO OLSTNAME
         GOTO      NTENOUGH IF EQUAL
         GOTO      NTENOUGH IF EOS
         MATCH    "02L" TO AKEY1
         GOTO      PACKL1 IF EQUAL
         PACK      NDATFLD2 FROM AKEY1,OLSTNAME
         CALL      CHKKEY1
         GOTO      LSTREADA
PACKL1   PACK      NDATFLD2 FROM AKEY1,OLSTNAME,QUES
         CALL      CHKKEY1
         GOTO      LSTREADA
FREFLOT1 MOVE      "02F" TO AKEY1
         GOTO      KEYLIST
LSTREADA
         CLEAR     LSTNUM
         MOVE      "****NO LIST FOUND*****" TO OLSTNAME
         CLEAR     NDATFLD1
           MOVE      C2 TO NDATPATH         *MAKE SURE ACCESS IS AIM.
.         call      rotdial
           CALL       NDATAIM
           IF          OVER
           CLEAR     LSTNUM
         move      c1 to lhcontin             *can't continue
         clear     str35
         GOTO      LSTAEXIT
           ENDIF
         CMATCH    YES TO WSW        *WITHDRAWNS WANTED?
         GOTO      DISLSTA IF EQUAL    *YES
         CMATCH    "W" TO STATUS
         GOTO      LSTRED1A IF EQUAL
         GOTO      DISLSTA
CHKKEY1  MOVELPTR  NDATFLD2 TO KEYCOUNT
         GOTO      NTENOUGH IF ZERO
         COMPARE   C6 TO KEYCOUNT
         GOTO      NTENOUGH IF LESS
         RETURN
NTENOUGH DISPLAY   *P1:24,*BLINKON,*HON,*EL,"NOT ENOUGH INFO TO SEARCH":
                   *HOFF;
         GOTO      LISTHELP
lcontin  move      no to str1
         KEYIN     *P1:1,*ES:
                       *P01:03,*EL,*dv,str35:
                   *P1:6,*EL,"Continue previous search ? ",*uc,*t60,*rv,STR1,*lc;
         cmatch    yes to str1
         goto      lhelp if not equal          
         move      str35 to olstname
LSTRED1A CLEAR     LSTNUM
         MOVE      "****NO LIST FOUND*****" TO OLSTNAME
.         call      rotdial
           CALL      NDATKG
           IF        OVER
           CLEAR     LSTNUM
         move      c1 to lhcontin
         GOTO      LSTAEXIT
           ENDIF
         CMATCH    YES TO WSW        *WITHDRAWNS WANTED?
         GOTO      DISLSTA IF EQUAL    *YES
         CMATCH    "W" TO STATUS
         GOTO      LSTRED1A IF EQUAL
         GOTO      DISLSTA
LSTRED2A CLEAR     LSTNUM
         MOVE      "****NO LIST FOUND*****" TO OLSTNAME
.         call      rotdial
           CALL      NDATKGp
           IF        OVER
           CLEAR     LSTNUM
         move      c1 to lhcontin
         GOTO      LSTAEXIT
           ENDIF
         CMATCH    YES TO WSW        *WITHDRAWNS WANTED?
         GOTO      DISLSTA IF EQUAL    *YES
         CMATCH    "W" TO STATUS
         GOTO      LSTRED2A IF EQUAL
         GOTO      DISLSTA
LSTAEXIT
         MOVE      YES TO OVER
LSTAEXT1 DISPLAY   *P1:1,*ES,*RESETSW;
.                   *P20:21,*HON,"ENTER (*) TO EXIT, (<) TO BACKUP":
.                   " or (?) FOR HELP",*HOFF;
         REP       ZFILL IN LSTNUM
         DISPLAY   *P75:17,*EL,LSTNUM;
         RETURN
LStaTRAP trapclr    F5
         noreturn
         clear      lstnum
         goto       lstaexit
.                  
.START PATCH #1.7 - REPLACED LOGIC
.DISLSTA  cmatch     "W" to status
.         if        equal
.         CALL      WITHD
.         DISPLAY   *P1:02,*EL,"##",LSTNUM,b1,*cyan,revdate,*white:
.                       *P01:03,*EL,*yellow,OLSTNAME,*white,*P1:6,*EL;
.         else
.         scan      "Office Use" IN mlstname
.         if         equal
.         DISPLAY   *P1:02,*EL,"##",LSTNUM,b1,*cyan,revdate,*white:
.                       *P01:03,*EL,*red,OLSTNAME,*white,*P1:6,*EL;
.         ELSE
.         DISPLAY   *P1:02,*EL,"##",LSTNUM,b1,*cyan,revdate,*white:
.                       *P01:03,*EL,OLSTNAME,*P1:6,*EL;
.         endif
.         endif
DISLSTA  cmatch     "W" to status
         if        equal
         CALL      WITHD
         DISPLAY   *P1:02,*EL,"##",LSTNUM,b1,*cyan,revdate,*white;
         if (ELSTCDE = "C")
                   DISPLAY    *P01:03,*EL,*yellow,STAR,OLSTNAME,*white,*P1:6,*EL;
         else
                   DISPLAY    *P01:03,*EL,*yellow,OLSTNAME,*white,*P1:6,*EL;
         endif
         else
         scan      "Office Use" IN mlstname
         if         equal
         DISPLAY   *P1:02,*EL,"##",LSTNUM,b1,*cyan,revdate,*white;
         if (ELSTCDE = "C")
                   DISPLAY    *P01:03,*EL,*red,STAR,OLSTNAME,*white,*P1:6,*EL;
         else
                   DISPLAY    *P01:03,*EL,*red,OLSTNAME,*white,*P1:6,*EL;
         endif
         ELSE
         DISPLAY   *P1:02,*EL,"##",LSTNUM,b1,*cyan,revdate,*white;
         if (ELSTCDE = "C")
                   DISPLAY    *P01:03,*EL,STAR,OLSTNAME,*P1:6,*EL;
         else
                   DISPLAY    *P01:03,*EL,OLSTNAME,*P1:6,*EL;
         endif
         endif
         endif
.END PATCH #1.7 - REPLACED LOGIC
         move      c2 to lhcontin
           KEYIN     *P1:6,"(ENTER), (P)rior  (*) TO END",*T254,*uc,STR1,*lc;
         CMATCH    STAR TO STR1
         GOTO      LSTAEXT1 IF EQUAL
           CMATCH    "P" TO STR1
           GOTO      lstrED2a IF EQUAL
         GOTO      LSTRED1A
WITHD    DISPLAY   *P1:6,*EL,*HON,*B,"WITHDRAWN",*B,*HOFF,*W
         RETURN
.............................................................................
LSETWIN  MOVE      "18" TO TOPWIND    NO SPECS,SET TO DEFAULTS
         MOVE      C24 TO BOTWIND
         MOVE      "40" TO LWIND
         MOVE      C80 TO RWIND
         RETURN
............................................................................

