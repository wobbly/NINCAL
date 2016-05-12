;    Last change:  AL    2 Jun 1999    8:04 am
ownhelp  DISPLAY   *SETSWALL  12:24:40:80;
         KEYIN     *P1:1,*ES:
                   *P1:12,*EL,"I WILL HELP YOU FIND THE OWNER NUMBER",*R:
                   *P1:12,*EL,"THAT YOU WANT":
                   *R,*P1:12
KEYOWN   KEYIN     *P1:1,*ES,"TYPE IN OWNER NAME : ":
                   *P1:2,*T60,OWNOCPY
         KEYIN     *P1:2,*DV,OWNOCPY,*P1:3,"OK ? ",*T60,str1:
                   *P1:3,*EL;
         CMATCH    NO TO str1
         GOTO      KEYOWN IF EQUAL
         CMATCH    B1 TO OWNOCPY
         GOTO      NTENOf IF EQUAL
         GOTO      NTENOf IF EOS
         move      "01F" to akey1
         PACK      NOWNFLD2 FROM AKEY1,OWNOCPY,QUES
OWNEADA  MATCH     "01F" TO AKEY1            *FREE FLOAT SEARCH?
         CALL      CHKKEY IF EQUAL           *YES, CHECK KEY LENGTH
         MOVE      "*****NO RECORD FOUND*****" TO OWNOCPY
         CALL      NOWNAIM
         GOTO      OWNAEXIT IF OVER
         GOTO      DISOWNA
CHKKEY   MOVELPTR  NOWNFLD2 TO KEYCOUNT
         COMPARE   C10 TO KEYCOUNT
         GOTO      ntenof IF LESS         *NOT ENOUGH INFO, LET THEM KNOW
         RETURN                             *OK, RETURN
OWNEADA1 MOVE      "*****NO RECORD FOUND*****" TO OWNOCPY
         CALL      NOWNKG
         GOTO      OWNAEXIT IF OVER
         GOTO      DISOWNA
OWNAEXIT
         CALL      NORECORD
OWNAEXT1 DISPLAY   *P1:1,*ES,*RESETSW:
                   *P20:21,*HON,"ENTER TO EXIT,":
                   " or (?) FOR HELP",*HOFF;
         GOTO      DISOWN
.
DISOWNA
         clear     npayfld
         PACK      npayfld FROM ownlon,c0
         CALL      npaykey
         IF        over
         DISPLAY   *P1:09,*EL,"##",OWNLON,"  ":
                   *P1:10,OWNLONM,*P1:11,OWNOCPY
         KEYIN     *P1:13,"TAP ENTER TO CONTINUE, (*) TO END",*T254,str1;
         CMATCH    "*" TO str1
         GOTO      OWNAEXT1 IF EQUAL
         GOTO      OWNEADA1
         else
         DISPLAY   *P1:09,*EL,"##",OWNLON,"  ":
                   *P1:10,OWNLONM,*P1:11,*red,OWNOCPY,*white
         KEYIN     *P1:13,"TAP ENTER TO CONTINUE, (*) TO END",*T254,str1;
         CMATCH    "*" TO str1
         GOTO      OWNAEXT1 IF EQUAL
         GOTO      OWNEADA1
         endif
DISOWN
         KEYIN     *P45:09,*DV,OWNOCPY,*P75:09,"OK?",*T60,str1;
         CMATCH    NO,str1
         GOTO      ownhelp IF EQUAL
         CMATCH    YES,str1
         return    IF EQUAL
         GOTO      DISOWN
.
NORECORD MOVE      "*****NO RECORD FOUND*****" TO OWNOCPY
         DISPLAY   *P1:14,*EL,OWNOCPY,*W2,*B,*P1:13,*EL
         RETURN
.
ntenof NORETURN
.
NOGOOD
         DISPLAY   *P1:24,*HON,*EL,*B,"NOT ENOUGH INFORMATION TO SEARCH ON!!!":
                   *RESETSW:
                   *P20:21,*HON,"ENTER TO EXIT,":
                   " or (?) FOR HELP",*HOFF;
         PAUSE     "3"
         return
