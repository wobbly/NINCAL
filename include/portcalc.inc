;----------------------------------------------------------------
;
;   Portcalc.inc - 
;
;   Copyright(c) Names in the News Ca., Inc. 2000
;
;   Author: David Herrick
;   Created: 1/24/2002 6:07:57 AM
;   Last change: DLH 3/5/2002 9:27:44 AM
;----------------------------------------------------------------

FINDPORT
.. begin 11Feb2002
.         move      "USER" to str25
.         clock     env to str25
.         if        over
.         move      "USERNAME" to str25
.         clock     env to str25
.         bump      str25,9
.         else
.         bump      str25,5
.         endif
.         clock     port to portinfo
.. end 11Feb2002
         move      str25 to user
         UNPACK    PORTINFO INTO TASK,ANS,NODE,ANS,PORTSCRN,ANS,ATTRIB
         PACK      LOGPORT FROM NODE,TASK
        clock      port to str3           .plb note
        unpack     str3 into str2,str1
        pack       str3 from str1,str2
        move       str3 to portnum         
         MOVE      PORTNUM TO PORTX
         MOVE      PORTNUM TO PORTN
         GOTO      CALLREAD
         GOTO      PORTEXIT
COMPR    MOVE      "DP" TO TYPINIT
         MOVE      "Crequest " TO NAME
         MOVE      "65" TO FUNC
         GOTO      PORTEXIT
. ............................................................................
DP       MOVE      "999" TO DIM3
         MOVE      DIM3 TO PORTX
         MOVE      PORTX TO PORTN
CALLREAD
         MOVE      PORTX TO NUSEFLD
         REP       zfill IN NUSEFLD
         CALL      NUSEKEY
         if        over
         IFZ       PC
         display   *p1:24,*el,*red,*hon,"I'm really lost ",*blinkon:
                   "call Info Srvs NOW !!!!!!!!!",*blinkoff,*hoff,*white:
                   *w10
         xif
         IFNZ      pc
         alert   caution,"Lost -- Can't Find your Port number!!!",result
         XIF
         stop
         endif          
         if        (PORTN = c2)              .is this via plbserve ?
.         alert   caution,"port 2 I am trying!!!",result
.              Move          "014" to Portx
.              move          "014" to portn
.              move          portx to nusefld
.              REP       zfill IN NUSEFLD
.              MOVE       C1 TO NUSEPATH    *ACCESS BY PORTNUMBER
.              CALL      NUSEKEY
.              MOVE      NUSEINIT TO TYPINIT
.              move      nuseinit to inits              .7/20/93 dlh
.              MOVE      NUSEUSER TO NAME
..........
.                   move      "                         ",str25
.                   call      Trim using str25
.drewtest
.              move      "\Program Files\NINCAL\port.ini" to taskname
..              call      "GU$INI;GET_FROM_INI" USING  taskname:
..                                                     "environment":
..                                                     "PLB_Client_Port":
..                                                     str25
.              call      "GU$INI;GET_FROM_INI" USING  taskname,"environment","PLB_Client_Port",str25
.                   reset     str25
.              Call       Trim using str25
.              pack       str55 from "get from = ",str25
.              display     *p1:23,str55,*w3
.                   alert     caution,str25,N10
.              move       str25 to portx
.................................
.                   alert     caution,porttest,N10
.              move      "..\Program Files\NINCAL\port.ini" to taskname
.              call      "GU$INI;GET_FROM_INI" USING  taskname:
.                                                     "environment":
.                                                     "PLB_Client_Port":
.                                                     porttest
.
.              Call       Trim using porttest
.              pack       str55 from "get from = ",porttest
.              display     *p1:23,str55,*w3.
.                   alert     caution,porttest,N10
.              move       porttest to portx
.................................
.              move       portx to portn
.              move       portx to nusefld
.              REP        zfill IN NUSEFLD
.              MOVE       C1 TO NUSEPATH    *ACCESS BY PORTNUMBER
.              CALL       NUSEKEY
.              MOVE       NUSEINIT TO TYPINIT
.              move       nuseinit to inits              .7/20/93 dlh
.              MOVE       NUSEUSER TO NAME
.              call       "GU$INI;WRITE_TO_INI" USING  taskname:
.                                                     "environment":
.                                                     "PLB_TEST_Fort":
.                                                     "testTEST":
.                                                     n3
.         endif

.         TRAP      io giving error if io
.         TRAP      intruder if io
.         OPEN      direct,"port.ini"
.         TRAPCLR   io
.darn it will have to use Winapi to read file
.         TRAP      io giving error if io
.         CLEAR     str25
.         READ      Direct,seq;str25
.         GOTO      intruder if over
.         MOVE      "Port=014" to str25
.         SCAN      "Port=" IN str25
.         if        equal
.         bump      str25 by 1
.         move      str25 TO portx
.         MOVE      PORTX TO PORTN
.         MOVE      PORTX TO NUSEFLD
.         REP       zfill IN NUSEFLD
.         CALL      NUSEKEY
.         GOTO      intruder if OVER
.         ENDIF
.         CLOSE     DIRECT
davetest
         Move      "Client_ID" to str25
         clock     ini,str25
         call      trim using str25
         move      str25 to portx
              move       portx to portn
;temp code 06Oct2003
               if             (portn = 14)
               goto           Intruder
               endif
;temp code 06Oct2003
              move       portx to nusefld
              REP        zfill IN NUSEFLD
              MOVE       C1 TO NUSEPATH    *ACCESS BY PORTNUMBER
              CALL       NUSEKEY
              MOVE       NUSEINIT TO TYPINIT
              move       nuseinit to inits              .7/20/93 dlh
              MOVE       NUSEUSER TO NAME
         ENDIF

         MOVE      NUSEINIT TO TYPINIT
         move      nuseinit to inits              .7/20/93 dlh
         MOVE      NUSEUSER TO NAME
         CLOSE     NUSEFILE
         MOVE      C0 TO NUSEFLAG
.. begin 11Feb2002
        call    Trim using NUSEUSER
        scan    "BILLING",NUSEUSER       
        if not equal
                move    NUSEUSER,str1
                loop
                        bump    NUSEUSER,1
                        cmatch  B1,NUSEUSER
                        until equal
                        until eos
                repeat
                if not eos
                        bump    NUSEUSER,1
                        move    NUSEUSER,str6
                        clear   str35
                        pack    str35,str1,str6
                        reset   nuseuser      .reset var for correct display on form
                endif
        endif
         move      str35 to user
.. end 11Feb2002
         GOTO      portexit
.
.
iNTRUDER
.         CLOSE     DIRECT
         IFZ       PC
         display   *p1:24,*el,*red,*hon,"YOU ARE NOT A RECOGNIZED USER",*blinkon:
                   "call Info Srvs NOW !!!!!!!!!",*blinkoff,*hoff,*white:
                   *w10
         xif
         IFNZ      pc
         clear      str55
         append     "YOU ARE NOT A RECOGNIZED USER!!!",str55
         APPEND     portn,str55
         reset      str55
         alert   caution,str55,result
         XIF
         stop


Hello    if        (soundflag = 1)
         SNDOPEN   sfile,"\\NINS1\e\netutils\media\goodsee.WAV"
         SNDPLAY   sfile
         SNDCLOSE   sfile
         endif
portexit RETURN
...............................................................................

