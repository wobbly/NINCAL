PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NCRCDD.INC
           include  Npasdd.inc
RELEASE  INIT      "2.0"        DLH MAJOR FRIGGIN OVERHAUL
Reldate   Init      "08 May 2012"
.RELEASE  INIT      "1.3"        12JAN00 ASH MAJOR FRIGGIN OVERHAUL
.release  init      "1.2"        29Nov99 ASH Small fixes
.release  init      "1.1"        7Apr98 DLH new keys and year 2000 compliance
.release  init      "1.0"
FILE6    IFILE     KEYLEN=6
KEY      DIM      6
key6     dim       6
NAME     DIM       20
KEYLEN   FORM      2
ANS      DIM       1
lr       dim       6
dim2     dim        2
.begin patch 2.0
.PASS     DIM       5

x         plform     NCRC0003
pass      plform    Passwrd

yesno1    integer   1,"0x000024"                yes no buttons, question Icon

.Load Forms, Always load parent form first
            formload       x
          formload pass
          WinHide               
Timer     Timer
          create          TIMER,18000     .30 minutes
            activate TIMER,Timeout,RESULT
.end patch 2.0


.begin patch 2.0
.PASSCD   move      "NCRC0003" TO PROGRAM
PASSCD   
          move      "NCRC0003" TO PROGRAM
         MOVE      "NAMES IN THE NEWS" TO COMPNME
         MOVE      "DELETE CORR/CANC RECORDS" TO STITLE
               NCRC0003ListView1.InsertColumn using "LR##",100,1
               NCRC0003ListView1.InsertColumn using "Code",100,2
               NCRC0003ListView1.InsertColumn using "Date",165,3
               NCRC0003ListView1.InsertColumn using "Typist",100,4
               activate        NCRC0003EditTextLR
               Setfocus        NCRC0003EditTextLR
          setprop        NCRC0003ButtonDel,enabled=0,visible=0
          setprop        NCRC0003ButtonNext,enabled=0,visible=0

.         CALL      PAINT
.         KEYIN     *P1:4,"PLEASE ENTER PASSWORD: ",*EOFF,PASS,*EON
.         RESET     PASS TO 4
.         RESET     PASS
.         rep        LowUp,pass
.         MATCH     "COSMO",PASS
.         GOTO      END1 IF NOT EQUAL
.         TRAP      END1 IF INT
.         MOVE       "EXIT" TO PF5
.         CALL       FUNCDISP
.         TRAP      END1 IF F5
          call      CheckPass
.............................
          
              loop
               waitevent
                setitem timer,0,18000   .reset to 30 minutes
              repeat

Timeout
        beep
        beep
        beep
        winshow
        stop
.CheckitOut - from Go button
CheckitOut
          getItem   NCRC0003EditTextLR,0,sTR6
          call      Trim using str6
          count     N1,STR6
          if        (n1=c1)
          pack      Key6 from "00000",str6
          elseif    (n1=c2)
          pack      Key6 from "0000",str6
          elseif    (n1=c3)
          pack      Key6 from "000",str6
          elseif    (n1=c4)
          pack      Key6 from "00",str6
          elseif    (n1=c5)
          pack      Key6 from "0",str6
          else
          pack      Key6 from str6
          endif
         pack      NCRCFLD,KEY6
         call      NCRCKEY
         if over
          alert     note,"No Records FOund!",result
         endif
         NCRC0003ListView1.DeleteAllItems giving N9
          Call      DoDoDoit
          
               Return
Next
                    call     NCRCKS
                    if        over
                    setprop        NCRC0003ButtonDel,enabled=0,visible=0
                    setprop        NCRC0003ButtonNext,enabled=0,visible=0
                    Return
                    endif
                    if (NCRCKEY <> NCRCFLD)
                    setprop        NCRC0003ButtonDel,enabled=0,visible=0
                    setprop        NCRC0003ButtonNext,enabled=0,visible=0
                    return
                    endif
                    Call      DoDoDoit
         
.END PATCH 1.3 - OVERHAUL REPLACED LOGIC
          Return
DoDoDoit
               NCRC0003ListView1.Insertitem giving N9 using Ncrcfld
               NCRC0003ListView1.SetItemText using N9,Ncrccode,1
               clear          str10
               pack str10 from NCRCMM,SLASH,NCRCDD,SLASH,NCRCCC,NCRCYY
               NCRC0003ListView1.SetItemText using N9,str10,2
               NCRC0003ListView1.SetItemText using N9,ncrctyp,3
               Setfocus        NCRC0003ListView1
              NCRC0003ListView1.EnsureVisible using n9,0
              NCRC0003ListView1.SetItemState Using *Index=N9,*State=2,*StateMask=2
              setprop        NCRC0003ButtonDel,enabled=1,visible=1
          setprop        NCRC0003ButtonNext,enabled=1,visible=1
          REturn

.end patch 2.0

. 
QUES     DISPLAY   *P1:7,"NUMBER:":
                      *P1:8,"(this number WILL NOT be zero-filled or right":
                      "-justified....enter exact key)",*EF

K6       KEYIN     *P8:7,KEY6
         BUMP      KEY BY 7
         DISPLAY   *P20:7,"AFTER BUMP, KEY=",KEY
         MOVE      KEY6,KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
.START PATCH 1.3 - OVERHAUL REMMED LOGIC
.         OPEN      FILE6,"NINORD3"
.         TRAP      NOFILE GIVING ERROR IF IO
.         FILEPI    1;FILE6
.END PATCH 1.3 - OVERHAUL REMMED LOGIC
.START PATCH 1.3 - OVERHAUL REPLACED LOGIC
.         readkp    file6;key,cc,yy,mm,dd,str1
.         DISPLAY   *EL,*P10:10,"KEY ",KEY,"    ",b1,str1,b1,mm,slash,dd,slash,cc,yy
         pack      NCRCFLD,KEY6
         call      NCRCKEY
         if over
                   goto NOREC
         endif
         DISPLAY   *EL,*P10:10,"KEY=",NCRCKEY,COMMA,B1,"CODE=",NCRCCODE,COMMA,B1,"DATE=",NCRCMM,SLASH,NCRCDD,SLASH,NCRCCC,NCRCYY,B1,"TYPIST=",NCRCTYP
.END PATCH 1.3 - OVERHAUL REPLACED LOGIC
         KEYIN     *EL,*P1:22,"IS THIS RECORD YOU WANT ??",ANS
.START PATCH 1.2 -REPLACED LOGIC
.         goto      again if eos
.         CMATCH    "*" TO ANS
.         GOTO      ques IF EQUAL 
..         CMATCH    "Y" TO ANS
..         GOTO      KILL6 IF EQUAL
         if (ANS = STAR)
                   goto QUES
         elseif (ANS = YES)
                   goto KILL6
         endif
.END PATCH 1.2 -REPLACED LOGIC
.START PATCH 1.3 - OVERHAUL REMMED LOGIC
.         filepi    1;file6
.         readkp    FILE6;lr,cc,yy,mm,dd,str1
.         match      lr to key        hit ??
.         goto      keyseq if not equal
.show     DISPLAY   *EL,*P10:10,"KEY ",KEY,"    ",b1,str1,b1,mm,slash,dd,slash,cc,yy
.AGAIN    KEYIN     *EL,*P1:22,"IS THIS RECORD YOU WANT ??",ANS
.         goto      again if eos
.         CMATCH    "*" TO ANS
.         GOTO      ques IF EQUAL
.         CMATCH    "Y" TO ANS
.         GOTO      KILL6 IF EQUAL
.         filepi    1;file6
.END PATCH 1.3 - OVERHAUL REMMED LOGIC
keyseq   
.START PATCH 1.3 - OVERHAUL REPLACED LOGIC
.         readks    FILE6;lr,cc,yy,mm,dd,str1
.         goto      norec if not equal
.         goto      show
         loop
                    call     NCRCKS
                    goto NOREC if over
                    if (NCRCKEY <> NCRCFLD)
                              goto NOREC
                    endif
                    DISPLAY   *EL,*P10:10,"KEY=",NCRCKEY,COMMA,B1,"CODE=",NCRCCODE,COMMA,B1,"DATE=",NCRCMM,SLASH,NCRCDD,SLASH,NCRCCC,NCRCYY,B1,"TYPIST=",NCRCTYP
                    KEYIN     *EL,*P1:22,"IS THIS RECORD YOU WANT ??",ANS
                    if (ANS = STAR)
                              goto QUES
                    elseif (ANS = YES)
                              break
                    endif
         repeat
.END PATCH 1.3 - OVERHAUL REPLACED LOGIC
KILL6
.START PATCH 1.3 - OVERHAUL REPLACED LOGIC
.         filepi    3;file6
..         READ      FILE6,KEY;;
..         DISPLAY   *EL,*P10:10,"KEY ",KEY,"    ",b1,ans,b1,mm,slash,dd,slash,yy
..         KEYIN     *EL,*P1:22,"are you sure ??",str1
..        CMATCH    "Y" TO str1
..         GOTO       keyseq IF not EQUAL
.          pack      key6 from lr
.         display   *p10:14,key6,*w3
..         bump      key by 7
..         move      key6 to key
../         READ      FILE6,KEY6;;
../         if        over
../         display   *p10:15,"WE ARE OVER",*w3
../         goto      end1
../         endif
.         DELETE   FILE6,key6
.         TRAPCLR   IO
.         CLOSE     FILE6
.....
         call   NCRCDEL
.END PATCH 1.3 - OVERHAUL REPLACED LOGIC
         KEYIN     *P1:22,*EL,*W4,*P14:22,"Would you like to do another??",STR1
         CMATCH     YES,STR1
         GOTO       K6 IF EQUAL
         GOTO      END1
.START PATCH 1.3 - OVERHAUL REMMED LOGIC
.KEY6     RESET     KEY
.         BUMP      KEY BY 13
.         MOVE      KEY6 TO KEY
.         FILEPI    1;FILE6
.         DELETEK   FILE6,KEY
.         TRAPCLR   IO
.         CLOSE     FILE6
.         GOTO      NOREC IF OVER
.         KEYIN     *P1:22,*EL,*W4,*P14:22,"Would you like to do another??",STR1
.         CMATCH     YES,STR1
.         GOTO       K6 IF EQUAL
.         GOTO      END1
.END PATCH 1.3 - OVERHAUL REMMED LOGIC
NOREC    BEEP
         BEEP
         DISPLAY   *P1:24,*EL,"NO RECORD WITH THIS KEY ON FILE !!!!!",*W;
         DISPLAY   *W,*W;
         GOTO      QUES

.Begin patch 2.0          
CheckPass
          pack      str55,"              To Proceed"
          setitem   PasswordStatMssg1,0,str55
          setprop   PasswordStatMssg1,visible=1
          setitem   PasswordEdit,0,""
          setfocus PasswordEdit
          clear     NPASFLD
          setprop   Passwrd,visible=1
          if (NPASFLD <> "COSMO")
          alert     note,"Invalid password!",result
          move      No,passflag
          Else
          alert     note,"permitted!",result
          move      Yes,passflag
          endif
          return
.end patch 2.0          
.
NOFILE   BEEP
         BEEP
         DISPLAY   *P1:23,*EL,"ERROR IS",ERROR
         DISPLAY   *P1:24,*EL,"FILE NOT ONLINE !!",*W,*W,*W;
         TRAPCLR   IO
END1
         STOP
           include  Npasio.inc
         INCLUDE   NCRCIO.INC
         include   comlogic.inc
