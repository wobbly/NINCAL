* ****************************************************************************
* TO MARK AN UNBILLED REC BILLED CHANGE SECOND BYTE OF NININV OR CMPINV FROM
* 0 TO B
* X TO Q
* TO MARK AN UNPAID PAID
* 0 TO P
* ****************************************************************************
* TO REINSTATE AN ORDER CHANGE SECOND BYTE OF NINORD OR CMPORD FROM
* X TO 0
* Q TO B
* *****************************************************************************
* *****************************************************************************
PC         EQU         0
         INCLUDE   COMMON.inc
           INCLUDE   CONS.inc
           include  Npasdd.inc
Release   INIT 	"2.1"	DLH  allow change of OCCODE
reldate	Init	"2014 October 22"
.Release   INIT 	"2.0"	DLH  GUI	
.RELEASE  INIT      "1.22"         ASH  03JUN2013 New LCR Status code
.RELEASE  INIT      "1.21"         JD  01OCT2007 change IF to and not an OR
.RELEASE  INIT      "1.2"         DMB 18JUN2005 Changed IP address of File Manager
.RELEASE  INIT      "1.1"         DLH 30SEP92  CONVERT TO PCBUS.
IFILE    IFILE     KEYLEN=6,VAR=566,UNCOMP
DATA     DIM       1
CODE     DIM       1
OCCODE	Dim	1
LR       DIM       6
INAME    DIM       20
HOldName  Dim       20
CompID    DIm       1                mailer exclusive bytte
CompID2   Dim       1              LIst exclusive byte
OHIST     Dim       1
Obildrct  Dim       1
OK       DIM       5
x              plform         Mark001
pass      plform    Passwrd

               winhide
yesno1    integer   1,"0x000024"                yes no buttons, question Icon
Changes   Form      1
.Load Forms, Always load parent form first
            formload       x
          formload pass
               
Timer     Timer
          create          TIMER,18000     .30 minutes
            activate TIMER,Timeout,RESULT
          clear     taskname
          append    "TO MARK A UNBILLED ORDER BILLED",taskname
          append    CRLF,taskname
          append    "Change : '0' to 'B' or 'X' to 'Q'",taskname
          append    CRLF,taskname
          append    CRLF,taskname
          append    "TO MARK A BILLED ORDER UNBILLED",taskname
          append    CRLF,taskname
          append    "Change : 'B' to '0' or 'Q' to 'X'",taskname
          append    CRLF,taskname
          append    CRLF,taskname
          append    "TO Change Company Affiliation",taskname
          append    CRLF,taskname
          append    "Change : ' ' to 'P' or 'P' to ' ' ",taskname
          append    CRLF,taskname
          append    CRLF,taskname
          append    CRLF,taskname
          append    "TO Campaign (screen 7) test status",taskname
          append    CRLF,taskname
          append    "' '=Continuation  '1'=Test, '2'=Retest",taskname
          append    CRLF,taskname
          Reset     Taskname
          SetItem   Mark001EditText006,0,Taskname

         TRAP      STOP IF F5
          pack      str55,"              To Proceed"
          setitem   PasswordStatMssg1,0,str55
          setprop   PasswordStatMssg1,visible=1
          setitem   PasswordEdit,0,""
          setfocus PasswordEdit
          clear     NPASFLD
          Clear     Progcode
          setprop   Passwrd,visible=1
          rep	lowup,Npasfld
          if (NPASFLD <> "COSMO")
          alert caution,"Invalid Password...BYE!",result
          Stop
          endif
.............................
          alert     note,"permitted!",result
          
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
.	move	"NINPRINT.isi|NINS1:502",holdname
          getItem   Mark001EditText003,0,str25
          call      Trim using str25
          Rep       LowUp in Str25
          if        (HoldName = "")
          Move      Str25,HOldname
          endif
          
          if        (Str25 = "NINORD" & Holdname = str25)
          OPEN      IFILE,"NINORD.ISI|NINS1:502"
          Elseif    (Str25 = "NINORD" & Holdname <>  str25)
          Close     IFile
          MOve      Str25,HOldname
          OPEN      IFILE,"NINORD.ISI|NINS1:502"
          Elseif    (Str25 <> "NINORD")
          Close     IFile
          MOve      Str25,HOldname
          OPEN      IFILE,INAME
          endif

          getItem   Mark001EditText001,0,str6
          call      Trim using str6
          move      str6 to LR
          if        (LR = "")
          SetItem   Mark001StatText007,0,"No Record Found"
          return
          endif
.                   READ                IFILE,LR;DATA,CODE,*tab=335,compid,compid2
          READ                IFILE,LR;DATA,CODE,*tab=84,OCCODE,*tab=132,Obildrct,*tab=176,Ohist,*tab=335,compid,compid2
          goto      Dododoit if not over
          SetItem   Mark001StatText007,0,"No Record Found"
          Return
DoDoDoit


          Setitem   Mark001EditText2,0,Data
          Setitem   Mark001EditText002,0,Code
          Setitem   Mark001EditText004,0,Compid
          Setitem   Mark001EditText005,0,Compid2
          Setitem   Mark001EditText007,0,Obildrct
          SetItem   Mark001StatText007,0,"Found Record"
          SetItem   Mark001EditText1,0,Ohist
.begin patch 2.1	
	  setitem	Mark001EditText008,0,OCCode
.end patch 2.1	
          REturn
Saveit
          Move      C0,changes

          Getitem   Mark001EditText2,0,str1
          if        (Str1 <> Data)
                    If        (str1 <> "S" and STR1 <> "F")
                    alert type=yesno1," Invalid Order Code OK????",n1
                              if (n1="7")    . 6 = yes , 7 = no          
                              return
                              endif
                    endif     
          add       c1 to Changes
          move      Str1,Data
          endif





          Getitem   Mark001EditText002,0,Str1
          if        (Str1 <> Code)
.Start patch 1.21
                    If        (str1 <> "0" and STR1 <> "B" and str1 <> "Q" and str1 <> "X")
.                   If        (str1 <> "0" or STR1 <> "B" or str1 <> "Q" or str1 <> "X")
.End patch 1.21
                    alert type=yesno1," Invalid Order Code OK????",n1
                              if (n1="7")    . 6 = yes , 7 = no          
                              return
                              endif
                    endif     
          add       c1 to Changes
          move      Str1,Code
          endif


        move    b1,str1
          Getitem   Mark001EditText004,0,Str1
          if        (Str1 <> Compid)
.Start patch 1.21
.                   If        (str1 <> "P" or STR1 <> "" or str1 <> " ")
                    If        (str1 <> "P" and STR1 <> "" and str1 <> " ")
.End patch 1.21
                    alert type=yesno1," Invalid EXCL Code OK????",n1
                              if (n1="7")    . 6 = yes , 7 = no          
                              return
                              endif
                    endif     
          add       c1 to Changes
          move      Str1,Compid
          endif
          
        move    b1,str1
          Getitem   Mark001EditText005,0,Str1
          if        (Str1 <> Compid2)
.Start patch 1.21
.                   If        (str1 <> "P" or STR1 <> "" or str1 <> " ")
                    If        (str1 <> "P" and STR1 <> "" and str1 <> " ")
.End patch 1.21
                    alert type=yesno1," Invalid EXCL Code OK????",n1
                              if (n1="7")    . 6 = yes , 7 = no          
                              return
                              endif
                    endif     
          add       c1 to Changes
          move      Str1,Compid2
          endif

        move    b1,str1
          Getitem   Mark001EditText007,0,Str1
          if        (Str1 <> Obildrct)
                    If        (str1 <> "Y" and STR1 <> "N" and str1 <> " ")
                    alert type=yesno1," Invalid bill direct Code OK????",n1
                              if (n1="7")    . 6 = yes , 7 = no          
                              return
                              endif
                    endif     
          add       c1 to Changes
          move      Str1,Obildrct
          endif

        move    b1,str1

          GetItem   Mark001EditText1,0,str1
          if        (Str1 <> Ohist)
.START PATCH 1.22 REPLACED LOGIC
.                    If        (str1 <> "l" & STR1 <> "L" & str1 <> "p" & str1 <> "e" & str1 <> "E" & str1 <> "*" & str1 <> "z")
                    If        (str1 <> "l" & STR1 <> "L" & str1 <> "p" & str1 <> "e" & str1 <> "E" & str1 <> "*" & str1 <> "z" & str1 <> "t")
.END PATCH 1.22 REPLACED LOGIC
                    alert type=yesno1," Invalid Ohist Code OK????",n1
                              if (n1="7")    . 6 = yes , 7 = no          
                              return
                              endif
                    endif     
          add       c1 to Changes
          move      Str1,Ohist
          endif
.begin patch 2.1

	  Getitem	Mark001EditText008,0,str1
	  	if	(str1 <> OCCODE)
                    If        (str1 <> " " & STR1 <> "0" & str1 <> "1")
                    alert type=yesno1," Invalid Occode Code OK????",n1
                              if (n1="7")    . 6 = yes , 7 = no          
                              return
                              endif
                    endif     
          add       c1 to Changes
          move      Str1,Occode
          endif
.end patch 2.1


          IF        (changes > 0)
          FILEPI              1;IFILE
          UPDATAB             IFILE;DATA,CODE,*tab=84,OCCODE,*tab=132,Obildrct,*tab=176,Ohist,*tab=335,compid,compid2
          pack      Str25 from Changes," changes made"
          return
          Else
          SetItem   Mark001StatText007,0,"No Changes ??"
          REturn
          endif


          
.         DISPLAY    *P01:23,*ES
.         DISPLAY   *P01:23,*EL,"HIT F3 TO END"
COSMO
         KEYIN     *P01:01,*EL,*EOFF,"ENTER PASSWORD",OK
         MATCH     "COSMO",OK
         STOP      IF NOT EQUAL
         KEYIN     *P1:4,*EF,"ENTER INPUT FILE: ",INAME
         DISPLAY   *P01:12,"TO MARK A UNBILLED ORDER BILLED CHANGE:":
                   *P04:13,"0 TO B   OR   X TO Q ":
                   *P01:14,"TO REINSTATE AN ORDER CHANGE:":
                   *P04:15,"X TO 0   OR   Q TO B ":
                   *P01:14,"TO Change Exclusivity of AN ORDER CHANGE:":
                   *P04:15,"P TO ' '   OR  ' ' TO P "
                   
         DISPLAY   *P1:24,"THIS PROGRAM UPDATES THE SECOND (STATUS) BYTE";
         rep       lowup in iname
         if        (iname = "NINORD")
.Patch 1.2 Begin
.         OPEN      IFILE,"NINORD.ISI|20.20.30.103:502"
         OPEN      IFILE,"NINORD.ISI|NINS1:502"
.Patch 1.2 End         
         else
         OPEN      IFILE,INAME
         endif
START    KEYIN     *P1:6,*EL,"ENTER LR TO BE MARKED: ",*ZF,*JR,LR
         CMATCH    " ",LR
         GOTO      START IF EOS
         MATCH     "00000*",LR
         GOTO      STOP IF EQUAL
         READ      IFILE,LR;DATA,CODE,*tab=335,compid,compid2
         GOTO      NOHIT IF OVER
         DISPLAY   *P1:4,*EL,"PRESENT CODE IN THIS BYTE IS: ",CODE
         DISPLAY   *P1:6,*EL,"PRESENT CompID IS           : ",COmpId
         DISPLAY   *P1:8,*EL,"PRESENT COmpID2 IS          : ",COmpid2
         KEYIN     *P1:5,*EL,"ENTER CODE TO BE UPDATED    : ",*RV,CODE
         KEYIN     *P1:7,*EL,"ENTER CODE TO BE UPDATED    : ",*RV,CompID
         KEYIN     *P1:9,*EL,"ENTER CODE TO BE UPDATED    : ",*RV,CompID2
         FILEPI    1;IFILE
         UPDATAB   IFILE;DATA,CODE,*tab=335,compid,compid2
         GOTO      START
NOHIT    DISPLAY   *P1:24,"LR NUMBER NOT IN FILE !!!!",*W,*W,*W;
         GOTO      START
STOP     winshow
	STOP
           include  Npasio.inc
           INCLUDE   COMLOGIC.inc

