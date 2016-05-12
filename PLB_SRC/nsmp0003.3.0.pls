*******************************************************************************
. THIS IS THE sample description maint                                                  *
*******************************************************************************
. Written for Names in the News California By David Herrick                   *
*******************************************************************************
.
.
.
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
;Patch3.0
                                 include    compdd.inc
                                 include    cntdd.inc
.         INCLUDE   NMLRDD.inc
;Patch3.0
         include   nsmpdd.inc
         INCLUDE   HP.inc
page     form      2
mlrques  init      "mailer number : "
MODE     FORM      "18"
DIM2     DIM       2
ANS1     FORM      1

RELEASE  INIT      "3.0"       DMB 26MAY2004 Mailer Coversion
.RELEASE  INIT      "2.9"       ASH 11sep2003 ASH SAMPLE FORMAT CONVERTED - THIS PROGRAM IS PRETTY MUCH OBSOLETE!!!!
.RELEASE  INIT      "2.8"       ASH 02OCT2000 ASH NEW SERVER ADDED
.RELEASE  INIT      "2.7"       ASH 18FEB00 SAMPLE DIRECTORY MOVED
.release  init      "2.6"       ASH 02Nov98 NINSAMPL Y2K, File expansion
.release  init      "2.5"       DLH 20May98 Flag changed to Attrib
.release  init      "2.0"       DLH 07feb96 add delete function
.Release  init      "p.01"      dlh 25nov94.
Attrib    init      "c:\command.com /c attrib "

attropt  init      " -R"
.START PATCH 2.7 - REPLACED LOGIC
.DR       INIT      "f:\DATA\SAMPLES\s"
.START PATCH 2.8 REPLACED LOGIC
.DR       INIT      "G:\DATA\SAMPLES\s"
DR       DIM       35
         PACK      DR,NTWKPATH1,"SAMPLES\s"
.END PATCH 2.8 REPLACED LOGIC
.END PATCH 2.7 - REPLACED LOGIC
IMGNAME DIM       30
.START PATCH 2.9 REPLACED LOGIC
.DCX      INIT      ".DCX"        
DCX      INIT      ".TIF"
.END PATCH 2.9 REPLACED LOGIC
.
.
.
         MOVE      "MSMP0003" TO PROGRAM
         MOVE      "NAMES IN THE NEWS" TO COMPNME
.         MOVE      "K:NINMLR" TO NMLRNAME
         MOVE      C1 TO NMLRPATH
         move      "Exit" to pf5
         CALL      PAINT
         call      funcdisp
         trap      end if f5
start    KEYIN     *RESETSW:
                   *P35:05,"Mailer Number : ",*JR,*ZF,*white,nsmpmlr;
         scan      star in nsmpmlr
         goto      end if equal
         PACK      MKEY FROM nsmpmlr,Z3
         CALL      NMLRKEY
         DISPLAY    *P35:06,*red,MCOMP
keysmp   move      "001" to nsmpnum
         KEYIN     *P35:07,*yellow,"      SAMPLE NUMBER : ",*JR,*ZF,*Rv,*white,NSMPNUM;
         scan      star in nsmpnum
         goto      end  if equal
         PACK      NSMPFLD FROM nsmpmlr,NSMPNUM
         REP       ZFILL IN NSMPFLD
         CALL      NSMPKEY
         IF        OVER
         keyin     *P1:24,*EL,*B,*red,"NO SAMPLE DESC FOUND",*white,"Add it ?":
                    str1
         unpack     nsmpfld from nsmpmlr,nsmpnum
         cmatch     yes to str1
         goto       addsmp if equal
         goto       start
         ELSE
.Start Patch #2.6 - remmed and replaced line
.         unpack     nsmpdte into mm,dd,yy
         unpack     nsmpdte into str2,yy,mm,dd
.End Patch #2.6 - remmed and replaced line
         DISPLAY   *P35:08,NSMPDES1,B1,mm,slash,dd,slash,str2,yy:
                   *P35:09,NSMPDES2,b1,nsmpdate;
         ENDIF
         keyin     *p1:24,*red,"Modify ? ",*white,str1;
         cmatch    no to str1
         goto      keysmp if equal
         cmatch    yes to str1
         goto      modsmp if equal
         cmatch    "D" to str1
         goto      delsmp if equal
         goto       start
addsmp   keyin     *p20:08,"Line 1: ",*p52:08," Date of sample mm/dd/yy":
                   *p20:09,"Line 2: ":
                   *p28:08,*jl,*rv,nsmpdes1," ",*p68:08,*+,mm,"/",dd,"/",yy,*-:
                   *p28:09,*jl,*rv,nsmpdes2:
                   *p1:24,*el,"OK ?",str1
        cmatch     no to str1
        goto       addsmp if equal
        cmatch     star to str1
        goto       start if equal
.Start Patch #2.6 - remmed and replaced line
.        pack       nsmpdte from mm,dd,yy
        pack       nsmpdte from cc,yy,mm,dd
.End Patch #2.6 - remmed and replaced line
        clock      date to today
        unpack     today into mm,str1,dd,str1,yy
.Start Patch #2.6 - remmed and replaced line
.        pack       nsmpdate from mm,dd,yy
        pack       nsmpdate from cc,yy,mm,dd
        pack       NSMPUSER,user
.End Patch #2.6 - remmed and replaced line
        call       nsmpwrt
        goto       start           
modsmp  
.Start Patch #2.6 - remmed and replaced line
.        unpack     nsmpdte into mm,dd,yy
        if         (yy = "99" or YY = "98" or yy = "97")
        move       "19" to cc
        else
        move       "20" to cc
        endif
        unpack     nsmpdte into str2,yy,mm,dd
.End Patch #2.6 - remmed and replaced line
        display    *p28:08,nsmpdes1,*p52:08," Date of sample ":
                   mm,slash,dd,slash,yy:
                   *p28:09,nsmpdes2
        keyin      *p28:08,*jl,*rv,nsmpdes1,*p68:08,*+,mm,"/",dd,"/",yy,*-:
                   *p28:09,*jl,*rv,nsmpdes2:
                   *p1:24,*el,"OK ?",str1
        cmatch     no to str1
        goto       addsmp if equal
        cmatch     star to str1
        goto       start if equal
.Start Patch #2.6 - remmed and replaced line
.        pack       nsmpdte from mm,dd,yy
        pack       nsmpdte from cc,yy,mm,dd
.End Patch #2.6 - remmed and replaced line
        clock      date to today
        unpack     today into mm,str1,dd,str1,yy
.Start Patch #2.6 - remmed and replaced line
.        pack       nsmpdate from mm,dd,yy
        pack       nsmpdate from cc,yy,mm,dd
        pack       NSMPUSER,user
.End Patch #2.6 - remmed and replaced line
        call       nsmpupd
        goto       start          
delsmp   keyin     *p25:24,"Are you sure you wish to delete this record ? ",str1
         cmatch    yes to str1
         goto      start if not equal
         display   *p25:24,*el,"Deleting Sample Description"
         call      nsmpdel
         PACK      IMGNAME FROM DR,nsmpmlr,NSMPNUM,DCX
         display   *p25:24,*el,"Deleting Sample File"
         pack      taskname from attrib,imgname,attropt
         execute   taskname
         erase     imgname
         if        not over
         display   *p25:24,*el,"Deletion Done",*w4,*p25:24,*el
         else
         display   *p25:24,*el,"Deletion Error",*b,*w5
         endif
         goto      start
;Patch3.0
                                 include    compio.inc
                                 include    cntio.inc
;         INCLUDE   NMLRIO.inc
;Patch3.0
         include   nsmpio.inc
         INCLUDE   COMLOGIC.inc

