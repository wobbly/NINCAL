PC       EQU       0
         INC       COMMON.INC
         INCLUDE   CONS.INC
;         INC       NBRKDD.INC
         include   oslspern.inc
          include compdd.inc
          include cntdd.inc
         inc       hp.inc
.START PATCH 1.6 ADDED LOGIC
          include   NUSEDD.INC
          include winapi.inc
.END PATCH 1.6 ADDED LOGIC
release   init      "1.8"        DLH   sunbelt PDF
Reldate   Init      "2013 April 23"       
.release   init      "1.71"        DLH   8Mar2007 Expand Oslspern
.release            init      "1.7"        DLH    28Jul2006 PATCHED PDF995.INI LOGIC
.Release        Init           "1.6"    ASH  27June2005  PCL2PDF Retirement
.Release        Init           "1.5"    DLH  23July2002  Inactive status
.RELEASE   INIT      "1.4"       ASH 05AUG98 Y2K CONVERSION
.release  INIT      "1.3"       DLH 03AUG93 ADDED SALESPERSON
..RELEASE  INIT      "1.2"       DLH 20JAN93   ADDED BROKER CONTACT
.RELEASE  INIT      "1.1"      DLH 29APR92
.
.needed for printing to PDF for non native fonts and things like Copyright to appear correctly in the pdf
PDF_FLAGS_WIN_ANSI_ENCODING EQU 128

.START PATCH 1.6 REPLACED LOGIC
prfile    pfile
userlogn dim        7
font2   font
font3   font
.END PATCH 1.6 REPLACED LOGIC
SYSDATE  DIM       8
DATEMASK INIT      "99/99/99"
PAGE     FORM      5
DATE     DIM       8
LINES    FORM      2
ZERO     FORM      "0"
ONE      FORM      "1"
FOUR     FORM      "4"
TEN      FORM      "10"
COUNT    FORM      5
TABLE    FORM      1
;.
BRKUM1   DIM       6      1-4    BROKER NUMBER. **KEY**
BRKCNT1  DIM       3
.BRCNTCT1 DIM       25     5-29   BROKER CONTACT NAME.
BRCNTCT1 DIM       45     5-29   BROKER CONTACT NAME.
.BRCOMP1  DIM       25    30-54   BROKER COMPANY NAME.
BRCOMP1  DIM       45    30-54   BROKER COMPANY NAME.
BRADDR1  DIM       25    55-79   BROKER ADDRESS
BRCITY1  DIM       15    80-94   BROKER CITY.
BRSTATE1 DIM       2     95-96   BROKER STATE.
BRZIP1   DIM       10    97-106  BROKER ZIP. LEFT JUST.
BRKAME1  DIM       10   107-116  PASSWORD NAME
.BRREVDT1 DIM       6    117-122  REVISED DATE.
BRREVDT1 DIM       8    117-122  REVISED DATE.
brksls1  dim       25            salesperson name
BrkInactive1 Dim     9             blank or "Inactive"
.
BRKUM2   DIM       6      1-4    BROKER NUMBER. **KEY**
BRKCNT2  DIM       3
.BRCNTCT2 DIM       25     5-29   BROKER CONTACT NAME.
BRCNTCT2 DIM       45     5-29   BROKER CONTACT NAME.
.BRCOMP2  DIM       25    30-54   BROKER COMPANY NAME.
BRCOMP2  DIM       45    30-54   BROKER COMPANY NAME.
BRADDR2  DIM       25    55-79   BROKER ADDRESS
BRCITY2  DIM       15    80-94   BROKER CITY.
BRSTATE2 DIM       2     95-96   BROKER STATE.
BRZIP2   DIM       10    97-106  BROKER ZIP. LEFT JUST.
BRKAME2  DIM       10   107-116  PASSWORD NAME
.BRREVDT2 DIM       6    117-122  REVISED DATE.
BRREVDT2 DIM       8    117-122  REVISED DATE.
brksls2  dim       25            salesperson name
BrkInactive2 Dim     9             blank or "Inactive"
.
BRKUM3   DIM       6      1-4    BROKER NUMBER. **KEY**
BRKCNT3  DIM       3
.BRCNTCT3 DIM       25     5-29   BROKER CONTACT NAME.
BRCNTCT3 DIM       45     5-29   BROKER CONTACT NAME.
.BRCOMP3  DIM       25    30-54   BROKER COMPANY NAME.
BRCOMP3  DIM       45    30-54   BROKER COMPANY NAME.
BRADDR3  DIM       25    55-79   BROKER ADDRESS
BRCITY3  DIM       15    80-94   BROKER CITY.
BRSTATE3 DIM       2     95-96   BROKER STATE.
BRZIP3   DIM       10    97-106  BROKER ZIP. LEFT JUST.
BRKAME3  DIM       10   107-116  PASSWORD NAME
.BRREVDT3 DIM       6    117-122  REVISED DATE.
BRREVDT3 DIM       8    117-122  REVISED DATE.
brksls3  dim       25            salesperson name
BrkInactive3 Dim     9             blank or "Inactive"
.
BRKUM4   DIM       6      1-4    BROKER NUMBER. **KEY**
BRKCNT4  DIM       3
.BRCNTCT4 DIM       25     5-29   BROKER CONTACT NAME.
BRCNTCT4 DIM       45     5-29   BROKER CONTACT NAME.
.BRCOMP4  DIM       25    30-54   BROKER COMPANY NAME.
BRCOMP4  DIM       45    30-54   BROKER COMPANY NAME.
BRADDR4  DIM       25    55-79   BROKER ADDRESS
BRCITY4  DIM       15    80-94   BROKER CITY.
BRSTATE4 DIM       2     95-96   BROKER STATE.
BRZIP4   DIM       10    97-106  BROKER ZIP. LEFT JUST.
BRKAME4  DIM       10   107-116  PASSWORD NAME
.BRREVDT4 DIM       6    117-122  REVISED DATE.
BRREVDT4 DIM       8    117-122  REVISED DATE.
brksls4  dim       25            salesperson name
BrkInactive4 Dim     9             blank or "Inactive"
.
.
BRDATE1  DIM       10
BRDATE2  DIM       10
BRDATE3  DIM       10
BRDATE4  DIM       10
slspern  dim       25
.        IFZ       PC
.         OPEN      COMPFILE7,"\\nins1\e\data\COMPANY.SRT",EXCLUSIVE
.START PATCH 1.6 REPLACED LOGIC
.         OPEN      COMPFILE7,INPNAME,EXCLUSIVE
          if (INPNAME = "")
.Only if running by itself!
                    pack      taskname,"\\nins1\e\data\text\company.dat,c:\work\company.srt;S=#"228='T'#",7-62"
                    sort      taskname
                    OPEN      COMPFILE7,"c:\work\company.srt",EXCLUSIVE
          else
                    OPEN      COMPFILE7,INPNAME,EXCLUSIVE
          endif
.END PATCH 1.6 REPLACED LOGIC
          MOVE      C1,COMPFLAG
;         OPEN      NBRKFLE3,"\\nins1\e\data\NINBRK.SRT",EXCLUSIVE
;temp for dave
;         OPEN      NBRKFLE3,"c:\work\unsusedbrk.dat",EXCLUSIVE
          XIF
          IFNZ      PC
;         OPEN      NBRKFLE7,"NINBRK/SORT",EXCLUSIVE
          XIF
.
;         MOVE      C1 TO NBRKFLG3
;         MOVE      C3 TO COMPPATH
;         MOVE      C3 TO NBRKPATH
          CLOCK     DATE TO DATE
          IFNZ      PC
                    MOVE      DATEMASK TO SYSDATE
                    EDIT      DATE TO SYSDATE
          XIF
          IFZ       PC
                    MOVE      DATE TO SYSDATE
          XIF
          MOVE      SYSDATE TO TODAY
.START PATCH 1.6 REPLACED LOGIC
.         match     b8 to program
.         if equal
          call      Trim using PROGRAM
          if (PROGRAM = "")
.END PATCH 1.6 REPLACED LOGIC
                    MOVE      "COMP0003" TO PROGRAM
                    move      "LOCAL" TO PRTNAME
.START PATCH 1.6 ADDED LOGIC
                    move      "1",FUNC
.END PATCH 1.6 ADDED LOGIC
          endif
          CALL      PAINT
          MOVE      "Names In The News Inc" TO COMPNME
          MOVE      "MASTER Broker/Consultant PRINT" TO STITLE
          match     "LOCAL" TO PRTNAME
          IF NOT EQUAL
                    display   *p10:12,"Print File : ",prtname
.START PATCH 1.6 REPLACED LOGIC
.                   PACK      PRTFILE FROM PDRIVE,PRTNAME
.                   SPLOPEN   PRTFILE
.                   print     hpdupl,hp17ptch,*f
                    if (FUNC = "2")
                              call      OpenPrtFile
                    else
                              PACK      PRTFILE FROM PDRIVE,PRTNAME
                              SPLOPEN   PRTFILE
                              print     hpdupl,hp17ptch,*f
                    endif
          else
PDFQuestion
                    clear     str1
                    keyin     *P10:13,*EL,"PDF Format? : ",*B,str1
                    if (str1 = "Y" | str1 = "y")
                              if (PORTN = 0)
                                        alert     note,"You must provide a Port Number!",result
                                        keyin     *P10:14,*EL,"Port Number : ",*B,PORTN
                                        if (PORTN = 0)
                                                  goto PDFQuestion
                                        endif
                              endif
                              move      C0,NUSEFLD
                              move      C1,NUSEPATH
                              move      PORTN,NUSEFLD
                              rep       zfill,NUSEFLD
                              call      NUSEKEY
                              scan      "INVALID",NUSEUSER
                              if equal
                                        alert     note,"Invalid Port Number for PDF creation!",result
                                        clear     PORTN
                                        goto PDFQuestion
                              endif
                              reset     NUSEUSER
                              scan      "BILLING",NUSEUSER
                              if equal
                                        alert     note,"Invalid Port Number for PDF creation!",result
                                        clear     PORTN
                                        goto PDFQuestion
                              endif
                              reset     NUSEUSER
                              call      Trim using NUSEUSER
                              move      NUSEUSER,str1
                              loop
                                        bump      NUSEUSER,1
                                        cmatch    B1,NUSEUSER
                                        until equal
                                        until eos
                              repeat
                              if not eos
                                        bump      NUSEUSER,1
                                        move      NUSEUSER,str6
                                        clear     userlogn
                                        pack      userlogn,str1,str6
                              endif
                              reset     NUSEUSER
.
                              call      OpenPrtFile
                              move      "2",FUNC
                    else
                              PACK      PRTFILE FROM "c:\work\COMPANY.LST"
                              SPLOPEN   PRTFILE
                              print     hpdupl,hp17ptch,*f
                    endif
.END PATCH 1.6 REPLACED LOGIC
          ENDIF
.START PATCH 1.6 ADDED LOGIC
          if (FUNC = "2")
                    create  font2,"Arial",size=8
                    create  font3,"Arial",size=8,italic
.Set up columns
                    move    "500",column
                    move    "2375",column1
                    move    "4375",column2
                    move    "6375",column3
                    prtpage prfile;*UNITS=*HIENGLISH,*font=font2;
          endif
.END PATCH 1.6 ADDED LOGIC
.
          CALL      HEADER
.
LOOP
          CALL      COMPSEQ
.         CALL      NBRKSEQ
          GOTO      EOJ IF OVER
          move      c0 to n3
          move      n3 to str3
          rep       zfill,str3
          packkey   cnctfld to compnum,str3
          call      cncttst
          if over
                    clear     nbrkvars
                    goto keepon
          endif
          loop
                    move      n3 to str3
                    rep       zfill,str3
                    packkey   cnctfld to compnum,str3
                    call      cnctkey
          until over
Keepon
          call      MVBRVARS
          ADD       ONE TO COUNT
          ADD       ONE TO TABLE
          DISPLAY   *P10:12,*EL,"RECORDS PROCESSED : ",COUNT
.
          move      osls0 to slspern
          move      brsales to n2
          LOAD      SLSpern FROM n2 OF OSLS1,OSLS2,OSLS3,OSLS4,OSLS5,OSLS6:
                    OSLS7,OSLS8,OSLS9,OSLS10,OSLS11,OSLS12,OSLS13,OSLS14:
                    OSLS15,OSLS16,OSLS17,OSLS18,OSLS19,OSLS20,OSLS21,OSLS22,osls23,osls24,osls25:
                    osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
          BRANCH    TABLE OF ONE,TWO,THREE,FOUR
.         CALL      PRINT
.         MOVE      ZERO TO TABLE
          DISPLAY   *P1:23,*BLINKON,*HON,"OOOPS",*B
          GOTO LOOPer
.
ONE
          move      compnum to brkum1
;         MOVE      BRKNUM TO BRKUM1
          MOVE      BRKCNT TO BRKCNT1
          MOVE      BRCNTCT TO BRCNTCT1
          MOVE      BRCOMP TO BRCOMP1
          MOVE      BRADDR TO BRADDR1
          MOVE      BRCITY TO BRCITY1
          MOVE      BRSTATE TO BRSTATE1
          MOVE      BRZIP TO BRZIP1
          MOVE      BRNAME TO BRKAME1
          MOVE      BRREVDAT TO BRREVDT1
          move      slspern to brksls1
          if (brINactive = "T")
                    move      " Inactive" to brkInactive1
          else
                    clear     BrkInactive1
          endif
;         .
          GOTO LOOPer
.
TWO
          move      compnum to brkum2
;         MOVE      BRKNUM TO BRKUM2
          MOVE      BRKCNT TO BRKCNT2
          MOVE      BRCNTCT TO BRCNTCT2
          MOVE      BRCOMP TO BRCOMP2
          MOVE      BRADDR TO BRADDR2
          MOVE      BRCITY TO BRCITY2
          MOVE      BRSTATE TO BRSTATE2
          MOVE      BRZIP TO BRZIP2
          MOVE      BRNAME TO BRKAME2
          MOVE      BRREVDAT TO BRREVDT2
          move      slspern to brksls2
          if (brINactive = "T")
                    move      " Inactive" to brkInactive2
          else
                    clear     BrkInactive2
          endif
.
          GOTO LOOPer
.
THREE
          move      compnum to brkum3
;         MOVE      BRKNUM TO BRKUM3
          MOVE      BRKCNT TO BRKCNT3
          MOVE      BRCNTCT TO BRCNTCT3
          MOVE      BRCOMP TO BRCOMP3
          MOVE      BRADDR TO BRADDR3
          MOVE      BRCITY TO BRCITY3
          MOVE      BRSTATE TO BRSTATE3
          MOVE      BRZIP TO BRZIP3
          MOVE      BRNAME TO BRKAME3
          MOVE      BRREVDAT TO BRREVDT3
          move      slspern to brksls3
          if (brINactive = "T")
                    move      " Inactive" to brkInactive3
          else
                    clear     BrkInactive3
          endif
.
          GOTO LOOPer
.
FOUR
          move      compnum to brkum4
;         MOVE      BRKNUM TO BRKUM4
          MOVE      BRKCNT TO BRKCNT4
          MOVE      BRCNTCT TO BRCNTCT4
          MOVE      BRCOMP TO BRCOMP4
          MOVE      BRADDR TO BRADDR4
          MOVE      BRCITY TO BRCITY4
          MOVE      BRSTATE TO BRSTATE4
          MOVE      BRZIP TO BRZIP4
          MOVE      BRNAME TO BRKAME4
          MOVE      BRREVDAT TO BRREVDT4
          move      slspern to brksls4
          if (brINactive = "T")
                    move      " Inactive" to brkInactive4
          else
                    clear     BrkInactive4
          endif
.
          CALL      PRINT
          MOVE      ZERO TO TABLE
looper
          add       c1 to n3
          repeat
          GOTO LOOP
.
PRINT
.START PATCH 1.6 REPLACED LOGIC
.         COMPARE   "64" TO LINES
.         CALL      HEADER IF GREATER
.         CALL      HEADER IF EQUAL
.         PRINT     *1,"## ",BRKUM1,"/",BRKCNT1,*33,"## ",BRKUM2,"/",BRKCNT2:
.                   *65,"## ",BRKUM3,"/",BRKCNT3,*97,"## ",BRKUM4,"/",BRKCNT4:
.                   *N:
.                   *1,BRCNTCT1,*33,BRCNTCT2,*65,BRCNTCT3,*97,BRCNTCT4:
.                   *N:
.                   *1,BRCOMP1,*33,BRCOMP2,*65,BRCOMP3,*97,BRCOMP4:
.                   *FLUSH;
.         PRINT     *1,BRCOMP1,*33,BRCOMP2,*65,BRCOMP3,*97,BRCOMP4:
.                   *N:
.                   *1,BRADDR1,*33,BRADDR2,*65,BRADDR3,*97,BRADDR4:
.                   *N:
.                   *1,BRCITY1,"  ",BRSTATE1," ",BRZIP1:
.                   *33,BRCITY2,"  ",BRSTATE2," ",BRZIP2:
.                   *65,BRCITY3,"  ",BRSTATE3," ",BRZIP3:
.                   *97,BRCITY4,"  ",BRSTATE4," ",BRZIP4
............................
          if (FUNC = "2")
                    if (row > 9000)
                    prtpage prfile;*NEWPAGE;
                    CALL      HEADER
                endif
                    prtpage prfile;*pcolumn:row,"## ",BRKUM1,"/",BRKCNT1;
                    prtpage prfile;*pcolumn1:row,"## ",BRKUM2,"/",BRKCNT2;
                    prtpage prfile;*pcolumn2:row,"## ",BRKUM3,"/",BRKCNT3;
                    prtpage prfile;*pcolumn3:row,"## ",BRKUM4,"/",BRKCNT4;
                    add       sixlpi,row
                    prtpage prfile;*pcolumn:row,BRCNTCT1;
                    prtpage prfile;*pcolumn1:row,BRCNTCT2;
                    prtpage prfile;*pcolumn2:row,BRCNTCT3;
                    prtpage prfile;*pcolumn3:row,BRCNTCT4;
                    add       sixlpi,row
                    prtpage prfile;*pcolumn:row,BRCOMP1;
                    prtpage prfile;*pcolumn1:row,BRCOMP2;
                    prtpage prfile;*pcolumn2:row,BRCOMP3;
                    prtpage prfile;*pcolumn3:row,BRCOMP4;
                    add       sixlpi,row
                    prtpage prfile;*pcolumn:row,BRADDR1;
                    prtpage prfile;*pcolumn1:row,BRADDR2;
                    prtpage prfile;*pcolumn2:row,BRADDR3;
                    prtpage prfile;*pcolumn3:row,BRADDR4;
                    add       sixlpi,row
                    prtpage prfile;*pcolumn:row,BRCITY1,"  ",BRSTATE1," ",BRZIP1;
                    prtpage prfile;*pcolumn1:row,BRCITY2,"  ",BRSTATE2," ",BRZIP2;
                    prtpage prfile;*pcolumn2:row,BRCITY3,"  ",BRSTATE3," ",BRZIP3;
                    prtpage prfile;*pcolumn3:row,BRCITY4,"  ",BRSTATE4," ",BRZIP4;
          else
                    COMPARE   "64" TO LINES
                    CALL      HEADER IF GREATER
                    CALL      HEADER IF EQUAL
                    PRINT     *1,"## ",BRKUM1,"/",BRKCNT1,*33,"## ",BRKUM2,"/",BRKCNT2:
                              *65,"## ",BRKUM3,"/",BRKCNT3,*97,"## ",BRKUM4,"/",BRKCNT4:
                              *N:
                              *1,BRCNTCT1,*33,BRCNTCT2,*65,BRCNTCT3,*97,BRCNTCT4:
                              *N:
                              *1,BRCOMP1,*33,BRCOMP2,*65,BRCOMP3,*97,BRCOMP4:
                              *FLUSH;
                    PRINT     *1,BRCOMP1,*33,BRCOMP2,*65,BRCOMP3,*97,BRCOMP4:
                              *N:
                              *1,BRADDR1,*33,BRADDR2,*65,BRADDR3,*97,BRADDR4:
                              *N:
                              *1,BRCITY1,"  ",BRSTATE1," ",BRZIP1:
                              *33,BRCITY2,"  ",BRSTATE2," ",BRZIP2:
                              *65,BRCITY3,"  ",BRSTATE3," ",BRZIP3:
                              *97,BRCITY4,"  ",BRSTATE4," ",BRZIP4
          endif
.END PATCH 1.6 REPLACED LOGIC
.Y2K Conversion Patch
.         MOVE      DATEMASK TO BRDATE1
.         MOVE      DATEMASK TO BRDATE2
.         MOVE      DATEMASK TO BRDATE3
.         MOVE      DATEMASK TO BRDATE4
.         EDIT      BRREVDT1 TO BRDATE1
.         EDIT      BRREVDT2 TO BRDATE2
.         EDIT      BRREVDT3 TO BRDATE3
.         EDIT      BRREVDT4 TO BRDATE4
          UNPACK    BRREVDT1 TO CC,YY,MM,DD
          PACK      BRDATE1  WITH MM,SLASH,DD,SLASH,CC,YY
          UNPACK    BRREVDT2 TO CC,YY,MM,DD
          PACK      BRDATE2  WITH MM,SLASH,DD,SLASH,CC,YY
          UNPACK    BRREVDT3 TO CC,YY,MM,DD
          PACK      BRDATE3  WITH MM,SLASH,DD,SLASH,CC,YY
          UNPACK    BRREVDT4 TO CC,YY,MM,DD
          PACK      BRDATE4  WITH MM,SLASH,DD,SLASH,CC,YY
.START PATCH 1.6 ADDED LOGIC
          if (FUNC = "2")
                    add       sixlpi,row
                    prtpage prfile;*pcolumn:row,"UPDATED : ",BRDATE1;
                    prtpage prfile;*pcolumn1:row,"UPDATED : ",BRDATE2;
                    prtpage prfile;*pcolumn2:row,"UPDATED : ",BRDATE3;
                    prtpage prfile;*pcolumn3:row,"UPDATED : ",BRDATE4;
                    add       sixlpi,row
                    prtpage prfile;*pcolumn:row,"By ",BRKAME1;
                    prtpage prfile;*pcolumn1:row,"By ",BRKAME2;
                    prtpage prfile;*pcolumn2:row,"By ",BRKAME3;
                    prtpage prfile;*pcolumn3:row,"By ",BRKAME4;
                    add       sixlpi,row
                    prtpage prfile;*pcolumn:row,"Sls: ",*font=font3,BRKsls1,*font=font2;
                    prtpage prfile;*pcolumn1:row,"Sls: ",*font=font3,BRKsls2,*font=font2;
                    prtpage prfile;*pcolumn2:row,"Sls: ",*font=font3,BRKsls3,*font=font2;
                    prtpage prfile;*pcolumn3:row,"Sls: ",*font=font3,BRKsls4,*font=font2;
                    add       sixlpi,row
                    add       sixlpi,row
          else
.END PATCH 1.6 ADDED LOGIC
                    PRINT     *1,"UPDATED : ",BRDATE1:
                              *33,"UPDATED : ",BRDATE2:
                              *65,"UPDATED : ",BRDATE3:
                              *97,"UPDATED : ",BRDATE4:
                              *N:
                              *1,"By ",BRKAME1:
                              *33,"By ",BRKAME2:
                              *65,"By ",BRKAME3:
                              *97,"By ",BRKAME4,*L:
                              *1,"Sls: ",hpitalic,BRKsls1,hpuprght:
                              *33,"Sls: ",hpitalic,BRKsls2,hpuprght:
                              *65,"Sls: ",hpitalic,BRKsls3,hpuprght:
                              *97,"Sls: ",hpitalic,BRKsls4,hpuprght,*l
.START PATCH 1.6 ADDED LOGIC
          endif
.END PATCH 1.6 ADDED LOGIC
.
          ADD       TEN TO LINES
.
          CLEAR     BRKUM1
          CLEAR     BRKUM2
          CLEAR     BRKUM3
          CLEAR     BRKUM4
.
          CLEAR     BRKCNT1
          CLEAR     BRKCNT2
          CLEAR     BRKCNT3
          CLEAR     BRKCNT4
.
          CLEAR     BRCNTCT1
          CLEAR     BRCNTCT2
          CLEAR     BRCNTCT3
          CLEAR     BRCNTCT4
.
          CLEAR     BRCOMP1
          CLEAR     BRCOMP2
          CLEAR     BRCOMP3
          CLEAR     BRCOMP4
.
          CLEAR     BRADDR1
          CLEAR     BRADDR2
          CLEAR     BRADDR3
          CLEAR     BRADDR4
.
          CLEAR     BRCITY1
          CLEAR     BRCITY2
          CLEAR     BRCITY3
          CLEAR     BRCITY4
.
          CLEAR     BRSTATE1
          CLEAR     BRSTATE2
          CLEAR     BRSTATE3
          CLEAR     BRSTATE4
.
          CLEAR     BRZIP1
          CLEAR     BRZIP2
          CLEAR     BRZIP3
          CLEAR     BRZIP4
.
          CLEAR     BRKAME1
          CLEAR     BRKAME2
          CLEAR     BRKAME3
          CLEAR     BRKAME4
.
          CLEAR     BRREVDT1
          CLEAR     BRREVDT2
          CLEAR     BRREVDT3
          CLEAR     BRREVDT4
.
          CLEAR     BRKsls1
          CLEAR     BRKsls2
          CLEAR     BRKsls3
          CLEAR     BRKsls4
.
          Clear     BrkInactive1
          Clear     BrkInactive2
          Clear     BrkInactive3
          Clear     BrkInactive4
          RETURN
.
HEADER
.START PATCH 1.6 ADDED LOGIC
          if (FUNC = "2")
                    ADD       ONE TO PAGE
                    move      "375",row
                    prtpage prfile;*pcolumn:row,"CONFIDENTIAL";
                    prtpage prfile;*p3000:row,"* * *   N I N   M A S T E R   B R O K E R   F I L E   * * *";
                    prtpage prfile;*p7000:row,"DATE: ",SYSDATE;
                    add       sixlpi,row
                    prtpage prfile;*p7000:row,"PAGE: ",PAGE;
                    move      "1000",row
          else
.END PATCH 1.6 ADDED LOGIC
                    MOVE      FOUR TO LINES
                    ADD       ONE TO PAGE
                    PRINT     *F,*N,*1,"CONFIDENTIAL":
                              *26,"* * *   N I N   M A S T E R   ":
                              "B R O K E R   F I L E   * * *":
                              *119,"DATE: ",SYSDATE:
                              *N,*119,"PAGE: ",PAGE,*L
.START PATCH 1.6 ADDED LOGIC
          endif
.END PATCH 1.6 ADDED LOGIC
          RETURN
.START PATCH 1.6 ADDED LOGIC
OpenPrtFile
.         call      "GU$INI;GET_FROM_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.                   "Parameters":
.                   "ProcessPDF":
.                   str45
.begin patch xxx
.                                                  Call      GetPDFPath
.                                                 pack      str45 from PDFPATH,"\res\pdf995.ini"
. 
.                                                 call      "GU$INI;WRITE_TO_INI" USING str45:
.                                                            "Parameters":
.                                                            "ProcessPDF":
.                                                            "\\nins1\e\apps\winbatch\Del995flag.exe":
.                    result
.          if (result = C0)
..Prepare Flag file
.                    pack      str45 from PDFPATH,"\Flag.dat"
.                    prep      tempfile,str45
.                    write     tempfile,SEQ;"flag set"
.                    close     tempfile
.          endif
.
          PRTOPEN   prfile,"PDF:","c:\work\pdf\COMPANY.pdf",Flags=PDF_FLAGS_WIN_ANSI_ENCODING
          return
.END PATCH 1.6 ADDED LOGIC

EOJ
          MATCH     "          " TO BRCOMP1
          GOTO      EOJ1 IF EOS
          GOTO      EOJ1 IF EQUAL
          CALL      PRINT
EOJ1
.START PATCH 1.6 REPLACED LOGIC
.         PRINT     hPPORT,hpdupoff,hpreset,*FLUSH
          if (FUNC = "2")
                    prtclose prfile
.Give the file a chance of rendering itself before updating the INI file.

.          call      GetPDfPAth
.          pack      str45 from PDFPATH,"\flag.dat"
.                    pack      APIFileName,STR45,hexzero
..START PATCH 1.7 REPLACED LOGIC
..                             loop
.                              for N3,C1,"100"
..END PATCH 1.7 REPLACED LOGIC 
.                              call      FindFirstFile
.                              until (APIResult = 0 | APIResult = hexeight)
.                              pause     "1"
.                    repeat
                    pause     "2"
          else
                    PRINT     hPPORT,hpdupoff,hpreset,*FLUSH
                    match     "LOCAL" TO PRTNAME
                    IF EQUAL
                              close     COMPFILE7
                              splclose
.Print out since not run by Batch program
                              clear     taskname
                              call      GetWinVer
                              Path      Exist,"c:\windows"
                              if over                       .nt/2000
                                        append    "!c:\winnt\system32\cmd.exe /c ",taskname
                              elseif (osflag = c6)          .XP
                                        append    "!c:\windows\system32\cmd.exe /c ",taskname
                              else                          .95/98
                                        append    "!c:\command.com /c ",taskname
                              endif
                              append    "copy c:\work\COMPANY.lst \\NINs2\Laser2",taskname
                              reset     taskname
                              execute   taskname
.Clean up afterwards
                              erase     "c:\work\COMPANY.srt"
                              erase     "c:\work\COMPANY.lst"
                    endif
          ENDIF
.END PATCH 1.6 REPLACED LOGIC
          shutdown  "cls"
          STOP

          include   cntio.inc
          include   compio.inc
;         INC       NBRKIO.NEW
;         INCLUDE   NBRKIO.INC
.START PATCH 1.6 ADDED LOGIC
          include   NUSEIO.INC
.END PATCH 1.6 ADDED LOGIC
          INCLUDE   COMLOGIC.INC