PC       EQU       0
         INC       COMMON.inc
         INC       NRTNDD.inc
         inc       hp.inc
         INCLUDE   CONS.inc
.START PATCH 1.5 ADDED LOGIC
          include   NUSEDD.INC
          include winapi.inc
.END PATCH 1.5 ADDED LOGIC
.
RELEASE   INIT     "1.6"        DLH Sunbelt PDF
Reldate   Init      "2013 April 25"
.RELEASE   INIT     "1.5"        ASH 28JUN2005 RETIREMENT OF PCL2PDF
.RELEASE   INIT     "1.4"        ASH 02OCT2000 NEW SERVER ADDED
.RELEASE   INIT     "1.3"        ASH 20AUG98 RETURN-TO Y2K
.RELEASE   INIT     "1.2"         JD 02JUN94 print to laser.
.RELEASE   INIT     "1.1"        DLH 28APR92  CONVERT TO NEW INCLUDES,CONS,
.                               COMLOGIC.
.
.START PATCH 1.5 REPLACED LOGIC
prfile    pfile
userlogn dim        7
font2   font
font3   font
.END PATCH 1.5 REPLACED LOGIC
SYSDATE  DIM       8
DATEMASK INIT      "99/99/99"
PAGE     FORM      5
DATE     DIM       8
LINES    FORM      2
ONE      FORM      "1"
FOUR     FORM      "4"
TEN      FORM      "10"
COUNT    FORM      5
TABLE    FORM      1
.
RTNUM1   DIM       4      1-4    RETURN-TO NUMBER. **KEY**
.RTCNTCT1 DIM       25     5-29   RETURN-TO CONTACT NAME.
RTCNTCT1 DIM       45     5-49   RETURN-TO CONTACT NAME.
.RTCOMP1  DIM       25    30-54   RETURN-TO COMPANY NAME.
RTCOMP1  DIM       45    50-94   RETURN-TO COMPANY NAME.
RTADDR1  DIM       25    95-119  RETURN-TO ADDRESS
RTCITY1  DIM       15   120-134  RETURN-TO CITY.
RTSTATE1 DIM       2    135-136  RETURN-TO STATE.
RTZIP1   DIM       10   137-146  RETURN-TO ZIP. LEFT JUST.
RTNAME1  DIM       10   147-156  PASSWORD NAME
.RTREVDT1 DIM       6    117-122  REVISED DATE
RTREVDT1 DIM       8    157-164  REVISED DATE.
.
RTNUM2   DIM       4      1-4    RETURN-TO NUMBER. **KEY**
.RTCNTCT2 DIM       25     5-29   RETURN-TO CONTACT NAME.
RTCNTCT2 DIM       45     5-49   RETURN-TO CONTACT NAME.
.RTCOMP2  DIM       25    30-54   RETURN-TO COMPANY NAME.
RTCOMP2  DIM       45    50-94   RETURN-TO COMPANY NAME.
RTADDR2  DIM       25    95-119  RETURN-TO ADDRESS
RTCITY2  DIM       15   120-134  RETURN-TO CITY.
RTSTATE2 DIM       2    135-136  RETURN-TO STATE.
RTZIP2   DIM       10   137-146  RETURN-TO ZIP. LEFT JUST.
RTNAME2  DIM       10   147-156  PASSWORD NAME
.RTREVDT2 DIM       6    117-122  REVISED DATE
RTREVDT2 DIM       8    157-164  REVISED DATE.
.
RTNUM3   DIM       4      1-4    RETURN-TO NUMBER. **KEY**
.RTCNTCT3 DIM       25     5-29   RETURN-TO CONTACT NAME.
RTCNTCT3 DIM       45     5-49   RETURN-TO CONTACT NAME.
.RTCOMP3  DIM       25    30-54   RETURN-TO COMPANY NAME.
RTCOMP3  DIM       45    50-94   RETURN-TO COMPANY NAME.
RTADDR3  DIM       25    95-119  RETURN-TO ADDRESS
RTCITY3  DIM       15   120-134  RETURN-TO CITY.
RTSTATE3 DIM       2    135-136  RETURN-TO STATE.
RTZIP3   DIM       10   137-146  RETURN-TO ZIP. LEFT JUST.
RTNAME3  DIM       10   147-156  PASSWORD NAME
.RTREVDT3 DIM       6    117-122  REVISED DATE
RTREVDT3 DIM       8    157-164  REVISED DATE.
.
RTNUM4   DIM       4      1-4    RETURN-TO NUMBER. **KEY**
.RTCNTCT4 DIM       25     5-29   RETURN-TO CONTACT NAME.
RTCNTCT4 DIM       45     5-49   RETURN-TO CONTACT NAME.
.RTCOMP4  DIM       25    30-54   RETURN-TO COMPANY NAME.
RTCOMP4  DIM       45    50-94   RETURN-TO COMPANY NAME.
RTADDR4  DIM       25    95-119  RETURN-TO ADDRESS
RTCITY4  DIM       15   120-134  RETURN-TO CITY.
RTSTATE4 DIM       2    135-136  RETURN-TO STATE.
RTZIP4   DIM       10   137-146  RETURN-TO ZIP. LEFT JUST.
RTNAME4  DIM       10   147-156  PASSWORD NAME
.RTREVDT4 DIM       6    117-122  REVISED DATE
RTREVDT4 DIM       8    157-164  REVISED DATE.
.
RTDATE1  DIM       10
RTDATE2  DIM       10
RTDATE3  DIM       10
RTDATE4  DIM       10
.
          IFNZ PC
                    OPEN      NRTNFLE3,"NINRTN/SORT",EXCLUSIVE
          XIF
          IFZ PC
.START PATCH 1.4 REPLACED LOGIC
.         OPEN      NRTNFLE3,"g:\DATA\NINRTN.SRT",EXCLUSIVE
.START PATCH 1.5 ADDED LOGIC
                    if (INPNAME = "")
.Only if running by itself!
                              pack      taskname,"\\nins1\e\data\text\NINRTN.dat,",NTWKPATH1,"NINRTN.srt;50-74"
                              sort      taskname
                    endif
.END PATCH 1.5 ADDED LOGIC
                    PACK      STR35,NTWKPATH1,"NINRTN.SRT"
                    OPEN      NRTNFLE3,STR35,EXCLUSIVE
.END PATCH 1.4 REPLACED LOGIC
          XIF
          MOVE      C1 TO NRTNFLG3
          MOVE      C3 TO NRTNPATH
.
          CLOCK     DATE TO DATE
          IFNZ PC
                    MOVE      DATEMASK TO SYSDATE
                    EDIT      DATE TO SYSDATE
          XIF
          IFZ PC
                    MOVE      DATE TO SYSDATE
          XIF
          MOVE      SYSDATE TO TODAY
          MOVE      "NINCAL" TO COMPNME
          MOVE      "MASTER RETURN-TO PRINT" TO STITLE
          MOVE      "NRTN0002" TO PROGRAM
.START PATCH 1.4 REPLACED LOGIC
.         splopen   "g:\data\ninrtn.lst"
.START PATCH 1.5 REPLACED LOGIC
.         PACK      STR35,NTWKPATH1,"NINRTN.LST"
.         splopen   STR35
..END PATCH 1.4 REPLACED LOGIC
.         print     hpdups,hp17ptch,*f
.         CALL      PAINT
.
          CALL      PAINT
          if (INPNAME = "")
.Only if running by itself!
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
                              move      "2",FUNC
                    endif
          endif
          if (FUNC = "2")
                    call      OpenPrtFile
                    create  font2,"Arial",size=8
                    create  font3,"Arial",size=8,italic
.Set up columns
                    move    "500",column
                    move    "2375",column1
                    move    "4375",column2
                    move    "6375",column3
                    prtpage prfile;*UNITS=*HIENGLISH,*font=font2;
          else
                    PACK      STR35,NTWKPATH1,"NINRTN.LST"
                    splopen   STR35
                    print     hpdups,hp17ptch,*f
          endif
.END PATCH 1.5 REPLACED LOGIC
.
          CALL      HEADER
.
LOOP
          CALL      NRTNSEQ
          GOTO EOJ IF OVER
.temporary for Suzie
	UpperCase 	RTCOMP
	scan		"INFOGROUP",RTCOMP
	goto	okok if equal
	UpperCase	OwnEmail
	scan		"INFOGROUP",ownEmail
	goto	okok if equal
	goto Loop

okok	reset	RTCOMP
	reset	OwnEmail


          ADD       ONE TO COUNT
          ADD       ONE TO TABLE
          DISPLAY   *P10:12,*EL,"RECORDS PROCESSED : ",COUNT
.
          BRANCH    TABLE OF ONE,TWO,THREE,FOUR
.         CALL      PRINT
.         MOVE      C0 TO TABLE
          DISPLAY   *P1:23,*BLINKON,*HON,"OOOPS",*B
          GOTO LOOP
.
ONE
          MOVE      RTNUM TO RTNUM1
          MOVE      RTCNTCT TO RTCNTCT1
          MOVE      RTCOMP TO RTCOMP1
          MOVE      RTADDR TO RTADDR1
          MOVE      RTCITY TO RTCITY1
          MOVE      RTSTATE TO RTSTATE1
          MOVE      RTZIP TO RTZIP1
          MOVE      RTNAME TO RTNAME1
          MOVE      RTREVDAT TO RTREVDT1
.
          GOTO LOOP
.
TWO
          MOVE      RTNUM TO RTNUM2
          MOVE      RTCNTCT TO RTCNTCT2
          MOVE      RTCOMP TO RTCOMP2
          MOVE      RTADDR TO RTADDR2
          MOVE      RTCITY TO RTCITY2
          MOVE      RTSTATE TO RTSTATE2
          MOVE      RTZIP TO RTZIP2
          MOVE      RTNAME TO RTNAME2
          MOVE      RTREVDAT TO RTREVDT2
.
          GOTO LOOP
.
THREE
          MOVE      RTNUM TO RTNUM3
          MOVE      RTCNTCT TO RTCNTCT3
          MOVE      RTCOMP TO RTCOMP3
          MOVE      RTADDR TO RTADDR3
          MOVE      RTCITY TO RTCITY3
          MOVE      RTSTATE TO RTSTATE3
          MOVE      RTZIP TO RTZIP3
          MOVE      RTNAME TO RTNAME3
          MOVE      RTREVDAT TO RTREVDT3
.
          GOTO LOOP
.
FOUR
          MOVE      RTNUM TO RTNUM4
          MOVE      RTCNTCT TO RTCNTCT4
          MOVE      RTCOMP TO RTCOMP4
          MOVE      RTADDR TO RTADDR4
          MOVE      RTCITY TO RTCITY4
          MOVE      RTSTATE TO RTSTATE4
          MOVE      RTZIP TO RTZIP4
          MOVE      RTNAME TO RTNAME4
          MOVE      RTREVDAT TO RTREVDT4
.
          CALL      PRINT
          MOVE      C0 TO TABLE
          GOTO LOOP
.
PRINT
.START PATCH 1.5 REPLACED LOGIC
..        COMPARE   "64" TO LINES
.         CALL      HEADER IF GREATER
.         CALL      HEADER IF EQUAL
.         PRINT     *1,"## ",RTNUM1,*33,"## ",RTNUM2:
.                   *65,"## ",RTNUM3,*97,"## ",RTNUM4:
.                   *N:
.                   *1,RTCNTCT1,*33,RTCNTCT2,*65,RTCNTCT3,*97,RTCNTCT4:
.                   *N:
.                   *1,RTCOMP1,*33,RTCOMP2,*65,RTCOMP3,*97,RTCOMP4:
.                   *FLUSH;
.         PRINT     *1,RTCOMP1,*33,RTCOMP2,*65,RTCOMP3,*97,RTCOMP4:
.                   *N:
.                   *1,RTADDR1,*33,RTADDR2,*65,RTADDR3,*97,RTADDR4:
.                   *N:
.                   *1,RTCITY1,"  ",RTSTATE1," ",RTZIP1:
.                   *33,RTCITY2,"  ",RTSTATE2," ",RTZIP2:
.                   *65,RTCITY3,"  ",RTSTATE3," ",RTZIP3:
.                   *97,RTCITY4,"  ",RTSTATE4," ",RTZIP4
..Y2K Conversion Patch
..         MOVE      DATEMASK TO RTDATE1
..         MOVE      DATEMASK TO RTDATE2
..         MOVE      DATEMASK TO RTDATE3
..         MOVE      DATEMASK TO RTDATE4
..         EDIT      RTREVDT1 TO RTDATE1
..         EDIT      RTREVDT2 TO RTDATE2
..         EDIT      RTREVDT3 TO RTDATE3
..         EDIT      RTREVDT4 TO RTDATE4
.         UNPACK    RTREVDT1 TO CC,YY,MM,DD
.         PACK      RTDATE1 WITH MM,SLASH,DD,SLASH,CC,YY
.         UNPACK    RTREVDT2 TO CC,YY,MM,DD
.         PACK      RTDATE2 WITH MM,SLASH,DD,SLASH,CC,YY
.         UNPACK    RTREVDT3 TO CC,YY,MM,DD
.         PACK      RTDATE3 WITH MM,SLASH,DD,SLASH,CC,YY
.         UNPACK    RTREVDT4 TO CC,YY,MM,DD
.         PACK      RTDATE4 WITH MM,SLASH,DD,SLASH,CC,YY
..
.         PRINT     *1,"UPDATED : ",RTDATE1:
.                   *33,"UPDATED : ",RTDATE2:
.                   *65,"UPDATED : ",RTDATE3:
.                   *97,"UPDATED : ",RTDATE4:
.                   *N:
.                   *1,"By ",RTNAME1:
.                   *33,"By ",RTNAME2:
.                   *65,"By ",RTNAME3:
.                   *97,"By ",RTNAME4,*L,*L
..
.         ADD       TEN TO LINES
...............................
          UNPACK    RTREVDT1 TO CC,YY,MM,DD
          PACK      RTDATE1 WITH MM,SLASH,DD,SLASH,CC,YY
          UNPACK    RTREVDT2 TO CC,YY,MM,DD
          PACK      RTDATE2 WITH MM,SLASH,DD,SLASH,CC,YY
          UNPACK    RTREVDT3 TO CC,YY,MM,DD
          PACK      RTDATE3 WITH MM,SLASH,DD,SLASH,CC,YY
          UNPACK    RTREVDT4 TO CC,YY,MM,DD
          PACK      RTDATE4 WITH MM,SLASH,DD,SLASH,CC,YY
          if (FUNC = "2")
                    if (row > 9000)
                    prtpage prfile;*NEWPAGE;
                    CALL      HEADER
                endif
                    prtpage prfile;*pcolumn:row,"## ",RTNUM1;
                    prtpage prfile;*pcolumn1:row,"## ",RTNUM2;
                    prtpage prfile;*pcolumn2:row,"## ",RTNUM3;
                    prtpage prfile;*pcolumn3:row,"## ",RTNUM4;
                    add       sixlpi,row
                    prtpage prfile;*pcolumn:row,RTCNTCT1;
                    prtpage prfile;*pcolumn1:row,RTCNTCT2;
                    prtpage prfile;*pcolumn2:row,RTCNTCT3;
                    prtpage prfile;*pcolumn3:row,RTCNTCT4;
                    add       sixlpi,row
                    prtpage prfile;*pcolumn:row,RTCOMP1;
                    prtpage prfile;*pcolumn1:row,RTCOMP2;
                    prtpage prfile;*pcolumn2:row,RTCOMP3;
                    prtpage prfile;*pcolumn3:row,RTCOMP4;
                    add       sixlpi,row
                    prtpage prfile;*pcolumn:row,RTADDR1;
                    prtpage prfile;*pcolumn1:row,RTADDR2;
                    prtpage prfile;*pcolumn2:row,RTADDR3;
                    prtpage prfile;*pcolumn3:row,RTADDR4;
                    add       sixlpi,row
                    prtpage prfile;*pcolumn:row,RTCITY1,"  ",RTSTATE1," ",RTZIP1;
                    prtpage prfile;*pcolumn1:row,RTCITY2,"  ",RTSTATE2," ",RTZIP2;
                    prtpage prfile;*pcolumn2:row,RTCITY3,"  ",RTSTATE3," ",RTZIP3;
                    prtpage prfile;*pcolumn3:row,RTCITY4,"  ",RTSTATE4," ",RTZIP4;
                    add       sixlpi,row
                    prtpage prfile;*pcolumn:row,"UPDATED : ",RTDATE1;
                    prtpage prfile;*pcolumn1:row,"UPDATED : ",RTDATE2;
                    prtpage prfile;*pcolumn2:row,"UPDATED : ",RTDATE3;
                    prtpage prfile;*pcolumn3:row,"UPDATED : ",RTDATE4;
                    add       sixlpi,row
                    prtpage prfile;*pcolumn:row,"By ",RTNAME1;
                    prtpage prfile;*pcolumn1:row,"By ",RTNAME2;
                    prtpage prfile;*pcolumn2:row,"By ",RTNAME3;
                    prtpage prfile;*pcolumn3:row,"By ",RTNAME4;
                    add       sixlpi,row
                    add       sixlpi,row
          else
                    COMPARE   "64" TO LINES
                    CALL      HEADER IF GREATER
                    CALL      HEADER IF EQUAL
                    PRINT     *1,"## ",RTNUM1,*33,"## ",RTNUM2:
                              *65,"## ",RTNUM3,*97,"## ",RTNUM4:
                              *N:
                              *1,RTCNTCT1,*33,RTCNTCT2,*65,RTCNTCT3,*97,RTCNTCT4:
                              *N:
                              *1,RTCOMP1,*33,RTCOMP2,*65,RTCOMP3,*97,RTCOMP4:
                              *FLUSH;
                    PRINT     *1,RTCOMP1,*33,RTCOMP2,*65,RTCOMP3,*97,RTCOMP4:
                              *N:
                              *1,RTADDR1,*33,RTADDR2,*65,RTADDR3,*97,RTADDR4:
                              *N:
                              *1,RTCITY1,"  ",RTSTATE1," ",RTZIP1:
                              *33,RTCITY2,"  ",RTSTATE2," ",RTZIP2:
                              *65,RTCITY3,"  ",RTSTATE3," ",RTZIP3:
                              *97,RTCITY4,"  ",RTSTATE4," ",RTZIP4
.
                    PRINT     *1,"UPDATED : ",RTDATE1:
                              *33,"UPDATED : ",RTDATE2:
                              *65,"UPDATED : ",RTDATE3:
                              *97,"UPDATED : ",RTDATE4:
                              *N:
                              *1,"By ",RTNAME1:
                              *33,"By ",RTNAME2:
                              *65,"By ",RTNAME3:
                              *97,"By ",RTNAME4,*L,*L
.
                    ADD       TEN TO LINES
          endif
.END PATCH 1.5 REPLACED LOGIC
.
          CLEAR     RTNUM1
          CLEAR     RTNUM2
          CLEAR     RTNUM3
          CLEAR     RTNUM4
.
          CLEAR     RTCNTCT1
          CLEAR     RTCNTCT2
          CLEAR     RTCNTCT3
          CLEAR     RTCNTCT4
.
          CLEAR     RTCOMP1
          CLEAR     RTCOMP2
          CLEAR     RTCOMP3
          CLEAR     RTCOMP4
.
          CLEAR     RTADDR1
          CLEAR     RTADDR2
          CLEAR     RTADDR3
          CLEAR     RTADDR4
.
          CLEAR     RTCITY1
          CLEAR     RTCITY2
          CLEAR     RTCITY3
          CLEAR     RTCITY4
.
          CLEAR     RTSTATE1
          CLEAR     RTSTATE2
          CLEAR     RTSTATE3
          CLEAR     RTSTATE4
.
          CLEAR     RTZIP1
          CLEAR     RTZIP2
          CLEAR     RTZIP3
          CLEAR     RTZIP4
.
          CLEAR     RTNAME1
          CLEAR     RTNAME2
          CLEAR     RTNAME3
          CLEAR     RTNAME4
.
          CLEAR     RTREVDT1
          CLEAR     RTREVDT2
          CLEAR     RTREVDT3
          CLEAR     RTREVDT4
.
          RETURN
.
HEADER
.START PATCH 1.5 REPLACED LOGIC
.         MOVE      FOUR TO LINES
.         ADD       ONE TO PAGE
.         PRINT     *F,*1,"CONFIDENTIAL":
.                   *26,"* * *   N I N   M A S T E R   ":
.                   "R E T U R N - T O   F I L E   * * *":
.                   *119,"DATE: ",SYSDATE:
.                   *N,*119,"PAGE: ",PAGE,*L
...........................
          if (FUNC = "2")
                    ADD       ONE TO PAGE
                    move      "375",row
                    prtpage prfile;*pcolumn:row,"CONFIDENTIAL";
                    prtpage prfile;*p3000:row,"* * *   N I N   M A S T E R   R E T U R N - T O   F I L E   * * *";
                    prtpage prfile;*p7000:row,"DATE: ",SYSDATE;
                    add       sixlpi,row
                    prtpage prfile;*p7000:row,"PAGE: ",PAGE;
                    move      "1000",row
          else
                    MOVE      FOUR TO LINES
                    ADD       ONE TO PAGE
                    PRINT     *F,*1,"CONFIDENTIAL":
                              *26,"* * *   N I N   M A S T E R   ":
                              "R E T U R N - T O   F I L E   * * *":
                              *119,"DATE: ",SYSDATE:
                              *N,*119,"PAGE: ",PAGE,*L
          endif
.END PATCH 1.5 REPLACED LOGIC
          RETURN
.START PATCH 1.5 ADDED LOGIC
OpenPrtFile
.begin patch 1.6
..         call      "GU$INI;GET_FROM_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
..                   "Parameters":
..                   "ProcessPDF":
..                   str45
.          call      GetPDfPAth
.          pack      str45 from PDFPATH,"\res\pdf995.ini"
.          call      "GU$INI;WRITE_TO_INI" USING STR45:
.                    "Parameters":
.                    "ProcessPDF":
.                    "\\nins1\e\apps\winbatch\del995flag.exe":
.                    result
.          if (result = C0)
..Prepare Flag file
.          call      GetPDfPAth
.          pack      str45 from PDFPATH,"\flag.dat"
.                    prep      tempfile,STR45
.                    write     tempfile,SEQ;"flag set"
.                    close     tempfile
.          endif
.
.          PRTOPEN   prfile,"PDF995","c:\work\pdf\NINRTN"
          PRTOPEN   prfile,"PDF:","c:\work\pdf\NINRTN.PDF"
.end patch 1.6
          return
.END PATCH 1.5 ADDED LOGIC
EOJ
          MATCH     "          " TO RTCOMP1
          GOTO EOJ1 IF EOS
          GOTO EOJ1 IF EQUAL
          CALL      PRINT
EOJ1
.START PATCH 1.5 REPLACED LOGIC
.         PRINT     hPPORT,hpdupoff,hpreset,*FLUSH
          if (FUNC = "2")
                    prtclose prfile
.Give the file a chance of rendering itself before updating the INI file.
.begin patch 1.6
.          call      GetPDfPAth
.          pack      str45 from PDFPATH,"\flag.dat"
.                    pack      APIFileName,STR45,hexzero
.                    loop
.                              call      FindFirstFile
.                              until (APIResult = 0 | APIResult = hexeight)
.                              pause     "1"
.                    repeat
.end patch 1.6

                    pause     "2"
          else
                    PRINT     hPPORT,hpdupoff,hpreset,*FLUSH
                    if (INPNAME = "")
                              close     NRTNFLE3
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
                              append    "copy ",taskname
                              append    NTWKPATH1,taskname
                              append    "NINRTN.lst \\nts0\laser2",taskname
                              reset     taskname
                              execute   taskname
.Clean up afterwards
                              erase     "c:\work\NINRTN.srt"
                              erase     "c:\work\NINRTN.lst"
                    endif
          ENDIF
.END PATCH 1.6 REPLACED LOGIC
          shutdown  "cls"
          STOP

          INCLUDE   NRTNIO.inc
.START PATCH 1.5 ADDED LOGIC
          include   NUSEIO.INC
.END PATCH 1.5 ADDED LOGIC
          INCLUDE   COMLOGIC.inc