PC       EQU       0
          INCLUDE   COMMON.inc
          INCLUDE   CONS.inc
          INCLUDE   NORDDD.inc
          include   nowndd.inc
          INCLUDE   NDATDD.inc
release   init      "3.24"       DLH  changed to 6 months per SA
reldate   INit      "23 September 2009"
.release   init                "3.23"       DLH  fixed to take  recent orders as documented
.reldate   INit      "18 September 2009"
.release   init                "3.22"       DLH  Internal Index
.reldate   INit      "23 April 2008"
.release  init      "3.21"       JD  18Nov2005    New sunindex cmd, PLB90
.release  init      "3.2"       ASH 07APR2005     COMMPER Conversion
.release  init      "3.1"       DMB 11JUN04      Add code for order qty column that counts orders placed for year
.release  init      "3.0"       ASH 10MAY04      REWRITE FOR SPEED
.release  init      "2.2"       ASH 19MAR01      GOTO FILE MANAGER FOR NINORD
.release  init      "2.1"       ASH 28DEC98    NINORD Y2K, file expansion
.RELEASE  INIT      "R2.0"      DLH 12MARH92   ALL NEW INCLUDES ETC.

OUTPUT    FILE
.patch3.1
OUTPUT2   FILE
.patch3.1
.NORDFILEX IFILE     KEYLEN=8,FIXED=408,NODUPLICATES
NORDFILEX IFILE     KEYLEN=8,FIXED=408,Name="NINORDDATE.ISI|nins1:502"
YY1       dim       2
YY2       dim       2
YY3       dim       2
YY4       dim       2
YY5       dim       2
YY6       dim       2
YY7       dim       2
PackData DataList
today1    form      5
USED      form      5
.Patch3.1 added logic
AKEY2    INIT      "02R"
ORDS      FORM      9
check               form      5
check2    form      5
orddate   form      5
mask9      init    "ZZZ,ZZZ,ZZ9"         ;formatting vars
DIM11A                        DIM       11
.patch3.1
          move      "NDAT0043",PROGRAM
.          move      "CARDS not revised last 3mos/ordered previous week.",STITLE
          move      "CARDS not revised last 6mos/ordered previous week.",STITLE
          move      "Names In The News",COMPNME
          move      C1,NDATPATH  .SET ACCESS TO ISAM
          move      C2,NORDPATH
          call      PAINT
          move      "ABORT" TO PF5
          Trap      FIlegone giving error if IO
          call      FUNCDISP
. Create New Index for NINDAT.DAT
          display   *P1:8,"Indexing NINORD to read by Order Date"
.          goto      restart
          clear     taskname
.         pack      taskname,"!f:\apps\plb\code\sunidxnt \\nins1\e\data\text\ninord.dat c:\work\ninorddate l408 -202-209"
.patch3.21
.begin patch 3.22
.         pack      taskname,"!f:\apps\plb\code\sunindex \\nins1\e\data\text\ninord.dat c:\work\ninorddate l408 -202-209"
.          pack      taskname,"\\nins1\e\data\text\ninord.dat c:\work\ninorddate l408 -202-209"
          call      debug
          pack      taskname,"\\nins1\e\data\text\ninord.dat \\nins1\e\data\index\ninorddate.isi -202-209"
.patch3.21
.         INDEX     taskname
          INDEX     taskname,sundm="NINS1:502"
.         execute   taskname
.end patch 3.22
Restart   display   *P1:8,"Prepping Variables                     "
.
          create    PackData=1:1:1:1
          clock     timestamp,timestamp
          unpack    timestamp,CC,YY,MM,DD
          call      CVTJUL
          if (MM > "01" | (MM = "01" & DD > "07"))
                    for N1,"1","7"
                              store     YY,N1,YY1,YY2,YY3,YY4,YY5,YY6,YY7
                    repeat
          else
.Initialize with last years date
                    move      C0,N2
                    move      YY,N2
                    sub       C1,N2
                    move      N2,str2
                    rep       zfill,str2
                    for N1,"1","7"
                              store     str2,N1,YY1,YY2,YY3,YY4,YY5,YY6,YY7
                    repeat
.Apply this years date, where applicable
                    move      C0,N2
                    move      DD,N2
                    for N3,N2,"1",SEQ
                              store     YY,N3,YY7,YY6,YY5,YY4,YY3,YY2,YY1
                    repeat
          endif
.
          move      juldays,TODAY1
.
          trap      ABORT IF F5
.
          Move      "Cardrev.dat",filename
.          erase     "c:\work\CARDrev.dat"
.          prepare   OUTPUT,"c:\work\CARDrev.dat",exclusive
          call      debug
          erase     "\\nins1\e\data\CARDrev.dat"
          prepare   OUTPUT,"\\nins1\e\data\CARDrev.dat|NINS1:502",exclusive
.patch3.1
          Move      "dataup.dat",filename
.          prepare   OUTPUT2,"\\nins1\e\data\dataup.dat",exclusive
          prepare   OUTPUT2,"\\nins1\e\data\dataup.dat|NINS1:502",exclusive
.patch3.1
.
          display   *P1:8,"Reading Files"
          move      C3,ndatlock
          move      C3,nordlock
          move      C1,nordpath
          Move      "NINORDDATE.dat",filename
.          pack      NORDNME1,"c:\work\ninorddate.isi"
.          open      NORDFILEX,NORDNME1,read
          pack      taskname from "ninorddate.isi|nins1:502"
          open      NORDFILEX,taskname
.
          move      C1,N1
          sub       "6",TODAY1,N5
          for N6,N5,TODAY1
                    move      N6,JULDAYS
                    call      CVTGREG
                    load      YY,N1,YY1,YY2,YY3,YY4,YY5,YY6,YY7
                    pack      str8,CC,YY,MM,DD
                    rep       zfill,str8
                    pack      str10,MM,SLASH,DD,SLASH,CC,YY
                    display   *P1:8,"Reading Files for ",str10
                    move      C1,N10
                    read      NORDFILEX,str8;ORDVARS
                    loop
                              until over
                              pack      str9,OODTEC,OODTEY,OODTEM,OODTED
                              until (str8 <> str9)
                              add       C1,N10
                              display   *P1:10,"Reading Order Record ",N10
                              display   *P1:12,"Search ",str8
                              display   *P1:13,"Actual ",str9
                              if (OSTAT <> "p" & OSTAT <> "l" & OSTAT <> "x" & OSTAT <> "z")
                                        PackData.FindStringExact giving result using OLNUM,0
                                        if (result = SEQ)
                                                  pack      NDATFLD,OLNUM
                                                  move      "NDATKEY",Location
                                                  pack      KeyLocation,"Key: ",NDATFLD
                                                  call      NDATKEY
                                                  if not over
                                                            if ((ELSTCDE <> "C" | ELSTCDE <> "P") & STATUS = " ")
                                                                      unpack    REVDATE,CC,YY,MM,DD
                                                                      call      CVTJUL
                                                                      sub       JULDAYS,TODAY1,howmany
.begin patch 3.24
.                                                                      if (howmany >= 90)
                                                                      if (howmany >= 180)
.end patch 3.24
                                                                                insertitem PackData,999999,OLNUM
.patch3.1 added logic
                                                                                clear     ords
                                                                                clear check
                                                                                clear check2
                                                                                clear orddate
                                                           REP       zfill IN LSTNUM
                                                           PACK      NORDFLD2,AKEY2,LSTNUM
                                                           CLEAR     NORDFLD1
                                                           CLEAR     NORDFLD3
                                                           CLEAR     NORDFLD4
.
                                                           CALL      NORDAIM
                                                           GOTO      NOORDS IF OVER
.                        GOTO      CheckOtherOrds  if (Ostat <> "0" and Ostat <> "B")
                                                           CMATCH    "p" TO OSTAT       Pending order ?
                                                           GOTO      CheckOtherOrds IF EQUAL     YES, skip.
                                                           CMATCH    "x" TO OSTAT       Cancelled Pending order ?
                                                           GOTO      CheckOtherOrds IF EQUAL     YES, skip.
                        CMATCH    "l" TO OSTAT       LCR order ?
                                                           GOTO      CheckOtherOrds IF EQUAL     YES, skip.
                                                           CMATCH    "z" TO OSTAT       Cancelled LCR order ?
                                                           GOTO      CheckOtherOrds IF EQUAL     YES, skip.
                                                           MOVE      OODTEM TO MM
                                                           MOVE      OODTED TO DD
                                                           MOVE      OODTEY TO YY
                                                           CALL      CVTJUL
                                                           MOVE      juldays TO ORDDATE
                                                           MOVE      ORDDATE TO CHECK
                                                           move      today1 to check2
                                                           SUB       check FROM CHECK2
                                                           compare   "366" to check2
                                                           IF        LESS
                                                                                                              add c1 to ORDS             .ITs GOOD
                                                           ELSE
                                                                                         GOTO     CheckOtherOrds           .DATE OUT OF RANGE
                                                           ENDIF
CheckOtherOrds
                                                                                loop
                                                                     CALL      NORDKG
                                                                                until over
.                                 GOTO      CheckOtherOrds  if (Ostat <> "0" and Ostat <> "B")
                                                                  CMATCH    "p" TO OSTAT       Pending order ?
                                                                     GOTO      CheckOtherOrds IF EQUAL     YES, skip.
                                                                      CMATCH    "x" TO OSTAT       Cancelled Pending order ?
                                                                      GOTO      CheckOtherOrds IF EQUAL     YES, skip.
                                                                     CMATCH    "l" TO OSTAT       LCR order ?
                                                                  GOTO      CheckOtherOrds IF EQUAL     YES, skip.
                                                               CMATCH    "z" TO OSTAT       Cancelled LCR order ?
                                                                     GOTO      CheckOtherOrds IF EQUAL     YES, skip.
                                                               MOVE      OODTEM TO MM
                                                                     MOVE      OODTED TO DD
                                                                  MOVE      OODTEY TO YY
                                                               CALL      CVTJUL
                                                                     MOVE      juldays TO ORDDATE
                                                                  MOVE      ORDDATE TO CHECK
                                                               move      today1 to check2
                                                                            SUB       check FROM CHECK2
.what this would be a years worth supposed to be previous week
.                                                                     compare   "366" to check2
                                                                     compare   "8" to check2
                                                                  IF        LESS
                                                                                                              add c1 to ORDS             .ITs GOOD
                                                                                          endif
                                                                                repeat
NOORDS
                                                                                clear str9
                                                                                CLEAR     DIM11A
                                                                  move mask9 to dim11A
                                                           edit ORDS to dim11A
.START PATCH 3.2 REPLACED LOGIC
.                                                                               write     output,seq;b1,lstnum,ownnum,nlstcde,elstcde,commper,hotline,newdate,revdate,password,mlstname,universe,dim11a
                                                                                move      COMMPER,str3
                                                                                write     output,seq;b1,lstnum,ownnum,nlstcde,elstcde,str3,hotline,newdate,revdate,password,mlstname,universe,dim11a
.END PATCH 3.2 REPLACED LOGIC
.patch3.1
                                                                                write     output2,seq;datvars
.patch3.1
.;patch3.1 added logic
.                                                                               write     output,seq;b1,lstnum,ownnum,nlstcde,elstcde,commper,hotline,newdate,revdate,password,mlstname,universe
                                                                                add       C1,USED
                                                                                display   *P12:18,"NUMBER OF LISTS WITH USAGE ",USED
                                                                                call debug
                                                                      endif
                                                            endif
                                                  endif
                                        endif
                              endif
                              readks    NORDFILEX;ORDVARS
                    repeat
                    add       C1,N1
          repeat
          shutdown

ABORT
          display   *P1:24,*EL,*B,*B,"JOB ABORTED, RESULTS NOT VALID",*W5
          shutdown

          INCLUDE   NORDIO.inc
          include   nownio.inc
          INCLUDE   NDATIO.inc
          INCLUDE   COMLOGIC.inc
