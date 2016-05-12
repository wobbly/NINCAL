PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc


.
RELEASE  INIT      "1.2"       DLH Add Neom0013 report 6 Paid with A/R and report 5 payables with A/R
reldate   init      "2013 August 07"
.RELEASE  INIT      "1.1"       DMB 07JUL03  Added PDF option
.reldate   init      "2003 July 07"
.;RELEASE  INIT      "1.0"       DMB 01MAR02  initial release to make income report form PLB-writes out to flat file
.                                           loinc.wbt reads file and creates report
LOINCFLE FILE
HOLD6    dim        6
HoldRep  dim        1
rep1  dim        1
Again    dim        1
Year1    dim        4
timer1   timer          .timer for dialog box
.
Quesbox        integer    1,"0x000004"    .Question box to add xref,beginning balance
         MOVE      "NLOINC0001" to PROGRAM
         MOVE      "Names in the News" TO COMPNME
.         prepare   LOINCFLE,"C:\work\loincrep.dat"
.         OPEN      LOINCFLE,"C:\work\loincrep"
.====================================================================================
        move    "NLOINC0001.PLS",Wprognme
        move    "LO Income Report",Wfunction
        move    "David Baca",Wauthor
        move    "1.0",Wrelease
        move    "February 27, 2002",Wreldate
        move        release,wrelease
        move        reldate,wreldate

dlgbx   plform  dialogwait
abt     plform  About
x       plform  NLOINC0001


     winhide

  formload x
  formload abt
  formload dlgbx
          clock timestamp to timestamp
          unpack timestamp,str4,str2
.         pack str6,str2,str4
.patch1.x add for new year
          move str4 to n4
          add c1 to n4
          move n4 to str4
.patch1.x
          
        move str4 to year1
        move "1988" to str4
        clear n4
        move str4 to n4
        clear n3
        loop
                  add c1 to n1
          insertitem NLOINC0001ComboBoxBegYR,n3,str4
                  insertitem NLOINC0001ComboBoxEndYR,n3,str4
          until (str4 = YEAR1)
                  add c1 to n4
                move n4 to str4
        repeat
.         setitem NLOINC0001EditText002,0,str6
        move str2 to n2
          setitem  NLOINC0001ComboBoxEndMo,n1,n2
        setitem  NLOINC0001ComboBoxEndYR,n1,c2
        setitem  NLOINC0001ComboBoxBegYR,n1,c2
        setfocus NLOINC0001ComboBoxBegMo
.         setfocus NLOINC0001EditText001
.====================================================================================
        loop
        waitevent
        repeat
.====================================================================================
Start
         call ordersetmousebusy
         prepare   LOINCFLE,"C:\work\loincrep.dat"


.==================================================
.Diskin
        WRITE   LOINCFLE,SEQ;comment
.====================================================
.Report Type
        getprop NLOINC0001Radio001,SELGROUPID=n1     1-Paid 2-Payables 3-Combined, 4-paid with AR, 5 payables with AR
        move    n1 to str1
        move    str1 to rep1
        WRITE   LOINCFLE,SEQ;str1

        if (str1 <> "2" & str1 <> "5")
*****************************************************
        clear n3
        clear n2
          getitem NLOINC0001ComboBoxBegMo,n2,n3
          getitem NLOINC0001ComboBoxBegMo,n3,str2
        if ((str2 = "")|(str2 = "."))
                  alert   caution,"Order Date Cannot be a null value!",result,"Bad Date"
                setfocus NLOINC0001ComboBoxBegMo
                  call ordersetmousefree
                return
        endif
        clear n2
        clear n3
        getitem NLOINC0001ComboBoxBegYR,n2,n3
          getitem NLOINC0001ComboBoxBegYR,n3,str4
        if ((str4 = "")|(str4 = "."))
                  alert   caution,"Order Date Cannot be a null value!",result,"Bad Date"
                    setfocus NLOINC0001ComboBoxBegYr
                call ordersetmousefree
                return
        endif
        pack str6 with str2,str4
****************************************************

.                 getitem NLOINC0001EditText001,0,str6
                call    TRIM using str6
                if      (str6="")
                          alert   caution,"Order Date Cannot be a null value!",result,"Bad Date"
.                           setfocus NLOINC0001EditText001
                        CLOSE   LOINCFLE
                            call ordersetmousefree
                        return
           endif
                CALL    ZFILLIT USING STR6
                if      (str6="000000")
                          alert   caution,"Order Date Cannot be a null value!",result,"Bad Date"
.                           setfocus NLOINC0001EditText001
                        CLOSE   LOINCFLE
                            call ordersetmousefree
                        return
           endif

                count   N2,str6
                if (N2 = 0)
                        alert   caution,"Order Date Must be in MMCCYY Format",result
.                           setfocus NLOINC0001EditText001
                              CLOSE   LOINCFLE
                            call ordersetmousefree
                        Return
                endif
                if (N2 <> 6)
                                  alert   caution,"Order Date Must be in MMCCYY Format",result
.                                     setfocus NLOINC0001EditText001
                                CLOSE   LOINCFLE
                                      call ordersetmousefree
                            Return
                endif

                unpack  str6,str2,cc,yy
                    move    str2,N2
                if (N2 > "12")
                                alert   caution,"Invalid Month!",result
                                              setfocus NLOINC0001ComboBoxBegMo
.                                        setfocus NLOINC0001EditText001
                                        CLOSE   LOINCFLE
                                                call ordersetmousefree
                              Return
                else
                        move    cc,N2
                                        if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                                alert   caution,"Invalid Year!",result
.                                                         setfocus NLOINC0001EditText001
                                                            setfocus NLOINC0001ComboBoxBegYr
                                                CLOSE   LOINCFLE
                                                          call ordersetmousefree
                                                Return
                                        elseif (N2 = "19")
                                                move    YY,N2
                                                if (N2 < "80")
                                                          alert   caution,"Invalid Year!",result
                                                                      setfocus NLOINC0001ComboBoxBegYr
.                                                                   setfocus NLOINC0001EditText001
                                                    CLOSE   LOINCFLE
                                                                    call ordersetmousefree
                                                        Return
                                                endif
                                        endif

                endif

                    move    str2,N2
                if (N2 < c1)
                                alert   caution,"Invalid Month!",result
                                                                  setfocus NLOINC0001ComboBoxBegMo
.                                                        setfocus NLOINC0001EditText001
                                                    CLOSE   LOINCFLE
                                                                    call ordersetmousefree
                                                        Return
                endif

**************************************************************
*****************************************************
        clear n3
        clear n2
          getitem NLOINC0001ComboBoxEndMo,n2,n3
          getitem NLOINC0001ComboBoxEndMo,n3,str2
        if ((str2 = "")|(str2 = "."))
                  alert   caution,"Order Date Cannot be a null value!",result,"Bad Date"
                setfocus NLOINC0001ComboBoxEndMo
                  call ordersetmousefree
                return
        endif
        clear n2
        clear n3
        getitem NLOINC0001ComboBoxEndYR,n2,n3
          getitem NLOINC0001ComboBoxEndYR,n3,str4
        if ((str4 = "")|(str4 = "."))
                  alert   caution,"Order Date Cannot be a null value!",result,"Bad Date"
                    setfocus NLOINC0001ComboBoxEndYr
                  call ordersetmousefree
                return
        endif
        pack str6 with str2,str4
****************************************************
.                 getitem NLOINC0001EditText002,0,str6
                call    TRIM using str6
                if      (str6="")
                          alert   caution,"Order Date Cannot be a null value!",result,"Bad Date"
.                           setfocus NLOINC0001EditText002
                        CLOSE   LOINCFLE
                            call ordersetmousefree
                        return
           endif
                CALL    ZFILLIT USING STR6
                if      (str6="000000")
                          alert   caution,"Order Date Cannot be a null value!",result,"Bad Date"
.                           setfocus NLOINC0001EditText002
                        CLOSE   LOINCFLE
                            call ordersetmousefree
                        return
           endif

                count   N2,str6
                if (N2 = 0)
                        alert   caution,"Order Date Must be in MMCCYY Format",result
.                           setfocus NLOINC0001EditText002
                              CLOSE   LOINCFLE
                            call ordersetmousefree
                        Return
                endif
                if (N2 <> 6)
                                  alert   caution,"Order Date Must be in MMCCYY Format",result
.                                     setfocus NLOINC0001EditText002
                                CLOSE   LOINCFLE
                                      call ordersetmousefree
                            Return
                endif

                unpack  str6,str2,cc,yy
                    move    str2,N2
                if (N2 > "12")
                                alert   caution,"Invalid Month!",result
                                              setfocus NLOINC0001ComboBoxEndMo
.                                        setfocus NLOINC0001EditText002
                                        CLOSE   LOINCFLE
                                                call ordersetmousefree
                              Return
                else
                        move    cc,N2
                                        if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                                alert   caution,"Invalid Year!",result
                                                            setfocus NLOINC0001ComboBoxEndYr
.                                                         setfocus NLOINC0001EditText002
                                                CLOSE   LOINCFLE
                                                          call ordersetmousefree
                                                Return
                                        elseif (N2 = "19")
                                                move    YY,N2
                                                if (N2 < "80")
                                                          alert   caution,"Invalid Year!",result
                                                                      setfocus NLOINC0001ComboBoxEndYr
.                                                                   setfocus NLOINC0001EditText002
                                                    CLOSE   LOINCFLE
                                                                    call ordersetmousefree
                                                        Return
                                                endif
                                        endif

                endif

                    move    str2,N2
                if (N2 < c1)
                                alert   caution,"Invalid Month!",result
                                                                  setfocus NLOINC0001ComboBoxEndMo
.                                                        setfocus NLOINC0001EditText002
                                                    CLOSE   LOINCFLE
                                                          call ordersetmousefree
                                                        Return
                endif
***************************************************************
        endif


*****************************************************
.==================================================
.==================================================
.Report Date
        getprop NLOINC0001Radio004,SELGROUPID=n1     1-Inv Date 2-Check Date 3-All Dates
        move    n1 to str1
        rep     "1I2C3A",str1
        WRITE   LOINCFLE,SEQ;str1
*********************************************************


*********************************************************

.==================================================
.Beginning

.         getitem NLOINC0001EditText001,0,str6
.        unpack  str6,str2,str4
.VAR BMO in winbatch
        clear n2
        clear n3
          getitem NLOINC0001ComboBoxBegMo,n2,n3
          getitem NLOINC0001ComboBoxBegMo,n3,str2
        WRITE   LOINCFLE,SEQ;str2
.VAR BYR in winbatch
        clear n2
        clear n3
        getitem NLOINC0001ComboBoxBegYR,n2,n3
          getitem NLOINC0001ComboBoxBegYR,n3,str4
        WRITE   LOINCFLE,SEQ;str4
.==================================================
.Ending
.         getitem NLOINC0001EditText002,0,str6
.        unpack  str6,str2,str4
.VAR EMO in winbatch
        clear n3
        clear n2
          getitem NLOINC0001ComboBoxEndMo,n2,n3
          getitem NLOINC0001ComboBoxEndMo,n3,str2
        WRITE   LOINCFLE,SEQ;str2
.VAR EYR in winbatch
        clear n2
        clear n3
        getitem NLOINC0001ComboBoxEndYR,n2,n3
          getitem NLOINC0001ComboBoxEndYR,n3,str4
        WRITE   LOINCFLE,SEQ;str4
.==================================================
.Preview
        getitem NLOINC0001CheckPreview,n2,n1
        move n1 to str1
        WRITE   LOINCFLE,SEQ;str1
.==================================================
.All Open Payables
        getitem NLOINC0001CheckALLPAY,n2,n1
        move n1 to str1
        WRITE   LOINCFLE,SEQ;str1
.==================================================
.Copies
        getitem NLOINC0001EditCopies,0,str3
        WRITE   LOINCFLE,SEQ;str3
.==================================================
.Diskin
.        WRITE   LOINCFLE,SEQ;comment
.====================================================
.Ask whether they wish to run another report after this one
Another
        alert   type=quesbox,"After this report has compeleted processing would you like to run another report?",result,"Another Report"
        if (result = c7)         .NO
                move    c0 to str1
                WRITE   LOINCFLE,SEQ;str1
                setprop NLOINC0001,visible=c0
;======================================================================         
;patch1.1
;Again 
        move        c0 to str1
        WRITE       LOINCFLE,SEQ;str1
********************************************************************
;PDF Option
        getitem     NLOINC0001Check001,n2,n1
        move        n1 to str1
        WRITE       LOINCFLE,SEQ;str1
;patch1.1
********************************************************************
                  CLOSE   LOINCFLE
.Creates Timers for DialogWait Message Box
                  create timer1,10
                activate timer1,tome1,result
.                 create timer2,60
.               activate timer2,tome2,result
                  setitem DIALOGWAITMESSAGE,0,"I'll let you know when I'm Finished..!!"
                  setprop DialogWait,visible=1
********************************************************************
                clear   taskname
                pack    taskname,"\\nins1\winbatch\loinc.exe ","b=",user,b1,"",func
.;                pack    taskname,"f:\apps\winbatch\loinc.exe ","b=",user,b1,"prin=",prin=func
                execute taskname
                call ordersetmousefree
                  alert   note,"Report is Done!!",result,"Report Complete"
        else
                move    c1 to str1
                WRITE   LOINCFLE,SEQ;str1
                if      (again = yes)
                          WRITE   LOINCFLE,SEQ;holdrep
          else
;patch1.1
                    move c0 to holdrep
                    write loincfle,seq;holdrep
;======================================================================         
;PDF Option
                    getitem   NLOINC0001Check001,n2,n1
                    move      n1 to str1
                    WRITE     LOINCFLE,SEQ;str1
;patch1.1
                  endif

                  CLOSE   LOINCFLE
                setprop NLOINC0001,visible=c0
********************************************************************
.Creates Timers for DialogWait Message Box
                  create timer1,10
                activate timer1,tome1,result
.                 create timer2,60
.               activate timer2,tome2,result
                  setitem DIALOGWAITMESSAGE,0,"I'll let you know when I'm Finished..!!"
                  setprop DialogWait,visible=1
********************************************************************
                clear   taskname
.                pack    taskname,"\\nts0\c\apps\winbatch\loinc.exe ","b=",user,b1,"prin=",func
                pack    taskname,"\\nins1\winbatch\loinc.exe ","b=",user,b1,"pa=",func
.;                pack    taskname,"f:\apps\winbatch\loinc.exe ","b=",user,b1,"prin=",func
          execute taskname
                move    YES to again
                move    rep1 to holdrep
                  call ordersetmousefree
                  alert   note,"Ready to do another!",result,"Report Complete"
                setprop NLOINC0001,visible=c1
                return
        endif
.==================================================
EOJ
        winshow
        stop
.===========================================================
.subroutines for message box
TOME1
          setprop DialogWait,visible=0
          destroy timer1
          return

OrderSetMouseBusy
        setmode *mcursor=*wait
        return
OrderSetMouseFree
        setmode *mcursor=*arrow
        return


         INCLUDE   COMLOGIC.inc

