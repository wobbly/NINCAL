.
     %IFDEF NOTUSED
                    Display   *p1:22,*el,"sorting Nprint.dat to Nprint.tmp"
                    pack      taskname,"\\NinS1\e\data\NPRINT.dat,\\NinS1\e\data\NPRINT.tmp;198-199,303-306,3-6,7-12"
                    sort      taskname,SUNDM="NINS1:502"
                    If        Over
                              pack      str255 from "Sort to nprint.tmp failed, ",CRLF,"Error: ",S$ERROR$,crlf:
                              "Yes = you fixed, No=try again, Canncel= abort"                 
                              ALERT    PLAIN,str255,RESULT
                              IF       (RESULT = 1)
                                        ALERT    NOTE,"YES was pressed.",RESULT
                              ELSEIF   (RESULT = 2)
                                        ALERT    NOTE,"NO was pressed.",RESULT
                              Goto     SortOwn
                              ELSEIF  (RESULT = 3)
                                        ALERT   NOTE,"CANCEL was pressed.",RESULT
                                        Shutdown "Cls"
                              ENDIF
                    endif
     %ENDIF
.
taskname DIM          300
.
        PACK  taskname, "-l\\nins1\e\data\debug.log -d \\NinS1\e\data\NPRINT.dat,\\NinS1\e\data\NPRINT.tst;198-199,303-306,3-6,7-12"
.
        keyin "hit enter to start sort test...:",s$cmdlin
.
        sort  taskname,SUNDM="NINS1:502"
        if over
         display "OVER......."
        else
         display "NOT OVER..."
        endif
.
        display "s$error...: '",*ll,s$error$,"'"
.
           keyin "hit enter to exit:",s$cmdlin
.