........................................
. Program:      TESTER.PLS
. Function:     Tester Program for Routines
. Author:       Andrew Harkins
. Date:         July 15,1998
. Release:      1.1
........................................

PC      EQU     1
.Include Files
        include common.inc
        include cons.inc
.Vars used to test RTRIM, LTRIM, TRIM
text1   dim     30
text2   dim     30
text3   dim     30
text4   dim     30
text5   dim     30
text6   dim     30

string1 dim     50
string2 dim     50
string3 dim     50
temp    dim     50
sourcestring dim        590

.Var Used In order To Load Includes
release dim     10

.Getinfo
        getinfo system,sourcestring

x       plform  tester
        winhide
.Load Forms, Always declare parent form first
        formload x
        
.Set String Values
        move    "s",string1
        move    "n",string2
        move    "f",string3
        
.Set Text Boxes
Right   clear   temp
        pack    temp,"*",string1,"*"
        setitem TesterStatText001,0,"Text string1 Before RTRIM:"
        setitem TesterStatText002,0,temp
        clear temp
        call    RTRIM   using string1
        pack    temp,"*",string1,"*"
        setitem TesterStatText003,0,"Text string1 After RTRIM:"
        setitem TesterStatText004,0,temp

Left    clear   temp
        pack    temp,"*",string2,"*"
        setitem TesterStatText005,0,"Text string2 Before LTRIM:"
        setitem TesterStatText006,0,temp
        clear temp
        call    LTRIM   using string2
        pack    temp,"*",string2,"*"
        setitem TesterStatText007,0,"Text string2 After LTRIM:"
        setitem TesterStatText008,0,temp 
               
Both    clear   temp
        reset   string3
        pack    temp,"*",string3,"*"
        setitem TesterStatText009,0,"Text string3 Before TRIM:"
        setitem TesterStatText010,0,temp
        clear temp
        call    TRIM   using string3
        pack    temp,"*",string3,"*"
        setitem TesterStatText011,0,"Text string3 After TRIM:"
        setitem TesterStatText012,0,temp
.............................
        bump    sourcestring,12
        move    sourcestring,str4
        setitem TesterStatText001,0,"screen width:"
        setitem TesterStatText002,0,str4
        bump    sourcestring,4
        move    sourcestring,str4
        setitem TesterStatText003,0,"screen height:"
        setitem TesterStatText004,0,str4                                                             


MainLoop
        loop
                waitevent
        repeat
        
.Include IO Files
        include comlogic.inc                
