PC      EQU     1
.Include Files
        include common.inc
        include cons.inc
        include oslspern.inc
        include nmlrdd.inc
.01/19/99
.
.code for Spin Keys.  Spin Key 'Object' 
.originated from Vertical Scroll Bars.  Basis for that code found in
.f:\library\develop\aharkin\scroll
 
ASH
.Files to open
credit   ifile   keylen=7
.Should be replaced with a global from cons.inc
howmany  form    9
result   form    9
sales   dim     15
badstat init    "B*P"
release init    "1.0"

.Colors
white   color
grey    color
RED     COLOR
BLACK   COLOR

.Declare forms, Always declare child forms first
x       plform  spin
        winhide

.Load Forms, Always load parent form first
        formload x

.Create Colors for EditText Inquiry
        create  white=*white
        create  grey=*ltgray
        create  RED=*RED
        create  black=*black
        setfocus Mailer2EditZip

        loop
                waitevent
        repeat

SpinKeyUp
        getitem Mailer2VScrollSales,0,N2
        sub     C1,N2
        call    Spin3
        return
SpinKeyDown
        getitem Mailer2VScrollSales,0,N2
        add     C1,N2
        call    Spin3
        return
                              
.Include IO file
        include comlogic.inc

