.::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.                               HPLJII
.                        Control codes for the
.                   HP Laserjet Series II printer.
.
.     Software is custom written and Fonts for the HP Laserjet II
.         have been custom tailored for American Cyanamid by
.
.               C.H.E.S. System Services      01-15-88
.
.::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.
.      Control characters for choosing fonts of different point sizes
.
HP06PT   INIT      033,"(6X"           06 point
HP10PT   INIT      033,"(0X"           10 point
HP11PT   INIT      033,"(1X"           11 point
HP12PT   INIT      033,"(2X"           12 point bold
HP14PT   INIT      033,"(4X"           14 point bold
.
HPCOUR   INIT      033,"(s3T"          Courier typeface
HPLINE   INIT      033,"(s0T"          Line Printer typeface
HPHELV   INIT      033,"(s4T"          Helvetica typeface
HPTMSRMN INIT      033,"(s5T"          Times Roman typeface
HPPRES   INIT      033,"(s8T"          Prestige Elite typeface
HPGOTHIC INIT      033,"(s6T"          Gothic typeface
.
.      Control characters for horizntl tab position from the left margin
.
HPT000   INIT      033,"*p000X"        0.00 inches from left margin
HPT025   INIT      033,"*p100X"        0.25 inches from left margin
HPT150   INIT      033,"*p450X"        1.50 inches from left margin
HPT175   INIT      033,"*p500X"        1.75 inches from left margin
HPT200   INIT      033,"*p600X"        2.00 inches from left margin
HPT225   INIT      033,"*p700X"        2.25 inches from left margin
HPT250   INIT      033,"*p750X"        2.50 inches from left margin
HPT300   INIT      033,"*p900X"        3.00 inches from left margin
HPT325   INIT      033,"*p950X"        3.25 inches from left margin
HPT350   INIT      033,"*p1050X"       3.50 inches from left margin
HPT375   INIT      033,"*p1100X"       3.75 inches from leftmargin
HPT400   INIT      033,"*p1150X"       4.00 inches from left margin
HPT450   INIT      033,"*p1300X"       4.50 inches from left margin
HPT475   INIT      033,"*p1425X"       4.75 inches from left margin
HPT525   INIT      033,"*p1600X"       5.25 inches from left margin
HPT550   INIT      033,"*p1700X"       5.20 inches from left margin
HPT575   INIT      033,"*p1725X"       5.75 inches from left margin
HPT650   INIT      033,"*p2000X"       6.50 inches from left margin
.
.      Control characters for vertical tab position
.
HPVU01   INIT      033,"&a-1R"         1 Line  up from current position
HPVU02   INIT      033,"&a-2R"         2 Lines up from current position
.
.
.      Control characters for drawing a line
.      LN1 = Across page, LN2 = Right half of page
.
HPLN1    INIT      033,"*c2400a10b0p",033,"*p+60Y"
HPLN2    INIT      033,"*p-22Y",033,"*p600X",033,"*c1800a02b0p"
.
. e.w. lake beginning to wreck havoc...
.
HPRESET  INIT      033,"E"
.
HPBON    INIT      033,"(s3B"               Bold on
HPBOFF   INIT      033,"(s0B"               Bold off
.
HPUNON   INIT      033,"&dD"                Underline on
HPUNOFF  INIT      033,"&d@"                Underline off
.
HPLIN6   INIT      033,"&l6D"               6 lines per inch
HPLIN8   INIT      033,"&l8D"               8
.
.                       left        right       top         bottom
HPLETTER INIT      033,"&a13L",033,"&a72M",033,"&l13E",033,"&l60F"
HPPORT   INIT      033,033,"H,P,FR1;"
HPLAND   INIT      033,033,"H,L,FR5;"
.
.
.                             symbol set
.                            /          spacing
.                           /          /    pitch
.                          /          /    /    point
.                         /          /    /    /   style
.                        /          /    /    /   /   stroke weight
.                        |          |   |     |   |  /  typeface
.                        |          |   |     |   |  | /
.
HPGOTH12 INIT      033,"(8U",033,"(s0p12.00h12.00v0s-2b6T"
HPGOTH10 INIT      033,"(8U",033,"(s0p14.29h10.00v0s-2b6T"
.
HPDTCH12 INIT      033,"(8U",033,"(s1p12.00v0s-2b4T"
HPDTCH10 INIT      033,"(8U",033,"(s1p10.69v0s-2b4T"
HPDTCH85 INIT      033,"(8U",033,"(s1p08.50v0s-2b4T"
.
PDTCH12  INIT      033,"(s5t",033,"s1P",033,"(s12V",033,"(s0S",033,"(s5T"
PDTCH10  INIT      033,"(s5t",033,"s1P",033,"(s10V",033,"(s0S",033,"(s5T"
PDTCH85  INIT      033,"(s5t",033,"s1P",033,"(s08V",033,"(s0S",033,"(s5T"
.
PGOTH12  INIT      033,"(s5t",033,"s1P",033,"(s12V",033,"(s0S",033,"(s6T"
PGOTH10  INIT      033,"(s5t",033,"s1P",033,"(s10V",033,"(s0S",033,"(s6T"
PGOTH85  INIT      033,"(s5t",033,"s1P",033,"(s08V",033,"(s0S",033,"(s6T"
.
HPSHADE  INIT      033,"*p325x1400Y":  position cursor
                   033,"*c1900A":      width
                   033,"*c600B":       height
                   033,"*c10G":        area fill%
                   033,"*c2P"          print it
.
HPTTRAY  INIT      033,"&l1H"          top tray
HPBTRAY  INIT      033,"&l4H"          bottom tray
