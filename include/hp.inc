;    Last change: DLH 6/11/2002 10:24:56 AM
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;                                       HPLJII
;                              Control codes for the
;                       HP Laserjet Series II printer.
;
;     Software is custom written and Fonts for the HP Laserjet II
;           have been custom tailored for American Cyanamid by
;
;                   C.H.E.S. System Services      01-15-88
;
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;
;      Control characters for choosing fonts of different point sizes
;
HP04PT     INIT        033,"(s4V"                 04 point
HP06PT     INIT        033,"(s6V"                 06 point
HP08PT     INIT        033,"(s8V"                 08 point
HP10PT     INIT        033,"(s10V"                 10 point
HP12PNT  INIT      033,"(s12V"           12 point                 jd 05/09/00 test
HP14PNT  INIT      033,"(s14V"           12 point                 jd 05/09/00 test
;????? below looks like font by id number
;HP06PT     INIT        033,"(6X"                 06 point
;HP10PT     INIT        033,"(0X"                 10 point
HP11PT     INIT        033,"(1X"               11 point
HP12PT     INIT        033,"(2X"               12 point bold
HP14PT     INIT        033,"(4X"               14 point bold
;
HPCOUR     INIT        033,"(s3T"              Courier typeface
HPLINE     INIT        033,"(s0T"              Line Printer typeface
HPPICA     init        033,"(s1T"              Pica
HPCGTIME INIT          033,"(s4101T"       CG TIMES Internal Scalable font.
HPUNIVER INIT          033,"(s4148T"       UNIVERS Internal Scalable font.
HPELITE    init        033,"(s2T"
HPHELV     INIT        033,"(s4T"              Helvetica typeface
HPTMSRMN INIT          033,"(s5T"              Times Roman typeface
HPTMRMN1 INIT          033,"(s22T"                Times Roman typeface landscape
HPPRES     INIT        033,"(s8T"              Prestige Elite typeface
HPGOTHIC INIT          033,"(s6T"              Gothic typeface
;
;...STYLE
HPUPRGHT INIT          033,"(s0S"             UPRIGHT
HPITALIC INIT          033,"(s1S"             ITALICS
;
;
;     Primary Spacing
;
hpfixed    init        033,"(s0P"              fixed spacing
hpprop     init        033 "(s1P"              Proportional spacing
;
;     PITCH
HP85PTCH INIT          033,"(s8.5H"           8.5 CPI
HP10PTCH INIT          033,"(s10H"            10 CPI
HP12PTCH INIT          033,"(s12H"            12 CPI
HP14PTCH INIT          033,"(s14H"            14 CPI
HP15PTCH INIT          033,"(s15H"            15 CPI
HP16PTCH INIT          033,"(s16H"            16 CPI
HP17PTCH INIT          033,"(s17H"            17 CPI
HP20PTCH INIT          033,"(s20H"            20 CPI
HP24PTCH INIT          033,"(s26H"            24 CPI
HPPTCH     INIT        033,"&k2S"             COMPRESSED.
; 
; 
;      Control characters for horizntl tab position from the left margin
;      positioned by dots, est. 300dpi.
;
HPT000     INIT        033,"*p000X"            0.00 inches from left margin
HPT0125    INIT        033,"*p050X"            0.125 inches from left margin
HPT025     INIT        033,"*p100X"            0.25 inches from left margin
HPT050     init        033,"*p200X"            0.50 inches from left margin
HPT075     init        033,"*p250X"            0.75 inches from left margin
HPT100     INIT        033,"*p300X"            1.00 inches from left margin
HPT125     INIT        033,"*p375X"            1.25 inches from left margin
HPT150     INIT        033,"*p450X"            1.50 inches from left margin
HPT175     INIT        033,"*p500X"            1.75 inches from left margin
HPT200     INIT        033,"*p600X"            2.00 inches from left margin
HPT225     INIT        033,"*p700X"            2.25 inches from left margin
HPT250     INIT        033,"*p750X"            2.50 inches from left margin
HPT275     INIT        033,"*p800X"            2.75 inches from left margin
HPT285     INIT        033,"*p825X"            2.85 inches from left margin
HPT300     INIT        033,"*p900X"            3.00 inches from left margin
HPT325     INIT        033,"*p950X"            3.25 inches from left margin
HPT350     INIT        033,"*p1050X"       3.50 inches from left margin
HPT375     INIT        033,"*p1100X"       3.75 inches from leftmargin
HPT400     INIT        033,"*p1200X"       4.00 inches from left margin
HPT425     INIT        033,"*p1275X"       5.25 inches from left margin
HPT450     INIT        033,"*p1300X"       4.50 inches from left margin
HPT475     INIT        033,"*p1425X"       4.75 inches from left margin
HPT500     INIT        033,"*p1500X"       5.00 inches from left margin
HPT510     INIT        033,"*p1525X"       5.10 inches from left margin
HPT515     INIT        033,"*p1545X"       5.15 inches from left margin
HPT525     INIT        033,"*p1600X"       5.25 inches from left margin
HPT540     INIT        033,"*p1670X"       5.40 inches from left margin
HPT550     INIT        033,"*p1700X"       5.50 inches from left margin
HPT575     INIT        033,"*p1725X"       5.75 inches from left margin
HPT600     INIT        033,"*p1800X"       6.00 inches from left margin
HPT6125    INIT        033,"*p1850X"       6.125 inches from left margin
HPT625     INIT        033,"*p1900X"       6.25 inches from left margin
HPT635     INIT        033,"*p1930X"       6.25 inches from left margin
HPT650     INIT        033,"*p2000X"       6.50 inches from left margin
HPT675     INIT        033,"*p2025X"       6.75 inches from left margin
HPT685     INIT        033,"*p2055X"       6.75 inches from left margin
HPT700     INIT        033,"*p2100X"       7.00 inches from left margin
HPT710     INIT        033,"*p2125X"       7.00 inches from left margin
HPT715     INIT        033,"*p2145X"       7.00 inches from left margin
HPT725     INIT        033,"*p2200X"       7.25 inches from left margin
HPT740     INIT        033,"*p2270X"       7.25 inches from left margin
HPT750     INIT        033,"*p2300X"       7.50 inches from left margin
HPT775     INIT        033,"*p2375X"       7.75 inches from left margin
HPT800     INIT        033,"*p2400X"       8.00 inches from left margin
HPT810     INIT        033,"*p2430X"       8.00 inches from left margin
HPT820     INIT        033,"*p2455X"       8.00 inches from left margin
HPT825     INIT        033,"*p2475X"       8.25 inches from left margin
HPT850     INIT        033,"*p2550X"       8.50 inches from left margin
HPT875     INIT        033,"*p2625X"       8.75 inches from left margin
HPT900     INIT        033,"*p2700X"       9.00 inches from left margin
HPT950     INIT        033,"*p2850X"       9.50 inches from left margin
HPT975     INIT        033,"*p2925X"       9.75 inches from left margin
HPT1000    INIT        033,"*p3000X"      10.00 inches from left margin
hpt1050    init        033,"*p3250X"
;
;      Control characters for vertical tab position
;
HPVU01     INIT        033,"&a-1R"             1 Line  up from current position
HPVU02     INIT        033,"&a-2R"             2 Lines up from current position
;
;
;      Control characters for drawing a line
;      LN1 = Across page, LN2 = Right half of page
;      ln3 = across landscape
;      ln4 = protrait orientation centered
;      ln5 = Landscape orientation centered
;
HPLN1      INIT        033,"*c2400a10b0p",033,"*p+60Y"
HPLN2      INIT        033,"*p-22Y",033,"*p600X",033,"*c1800a02b0p"
HPLN3      INIT        033,"*c2400a10b0p",033,"*p+120Y"
HPLN4      INIT        033,"*p-22Y",033,"*p300X",033,"*c1800a02b0p"
HPLN5      INIT        033,"*p-22Y",033,"*p650X",033,"*c1800a02b0p"
;
; e.w. lake beginning to wreck havoc...
;
HPRESET    INIT        033,"E"
HPdcard    INIT        "&f2y3X"         call macro#2 in laser for datacard.
hpupdinf INIT          "&f200y3X"         call macro#200 in laser for datacard.
HPORDER    INIT        "&f3y3X"         call macro#3 in laser for ORDER.
HPOMCPY    INIT        "&f300y3X"       call macro#300 for ORDER MLR COPY BOX.
HPOoCPY    INIT        "&f301y3X"       call macro#301 for ORDER own COPY BOX.
HPOfCPY    INIT        "&f302y3X"       call macro#302 for ORDER fulfilment COPY BOX.
HPOOfCPY INIT          "&f303y3X"       call macro#303 for ORDER office COPY BOX.
HPinv      INIT        "&f4y3X"         call macro#4 in laser for invoice.
HPSTMNT    INIT        "&f7y3X"         call macro#7 in laser for Statement.
HPpayfrm INIT          "&f8y3X"         call macro#8 in laser for payables Statement.
HPownst1 INIT          "&f800y3X"       call macro#800 in laser for section one of owner activety statemnt.
;.16Feb99 DLH  no program should be using these anymore
;HPltrhd   INIT        "&f9y3X"         call macro#9 in laser for letterhead.
;HPltrhd1 INIT         "&f10y3X"         call macro#10 in laser for landscape letterhead.
;.end 16Feb99
;
HPBON      INIT        033,"(s7B"                     Bold on    NIN NY uses 3B
HPBOFF     INIT        033,"(s0B"                     Bold off
;
HPUNON     INIT        033,"&dD"                      Underline on
HPUNOFF    INIT        033,"&d@"                      Underline off
;
HPLIN6     INIT        033,"&l6D"                     6 lines per inch
HPLIN8     INIT        033,"&l8D"                     8
;
;                             left          right top           bottom
HPLETTER INIT          033,"&a13L",033,"&a72M",033,"&l13E",033,"&l60F"
;hpletter sets  left and right margins in columns . note fixed or proportional font effects the column size
HPTOP      INIT        033,"&l0E"             set top margin 0 lines
HPBOT      INIT        033,"&l64F"            set page length 64 lines
HPPORT     INIT        033,"&l0O"
HPPORT1    INIT        033046154000117
HPLAND     INIT        033,"&l1O"
HPDUPL     INIT        033,"&l1S"            DUPLEX ON LONG EDGE.
HPDUPS     INIT        033,"&l2S"            DUPLEX ON SHORT EDGE.
HPDUPOFF INIT          033,"&l0S"            DUPLEX OFF.
;
;
;                                   symbol set
;                                  /              spacing
;                                 /            /    pitch
;                                /            /      /      point
;                               /            /      /    /   style
;                              /            /      /    /     /   stroke weight
;                              |            |     |     |     |  /    typeface
;                              |            |     |     |     |  | /
HPGOTH12 INIT          033,"(8U",033,"(s0p12.00h12.00v0s-2b6T"
HPGOTH10 INIT          033,"(8U",033,"(s0p14.29h10.00v0s-2b6T"
;
;HPDTCH20 INIT          033,"(8U",033,"(s1p14.00h20.00v0s-2b5T"
HPDTCH20 INIT          033,"(8U",033,"(s1p20.00h20.00v0s-2b5T"
HPDTCH16 INIT          033,"(8U",033,"(s1p16.00v0s-2b5T"
HPDTCH14 INIT          033,"(8U",033,"(s1p14.00v0s-2b5T"
HPDTCH12 INIT          033,"(8U",033,"(s1p12.00v0s-2b5T"
HPDTCH10 INIT          033,"(8U",033,"(s1p10.69h10.00v0s-2b5T"
HPDTCH85 INIT          033,"(8U",033,"(s1p08.50v0s-2b5T"
HPDTCH70 INIT          033,"(8U",033,"(s1p07.00v0s-2b5T"
HPDTCH06 INIT          033,"(8U",033,"(s1p06.00v0s-2b5T"
;
DTCH10FX INIT          033,"(8U",033,"(s0p10.69h10.00v0s-2b5T"
DTCH85FX INIT          033,"(8U",033,"(s0p08.50v0s-2b5T"    -fixed
;
HPSWSS20 INIT          033,"(8U",033,"(s1p14.00h20.00v0s-2b4T"
HPSWSS17 INIT          033,"(8U",033,"(s1p16.67h18v0s-2b4T"
HPSWSS16 INIT          033,"(8U",033,"(s1p16.00v0s-2b4T"
HPSWSS14 INIT          033,"(8U",033,"(s1p14.00v0s-2b4T"
HPSWSS12 INIT          033,"(8U",033,"(s1p12.00v0s-2b4T"
HPSWSS10 INIT          033,"(8U",033,"(s1p10.69h10.00v0s-2b4T"
HPSWSS85 INIT          033,"(8U",033,"(s1p08.50v0s-2b4T"
HPSWSS06 INIT          033,"(8U",033,"(s1p06.00v0s-2b4T"
;
HPUNIV14 INIT          033,"(8U",033,"(s1p14.00v0s-2b4148T"
HPUNIV12 INIT          033,"(8U",033,"(s1p12.00v0s-2b4148T"
HPUNIV10 INIT          033,"(8U",033,"(s1p10.69v0s-2b4148T"
HPUNIV85 INIT          033,"(8U",033,"(s1p08.50v0s-2b4148T"
HPUNIV06 INIT          033,"(8U",033,"(s1p06.00v0s-2b4148T"
;
;                             roman 8 symbol set
;                             /         primary font
;                            /         /   Point Size
;                           /         /   /     style (upright)
;                          /         /   /     /  stroke weight(demi light)
;                         /         /   /     /  / Typeface (times new roman)
;                        /         /   /     /  / /
HPtmsr24 INIT          033,"(8U",033,"(s1p24.00v0s-2b16901T"
HPtmsr17 INIT          033,"(8U",033,"(s0p16.67h9v0s-0b0T"    .fixed
HPtmsr14 INIT          033,"(8U",033,"(s1p14.69v0s-2b3T"
HPtmsr12 INIT          033,"(8U",033,"(s1p12.69v0s-2b3T"
HPtmsr10 INIT          033,"(8U",033,"(s1p10.69v0s-2b3T"
HPtmsr08 INIT          033,"(8U",033,"(s1p8.69v0s-2b3T"
HPtmsr06 INIT          033,"(8U",033,"(s1p6.00v0s-2b3T"
;
HPUSASCI INIT          033,"(0U"
HPLNDRAW INIT          033,"(0B"
HPSHADE    INIT        033,"*p325x1400Y":  position cursor
                       033,"*c1900A":      width
                       033,"*c600B":       height
                       033,"*c10G":            area fill%
                       033,"*c2P"              print it
;
HPBOX      INIT        033,"*p325x1400Y":  position cursor
                       033,"*c1900A":      width
                       033,"*c600B":       height
                       033,"*c3G":            area fill%
                       033,"*c3P"              print it
;
HPTTRAY    INIT        033,"&l1H"              top tray
HPBTRAY    INIT        033,"&l4H"              bottom tray
Bullet   init          0362     .presumes pc8 symbol set in use
;
copyrite init          033,"(1U",0136,033,"(8U"
;                             \     \             \
;                               \        copyright \
;                                \       dec 94      reset symbol set
;                                 legal symbol set
copyright init         033,"(9U",0251,033,"(8U"
;                             \     \             \
;                               \        copyright \
;                                \       dec 169     reset symbol set
;                                  Windows set
;panasonic codes.
;
;
;                                  
;                                  spacing
;                                 /                  
;                                /            point
;                               /            /           style
;                              /            /           /        
;                              |            |           |                 typeface
;                              |            |           |                /
PDTCH12    INIT        033,"s1P",033,"(s12V",033,"(s0S",033,"(s5T"
PDTCH10    INIT        033,"s1P",033,"(s10V",033,"(s0S",033,"(s5T"
PDTCH85    INIT        033,"s1P",033,"(s08V",033,"(s0S",033,"(s5T"
;
PGOTH12    INIT        033,"s1P",033,"(s12V",033,"(s0S",033,"(s6T"
PGOTH10    INIT        033,"s1P",033,"(s10V",033,"(s0S",033,"(s6T"
PGOTH85    INIT        033,"s1P",033,"(s08V",033,"(s0S",033,"(s6T"
;
PSWISS12  INIT          033,"s1P",033,"(s12V",033,"(s0S",033,"(s6T"
PSWISS10  INIT          033,"s1P",033,"(s10V",033,"(s0S",033,"(s6T"
PSWISS85  INIT          033,"s1P",033,"(s08V",033,"(s0S",033,"(s6T"
;
Pcour12     INIT        033,"s0P",033,"(s12V",033,"(s0S",033,"(s3T"
Pcour10     INIT        033,"s0P",033,"(s10V",033,"(s0S",033,"(s3T"
Pcour85     INIT        033,"s0P",033,"(s08V",033,"(s0S",033,"(s3T"
;
Pcent12     INIT        033,"s1P",033,"(s12V",033,"(s0S",033,"(s23T"
Pcent10     INIT        033,"s1P",033,"(s10V",033,"(s0S",033,"(s23T"
Pcent85     INIT        033,"s1P",033,"(s08V",033,"(s0S",033,"(s23T"
;cpi
pptch10     init        033,"(s10H"
pptch12     init        033,"(s12H"
pptch15     init        033,"(s15H"
pptch16     init        033,"(s16.66H"
;
plftmar    init        033,"&a3L"             left margin 3 columns
pdotcol    init        033,"*p50X"            
;
;      Control characters for choosing fonts of different point sizes
;
P72PT     INIT        033,"(7.2V"                 7.2 point
P08PT     INIT        033,"(8V"               08 point
P10PT     INIT        033,"(10V"               10 point
P11PT     INIT        033,"(11V"               11 point
P12PT     INIT        033,"(12V"               12 point 
P14PT     INIT        033,"(14V"               14 point 
P24ital init          033,"4"
P24uprt    init        033,"5"
P24bon     init        033,"E"
P24boff    init        033,"F"
p24pic10 init          033,"P"             pica 10
p2417cpi init          033,017
pPPORT     INIT        033,033,"H,P,FR1;"          panasonic super set controls
pPLAND     INIT        033,033,"H,L,FR5;"          panasonic super set controls
;
;.............
;fax (facsys)
;                             orientation  left marg         pitch          lpi      height
;                         /                 /               /              /          /
diskland init        033,"&l1O",033,"&a1L",033,"(s14.00H",033,"&l8D",033,"&a1V"
faxland    init      033,"&l1O",033,"&a15L",033,"(s16.66H",033,"&l8D",033,"&a1V"
faxport    init      033,"&l0O",033,"&a15L",033,"&a1V"
;diskport init        033,"&l0O",033,"&a01L",033,"&a80M",033,"10U",033,"(s0p8.46v16.67h0s3b23T"
diskport init        033,"&l0O",033,"&a01L",033,"10U",033,"(s0p8.46v16.67h0s3b23T"
;                          \      \            \      \                  \
;                   orientation  left marg  right margin symbol set     font spacing etc.

