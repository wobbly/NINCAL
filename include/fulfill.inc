;    Last change:  JD   16 Nov 2001    7:26 am
..04/23/02 DB add new fulfil28 Enertex Mktg
..04/03/02 DB updated PIDI fax #.
..11/19/01 DB updated PC DIR fax #.
..11/16/01 JD updated PIDI fax #.
..11/09/01 JD updated ful13/ful20 as well as their contact names.
..09/05/01 DB Changed Kable\Pub Aid Name, #,contact name
..07/30/01 DB Changed fulfill juliane/msgi to msgi/metro dir
..07/17/01 DB added new fulfil27 juliane/msgi dir
..03/13/01 DB Changed fulfil25 ATC From Anna h to Anna m
..01/05/00 ADDED NEW FULFIL 25 ATC
ful1     init      "FIDE"
ful2     init      "JETSON"
ful3     init      "ANACAPA"
ful4     init      "ANTARES"
ful5     init      "SMALL PUB"      SMALL PUBLISHERS
ful6     init      "DTM"            .note prob with 3 byte name pad to 4.
fuL7     init      "PIDI"
ful8     init      "EPSILON"
ful9     init      "LIST MAINTENANC"
FUL10    INIT      "KABLE"
.FUL10    INIT      "KABLE/PUB. AID"
FUL11    INIT      "THE FIDE GROUP"
FUL12    INIT      "NATL SUBSC.FUL."
.FUL13    INIT      "METRO SVC/LAURE"
FUL13    INIT      "METRO SVC"
FUL14    INIT      "NATL CONVSN SYS"
FUL15    INIT      "DYNAMARK"
FUL16    INIT      "IMI/INFO.MGT."
FUL17    INIT      "PACIF.COAST DIR"
FUL18    INIT      "INTERACTIVE SYS"
FUL19    INIT      "DRCT ACCESS MKTG"
FUl20    init      "METRO DIRECT"
FUl21    init      "L & E MERIDIAN"
FUl22    init      "ARG/RICHIE G."
FUl23    init      "FFA/CATHERINE"
FUl24    init      "BLAEMIRE COMM."
FUl25    init      "ANNA M./ATC"
FUl26    init      "TDMC"               .Currently used only by Program 1
FUL27    INIT      "MSGI/METRO DIR"
FUL28    INIT      "Enertex Mktg"
.
ovrTEL1  init      "7737742975"
ovrTEL2  init      "2033536661"
ovrTEL3  init      "6124816363"
.
fulTEL1  init      "8056858561"
fulTEL2  init      "5169791960"
fulTEL3  init      "8055660305"
fulTEL4  init      "6312345472"
fulTEL5  init      "3154379863"      SMALL PUBLISHERS
fulTEL6  init      "6305955361"
.fuLTEL7  init      "7036835458"
fuLTEL7  init      "7036839537"
fulTEL8  init      "6172731359"     EPSILON 
fulTEL9  init      "2035526799"     LIST MAINTENANCE"
FULTEL10 INIT      "8157345202"     KABLE
.FULTEL10 INIT      "8157341129"     KABLE/PUB.AID"
fulTEL11 init      "8056858561"     THE FIDE GROUP
fulTEL12 init      "7146939704"     natl susbcription fulfillment.
FULTEL13 INIT      "9173397164"     METRO SVC/LAUREN .per CO 21Oct99 DLH
FULTEL14 INIT      "7038470381"     NATIONAL CONVERSION SYSTEMS   12/12/95 DLH
FULTEL15 INIT      "2127857503"     DYNAMARK                      11/21/96 JD
FULTEL16 INIT      "4084280715"     IMI INFO MGMT
FULTEL17 INIT      "5624329716"     PACIF.COAST DIR
.FULTEL17 INIT      "5624338935"     PACIF.COAST DIR
FULTEL18 INIT      "7032475445"     INTERACTIVE SRVCS              12/02/98
FULTEL19 INIT      "5163640644"     Drct Access                     08/12/99
FULTEL20 INIT      "9173397163"     Metro Drct                          12/22/99 per SA
FULTEL21 INIT      "7039137050"     L & E Meridian                          06/05/00 per SMM
FUltel22 init      "5165644197"     ARG/Richie
FUltel23 init      "3015852595"     FFA
FUltel24 init      "7036205339"     BLAEMIRE
FUltel25 init      "3045352667"     ATC
FUltel26 init      "4153827088"     TDMC        .Currently used only by Program 1
FULTEL27 INIT      "9173397164"     MSGI/METRO DIRECT
FULTEL28 INIT      "5106288313"     ENERTEX MKTG
.
ovroct1  form      2                number of orders
ovroct2  form      2                number of orders
ovroct3  form      2                number of orders
.
fuloct1  form      2                number of orders
fuloct2  form      2                number of orders
fuloct3  form      2                number of orders
fuloct4  form      2                number of orders
fuloct5  form      2                number of orders
fuloct6  form      2                number of orders
fuloct7  form      2                number of orders
fuloct8  form      2                number of orders
fuloct9  form      2                number of orders
fuloct10 form      2                number of orders
fuloct11 form      2                number of orders
fuloct12 form      2                number of orders
fuloct13 form      2                number of orders
fuloct14 form      2
fuloct15 form      2
fuloct16 form      2
fuloct17 form      2
fuloct18 form      2
fuloct19 form      2
fuloct20 form      2
fuloct21 form      2
fuloct22 form      2
fuloct23 form      2
fuloct24 form      2
fuloct25 form      2
fuloct26 form      2
fuloct27 FORM      2
fuloct28 FORM      2
.
ovrnum   form      2                table index
fulnum   form      2                table index
fulnum1  form      2                table index
.
Fulcnt1  init      " "               contact at Fide.
Fulcnt2  init      " "               contact at Jetson.
Fulcnt3  init      " "               contact at Anacapa.
Fulcnt4  init      "Sonya May"      contact at Antares.
Fulcnt5  init      " "               contact at Small pub.
Fulcnt6  init      "Mark"           contact at DTM.
Fulcnt7  init      " "               contact at PIDI.
Fulcnt8  init      " "               contact at Epsilon.
Fulcnt9  init      " "               contact at List Maintenance.
Fulcnt10 init      "Jan"          contact at KABLE Progressive
.Fulcnt10 init      "Marci"          contact at KABLE/Pub Aid.   Progressive
Fulcnt10a init     "Jan"            contact at KABLE/Pub Aid.     E Mag
Fulcnt11 init      " "               contact at FIDE.
Fulcnt12 init      " "               contact at NATL SUBSC.FUL.
.Fulcnt13 init      "Lauren Pulido"  contact at METRO SVC/LAURE.
Fulcnt13 init      "Juliane Langone"  contact at METRO SVC/Citizen Soldier.
Fulcnt14 init      " "               contact at NATL CONVSN SYS.
Fulcnt15 init      " "               contact at Dynamark.
Fulcnt16 init      " "               contact at IMI/INFO.MGT.
Fulcnt17 init      " "               contact at PACIF.COAST DIR.
Fulcnt18 init      " "               contact at INTERACTIVE SYS.
Fulcnt19 init      "Charmaine"       contact at DRCT ACCESS MKTG.
Fulcnt20 init      "Denise Ficara"   contact at METRO DIRECT/Am Friends Srv
Fulcnt21 init      "Adam Glantz"      contact at L & E MERIDIAN
Fulcnt22 init      "Richie/Rod"      contact at ARG/RICHIE G.
Fulcnt23 init      "Catherine"       contact at FFA/CATHERINE
Fulcnt24 init      " "               contact at BLAEMIRE COMM.
Fulcnt25 init      "Anna M."         contact at ANNA M.\ATC.
Fulcnt26 init      " "               contact at TDMC
Fulcnt27 INIT      " "               contact at MSGI/METRO DIRECT Earthwatch
Fulcnt28 INIT      " "               contact at ENERTEX Mktg
..........................................................................................
