         INCLUDE   COMMON.iNC
S1       INIT      "WUMPUS  "
S2       DIM       120
S3       DIM       2
S4       DIM       8
N2       FORM      "00"
N3       FORM      "00"
N4       FORM      "000"
N5       FORM      "00"
S5       DIM       2
N6       FORM      "00000"
S6       DIM       2
S7       DIM       2
S8       DIM       2
N7       FORM      "0000"
N8       FORM      "05"
N9       FORM      "0"
S9       DIM       6
N10      FORM      "000000"
S10      DIM       2
S11      DIM       2
S12      DIM       2
S13      DIM       2
S14      DIM       2
S15      DIM       2
S16      DIM       2
S17      DIM       2
S18      INIT      "0"
S19      INIT      "0"
S20      INIT      "0"
S21      INIT      "0"
N11      FORM      "00000"
N12      FORM      "00"
N13      FORM      "00"
N14      FORM      "00"
N15      FORM      "00"
N16      FORM      "00"
N17      FORM      "00"
N18      FORM      "000"
N19      FORM      "00000"
N20      FORM      "    .00000"
N21      FORM      "    .00"
S22      DIM       40
Z77      EQU       0176
+
         KEYIN     *bgcolor=3,*red:
                   *ES,"If this message is not in both UPPER and":
                   " lower case letters",*P1:2,"the WUMPUS game ":
                   "will not work",*P1:5,"Are there lower case l":
                   "etters on your screen? ",*uc,S22
         CMATCH    "Y",S22
         GOTO      X1 IF EOS
         GOTO      X1 IF NOT ZERO
         KEYIN     *ES,*P1:24,"HUNT THE WUMPUS.",*N,*R,"NEED INS":
                   "TRUCTIONS? ",*uc,S22
         CMATCH    "N",S22
         CALL      X2 IF NOT ZERO
         CLEAR     S2
         APPEND    "020508010310020412030514010406",S2
         APPEND    "050715060817010709081018020911",S2
         APPEND    "101219031113121420041315061416",S2
         APPEND    "151720071618091719111820131619",S2
         RESET     S2,1
         CALL      X3
         MOVE      S5,S10
         CALL      X3
         MOVE      S5,S11
         CALL      X3
         MOVE      S5,S12
         CALL      X3
         MOVE      S5,S13
         CALL      X3
         MOVE      S5,S14
         CALL      X3
         MOVE      S5,S15
         CALL      X3
         MOVE      S5,S16
         MOVE      S10,S17
         CLOCK     TIME,S4
         MOVE      S4,S3
         MOVE      S3,N12
         RESET     S4,4
         MOVE      S4,S3
         MOVE      S3,N14
         RESET     S4,7
         MOVE      S4,S3
         MOVE      S3,N16
         DISPLAY   *P1:24,*R,*R,"ENTRANCE IS IN CAVE ",S17;
         TABPAGE
X12      DISPLAY   *P1:24,*R,*R;
         ADD       "1",N18
         MATCH     S10,S17
         GOTO      X4 IF NOT ZERO
         DISPLAY   *P1:24,*R,"I SEE A LIGHT......EXIT NEARBY";
X4       CALL      X5
         COMPARE   "64",N3
         CALL      X6 IF NOT LESS
         CALL      X5
         COMPARE   "8",N3
         CALL      X7 IF LESS
         MOVE      S10,N6
         MULT      "6",N6
         SUB       "5",N6
         RESET     S2,N6
         MOVE      "0",S19
         MOVE      "0",S20
         CALL      X8
         MOVE      S2,S6
         BUMP      S2,2
         CALL      X8
         MOVE      S2,S7
         BUMP      S2,2
         CALL      X8
         MOVE      S2,S8
         CALL      X5
         CALL      X9
         DISPLAY   *P1:24,*R,"TUNNELS LEAD TO CAVES ",S8,", ",S6:
                   ", ",S7;
         CALL      X5
         COMPARE   "38",N3
         GOTO      X10 IF NOT ZERO
         DISPLAY   *P1:24,*R,"FEARLESS FREDDIE THE SUPERBAT DOES":
                   " NOT HANG OUT WITH THE OTHER SUPERBATS......":
                   *N,*R,"BUT FLIES AROUND BY HIMSELF LOOKING FO":
                   "R WUMPUS-HUNTERS TO GRAB....";
         CALL      X11
         GOTO      X12
X10      CALL      X5
         COMPARE   "77",N3
         GOTO      X13 IF NOT ZERO
         COMPARE   "0",N9
         GOTO      X13 IF NOT ZERO
         MOVE      "1",N9
         DISPLAY   *P1:24,*R,"IN 1910 ALEXANDER AGASSIZ, THE AME":
                   "RICAN ZOOLOGIST, ENTERED THIS BURROW  TO HUN":
                   "T",*N,*R,"WUMPI AND DISCOVERED THE BEAUTIFUL":
                   " MUSHROOMS THEY EAT. BEING A SCIENTIST, HE":
                   *N,*R,"DECIDED TO TASTE ONE. HE HAS BEEN EATI":
                   "NG THEM EVERSINCE AND HAS GROWN TOO BIG",*N:
                   *R,"TO CRAWL THROUGH THE ENTRANCE HOLE.",*W,*N:
                   *R,"HE HAS JUST GIVEN YOU ONE OF HIS ARROWS!!";
         ADD       "1",N8
         GOTO      X12
X13      KEYIN     *P1:24,*R,"WHAT NOW? ",*uc,S22;
         CMATCH    "S",S22
         GOTO      X14 IF ZERO
         CMATCH    "A",S22
         GOTO      X15 IF ZERO
         CMATCH    "M",S22
         GOTO      X16 IF ZERO
         CMATCH    "H",S22
         GOTO      X17 IF ZERO
         CMATCH    "E",S22
         GOTO      X18 IF ZERO
         CMATCH    "C",S22
         GOTO      X19 IF ZERO
         CMATCH    "W",S22
         GOTO      X12 IF ZERO
         DISPLAY   *P1:24,*R,"YOU CAN'T DO THAT!  IF YOU NEED HE":
                   "LP, ASK FOR IT";
         GOTO      X13
X14      CMATCH    "1",S21
         GOTO      X20 IF NOT ZERO
         DISPLAY   *P1:24,*R,"THE CURRENT LIMIT ON WUMPI IS 1 AN":
                   "D YOU ALREADY HAVE ONE.",*N,*R," IF I WERE Y":
                   "OU I'D GET OUT OF THE AS FAST AS I COULD!";
         GOTO      X21
X20      COMPARE   "0",N8
         GOTO      X22 IF NOT ZERO
         DISPLAY   *P1:24,*R,"YOUR QUIVVER IS EMPTY!!!",*W,*W;
         GOTO      X12
X22      KEYIN     *P1:24,*R,"WHICH CAVE? ",N2;
         MOVE      N2,S3
         CMATCH    " ",S3
         GOTO      X23 IF NOT ZERO
         CMOVE     "0",S3
X23      ADD       "1",N11
         SUB       "1",N8
         MATCH     S3,S6
         GOTO      X24 IF ZERO
         MATCH     S3,S7
         GOTO      X24 IF ZERO
         MATCH     S3,S8
         GOTO      X24 IF ZERO
         DISPLAY   *P1:24,*R,"'I SHOT AN ARROW INTO THE.........":
                   "'",*W,*W,*N,*R,"THAT CAVE DOESN'T EVEN CONNE":
                   "CT WITH CAVE ",S10,"YOU JUST WASTED AN ARROW":
                   "!!!";
         GOTO      X15
X24      MATCH     S3,S11
         GOTO      X25 IF ZERO
         DISPLAY   *P1:24,*R,"YOU MISSED THE WUMPUS";
         GOTO      X26
X25      CALL      X5
         COMPARE   "8",N3
         GOTO      X27 IF LESS
         DISPLAY   *P1:24,*R,"* * * * * * * * * * * * * * * * * ":
                   "* * * * * * * * * * * * * * * * * * * * * *":
                   *N,*R,"                        C O N G R A T ":
                   "U L A T I O N S",*N,*R,"* * * * * * * * * * ":
                   "* * * * * * * * * * * * * * * * * * * * * * ":
                   "* * * * * * *",*N,*R,*N,*R,"            YOU ":
                   "GOT A WUMPUS!!!";
         ADD       "1",N7
         MOVE      "1",S21
         MOVE      "00",S11
         CALL      X5
         COMPARE   "48",N3
         GOTO      X21 IF LESS
         DISPLAY   *P1:24,*R,"BEWARE OF ITS MATE !!";
X28      CALL      X5
         MATCH     S5,S16
         GOTO      X28 IF ZERO
         MOVE      S5,S11
         GOTO      X15
X27      DISPLAY   *P1:24,*R,"YOU ONLY WOUNDED THE WUMPUS....AND":
                   " NOW HE'S MAD";
X26      CALL      X6
X15      DISPLAY   *P1:24,*R,"YOU NOW HAVE ",N8," ARROWS";
X21      GOTO      X12
X16      KEYIN     *P1:24,*R,"WHICH CAVE WOULD YOU LIKE TO ENTER":
                   "? ",N2;
         MOVE      N2,S10
         CMATCH    " ",S10
         GOTO      X29 IF NOT ZERO
         CMOVE     "0",S10
X29      MATCH     S10,S6
         GOTO      X30 IF ZERO
         MATCH     S10,S7
         GOTO      X30 IF ZERO
         MATCH     S10,S8
         GOTO      X30 IF ZERO
         DISPLAY   *P1:24,*R,"THERE IS NO TUNNEL TO CAVE ",S10," A":
                   "ND YOU HAVE NO SHOVEL";
         GOTO      X16
X30      MATCH     S10,S14
         CALL      X11 IF ZERO
         MATCH     S10,S15
         CALL      X11 IF ZERO
         MATCH     S10,S11
         CALL      X31 IF ZERO
         MATCH     S10,S16
         GOTO      X32 IF ZERO
         MATCH     S10,S12
         GOTO      X33 IF ZERO
         MATCH     S10,S13
         GOTO      X33 IF ZERO
         GOTO      X12
X19      DISPLAY   *P1:24,*R,"YOU ARE NOW IN CAVE ",S10,*W,*W;
         CALL      X34
         GOTO      X12
X32      DISPLAY   *P1:24,*R,"A LANDSLIDE HAS BLOCKED THAT CAVE";
         CALL      X35
         GOTO      X16
X17      CALL      X36
         GOTO      X12
X18      MATCH     S10,S17
         GOTO      X37 IF ZERO
         DISPLAY   *P1:24,*R,"YOU'D NEED A SHOVEL TO GET OUT FRO":
                   "M CAVE ",S10,", AND YOU DON'T HAVE ONE";
         GOTO      X13
X37      DISPLAY   *P1:24,*R,"OUT OF THE CAVES!!";
         SUB       "1",N18
         COMPARE   "0",N7
         GOTO      X38 IF ZERO
         DISPLAY   *P1:24,*R,"GOOD HUNTING!!";
X38      MOVE      "0",N6
         MULT      "100",N7
         MOVE      "0",N20
         COMPARE   "0",N11
         GOTO      X39 IF ZERO
         MOVE      N7,N20
         GOTO      X39 IF ZERO
         DIV       N18,N20
         DIV       N11,N20
X39      MOVE      N20,N21
         DISPLAY   *P1:24,*R,"YOUR RATING IS ",N21,*W;
         COMPARE   "0",N21
         GOTO      X40 IF NOT ZERO
X77      DISPLAY   *P1:24,*R,"BETTER LUCK NEXT TIME",*W;
X40      COMPARE   "1",N9
         GOTO      X1 IF NOT ZERO
         DISPLAY   *P1:24,*R,*R,"REMEMBER GOOD OLD ALEXANDER AGA":
                   "SSIZ WHO GAVE YOU AN ARROW?",*W,*N,*R,"THE W":
                   "ORLD HAS PRESUMED HIM TO BE DEAD SINCE NO ON":
                   "E HAS HEARD FROM OR SEEN HIM",*N,*R,"SINCE H":
                   "E ENTERED THIS BURROW IN 1910. REALIZING THA":
                   "T YOU MAY ENCOUNTER",*N,*R,"OPPOSITION (FROM":
                   " HIS HEIRS), PLEASE DO HIM A FAVOR --",*W,*N:
                   *R,"TELL THE WORLD ALEXANDER AGASSIZ LIVES!!!";
X1       CHAIN     "MASTER"
X2       DISPLAY   *N,*R,"WELCOME TO HUNT THE WUMPUS !!",*N,*R,*R:
                   "THE WUMPUS COLONY INHABITS A BURROW OF 20 CA":
                   "VES. EACH CAVE HAS THREE",*N,*R,"TUNNELS LEA":
                   "DING TO OTHER CAVES"
         CALL      X41
         CALL      X34
         DISPLAY   *ES,*P1:24,*R,"THE OBJECTIVE IS TO DESCEND IN":
                   "TO THE BURROW AND HUNT WUMPI. AFTER YOU HAVE":
                   *N,*R,"KILLED AT LEAST ONE, YOU MUST RETURN T":
                   "O THE EXIT AND LEAVE THE BURROW.",*N,*R,"NOT":
                   "E THAT THERE MAY BE MORE THAN ONE WUMPUS IN ":
                   "THE BURROW.",*N,*R,*R,"THE WUMPI, NORMALLY O":
                   "F PLACID DISPOSITION, HAVE A KEEN SENSE OF S":
                   "MELL AND SHOW",*N,*R,"A GREAT DEVOTION TO ON":
                   "E ANOTHER. SHOULD THERE BE MORE THAN ONE WUM":
                   "PUS IN THE",*N,*R,"BURROW, THE SURVIVOR WILL":
                   " PROBABLY COME HUNTING YOU WITH MAYHEM IN MI":
                   "ND.",*N,*R,*R,"YOU HAVE FIVE ARROWS. UNDER C":
                   "ONDITIONS OF STRESS, YOU MAY DROP YOUR ARROW":
                   "S.",*N,*R,"YOUR RATING IS A FUNCTION OF THE ":
                   "NUMBER OF WUMPI SHOT IN THE TIME SPENT IN TH":
                   "E",*N,*R,"BURROW.";
         CALL      X41
         DISPLAY   *ES,*P1:24,*R,*R,"* * * *   H A Z A R D S    ":
                   "* * * *",*N,*R,*R,"1--BOTTOMLESS PITS:",*N,*R:
                   "   TWO CAVES CONTAIN BOTTOMLESS PITS. IF YOU":
                   " FALL IN A PIT, GOODBYE!",*N,*R,"2--SUPERBAT":
                   "S:",*N,*R,"   TWO CAVES CONTAIN NESTS OF SUP":
                   "ERBATS. THEY ARE PEACEFUL CREATURES WHEN",*N:
                   *R,"   LEFT ALONE. SHOULD YOU STUMBLE ACROSS ":
                   "THEIR NEST, YOU WILL BE PICKED UP",*N,*R,"  ":
                   " AND DEPOSITED IN ANOTHER CAVE AT  RANDOM, W":
                   "HICH MAY HAVE ANOTHER HAZARD.",*N,*R,"3--EAR":
                   "THQUAKES:",*N,*R,"   THESE ARE A COMMON OCCU":
                   "RRENCE. SHOULD ONE HAPPEN, THE GEOGRAPHY OF ":
                   "THE",*N,*R,"   CAVES WILL BE CHANGED. THE PI":
                   "TS MAY FILL UP AND A CAVE BLOCKED BY ROCKS":
                   *N,*R,"   THE BATS WILL BE DISTURBED AND MAY ":
                   "SEEK OUT A NEW CAVE. THE WUMPI WILL",*N,*R,"  ":
                   " ALSO BECOME ALARMED AND MAY MOVE TO A NEW C":
                   "AVE",*W9,*N,*R,"4--WUMPUS:",*N,*R,"   THE WUMPUS":
                   " IS NOT BOTHERED BY ANY HAZARDS (IT HAS SUCK":
                   "ERS ON ITS FEET AND",*N,*R,"   IS TOO BIG FO":
                   "R A BAT TO LIFT). THE WUMPUS IS MOVING ABOUT":
                   " IN THE CAVES",*N,*R,"   LOOKING FOR FOOD. I":
                   "T NORMALLY EATS A SPECIES OF MUSHROOMS, BUT ":
                   "MAY EAT YOU",*N,*R,"   SHOULD YOU ENTER THE ":
                   "CAVE WHERE IT IS FEEDING. IT NORMALLY MOVE S":
                   "LOWLY",*N,*R,"   FROM CAVE TO CAVE, BUT WHEN":
                   " FRIGHTENED, MAY TELEPORT TO ANOTHER CAVE";
         CALL      X41
         DISPLAY   *ES
X36      DISPLAY   *P1:24,*R,*R,"EACH TURN YOU MAY:",*N,*R,"MOVE":
                   "--TO AN ADJACENT CAVE THROUGH THE CONNECTING":
                   " TUNNEL",*N,*R,"SHOOT--AN ARROW THROUGH A TU":
                   "NNEL TO TRY TO HIT THE WUMPUS",*N,*R,"ARROW-":
                   "-COUNT YOUR REMAINING ARROWS",*N,*R,"CAVE--D":
                   "ISPLAY THE MAP OF THE BURROW",*N,*R,"WAIT--F":
                   "OR THE WUMPUS TO MOVE CLOSER",*N,*R,"EXIT--F":
                   "ROM THE BURROW IF YOU ARE AT THE ENTRANCE",*N:
                   *R,"HELP--GET THESE INSTRUCTIONS";
X41      KEYIN     *P1:24,*R,*R,"PRESS #"ENTER#" TO CONTINUE",S22;
         RETURN
X34      DISPLAY   *ES,"THIS IS HOW A WUMPUS",*N,"BURROW LOOKS. ":
                   "THE",*N,"NUMBERS ARE CAVES,",*N,"THE #"*#" M":
                   "ARK",*N,"THE TUNNELS",*P36:9,"1****",*L,"*":
                   *L,"2",*P42:12,"*",*P42:13,"*",*P41:14,"*",*P40:15:
                   "3",*P39:15,"*",*P38:15,"*",*P37:15,"*",*P36:15:
                   "*",*P35:15,"*",*P34:15,"*",*P33:15,"*",*P32:15:
                   "4",*P31:14,"*",*P30:13,"*",*P30:12,"*",*P30:11:
                   "5",*P31:10,"*",*P32:9,"****",*P29:11,"*",*P28:11:
                   "*",*P27:11,"*",*P26:11,"*",*P25:11,"6",*P25:10:
                   "*",*P26:9,"*",*P27:8,"*",*P28:7,"7",*P29:6,"*":
                   *P30:5,"***** ",*P36:8,"*",*P36:7,"*",*P36:6:
                   "*",*P36:5,"8 *****",*P43:6,"*",*P44:7,"9",*L:
                   "*",*L,"*",*L,"*",*P44:11,"***10",*P47:12,"*":
                   *P47:13,"*",*P47:14,"*",*P47:15,"11",*P46:16:
                   "*",*P45:17,"*",*P41:16,"*",*L,"*",*L,"12",*P42:19:
                   "*",*P41:20,"*",*P40:20,"*",*P39:20,"*",*P38:20:
                   "*",*P35:20,"13",*P33:20,"*",*P32:20,"*",*P31:20:
                   "*",*P30:19,"*",*P31:16,"*",*P30:17,"*",*P28:18:
                   "14",*P27:17,"*",*P26:16,"*",*P24:15,"15",*P25:14:
                   "*",*P25:13,"*",*P25:12,"*",*P23:15,"*",*P22:16:
                   "*",*P21:16,"*",*P20:16,"*",*P19:16,"*",*P18:17:
                   "*",*P16:17,"16",*P16:16,"*",*P16:15,"*",*P16:14:
                   "*",*P16:13,"*",*P16:12,"*",*P16:11,"*",*P16:10:
                   "*",*P16:9,"*",*P16:8,"*",*P17:7,"*",*P18:6,"*":
                   *P19:5,"*",*P20:4,"*",*P21:3,"*",*P27:6,"*",*P26:5:
                   "*",*P25:4,"*",*P24:3,"*",*P22:2,"17 ********":
                   "***************",*P45:6,"*",*P46:5,"*",*P47:4:
                   "*",*P48:3,"*",*P49:2,"18",*L,"*",*L,"*",*L,"*":
                   *L,"*",*L,"*",*P56:8,"*",*P56:9,"*",*P56:10,"*":
                   *P56:11,"*",*P56:12,"*",*P56:13,"*",*P56:14,"*":
                   *P56:15,"*",*P56:16,"*",*P54:17,"*",*P50:16,"**":
                   "**",*P49:15,"*",*P55:17,"*19",*P55:18,"*",*P54:19:
                   "*",*P53:20,"*",*P52:21,"*",*P51:22,"*",*P50:23:
                   "*",*P49:24,"*",*P48:24,"*",*P47:24,"*",*P46:24:
                   "*",*P45:24,"*",*P44:24,"*",*P43:24,"*",*P42:24:
                   "*",*P41:24,"*",*P40:24,"*",*P39:24,"*",*P38:24:
                   "*",*P36:24,"20 ",*P34:24,"*",*P33:24,"*",*P32:24:
                   "*",*P31:24,"*",*P30:24,"*",*P29:24,"*",*P28:24:
                   "*",*P27:24,"*",*P26:24,"*",*P25:24,"*",*P24:24:
                   "*",*P23:24,"*",*P36:21,"*",*P36:23,"*",*P36:22:
                   "*",*P22:23,"*",*P21:22,"*",*P20:21,"*",*P19:20:
                   "*",*P18:19,"*",*P17:18,"*";
         KEYIN     *P1:23,"PRESS #"ENTER#"",*P1:24,"TO CONTINUE":
                   S22;
         RETURN
X7       DISPLAY   *P1:24,*R,"* * * E A R T H Q U A K E",Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   " E A R T H Q U A K E",Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,"E A ":
                   "R T H Q U A K E",Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77," E A R T H Q U A":
                   " K E",Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,"E A R T H Q U A K E":
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,"     E A R T H Q U A K E",Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,"E A R T ":
                   "H Q U A K E",Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,"   E A R T H Q U A K":
                   " E",Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,"E A ":
                   "R T H Q U A K E",Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,"  E A R T H Q U ":
                   "A K E",Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,"E A R T H Q ":
                   "U A K E",Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77," E A R T H Q U A K E",Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,"E A R T H Q U A K E",Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,"  ":
                   " E A R T H Q U A K E",Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,"E A R T H Q U A K E",Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77:
                   Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77,Z77," E A":
                   " R T H Q U A K E";
         CALL      X42
         MOVE      S5,S12
         CALL      X42
         MOVE      S5,S13
         CALL      X42
         MOVE      S5,S14
         CALL      X42
         MOVE      S5,S15
         CALL      X42
         MOVE      S5,S16
         COMPARE   "12",N3
         GOTO      X43 IF LESS
         COMPARE   "73",N3
         GOTO      X44 IF LESS
         CMATCH    "1",S21
         GOTO      X44 IF ZERO
X45      CALL      X5
         MATCH     S5,S16
         GOTO      X45 IF ZERO
         MOVE      S5,S11
X44      RETURN
X43      CALL      X5
         MATCH     S5,S12
         GOTO      X45 IF ZERO
         MATCH     S5,S13
         GOTO      X45 IF ZERO
         MATCH     S5,S14
         GOTO      X45 IF ZERO
         MATCH     S5,S15
         GOTO      X45 IF ZERO
         MATCH     S5,S16
         GOTO      X45 IF ZERO
         MOVE      S5,S17
         RETURN
X42      CALL      X5
         MATCH     S5,S10
         GOTO      X42 IF ZERO
         MATCH     S5,S11
         GOTO      X42 IF ZERO
         MATCH     S5,S17
         GOTO      X42 IF ZERO
         RETURN
X8       MATCH     S2,S11
         GOTO      X46 IF NOT ZERO
         DISPLAY   *P1:24,*R,"I SMELL A WUMPUS";
X46      MATCH     S2,S12
         GOTO      X47 IF NOT ZERO
         CMATCH    "1",S19
         GOTO      X48 IF ZERO
         MOVE      "1",S19
         DISPLAY   *P1:24,*R,"I FEEL A DRAFT";
         GOTO      X47
X48      DISPLAY   *P1:24,*R,"......A BIG DRAFT";
X47      MATCH     S2,S13
         GOTO      X49 IF NOT ZERO
         CMATCH    "1",S19
         GOTO      X50 IF ZERO
         DISPLAY   *P1:24,*R,"I FEEL A DRAFT";
         MOVE      "1",S19
         GOTO      X49
X50      DISPLAY   *P1:24,*R,"---A LARGE, COLD DRAFT";
X49      MATCH     S2,S14
         GOTO      X51 IF NOT ZERO
         CMATCH    "1",S20
         GOTO      X52 IF ZERO
         MOVE      "1",S20
         DISPLAY   *P1:24,*R,"BATS NEARBY";
         GOTO      X49
X52      DISPLAY   *P1:24,*R,"....HORRIBLE LOTS OF BATS; PAPA BA":
                   "TS, MAMA BATS, LITTLE SQUEAKING BABY BATS";
X51      MATCH     S2,S15
         GOTO      X53 IF NOT ZERO
         CMATCH    "1",S18
         GOTO      X54 IF NOT ZERO
         DISPLAY   *P1:24,*R,"..  LOTS AND LOTS OF BATS.";
         GOTO      X53
X54      DISPLAY   *P1:24,*R,"BATS NEARBY";
X53      RETURN
X5       CLOCK     TIME,S4
         RESET     S4,7
         MOVE      S4,S3
         MOVE      S3,N2
         ADD       N2,N3
         MOVE      N3,N5
         DIV       "5",N5
         ADD       "1",N5
         MOVE      N5,S5
         CMATCH    " ",S5
         RETURN    IF NOT ZERO
         CMOVE     "0",S5
         RETURN
X9       BRANCH    N3 OF X55,X56,X57,X58,X59,X60,X61,X62,X63,X64:
                   X65,X57,X66,X67,X56,X56,X68,X69,X61,X70,X64,X65:
                   X57,X66,X67,X56,X56,X68,X69,X61,X55,X56,X57,X58:
                   X59,X60,X61,X62,X63,X70,X64,X65,X57,X66,X67,X56:
                   X56,X68,X69,X61,X55,X56,X57,X58,X59,X60,X61,X62:
                   X63,X70,X64,X65,X57,X66,X67,X56,X56,X68,X69,X61:
                   X55,X56,X57,X58,X59,X60,X61,X62,X63,X70,X64,X65:
                   X57,X66,X67,X56,X56,X68,X69,X61,X55,X56,X57,X58:
                   X59,X60,X61,X62,X63,X70
X61      DISPLAY   *P1:24,*R,"AHA!!!....WUMPUS TRACKS";
         RETURN
X55      DISPLAY   *P1:24,*R,"O",*L,"O",*L,"O",*L,"PPPPPPPSSSSSS":
                   "......SLIPPED ON LOOSE GRAVEL";
         GOTO      X35
X63      DISPLAY   *P1:24,*R,"OOPS!!..YOU FELL IN AN UNDERGROUND":
                   " POOL";
         GOTO      X35
X64      DISPLAY   *P1:24,*R,"THIS LOOKS LIKE A NICE CAVE; WHY N":
                   "OT STOP FOR LUNCH?";
         RETURN
X57      DISPLAY   *P1:24,*R,"TAKE CARE WITH THAT FLASHLIGHT!";
         RETURN
X56      RETURN
X65      DISPLAY   *P1:24,*R,"AHA!...FOUND AN OLD ARROW, LUCKY Y":
                   "OU!";
         ADD       "1",N8
         RETURN
X58      DISPLAY   *P1:24,*R,"TIME FOR MUNCHIES--BREAK OUT THE C":
                   "ANDY BARS";
         DISPLAY   *P1:24,*R,*W,"YUM",*W,"   YUM",*W,*W,"       ":
                   "                 SLURP",*W;
         RETURN
X60      DISPLAY   *P1:24,*R,"YUCK!!!!....STEPPED IN SOME SQUASH":
                   "Y MUSHROOMS";
         GOTO      X35
X68      DISPLAY   *P1:24,*R,"ARRRRGGGGGHHHHHH!!!!! STEPPED ON A":
                   "N OLD ARROW. PICK IT UP AND USE IT ANYHOW";
         ADD       "1",N8
         RETURN
X66      DISPLAY   *P1:24,*R,"SPLASH!!  SPLASH!!........OUCHHH!!":
                   "..BITTEN BY FISH IN UNDERGROUND POOL";
         GOTO      X35
X59      DISPLAY   *P1:24,*R,"&%$###"!'&%$###"%$ RAN INTO &%$#"##":
                   "%$ ROCK";
         GOTO      X35
X67      DISPLAY   *P1:24,*R,"NICE POOL AHEAD--LET'S TAKE A SKIN":
                   "NY DIP!!";
         RETURN
X70      DISPLAY   *P1:24,*R,"     O",*N,*R,"    O O",*N,*R,"   ":
                   " O U",*N,*R,"   O   C",*N,*R,"OOO     H!!!! ":
                   " STEPPED ON STALAGMITE!!!";
         GOTO      X35
X62      DISPLAY   *P1:24,*R,"HHHMMMMMMM.....BATS USED TO LIVE H":
                   "ERE";
         RETURN
X69      DISPLAY   *P1:24,*R,"EUREKA!!  A WUMPUS WHISKER!";
         RETURN
X35      CALL      X5
         DIV       "2",N5
         COMPARE   N8,N5
         RETURN    IF NOT LESS
         COMPARE   "0",N8
         RETURN    IF ZERO
         DISPLAY   *P1:24,*R,"AARRRGGGGHHHHH!!!!!    DROPPED ARR":
                   "OWS!!";
         COMPARE   "0",N5
         GOTO      X71 IF NOT ZERO
         COMPARE   "1",N8
         GOTO      X72 IF NOT ZERO
         DISPLAY   *P1:24,*R,"WHEW!!.............FOUND IT!!!";
         RETURN
X72      DISPLAY   *P1:24,*R,"WHEW!!.............FOUND THEM ALL!":
                   "!!";
         RETURN
X71      SUB       N5,N8
         CLEAR     S22
         APPEND    "LOST ",S22
         APPEND    N5,S22
         APPEND    " ARROW",S22
         COMPARE   "1",N5
         GOTO      X73 IF ZERO
         APPEND    "S",S22
X73      APPEND    ", ",S22
         APPEND    N8,S22
         APPEND    " ARROW",S22
         COMPARE   "1",N8
         GOTO      X74 IF ZERO
         APPEND    "S",S22
X74      APPEND    " LEFT.",S22
         RESET     S22,1
         DISPLAY   *P1:24,*R,S22;
         RETURN
X3       CALL      X5
         MATCH     S5,S10
         GOTO      X3 IF ZERO
         MATCH     S5,S11
         GOTO      X3 IF ZERO
         MATCH     S5,S12
         GOTO      X3 IF ZERO
         MATCH     S5,S13
         GOTO      X3 IF ZERO
         MATCH     S5,S14
         GOTO      X3 IF ZERO
         MATCH     S5,S15
         GOTO      X3 IF ZERO
         MATCH     S5,S16
         GOTO      X3 IF ZERO
         RETURN
X6       CMATCH    "1",S21
         RETURN    IF ZERO
         MOVE      S11,N2
         MOVE      N2,N6
         MULT      "6",N6
         SUB       "5",N6
         RESET     S2,N6
         CALL      X5
         DIV       "7",N5
         MULT      "2",N5
         BUMP      S2,1
         MOVE      S2,S11
         RETURN
X31      DISPLAY   *P1:24,*R,"OOPS!!...BUMPED A WUMPUS";
         CALL      X5
         COMPARE   "25",N3
         GOTO      X75 IF NOT LESS
         CALL      X35
         DISPLAY   *P1:24,*R,"HEE, HEE, HEE..THE WUMPUS'LL GET Y":
                   "A NEXT TIME";
         GOTO      X6
X75      DISPLAY   *P1:24,*R,*R,"!! C H O M P !!.........THE WUM":
                   "PUS GOT YOU";
         GOTO      X76
X33      DISPLAY   *P1:24,*R,*R,"Y",*R,"I",*R,"I",*R,"E",*R,"E":
                   *R,"E",*R,"EE",*R,"E",*R,*R,"EE",*R,"E",*R,"E":
                   *R,"E",*R,"EE",*R,"E",*R,"E",*R,"EEEEE",*R,"E":
                   *R,"E",*R,"       FELL IN A",*blinkon," PIT              ":
                   *blinkoff:
                   *R,"T",*R,"T",*W,*W,*W;
X76      DISPLAY   *P1:24,*R,*R,*green,"* * *  S  *  N  *  I  *  C  *  ":
                   "K  *  E  *  R  * * *",*N,*R,*R,*R,*yellow,"YOU LOSE!":
                   "!!!",*red,*hoff,*W,*W,*W;
         GOTO      X77
X11      DISPLAY   *P1:24,*R,"ZAP!!!!.......SUPERBAT SNATCH!!!":
                   *W;
X78      CALL      X5
         MATCH     S5,S16
         GOTO      X78 IF ZERO
         MOVE      S5,S10
         MOVE      S10,N6
         MULT      "6",N6
         SUB       "5",N6
         RESET     S2,N6
         MOVE      S2,S6
         BUMP      S2,2
         MOVE      S2,S7
         BUMP      S2,2
         MOVE      S2,S8
         RETURN
         CHAIN     "MASTER"
