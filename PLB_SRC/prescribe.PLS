        include          common.inc
.
        splopen    "\\NINs2\Laser8","R"
.       print      "!R!FRPO I0,'REMING'; EXIT;"
.       print      "!R!FRPO M2,2;EXIT;"
.       print      "!R!FRPO M2,2; STAT1; EXIT;"
.       print      *n,*n,"test"
        
.       splopen    "DH-3500","A"
.       print      "!R! Unit I; Box 2, 2; Box 1, 1, E; Cir 1; Box 1,1; Page;  ":
.                  "res: UNIT P; SFNT 'Swiss742SWC-BoldItalic', 16; UNIT C; ":
.                  "MAP 8, 8; CIR .2; MRP -.1, -.1; ":
.                  "SCP; SCP; SCP; SCP; SCP; SCP; SCP; ":
.                  "MRPA 2, 0; TEXT 'N' ; ":
.                  "RPP; MRPA 2, 45; TEXT 'NE'; ":
.                  "RPP; MRPA 2, 90; TEXT 'E'; ":
.                  "RPP; MRPA 2, 135; TEXT 'SE'; ":
.                  "RPP; MRPA 2, 180; TEXT 'S'; ":
.                  "RPP; MRPA 2, 225; TEXT 'SW'; ":
.                  "RPP; MRPA 2, 270; TEXT 'W'; ":
.                  "RPP; MRPA 2, 315; TEXT 'NW'; ":
.                  "PAGE ; EXIT;"
.       print      "!R!CALLSA,4.50,09.30; EXIT;"
.       print      "!R!FRPO o0,0;EXIT;"
       print        "!R!UPGR'SYS';EXIT;"
          print      *F       
        splclose
        release
        stop
