copyrite init	   033,"(1U",0136,033,"(8U"
.			\     \		\
.			  \	 copyrite \
.			   \	 dec 94	   reset symbol set
.			    legal symbol set
copyrite1 init	   033,"(17U",0270,033,"(8U"
.			\     \		\
.			  \	 copyrite \
.			   \	 dec 184	   reset symbol set
.			    pc-850 symbol set
copyrite2 init	   033,"(14U",0250,033,"(8U"
.			\     \		\
.			  \	 copyrite \
.			   \	 dec 168	   reset symbol set
.			    vENTURA us symbol set
copyrite3 init	   033,"(13U",0250,033,"(8U"
.			\     \		\
.			  \	 copyrite \
.			   \	 dec 168	   reset symbol set
.			    vENTURA int symbol set
COPYRITE4  INIT       033,"(19M",0251,033,"(8U"
COPYRITE5  INIT       033,"(7J",0245,033,"(8U"
COPYRITE6  INIT       033,"(9U",0251,033,"(8U"

        SPLOPEN      "\\nins1\d\USERS\DHERRIC\COPYRITE.LST"
        PRINT        "LEGAL ",COPYRITE
        PRINT        "pc-850 ",COPYRITE1
        PRINT        "vENTURA US ",COPYRITE2
        PRINT        "vENTURA US ",COPYRITE3
        PRINT         "????",COPYRITE4
        PRINT         "????",0251
        print          "desktop symbol set ",copyrite5
        print          "windows ",copyrite6
        SPLCLOSE
        RELEASE
        STOP
