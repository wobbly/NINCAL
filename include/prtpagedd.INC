PRTPG24B        Font
PRTPG24I        Font
PRTPG12         Font
PRTPG12I        Font
PRTPG12B        Font
PRTPG10         Font
PRTPG10B        Font
PRTPG10I        Font
PRTPG9         Font
PRTPG9B        Font
PRTPG9I        Font
PRTPG6           Font
PRTPG85           Font
PRTPG7           Font
PRTPG9BI           Font
PRTPGfont7         font
PRTPGfont8         font
PRTPGfont9         font
NINLogo       PICT
NINLogoColorbox      Pict
NINLogobox      Pict
CopyRightS     Init            "©"
.
Laser                 PFILE

               create  PRTpg24B,"Times New Roman",size=24,Bold
               create  PRTpg24I,"Times New Roman",size=24,Italic
               create  PRTpg12,"Times New Roman",size=12
               create  PRTpg12B,"Times New Roman",size=12,Bold
               create  PRTpg12I,"Times New Roman",size=12,Italic
               create  PRTpg10,"Times New Roman",size=10
               create  PRTpg10B,"Times New Roman",size=10,Bold
               create  PRTpg10I,"Times New Roman",size=10,Italic
               create  PRTpg9,"Times New Roman",size=9
               create  PRTpg9B,"Times New Roman",size=9,Bold
               create  PRTpg9I,"Times New Roman",size=9,Italic
               create  PRTpg6,"Times New Roman",size=6
               create  PRTpg9BI,"Times New Roman",size=9,Bold,Italic
               create  PRTpg7,"Times New Roman",size=7
               create  PRTpg85,"Times New Roman",Size="8.5"
              create          PRTPGfont7,"Helvetica",size=12
;              create          PRTPGfont7,"Helvetica",size=14,bold
              create          PRTPGfont8,"Helvetica",size=14,italic
              create          PRTPGfont9,"Arial",size=12
              move            "750",column                              ;requires COns.inc
              move            "1750",column1                              ;requires COns.inc
              move            "3000",column2                              ;requires COns.inc

               CREATE         NINLogo=3:13:30:50:
                              "\\nins1\e\netutils\NIN logo black outline.jpg"

               CREATE         NINLogoColorbox=3:10:30:40:
                              "\\nins1\e\netutils\logo color box only.jpg"
               CREATE         NINLogobox=3:10:30:40:
                              "\\nins1\e\netutils\logo block Outline box.jpg"

