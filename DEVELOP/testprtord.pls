PC       EQU       0
          INCLUDE   COMMON.INC
          INCLUDE   CONS.INC
          include   hp.inc
          include   compdd.inc
          INclude   Norddd.inc
font1               font
Font4               font
font5               font
fontO8              font
fontO9              font
fontO9I             font
fontO10             font
fontO10n  font
fontO10B  font
fontO12B  font
fontO14             font
FontO14B  font
FontO14BI font
FontO18I  font
FontO7              font
FontO7dot5          font
FontO7dot5B         font
FontO7dot5I         font
FontO7dot5BI        font
FontO18B  font
FontO18BI font
PRTPG24B  font
PRTPG24I  font
PRTPG10             font
.START PATCH 9.41 ADDED LOGIC
Font08I   font
Font08BI font
.END PATCH 9.41 ADDED LOGIC
.Create fonts to be used
sevenfive form      "7.5"
Laser     pfile
externalmode        integer 1
Blockout  PICT
Blockout1 PICT


          create    font1,"Times New Roman",size=14,bold
          create    fontO8,"Times New Roman",size=8
          create    font5,"Times New Roman",size=11
          Create    fontO9,"Times New Roman",size=9
          Create    fontO9I,"Times New Roman",size=9,Italic
          create    fontO10,"Times New Roman",size=10
          create    fontO10n,"Courier New",size=11
          create    fontO10B,"Times New Roman",size=10,Bold
          create    fontO12B,"Times New Roman",size=12,Bold
          create    fontO14,"Times New Roman",size=14
          create    fontO14B,"Times New Roman",size=14,Bold
          create    fontO14BI,"Times New Roman",size=14,Bold,Italic
          create    fontO18I,"Times New Roman",size=18,Italic
          create    fontO7,"Times New Roman",size=7
          create    fontO7dot5,"Times New Roman",size=sevenfive
          create    fontO7dot5I,"Times New Roman",size=sevenfive,Italic
          create    fontO7dot5b,"Times New Roman",size=sevenfive,Bold
          create    fontO7dot5bI,"Times New Roman",size=sevenfive,Bold,Italic
          create    fontO18B,"Times New Roman",size=18,Bold
          create    fontO18BI,"Times New Roman",size=18,Bold,Italic
.
          create    PRTpg24B,"Times New Roman",size=24,Bold
          create    PRTpg24I,"Times New Roman",size=24,Italic
          create    PRTpg10,"Times New Roman",size=10
.START PATCH 9.41 ADDED LOGIC
          create    font08I,"Times New Roman",size=8,Italic
          create    font08bI,"Times New Roman",size=8,Bold,Italic
font7     font
font8     font
font9     font
NINLogo   PICT
          move      "750",column
          move      "1750",column1
          move      "3000",column2
          create    font7,"Helvetica",size=14,bold
          create    font8,"Helvetica",size=14,italic
          create    font9,"Arial",size=12
.START PATCH 9.38.5 REPLACED LOGIC
.         CREATE    NINLogo=3:13:30:50:
.                   "\\nts0\c\netutils\NIN logo black outline.jpg"
          if (externalMode)
                    CREATE  NINLogo=3:13:30:50,"..\images\NIN logo black outline.jpg"
          else
                    CREATE    NINLogo=3:13:30:50:
                              "\\nins1\e\netutils\NIN logo black outline.jpg"
          endif
.                    CREATE    blockout=3:20:30:50:
.                              "\\nins1\e\netutils\blockout.tif"
.                    CREATE    blockout1=3:20:30:50:
.                              "\\nins1\3\netutils\blockout2.tif"
  
.         Prtopen   Laser,"",""
.         call      prtordfrmPL
.         prtpage   Laser;*NewPage
.         call      prtordfrmPL
.         call      Whitebox
.         prtpage   Laser;*NewPage
.         call      prtordfrmPL
.                   CREATE    blockout=3:13:30:50:
.                             "\\nts0\c\netutils\blockout.gif"
.         call      Greenbox
.         stop
          move      " ",compexcl
          Prtopen   Laser,"",""
.brokerage
          call      prtordfrmGuiB
          call      PRtMlrboxGui
         prtpage   Laser;*NewPage
.          call      prtordfrmGuiB
.          call      prtofficeboxGui
.         prtpage   Laser;*NewPage
.          call      prtordfrmGuiB
.          call      prtOwnerboxGui
.         prtpage   Laser;*NewPage
.          call      prtordfrmGuiB
.          call      prtFulFIlboxGui


.LM
          call      prtordfrmGuia
          call      PRtMlrboxGui
         prtpage   Laser;*NewPage
          call      prtordfrmGuia
          call      prtofficeboxGui
         prtpage   Laser;*NewPage
          call      prtordfrmGuia
          call      prtOwnerboxGui
         prtpage   Laser;*NewPage
          call      prtordfrmGuia
          call      prtFulFIlboxGui

.          call      prtofficeboxGui
.         prtpage   Laser;*NewPage
.          call      prtordfrmGuiA
.          call      prtownerboxGui
.         prtpage   Laser;*NewPage
.          call      prtordfrmGuiA
.          call      prtFulfilboxGui


.         prtpage   Laser;*p=1000:4903,*font=FontO7Dot5I,"Mailer Guarantees ",onetper,"% payment on Gross Names Shipped"
.         prtpage   Laser;*p=1000:5033,"& will pay $",onetrc,"/m running charge on unused names."
.         prtpage   Laser;*NewPage
.         call      PrtOrdFrmGuiA
.         prtpage   Laser;*p=1000:4903,*font=FontO9I,"Mailer Guarantees ",onetper,"% payment on Gross Names Shipped"
.         prtpage   Laser;*p=1000:5033,"& will pay $",onetrc,"/m running charge on unused names.",*Font=FontO10
.         call      PrtOwnerBoxGui
          prtclose  Laser
          stop
.          include   \\nins1\e\library\develop\prtorderpage1.inc
          include   \\nins1\e\library\include\prtorderpage1.inc