PC            Equ             0
              INclude         Common.inc
              INclude         Cons.inc
              include         Nowndd.inc
              include         prtpagedd.inc
Release       INit            "Not"              
Own           Ifile           KEYLEN=4,FIXED=190                            
.str255        Dim             255              
              
test        
; OPen           Own,"\\nins1\e\data\index\ninown.isi"
              keyin           str1
               PRTOPEN        Laser,"",""

;              call            LandscapeLTRHEAD
;               prtpage        Laser;*NEWPAGE
;              call            PortraitLTRHEAD
;               prtpage        Laser;*NEWPAGE
              call            LandscapeLogo
              Move            "This is a test of ClipText, we will see what Happens",str255
              prtpage         Laser;*p750:2400,*Font=PRTpg12,"Client## test test test Names in the News":
                              *p750:2600,*Font=PRTPGfont9,"Client## test test test Names in the News":
                              *p750:2800,*Font=PRTPGfont7,"Client## test test test Names in the News":
                              *CLIPTEXT=2900:6500:150:1000:Str255
               call            CopyRightland
               prtpage        Laser;*NEWPAGE
;              call            PortraitLogo
;              call            CopyRightport using "2001-2005"
;               prtpage        Laser;*NEWPAGE
;              close           Own
              stop
              
              
              
              include         Nownio.inc              
              include         prtpageio.inc
              INclude         COmlogic.inc