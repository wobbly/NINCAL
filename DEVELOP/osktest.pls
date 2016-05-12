pc        equ       0
.invoke on screen keyboard
          inc       common.inc
          inc       cons.inc
Release   init      "pre"
          Call      Get64OS

oskfile      Init      "c:\windows\syswow64\osk.exe"

          Call      Get64OS
          if  (Bit64Flag = No)    
          MOVe      "c:\windows\system32\osk.exe",Oskfile
          else
          Move      "c:\windows\syswow64\osk.exe",Oskfile
          endif
Main    PLFORM  osktest.plf
        FORMLOAD        Main
        LOOP
        EVENTWAIT
        REPEAT

          inc       comlogic.inc