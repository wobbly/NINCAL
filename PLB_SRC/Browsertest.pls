           inc        common.inc
           inc        cons.inc   
pc         equ        0
Release    Init       ".05"
Reldate    Init       "Not"
taskname2  dim        1000
taskname3  dim        1000
DimPtr                dim        ^
DimPtr1               dim        ^
hold                  dim        100
hold2                 dim        100
name                  dim        500 to hold munged name with formatting
.
white      color
grey       color
           create     white=*white
           create     grey=220:220:220
           
x          plform     Mung
           winhide
           formload x

           MungWebBrowser.Navigate2 USING "\\nins1\e\data\501c\015894Cert.pdf"
           loop
                      eventwait
           repeat
MungRetrieve
           move       B55,taskname2
           clear      taskname2
           return

EnableUpper
           setitem MungEditOldEmail,0,""
           setprop MungEditOldEmail, enabled=1,bgcolor=white
           setprop MungCheckLink, enabled=1
           return

DisableUpper
           setprop MungEditOldEmail, enabled=0, bgcolor=grey
           setprop MungCheckLink, enabled=0
           return
EnableLower
           setitem MungCheckLink, 0,C1  // lower will always be a linked address
           setitem MungEditName,0,""
           setprop MungEditName, enabled=1, bgcolor=white
           setprop MungEditDomain, enabled=1, bgcolor=white
           return

DisableLower
           setitem MungCheckLink, 0,C0  // uncheck link
           setprop MungEditName, enabled=0,bgcolor=grey
           setprop MungEditDomain, enabled=0,bgcolor=grey
           return

           include    comlogic.inc