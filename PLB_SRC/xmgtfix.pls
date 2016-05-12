pc         equ        0
           inc        common.inc
           inc        cons.inc
           inc        xmgtdd.inc
release    init       "not"
reldate    init       "no"
file       file


           open       file,"\\nins1\e\data\text\exchmgnt.save|nins1:502"
           loop
           read       file,seq;xmgtlr:
                      xmgtQty:
                      xmgtDate:
                      xmgtOrd:
                      xmgtInv:
                      xmgtRate:
                      xmgtown:
                      xmgtlist
           until      over
           packkey    xmgtfld,xmgtlr
           call       xmgtwrt
           repeat
           
           inc        xmgtio.inc
           include    comlogic.inc
           
           
           