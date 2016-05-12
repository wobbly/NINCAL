        include    common.inc
        include    cons.inc
release init       "not"
        move       "03" to mm
        move       "01" to dd
        move       "00" to yy
        move       "20" to cc
        call       cvtjul
        display    *p10:12,*ef,juldays,b1,nfeb,*w10
        stop
        include    comlogic.inc

