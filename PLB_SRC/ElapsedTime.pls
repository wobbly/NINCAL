.  ===========================================================================
. ElapsedTime.inc - report elapsed time between two times passed to routine.
.   Parameters:
.       StartTime   dim     12          // hh:mm:ss.xxx
.       EndTime     dim     12          // hh:mm:ss.xxx
.       ElapsedTime dim     12
.  ===========================================================================
.  ===========================================================================
. 2001.04.11 s.elliott - created.
. 2001.04.25 s.elliott - add "1" to #ElapsedSecs.
. 2007.02.01 s.elliott - changed add "1" to incr.
. 2008.03.07 s.elliott - Allowed accuracy to the millisecond.
. 2008.10.17 s.elliott - Changed ConvToSecs and ConvToTime to be called externally.
. 2013.08.05 s.elliott - Changed ConvToTime to allow for 3-digit hours.
. 2013.08.20 s.elliott - Added ConvTimeToText to convert formatted time (hh:mm:ss.xxx) 
.                           to text (dd Days, hh Hours, mm Minutes, ss.xxx Seconds)
.
.  ===========================================================================
#StartTime  dim     ^                   // hh:mm:ss.xxx (24-hour time)
#EndTime    dim     ^                   // hh:mm:ss.xxx (24-hour time)
#ElapsedTime dim    ^                   // hh:mm:ss.xxx (24-hour time)

#lsLocVars  list                                                                // 2008/03/07 see
#StartSecs  form    9.3                                                         // 2008/03/07 see
#EndSecs    form    9.3                                                         // 2008/03/07 see
#ElapsedSecs form   9.3                                                         // 2008/03/07 see

#MilSeconds form    0.3                 // number of milliseconds               // 2008/03/07 see
#seconds    form    15.3                // number of seconds                    // 2008/03/07 see
#minutes    form    15.3                // number of minutes                    // 2008/03/07 see
#hours      form    15.3                // number of hours                      // 2008/03/07 see
#days       form    15.3                // number of days                       // 2013/08/20 see

#sDDD       dim     3                   // days                                 // 2013/08/20 see
#sHH        dim     2                   // hours
#sHHH       dim     3                   // hours                                // 2013/08/20 see
#sMM        dim     2                   // minutes
#sSS        dim     2                   // seconds
#sMS        dim     4                   // milliseconds (.xxx)                  // 2008/03/07 see
#nDDD       form    3                   // days                                 // 2013/08/20 see
#nHH        form    2
#nHHH       form    3                                                           // 2013/08/05 see
#nMM        form    2
#nSS        form    2
#nMS        form    3                                                           // 2008/03/07 see
#dim1       dim     1
#form2      form    2
work06     dim     6
            listend                                                             // 2008/03/07 see
.  ===========================================================================
ElapsedTime routine #StartTime,#EndTime,#ElapsedTime
    unpack  "" into #lsLocVars                                                  // 2008/03/07 see
    call    ConvToSecs with #StartTime,#StartSecs
    call    ConvToSecs with #EndTime,#EndSecs
    subtract #StartSecs from #EndSecs giving #ElapsedSecs
.    incr    #ElapsedSecs                                        // 4/25/2001    // 2007/02/01 see
    call    ConvToTime with #ElapsedSecs,#ElapsedTime
    return

.  ===========================================================================
.*
.* convert a formatted time (hh:mm:ss.xxx) to equivalent seconds
.*
#ctsi       dim     ^
#ctso       form    ^
ConvToSecs routine #ctsi,#ctso                                                  // 2008/10/17 see
    unpack  "" into #seconds,#minutes,#hours,#sHH,#sMM,#sSS,#sMS,#dim1
    unpack  #ctsi into #sHH,#dim1,#sMM,#dim1,#sSS,#sMS                          // 2008/03/07 see
    chop    #sMS                                                                // 2008/03/07 see
    move    #sMS to #MilSeconds         // .xxx                                 // 2008/03/07 see
    move    #sHH to #hours
    move    #sMM to #minutes
    move    #sSS to #seconds
    for #form2 from 1 to 2
        multiply "60" by #hours         // hours to seconds
    repeat
    multiply "60" by #minutes           // minutes to seconds
    calc    #ctso = #MilSeconds + #seconds + #minutes + #hours                  // 2008/03/07 see
    return

.  ===========================================================================
.*
.* convert seconds to an equivalent formatted time (hh:mm:ss.xxx)
.*
#ctti       form    ^
#ctto       dim     ^
ConvToTime routine #ctti,#ctto                                                  // 2008/10/17 see
    unpack "" into #hours,#minutes,#seconds:                                    // 2008/03/07 see
                   #sHH,#sMM,#sSS,#sMS,#nHH,#nMM,#nSS,#nMS,#nHHH                // 2013/08/05 see
    loop
    while ( #ctti > 60 )                // seconds to minutes
        incr    #minutes                                                        // 2007/02/01 see
        subtract "60" from #ctti
    repeat
    loop
    while ( #minutes > 60 )             // minutes to hours
        incr    #hours                                                          // 2007/02/01 see
        subtract "60" from #minutes
    repeat
    move    #ctti to #seconds           // remaining seconds
    explode #seconds by "." into #nSS,#nMS                                      // 2008/03/07 see
.    move    #nSS to #sSS                                                        // 2008/03/07 see
    pack    #sMS with ".",#nMS                                                  // 2008/03/07 see
    move    #hours to #nHH
    if ( over )                         // significant digits were lost         // 2013/08/05 see
        move    #hours to #nHHH                                                 // 2013/08/05 see
    endif                                                                       // 2013/08/05 see
.    move    #nHH to #sHH
    move    #minutes to #nMM
.    move    #nMM to #sMM
    
    if ( #nHHH > 0 )                    // 3-digit hours                        // 2013/08/05 see
        pack    #ctto with #nHHH,":",#nMM,":",#nSS,#sMS                         // 2013/08/05 see
    else                                                                        // 2013/08/05 see
        pack    #ctto with #nHH,":",#nMM,":",#nSS,#sMS                          // 2008/03/07 see
    endif                                                                       // 2013/08/05 see
    replace " 0" in #ctto
    return

.  ===========================================================================
.*
.* convert formatted time (hh:mm:ss.xxx) to text (dd Days, hh Hours, mm Minutes, ss.xxx Seconds)
.*
#cttti      dim     ^
#cttto      dim     ^
ConvTimeToText routine #cttti,#cttto                                            // 2013/08/20 see
    unpack "" into #cttto,#nDDD,#sDDD,#sHHH,#nHHH,#sMM,#nMM,#sSS,#nSS,#sMS,#nMS             
    explode #cttti by ":" into #nHHH,#nMM,work06
    explode work06 by "." into #nSS,#nMS
    loop
    while ( #nHHH > 24 )                // hours to days
        incr    #nDDD
        subtract "24" from #nHHH
    repeat
    squeeze #nDDD to #sDDD
    squeeze #nHHH to #sHHH
    squeeze #nMM to #sMM
    squeeze #nSS to #sSS
    squeeze #nMS to #sMS
.    pack    #cttto with #sDDD," Days, ",#sHHH," Hours, ",#sMM," Minutes, ",#sSS,".",#sMS," Seconds"
    if ( #nDDD > 0 )
        pack    #cttto with #cttto,#sDDD," Days, "
    endif
    if ( #nHHH > 0 )
        pack    #cttto with #cttto,#sHHH," Hours, "
    endif
    if ( #nMM > 0 )
        pack    #cttto with #cttto,#sMM," Minutes, "
    endif
    if ( #nSS + #nMS > 0 )
        pack    #cttto with #cttto,#sSS,".",#sMS," Seconds "
    endif
    return
    
.  ===========================================================================
.                       ---- END OF ElapsedTime.inc ----
.  ===========================================================================
