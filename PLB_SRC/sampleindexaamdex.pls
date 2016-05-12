BadSw	form	1
Result	form	5
nwork01	form	1
if1actions ifile
#work250 dim	250
sFolder	init	"c:\data"
#FMadrs	init	"10.100.1.1:3933"
.------------------------------------------------------------------------------
.*
.* If INDEX fails, BadSw is set and...
.* ...Result = 1 if it can't open the file exclusively
.* ...Result = 2 if an ISAM index fails
.* ...Result = 3 if the AAM index fails
.*
actions_Index
    unpack "" into BadSw,Result  // assume okay
    getfile if1actions,mode=nwork01	// find out if it's already open
    call actions_close if ( equal )	// close it if it's already open
    call actions_open_ex		// try to open it exclusively
    if ( BadSw )   			// oops, someone is in it right now
        move   "1" to result
        return
    endif
    close iflactions			// close it for the indexing
    pack  #work250 with sFolder,"\actions.isi|",#FMadrs
    lowercase #work250
    erase #work250
    pack  #work250 with sFolder,"\actions,",sFolder,"\actions.isi,L520 -N,28-29,30-49,50-55,56-63,64-71,199-201,72-75,214-216"
    index #work250,SUNDM=#FMadrs
    if ( over )    			// failed
        set 	BadSw
        move   	"2" to result
        return
    endif
    pack  #work250 with sFolder,"\actions2.isi|",#FMadrs
    lowercase #work250
    erase #work250
    pack  #work250 with sFolder,"\actions2,",sFolder,"\actions2.isi,L520 -N,28-29,56-63,64-71,214-216,217-218,72-75,199-201,30-49,50-55"
    index #work250,SUNDM=#FMadrs
    if ( over )    // failed
        set BadSw
        move   "2" to result
        return
    endif
    pack  #work250 with sFolder,"\actions.aam|",#FMadrs
    lowercase #work250
    erase #work250
    pack  #work250 with sFolder,"\actions,,L520 -U,D=~,1-1,28-29,30-49,50-55,56-63,64-71,72-75,76-78,79-198,199-201,202-202,208-213,214-216,217-218,219-222,223-226,227-234,235-238,392-392,393-400"
    aamdex #work250,SUNDM=#FMadrs
    if ( over )    // failed
        set BadSw
        move   "3" to result
        return
    endif
    return
    
.------------------------------------------------------------------------------