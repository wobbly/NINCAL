//////////////////////////////////////////////////////////////////////////////
//
//  PROGRAM:    guidGen.inc
//
//   AUTHOR:    Mandie Lyons (mlyons@adjacency.net)
//
//     DATE:    03 NOV 2004
//
//  PURPOSE:    generates a random GUID for use as an ISAM key
//
// REVISION:    03 NOV 2004     VER01   MLYONS    Created
//
//////////////////////////////////////////////////////////////////////////////
#iwk1           integer 01
#hiByte         integer 01
#loByte         integer 01
#result         integer 04

#cwk1           dim     01
#rawResult      dim     16
#ASCIIresult    dim     32
#GUID           dim     ^
#hexData        dim     01(0..15),("0"):
                                    ("1"):
                                    ("2"):
                                    ("3"):
                                    ("4"):
                                    ("5"):
                                    ("6"):
                                    ("7"):
                                    ("8"):
                                    ("9"):
                                    ("A"):
                                    ("B"):
                                    ("C"):
                                    ("D"):
                                    ("E"):
                                    ("F")

CoCreateGuid    profile ole32:
                        CoCreateGuid:
                        int4:
                        dim
//////////////////////////////////////////////////////////////////////////////
    goto #end
//////////////////////////////////////////////////////////////////////////////
generateGUID routine #GUID

    winapi CoCreateGuid giving #result using #rawResult
    clear #ASCIIresult
    loop
        move #rawResult to #cwk1
        move #cwk1 to #iwk1
        divide "16" into #iwk1 giving #hiByte
        calc #loByte = (#iwk1 - (#hiByte * 16))
        append #hexData(#hiByte) to #ASCIIresult
        append #hexData(#loByte) to #ASCIIresult
        bump #rawResult
    repeat until eos
    reset #ASCIIresult
    move "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX" to #GUID
    edit #ASCIIresult into #GUID

    return
//////////////////////////////////////////////////////////////////////////////
#end
//////////////////////////////////////////////////////////////////////////////
