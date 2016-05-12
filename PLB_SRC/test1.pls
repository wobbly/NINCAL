PC        Equ       0
          Include   Common.inc
          Include   COns.inc
Release   Init      "test1"
REldate   Init      "03 Aug 2010"


DimPtr    Dim       ^
Carr      init      0x7f
Fill500   Dim       500
Olrn      dim       6
MCOMP     INit      "This is a dummy client name"
MouseForm form      10
FarRight form       4
FarBottom form      4
T1        form      4
L1        form      4
EditPtr   EditText  ^
TestForm       external "TEST;DisplayForm"
x         plform    TEST1
mFile     menu
FData     init      "&File;&Print;-;E&xit"

          formload  x
          
          create    TEST1;mfile,Fdata
          
          Move      "010286",str6
          Move      "0579",str4
          move      "150150",olrn

          loop
                    eventwait
          repeat

          Shutdown


          
CalcPseudoMouseForm LRoutine EditPtr
.Function used to give a psuedo         Mouse_Down_Event in order to display OrderInfo
.by hitting the     F3 key while sitting on       certain   Edit Text Boxes.
          getprop   EditPtr,top=T1,left=L1,height=N8,width=N9
          calc      MouseForm=((N9*10000)+N8)
          return


OrderDisplayMailerMaster 
          move    C8,N4
          call    OrderDisplayXSTAT using TEST,str6,Str4,OLRN,N4,MouseForm,T1,L1          
                            getprop   daveEditText001,readonly=N9
                            if (N9 = C0)
                                      clear     taskname
                                      getitem   daveEditText001,0,Fill500
                                      if (Fill500 <> "")
                                                append    carr,taskname
                                      endif 
                                      call      Trim using MCOMP
                                      append    MCOMP,taskname
                                      append    " has No",taskname
.                                      append    DimPtr,taskname
                                      append    " names.",taskname
                                      reset     taskname
                                      pack      Fill500,Fill500,taskname
                                      setitem   daveEditText001,0,Fill500
                            endif 
                            return 
          move    C8,N4
          move      "000",str3
          call    OrderDisplayMailer using TEST,STR4,STR3,N4,MouseForm,T1,L1
          call    OrderDisplayXSTAT using Test,str6,str4,olrn,N4,MouseForm,T1,L1
            clear     taskname
            getitem daveEditText001,0,Fill500
            if (Fill500 <>     "")
                      append    carr,Fill500
            endif
            call      Trim using MCOMP
            append    MCOMP,taskname
            append    " has ",taskname
            move      "100000",str10
            call      FormatNumeric using str10,str13
            append    str13,taskname
            append    " names.",taskname
            reset     taskname
            pack      Fill500,Fill500,taskname
            setitem   DaveEditTExt001,0,Fill500
  
          RETURN 
            
          
          
          
          include   comlogic.inc
          