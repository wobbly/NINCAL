.Nloinc0008 -- change all emails from --- to
pc        equ       0
          Include   COmmon.inc
          Include   Cons.inc
          include   Lincdd.inc
Release   init      "1.0"
Reldate   Init      "2013 September 9" replace PP with SM
.Reldate   Init      "30 Nov 2011" replace SS with SA & IB
.Reldate   Init      "25 May 2010" replace Kelly with FOrder

.Reldate   Init      "12 February 2010"
File      File
Output    File
From      Dim       55
To        Dim       55


.          Goto      Change


          Open      File,"e:\data\text\ListIncome.dat|NINS1:502"
Loop      Loop
          read      File,seq;LIncVARS
          until     over
.          match     "    ",LIncRECIPIENT
.          goto      Loop if equal

.          rep       Lowup in LIncRECIPIENT
.          scan      "ShirleySchoevaars" in LIncRECIPIENT
          scan      "piapayne" in LIncRECIPIENT
          goto      update if equal
          scan      "PiaPayne" in LIncRECIPIENT
          goto      update if equal
          if        (LIncAuto = "Y")
          cmatch    b1,LIncRECIPIENT
          goto      Update if eos
          goto      Update if equal
          endif
          Scan      "herric" in LIncRECIPIENT
          goto      update if equal
.          Scan      "IBECK" in LIncRECIPIENT
.          goto      update if equal
.          Scan      "SHIRLEY" in LIncRECIPIENT
.          goto      update if equal
.          Scan      "SSCHOE" in LIncRECIPIENT
.          goto      update if equal
          repeat          
          stop
Update
          packkey   LIncFLD1,LincREcID
          Move      C2,LincPath
          call      LincTst
          if        not over
.                    if        (LIncList <> "003921" & LIncList <> "009962" & LIncList <> "024223" & LIncList <> "012049" & LIncList <> "007302" & LIncList <> "018900" & LIncList <> "017203")
.                    move      "SusanAnstrand@nincal.com",LincRecipient
.                    Else
.                    Move      "IngaBeck@nincal.com",LincRecipient
.                    endif
                    Move      "SuzieMcGuire@nincal.com",LincRecipient
          call      LincUpd
          endif
          goto      Loop

          stop

Change
          Open      File,"\\nins1\e\data\text\ListIncome.dat|NINS1:502",read
          Prepare   Output,"\\nins1\e\data\text\ListIncome.new|NINS1:502",exclusive
Loop1     Loop
          read      File,seq;LIncList,LincREcID,LINCDATEBY,LIncTYPE,LMONTH:
                    LIncREP1,LIncREP2,LIncREP3,str4,LIncRECIPIENT,LIncAuto,LIncCOMMENTS
          until     over
          Clear     Str8
          if        (str4 <> "" & str4 <> "   0")
                    if        (LincList = "001123")
                    Pack      Str8 with Str4,"0701"
                    else
                    Pack      str8 with Str4,"0101"
                    endif
                    move      c0,lincyear
                    Move      Str8,LincYear
          else
          Move      C0,LincYear          
          endif          
          Write     Output,seq;LIncList,LincREcID,LINCDATEBY,LIncTYPE,LMONTH:
                    LIncREP1,LIncREP2,LIncREP3,LincYear,LIncRECIPIENT,LIncAuto,LIncCOMMENTS
          Repeat
          
          Weof      output,seq
          Close     File
          Close     Output
          stop

          include   Comlogic.inc

          include   Lincio.inc
