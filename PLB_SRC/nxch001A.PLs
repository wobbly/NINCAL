..........................................................
.
. Program:  Nxch001a.pls
. Author:   Andrew Harkins
. Date:     March 30, 2005
. Function: Allows reports to be pulled showing erroneous records in:  NINXNUM.DAT, NINXCHNG.DAT
.
..........................................................

PC       EQU       0
          INCLUDE   COMMON.inc
          INCLUDE   CONS.inc
          INCLUDE   NxngDD.inc
          INCLUDE   NxchDD.inc
          include   nxrfdd.inc
          include   norddd.inc
          include   compdd.inc
          include   cntdd.inc

release  init      "1.0"      30MAR2005  ASH      ORIGINAL RELEASE

tempfile2 file
tempfilea afile
FrmPtr              form      ^
FileName1 dim       75
FileName2 dim       75
FilePath  dim       75
DumpFileName        dim       200
CancFlag  dim       1
str6a               dim       6

ExchangeCleanUpReports
.This is called by BUTIL Only!
.1 = Master - No Details
.2 = Master - Clients not listed as 'Mailer'
.3 = Detail - No Master Accounts
.4 = Detail - Clients not listed as 'Mailer'
.5 = No Corresponding LR Number OR Corresponding LR Number has different Mlr/List combo
          move      C1,result
          call      Trim using user
          if (user = "")
                    pack      taskname,"I have lost track of who you are!",newline,"If I continue I will not be able to",newline,"email you a confirmation.",newline,"Do you want to continue anyway?"
                    alert     plain,taskname,result
          endif
          if (result = 1)
                    if (INPNAME = "1")
                              call      MasterNoDetail
                    elseif (INPNAME = "2")
                              call      MasterNotMailer
                    elseif (INPNAME = "3")
                              call      DetailNoMaster
                    elseif (INPNAME = "4")
                              call      DetailNoMailer
                    elseif (INPNAME = "5")
                              call      DetailNoLR
                    endif
          endif
          shutdown

MasterNoDetail Routine
.Finds Master Accounts that have either no Detail Accounts OR a Beg. Balance Detail Account only
          call      CreateTempFile using C0
          call      GetDumpFile
.
          if (CancFlag = NO)
                    clear     taskname
.begin DH
.                   if (osflag = c1 | osflag = c5)
.                             pack      taskname,"!c:\winnt\system32\cmd.exe \\nins1\e\apps\plb\code\sunadxnt \\nins1\e\data\text\ninxchng.dat c:\work\dump -1-12"                    ."
.                   elseif (osflag = c3 | osflag = c4)
.                             pack      taskname,"!c:\command.com xcopy \\nins1\e\apps\plb\code\sunadxnt \\nins1\e\data\text\ninxchng.dat c:\work\dump -1-12"                         ."
.                   elseif (osflag = c6)
.                             pack      taskname,"!c:\windows\system32\cmd.exe \\nins1\e\apps\plb\code\sunadxnt \\nins1\e\data\text\ninxchng.dat c:\work\dump -1-12"        ."
.                   endif
.                   execute   taskname
          pack      taskname,"\\nins1\e\data\text\ninxchng.dat c:\work\dump -1-12"
.>Patch 1.31 End
          AAmdex    Taskname

                    open      tempfilea,"c:\work\dump.aam",EXCLUSIVE
.
                    open      tempfile,FileName1,EXCLUSIVE
                    loop
                              read      tempfile,SEQ;ACCKEY:
                                        nxngdate:
                                        ENTRY:
                                        Flag
                              until over
                              pack      str15,"01X",ACCKEY
                              read      tempfilea,str15;EXKEY
                              if over
.Should never happen!!
                              else
                                        loop
                                                  until over
                                                  unpack    EXKEY,str12,str5
                                                  call      Trim using str5
                                                  move      C0,N5
                                                  move      str5,N5
                                                  if (N5 > C0)
                                                            write     tempfile2,SEQ;ACCKEY:
                                                                      nxngdate:
                                                                      ENTRY:
                                                                      Flag
                                                            break
                                                  endif
                                                  readkg    tempfilea;EXKEY
                                        repeat
                              endif
                    repeat
                    call      EmailNotification
          endif
          close     tempfile
          close     tempfile2
          close     tempfilea
          erase     FileName1
          erase     "c:\work\dump.aam"
          return

MasterNotMailer Routine
.Finds Master Accounts that have a Mailer Not Listed as a 'Mailer' in COMPANY.DAT
          call      CreateTempFile using C0
          call      GetDumpFile
.
          if (CancFlag = NO)
                    open      tempfile,FileName1,EXCLUSIVE
                    loop
                              read      tempfile,SEQ;ACCKEY:
                                        nxngdate:
                                        ENTRY:
                                        Flag
                              until over
                              unpack    ACCKEY,COMPFLD
                              move      "MNM-COMPKEY",Location
                              pack      KeyLocation,"Key: ",COMPFLD
                              call      COMPKEY
                              if over
                                        move      "Mailer 1 - NotinFile:",str25
                                        call      MasterNotMailerError
                              elseif (COMPMLRFLG <> "T")
                                        move      "Mailer 1 - NotMailer:",str25
                                        call      MasterNotMailerError
                              else
                                        unpack    ACCKEY,str6,COMPFLD
                                        move      "MNM-2-COMPKEY",Location
                                        pack      KeyLocation,"Key: ",COMPFLD
                                        call      COMPKEY
                                        if over
                                                  move      "Mailer 2 - NotinFile:",str25
                                                  call      MasterNotMailerError
                                        elseif (COMPMLRFLG <> "T")
                                                  move      "Mailer 2 - NotMailer:",str25
                                                  call      MasterNotMailerError
                                        endif
                              endif
                    repeat
                    call      EmailNotification
          endif
          close     tempfile
          close     tempfile2
          erase     FileName1
          return

MasterNotMailerError
          write     tempfile2,SEQ;str25:
                    ACCKEY:
                    nxngdate:
                    ENTRY:
                    Flag
          return

DetailNoMaster Routine
.Finds Detail Records that do not have associated Master Accounts
          call      CreateTempFile using C1
          call      GetDumpFile
.
          if (CancFlag = NO)
                    open      tempfile,FileName1,EXCLUSIVE
                    loop

                              read      tempfile,SEQ;nxchvars
                              until over
                              unpack    EXKEY,str6,str6a
.
                              pack      NXNGFLD1,"01X",str6
                              pack      NXNGFLD2,"02X",str6a
                              move      "DNM-NXNGAIM",Location
                              pack      KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
                              call      NXNGAIM
                              if over
                                        pack      NXNGFLD1,"01X",str6a
                                        pack      NXNGFLD2,"02X",str6
                                        move      "DNM-NXNGAIM-2nd",Location
                                        pack      KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
                                        call      NXNGAIM
                                        if over
                                                  write     tempfile2,SEQ;"No Master Record: ",nxchvars
                                        endif
                              endif
                    repeat
                    call      EmailNotification
          endif
          close     tempfile
          close     tempfile2
          erase     FileName1
          return

DetailNoMailer Routine
.Finds Detail Records that have a Mailer Not Listed as a 'Mailer' in COMPANY.DAT
          call      CreateTempFile using C1
          call      GetDumpFile
.
          if (CancFlag = NO)
                    open      tempfile,FileName1,EXCLUSIVE
                    loop
                              read      tempfile,SEQ;nxchvars
                              until over
                              unpack    EXKEY,COMPFLD
                              move      "DNM-COMPKEY",Location
                              pack      KeyLocation,"Key: ",COMPFLD
                              call      COMPKEY
                              if over
                                        move      "Mailer 1 - NotinFile:",str25
                                        call      DetailNotMailerError
                              elseif (COMPMLRFLG <> "T")
                                        move      "Mailer 1 - NotMailer:",str25
                                        call      DetailNotMailerError
                              else
                                        unpack    EXKEY,str6,COMPFLD
                                        move      "DNM-2-COMPKEY",Location
                                        pack      KeyLocation,"Key: ",COMPFLD
                                        call      COMPKEY
                                        if over
                                                  move      "Mailer 2 - NotinFile:",str25
                                                  call      DetailNotMailerError
                                        elseif (COMPMLRFLG <> "T")
                                                  move      "Mailer 2 - NotMailer:",str25
                                                  call      DetailNotMailerError
                                        endif
                              endif
                    repeat
                    call      EmailNotification
          endif
          close     tempfile
          close     tempfile2
          erase     FileName1
          return

DetailNotMailerError
          write     tempfile2,SEQ;str25:
                    nxchvars
          return

DetailNoLR Routine
.Finds Detail Records that do not have corresponding LR Numbers, or have different Mlr/List Combo
          call      CreateTempFile using C1
          call      GetDumpFile
.
          if (CancFlag = NO)
                    open      tempfile,FileName1,EXCLUSIVE
                    loop
                              read      tempfile,SEQ;nxchvars
                              until over
                              pack      NORDFLD,LR
                              move      "DNLR-NORDKEY",Location
                              pack      KeyLocation,"Key: ",NORDFLD
                              call      NORDKEY
                              if over
                                        move      "Not in NINORD:   ",str25
                                        call      DetailNoLRError
                              else
                                        unpack    EXKEY,str6,str6a
.Start Temp logic until Order file converted
                                        pack      COMPFLD3,OMLRNUM
                                        move      "COMPKEY3",Location
                                        pack      KeyLocation,"Key: ",COMPFLD3
                                        call      COMPKEY3
.                                       if (str6 <> OMLRNUM AND str6a <> OMLRNUM)
                                        if (str6 <> COMPNUM AND str6a <> COMPNUM)
                                                  move      "LR Mlr No Match: ",str25
                                                  call      DetailNoLRError
                                        else
                                                  move      C1,NXRFPATH
                                                  clear     NXRFFLD2
                                                  pack      NXRFFLD,OLNUM
                                                  move      "NXRFKEY",Location
                                                  pack      KeyLocation,"Key: ",NXRFFLD
                                                  call      NXRFKEY
                                                  if (str6 <> NXRFMLR AND str6a <> NXRFMLR)
                                                            move      "LR List No Match:",str25
                                                            call      DetailNoLRError
                                                  endif
                                        endif
.End Temp logic until Order file converted
                              endif
                    repeat
                    call      EmailNotification
          endif
          close     tempfile
          close     tempfile2
          erase     FileName1
          return

DetailNoLRError
          write     tempfile2,SEQ;str25:
                    nxchvars
          return

CreateTempFile Routine FrmPtr
.FrmPtr  = '0' - NINXNUM, '1' - NINXCHNG
          clock     timestamp,timestamp
          pack      FileName1,"c:\work\",timestamp,".dat"
          erase     FileName1
.
          call      getwinver
          clear     taskname
.          if (osflag = c1 | osflag = c5)
.                    append    "!c:\winnt\system32\cmd.exe /c copy \\nins1\e\data\text\",taskname               ."
.          elseif (osflag = c3 | osflag = c4)
.                    append    "!c:\command.com /c copy \\nins1\e\data\text\",taskname                          ."
.          elseif (osflag = c6)
.                    append    "!c:\windows\system32\cmd.exe /c copy \\nins1\e\data\text\",taskname   ."
.          endif
..         if (osflag = c1 | osflag = c5)
..                   append    "!c:\winnt\system32\cmd.exe /c copy \\nts2\d\data\text\",taskname               ."
..         elseif (osflag = c3 | osflag = c4)
..                   append    "!c:\command.com /c copy \\nts2\d\data\text\",taskname                          ."
..         elseif (osflag = c6)
..                   append    "!c:\windows\system32\cmd.exe /c copy \\nts2\d\data\text\",taskname   ."
..         endif
          if (FrmPtr = C0)
                    append    "\\nins1\e\data\text\NINXNUM.DAT ",taskname
          else
                    append    "\\nins1\e\data\text\NINXCHNG.DAT ",taskname
          endif
.          append    FileName1,taskname
          reset     taskname
          Copyfile  Taskname,Filename1
.          execute   taskname
          return

GetDumpFile
          move      NO,CancFlag
          clear     DumpFileName
          move      "ExchDump.dat",FileName2
          move      "c:\work",FilePath
          GETFNAME PREP,"Prepare Dump File",FileName2,FilePath,"DAT"
          if over
                    pack      taskname,"You did not Select a File Name!",newline,"Did you wish to run this report?"
                    alert     plain,taskname,result
                    if (result = 1)
                              goto GetDumpFile
                    else
                              move      YES,CancFlag
                    endif
          endif
          pack      DumpFileName,FilePath,FileName2
          prepare   tempfile2,DumpFileName,Exclusive
          return

EmailNotification
.Email note back to them
          if (DumpFileName <> "")
.Safety Check
                    call      Trim using user
                    if (user <> "")
                              move      "This is a message from       NXCH001A.",MailSubjct
                              append    "Your file was created!",MailBOdy
                              append    CRLF,Mailbody
                              Append    "Location:  ",Mailbody
                              Append    DumpFileName,Mailbody
                              append    CRLF,Mailbody
                              Append    "FILE:///",Mailbody
                              Append    DumpFileName,Mailbody
                              append    CRLF,Mailbody
                              Reset     MailBOdy
                              pack      MailTo,user,"@nincal.com"
                              pack      MailFrom,user,"@nincal.com"
                              call      SendMail
                    endif
          endif
          return

          include   compio.inc
          include   cntio.inc
          INCLUDE   Nxngio.inc
          INCLUDE   Nxchio.inc
          include   nxrfio.inc
          include   nordio.inc
          include   comlogic.INC
