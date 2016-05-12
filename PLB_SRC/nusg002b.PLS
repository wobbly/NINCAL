        include common.inc
        include cons.inc
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
        include ndatdd.inc
          include   nusgdd.inc

release init    "1.3"         DLH       .Logo logic
reldate   Init      "13 August 2008"
.release init    "1.2"        ASH       18JUN2007 PLI Inclusion
.release init    "1.1"        ASH       09AUG2004 Logo Conversion
.release init    "1.0"        ASH       Initial Release
.
prfile    pfile     ^
PackData DataList
DimPtr    dim       ^
FrmPtr    form      ^
ColNum    form      1
pagenum   form      1
FirstRec form       9
.START PATCH 1.3
FrmPtr3   form      ^
.END PATCH 1.3

.Define Fonts to be used
font1     font
font2     font
font3     font
.START PATCH 1.2 ADDED LOGIC
font7     font
.END PATCH 1.2 ADDED LOGIC
page      form      9
.
.Testing
.         move      "014862",str6
.         call      PrintUsageToPFile using str6
          shutdown

.begin patch 1.3
.PrintUsageToPFile Routine DimPtr,FrmPtr,prfile
PrintUsageToPFile Routine DimPtr,FrmPtr,prfile,FrmPtr3
.end patch 1.3
.DimPtr  = List Number
.FrmPtr  = Duplexing, '1'=no, '2'=yes
.FrmPtr3  = Company
.prfile = Print File
.begin patch 1.3
          Move      FrmPtr3,Company
.end patch 1.3
          call      Trim using DimPtr
          if (DimPtr = "")
                    return
          endif
          move      C1,NDATPATH
          pack      NDATFLD,DimPtr
          call      ZFillIt using NDATFLD
          move      "NDATKEY",Location
          pack      KeyLocation,"Key: ",NDATFLD
          call      NDATKEY
          if over
                    return
          elseif (NDATLUSAGE = "F")
                    return
          endif
.Testing
.         PRTOPEN   prfile,"\\NTS0\Laser8","FAXFILE.PRN"
.Initialize variables
          create    font1,"Arial",size=14,bold
          create    font2,"Arial",size=11 
          create    font3,"Arial",size=10
        create  font10,"Times New Roman",size=24,bold
        create  font11,"Times New Roman",size=24,italic
        create  font12,"Times New Roman",size=12
        create  font13,"Arial",size=9
.START PATCH 1.93 ADDED LOGIC
          create    font7,"Times New Roman",size=9
.END PATCH 1.93 ADDED LOGIC
          destroy PackData
          create    PackData=1:1:1:1,sorted=1
.START PATCH 1.1 ADDED LOGIC
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 1.1 ADDED LOGIC.
          move      "250",column
          move      "1200",column1
          move      "4200",column2
.
          move      C0,page
          move      C0,ColNum
.
          pack      NUSGFLD1,"01X",LSTNUM
          move      "NUSGAIM",Location
          pack      KeyLocation,"Key: ",NUSGFLD1
          call      NUSGAIM
          loop
                    until over
                    pack      COMPFLD,NUSGMLR
                    move      "COMPKEY",Location
                    pack      KeyLocation,"Key: ",COMPFLD
                    call      COMPKEY
                    if not over
                              if (COMPMUSAGE <> "F")
                                        insertitem PackData,999999,COMPCOMP
                              endif
                    endif
                    move      "NUSGKG",Location
                    pack      KeyLocation,"Key: ",NUSGFLD1
                    call      NUSGKG
          repeat    
          PackData.GetCount giving howmany
          if (howmany > C0)
                    call      UsagePrintHeader
                    sub       C1,howmany
                    for N9,C0,howmany
                              PackData.GetText giving COMPCOMP using N9
                              call      UsagePrintDetail
                    repeat
          endif
.Clean Up
          destroy   font1
          destroy   font2
          destroy   font3
        destroy  font10
        destroy  font11
        destroy  font12
        destroy  font13
          destroy PackData
          prtpage   prfile;*duplex=1;
.Testing
.         prtclose prfile
        return

UsagePrintHeader
.Starting point for first row set here - if this changes border value needs to change!!!!!!!
          add       C1,page
...........................
          if (FrmPtr = 2)
                    prtpage   prfile;*UNITS=*HIENGLISH,*duplex=2;
          else
                    prtpage   prfile;*UNITS=*HIENGLISH;
          endif
...........................
          move      "300",row
          prtpage   prfile;*p7000:50,*font=font2,*uloff,"page ",page;
.START PATCH 1.1 REPLACED LOGIC
.         prtpage   prfile;*p2700:row,*font=font10,"Names";
.         prtpage   prfile;*font=font11,"  in the News";
.         add       eightlpi,row
.         add       eightlpi,row
.         add       eightlpi,row
.         prtpage   prfile;*p1000:row,*pensize=10,*line=7100:row;
.         add       "60",row
.         prtpage   prfile;*p2700:row,*font=font12,"C  A  L  I  F  O  R  N  I  A        I  N  C .";
..Go ahead and print the last line now
..Bullets produced using:  Alt+0149
.         prtpage   prfile;*p1500:9950,*font=font13,"1300 Clay St., 11th Floor, Oakland, CA 94612-1429 • 415-989-3350 • Fax 415-433-7796";
............
.START PATCH 1.2 REPLACED LOGIC
.         prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
.begin patch 1.3
.         if (ELSTCDE = "P")
          if (Company = c2)
.end patch 1.3
                    prtpage   prfile;*p=1630:50,*font=font10,*Alignment=*Center,"Pacific Lists, Inc.":
                              *p=1630:425,*font=font7,"180 Grand Avenue, Suite 1365":
                              *p=1630:550,"Oakland, CA 94612-3716":
                              *p=1630:675,"415-945-9450 ","·"," Fax 415-945-9451":
                              *p=1630:800,"A Division of Names in the News":
                              *font=font2,*Alignment=*Left
          else
                    prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
          endif
.END PATCH 1.2 REPLACED LOGIC
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          add       "60",row
          add       eightlpi,row
.END PATCH 1.1 REPLACED LOGIC
          add       eightlpi,row
          add       eightlpi,row 
          add       eightlpi,row 
          add       eightlpi,row 
          prtpage   prfile;*pcolumn:row,*font=font1,MLSTNAME;
          add       eightlpi,row
          add       eightlpi,row
          prtpage   prfile;*pcolumn:row,*font=font2,"12 MONTH CONTINUATION USAGE";
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
.
          move      C1,ColNum
          move      row,FirstRec
...........................
          if (FrmPtr = 2)
                    move      C1,pagenum
          endif
...........................
        return

UsagePrintDetail
          if (row >= 9620)
                    if (ColNum = 1)
                              move      C2,ColNum
                              move      FirstRec,row
                    else
                  prtpage prfile;*NEWPAGE;
...........................
                              if (FrmPtr = 2)
                                        if (pagenum = 1)
                                                  move      C1,ColNum
                                                  move      "300",row
                                                  move      row,FirstRec
                                                  move      C0,pagenum
                                        else
                                              call    UsagePrintHeader
                                        endif
                              else
                                        move      C1,ColNum
                                    call    UsagePrintHeader
                              endif
...........................
                    endif
        endif
          if (ColNum = 1)
                    move      column,result
          else
                    move      column2,result
          endif
        prtpage prfile;*presult:row,*font=font3,*boldoff,COMPCOMP;
          add       eightlpi,row
          return

          INCLUDE   COMPIO.inc
          INCLUDE   CNTIO.inc
        include ndatio.inc
          include   nusgio.inc
        include comlogic.inc
