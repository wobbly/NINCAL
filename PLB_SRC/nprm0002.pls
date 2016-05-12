........................................
. Program:      NPRM0002.PLS
. Function:     Promo Report Program
. Author:       Andrew Harkins
. Orig. Date:   December 12, 2001
. Release:      1.0
........................................

PC      EQU     1
.Include Files
        include common.inc
        include cons.inc
release init    "1.16"   ASH  16SEP2004 Rewrite by Marketing
.release init    "1.15"   ASH 09AUG2004 Logo Conversion
.release init    "1.14"   DM  23CT2003 PATCH TO ADD BC TO LIST NEWS LETTER
.release init    "1.13"   DM  07CT2003 PATCH TO REMOVE BLO FROM LIST NEWS LETTER
.release init    "1.12"   DM  29SEP2003 PATCH TO REPLACE JN WITH JG. REMOVE USERS FROM DATA CARDS, COUNTS & CLEARANCES AND ADD ALL TO ORDERS, PER BLO
.release init    "1.11"   ASH 24JAN2003 TEMPORARY PATCH UNTIL NEW LIST MANAGER IS HIRED
.release init    "1.1"   DMB  20SEP02  XP Patch
;release init    "1.0"   ASH 12DEC01  DEVELOPMENT RELEASE

;osflag   form      1         .1=win 95,98, 2=NT
DimPtr    dim       ^
.Files to open
prfile  pfile
font1   font
font2   font
font3   font
font4   font
font5   font
font6   font
font7   font
.
PromoPrintFile Routine DimPtr
mss1    plform  Error
        formload mss1

.START PATCH 1.16 REPLACED LOGIC
.        create  font1,"Arial",size=36
.        create  font2,"Arial",size=48 
.        create  font3,"Times New Roman",size=24
.        create  font4,"Arial",size=18,italic=1
.        create  font5,"Times New Roman",size=14,bold=1,italic=1
.        create  font6,"Times New Roman",size=14
.        create  font7,"Arial",size=12,bold=1
.
.        create  font10,"Times New Roman",size=24,bold
.        create  font11,"Times New Roman",size=24,italic
.        create  font12,"Times New Roman",size=12
.        create  font13,"Arial",size=9
........................................................
          create    font1,"Arial",size=30,bold=1
          create    font2,"Arial",size=12,bold=1 
          create    font3,"Arial",size=18,bold=1,italic=1
          create    font4,"Arial",size=14,italic=1
          create    font5,"Arial",size=12,bold=1
          create    font6,"Arial",size=12
          create    font7,"Arial",size=8
.END PATCH 1.16 REPLACED LOGIC

.START PATCH 1.15 ADDED LOGIC
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 1.15 ADDED LOGIC

        move    "NPRM0002",WPrognme

.Find out system information
        getinfo system,str6
        unpack  str6 into str1,str1
        move    C0,osflag
        if (str1 = "3" or str1 = "4")
                move    C1,osflag
;OSPatch1.1 
        elseif (str1 = "1" | str1 = "5"| str1 = "6")
;        elseif (str1 = "1" | str1 = "5")
;OSPatch1.1
                move    C2,osflag
        endif

.Set up columns
.START PATCH 1.16 REPLACED LOGIC
.        move    "800",column
.        move    "1000",column1
.        move    "1350",column2
.        move    "2350",column3                   .Title
.        move    "3500",column4
.        move    "5500",column5
...............................
          move      "1000",column
          move      "1400",column1
          move      "2500",column2                .Title
          move      "2750",column3
          move      "4000",column4
.END PATCH 1.16 REPLACED LOGIC
.
          call      PromoOpenFile
          call      PromoPrintHeader
          call      PromoPrintRecord
        PRTCLOSE prfile         .CLOSE AND PRINT LAST FILE
          return
.        shutdown


PromoOpenFile
.        move    OWNFAX,faxnum
.        match   "0000000000",faxnum
.        if      equal
.                move    YES,PrtFlag             .PRINT IT
.        else
.                type    faxnum
.                if not equal
.                        move    YES,PrtFlag     .PRINT IT
.                else
.                        move    NO,PrtFlag      .FAX IT
.                        count   N2,faxnum
.                        compare C10,N2
.                        if equal
.                                move    C1,LONGDIST
.                                unpack  faxnum,str3,str7
.                                match   "510",str3
.                                if equal
.                                        move    str7,faxnum
.                                        clear   LONGDIST
.                                else
.                                        match   B3,str3
.                                        if equal
.                                                move    str7,faxnum
.                                                clear   LONGDIST
.                                        endif
.                                endif
.                        endif
.                endif
.        endif
.        if (PrintFlag <> C0)
.                move    YES,PrtFlag
.        endif
.
..START NEW TEST - PREVENT FAXES FROM BEING CREATED
..        MOVE    YES,PRTFLAG
..END NEW TEST        
..
.        if (PrtFlag = YES)
..Printer of your choice
.                if (PrintFlag = C0 | PrintFlag = 3)     .Laser2 = Default
.                        if (osflag = c2)         .nt
.                                PRTOPEN prfile,"\\NTS0\Laser2","FAXFILE.PRN"
.                        elseif (osflag = c1)         .win 95 98
.                                PRTOPEN prfile,"Laser2","FAXFILE.PRN"
.                        else   .(osflag = c0)         .Don't know prompt for printer
.                                PRTOPEN prfile,"-","FAXFILE.PRN"
.                        endif
.                elseif (PrintFlag = 1)  .Laser3
.                        if (osflag = c2 | osflag = C5)         .nt
.                                PRTOPEN prfile,"\\NTS0\Laser3 Blankstock","FAXFILE.PRN"
.                        elseif (osflag = c1)         .win 95 98
.                                PRTOPEN prfile,"Laser3 Blankstock","FAXFILE.PRN"
.                        else   .(osflag = c0)         .Don't know prompt for printer
.                                PRTOPEN prfile,"-","FAXFILE.PRN"
.                        endif
.                elseif (PrintFlag = 2)  .PDF
.                        clear   str25
.                        append  OLON,str25
.                        append  "_",str25
.                        append  OLNUM,str25
.                        reset   str25
.                        PRTOPEN prfile,"Acrobat Distiller",str25
.                        pack    str55,str25,".pdf"
.                endif
.                ADD     C1,COUNTR2
.        else
..START TEST - PREVENT FILES FROM ACTUALLY BEING SENT OUT TO CLIENTS
..                clear   LONGDIST
..                MOVE    "4337796",FAXNUM
..END TEST
..Create spool file to concatenate with prtfile and send to fax machine
..PRTOPEN will not allow embedded formatting codes and so it has to be done this way :(        
..Following line MUST appear in PLBWIN.INI:  PLBVOL_P=F:\DATA\FAX  !!!!
..This will give the path to find HDRFILE.PRN.  All files associated with faxes
..will now appear in this new subdirectory.  (ASH)
.                move    "                                        ",APIFileName
.                clear   APIFileName
.                pack    APIFileName,NTWKPATH4,"fax\hdrfile.prn",hexzero
.                call    DeleteFile
.                if (APIResult = 0 | APIResult = hexeight)
.                endif
.                SPLOPEN "HDRFILE/PRN:P"
.                print   "^[D",longdist,faxnum,"^[N",OWNOCPY:
.                        "^[S",CNTNAME,B2,CNTPHONE," ^]"
.                SPLCLOSE   
.                PRTOPEN prfile,"FAXFILE","FAXFILE.PRN"
.                ADD     C1,COUNTR
.        endif
.        if (PrintFlag = 0)
.                DISPLAY *P10:12,*EL,"FAX   COUNT ",COUNTR
.                DISPLAY *P10:14,*EL,"PRINT COUNT ",COUNTR2
.        endif
.............................
          PRTOPEN prfile,"FAXFILE","FAXFILE.PRN"
        RETURN
       
.Print Heading
PromoPrintHeader
.Starting point for first row set here - if this changes border value needs to change!!!!!!!
          prtpage prfile;*UNITS=*HIENGLISH;
          move    "300",row
.START PATCH 1.15 REPLACED LOGIC
.         prtpage prfile;*p2700:row,*font=font10,"Names";
.         prtpage prfile;*font=font11,"  in the News";
.         add     eightlpi,row
.         add     eightlpi,row
.         add     eightlpi,row
.         prtpage prfile;*p1000:row,*pensize=10,*line=7100:row;
.         add     "60",row
.         prtpage prfile;*p2700:row,*font=font12,"C  A  L  I  F  O  R  N  I  A        I  N  C .";
..Go ahead and print the last line now
..Bullets produced using:  Alt+0149
.         prtpage prfile;*p1500:9950,*font=font13,"1300 Clay St., 11th Floor, Oakland, CA 94612-1429 • 415-989-3350 • Main Fax 415-433-7796";
.         prtpage prfile;*p1800:10085,*font=font13,"List Management Fax 510-628-8313 • email: name@nincal.com • www.nincal.com";
...................
          add     eightlpi,row
          add     eightlpi,row
          add     eightlpi,row
          add     "60",row
          prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
.END PATCH 1.15 REPLACED LOGIC
          return

PromoPrintRecord
.        if (row >= 8372)        .Position of Largest Possible Last Record (would include 7 lines of Special Instructions)
.                prtpage prfile;*NEWPAGE;
.                call    PromoPrintHeader
.                call    PromoListHeader
.        endif
.START PATCH 1.16 REPLACED LOGIC
.         prtpage prfile;*pcolumn3:1500,*font=font2,"L     N";
.         prtpage prfile;*p2700:1650,*font=font1,"IST";
.         prtpage prfile;*p4150:1650,"EWS";
..
..        prtpage prfile;*pcolumn:3000,*font=font7,"To:  ",DimPtr;
..FOR TESTING PURPOSES ONLY!!
.         prtpage prfile;*pcolumn:3000,*font=font7,"To:  ";
..
.         prtpage prfile;*pcolumn:3800,*font=font3,"- New Exclusives";
.         prtpage prfile;*pcolumn:4200,"- Newly Updated Data Cards";
.         prtpage prfile;*pcolumn:4600,"- New Counts & Pricing";
..
.         prtpage prfile;*pcolumn:5675,*font=font4,"Call our List Specialists to get your mailer the hottest";
.         prtpage prfile;*pcolumn:6000,"new names on the market!";
..
..START PATCH 1.11 REPLACED LOGIC
..        prtpage prfile;*pcolumn:6500,*font=font5,"For Data Cards, Counts & Clearances...";
..        prtpage prfile;*pcolumn1:6850,"•"
..        prtpage prfile;*pcolumn2:6850,*font=font6,"Kathy Obee";
..        prtpage prfile;*pcolumn4:6850,"(510) 302-4645";
..        prtpage prfile;*pcolumn5:6850,"KathyObee@nincal.com";
...
..        prtpage prfile;*pcolumn:7200,*font=font5,"For Orders...";
..        prtpage prfile;*pcolumn1:7550,"•"
..        prtpage prfile;*pcolumn2:7550,*font=font6,"Agnes Alvarez";
..        prtpage prfile;*pcolumn4:7550,"(510) 302-4646";
..        prtpage prfile;*pcolumn5:7550,"AgnesAlvarez@nincal.com";
..        prtpage prfile;*pcolumn1:7775,*font=font5,"•"
..        prtpage prfile;*pcolumn2:7775,*font=font6,"Tami Barocio";
..        prtpage prfile;*pcolumn4:7775,"(510) 302-4643";
..        prtpage prfile;*pcolumn5:7775,"TamiBarocio@nincal.com";
..        prtpage prfile;*pcolumn1:8000,*font=font5,"•"
..        prtpage prfile;*pcolumn2:8000,*font=font6,"Jane Nagatoshi";
..        prtpage prfile;*pcolumn4:8000,"(510) 302-4641";
..        prtpage prfile;*pcolumn5:8000,"JaneNagatoshi@nincal.com";
...
..        prtpage prfile;*pcolumn:8350,*font=font5,"For Recommendations...";
..        prtpage prfile;*pcolumn1:8700,"•"
..        prtpage prfile;*pcolumn2:8700,*font=font6,"Bonnie Olson";
..        prtpage prfile;*pcolumn4:8700,"(510) 302-4640";
..        prtpage prfile;*pcolumn5:8700,"BonnieOlson@nincal.com";
..END PATCH 1.11 REPLACED LOGIC
.......................................................
..START PATCH 1.12 REPLACED LOGIC
..        prtpage prfile;*pcolumn:6725,*font=font5,"For Data Cards, Counts & Clearances...";
..        prtpage prfile;*pcolumn1:7075,"•"
..        prtpage prfile;*pcolumn2:7075,*font=font6,"Kathy Obee";
..        prtpage prfile;*pcolumn4:7075,"(510) 302-4645";
..        prtpage prfile;*pcolumn5:7075,"KathyObee@nincal.com";
...
..        prtpage prfile;*pcolumn:7425,*font=font5,"For Orders...";
..        prtpage prfile;*pcolumn1:7775,"•"
..        prtpage prfile;*pcolumn2:7775,*font=font6,"Agnes Alvarez";
..        prtpage prfile;*pcolumn4:7775,"(510) 302-4646";
..        prtpage prfile;*pcolumn5:7775,"AgnesAlvarez@nincal.com";
..        prtpage prfile;*pcolumn1:8000,*font=font5,"•"
..        prtpage prfile;*pcolumn2:8000,*font=font6,"Jane Nagatoshi";
..        prtpage prfile;*pcolumn4:8000,"(510) 302-4641";
..        prtpage prfile;*pcolumn5:8000,"JaneNagatoshi@nincal.com";
...
..        prtpage prfile;*pcolumn:8350,*font=font5,"For Recommendations...";
..        prtpage prfile;*pcolumn1:8700,"•"
..        prtpage prfile;*pcolumn2:8700,*font=font6,"Bonnie Olson";
..        prtpage prfile;*pcolumn4:8700,"(510) 302-4640";
..        prtpage prfile;*pcolumn5:8700,"BonnieOlson@nincal.com";
..END PATCH 1.12 REPLACED LOGIC
.......................................................
..START PATCH 1.13 REPLACED LOGIC - Removing BLO and "for orders..." message
..prtpage prfile;*pcolumn:7000,*font=font5,"For Orders...";.
.         prtpage prfile;*pcolumn1:7350,"•"
.         prtpage prfile;*pcolumn2:7350,*font=font6,"Agnes Alvarez";
.         prtpage prfile;*pcolumn4:7350,"(510) 302-4646";
.         prtpage prfile;*pcolumn5:7350,"AgnesAlvarez@nincal.com";
..START PATCH 1.14 ADDED LOGIC - Adding BC "for orders..." message
.         prtpage prfile;*pcolumn1:7575,*font=font5,"•"
.         prtpage prfile;*pcolumn2:7575,*font=font6,"Becky Chavez";
.         prtpage prfile;*pcolumn4:7575,"(510) 302-4641";
.         prtpage prfile;*pcolumn5:7575,"BeckyChavez@nincal.com";
..END PATCH 1.14 ADDED LOGIC - Added BC "for orders..." message
.         prtpage prfile;*pcolumn1:7800,*font=font5,"•"
.         prtpage prfile;*pcolumn2:7800,*font=font6,"Joey Gamache";
.         prtpage prfile;*pcolumn4:7800,"(510) 302-4644";
.         prtpage prfile;*pcolumn5:7800,"JoeyGamache@nincal.com";
..        prtpage prfile;*pcolumn1:8025,"•"
..        prtpage prfile;*pcolumn2:8025,*font=font6,"Kathy Obee";
..        prtpage prfile;*pcolumn4:8025,"(510) 302-4645";
..        prtpage prfile;*pcolumn5:8025,"KathyObee@nincal.com";
.. 
..        prtpage prfile;*pcolumn:8150,*font=font5,"For Recommendations...";
..        prtpage prfile;*pcolumn1:8375,"•"
..        prtpage prfile;*pcolumn2:8375,*font=font6,"Bonnie Olson";
..        prtpage prfile;*pcolumn4:8375,"(510) 302-4640";
..        prtpage prfile;*pcolumn5:8375,"BonnieOlson@nincal.com";
..END PATCH 1.12 REPLACED LOGIC
..END PATCH 1.13 REPLACED LOGIC - Removed BLO and "for orders..." message
..START PATCH 1.15 ADDED LOGIC
.         prtpage prfile;*pcolumn1:8475,*font=font5,"•"
.         prtpage prfile;*pcolumn2:8475,*font=font6,"List Management Fax";
.         prtpage prfile;*pcolumn4:8475,"(510) 628-8313";
..END PATCH 1.15 ADDED LOGIC
..............................................................
          prtpage prfile;*pcolumn3:2000,*font=font1,"LIST NEWS";
.
          prtpage prfile;*pcolumn:3000,*font=font2,"To:   ",DimPtr;
.FOR TESTING PURPOSES ONLY!!
.         prtpage prfile;*pcolumn:3000,*font=font2,"To:  ";
.
          prtpage prfile;*pcolumn:3450,*font=font3,"•";
          prtpage prfile;*pcolumn1:3450,"Freshly Updated Data Cards";
          prtpage prfile;*pcolumn:3750,"•";
          prtpage prfile;*pcolumn1:3750,"New Exclusives";
          prtpage prfile;*pcolumn:4050,"•";
          prtpage prfile;*pcolumn1:4050,"New Counts & Pricing";
.
          prtpage prfile;*pcolumn:5000,*font=font4,"Contact our list experts to get the hottest new names on the market!";
.
          prtpage prfile;*pcolumn:5625,*font=font5,"ORDERS"
          prtpage prfile;*pcolumn:6000,*font=font6,"Becky Chavez"
          prtpage prfile;*pcolumn2:6000,"510-302-4641"
          prtpage prfile;*pcolumn4:6000,"BeckyChavez@NamesintheNews.com"
          prtpage prfile;*pcolumn:6200,"Agnes Alvarez"
          prtpage prfile;*pcolumn2:6200,"510-302-4646"
          prtpage prfile;*pcolumn4:6200,"AgnesAlvarez@NamesintheNews.com"
.
          prtpage prfile;*pcolumn:6770,*font=font5,"CLEARANCES, COUNTS, SHIPPING"
          prtpage prfile;*pcolumn:7145,*font=font6,"Joey Gamache"
          prtpage prfile;*pcolumn2:7145,"510-302-4644"
          prtpage prfile;*pcolumn4:7145,"JoeyGamache@NamesintheNews.com"
          prtpage prfile;*pcolumn:7345,"Joseph Cabral"
          prtpage prfile;*pcolumn2:7345,"510-302-4643"
          prtpage prfile;*pcolumn4:7345,"JosephCabral@NamesintheNews.com"
.
          prtpage prfile;*pcolumn:7915,*font=font5,"LIST RECOMMENDATIONS"
.         prtpage prfile;*pcolumn:8290,*font=font6,"Steve Kehrli"
.         prtpage prfile;*pcolumn2:8290,"510-302-4640"
.         prtpage prfile;*pcolumn4:8290,"SteveKehrli@NamesintheNews.com"
          prtpage prfile;*pcolumn:8290,*font=font6,"Katinka Partridge"
          prtpage prfile;*pcolumn2:8290,"510-302-4632"
          prtpage prfile;*pcolumn4:8290,"KatinkaPartridge@NamesintheNews.com"
.
          prtpage prfile;*pcolumn:10100,*font=font7,"To have your name removed from our fax promotion list please call Katinka Partridge at 415-989-3350, or send an email to"
          prtpage prfile;*pcolumn:10225,"KatinkaPartridge@NamesintheNews.com"
.END PATCH 1.16 REPLACED LOGIC
        return

trapit
        return
.Include IO file
        include comlogic.inc
