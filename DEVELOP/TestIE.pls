..............................................................................
.1 TestIE - TEST INTERNET EXPLORER
.2 
.2 Creates a control that uses the internet explorer.
.2 This program creates the control on the main window.
.2
.2 I am not sure if this will work on all systems.
.2 I went into the Designer and add a "Microsoft Internet Controls".
.2 Then I exported the code to a file (Right click on the Form and select
.2 Export Code.)
.2 The Export Code file (TestIE.EXP) showed that "Microsoft Internet Control" 
.2 was using Class "{8856F961-340A-11D0-A96B-00C04FD705A2}"
.2 I brought up regedit and did a search on 8856F961-340A-11D0-A96B-00C04FD705A2.
.2 Its ProgID was Shell.Explorer
.2 The 8856F961-340A-11D0-A96B-00C04FD705A2 and Shell.Explorer both worked but
.2 I prefer to use the descriptive Shell.Explorer.
.2
.2 Then I added a Edittext and Button.
.2 Enter the GIF file location in the Edittext then press the Go Button.
.2 You can also enter a website like www.yahoo.com and it will go there too.
.2 Have some fun.
.2
.2 Drop me a line if you figure out how to get rid of the vertical scoll bar.
.2 I looked at the properties real quick but did not see anything to turn
.2 them off. The SetWindowLong API might be able to do it if all else fails.
.2 Contact me if you need some help with the SetWindowLong API.
.3 
.3 Files                                                 Read         Updated
.4 
.5 06/22/01 GW Wrote Program
..............................................................................
..         INC       CCCOMMON.INC
..         INC       PRGMISC.INC
         INC       PLBEQU.INC
.
CCMENU   DIM       %12   .Return to menu name
.
..ctlIE CONTROL Class="{8856F961-340A-11D0-A96B-00C04FD705A2}"
..ctlIE CONTROL Class="Shell.Explorer"
ctlIE CONTROL
.
lblURL STATTEXT
txtURL EDITTEXT
strURL DIM 300
btnGO BUTTON
.
..$BTNTEXT              DEFINE  2147483666    ;Hex value 0x80000012
.
         DISPLAY   *ES
         TRAP      CHAINOUT IF INT
         SETMODE   *PIXEL=ON
.
 	CREATE    btnGO=3:24:3:40,"&Go"
 	ACTIVATE  btnGO
         SETPROP   btnGO,DEFAULT=$On
.
 	CREATE    txtURL=3:24:44:600,FGCOLOR=$BTNTEXT
 	ACTIVATE  txtURL
.
 	CREATE    ctlIE=28:400:3:600,Class="Shell.Explorer"
 	ACTIVATE  ctlIE
.
..         SETPROP   ctlIE,*AddressBar=0     .Did not work
..         SETPROP   ctlIE,*StatusBar=$ON
.
         EVENTREG  btnGO,$Click,btnGOClick
.
..         MOVE      "http://www.yahoo.com/",strURL    .http worked
         MOVE      "c:\work\drew.gif",strURL
         SETITEM   txtURL,0,strURL
         SETFOCUS  txtURL
..         DISPLAY   *N,"strURL=",strURL
.
         LOOP
           EVENTWAIT
         REPEAT
.
btnGOClick
         GETITEM   txtURL,0,strURL
         ctlIE.Navigate USING *URL=strURL
         SETFOCUS  txtURL
         RETURN
.
CHAINOUT MATCH     " ",CCMENU
         STOP      IF EOS
         CHAIN     CCMENU
.
.
.
..         INC       ENTRYEDT.INC
..         INC       ENTRYED3.INC
..         INC       CCV1SQN2.INC
..         INC       CCCOMCAL.INC
.
.
