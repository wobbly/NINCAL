*******************************************************************************
*                                                                             *     
*     Program Name         : ProBar.Rtn Version 1.0                             *     
*                                                                             *     
*     Type of program      : Simple progress bar                              *     
*                                                                             *     
*     Compilation          : Compile as a ROUTINE                             *     
*                                                                             *     
*     Author               : Festus Redelinghuys - plbsa@icon.co.za           *
*                                                                             *     
*     Date                 : 12 November 1998                                 *     
*                                                                             *     
*     Copyright            : All rights reserved                              *     
*                                                                             *     
*******************************************************************************

.   This is a simplex box with a progress bar that displays progress.
.   This routine automatically converts a value within a range to a %
.   'LoRange'  = Lower value of range  - Normally zero up to    9,999,998
.   'CurValue' = Current value within the range set by LoRange and HiRange
.   'HiRange'  = Higher value of range - Anything between 1 and 9,999,999
.   'Text'     = is the text that is displayed for the progress bar
.   'Title'    = is the title on the window
.   'Show'     = controls whether the box is displayed or not. 0=Turned Off
.                                                              1=Turned On
.   Any of these values can be changed at anytime but will only be reflected
.   when an update call is made.
.   Place the update in a close loop for frequent updates

ProWindow       Window
ProBar          Progress
ProText         StatText
Color           Color

ShowStatus      Form            "0"     Current display status. 0=Off,1=On
BarValue        Form            3       % value of progress bar
Calc1           Form            16.10   Work variable 1
Calc2           Form            16.10   Work variable 2
Calc3           Form            16.10   Work variable 3
Prev            Form            16.10

LoRange         Form            ^       Lowest possible value ( NOT % )
CurValue        Form            ^       Progress bar value between Lo and High
HiRange         Form            ^       Highest value, NOT %
Text            Dim             ^       Text to be displayed above progress bar
Show            Form            ^       Turn display on/off. 0=Off,1=On
Title           Dim             ^       Title to be display on window

...............................................................................
.   The ONLY program entry point

Update          ROUTINE         LoRange,CurValue,HiRange,Text,Show,Title
                
                SetMode         *Pixel=On
                If              ( Show = 1 & ShowStatus = 0 )  Turn On
                 Call           DisplayOn
                Else IF         ( Show = 0 & ShowStatus = 1 )  Turn Off
                 Call           DisplayOff
                Else IF         ( Show = 1 & ShowStatus = 1 )  Update Bar
                 Call           DisplayUpdate
                Endif
                EventCheck
                RETURN

.   Update the progress bar % fill
.   
DisplayUpdate
                Return If       ( LoRange >= HiRange )         Illegal range?
                Return If       ( CurValue > HiRange | CurValue < LoRange )
                Sub             LoRange,HiRange,Calc1          Get range
                Div             CurValue,Calc1,Calc3
                Div             Calc3,"100",Calc1              Calc Bar %
                Move            Calc1,BarValue                 Round off
                IF              ((BarValue-Prev)>=1|(BarValue-Prev)<=-1|BarValue=100|BarValue=0)
                 Setitem        ProBar,0,BarValue              Update progress bar display
                 Setitem        ProText,0,Text                 Change the display text
                 Move           BarValue,Prev                  Remember last %
                Endif
                Return                                         Done

.   Turn on the progress bar.

DisplayOn
                Create          ProWindow=201:270:156:494:     Create window
                                BgColor=2147483663:
                                Enabled=1:
                                FgColor=2147483666:
                                ObjectId=0:
                                Title=Title:
                                MinBox=0:
                                MaxBox=0:
                                SysMenu=0:
                                Caption=1:
                                ClipCtrl=0:
                                Appearance=1:
                                WinType=4:
                                AutoRedraw=1:
                                ScrollBar=3:
                                Font="'>MS Sans Serif'(8)":
                                GridAlign=0:
                                GridSizeH=10:
                                GridSizeV=10:
                                Units=5
                Create          Color=*BLUE
                Create          ProWindow;ProBar=40:60:10:330: Create Progress bar
                                BgColor=16777215:
                                Style=1:
                                DropId=0:
                                Enabled=1:
                                FgColor=Color:
                                ObjectId=0:
                                ZOrder=40:
                                Percent=1

                Create          ProWindow;ProText=10:30:10:330,Text,"'>MS Sans Serif'(8)":
                                Alignment=2:
                                BgColor=2147483663:
                                Border=0:
                                BdrColor=0:
                                Style=1:
                                DropId=0:
                                Enabled=1:
                                FgColor=2147483666:
                                ObjectId=0:
                                ZOrder=50:
                                BackStyle=1:
                                UseAltKey=1

                Activate        ProBar
                Activate        ProText
                Activate        ProWindow
                Move            "1",ShowStatus
                Call            DisplayUpdate
                RETURN

.   Turn OFF the progress bar

DisplayOff
                Destroy         ProBar
                Destroy         ProText
                Destroy         ProWindow
                Move            "0",ShowStatus
                RETURN
