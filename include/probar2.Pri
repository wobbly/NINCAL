*******************************************************************************
*                                                                             *     
*     Program Name         : ProBar.Rtn Version 1.0                           *     
*                                                                             *     
*     Type of program      : Simple progress bar for inline use               *     
*                                                                             *     
*     Compilation          : Include into a application                       *     
*                                                                             *     
*     Author               : Festus Redelinghuys - plbsa@icon.co.za           *
*                                                                             *     
*     Date                 : 12 November 1998                                 *     
*                                                                             *     
*     Copyright            : All rights reserved                              *     
*                                                                             *     
*******************************************************************************
                      Setflag   Equal
                      Goto      ProBarJump If Equal   Avoid illegal entry
...............................................................................
          
ProBarLoRange   Form            "0000000"               Lowest possible value ( NOT % )
ProBarCurValue  Form            "0000000"               Progress bar value between Lo and High
ProBarHiRange   Form            "0000100"               Highest value, NOT %
ProBarText      Dim             100                     Text to be displayed above progress bar
ProBarShow      Form            "0"                     Turn display on/off. 0=Off,1=On
ProBarTitle     Dim             100                     Title to be display on window

ProBarUpdate    CallS           "ProBar2.PLC;Update" Using ProBarLoRange: Lowest possible value ( NOT % )
                                ProBarCurValue:         Progress bar value between Lo and High
                                ProBarHiRange:          Highest value, NOT %
                                ProBarText:             Text to be displayed above progress bar
                                ProBarShow:             Turn display on/off. 0=Off,1=On
                                ProBarTitle             Title to be display on window
                RETURN

*******************************************************************************
ProBarJump                                                                    
*                          END OF ProBar.PRI                                  *
*******************************************************************************
