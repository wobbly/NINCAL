PC EQU 0
          include common.inc
          include cons.inc
TeamPtr form  ^
RepPtr1 form  ^
RPTR1   form  ^
.patch1.2
PDFPTR  DIM   ^
FISCPTR FORM  ^
.patch1.2
.patch1.3
DatePTR FORM  ^
.patch1.3
.Patch5.05
OPTPTR  DIM   45
.Patch5.05
.For nord0006  
.Reporter    plform  nord006b
Radios          radio  (14)
Button          button (2)
ComboBoxes      combobox 
StatTextBoxes   stattext (2)
CheckBoxes      CheckBox 
ObjectColl1     collection
edittextboxes   edittext
.OrderCreateCampaign external "NCMP0002;CreateCampaign"
.call      OrderCreateCampaign using NCMPFLD,Order6ComboShip,userlogn
.............................................................................
OrderPickGetCriteria Routine RPTR1,RepPTR1,TeamPtr,PDFPTR,FISCPTR,Dateptr
Reporter plform nord006b
          formload reporter
.......................................................................................................................................          
Release  init "1.4"   DLH Y report cleanup
Reldate   Init      "2013 May 2"
.Release  init "1.3"   04/28/03 DMB Added order/mail date radios, created click events for fiscal month mlr\listsum
.Reldate   Init      "2003 April 28"
.Release  init "1.2"   03/13/03 DMB Added fiscal month edit text box and pdf option for mailer summary
.Release  init "1.1"  03/11/03 DMB Added optioin for download only for Y report

.Release  init "1.0"  07/29/02 DMB External for Nord0006 to allow criteria to be selected before pick begins
.......................................................................................................................................
.RepPtr1 - Which Report
.TeamPtr which Team if applicable

.......................................................................................................................................
.Yreport or Mailer List Summary
.......................................................................................................................................
        move RPTR1 to n5
        Branch  N5 to YREPORT,SUMMARY

YREPORT
        call    ReporterDestroyObjects
        setprop Reporter,title="Y Report Criteria"
        create  nord006b;Radios(1)=50:65:10:180,"PDF In house copy",groupid=10,objectid=1
        create  nord006b;Radios(14)=70:85:10:180,"Email .csv",groupid=10,objectid=10
        create  nord006b;Radios(2)=90:105:10:180,"Email .csv & PDF",groupid=10,objectid=2
        create  nord006b;Radios(3)=110:125:10:180,"Select",groupid=10,objectid=3
        create  nord006b;Radios(4)=130:145:10:180,"Alpha Client",groupid=10,objectid=4
        create  nord006b;Radios(5)=150:165:10:180,"NWF Select .csv",groupid=10,objectid=5
        create  nord006b;Radios(9)=170:185:10:180,"Y + NWF .csv",groupid=10,objectid=9
.Client Section
        create  nord006b;Radios(6)=50:80:300:450,"Pdf Client Copy ",groupid=10,objectid=6
        create  nord006b;Radios(7)=85:100:300:400,"Client Select",groupid=10,objectid=7
        create  nord006b;Radios(8)=105:115:300:450,"Client Alpha Client",groupid=10,objectid=8

.Download Folder
        create  nord006b;StatTextBoxes(1)=225:240:10:180,"Sales Folder","'>MS Sans Serif'(8)",ToolTip="Which Team do you Belong?"
        create  nord006b;ComboBoxes=245:260:10:180,"",";S)usan;J)eanette;S)uzie"
        activate Radios(1),CheckAble,result
        activate Radios(2),CheckAble,result
        activate Radios(3),CheckAble,result
        activate Radios(4),CheckAble,result
        activate Radios(5),CheckAble,result
        activate Radios(6),CheckAble,result
        activate Radios(7),CheckAble,result
        activate Radios(8),CheckAble,result
        activate Radios(9),CheckAble,result
        activate Radios(14),CheckAble,result
        activate ComboBoxes
        activate stattextboxes(1) 
        setitem Radios(1),n1,c1
        goto LoadButtons
Summary
        call ReporterDestroyObjects
        setprop Reporter,title="5 Year Mailer/List Usage Report Criteria"
        create  nord006b;Radios(10)=120:135:10:120,"Calendar",groupid=10,objectid=1
        create  nord006b;Radios(11)=140:155:10:120,"Fiscal Year",groupid=10,objectid=2
 
.        create  nord006b;Radios(12)=160:175:10:220,"123 Calendar - Does Anyone use this??",groupid=10,objectid=3
.        create  nord006b;Radios(13)=180:195:10:220,"123 Fiscal - Does Anyone use this??",groupid=10,objectid=4
.patch1.3
        activate Radios(10),selected,result
        activate Radios(11),selected,result
.patch1.3
.        activate Radios(12)
.        activate Radios(13)
        setitem Radios(10),n1,c1
.patch1.2
        create  nord006b;CheckBoxes=120:135:300:395,"PDF",ALIGNTEXT=1
        create  nord006b;StatTextBoxes(2)=140:155:300:370,"Fiscal Month","'>MS Sans Serif'(8)"
        create  nord006b;EditTextBoxes=140:160:370:395,MaxChars=2,EditType=2,SelectAll=1,Style=1,Border=1
        activate checkBoxes
        activate StatTextBoxes(2)
        activate edittextboxes
.patch1.2
.patch1.3
        create  nord006b;Radios(12)=120:135:130:280,"Order Date",groupid=20,objectid=2
        create  nord006b;Radios(13)=140:155:130:280,"Mail Date",groupid=20,objectid=1
        activate Radios(12)
        activate Radios(13)
.        setprop radios(12),enabled=c0
.        setprop radios(13),enabled=c0
        setitem Radios(13),n1,c1
        setitem EditTextBoxes,c0,""
        setprop EditTextBoxes,enabled=c0
.patch1.3
LoadButtons
        create  nord006b;Button(1)=270:290:10:100,"&Cancel"
        create  nord006b;Button(2)=270:290:380:470,"&OK",default=1

        activate Button(1),Cancelrep,result
        activate Button(2),GetRepInfo,result


        listins  ObjectColl1,StatTextBoxes,ComboBoxes,Radios(1),Radios(2):
          Radios(3),Radios(4),Radios(5),Radios(6),Radios(7),Radios(8),Radios(9),Radios(10):
          Radios(11),Radios(12),Radios(13),Radios(14)
        setprop  Reporter,visible=1


          loop 
          waitevent 
          repeat


ReporterDestroyObjects
        destroy ObjectColl1
        return
CheckAble
.Click Event for Radio for Y summary to disable team download
          getprop Radios(1),SELGROUPID=n3
                    if (((n3 = "6")|(n3 = "8"))|(n3 ="1"))
                    setprop comboboxes,enabled=c0
                    return
          else
                    setprop comboboxes,enabled=c1
                    return
          endif
.patch1.3
Selected
.Click Event for Radio for MLR/LST Summary to disable fiscal year
          getprop Radios(10),SELGROUPID=n3
                    if (n3 = "1")
                                        setitem EditTextBoxes,c0,""
                    setprop EditTextBoxes,enabled=c0
                    return
          else if(n3="2")
                                        setitem EditTextBoxes,c0,""
                    setprop EditTextBoxes,enabled=c1
                    return
                    else
                                        return
          endif
.patch1.3
GetRepInfo

          
          if (RPTR1 = 1)
            getprop Radios(1),SELGROUPID=n3
            move n3 to REPPTR1
                      if (((n3 = "6")|(n3 = "8"))|(n3 ="1"))
                    setitem comboboxes,n2,c0
            endif
            getitem comboboxes,n2,n1
            move n1 to TEAMPTR
                    else
.MlrSummary
            getprop Radios(10),SELGROUPID=n3
            move n3 to REPPTR1
.patch1.2
                      getitem checkboxes,0,n1         .;pdf?
                      if (n1 = c1)
                              move YES to PDFPTR
                      else 
                              move NO to PDFPTR
                      endif
.patch1.3
            getprop Radios(12),SELGROUPID=DATEPTR
.patch1.3
                      if (repptr1 = c2)
                              getitem edittextboxes,0,str2                ;fiscal month
                              move str2 to n2
                              if ((n2 > c0)&(n2<c13))
                                      rep zfill,str2
                                      move str2,n2
                                      move n2 to fiscptr
                              else
                                      alert caution,"Need a valid Fiscal Year",result
                                      return
                              endif
                       else
                                move c0 to fiscptr
                     endif
.patch1.2                      
          endif
                    setprop  Reporter,visible=0
          noreturn
          return
Cancelrep
          move c0 to RPTR1
                    setprop  Reporter,visible=0
          noreturn
          return

          include comlogic.inc
