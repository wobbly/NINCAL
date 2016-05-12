RootXML             XFILE
CustomXML           XFILE
ListingXML          XFILE
mgrNameXML          XFILE
AddrXML             XFILE
phoneXML  XFILE
phoneFormattedXML XFILE
.
smidID    DIM       20
smidUnitId DIM      20
listType DIM        20
titlename dim       20
.
mgrName   dim 40
mgr_smidID          DIM       20
mgr_smidUnitId      DIM       20
addrType  DIM       20
addrLine3 DIM       50
addrCity  DIM       50
addrState DIM       2
addrZip             DIM       5
.
phoneType DIM       20
phoneAreaCode       DIM       3
phoneExchange       DIM       3
phoneSuffix         DIM       4
phoneExt  DIM       10
.
.
Reply     DIM       1
Seq       FORM      "-1"
*
.Open the file
.     
          OPEN      RootXML,"\\nins1\e\DATA\nEWScALIFORNIAdmls.xml"
*
.Retrieve the listing record
.         
          READ      RootXML,Seq;listing=ListingXML
          CALL      Error if over
*
.Retrieve the listing info
.         
          READ      ListingXML,Seq;$smidID=smidID,$smidUnitID=smidUnitId,$listType=listType:
                    titleName=titleName,mgrName=mgrNameXML,addr=addrXML
          CALL      Error if OVER
*
.Mgr Record
.         
          READ      mgrNameXML,Seq;$smidID=mgr_SmidID,$smidUnitID=mgr_smidUnitId,mgrName
          CALL      Error if OVER
*
.Addr Record
.
          READ      addrXML,Seq;addrType=addrType,addrLine3=addrLine3,addrCity=addrCity:
                     addrState=addrState,addrZip=addrZip,phone=phoneXML
          CALL      Error if over                 
*
.Show the info
.     
          DISPLAY   "smidId: ",smidID,*N,"smidUnitID: ",smidUnitId,*N,"listType=",listType:
                    *N,"titleName: ",titleName,*N,"mgrName: ",mgrName:
                    *N,"mgr_smidID: ",mgr_smidID,*N,"mgr_smidUnitId: ",mgr_smidUnitId:
                    *N,"addrType: ",addrType,*N,"addrLine3: ",addrLine3,*N,"addrCity: ",AddrCity:
                    *N,"addrState: ",addrState,*N,"addrZip: ",addrZip
*
.Loop through the phone records
.     
          LOOP
          READ      phoneXML,Seq;phoneType=phoneType,phoneFormatted=phoneFormattedXML
          UNTIL     OVER
.         
          READ      phoneFormattedXML,Seq;phoneAreaCode=phoneAreaCode,phoneExchange=phoneExchange:
                    phoneSuffix=phoneSuffix,phoneExt=phoneExt
.                   
          DISPLAY   "phone type: ",PhoneType,*N,"phoneAreaCode: ",phoneAreaCode:
                    *N,"phoneExchange: ",phoneExchange,*N,"phoneSuffix: ",phoneSuffix:
                    *N,"phoneExt: ",phoneExt
          REPEAT              
.
          KEYIN     Reply
          STOP
.
Error     
          KEYIN     "Read failed. ",Reply
          STOP
