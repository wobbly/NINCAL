////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                                //
//      Developed For:  Names in the News                                                         //
//       Developed By:  The Adjacency Group - 316.284.9330                                        //
//                                                                                                //
////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                                //
//  PROGRAM:    DBFCONVERT.PLS                                                                    //
//                                                                                                //
//   AUTHOR:    Brian Jackson                                                                     //
//                                                                                                //
//     DATE:    02 April 2007                                                                     //
//                                                                                                //
//  PURPOSE:    Utility program for converting a directory of DBF files to flat ASCII files.      //
//               This utility also generates *.IO files with the file layout and a FILE           //
//               declaration for use in PLB programs.                                             //
//                                                                                                //
// REVISION:    Ver  Date       Who!  Details                                                     //
//                                                                                                //
//              1.0  04/02/07   bjj    Initial Version                                            //
////////////////////////////////////////////////////////////////////////////////////////////////////
curPath        dim            260
filename       dim            260

seq            form           "-1"
eof            form           "-3"
fileCount      form           10.4
currentCount   form           10.4
percent        form           10.4
lp             form           10

mainForm       plform         "dbfconvert.plf"
waitForm       plform         "dbfconvert2.plf"

headerRec      record
version        integer        1   //000
YYMMDD         dim            3   //001-003
recCount       integer        4   //004-007
headerLen      integer        2   //008-009
recLength      integer        2   //010-011
filler         dim            20  //012-031
               recordend

fieldRec       record
name           dim            11  //000-010
type           dim            1   //011
reserved       dim            4   //012-015
length         integer        1   //016
reserved2      dim            15  //017-031
               recordend
////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////
start
               winhide
               formload       mainForm
               formload       waitForm

               mainWinLV.InsertColumnEx using *Index=0,*Text="DBF File Name",*Width=200
               mainWinLV.InsertColumnEx using *Index=1,*Text="Record Count",*Width=200,*Format=3
               mainWinLV.InsertColumnEx using *Index=2,*Text="Status",*Width=200

               setprop        mainWinInputDirET,text=curPath
               setprop        mainWinOutputDirET,text=curPath
               setprop        mainWinConvertBT,enabled=0
               
               setprop        mainWin,visible=1
               setfocus       mainWin
               loop
                 waitevent
               repeat
////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////
onCloseMainWin

               call           onClickMainWinCancelBT
               return
////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////
onClickMainWinInputBrowseBT

               mainWin.BrowseForFolder giving curPath using "Select Location of Source DBF Files...":
                              0,33

               return if ( curPath = "" )

               setprop        mainWinInputDirET,text=curPath
               call           getDirContents
               getprop        mainWinOutputDirET,text=curPath
               if ( curPath != "" )
                 setprop        mainWinConvertBT,enabled=1
               endif
               return
////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////
onClickMainWinOutputBrowseBT

               mainWin.BrowseForFolder giving curPath using "Select Location of Destination PLB Files...":
                              0,33

               return if ( curPath = "" )

               setprop        mainWinOutputDirET,text=curPath
               getprop        mainWinInputDirET,text=curPath
               if ( curPath != "" )
                 setprop        mainWinConvertBT,enabled=1
               endif
               return
////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////
onClickMainWinCancelBT lfunction
               entry
alertResult    integer        4

               alert          plain,"Exit DBF converter?",alertResult,"Confirm"
               return         if ( alertResult != 1 )
               stop
               functionend
////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////
onClickMainWinConvertBT lfunction
               entry
workDir        dim            260
fileName       dim            260
shortFN        dim            260
outputPath     dim            260
fileCount      form           4
index          form           4

               setmode        *mcursor=*wait
               setprop        mainWin,enabled=0

               getprop        mainWinInputDirET,text=workDir
               endset         workDir
               if ( workDir != "\" )
                 append         "\" to workDir
               endif
               reset          workDir

               getprop        mainWinOutputDirET,text=outputPath
               endset         outputPath
               if ( outputPath != "\" )
                 append         "\" to outputPath
               endif
               reset          outputPath

               mainWinLV.GetItemCount giving fileCount
               
               for            index from "0" to ( fileCount - 1 )
                 mainWinLV.GetItemText giving shortFN using *Index=index
                 pack           fileName from workDir,shortFN
                 mainWinLV.SetItemText using *index=index,*Subitem=2,*Text="Processing..."
                 call           convertFile using fileName,outputPath,shortFN
                 mainWinLV.SetItemText using *index=index,*Subitem=2,*Text="Converted"
               repeat

               setprop        mainWinSB.Panels(0),text="Done"

               setmode        *mcursor=*arrow
               setprop        mainWin,enabled=1
               return
               functionend
////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////
getDirContents lfunction
               entry
dirString      dim            260
fullPath       dim            260
cwk10          dim            10
recCount       form           10

               setmode        *mcursor=*wait
               setprop        mainWin,enabled=0
               setprop        waitWinPB,value=0
               setprop        waitWinDBFNameST,text=""
               setprop        waitWin,visible=1
               getprop        mainWinInputDirET,text=curPath
               endset         curPath
               if ( curPath != "\" )
                 append         "\" to curPath
               endif
               reset          curPath
               move           curPath to dirString
               endset         dirString
               append         "*.dbf" to dirString
               reset          dirString

               mainWinLV.DeleteAllItems

               mainWinHiddenDL.ResetContent
               mainWinHiddenDL.Dir giving fileCount using *Filespec=dirString,*Flags=0
               clear          currentCount

               loop
                 mainWinHiddenDL.GetText giving filename using *Index=0
                 until          (fileName="")
                 setprop        waitWinDBFNameST,text=fileName
                 mainWinHiddenDL.DeleteString using *Index=0
                 pack           fullPath from curPath,fileName
                 call           getDBFHeaderInfo giving recCount using fullPath
                 move           recCount to cwk10
                 mainWinLV.InsertItemEx using *Index=999999,*Text=filename,*Subitem1=cwk10:
                              *Subitem2="Ready to Convert"
                 checkevent
                 incr           currentCount
                 calc           percent = currentCount/fileCount*100
                 setprop        waitWinPB,value=percent
               repeat

               setprop        waitWinPB,value=100
               setmode        *mcursor=*arrow
               setprop        waitWin,visible=0
               setprop        mainWin,enabled=1
               setfocus       mainWin
               return
               functionend
////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////
getDBFHeaderInfo lfunction
dbfName        dim            260
               entry
fDBFFile       file

               open           fDBFFile,dbfName,read
               read           fDBFFile,seq;*abson,headerRec;
               close          fDBFFile

               return         using headerRec.recCount
               functionend
////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////
convertFile    lfunction
dbfName        dim            260
outputPath     dim            260
shortFN        dim            260
               entry
nameVar        dim            11
filename       dim            260
displayString  dim            260
cNull          init           0x00
byte           integer        1
fDBFFile       file
fPLBFile       file            
fieldCount     form           5
count          form           5
zero           form           "0"
xFields        xfile
txtRecord      dim            ^
firstRecord    dim            ^
recordCount    form           10
recLength      form           5
cwk5           dim            5
count2         form           5
cwk5a          dim            5
cwk5b          dim            5
dataType       dim            1
decimalPos     form           5
decimalPlaces  form           10
cwk30          dim            30
workVar        dim            260

               // get the filename without the extension
               uppercase      shortFN
               scan           ".DBF" in shortFN
               bump           shortFN by -1
               lenset         shortFN
               reset          shortFN

               // prep the flat ASCII output file
               pack           filename from outputPath,shortFN,".txt"
               erase          filename
               prep           fPLBFile,filename,exclusive

               // create an in-memory XML file for storing the field sizes/types for later use
               //  in creating the PLB source file
               prep           xFields,"","FIELDS",RECORDSET="FIELD"

               open           fDBFFile,dbfName,exclusive
               read           fDBFFile,seq;*abson,headerRec;

               // based on the header length reported in the header record, we calculate the
               //  number of fields in each record, and how many field headers we need to read
               calc           fieldCount = ( headerRec.headerLen / 32 ) - 1
               for            count from "1" to fieldCount
               read           fDBFFile,seq;*abson,fieldRec;
               explode        fieldRec.name with cNull into nameVar
               write          xFields,eof;*ll,$name=nameVar,$type=fieldRec.type,$length=fieldRec.length
               repeat

               // check the next byte after the field records.  If it's not a carriage return
               //  (ASCII 13), then the file format is bad
               read           fDBFFile,seq;*abson,byte;
               if ( byte != 13 )
                 return         using (-1)
               endif

               fposit         fDBFFile,count

               reposit        fDBFFile,headerRec.headerLen
               reposit        xFields,zero

               // make buffer variables exactly the right size for our record length
               dmake          txtRecord,(headerRec.recLength-1)
               dmake          firstRecord,(headerRec.recLength-1)

               // loop through the DBF data and convert it to ASCII
               loop
                 clear          byte
                 read           fDBFFile,seq;*abson,byte;
                 until          over
                 add            "1" to recordCount
                 pack           displayString from "Reading Record: ",recordCount
                 setprop        mainWinSB.Panels(0),text=displayString
                 checkevent
                 read           fDBFFile,seq;*abson,txtRecord;
                 if ( byte = 32 )
                   write          fPLBFile,seq;txtRecord
                 endif
                 // save the first record off, so we can use that in the IO file generation
                 if ( firstRecord = "" )
                   move           txtRecord to firstRecord
                 endif
               repeat
               close          fPLBFile

               // prep the PLB source file
               setprop        mainWinSB.Panels(0),text="Generating IO file"
               pack           filename from outputPath,shortFN,".io"
               erase          filename
               prep           fPLBFile,filename,exclusive

               // write out a PLB source header
               subtract       "1" from headerRec.recLength giving count2
               move           count2 to cwk5
               squeeze        cwk5 into cwk5
               move           "1" to count
               write          fPLBFile,seq;"////////////////////////////////////////////////////////////////////////////////"
               write          fPLBFile,seq;*ll,"// ",shortFN,".IO"
               write          fPLBFile,seq;*ll,"// automatically generated PLB include file for converted"
               write          fPLBFile,seq;*ll,"//  ",shortFN,".DBF file"
               write          fPLBFile,seq;"////////////////////////////////////////////////////////////////////////////////"

               // write a line for declaring a file handle with the correct name and record length
               write          fPLBFile,seq;*ll,"f",shortFN,"File   file          name=#"",shortFN,".txt#",fixed=",cwk5
               write          fPLBFile,seq;""

               // write a line declaring a record set
               write          fPLBFile,seq;*ll,"f",shortFN,"IO     record"

               // loop through the XML file we created during the conversion and generate data types
               loop
                 read           xFields,seq;$name=nameVar,$length=recLength,$type=dataType
                 until          over
                 move           count to cwk5
                 add            recLength to count
                 subtract       "1" from count giving count2
                 move           count2 to cwk5a
                 move           recLength to cwk5b
                 replace        " 0" in cwk5
                 replace        " 0" in cwk5a
                 squeeze        cwk5b into cwk5b
                 if ( dataType = "N" )
                   // FORM data, could either be with or without a decimal point - we determine this
                   //  by looking at the first record we converted
                   move           cwk5 to lp
                   reset          firstRecord to lp
                   setlptr        firstRecord to count2
                   move           firstRecord to workVar
                   setlptr        firstRecord
                   reset          firstRecord
                   whereis        "." in workVar giving decimalPos
                   if ( decimalPos > 0 )
                     calc           decimalPlaces = ( recLength - decimalPos)
                     subtract       "1" from decimalPos
                     pack           cwk30 from decimalPos,".",decimalPlaces
                     squeeze        cwk30 into cwk30
                     move           cwk30 to cwk5b
                   endif
                   write          fPLBFile,seq;nameVar," form          ",cwk5b,"     //",cwk5,"-",cwk5a
                 else
                   // DIM data, just output the field size
                   write          fPLBFile,seq;nameVar," dim           ",cwk5b,"     //",cwk5,"-",cwk5a
                 endif
               repeat

               // close the IO and DBF files, and return
               write          fPLBFile,seq;"              recordend"
               close          fPLBFile
               close          fDBFFile

               return
               functionend
////////////////////////////////////////////////////////////////////////////////////////////////////
