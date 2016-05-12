.------------------------------------------------------------------------------
.     Name: PlbAutomation.inc.pls
. Language: PL/B
.   Author: Brian C. Swing
.Orig.Date: Dec 14, 2000
.  Purpose: include for doing automation
.Revisions:
. 2000.12.22  s.elliott - added status window, local variables,
.                           passed variables @ entrypoint.
. 2001.10.04 bswing - added logic to set the default printer when doing
.                     a mail merge with word.
. 2002.07.21 s.elliott - file_pref to LRSchoice.
. 2002.08.23 s.elliott - changed error message text.
.                       fixed ini_entry pack (use "L", not LRSchoice).
.------------------------------------------------------------------------------
#WordApp AUTOMATION  Class="Word.Application"   // Word object
#Docs     AUTOMATION                     // Word #Document collection object
#Doc      AUTOMATION                     // Word document object
#MyMerge  Automation
#MyDataSource Automation
#firstrec    integer 4,"1"
#lastrec     integer 4,"0xfffffff0"
#destination integer 4,"1"
#state        integer 1
#nstate         form    3               // form version of integer #state      // 01/02/2001
#iONE         INTEGER 1,"1"
#iZERO        INTEGER 1,"0"
.------------------------------------------------------------------------------
#prg_name   dim     50                  // program name                        // 12/22/2000
#doc_name   dim     100                 // merge document name                 // 12/22/2000
#win      window
#msgtxt   stattext
dftPrter  dim  100     // saved variable for default printer
.------------------------------------------------------------------------------
#CopyFileA   Profile kernel32,CopyFileA,Int4,Dim,Dim,Int4
#cfilenames  dim     100                 // source
#cfilename   dim     100                 // destination
#term        INIT    0x0                 // null terminator
#res         integer 4                   // copyfile result
#overwrite   integer 4,"0"               // allow overwrite of existing file
.------------------------------------------------------------------------------
.------------------------------------------------------------------------------
PrintWordAutomation routine #doc_name,#prg_name                                // 12/22/2000
.   *
.   * make little status window
.   *
     create #win=1:50:1:204,caption=0,wintype=2,winpos=2:
            bgcolor=0x8000000F,fgcolor=0x80000012
     create #win;#msgtxt=2:22:2:202,"Preparing to print the documents....":
            ">ARIAL",border=0,bgcolor=0x8000000F,fgcolor=0x80000012
     activate #win
     activate #msgtxt
.   *
.   * copy the Word document to the temporary folder
.   *
    pack    ini_entry with "PLBENV_frmL"    // report forms in Live area only  // 8/23/2002
    clock   ini,ini_entry
    pack    #cfilenames with ini_entry,#doc_name,#term        // source        // 12/22/2000
    pack    #cfilename with "c:\",temp_folder,#doc_name,#term // destination   // 12/22/2000
    winapi #CopyFileA giving #res using #cfilenames,#cfilename,#overwrite
    if (#res = #iZERO)
.        pack work1024 with "Error copying report template:":
.            hex7f,ini_entry,#doc_name:
.            hex7f,"to local computer:":
.            hex7f,"c:\",temp_folder,#doc_name
.        alert stop,work1024,nwork01,#prg_name
        alert stop,"Error copying report template to local computer!":         // 8/23/2002
                    nwork01,#prg_name
        stop
    endif
    TRAP    bad_object IF OBJECT
.   *
.   * Start the Word Application object
.   *
    CREATE  #WordApp
.   *
.   * Hide window & focus passed to hidden window
.   *
    move    "0" to nwork05
    setprop #WordApp,*WindowState=nwork05
.   *
.   * get the printer name that is the report printer.
.   *
    call    RetrievePrinterName using "5",nwork01,work80                       // 10/4/2001
.   * save default printer off
    count   chars in work80                                                    // 10/9/2001
    if not zero                                                                // 10/9/2001
       getprop #WordApp,*activeprinter=dftPrter                                // 10/4/2001
       setprop #WordApp,*activeprinter=work80                                  // 10/4/2001
    endif
.   *
.   * Now get the documents object and create a new object
.   *
    GETPROP   #WordApp,*Documents=#Docs
.   *
.   * Now get the actual word document
.   *
    SETPROP   #WordApp,*LEFT=3000        // display it off the screen
    pack    filename with "c:\",temp_folder,#doc_name                          // 12/22/2000
    #Docs.Open GIVING #Doc USING *Filename=filename
    getprop   #Doc,*mailmerge=#MyMerge
    Getprop   #MyMerge,*datasource=#MyDataSource
.   *
.   * find out the state (Read-only, Long integer) of the Mail Merge object
.   *
.   state 0 = wdNormalDocument
.   state 1 = wdMainDocumentOnly
.   state 2 = wdMainAndDataSource
.   state 3 = wdMainAndHeader
.   state 4 = wdMainAndSourceAndHeader
.   state 5 = wdDataSource
    Getprop   #MyMerge,*state=#state
    move    #state to #nstate
    IF (#state != 2)                    // must be: wdMainAndDataSource
        pack    work80 with "Invalid mail-merge document OR ":
                         "data source file not available (",#nstate,")"
        alert   caution,work80,nwork01,#prg_name
    ELSE
        setitem #msgtxt,0,"Printing...."
        setprop #MyDataSource, *firstrecord=#firstrec, *lastrecord=#lastrec
        setprop #MyMerge, *destination=#destination
        #MyMerge.EXECUTE using #iONE       //(TRUE ..... DISPLAY DIALOG)
    ENDIF
.   *
.   * restore default printer
.   *
    count   chars in work80                                                    // 10/9/2001
    if not zero                                                                // 10/9/2001
       setprop #WordApp,*activeprinter=dftPrter                                // 10/4/2001
    endif
.   *
.   * close the documents object
.   *
    #WordApp.quit  using 0
    Destroy #WordApp
    destroy #msgtxt
    destroy #win
    trapclr object
    return

.------------------------------------------------------------------------------
.------------------------------------------------------------------------------
