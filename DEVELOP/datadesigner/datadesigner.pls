.------------------------------------------------------------------------------
. datadesigner.pls
.------------------------------------------------------------------------------
.   2001.02.27  s.elliott - created.
.   2001.03.22  s.elliott - added code to extract path from Definition File
.                           Save routine for use in File Layout creation rtn.
.   2001.04.02  s.elliott - added MRU & Notes.
.------------------------------------------------------------------------------
            INCLUDE   \\clybc0701\jis$\Dev\P\Common\common.uda.pls
prg_name    init    "DataDesigner"
.*
.* datadesigner INI file
.*
ini_file    file    fixed=80            // Data Designer INI file
ini_name    init    "datadesigner.ini"
ini_Rec     record
keyword     dim     10
value       dim     70
            recordend
.*
.* data definition file information
.*
dd_file     file    variable            // Data Designer Definition file
dd_rec      record
label       dim     15      //   1-  15
type        dim     4       //  16-  19
size        dim     5       //  20-  24
array       dim     10      //  25-  34
columns     dim     10      //  35-  44
keys        dim     25      //  45-  69
comment     dim     50      //  70- 119
            recordend
.*
.* record layout file information
.*
rl_file     file    variable            // Record Layout file
rldatn      dim     50                  // data file name
rlfold      dim     3                   // folder abbrev. where data file is located
rldesc      dim     100                 // data file description
rlrecn      dim     10                  // data file record name
rlrecl      dim     4                   // data file record length
.*
.* field listview definitions
.*
LV_names    dim     10(0..6),("Label"),("Type"),("Size"),("Array"),("Columns"):
                             ("Keys"),("Comment")
LV_sizes    form    3(0..6),("75"),("50"),("50"),("70"),("80"),("90"),("250")
LV_aligns   integer 4(0..6),("0x0002"),("0x0002"),("0x0001"),("0x0002"),("0x0002"):
                            ("0x0002"),("0x0000")
.LV_aligns   integer 4(0..6),(LVCFMT_CENTER),(LVCFMT_CENTER),(LVCFMT_RIGHT):
.                            (LVCFMT_CENTER),(LVCFMT_CENTER):
.                            (LVCFMT_CENTER),(LVCFMT_LEFT)
LV_subitms  form    " 6"
LV_lbl_SI   form    "0"                 // label subitem #
LV_typ_SI   form    "1"                 // type subitem #
LV_siz_SI   form    "2"                 // size subitem #
LV_arr_SI   form    "3"                 // array subitem #
LV_col_SI   form    "4"                 // column subitem #
LV_key_SI   form    "5"                 // key subitem #
LV_com_SI   form    "6"                 // comment subitem #
.*
.* modification listview definitions
.*
mLV_names    dim     12(0..2),("Date"),("Who"),("Description")
mLV_sizes    form    3(0..2),("75"),("75"),("380")
mLV_aligns   integer 4(0..2),("0x0002"),("0x0002"),("0x0002")
.mLV_aligns   integer 4(0..2),(LVCFMT_CENTER),(LVCFMT_CENTER),(LVCFMT_CENTER)
mLV_subitms  form    " 2"
mLV_dat_SI   form    "0"                 // date subitem #
mLV_who_SI   form    "1"                 // who subitem #
mLV_des_SI   form    "2"                 // desc subitem #
.*
.* listview rows
.*
currndx     form    4                   // current field in listview
othrndx     form    4                   // other field in listview (above or below)
totlndx     form    4                   // total fields in listview
.*
.* size & column information
.*
startcol    form    4                   // starting column of field
endcol      form    4                   // endind column of field
array       form    4(3)                // allow for 3-dimensional arrays
arrayndx    form    1                   // which array dimension are we using?
.*
.* index key specifications
.*
.keysndx     form    2                   // which index dimension are we using?
aamspecs    dim     200                 // up to 20 AAM keys are allowed
aamkeylen   form    5(20)               // key length for up to 20 AAM keys
aamkeyname  dim     32(20)              // aam key field name
aamkeysrch  dim     1(20)               // up to 20 AAM key search flags
aamkeyspecs dim     9(20)               // up to 20 AAM keys
aamndx      form    2                   // which AAM dimension are we using?
aamndxtot   form    2                   // total defined aam keys
ismspecs    dim     50(9)               // up to 50 chars per 9 ISAM indexes
ismkeylen   form    5(9)                // key length for up to 9 ISAM indexes
ismkeyspecs dim     9(9,9)              // up to 9 fields per up to 9 ISAM files
ismkeyname  dim     32(9,9)             // isam key field name
ismndxn     form    1                   // isam index number
ismndxo     form    1                   // field order within isam index#
ismndxtot   form    1                   // total defined isam indexes

.*
.* other information
.*
quote       equate  0x0022              // quote mark
quit        form    1                   // quit the program?
saved       form    1                   // has this dd record been saved?
firsttime   form    1                   // first time through loop?
tmpfile     file    var                 // temporary file variable

dd_form    plform  \\clybc0701\jis$\dev\p\stuart\DataDesigner.plf
ddvu_form  plform  \\clybc0701\jis$\dev\p\stuart\DataDesignerVu.plf
ddmru_form plform  \\clybc0701\jis$\dev\p\stuart\DataDesignerMru.plf
ddnote_form plform  \\clybc0701\jis$\dev\p\stuart\DataDesignerNote.plf

mrufile     dim     1                   // is the file to open from the MRU list?
mrulist     dim     70(5)
.------------------------------------------------------------------------------
    winhide
    formload dd_form
    loop
.        eventcheck
        eventwait
    repeat until (quit)
    stop

.------------------------------------------------------------------------------
initialize
    call init_lview
    disableitem savefile_butt
    disableitem resetfile_butt
    disableitem create_butt
    disableitem view_butt
    call read_ini
    return

.------------------------------------------------------------------------------
savefile_button_clicked
    setitem status_stat,0,"Saving definition file...."
.   *
.   * prepare data definition file
.   *
    getitem file_edit,0,filename
    trap nodeffile if io
    erase filename
    open dd_file,filename,exclusive
    trapclr io
.   *
.   * write data definition information
.   *
    unpack "" into dd_rec
    move "!desc" to dd_rec.label        // data file description
    getitem desc_edit,0,dd_rec.comment
    write dd_file,seq;dd_rec
    unpack "" into dd_rec
    move "!dname" to dd_rec.label       // data file name
    getitem dataname_edit,0,dd_rec.comment
    write dd_file,seq;dd_rec
    unpack "" into dd_rec
    move "!fabbr" to dd_rec.label       // folder abbreviation
    getitem folder_edit,0,dd_rec.comment
    write dd_file,seq;dd_rec
    unpack "" into dd_rec
    move "!rname" to dd_rec.label       // record name
    getitem recordname_edit,0,dd_rec.comment
    write dd_file,seq;dd_rec
    unpack "" into dd_rec
    move "!rsize" to dd_rec.label       // record size
    getitem recordsize_edit,0,dd_rec.comment
    write dd_file,seq;dd_rec
.   *
.   * write modifications
.   *
    modi_lview.GetItemCount GIVING totlndx
    for currndx from "0" to (totlndx - 1)
        call get_lview_modi using currndx
        move "!modi" to dd_rec.label
        write dd_file,seq;dd_rec
    repeat
.   *
.   * write fields
.   *
    fields_lview.GetItemCount GIVING totlndx
    for currndx from "0" to (totlndx - 1)
        call get_lview_fields using currndx
        write dd_file,seq;dd_rec
    repeat
    move "1" to saved
    setitem status_stat,0,"Saved definition file."
    disableitem savefile_butt
    enableitem create_butt
    enableitem view_butt
    enableitem resetfile_butt
.   *
.   * remember path
.   *
    unpack "" into work250              // null pathname
    movelptr filename to ndx2
    for ndx from ndx2 to "1" by "-1"
        cmove filename to ans
        if (ans = "\")                  // pathname ends here
            setlptr filename to ndx
            reset filename
            move filename to work250    // extract path from DDD file.
            break
        endif
    repeat
    if (pathname != work250)            // new pathname
        count chars of work250          // does new pathname have a value?
        if not zero                     // yes
            move work250 to pathname    // move new pathname to pathname
        endif
    endif
    return

nodeffile
    prep dd_file,filename,exclusive
    return

.------------------------------------------------------------------------------
getfile_button_clicked
.   *
.   * select item from MRU list or open new DDD file?
.   *
    formload ddmru_form
    return

getfile_selection_made
.   *
.   * open, or, pick & open, DDD file
.   *
    setitem status_stat,0,"Loading definition file...."
    call save_needed with nwork01       // 1=needed, 0=not needed
    if (not nwork01)
        if (mrufile = "Y")
            setflag not over
        else
            GETFNAME OPEN,"Open Data Designer Definition",fileNAME,PATHname,"ddd"
            pack filename1 with pathname,filename
        endif
        if not over
            trap no_DDD if io
            open dd_file,filename1,exclusive
            trapclr io
            call load_dd_data
            close dd_file
            call mru_put with filename1     // put filename into MRU list
            setitem status_stat,0,"Loaded definition file."
            disableitem savefile_butt
            enableitem create_butt
            enableitem view_butt
            enableitem resetfile_butt
        endif
    endif
    move "N" to mrufile
    return

no_DDD
    noreturn
    pack work80 with "That file couldn't be opened!":
                     hex7f,s$error$
    alert caution,work80,nwork01,prg_name
    return

.------------------------------------------------------------------------------
view_button_clicked                     // view record layout
    formload ddvu_form
    return

.------------------------------------------------------------------------------
create_button_clicked                   // create record layout
    fill " " into work60
    replace " -" in work60
.   *
.   * create record layout file name
.   *
    call prepare_output_files
    pack filename with pathname,rldatn,".rec.pls"
.   *
.   * create record layout file
.   *
    pack work50 with "Creating ",filename," file...."
    setitem status_stat,0,work50
    call check_exist with filename,ans
    return if (ans = "Y")
    erase filename
    prep rl_file,filename
.   *
.   * write data file description header
.   *
    write rl_file,seq;".",work60
    write rl_file,seq;". ",*ll,rldatn," - ",rldesc
    write rl_file,seq;".",work60
.   *
.   * write data file modification header
.   *
    modi_lview.GetItemCount GIVING totlndx
    for ndx from "0" to (totlndx - 1)       // for each modi in listview....
.       *
.       * get information from listivew
.       *
        call get_lview_modi with ndx
        call trim with dd_rec.keys
        write rl_file,seq;". ",dd_rec.columns," ",*ll,dd_rec.keys," - ",dd_rec.comment
    repeat
    write rl_file,seq;".",work60
    write rl_file,seq;""
.   *
.   * write file declarations
.   *
    call get_index_info
.       * write key information
    write rl_file,seq;*ll,rlrecn,"_flist filelist"
....    write rl_file,seq;*ll,rlrecn,"_FL FILE fixed=",rlrecl,",name=filenameS"
    if (aamndxtot > 0)
        write rl_file,seq;*ll,rlrecn,"_AF AFILE fixed=",rlrecl,",name=filename"
    endif
    if (ismndxtot > 0)
        for ismndxn from "1" to ismndxtot
            move ismndxn to nwork01
            write rl_file,seq;*ll,rlrecn,"_I",nwork01," IFILE fixed=",rlrecl,",name=filename",nwork01
        repeat
    endif
    write rl_file,seq;blank15,"filelistend"
    write rl_file,seq;""
.   *
.   * write isam & aam key variables
.   *
    if (ismndxtot > 0)
        for ismndxn from "1" to ismndxtot
.           * build the list of fields for each isam key
            unpack "" into work250
            for ismndxo from "1" to "9"
                count chars in ismkeyname(ismndxn,ismndxo)
                if not zero
                    pack work250 with work250,ismkeyname(ismndxn,ismndxo),"+"
                endif
            repeat
            move "+" to work01
            call remove_trailer with work250,work01
.           * now, write the isam key variables
            move ismndxn to nwork01
            write rl_file,seq;*ll,rlrecn,"_IK",nwork01," dim ",ismkeylen(ismndxn):
                             " // ",work250
            write rl_file,seq;*ll,rlrecn,"_OK",nwork01," dim ",ismkeylen(ismndxn):
                             " // ",work250
        repeat
    endif
    if (aamndxtot > 0)
        write rl_file,seq;""
        write rl_file,seq;*ll,rlrecn,"_akeys list"
        for aamndx from "1" to aamndxtot
            unpack "" into work02,nwork05
            move aamndx to work02
            replace " 0" in work02
            move (aamkeylen(aamndx) + 3) to nwork05
            write rl_file,seq;*ll,rlrecn,"_AK",work02," dim ",nwork05:
                             " // ",work02,aamkeysrch(aamndx)," ",aamkeyname(aamndx)
        repeat
        write rl_file,seq;*ll,blank15,"listend"
    endif
    write rl_file,seq;*ll,""
.   *
.   * write data file name
.   *
    write rl_file,seq;*ll,rlrecn,"_name init ",quote,rldatn,quote
    write rl_file,seq;*ll,rlrecn,"_fold init ",quote,rlfold,quote
    write rl_file,seq;""
    write rl_file,seq;".",work60
.   *
.   * write field names
.   *
    write rl_file,seq;*ll,rlrecn,"_rec record"
    fields_lview.GetItemCount GIVING totlndx
    for ndx from "0" to (totlndx - 1)       // for each field in listview....
.       *
.       * get information from listivew
.       *
        call get_lview_fields with ndx
        call trim with dd_rec.label
        call trim with dd_rec.size
        call trim with dd_rec.comment
        if (dd_rec.label = ".")
            write rl_file,seq;*ll,dd_rec.label,*ll,dd_rec.comment
        else
            call trim with dd_rec.type
            lowercase dd_rec.type
            if (dd_rec.type = "rec")            // record prototype
                write rl_file,seq;dd_rec.label," record like ",dd_rec.array:
                         " // ",dd_rec.columns," ",dd_rec.keys," ",*ll,dd_rec.comment
            else
.                write rl_file,seq;*ll,dd_rec.label," dim ",*ll,dd_rec.size,dd_rec.array:
.                         " // ",dd_rec.columns," ",dd_rec.keys," ",dd_rec.comment
                write rl_file,seq;dd_rec.label," dim ",dd_rec.size,dd_rec.array:
                         " // ",dd_rec.columns," ",dd_rec.keys," ",*ll,dd_rec.comment
            endif
        endif
    repeat
    write rl_file,seq;blank15,"recordend"
    write rl_file,seq;""
.   *
.   * write numeric variables
.   *
    move "1" to firsttime
    for ndx from "0" to (totlndx - 1)       // for each field in listview....
.       *
.       * get information from listivew
.       *
        call get_lview_fields with ndx
        chop dd_rec.comment,dd_rec.comment
        lowercase dd_rec.type
        if (dd_rec.type = "form")
            if (firsttime)
                write rl_file,seq;".",work60
                write rl_file,seq;*ll,rlrecn,"_nlist list"
                move "0" to firsttime
            endif
            write rl_file,seq;*ll,rlrecn,"_n",dd_rec.label," form ",dd_rec.size,dd_rec.array:
                            " // ",dd_rec.columns," ",dd_rec.keys," ",*ll,dd_rec.comment
        endif
    repeat
    if (not firsttime)
        write rl_file,seq;blank15,"listend"
        write rl_file,seq;""
    endif
.   *
.   * write any notes from RTF file
.   *
    call ddn_filename                   // get DDD Notes filename
    trap nonotes if io
    move "0" to badsw
    open tmpfile,filename1
    if (not badsw)
        write rl_file,seq;".",work60
        move "0" to count
        loop
            unpack "" into work250,dim250,work05
            read tmpfile,seq;work250
        until over
            add "1" to count
            if (count > 1)              // skip first line
                if (count = 2)          // skip formatting in line 2
                    scan " " in work250
                    bump work250 by 1
                    move work250 to dim250
                else                    // skip formatting in other lines
                    unpack work250 into work05,dim250
                endif
                chop dim250,dim250
                if (dim250 != "}" and dim250 != "")
                    write rl_file,seq;".",dim250
                endif
            endif
        repeat
        close tmpfile
        write rl_file,seq;""
    endif
.   *
.   * write end-of-file lines
.   *
    write rl_file,seq;".",work60
    write rl_file,seq;". ",blank20,"End of ",*ll,rldatn,".rec.pls"
    write rl_file,seq;".",work60
    close rl_file


.   **
.   ** write the IO file
.   **
    pack filename with pathname,rldatn,".io.pls"
    pack work50 with "Creating ",filename," file...."
    setitem status_stat,0,work50
.   *
.   * create record layout file
.   *
    call check_exist with filename,ans
    return if (ans = "Y")
    erase filename
    prep rl_file,filename
.   *
.   * write data file description header
.   *
    write rl_file,seq;".",work60
    write rl_file,seq;". ",*ll,rldatn," - ",rldesc
    write rl_file,seq;".",work60
    write rl_file,seq;""
.   *
.   * write the OPEN & CLOSE routines
.   *
    write rl_file,seq;*ll,rlrecn,"_OPEN"
....    write rl_file,seq;blank5,"pack filenames with ",quote,"$",quote,",":
....                      *ll,rlrecn,"_fold,file_pref,",quote,"\",quote,",":
....                      *ll,rlrecn,"_name,",quote,".txt|",quote,",FM_Address"
    if (aamndxtot > 0)
        write rl_file,seq;blank5,"pack filename with ",quote,"$",quote,",":
                      *ll,rlrecn,"_fold,file_pref,",quote,"\",quote,",":
                      *ll,rlrecn,"_name,",quote,".aam|",quote,",FM_Address"
    endif
    if (ismndxtot > 0)
        for ismndxn from "1" to ismndxtot
            move ismndxn to nwork01
            write rl_file,seq;blank5,"pack filename",nwork01," with ",quote,"$",quote,",":
                      *ll,rlrecn,"_fold,file_pref,",quote,"\",quote,",":
                      *ll,rlrecn,"_name,",quote,".i0",nwork01,"|",quote,",FM_Address"
        repeat
    endif
    write rl_file,seq;blank5,"open ",*ll,rlrecn,"_flist"
    write rl_file,seq;blank5,"return"
    write rl_file,seq;""
    write rl_file,seq;*ll,rlrecn,"_CLOSE"
    write rl_file,seq;blank5,"close ",*ll,rlrecn,"_flist"
    write rl_file,seq;blank5,"return"
    write rl_file,seq;""
.   *
.   * write the READ routines
.   *
    write rl_file,seq;".",work60
    if (ismndxtot > 0)
        for ismndxn from "1" to ismndxtot
            move ismndxn to nwork01
            write rl_file,seq;*ll,rlrecn,"_RD",nwork01
            write rl_file,seq;blank5,"read ",*ll,rlrecn,"_I",nwork01,",":
                              *ll,rlrecn,"_IK",nwork01,";",*ll,rlrecn,"_rec"
            write rl_file,seq;blank5,"call ",*ll,rlrecn,"_okeys if not over"
            write rl_file,seq;blank5,"return"
            write rl_file,seq;""
            write rl_file,seq;*ll,rlrecn,"_KS",nwork01
            write rl_file,seq;blank5,"readks ",*ll,rlrecn,"_I",nwork01,";":
                             *ll,rlrecn,"_rec"
            write rl_file,seq;blank5,"call ",*ll,rlrecn,"_okeys if not over"
            write rl_file,seq;blank5,"return"
            write rl_file,seq;""
            write rl_file,seq;*ll,rlrecn,"_KP",nwork01
            write rl_file,seq;blank5,"readkp ",*ll,rlrecn,"_I",nwork01,";":
                              *ll,rlrecn,"_rec"
            write rl_file,seq;blank5,"call ",*ll,rlrecn,"_okeys if not over"
            write rl_file,seq;blank5,"return"
            write rl_file,seq;""
        repeat
    endif
    if (aamndxtot > 0)
        write rl_file,seq;*ll,rlrecn,"_ARD"
        write rl_file,seq;blank5,"read ",*ll,rlrecn,"_AF,":
                          *ll,rlrecn,"_AKEYS;",*ll,rlrecn,"_rec"
        write rl_file,seq;blank5,"call ",*ll,rlrecn,"_okeys if not over"
        write rl_file,seq;blank5,"return"
        write rl_file,seq;""
        write rl_file,seq;*ll,rlrecn,"_AKG"
        write rl_file,seq;blank5,"readkg ",*ll,rlrecn,"_AF,":
                          *ll,rlrecn,"_AKEYS;",*ll,rlrecn,"_rec"
        write rl_file,seq;blank5,"call ",*ll,rlrecn,"_okeys if not over"
        write rl_file,seq;blank5,"return"
        write rl_file,seq;""
        write rl_file,seq;*ll,rlrecn,"_AGP"
        write rl_file,seq;blank5,"readKGP ",*ll,rlrecn,"_AF,":
                          *ll,rlrecn,"_AKEYS;",*ll,rlrecn,"_rec"
        write rl_file,seq;blank5,"call ",*ll,rlrecn,"_okeys if not over"
        write rl_file,seq;blank5,"return"
        write rl_file,seq;""
    endif
.   *
.   * write WRITE routine
.   *
    write rl_file,seq;".",work60
    write rl_file,seq;*ll,rlrecn,"_WRT"
    write rl_file,seq;blank5,"move ",quote,"1",quote," to badsw"
    write rl_file,seq;blank5,"call ",*ll,rlrecn,"_ikeys"
    write rl_file,seq;blank5,"filepi 2;",*ll,rlrecn,"_flist"
    write rl_file,seq;blank5,"read ",*ll,rlrecn,"_I1,",*ll,rlrecn,"_IK1;work01"
    write rl_file,seq;blank5,"if over"
    write rl_file,seq;blank10,"write ",*ll,rlrecn,"_flist;",*ll,rlrecn,"_rec"
    write rl_file,seq;blank10,"move ",quote,"0",quote," to badsw"
    write rl_file,seq;blank5,"endif"
    write rl_file,seq;blank5,"filepi 0"
    write rl_file,seq;blank5,"return"
    write rl_file,seq;""
.   *
.   * write UPDATE routine
.   *
    write rl_file,seq;".",work60
    write rl_file,seq;*ll,rlrecn,"_UPD"
    write rl_file,seq;blank5,"move ",quote,"1",quote," to badsw"
    write rl_file,seq;blank5,"call ",*ll,rlrecn,"_ikeys"
    write rl_file,seq;blank5,"filepi 2;",*ll,rlrecn,"_flist"
    write rl_file,seq;blank5,"read ",*ll,rlrecn,"_I1,",*ll,rlrecn,"_IK1;work01"
    write rl_file,seq;blank5,"if not over"
    write rl_file,seq;blank10,"update ",*ll,rlrecn,"_flist;",*ll,rlrecn,"_rec"
    write rl_file,seq;blank10,"move ",quote,"0",quote," to badsw"
    write rl_file,seq;blank5,"endif"
    write rl_file,seq;blank5,"filepi 0"
    write rl_file,seq;blank5,"return"
    write rl_file,seq;""
.   *
.   * write DELETE routine
.   *
    write rl_file,seq;".",work60
    write rl_file,seq;*ll,rlrecn,"_DEL"
    write rl_file,seq;blank5,"move ",quote,"1",quote," to badsw"
    write rl_file,seq;blank5,"call ",*ll,rlrecn,"_ikeys"
    write rl_file,seq;blank5,"filepi 2;",*ll,rlrecn,"_flist"
    write rl_file,seq;blank5,"read ",*ll,rlrecn,"_I1,",*ll,rlrecn,"_IK1;work01"
    write rl_file,seq;blank5,"if not over"
    write rl_file,seq;blank10,"delete ",*ll,rlrecn,"_flist"
    write rl_file,seq;blank10,"move ",quote,"0",quote," to badsw"
    write rl_file,seq;blank5,"endif"
    write rl_file,seq;blank5,"filepi 0"
    write rl_file,seq;blank5,"return"
    write rl_file,seq;""
.   *
.   * write IKEYS & OKEYS & AKEYS routines
.   *
    write rl_file,seq;".",work60
    if (ismndxtot > 0)
        for ndx from "1" to "2"
            if (ndx = 1)
                write rl_file,seq;*ll,rlrecn,"_IKEYS"
            else
                write rl_file,seq;*ll,rlrecn,"_OKEYS"
            endif
            for ismndxn from "1" to ismndxtot
                unpack "" into work250
                for ismndxo from "1" to "9"
                    count chars in ismkeyname(ismndxn,ismndxo)
                    if not zero
                        pack work250 with work250,rlrecn,"_rec.",ismkeyname(ismndxn,ismndxo),","
                    endif
                repeat
                move "," to work01
                call remove_trailer with work250,work01
                move ismndxn to nwork01
                if (ndx = 1)
                    write rl_file,seq;blank5,"packkey ",*ll,rlrecn,"_IK",nwork01," with ":
                                      work250
                else
                    write rl_file,seq;blank5,"packkey ",*ll,rlrecn,"_OK",nwork01," with ":
                                      work250
                endif
            repeat
            write rl_file,seq;blank5,"return"
            write rl_file,seq;""
        repeat
    endif
    if (aamndxtot > 0)
        write rl_file,seq;*ll,rlrecn,"_AKEYS"
        write rl_file,seq;blank5,"unpack ",quote,quote," into ",*ll,rlrecn,"_akeys"
        for aamndx from "1" to aamndxtot
            move aamndx to work02
            replace " 0" in work02
            write rl_file,seq;blank5,"count chars in ",*ll,rlrecn,"_rec.",aamkeyname(aamndx)
            write rl_file,seq;blank5,"if not zero"
            write rl_file,seq;blank10,"setlptr ",*ll,rlrecn,"_rec.",aamkeyname(aamndx)," to chars"
            write rl_file,seq;blank10,"pack ",*ll,rlrecn,"_ak",work02," with ":
                              quote,work02,aamkeysrch(aamndx),quote,",":
                              *ll,rlrecn,"_rec.",aamkeyname(aamndx)
            write rl_file,seq;blank5,"endif"
            write rl_file,seq;""
        repeat
        write rl_file,seq;blank5,"return"
        write rl_file,seq;""
    endif
.   *
.   * write CLEAR & CONVERT routines
.   *
    write rl_file,seq;".",work60
    write rl_file,seq;*ll,rlrecn,"_CLEAR"
    write rl_file,seq;blank5,"unpack ",quote,quote," into ",*ll,rlrecn,"_rec"
    write rl_file,seq;blank5,"return"
    write rl_file,seq;""
    write rl_file,seq;".",work60
    write rl_file,seq;*ll,rlrecn,"_CONVERT"
    for ndx from "0" to (totlndx - 1)       // for each field in listview....
        call get_lview_fields with ndx
        lowercase dd_rec.type
        if (dd_rec.type = "form")
            write rl_file,seq;blank5,"move ",*ll,rlrecn,"_rec.",dd_rec.label," to ",*ll,rlrecn,"_n",dd_rec.label
        endif
    repeat
    write rl_file,seq;blank5,"return"
    write rl_file,seq;""
    write rl_file,seq;".",work60
    write rl_file,seq;". ",blank20,"End of ",*ll,rldatn,".io.pls"
    write rl_file,seq;".",work60
    close rl_file
.   **
.   ** write the BATCH file for indexing
.   **
    pack filename with pathname,rldatn,".bat"
    pack work50 with "Creating ",filename," file...."
    setitem status_stat,0,work50
.   *
.   * create record layout file
.   *
    call check_exist with filename,ans
    return if (ans = "Y")
    erase filename
    prep rl_file,filename
.   *
.   * write data file description header
.   *
    write rl_file,seq;"::",work60
    write rl_file,seq;":: ",*ll,rldatn," - ",rldesc
    write rl_file,seq;"::",work60
    write rl_file,seq;""
    write rl_file,seq;"call reformat ",*ll,rldatn,".txt,",*ll,rldatn,".new -krtzl=",rlrecl
    write rl_file,seq;"rename ",*ll,rldatn,".new,",*ll,rldatn,".txt"
    write rl_file,seq;""
    if (ismndxtot > 0)
        for ismndxn from "1" to ismndxtot
            move ismndxn to nwork01
            write rl_file,seq;"del ",*ll,rldatn,".i0",nwork01
            unpack "" into work250
            for ismndxo from "1" to "9"
                count chars in ismkeyspecs(ismndxn,ismndxo)
                if not zero
                    pack work250 with work250,ismkeyspecs(ismndxn,ismndxo),","
                endif
            repeat
            move "," to work01
            call remove_trailer with work250,work01
            write rl_file,seq;"sunidxnt ",*ll,rldatn,",",*ll,rldatn,".i0",nwork01:
                              ",L",rlrecl," -N,",work250
        repeat
    endif
    if (aamndxtot > 0)
        write rl_file,seq;"del ",*ll,rldatn,".aam"
        unpack "" into work250
        for aamndx from "1" to "20"
            count chars in aamkeyspecs(aamndx)
            if not zero
                pack work250 with work250,aamkeyspecs(aamndx),","
            endif
        repeat
        move "," to work01
        call remove_trailer with work250,work01
        write rl_file,seq;"sunadxnt ",*ll,rldatn,",,L=",rlrecl," -U,D=~,",work250
    endif
    write rl_file,seq;""
    write rl_file,seq;"::",work60
    write rl_file,seq;":: ",blank20,"End of ",*ll,rldatn,".bat"
    write rl_file,seq;"::",work60
    close rl_file
    setitem status_stat,0,"Created output files."
    return

.------------------------------------------------------------------------------
notes_button_clicked
    setitem status_stat,0,"Loading notes file...."
    formload ddnote_form
    return

DDN_intialize
    setfocus DDN_rich
    call ddn_filename
    trap nortf if io
    open tmpfile,filename1
    trapclr io
    close tmpfile
    DDN_rich.loadfile using *bstrFilename=filename1
..    move "0" to ndx
..    DDN_rich.Upto using "`0"      // move insertion point to location identified by 'characterset'
    return

nortf                                   // Notes RTF file doesn't yet exist
    noreturn
    return

nonotes                                 // Notes RTF file doesn't yet exist
    move "1" to badsw
    return

ddn_button_clicked routine ans
    call ddn_filename
    if (ans = "O")                      // okay button clicked
        setitem status_stat,0,"Saving notes file...."
        DDN_rich.savefile using *bstrFilename=filename1
    else                                // cancel button clicked
        alert plain,"Save Notes?",nwork01,prg_name
        if (nwork01 = 1)                    // save notes
            setitem status_stat,0,"Saving notes file...."
            DDN_rich.savefile using *bstrFilename=filename1
        endif
    endif
    destroy dd_note_wind
    return

ddn_filename
    getitem file_edit,0,filename
    pack filename1 with filename,".rtf"
.    pack work80 with "RTF filename: ",filename1
.    alert note,work80,nwork01,prg_name
    return

DDN_select_button_clicked
    call ddn_filename
    open tmpfile,filename1
    loop
        read tmpfile,seq;work250
    until over
        alert note,work250,nwork01,prg_name
    repeat
    close tmpfile
    return

.------------------------------------------------------------------------------
resetfile_button_clicked
    alert plain,"Are you sure you want to reset all record definitions?",nwork01
    if (nwork01 = 1)
        setitem status_stat,0,"Resetting definition data...."
        call init_lview
        setitem file_edit,0,""
        setitem desc_edit,0,""
        setitem dataname_edit,0,""
        setitem folder_edit,0,""
        setitem recordname_edit,0,""
        setitem recordsize_edit,0,""
        call reset_button_clicked
        call modi_reset_button_clicked
        move "1" to saved
        enableitem getfile_butt
        disableitem savefile_butt
        disableitem create_butt
        disableitem view_butt
        setitem status_stat,0,"Reset definition data...."
    endif
    return

.------------------------------------------------------------------------------
fields_listview_clicked                 // field in list clicked
    call get_lview_fields using currndx
    setitem label_edit,0,dd_rec.label
    setitem type_edit,0,dd_rec.type
    setitem size_edit,0,dd_rec.size
    setitem array_edit,0,dd_rec.array
    setitem keys_edit,0,dd_rec.keys
    setitem comment_edit,0,dd_rec.comment
    call setxofy
    return

.------------------------------------------------------------------------------
modi_listview_clicked                   // modification in list clicked
    call get_lview_modi using currndx
    setitem modi_date_edit,0,dd_rec.columns
    setitem modi_who_edit,0,dd_rec.keys
    setitem modi_desc_edit,0,dd_rec.comment
    return

.------------------------------------------------------------------------------
up_button_clicked                       // move current field up in list
    if (currndx = 0)
        alert note,"Current field is already at the top.",nwork01
    else
        setitem status_stat,0,"Moving field up...."
        move (currndx - 1) to othrndx
        call move_fields
        move othrndx to currndx
        call set_focused
        call calc_columns
        move "0" to saved
        setitem status_stat,0,"Moved field up."
    endif
    enableitem savefile_butt
    return

.------------------------------------------------------------------------------
down_button_clicked                     // move current field down in list
    fields_lview.GetItemCount GIVING totlndx
    if (currndx = (totlndx - 1))
        alert note,"Current field is already at the bottom.",nwork01
    else
        setitem status_stat,0,"Moving field down...."
        move (currndx + 1) to othrndx
        call move_fields
        move othrndx to currndx
        call set_focused
        call calc_columns
        move "0" to saved
        setitem status_stat,0,"Moved field down."
    endif
    enableitem savefile_butt
    return

.------------------------------------------------------------------------------
delete_button_clicked                   // delete field in list
    setitem status_stat,0,"Deleting field...."
    fields_lview.GetItemCount GIVING totlndx
    for ndx from currndx to (totlndx - 2)
        fill " " into dd_rec
        move (ndx + 1) to othrndx       // reference next field
.       *
.       * get items from next row
.       *
        call get_lview_fields with othrndx

.       *
.       * put those items in current row
.       *
        call set_lview_fields with ndx
    repeat
    fields_lview.DeleteItem USING *Index=(totlndx - 1)
    call set_focused
    call calc_columns
    move "0" to saved
    setitem status_stat,0,"Deleted field."
    enableitem savefile_butt
    return

.------------------------------------------------------------------------------
add_button_clicked                      // add new field to list
    setitem status_stat,0,"Adding field...."
    call get_edit_to_fields

.   *
.   * put field info into new row of listview
.   *
    fields_lview.GetItemCount GIVING totlndx
    fields_lview.insertitem giving currndx using *text=dd_rec.label, *index=totlndx
    call set_lview_fields with currndx
    fields_lview.EnsureVisible USING *Index=currndx, *Partial=$false
.   *
    call set_focused
    call calc_columns
    call setxofy
    setfocus label_edit
    move "0" to saved
    setitem status_stat,0,"Added field."
    enableitem savefile_butt
    return

.------------------------------------------------------------------------------
update_button_clicked                   // update field in list
    setitem status_stat,0,"Updating field...."
    call get_edit_to_fields
.   *
.   * put field info into current row of listview
.   *
    call set_lview_fields with currndx
.   *
    call set_focused
    call calc_columns
    move "0" to saved
    setitem status_stat,0,"Updated field."
    enableitem savefile_butt
    return

.------------------------------------------------------------------------------
reset_button_clicked
    setitem label_edit,0,""
    setitem type_edit,0,""
    setitem size_edit,0,""
    setitem array_edit,0,""
    setitem keys_edit,0,""
    setitem comment_edit,0,""
    setfocus label_edit
    return

.------------------------------------------------------------------------------
modi_add_button_clicked                 // add new modification to list
    setitem status_stat,0,"Adding modification...."
    call get_edit_to_modi

.   *
.   * put modification info into new row of modi listview
.   *
    modi_lview.GetItemCount GIVING totlndx
    modi_lview.insertitem giving currndx using *text=dd_rec.columns, *index=totlndx
    call set_lview_modi with currndx
    modi_lview.EnsureVisible USING *Index=currndx, *Partial=$false
.   *
    call set_focused
    setfocus label_edit
    move "0" to saved
    setitem status_stat,0,"Added modification."
    setfocus modi_new_butt
    enableitem savefile_butt
    return

.------------------------------------------------------------------------------
modi_update_button_clicked                  // update modification in list
    setitem status_stat,0,"Updating modification...."
    call get_edit_to_modi
.   *
.   * put modification info into current row of modi listview
.   *
    call set_lview_modi with currndx
.   *
    call set_focused
    move "0" to saved
    setitem status_stat,0,"Updated modification."
    enableitem savefile_butt
    return

.------------------------------------------------------------------------------
modi_reset_button_clicked
    setitem modi_date_edit,0,""
    setitem modi_who_edit,0,""
    setitem modi_desc_edit,0,""
    return

.------------------------------------------------------------------------------
modi_new_button_clicked
    unpack "" into work08,work10,work60
    call modi_reset_button_clicked
    clock timestamp to work08
    unpack work08 into ccyy,mm,dd
    pack work10 with ccyy,".",mm,".",dd
    setitem modi_date_edit,0,work10
    getinfo system,work60
    reset work60 to 36
    setlptr work60 to 55
    setitem modi_who_edit,0,work60
    setfocus modi_desc_edit
    return

.------------------------------------------------------------------------------
ok_button_clicked                       // ok button clicked
    call save_needed with nwork01       // 1=needed, 0=not needed
    if (not nwork01)
        call write_ini
        stop
    endif
    return

.------------------------------------------------------------------------------
cancel_button_clicked
    alert plain,"Do you want to quit?",quit
    return

.------------------------------------------------------------------------------
.*
.* load the DataDesigner data into the window objects
.*
load_dd_data
    setitem status_stat,0,"Loading definition data...."
    setitem file_edit,0,filename1
    call init_lview
    loop
        read dd_file,seq;dd_rec
    until over
        call trim with dd_rec.label
        select using dd_rec.label
        when "!desc"                    // data file description
            setitem desc_edit,0,dd_rec.comment
        when "!dname"                   // data file name
            setitem dataname_edit,0,dd_rec.comment
        when "!fabbr"                   // folder abbreviation
            setitem folder_edit,0,dd_rec.comment
        when "!rname"                   // record name
            setitem recordname_edit,0,dd_rec.comment
        when "!rsize"                   // record size
            setitem recordsize_edit,0,dd_rec.comment
        when "!modi"                    // modifications
.           *
.           * load the modification listview
.           *
            modi_lview.GetItemCount GIVING totlndx
            modi_lview.insertitem giving currndx using *text=dd_rec.columns, *index=totlndx
            call set_lview_modi with currndx
        default
.           *
.           * load the field listview
.           *
            fields_lview.GetItemCount GIVING totlndx
            fields_lview.insertitem giving currndx using *text=dd_rec.label, *index=totlndx
            call set_lview_fields with currndx
        endselect
    repeat
    call setxofy
    setitem status_stat,0,"Loaded definition data."
    return

.------------------------------------------------------------------------------
.*
.* prepare the listview
.*
init_lview
    setitem status_stat,0,"Initializing Field listview...."
    setprop fields_lview,autoredraw=autotrue
    fields_lview.DeleteAllContents
    for ndx from "0" to LV_subitms
        fields_lview.insertcolumn using LV_names(ndx),LV_sizes(ndx),ndx,LV_aligns(ndx)
    repeat

    fields_lview.Setextendedstyle Using *Mask=lvs_Ex_Oneclickactivate,*Style=autotrue
    fields_lview.Setextendedstyle Using *Mask=lvs_Ex_Underlinehot,*Style=autotrue
    fields_lview.Setextendedstyle Using *Mask=lvs_Ex_Gridlines,*Style=autotrue
    fields_lview.Setextendedstyle Using *Mask=lvs_Ex_FullRowSelect,*Style=autotrue

    setitem status_stat,0,"Initializing Modification listview...."
    setprop modi_lview,autoredraw=autotrue
    modi_lview.DeleteAllContents
    for ndx from "0" to mLV_subitms
        modi_lview.insertcolumn using mLV_names(ndx),mLV_sizes(ndx),ndx,mLV_aligns(ndx)
    repeat

    modi_lview.Setextendedstyle Using *Mask=lvs_Ex_Oneclickactivate,*Style=autotrue
    modi_lview.Setextendedstyle Using *Mask=lvs_Ex_Underlinehot,*Style=autotrue
    modi_lview.Setextendedstyle Using *Mask=lvs_Ex_Gridlines,*Style=autotrue
    modi_lview.Setextendedstyle Using *Mask=lvs_Ex_FullRowSelect,*Style=autotrue

    setitem status_stat,0,"Initialized listviews."
    return

.------------------------------------------------------------------------------
.*
.* move a field up or down in the listview
.*
move_fields
.   *
.   * save current fields data into edittext objects
.   *
    call get_lview_fields using currndx
    setitem label_edit,0,dd_rec.label
    setitem type_edit,0,dd_rec.type
    setitem size_edit,0,dd_rec.size
    setitem array_edit,0,dd_rec.array
    setitem keys_edit,0,dd_rec.keys
    setitem comment_edit,0,dd_rec.comment
.   *
.   * save previous/next fields data into dd_rec
.   *
    call get_lview_fields using othrndx
.   *
.   * put current fields data into previous/next fields place
.   *
    fill " " into work50
    getitem label_edit,0,work50
    fields_lview.setitemtext using *index=othrndx, *text=work50, *subitem=LV_lbl_SI
    fill " " into work50
    getitem type_edit,0,work50
    fields_lview.setitemtext using *index=othrndx, *text=work50, *subitem=LV_typ_SI
    fill " " into work50
    getitem size_edit,0,work50
    fields_lview.setitemtext using *index=othrndx, *text=work50, *subitem=LV_siz_SI
    fill " " into work50
    getitem array_edit,0,work50
    fields_lview.setitemtext using *index=othrndx, *text=work50, *subitem=LV_arr_SI
    fill " " into work50
    getitem keys_edit,0,work50
    fields_lview.setitemtext using *index=othrndx, *text=work50, *subitem=LV_key_SI
    fill " " into work50
    getitem comment_edit,0,work50
    fields_lview.setitemtext using *index=othrndx, *text=work50, *subitem=LV_com_SI
.   *
.   * put previous/next fields data into current fields place
.   *
    call set_lview_fields using currndx
    return

.------------------------------------------------------------------------------
calc_columns
    setitem status_stat,0,"Calculating columns...."
    move "0" to startcol,endcol
    move "1" to startcol                // prepare for first field
    fields_lview.GetItemCount GIVING totlndx
    for ndx from "0" to (totlndx - 1)
        unpack "" into dd_rec,nwork05,nwork10,work10
.       *
.       * get the pertinent information
.       *
        call get_lview_fields using ndx
.       *
.       * calculate columns
.       *
        call trim with dd_rec.size
        move dd_rec.size to nwork05
        move dd_rec.array to work10
.       *
.       * get the array information
.       *
        move "0" to array               // zero out all array definitions
        loop
            fill " " into work01
            move work10 to work01
            if (work01 = "(")           // start of array definition
                move "1" to arrayndx    // first array dimension
                unpack "" into work03
            elseif (work01 = ")")       // end of array definition
                move work03 to array(arrayndx)
                setflag EOS
            elseif (work01 = ",")       // next array dimension
                move work03 to array(arrayndx)
                add "1" to arrayndx
                unpack "" into work03
            elseif (work01 >= "0" and work01 <= "9") // a number
                pack work03 with work03,work01
            endif
            bump work10 by 1
        repeat until EOS
        move "1" to nwork10             // first dimension must be multiplied by one
        for arrayndx from "1" to "3"
            if (array(arrayndx) > 0)
                multiply array(arrayndx) by nwork10
            endif
        repeat
        multiply nwork10 by nwork05     // array dimensions x size
.       *
        add nwork05 to endcol
        pack dd_rec.columns with startcol,"-",endcol
.       *
.       * put calculated columns into listview
.       *
        fields_lview.setitemtext using *index=ndx, *text=dd_rec.columns, *subitem=LV_col_SI
.       *
.       * prepare for next field
.       *
        move endcol to startcol
        add "1" to startcol
    repeat
    fill " " into work10
    move endcol to work10
    setitem recordsize_edit,0,work10
    setitem status_stat,0,"Calculated columns."
    return

.------------------------------------------------------------------------------
.*
.* set the focus to the current item in the listview
.*
set_focused
    fields_lview.SetItemState USING *Index=currndx:
                                    *State=LVIS_SELECTED,*Statemask=LVIS_SELECTED
    fields_lview.SetItemState USING *Index=currndx:
                                    *State=LVIS_FOCUSED,*Statemask=LVIS_FOCUSED
    return

.------------------------------------------------------------------------------
.------------------------------------------------------------------------------
.*
.* get field info from edittext objects
.*
get_edit_to_fields
    unpack "" into dd_rec
    getitem label_edit,0,dd_rec.label
    getitem type_edit,0,dd_rec.type
    getitem size_edit,0,dd_rec.size
    getitem array_edit,0,dd_rec.array
    getitem keys_edit,0,dd_rec.keys
    getitem comment_edit,0,dd_rec.comment
    return

.*
.* get modification info from edittext objects
.*
get_edit_to_modi
    unpack "" into dd_rec
    getitem modi_date_edit,0,dd_rec.columns
    getitem modi_who_edit,0,dd_rec.keys
    getitem modi_desc_edit,0,dd_rec.comment
    return

.------------------------------------------------------------------------------
.*
.* get dd_rec fields from listview
.*
glf_ndx  form    ^
get_lview_fields routine glf_ndx
    unpack "" into dd_rec
    fields_lview.getitemtext giving dd_rec.label using *index=glf_ndx,*subitem=LV_lbl_SI
    fields_lview.getitemtext giving dd_rec.type using *index=glf_ndx,*subitem=LV_typ_SI
    fields_lview.getitemtext giving dd_rec.size using *index=glf_ndx,*subitem=LV_siz_SI
    fields_lview.getitemtext giving dd_rec.array using *index=glf_ndx,*subitem=LV_arr_SI
    fields_lview.getitemtext giving dd_rec.columns using *index=glf_ndx,*subitem=LV_col_SI
    fields_lview.getitemtext giving dd_rec.keys using *index=glf_ndx,*subitem=LV_key_SI
    fields_lview.getitemtext giving dd_rec.comment using *index=glf_ndx,*subitem=LV_com_SI
    return

get_lview_modi routine glf_ndx
    unpack "" into dd_rec
    modi_lview.getitemtext giving dd_rec.columns using *index=glf_ndx,*subitem=mLV_dat_SI
    modi_lview.getitemtext giving dd_rec.keys using *index=glf_ndx,*subitem=mLV_who_SI
    modi_lview.getitemtext giving dd_rec.comment using *index=glf_ndx,*subitem=mLV_des_SI
    return

.------------------------------------------------------------------------------
.*
.* set dd_rec fields into listview
.*
slf_ndx  form    ^
set_lview_fields routine slf_ndx
    call trim with dd_rec.array
    call trim with dd_rec.keys
    fields_lview.setitemtext using *index=slf_ndx, *text=dd_rec.label, *subitem=LV_lbl_SI
    fields_lview.setitemtext using *index=slf_ndx, *text=dd_rec.type, *subitem=LV_typ_SI
    fields_lview.setitemtext using *index=slf_ndx, *text=dd_rec.size, *subitem=LV_siz_SI
    fields_lview.setitemtext using *index=slf_ndx, *text=dd_rec.array, *subitem=LV_arr_SI
    fields_lview.setitemtext using *index=slf_ndx, *text=dd_rec.columns, *subitem=LV_col_SI
    fields_lview.setitemtext using *index=slf_ndx, *text=dd_rec.keys, *subitem=LV_key_SI
    fields_lview.setitemtext using *index=slf_ndx, *text=dd_rec.comment, *subitem=LV_com_SI
    return

set_lview_modi routine slf_ndx
    call trim with dd_rec.columns
    call trim with dd_rec.keys
    call trim with dd_rec.comment
    modi_lview.setitemtext using *index=slf_ndx, *text=dd_rec.columns, *subitem=mLV_dat_SI
    modi_lview.setitemtext using *index=slf_ndx, *text=dd_rec.keys, *subitem=mLV_who_SI
    modi_lview.setitemtext using *index=slf_ndx, *text=dd_rec.comment, *subitem=mLV_des_SI
    return

.------------------------------------------------------------------------------
sn      form    1
save_needed procedure with sn
    move "0" to sn
    getfile dd_file,TXTNAME=work01
    if equal                            // file open
        if (not saved)
            alert plain,"Do you want to save the current file first?",nwork01
            if (nwork01 = 1)
                move "1" to sn
            endif
        endif
    endif
    return

.------------------------------------------------------------------------------
.*
.* set the "Field x of y" display
.*
setxofy
    unpack "" into totlndx,nwork04,work04
    fields_lview.GetItemCount GIVING totlndx
    move (currndx + 1) to nwork04
    move nwork04 to work04
    setitem currndx_edit,0,work04
    move totlndx to work04
    setitem totlndx_edit,0,work04
    return

.------------------------------------------------------------------------------
.*
.* get the index (isam & aam) information from the field key definitions
.* put the keyspecs (columns), field names, etc. into arrays
.*
get_index_info
    unpack "" into ismspecs,aamspecs,ismndxtot,aamndxtot:
                   ismkeylen,aamkeylen
    fields_lview.GetItemCount GIVING totlndx
    for ndx from "0" to (totlndx - 1)       // for each field in listview....
.       *
.       * get information from listivew
.       *
        call get_lview_fields with ndx
.       *
.       * strip leading spaces from field columns
.       *
        unpack dd_rec.columns into startcol,ans,endcol
        fill " " into work04,work04a,work10,dd_rec.columns
        move startcol to work04
        loop
            match " " to work04
        while equal
            bump work04 by 1
        repeat
        move endcol to work04a
        loop
            match " " to work04a
        while equal
            bump work04a by 1
        repeat
        pack dd_rec.columns with work04,"-",work04a
.       *
.       * break up index definitions into arrays
.       *
        loop //for each key in the current field:
            move dd_rec.keys to work03       // each key definition is 3 chars
            move dd_rec.keys to work01
            if (work01 = "I" or work01 = "i")       // isam index
                unpack "" into ismndxn,ismndxo,nwork05
                unpack work03 into ans,ismndxn,work01
                replace "A1a1B2b2C3c3D4d4E5e5F6f6G7g7H8h8I9i9" in work01
                move work01 to ismndxo              // isam field order
                if (ismndxn > 0 & ismndxn <= 9 & ismndxo > 0 & ismndxo <= 9)
                    move dd_rec.label to ismkeyname(ismndxn,ismndxo)
                    move dd_rec.columns to ismkeyspecs(ismndxn,ismndxo)
                    if (ismndxn > ismndxtot)
                        move ismndxn to ismndxtot   // get total isam index count
                    endif
                    unpack "" into work05,nwork05
                    move dd_rec.size to work05
                    call trim with work05
                    move work05 to  nwork05
                    add nwork05 to ismkeylen(ismndxn)
                endif
            elseif (work01 != " ")                  // aam index
                unpack "" into aamndx,work01
                unpack work03 into aamndx,work01
                if (aamndx > 0 & aamndx <= 20)
                    move work01 to aamkeysrch(aamndx)
                    move dd_rec.label to aamkeyname(aamndx)
                    move dd_rec.columns to aamkeyspecs(aamndx)
                    if (aamndx > aamndxtot)
                        move aamndx to aamndxtot   // get total aam key count
                    endif
                    unpack "" into work05,nwork05
                    move dd_rec.size to work05
                    call trim with work05
                    move work05 to  nwork05
                    move nwork05 to aamkeylen(aamndx)
                endif
            endif
            bump dd_rec.keys by 4                       // get next key for this field
        repeat until EOS
        reset dd_rec.keys
.       *
.       * now build index key specs
.       *
        unpack "" into ismspecs,aamspecs
.       * build ISAM key specs
        for ismndxn from "1" to ismndxtot   // for each isam file
            for ismndxo from "1" to "9"     // for each key in each isam file
                count chars in ismkeyspecs(ismndxn,ismndxo)
                if not zero
                    pack ismspecs(ismndxn) with ismspecs(ismndxn),ismkeyspecs(ismndxn,ismndxo),","
                endif
            repeat
        repeat
.       * remove trailing commas
        move "," to work01
        for ismndxn from "1" to ismndxtot
            call remove_trailer with ismspecs(ismndxn),work01
        repeat
.       * build AAM key specs
        for aamndx from "1" to "20"
            count chars in aamkeyspecs(aamndx)
            if not zero
                pack aamspecs with aamspecs,aamkeyspecs(aamndx),","
            endif
        repeat
.       * remove trailing comma
        move "," to work01
        call remove_trailer with aamspecs,work01

    repeat
    return

.------------------------------------------------------------------------------
rt_dim      dim     ^
rt_char     dim     ^
remove_trailer lroutine rt_dim,rt_char
    movelptr rt_dim to nwork03
    endset rt_dim
    move rt_dim to ans
    if (ans = rt_char)
        cmove " " to rt_dim
        reset rt_dim
        subtract "1" from nwork03
        setlptr rt_dim to nwork03
    endif
    return

.------------------------------------------------------------------------------
prepare_output_files
    unpack "" into rldatn,rldesc,rlrecn,rlrecl,filename
    getitem dataname_edit,0,rldatn
    getitem desc_edit,0,rldesc
    getitem recordname_edit,0,rlrecn
    getitem recordsize_edit,0,rlrecl
    getitem folder_edit,0,rlfold
    chop rldatn,rldatn
    chop rldesc,rldesc
    chop rlrecn,rlrecn
    chop rlrecl,rlrecl
    chop rlfold,rlfold
    return

.------------------------------------------------------------------------------
cef     dim     ^
cer     dim     ^
check_exist lroutine cef,cer
    trap no_file if io
    open rl_file,cef
    close rl_file
    pack work80 with "The ",cef," file already exists!  Do you want to overwrite it?"
    alert plain,work80,nwork01
    if (nwork01 = 1)                    // yes
        move "N" to cer
    else                                // no, cancel
        move "Y" to cer
    endif
    return

no_file
    noreturn
    move "N" to cer
    return

.------------------------------------------------------------------------------
.   Most Recently Used List Code
.------------------------------------------------------------------------------
.*
.* load stat-text objects with MRU list
.*
mru_initialize                          // called from Mru dialog box
    setitem mru1_stat,0,mrulist(1)
    setitem mru2_stat,0,mrulist(2)
    setitem mru3_stat,0,mrulist(3)
    setitem mru4_stat,0,mrulist(4)
    setitem mru5_stat,0,mrulist(5)
.   *
.   * disable any null MRU items
.   *
    count chars in mrulist(1)
    if zero
        disableitem mru1_stat
    endif
    count chars in mrulist(2)
    if zero
        disableitem mru2_stat
    endif
    count chars in mrulist(3)
    if zero
        disableitem mru3_stat
    endif
    count chars in mrulist(4)
    if zero
        disableitem mru4_stat
    endif
    count chars in mrulist(5)
    if zero
        disableitem mru5_stat
    endif
    return

mru_open_button_clicked                 // called from Mru dialog box
    destroy mru_wind
    move "N" to mrufile
    call getfile_selection_made
    return

mru_cancel_button_clicked               // called from Mru dialog box
    destroy mru_wind
    return

.*
.* get filename from MRU list
.*
gmrur   form    2
mru_get lroutine gmrur                  // called from Mru dialog box
    destroy mru_wind
    move mrulist(gmrur) to filename1
    move "Y" to mrufile
    call getfile_selection_made
    return

.*
.* put just-loaded filename into MRU list
.*
pmruf   dim     ^
mru_put  lroutine pmruf
    move "4" to ndx2                    // default starting point for 'insert' loop
.   *
.   * look for just-loaded filename in MRU list
.   *
    for ndx from "1" to "5"
        if (pmruf = mrulist(ndx))
            move ndx to ndx2
            subtract "1" from ndx2      // starting point for 'insert' loop
        endif
    repeat

.   *
.   * insert just-loaded filename into MRU list
.   *
    for ndx from ndx2 to "1" by "-1"
        move mrulist(ndx) to mrulist(ndx+1)
    repeat
    move pmruf to mrulist(1)
    return

.------------------------------------------------------------------------------
.   Viewer Code
.------------------------------------------------------------------------------
view_choice  form    1
DDV_intialize
    setitem reclay_radi,0,1
    return

ddv_ok_button_clicked
    destroy dd_view_wind
    return

ddv_cancel_button_clicked
    destroy dd_view_wind
    return

ddv_view_button_clicked
    call prepare_output_files
    getitem reclay_radi,0,nwork01
    if (nwork01 = 1)
        move "1" to view_choice
    else
        getitem iofile_radi,0,nwork01
        if (nwork01 = 1)
            move "2" to view_choice
        else
            move "3" to view_choice
        endif
    endif
    if (view_choice = 1)                // record layout
        pack filename with pathname,rldatn,".rec.pls"
    elseif (view_choice = 2)            // io routines
        pack filename with pathname,rldatn,".io.pls"
    else                                // batch file
        pack filename with pathname,rldatn,".bat"
    endif
    open rl_file,filename
    viewer_dlst.ResetContent
    loop
        read rl_file,seq;work250
    until over
        viewer_dlst.AddString giving nwork05 using *String=work250
    repeat
    close rl_file
    return

.------------------------------------------------------------------------------
.   DataDesigner.ini Code
.------------------------------------------------------------------------------
read_ini
    trap prepare_ini if io
    open ini_file,ini_name
    loop
        read ini_file,seq;ini_rec
    until over
        chop ini_rec.keyword,ini_rec.keyword
        if (ini_rec.keyword = "mru1")
            move ini_rec.value to mrulist(1)
        elseif (ini_rec.keyword = "mru2")
            move ini_rec.value to mrulist(2)
        elseif (ini_rec.keyword = "mru3")
            move ini_rec.value to mrulist(3)
        elseif (ini_rec.keyword = "mru4")
            move ini_rec.value to mrulist(4)
        elseif (ini_rec.keyword = "mru5")
            move ini_rec.value to mrulist(5)
        endif
    repeat
    close ini_file
    return

write_ini
    erase ini_name
    call prepare_ini
    for ndx from "1" to "5"
        unpack "" into ini_rec
        move ndx to nwork01
        move nwork01 to work01
        pack ini_rec.keyword with "mru",work01
        move mrulist(ndx) to ini_rec.value
        write ini_file,seq3;ini_rec
    repeat
    return

prepare_ini
    prepare ini_file,ini_name
    return

.------------------------------------------------------------------------------
            INCLUDE   \\clybc0701\jis$\Dev\P\Common\strings.inc
.------------------------------------------------------------------------------
.                       end of Data Designer
.------------------------------------------------------------------------------
