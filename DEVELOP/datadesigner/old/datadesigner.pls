.------------------------------------------------------------------------------
. datadesigner.pls
.------------------------------------------------------------------------------
.   2001.02.27  s.elliott - created.
.------------------------------------------------------------------------------
            INCLUDE   \\clybc0701\jis$\Dev\P\Common\common.uda.pls
.*
.* data definition file information
.*
dd_file     file    variable            // Data Designer file
dd_rec      record
label       dim     32
type        dim     4
size        dim     5
array       dim     10
columns     dim     10
keys        dim     25
comment     dim     50
            recordend
.*
.* record layout file information
.*
rl_file     file    variable            // Record Layout file
rldatn      dim     50                  // data file name
rldesc      dim     100                 // data file description
rlrecn      dim     10                  // data file record name
rlrecl      dim     4                   // data file record length
rlkeys      dim     10                  // data field record keys
rllabl      dim     32                  // data field label name
rlcols      dim     10                  // data field record columns
.*
.* listview definitions
.*
LV_names    dim     10(0..6),("Label"),("Type"),("Size"),("Array"),("Columns"):
                             ("Keys"),("Comment")
LV_sizes    form    3(0..6),("75"),("50"),("50"),("70"),("80"),("60"),("250")
LV_aligns   integer 4(0..6),("0x0002"),("0x0002"),("0x0001"),("0x0002"),("0x0002"):
                            ("0x0002"),("0x0000")
LV_subitms  form    " 6"
LV_lbl_SI   form    "0"                 // label subitem #
LV_typ_SI   form    "1"                 // type subitem #
LV_siz_SI   form    "2"                 // size subitem #
LV_arr_SI   form    "3"                 // array subitem #
LV_col_SI   form    "4"                 // columns subitem #
LV_key_SI   form    "5"                 // keys subitem #
LV_com_SI   form    "6"                 // comment subitem #
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
keysfld     dim     3(6)                // index key definitions for current field
keysndx     form    2                   // which index dimension are we using?
aamspec     dim     200                 // up to 20 AAM keys are allowed
aamspecs    dim     1(20)               // up to 20 AAM keys are allowed
aamndx      form    2                   // which AAM dimension are we using?
ismspecsID  dim     3(10)               // up to 10 ISAM index IDs
ismspecs    dim     50(10)              // up to 50 chars per 10 ISAM indexes are allowed
.*
.* other information
.*
quit        form    1                   // quit the program?
saved       form    1                   // has this dd record been saved?
dd_form    plform  \\clybc0701\jis$\dev\p\stuart\DataDesigner.plf
.------------------------------------------------------------------------------
    winhide
    formload dd_form
    loop
        eventcheck
    repeat until (quit)
    stop

.------------------------------------------------------------------------------
initialize
    call init_lview
    return

.------------------------------------------------------------------------------
savefile_button_clicked
.   *
.   * prepare data definition file
.   *
    getitem file_edit,0,filename
    open dd_file,filename,exclusive
.   *
.   * write data definition information
.   *
    unpack "" into dd_rec,work50,filename
    move "!desc" to dd_rec.label        // data file description
    getitem desc_edit,0,dd_rec.comment
    write dd_file,seq;dd_rec
    unpack "" into dd_rec,work50,filename
    move "!dname" to dd_rec.label       // data file name
    getitem dataname_edit,0,dd_rec.comment
    write dd_file,seq;dd_rec
    unpack "" into dd_rec,work50,filename
    move "!rname" to dd_rec.label       // record name
    getitem recordname_edit,0,dd_rec.comment
    write dd_file,seq;dd_rec
    unpack "" into dd_rec,work50,filename
    move "!rsize" to dd_rec.label       // record size
    getitem recordsize_edit,0,dd_rec.comment
    write dd_file,seq;dd_rec
.   *
.   * write fields
.   *
    fields_lview.GetItemCount GIVING totlndx
    for currndx from "0" to (totlndx - 1)
        unpack "" into dd_rec
        fields_lview.getitemtext giving dd_rec.label using *index=currndx:
                                                             *subitem=LV_lbl_SI
        fields_lview.getitemtext giving dd_rec.type using *index=currndx:
                                                             *subitem=LV_typ_SI
        fields_lview.getitemtext giving dd_rec.size using *index=currndx:
                                                             *subitem=LV_siz_SI
        fields_lview.getitemtext giving dd_rec.array using *index=currndx:
                                                             *subitem=LV_arr_SI
        fields_lview.getitemtext giving dd_rec.columns using *index=currndx:
                                                             *subitem=LV_col_SI
        fields_lview.getitemtext giving dd_rec.keys using *index=currndx:
                                                             *subitem=LV_key_SI
        fields_lview.getitemtext giving dd_rec.comment using *index=currndx:
                                                             *subitem=LV_com_SI
        write dd_file,seq;dd_rec
    repeat
    move "1" to saved
    return

.------------------------------------------------------------------------------
getfile_button_clicked
    call save_needed with nwork01       // 1=needed, 0=not needed
    if (not nwork01)
        GETFNAME OPEN,"Open Data Definition",fileNAME,PATHname,"txt"
        if not over
            pack filename1 with pathname,filename
            open dd_file,filename1,exclusive
            call load_dd_data
            close dd_file
        endif
    endif
    return

.------------------------------------------------------------------------------
view_button_clicked                     // view record layout
    return

.------------------------------------------------------------------------------
create_button_clicked                   // create record layout
.   *
.   * create record layout file name
.   *
    unpack "" into rldatn,rldesc,rlrecn,rlrecl,filename
    getitem dataname_edit,0,rldatn
    getitem desc_edit,0,rldesc
    getitem recordname,0,rlrecn
    getitem recordsize,0,rlrecl
    pack filename with pathname,rldatn,".rec.pls"
.   *
.   * create record layout file
.   *
    erase filename
    prep rlfile,filename
.   *
.   * write data file description header
.   *
    write rlfile,seq;".----------------------------------------------------------"
    write rlfile,seq;". ",rldatn," - ",rldesc
    write rlfile,seq;".----------------------------------------------------------"
    write rlfile,seq;""
.   *
.   * write file declarations
.   *
.NEED:   keyspecs            = columns
.        file declaration    = keys
.        key variables       = keys

ismkeyname  dim     32(9,9)             // isam key field name
ismkeyspecs dim     9(9,9)              // up to 9 fields per up to 9 ISAM files
aamkeyname  dim     32(20)              // aam key field name
aamkeys     dim     1(20)               // up to 20 AAM key search flags
aamkeyspecs dim     9(20)               // up to 20 AAM keys
ismndxn     form    1                   // isam index number
ismndxo     form    1                   // isam index order
aamndx      form    2
.       *
.       * get key information
.       *
    unpack "" into ismspecs,ismspecsID,aamspecs
    fields_lview.GetItemCount GIVING totlndx
    for ndx from "0" to (totlndx - 1)       // for each field....
.       *
.       * get information from listivew
.       *
        fields_lview.getitemtext giving rllabl using *index=ndx,*subitem=LV_lbl_SI
        fields_lview.getitemtext giving rlkeys using *index=ndx,*subitem=LV_key_SI
        fields_lview.getitemtext giving rlcols using *index=ndx,*subitem=LV_col_SI
.       *
.       * strip leading spaces from field columns
.       *
        unpack rlcols into startcol,ans,endcol
        fill " " into work04,work04a,work10,rlcols
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
        pack rlcols with work04,"-",work04a
.       *
.       * break up index definitions into arrays
.       *
        loop //for each key:
            move rlkeys to work03
            move rlkeys to work01
            if (work01 = "I" or work01 = "i")       // isam index
                unpack "" into ismndxn,ismndxo
                unpack work03 into ans,ismndxn,work01
                replace "A1a1B2b2C3c3D4d4E5e5F6f6G7g7H8h8I9i9" in work01
                move work01 to ismndxo              // isam field order
                if (ismndxn > 0 & ismndxn <= 9 & ismndxo > 0 & ismndxo <= 9)
                    move rllabl to ismkeyname(ismndxn,ismndxo)
                    move rlcols to ismkeyspecs(ismndxn,ismndxo)
                endif
            elseif (work01 != " ")                  // aam index
                unpack work03 into aamndx,work01
                if (aamndx > 0 & aamndx <= 20)
                    move work01 to aamkeys(aamndx)
                    move rllabl to aamkeyname(aamndx)
                    move rlcols to aamkeyspecs(amndx)
                endif
            endif
            bump rlkeys by 4                        // get next key for this field
        repeat until EOS
        reset rlkeys


.        unpack "" into keysfld          // blank out all key definitions
.        unpack "" into keysndx,work03
.        loop
.            fill " " into work01
.            move rlkeys to work01
.            if (work01 = ",")           // next array dimension
.                add "1" to keysndx
.                if (keysndx < 7)
.                    move work03 to keysfld(keysndx)
.                endif
.                unpack "" into work03
.            elseif (work01 != " ")
.                pack work03 with work03,work01
.            endif
.            bump rlkeys by 1
.        repeat until EOS
.        count chars in work03           // is anything in last keyspec?
.        if not zero                     // yes.
.            add "1" to keysndx
.            if (keysndx < 7)
.                move work03 to keysfld(keysndx)
.            endif
.        endif
.        reset rlkeys
.       *
.       * now build record key specs
.       *
.ismspecs    dim     50(10)              // index key definition for entire record
.ismspecsID  dim     3(10)
.aamspec     dim     200
.aamspecs    dim     1(20)
        move "0" to aamndx
        for ndx from "1" to keysndx
            move keysfld(ndx) to work01
            if (work01 = "I" or work01 = "i")       // isam index

            else                                    // aam index
                unpack "" into work01,work02,nwork02
                unpack keysfld(ndx) into work02,work01
                move work02 to nwork02
                move nwork01 to aamspecs(nwork02)
                if (nwork02 > aamndx)
                    move nwork02 to aamndx
                endif
            endif
        repeat
.       * build AAM key specs
        unpack "" into aamspec
        for nwork02 from "1" to aamndx
            move nwork02 to work02
            replace " 0" in work02
            pack aamspec with aamspec,work02,aamspecs(nwork02),","
        repeat
.       *
    repeat
.       * write key information
    write rlfile,seq;rlrecn,"_flist",*20,"filelist"
    write rlfile,seq;*20,"filelistend"
    return

.------------------------------------------------------------------------------
fields_listview_clicked                 // field in list clicked
    unpack "" into dd_rec
    fields_lview.getitemtext giving dd_rec.label using *index=currndx:
                                                       *subitem=LV_lbl_SI
    fields_lview.getitemtext giving dd_rec.type using *index=currndx:
                                                      *subitem=LV_typ_SI
    fields_lview.getitemtext giving dd_rec.size using *index=currndx:
                                                       *subitem=LV_siz_SI
    fields_lview.getitemtext giving dd_rec.array using *index=currndx:
                                                       *subitem=LV_arr_SI
    fields_lview.getitemtext giving dd_rec.keys using *index=currndx:
                                                      *subitem=LV_key_SI
    fields_lview.getitemtext giving dd_rec.comment using *index=currndx:
                                                         *subitem=LV_com_SI
    setitem label_edit,0,dd_rec.label
    setitem type_edit,0,dd_rec.type
    setitem size_edit,0,dd_rec.size
    setitem array_edit,0,dd_rec.array
    setitem keys_edit,0,dd_rec.keys
    setitem comment_edit,0,dd_rec.comment
    call setxofy
    return

.------------------------------------------------------------------------------
up_button_clicked                       // move current field up in list
    if (currndx = 0)
        alert note,"Current field is already at the top.",nwork01
    else
        move (currndx - 1) to othrndx
        call move_fields
        move othrndx to currndx
        call set_focused
        call calc_columns
        move "0" to saved
    endif
    return

.------------------------------------------------------------------------------
down_button_clicked                     // move current field down in list
    fields_lview.GetItemCount GIVING totlndx
    if (currndx = (totlndx - 1))
        alert note,"Current field is already at the bottom.",nwork01
    else
        move (currndx + 1) to othrndx
        call move_fields
        move othrndx to currndx
        call set_focused
        call calc_columns
        move "0" to saved
    endif
    return

.------------------------------------------------------------------------------
delete_button_clicked                   // delete field in list
    fields_lview.GetItemCount GIVING totlndx
    for ndx from currndx to (totlndx - 2)
        fill " " into dd_rec
        move (ndx + 1) to othrndx       // reference next field
.       *
.       * get items from next row
.       *
        fields_lview.getitemtext giving dd_rec.label using *index=othrndx:
                                                           *subitem=LV_lbl_SI
        fields_lview.getitemtext giving dd_rec.type using *index=othrndx:
                                                          *subitem=LV_typ_SI
        fields_lview.getitemtext giving dd_rec.size using *index=othrndx:
                                                          *subitem=LV_siz_SI
        fields_lview.getitemtext giving dd_rec.array using *index=othrndx:
                                                           *subitem=LV_arr_SI
        fields_lview.getitemtext giving dd_rec.keys using *index=othrndx:
                                                          *subitem=LV_key_SI
        fields_lview.getitemtext giving dd_rec.comment using *index=othrndx:
                                                             *subitem=LV_com_SI
.       *
.       * put those items in current row
.       *
        fields_lview.setitemtext using *index=ndx, *text=dd_rec.label, *subitem=LV_lbl_SI
        fields_lview.setitemtext using *index=ndx, *text=dd_rec.type, *subitem=LV_typ_SI
        fields_lview.setitemtext using *index=ndx, *text=dd_rec.size, *subitem=LV_siz_SI
        fields_lview.setitemtext using *index=ndx, *text=dd_rec.array, *subitem=LV_arr_SI
        fields_lview.setitemtext using *index=ndx, *text=dd_rec.keys, *subitem=LV_key_SI
        fields_lview.setitemtext using *index=ndx, *text=dd_rec.comment, *subitem=LV_com_SI
    repeat
    fields_lview.DeleteItem USING *Index=(totlndx - 1)
    call set_focused
    call calc_columns
    move "0" to saved
    return

.------------------------------------------------------------------------------
add_button_clicked                      // add new field to list
    call get_edit_to_fields

.   *
.   * put field info into new row of listview
.   *
    fields_lview.GetItemCount GIVING totlndx
    fields_lview.insertitem giving currndx using *text=dd_rec.label, *index=totlndx
    fields_lview.setitemtext using *index=currndx, *text=dd_rec.type, *subitem=LV_typ_SI
    fields_lview.setitemtext using *index=currndx, *text=dd_rec.size, *subitem=LV_siz_SI
    fields_lview.setitemtext using *index=currndx, *text=dd_rec.array, *subitem=LV_arr_SI
    fields_lview.setitemtext using *index=currndx, *text=dd_rec.keys, *subitem=LV_key_SI
    fields_lview.setitemtext using *index=currndx, *text=dd_rec.comment, *subitem=LV_com_SI
.   *
    call set_focused
    call calc_columns
    call setxofy
    setfocus label_edit
    move "0" to saved
    return

.------------------------------------------------------------------------------
update_button_clicked                   // update field in list
    call get_edit_to_fields
.   *
.   * put field info into current row of listview
.   *
    fields_lview.setitemtext using *index=currndx, *text=dd_rec.label, *subitem=LV_lbl_SI
    fields_lview.setitemtext using *index=currndx, *text=dd_rec.type, *subitem=LV_typ_SI
    fields_lview.setitemtext using *index=currndx, *text=dd_rec.size, *subitem=LV_siz_SI
    fields_lview.setitemtext using *index=currndx, *text=dd_rec.array, *subitem=LV_arr_SI
    fields_lview.setitemtext using *index=currndx, *text=dd_rec.keys, *subitem=LV_key_SI
    fields_lview.setitemtext using *index=currndx, *text=dd_rec.comment, *subitem=LV_com_SI
.   *
    call set_focused
    call calc_columns
    move "0" to saved
    return

.------------------------------------------------------------------------------
ok_button_clicked                       // ok button clicked
    call save_needed with nwork01       // 1=needed, 0=not needed
    stop if (not nwork01)
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
        when "!rname"                   // record name
            setitem recordname_edit,0,dd_rec.comment
        when "!rsize"                   // record size
            setitem recordsize_edit,0,dd_rec.comment
        default
.           *
.           * load the listview
.           *
            fields_lview.GetItemCount GIVING totlndx
            fields_lview.insertitem giving currndx using *text=dd_rec.label, *index=totlndx
            fields_lview.setitemtext using *index=currndx, *text=dd_rec.type, *subitem=LV_typ_SI
            fields_lview.setitemtext using *index=currndx, *text=dd_rec.size, *subitem=LV_siz_SI
            fields_lview.setitemtext using *index=currndx, *text=dd_rec.array, *subitem=LV_arr_SI
            fields_lview.setitemtext using *index=currndx, *text=dd_rec.columns, *subitem=LV_col_SI
            fields_lview.setitemtext using *index=currndx, *text=dd_rec.keys, *subitem=LV_key_SI
            fields_lview.setitemtext using *index=currndx, *text=dd_rec.comment, *subitem=LV_com_SI
        endselect
    repeat
    call setxofy
    return

.------------------------------------------------------------------------------
.*
.* prepare the listview
.*
init_lview
    setprop fields_lview,autoredraw=autotrue
    fields_lview.DeleteAllContents
    for ndx from "0" to LV_subitms
        fields_lview.insertcolumn using LV_names(ndx),LV_sizes(ndx),ndx,LV_aligns(ndx)
    repeat
.    fields_lview.insertcolumn using "Label",75,0,LVCFMT_CENTER
.    fields_lview.insertcolumn using "Type",50,1,LVCFMT_CENTER
.    fields_lview.insertcolumn using "Size",50,2,LVCFMT_RIGHT
.    fields_lview.insertcolumn using "Array",70,3,LVCFMT_CENTER
.    fields_lview.insertcolumn using "Columns",80,4,LVCFMT_CENTER
.    fields_lview.insertcolumn using "Keys",60,5,LVCFMT_CENTER
.    fields_lview.insertcolumn using "Comment",250,6,LVCFMT_LEFT

    fields_lview.Setextendedstyle Using *Mask=lvs_Ex_Oneclickactivate,*Style=autotrue
    fields_lview.Setextendedstyle Using *Mask=lvs_Ex_Underlinehot,*Style=autotrue
    fields_lview.Setextendedstyle Using *Mask=lvs_Ex_Gridlines,*Style=autotrue
    fields_lview.Setextendedstyle Using *Mask=lvs_Ex_FullRowSelect,*Style=autotrue
    return

.------------------------------------------------------------------------------
.*
.* move a field up or down in the listview
.*
move_fields
.   *
.   * save current fields data into edittext objects
.   *
    fill " " into dd_rec
    fields_lview.getitemtext giving dd_rec.label using *index=currndx:
                                                       *subitem=LV_lbl_SI
    setitem label_edit,0,dd_rec.label
    fields_lview.getitemtext giving dd_rec.type using *index=currndx:
                                                       *subitem=LV_typ_SI
    setitem type_edit,0,dd_rec.type
    fields_lview.getitemtext giving dd_rec.size using *index=currndx:
                                                      *subitem=LV_siz_SI
    setitem size_edit,0,dd_rec.size
    fields_lview.getitemtext giving dd_rec.array using *index=currndx:
                                                       *subitem=LV_arr_SI
    setitem array_edit,0,dd_rec.array
    fields_lview.getitemtext giving dd_rec.keys using *index=currndx:
                                                      *subitem=LV_key_SI
    setitem keys_edit,0,dd_rec.keys
    fields_lview.getitemtext giving dd_rec.comment using *index=currndx:
                                                         *subitem=LV_com_SI
    setitem comment_edit,0,dd_rec.comment
.   *
.   * save previous/next fields data into dd_rec
.   *
    fill " " into dd_rec
    fields_lview.getitemtext giving dd_rec.label using *index=othrndx:
                                                       *subitem=LV_lbl_SI
    fields_lview.getitemtext giving dd_rec.type using *index=othrndx:
                                                      *subitem=LV_typ_SI
    fields_lview.getitemtext giving dd_rec.size using *index=othrndx:
                                                      *subitem=LV_siz_SI
    fields_lview.getitemtext giving dd_rec.array using *index=othrndx:
                                                       *subitem=LV_arr_SI
    fields_lview.getitemtext giving dd_rec.keys using *index=othrndx:
                                                      *subitem=LV_key_SI
    fields_lview.getitemtext giving dd_rec.comment using *index=othrndx:
                                                         *subitem=LV_com_SI
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
    fields_lview.setitemtext using *index=currndx, *text=dd_rec.label, *subitem=LV_lbl_SI
    fields_lview.setitemtext using *index=currndx, *text=dd_rec.type, *subitem=LV_typ_SI
    fields_lview.setitemtext using *index=currndx, *text=dd_rec.size, *subitem=LV_siz_SI
    fields_lview.setitemtext using *index=currndx, *text=dd_rec.array, *subitem=LV_arr_SI
    fields_lview.setitemtext using *index=currndx, *text=dd_rec.keys, *subitem=LV_key_SI
    fields_lview.setitemtext using *index=currndx, *text=dd_rec.comment, *subitem=LV_com_SI
    return

.------------------------------------------------------------------------------
calc_columns
    move "0" to startcol,endcol
    move "1" to startcol                // prepare for first field
    fields_lview.GetItemCount GIVING totlndx
    for ndx from "0" to (totlndx - 1)
        unpack "" into dd_rec,nwork05,nwork10,work10
.       *
.       * get the pertinent information
.       *
        fields_lview.getitemtext giving dd_rec.size using *index=ndx:
                                                          *subitem=LV_siz_SI
        fields_lview.getitemtext giving dd_rec.array using *index=ndx:
                                                           *subitem=LV_arr_SI
.       *
.       * calculate columns
.       *
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
            INCLUDE   \\clybc0701\jis$\Dev\P\Common\strings.inc
.------------------------------------------------------------------------------
.                       end of Data Designer
.------------------------------------------------------------------------------
