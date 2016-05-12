WIN95
         GETINFO   SYSTEM,taskname
         UNPACK    taskname,str1,str1
         RETURN    IF (str1="3" or str1="5")
         
         RESET     taskname,56
         MOVE      taskname,STR13
         COUNT     n2,STR13
         SETLPTR   STR13,n2
         
. . Check for microsoft patch.
         RESET     str13,4
         IF        (str13="WINDOWS")
           RESET     str13
           MOVE      portn TO str3
           PACK      str35,"c:\PRT@@",PORT,".BAT"
           PREPARE   testfile,str35
           WRITE     testfile,SEQ;"DIR ",*LL,str13:
                      "\SYSTEM\V*.VXD > c:\PATCH.DAT"
           WEOF      testfile,SEQ
           CLOSE     testfile
           PACK      S$CMDLIN,"!",str35
           EXECUTE   S$CMDLIN
. . Open work file and check for patch.
           OPEN      testfile,"c:\PATCH"
           LOOP
             READ      testfile,SEQ;str35
           UNTIL     OVER
             UNPACK    str35,str35,str3,str8
.            UNPACK    str35,DIR28,DIR8
             MATCH     "VNETSUP",str35
             IF        EQUAL
               IF        (str8="05-30-97" | str8="05-11-98")
                 MOVE      "Y",VNETSUP
               ENDIF
             ENDIF
             MATCH     "VREDIR",str35
             IF        EQUAL
               IF        (str8="09-11-97" | str8="05-11-98")
                 MOVE      "Y",VREDIR
               ENDIF
             ENDIF
           REPEAT
           CLOSE     testfile
. . Was patch applied.
           IF        (VNETSUP="N" | VREDIR="N")
      Move    "This is a Error e-mail from master",SmtpSubject Subject

.   Set the text message that is send with the attachments

       Move    "This is an error message",SmtpTextMessage(1)   Array <Text message >
        Move    error,SmtpTextMessage(2)   Array <Text message >
        Move    "This is from subroutine WIN(%",SmtpTextMessage(3)   Array <Text message >
        Move    "THis machine does NOT have the correct VREDIR patches",SmtpTextMessage(4)   Array <Text message >
        Move    "4",SmtpTextIndexLast                               Index to last entry in TextMessage array

         call      errmesg             
             SHUTDOWN
           ENDIF
         ENDIF
         RESET     str8
         RETURN
.
