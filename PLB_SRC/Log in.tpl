PLBF   !   ,     K  º                (   -     M  W  LI                              M         ho¹ü@£¦Üeýä  ¼                                                                                                                                                                      frmLogin                               LIcmdOK                            ]   LIcmdCancel                     ô   a   LItxtUserName                   U  ¯   LIlblPassword                        LIlblUserName                        LItxtPassword                     ¯       Courier New Z             b        ÿÿ   Log in ß   !    # 
  *     +    5    9    U    e    f    g         ¸    ¿     MS Sans Serif P        (             OK X   !    # P   %    0           MS Sans Serif P                (    Cancel X   !    # P   %    /               ÿÿÿ                   MS Sans Serif P                 p     }    }       
         !    # d   %    \  }             9        í                     MS Sans Serif P                    
 &Password: 0   !    # `   %    3    8                           MS Sans Serif P                     &User Name:    !    # `   %    3    8               ÿÿÿ                   MS Sans Serif P                 p     }    }             0   !    # d   %    \  }             9        í        Click_LIcmdOK    Click_LIcmdCancel      <¨          4¨     frmLogin                        Window
LIcmdOK                         Button
LIcmdCancel                     Button
LItxtUserName                   EditText
LIlblPassword                   StatText
LIlblUserName                   StatText
LItxtPassword                   EditText
#EventType 	Form 	4
#EventResult 	Form 	11
#EventObjId 	Form 	8
#EventChar 	Dim 	1
#EventMod 	Form 	4

 GOTO #S
.===============================================================
Click_LIcmdOK
.
#Length   INTEGER	4
#UserName DIM		20
#Password DIM		20
.
    GETITEM	LItxtUserName,0,#UserName
    COUNT	#Length,#UserName
    IF		ZERO
    ALERT	Caution,"The user name is required.",#Length,"Error"
    RETURN
    ENDIF
.
    GETITEM	LItxtPassword,0,#Password
    COUNT	#Length,#Password
    IF		ZERO
    ALERT	Caution,"The password is required.",#Length,"Error"
    RETURN
    ENDIF
.    
    SETPROP	frmLogin,VISIBLE=$FALSE
.     
@   RETURN


.===============================================================
Click_LIcmdCancel
.
    SETPROP	frmLogin,VISIBLE=$FALSE
.    
@   RETURN

#S
