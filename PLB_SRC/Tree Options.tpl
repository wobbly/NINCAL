PLBF   !   ,       #  ;      ;      ;  m   ¨     ø    Form1                           ø         ¹ú¿yÍKÚkTLºV                                                                                                                                                                         frmOptions                             opt_cmdApply                       `   opt_cmdCancel                   ú   g   opt_cmdOK                       a  c   opt_pnlGroup1                   +Ä  L   opt_txtGroup1                        opt_pnlGroup2                   +  F   opt_txtGroup2                   ä     opt_pnlGroup3                   +r  F   opt_txtGroup3                   ¸     opt_Groups                      F  t   opt_tvOptions                   %º  i       MS Sans Serif P         þ    ê         ÿÿ   Options t   !    #   *     +    5    9    U    e    f    g         ¸    ¿     MS Sans Serif P        °            Apply Þ   !    # N   %          â     MS Sans Serif P        \            Cancel Þ   !    # N   %    /          â     MS Sans Serif P                    OK Þ   !    # N   %    0          â    !               «           # P  % Z   Ð    9    â    *                      MS Sans Serif P                      Group 1 Options    !    # \   % x   3    8                         «           # P  % d   Ð    9    â    *                      MS Sans Serif P                      Group 2 Options    !    # \   % x   3    8                         «           # P  % n   Ð    9    â    *                      MS Sans Serif P                      Group 3 Options    !    # \   % x   3    8                           MS Sans Serif P         Æ             Options    !    # n  % Ò   3      â     MS Sans Serif P    ê                 Z       !    #    % Ü   +    |                  Load_frmOptions    Close_frmOptions    Click_opt_cmdCancel    Click_opt_cmdOK    Click_opt_tvOptions        L¨           ¬¨     )     Ä¨     A     ¨     U     ´¨     frmOptions                      Window
opt_cmdApply                    Button
opt_cmdCancel                   Button
opt_cmdOK                       Button
opt_pnlGroup1                   Panel
opt_txtGroup1                   StatText
opt_pnlGroup2                   Panel
opt_txtGroup2                   StatText
opt_pnlGroup3                   Panel
opt_txtGroup3                   StatText
opt_Groups                      GroupBox
opt_tvOptions                   TreeView
#EventType 	Form 	4
#EventResult 	Form 	11
#EventObjId 	Form 	8
#EventChar 	Dim 	1
#EventMod 	Form 	4

 GOTO #S
.===============================================================
Close_frmOptions
.
    SETPROP	frmOptions,VISIBLE=$FALSE
.    
@   RETURN

.===============================================================
Load_frmOptions
.
#Handle INTEGER 4
*
.Create the option items
.
    opt_tvOptions.InsertItem Giving #Handle USING "Group 1",TVI_ROOT,TVI_LAST,*Param=1
    opt_tvOptions.InsertItem USING "Group 2",TVI_ROOT,TVI_LAST,*Param=2
    opt_tvOptions.InsertItem USING "Group 3",TVI_ROOT,TVI_LAST,*Param=3
.   
    opt_tvOptions.SelectItem USING #Handle,TVGN_CARET
.    	
@   RETURN

.===============================================================
Click_opt_cmdCancel
.
    SETPROP	frmOptions,VISIBLE=$FALSE
.    
@   RETURN

.===============================================================
Click_opt_cmdOK
.
    SETPROP	frmOptions,VISIBLE=$FALSE
.    
@   RETURN

.===============================================================
Click_opt_tvOptions
.
#Item	INTEGER 4
    opt_tvOptions.GetItemParam GIVING #Item USING #EventResult
.
    IF		(#Item = 1)
    SETPROP	opt_pnlGroup1,Visible=$TRUE
    SETPROP	opt_pnlGroup2,Visible=$FALSE
    SETPROP	opt_pnlGroup3,VISIBLE=$FALSE
.    
    ELSEIF	(#Item = 2)
    SETPROP	opt_pnlGroup1,Visible=$FALSE
    SETPROP	opt_pnlGroup2,Visible=$TRUE
    SETPROP	opt_pnlGroup3,VISIBLE=$FALSE
.    
    ELSEIF	(#Item = 3)
    SETPROP	opt_pnlGroup1,Visible=$FALSE
    SETPROP	opt_pnlGroup2,Visible=$FALSE
    SETPROP	opt_pnlGroup3,VISIBLE=$TRUE
.    
    ENDIF
.    
@   RETURN

#S
