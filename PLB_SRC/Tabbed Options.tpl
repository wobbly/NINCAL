PLBF   !   ,     �  �  �      �      �  Z        G  +  Form1                           G         �&l��9�M�'��]�)?}
  �                                                                                                                                                                      frmOptions                          �   opt_tabGroups                   �   x   opt_cmdApply                      `   opt_cmdCancel                   r  g   opt_cmdOK                       �  c   opt_pnlGroup1                   +<  L   opt_txtGroup1                   �  �   opt_pnlGroup2                   +  F   opt_txtGroup2                   \  �   opt_pnlGroup3                   +�  F   opt_txtGroup3                   0  �      �� MS Sans Serif P         �    �         ��  � Options �   !    # �  *     +    5    9    U    e    f    g    �     �    �    � MS Sans Serif P    �             
       !    # {  % 
   =� Group 1;Group 2;Group 3Q    ��  �    �    � MS Sans Serif P        8           � Apply �   !    # N   %    ��  �    �    � MS Sans Serif P        �            � Cancel �   !    # N   %    /    ��  �    �    � MS Sans Serif P        �            � OK �   !    # N   %    0    ��  �    �    !    ��          � �        *   # h  % Z   �    9    �    *            �         � MS Sans Serif P                     � Group 1 Options    !    # \   % x   3    8    ��  �    �    ��          � �        *   # h  % d   �    9    �    *            �         � MS Sans Serif P                     � Group 2 Options    !    # \   % x   3    8    ��  �    �    ��          � �        *   # h  % n   �    9    �    *            �         � MS Sans Serif P                     � Group 3 Options    !    # \   % x   3    8    ��  �    � 	       Close_frmOptions    Change_opt_tabGroups    Click_opt_cmdCancel    Click_opt_cmdOK       �          �     .     �     F     �     frmOptions                      Window
opt_tabGroups                   TabControl
opt_cmdApply                    Button
opt_cmdCancel                   Button
opt_cmdOK                       Button
opt_pnlGroup1                   Panel
opt_txtGroup1                   StatText
opt_pnlGroup2                   Panel
opt_txtGroup2                   StatText
opt_pnlGroup3                   Panel
opt_txtGroup3                   StatText
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
Change_opt_tabGroups

    SWITCH	#EventResult
    CASE	1
    SETPROP	opt_pnlGroup1,Visible=$TRUE
    SETPROP	opt_pnlGroup2,Visible=$FALSE
    SETPROP	opt_pnlGroup3,Visible=$FALSE
    CASE	2
    SETPROP	opt_pnlGroup1,Visible=$FALSE
    SETPROP	opt_pnlGroup2,Visible=$TRUE
    SETPROP	opt_pnlGroup3,Visible=$FALSE
    CASE	3
    SETPROP	opt_pnlGroup1,Visible=$FALSE
    SETPROP	opt_pnlGroup2,Visible=$FALSE
    SETPROP	opt_pnlGroup3,Visible=$TRUE
    ENDSWITCH
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

#S
