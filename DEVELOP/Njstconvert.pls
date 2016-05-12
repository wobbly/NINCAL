PC	EQU	0	
	include	common.inc
	include	cons.inc
	include	Njstdd.inc
RElease	INit	"1.0"
Reldate	INit	" "
File	FIle
Form91	Form	9.1
NewFIle	File	fixed=189
	
Main
	Open	File,"\\nts1\e\data\text\nadjust.Save",read
	Prepare	NewFIle,"\\nts1\e\data\text\NADJUST.DAT",exclusive
	Loop
	REad	File,seq;JSTBUSY:
		JSTSTAT:
		JSTMLR:   
		JSTLR:    
		JSTBILTO: 
		JSTPAYTO: 
		JSTAR:    
		JSTAP1:   
		JSTAP2:   
		jstap3:   
		JSTLRINC: 
		JSTNININC:
		JSTREUSE: 
		JSTCD:    
		JSTCRCT:  
		JSTSTAX:  
		JSTPOST:  
		JSTCTAX:  
		JSTREASN: 
		JSTCNT:   
		JSTINVNO: 
		JSTDATE:  
		JSTINVDT: 
		JSTSUBNO: 
		JSTISTAT: 
		jstqty:   
		jstqrsn:
		Form91
	Until	Over
	move	form91,JSTXNINC 
	Write	Newfile,seq;Jstvars
	move	c0,form91
.	call	njstwrt
	REpeat
	stop


	Include	Njstio.inc
	include 	comlogic.inc