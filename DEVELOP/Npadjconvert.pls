PC	EQU	0	
	include	common.inc
	include	cons.inc
	include	Njstdd.inc
RElease	INit	"1.0"
Reldate	INit	" "
File	FIle
NewFIle	File	fixed=192
Form91	Form	9.1
	
Main
	Open	File,"\\nts1\e\data\ninpadj.Save",read
	Prepare	NewFIle,"\\nts1\e\data\Ninpadj.DAT"
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
		Form91:
		str3
	Until	Over
	move	form91,JSTXNINC 
	Write	Newfile,seq;Jstvars,str3
	Move	c0,FOrm91
.	call	njstwrt
	REpeat
	stop


	Include	Njstio.inc
	include 	comlogic.inc