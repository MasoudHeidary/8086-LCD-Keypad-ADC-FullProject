DATA SEGMENT
;LCD DATA
;variables to keep track of port data
	LCD_PORTA_VAL DB 0
    LCD_PORTB_VAL DB 0
    LCD_PORTC_VAL DB 0
           
;port addresses  
    LCD_PORTA DW 00H 	;PORTA IS CONNECTED TO THE D7-D0
	LCD_PORTB DW 00H 	;PORTB0 IS RW, PORTB1 IS RS, PORTB2 IS EN
	LCD_PORTC DW 00H
	LCD_PORTR DW 00H	;PORT FOR IO CONTROL
	
;Save Address
    LCD1_A DB 0
    LCD1_B DB 0
    LCD1_C DB 0
    
    LCD2_A DB 0
    LCD2_B DB 0
    LCD2_C DB 0
    
    LCD1_PORTA equ 00010000B
    LCD1_PORTB equ 00010010B
    LCD1_PORTC equ 00010100B
    LCD1_PORTR equ 00010110B
    
    LCD2_PORTA equ 00011000B
    LCD2_PORTB equ 00011010B
    LCD2_PORTC equ 00011100B
    LCD2_PORTR equ 00011110B

;END LCD DATA
	            
;IO define
    Input equ 00000000B
    LED   equ 00000010B
    IOReg equ 00000110B
    
;temperature sensor
;///////////////////////////////////////////
;Read value for Temperature: 0(min) - 255(max) : 8bit   
;///////////////////////////////////////////
    TPortA equ 08H
    TPortB equ 0AH
    TPortC equ 0CH
    TReg   equ 0EH  
    
    ;for detect Covid-19
    TNormal   equ 180
    StringNewCustomer DB "New Customer$"
    StringTNormal DB "OK-Normal$"
    StringTAbNormal DB "COVID-19!!!$"
    
;ticket
    TicketNumber DW 00H         ;for save ticket number
    StringTicket DB "Ticket:$"    
    ServiceNumber DW 00H        ;for save serverice number
    StringService DB "Service:$"
    
ENDS

STACK SEGMENT
    DW   128  DUP(0)
ENDS



;//////////////////////////////////////////////////////MAIN CODE
CODE SEGMENT
START:
; set segment registers: 
    MOV AX, DATA
    MOV DS, AX
    MOV ES, AX  
    
;define IO ports
    MOV DX,LCD_PORTR
    MOV AL,10000000B   ;to make all ports output
    OUT DX,AL  
    

	CALL LCDInit
	CALL ServiceUpdate
    CALL UpdateTicket
	CALL IOInit
	
MainLoop:
    ;-----------------------------------check  present of new customer    
        CALL ReadPresent    ;save in AL
        
        CMP AL,01H          ;check custimer existion
        JE IF1              ;if customer exist
        JNE IF1_ELSE        ;if not exist
    IF1:
        CALL NewCustomer 
        JMP IF1_END 
        
    IF1_ELSE:
        CALL LCD1ON
        CALL LCD_CLEAR
        CALL LEDOFF
        CALL LCD1OFF 
        
    IF1_END:
    
    ;-----------------------------------check employers
    
        CALL ServiceCheckState  ;(AL)
        
        CMP AL,01H
        JE IF2                  ;key pressed
        JMP IF2_END
    IF2:
        CALL ServiceInc
        CALL ServiceUpdate
        CALL ServiceStateWait
    IF2_END:
    
        JMP MainLoop            ;stay in loop
	

	HLT
;end of main procedure
;//////////////////////////////////////////////////////END MAIN CODE


;////////////////////////////////////////////////////////IO


;start IOs
IOInit PROC Near
        PUSH AX
        
        MOV AL,10010000B ;A=input, B,C=output
        OUT IOReg,AL
    
        CALL LEDOFF
        
        MOV AL,99H       ;A,C=input, B=output
        OUT TReg,AL      ;control register
        
        POP AX
        RET
IOInit ENDP



;read Present sate and return in (AL)
;(AL=1) present , (AL=0) not present
ReadPresent PROC Near
        IN AL,Input 
        NOT AL          ;if present = 1 , not present = 0
        AND AL,01H      ;obtain bit 1
        RET 
ReadPresent ENDP 

;turn on just green led
GreenON PROC Near
        PUSH AX
        MOV AL,02H
        OUT LED,AL 
        POP AX
        RET
GreenON ENDP
 
 
;turn on just red led
RedON PROC Near
        PUSH AX
        MOV AL,01H
        OUT LED,AL 
        POP AX
        RET
RedON ENDP


;turn off leds
LEDOFF PROC Near
        PUSH AX
        MOV AL,00H
        OUT LED,AL
        POP AX
        RET
LEDOFF ENDP


;return AL as result
ReadTemp PROC Near
        PUSH AX
        PUSH CX
         
        MOV AL,0FFH      ;WR=High 
        OUT TPortB,AL    ;PortB 
        MOV AL,0FCH      ;start conv
        OUT TPortB,AL    ;Port B 
        
        NOP
        
        MOV AL,0FFH      ;WR=High
        OUT TPortB,AL    ;Port B
        
        MOV CX,1000
        CALL DELAY
      
        POP CX
        POP AX
        IN AL,TPortA   ;read temperature data
        RET
ReadTemp ENDP


;////////////////////////////////////////////////////////END IO



;to handle new customer present
NewCustomer PROC Near 
        
        CALL LCD1ON
        
        MOV DL,1
    	MOV DH,1
    	CALL LCD_SET_CUR    
    	
        LEA SI,StringNewCustomer
        CALL LCD_PRINTSTR
        
        CALL ReadTemp   ;AL
        MOV AH,TNormal
        CMP AL,AH
          
        JB Normal      ;blow
        JMP AbNormal   ;else
         
    Normal:
        CALL GreenON
        
        MOV DL,2
    	MOV DH,1
    	CALL LCD_SET_CUR
    	
        LEA SI,StringTNormal
        CALL LCD_PRINTSTR
        
        CALL LCD1OFF            ;off lcd1
        
        INC TicketNumber        ;increase ticket number                        
        CALL UpdateTicket       ;use lcd2
        
        JMP NewCustomerExit
    
    AbNormal:
        CALL RedON
                 
        MOV DL,2
    	MOV DH,1
    	CALL LCD_SET_CUR
    	
        LEA SI,StringTAbNormal
        CALL LCD_PRINTSTR
        
        CALL LCD1OFF
        
        JMP NewCustomerExit
        
    NewCustomerExit:
        CALL WaitCustomerPass
        RET
    
NewCustomer ENDP



WaitCustomerPass PROC Near
         PUSH AX
    Pass:
        CALL ReadPresent    ;AL
        CMP AL,00H
        JNE Pass
        POP AX
        RET
         
WaitCustomerPass ENDP



UpdateTicket PROC Near
        PUSH AX
        PUSH BX
        PUSH DX
        
        CALL LCD2ON
        
        MOV DL,1
    	MOV DH,1
    	CALL LCD_SET_CUR
    	
    	LEA SI,StringTicket
    	CALL LCD_PRINTSTR
        
        MOV DL,1
    	MOV DH,8
    	CALL LCD_SET_CUR   
        
        MOV AX,TicketNumber 
        
        CALL PrintBin
        CALL LCD2OFF
        
        POP DX
        POP BX
        POP AX
        RET        
UpdateTicket ENDP



;///////////////////////////////////////////////////////////Service

;(AL=0),(AL=1)
ServiceCheckState PROC Near
        
        IN AL,Input
        SHR AL,1
        NOT AL
        AND AL,01H
        
        RET
ServiceCheckState ENDP


;service number += 1
;cant be higher than TicketNumber
ServiceInc PROC Near
        PUSH AX
        MOV AX, ServiceNumber
        
        CMP AX,TicketNumber
        JGE ServiceIncExit
        
        INC ServiceNumber
    
    ServiceIncExit:
        POP AX    
        RET
ServiceInc ENDP


;to update service number in LCD
ServiceUpdate PROC Near
        PUSH AX
        PUSH DX
        
        CALL LCD2ON
        
        MOV DL,2
    	MOV DH,1
    	CALL LCD_SET_CUR
    	
    	LEA SI,StringService
    	CALL LCD_PRINTSTR
        
        MOV DL,2
    	MOV DH,9
    	CALL LCD_SET_CUR
    	
    	MOV AX,ServiceNumber
        
        CALL PrintBin
        CALL LCD2OFF
        
        POP DX
        POP AX
        RET
    
ServiceUpdate ENDP


;wait until employer release button
ServiceStateWait PROC Near
        PUSH AX
    LL:
        CALL ServiceCheckState
        CMP AL,00H
        JNE LL
        
        POP AX
        RET
ServiceStateWait ENDP


;///////////////////////////////////////////////////////////Print
                     
;AX as input(max 16bit)
;note: save the CX,SI,DX,AX
PrintBin proc
    PUSH AX 
    PUSH CX
    PUSH DX
    PUSH SI 
    mov cx,5
    mov si,10D

    DivTo10:
        mov dx,0000H
        div si  ;remain in dx
        push dx ;push to save digits
        loop DivTo10
    
    mov cx,5    ;print counter
    Print:
        POP AX  ;pop to load digits
        add AX,30H  ;change to ASCII   
        MOV AH,AL       
        
        PUSH CX
        CALL LCD_WRITE_CHAR
        POP CX
        
        LOOP Print  ;loop until end counter(cx)

    ;pop
        POP SI
        POP DX
        POP CX
        POP AX
        Ret
PrintBin ENDP  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                    ;
;		LCD function library.(USER)  ;
;                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              
LCDInit PROC Near
     
    CALL LCD1ON
    CALL LCD_INIT
    CALL LCD1OFF

    CALL LCD2ON
    CALL LCD_INIT
    CALL LCD2OFF
    
    RET
    
LCDInit ENDP

LCD1ON PROC Near
    PUSH AX
    
    MOV LCD_PORTA,LCD1_PORTA
    MOV LCD_PORTB,LCD1_PORTB
    MOV LCD_PORTC,LCD1_PORTC
    MOV LCD_PORTR,LCD1_PORTR
    
    MOV AL,LCD1_A
    MOV LCD_PORTA_VAL,AL 
    MOV AL,LCD1_B
    MOV LCD_PORTB_VAL,AL
    MOV AL,LCD1_C
    MOV LCD_PORTC_VAL,AL
    
    POP AX
    RET
LCD1ON ENDP



LCD2ON PROC Near
    PUSH AX
    
    MOV LCD_PORTA,LCD2_PORTA
    MOV LCD_PORTB,LCD2_PORTB
    MOV LCD_PORTC,LCD2_PORTC
    MOV LCD_PORTR,LCD2_PORTR
    
    MOV AL,LCD2_A
    MOV LCD_PORTA_VAL,AL 
    MOV AL,LCD2_B
    MOV LCD_PORTB_VAL,AL
    MOV AL,LCD2_C
    MOV LCD_PORTC_VAL,AL
    
    POP AX
    RET
LCD2ON ENDP
           

LCD1OFF PROC Near
    PUSH AX
    
    MOV AL,LCD_PORTA_VAL
    MOV LCD1_A,AL
    MOV AL,LCD_PORTB_VAL
    MOV LCD1_B,AL
    MOV AL,LCD_PORTC_VAL
    MOV LCD1_C,AL
    
    POP AX
    RET
LCD1OFF ENDP

LCD2OFF PROC Near
    PUSH AX
    
    MOV AL,LCD_PORTA_VAL
    MOV LCD2_A,AL
    MOV AL,LCD_PORTB_VAL
    MOV LCD2_B,AL
    MOV AL,LCD_PORTC_VAL
    MOV LCD2_C,AL
    
    POP AX
    RET
LCD2OFF ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                    ;
;		LCD function library.(CORE)  ;
;                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PROC DELAY
;input: CX, this value controls the delay. CX=50 means 1ms
;output: none
    PUSH CX
	JCXZ @DELAY_END
	@DEL_LOOP:
	LOOP @DEL_LOOP	
	@DELAY_END:
    POP CX
	RET
ENDP DELAY



; LCD initialization
PROC LCD_INIT
;input: none
;output: none
    PUSH AX
    PUSH CX
    PUSH DX
    
    MOV DX,LCD_PORTR
    MOV AL,80H
    OUT DX,AL
    

;make RS=En=RW=0
	MOV AL,0
	CALL OUT_B
;delay 20ms
	MOV CX,1000
	CALL DELAY
;reset sequence
	MOV AH,30H
	CALL LCD_CMD
	MOV CX,250
	CALL DELAY
	
	MOV AH,30H
	CALL LCD_CMD
	MOV CX,50
	CALL DELAY
	
	MOV AH,30H
	CALL LCD_CMD
	MOV CX,500
	CALL DELAY
	
;function set
	MOV AH,38H
	CALL LCD_CMD
	
	MOV AH,0CH
	CALL LCD_CMD
	
	MOV AH,01H
	CALL LCD_CMD
	
	MOV AH,06H
	CALL LCD_CMD
	
	POP DX 
	POP CX
	POP AX  
	RET	
ENDP LCD_INIT




;sends commands to LCD
PROC LCD_CMD
;input: AH = command code
;output: none

;save registers
	PUSH CX
	PUSH AX
;make rs=0
	MOV AL,LCD_PORTB_VAL
	AND AL,0FDH		;En-RS-RW
	CALL OUT_B
;set out data pins
	MOV AL,AH
	CALL OUT_A
;make En=1
	MOV AL,LCD_PORTB_VAL
	OR	AL,100B		;En-RS-RW
	CALL OUT_B
;delay 1ms
	MOV CX,50
	CALL DELAY
;make En=0
	MOV AL,LCD_PORTB_VAL
	AND AL,0FBH		;En-RS-RW
	CALL OUT_B
;delay 1ms
	MOV CX,50
	CALL DELAY
;restore registers
	POP AX
	POP CX	
	RET
ENDP LCD_CMD




PROC LCD_CLEAR
    PUSH AX
	MOV AH,1
	CALL LCD_CMD
	POP AX
	RET	
ENDP LCD_CLEAR



;writes a character on current cursor position
PROC LCD_WRITE_CHAR
;input: AH
;output: none

;save registers
	PUSH AX
	PUSH CX
;set RS=1
	MOV AL,LCD_PORTB_VAL
	OR	AL,10B		;EN-RS-RW
	CALL OUT_B
;set out the data pins
	MOV AL,AH
	CALL OUT_A
;set En=1
	MOV AL,LCD_PORTB_VAL
	OR	AL,100B		;EN-RS-RW
	CALL OUT_B
;delay 1ms
	MOV CX,50
	CALL DELAY
;set En=0
	MOV AL,LCD_PORTB_VAL
	AND	AL,0FBH		;EN-RS-RW
	CALL OUT_B
;return
    POP CX
	POP AX
	RET	
ENDP LCD_WRITE_CHAR





;prints a string on current cursor position
PROC LCD_PRINTSTR
;input: SI=string address, string should end with '$'
;output: none

;save registers
	PUSH SI
	PUSH AX
;read and write character
	@LCD_PRINTSTR_LT:
		LODSB
		CMP AL,'$'
		JE @LCD_PRINTSTR_EXIT
		MOV AH,AL
		CALL LCD_WRITE_CHAR	
	JMP @LCD_PRINTSTR_LT
	
;return
	@LCD_PRINTSTR_EXIT:
	POP AX
	POP SI
	RET	
ENDP LCD_PRINTSTR




;sets the cursor
PROC LCD_SET_CUR
;input: DL=ROW, DH=COL
;		DL = 1, means upper row
;		DL = 2, means lower row
;		DH = 1-8, 1st column is 1
;output: none

;save registers
	PUSH AX
	PUSH DX
;LCD uses 0 based column index
	DEC DH
;select case	
	CMP DL,1
	JE	@ROW1
	CMP DL,2
	JE	@ROW2
	JMP @LCD_SET_CUR_END
	
;if DL==1 then
	@ROW1:
		MOV AH,80H
	JMP @LCD_SET_CUR_ENDCASE
	
;if DL==2 then
	@ROW2:
		MOV AH,0C0H
	JMP @LCD_SET_CUR_ENDCASE
		
;execute the command
	@LCD_SET_CUR_ENDCASE:	
	ADD AH,DH
	CALL LCD_CMD
	
;exit from procedure
	@LCD_SET_CUR_END:
	POP DX
	POP AX
	RET
ENDP LCD_SET_CUR






PROC LCD_SHOW_CUR
;input: none
;output: none
	PUSH AX
	MOV AH,0FH
	CALL LCD_CMD
	POP AX
	RET
ENDP LCD_SHOW_CUR




PROC LCD_HIDE_CUR
;input: none
;output: none
	PUSH AX
	MOV AH,0CH
	CALL LCD_CMD
	POP AX
	RET
ENDP LCD_HIDE_CUR



;sends data to output port and saves them in a variable
PROC OUT_A
;input: AL
;output: LCD_LCD_PORTA_VAL
	PUSH DX
	MOV DX,LCD_PORTA
	OUT DX,AL
	MOV LCD_PORTA_VAL,AL
	POP DX
	RET	
ENDP OUT_A


PROC OUT_B
;input: AL
;output: LCD_PORTB_VAL	
	PUSH DX
	MOV DX,LCD_PORTB
	OUT DX,AL
	MOV LCD_PORTB_VAL,AL
	POP DX
	RET
ENDP OUT_B

PROC OUT_C
;input: AL
;output: LCD_LCD_PORTC_VAL	
	PUSH DX
	MOV DX,LCD_PORTC
	OUT DX,AL
	MOV LCD_PORTC_VAL,AL
	POP DX
	RET
ENDP OUT_C




CODE ENDS ;end of CODE segment
END START ; set entry point and stop the assembler.
