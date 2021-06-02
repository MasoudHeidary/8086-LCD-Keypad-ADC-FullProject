DATA SEGMENT
;LCD DATA
;variables to keep track of port data
	LCD_PORTA_VAL DB 0
    LCD_PORTB_VAL DB 0
    LCD_PORTC_VAL DB 0
           
;port addresses  
    LCD_PORTA equ 00001000B 
	LCD_PORTB equ 00001010B 	
	LCD_PORTC equ 00001100B
	LCD_PORTR equ 00001110B	    

;END LCD DATA
	
;keypad define

    KeypadPortA equ 00000000B
    KeypadPortB equ 00000010B
    KeypadPortR equ 00000110B

    CountA equ 5
    CountB equ 8

    KeypadA DB 00010000B
            DB 00100000B
            DB 01000000B
            DB 00001000B
            
    KeypadB DB 00100000B
            DB 00000001B
            DB 01000000B
            DB 00010000B
            DB 00001000B
            DB 00000010B
            DB 00000100B
    
    NumA    DB 3
            DB 6
            DB 9
            DB 0FH
            
    NumB    DB 1
            DB 2
            DB 4
            DB 5
            DB 7
            DB 8
            DB 0
            
;Password
    Password equ 1234
    StringPasswordInput DB "Enter Password$"
    StringPasswordCorrect DB "Access Granted$"
    StringPasswordCorrect2 DB "Lock = 1$"
    StringPasswordInCorrect DB "Acess Denied$"
    StringPasswordInCorrect2 DB "Press 1$"
    
    
    PasswordInCorrectCounter DB 0   ;to save number of fault
    PasswordMAXInCorrect equ 2D     ;MAX number of incorrect

;Alarm
    AlarmPort equ 00000100B
    StringAlarm DB  "LOOK DOWN!$"    
    
ENDS

STACK SEGMENT
    DW   128  DUP(0)
ENDS


;/////////////////////////////////////////////MAIN Code
CODE SEGMENT
START:
; set segment registers: 
    MOV AX, DATA
    MOV DS, AX
    MOV ES, AX   
    	
	CALL LCD_INIT
	CALL KeypadInit

	
MainLoop:
    CALL LCD_CLEAR
    LEA SI,StringPasswordInput
    CALL LCD_PRINTSTR
    
    
    CALL KeypadRead     ;(AX)
    CMP AX,Password
    JE T
    JMP F
    
T:
    CALL PasswordCorrect
    JMP PasswordExit    
    
F:
    CALL PasswordInCorrect
    CALL AlarmCheck     ;(AL)
    

PasswordExit:     


    MOV CX,0FFFFH
    ;CALL DELAY
    MOV CX,0FFFFH
    LOOP MainLoop

ExitMain:	

	HLT
;end of main procedure
;/////////////////////////////////////////////END MAIN Code
                    
  
;//////////////////////////////////////////Password


;handle correct password
PasswordCorrect PROC Near
        PUSH AX
        PUSH DX
        PUSH SI
        
        MOV PasswordInCorrectCounter, 00H   ;set zero incorrect counter
        
        CALL LCD_CLEAR
        
        LEA SI,StringPasswordCorrect
        CALL LCD_PRINTSTR
        
        MOV DL,2
    	MOV DH,1
    	CALL LCD_SET_CUR
    	
    	LEA SI,StringPasswordCorrect2
    	CALL LCD_PRINTSTR
    
    LL:	
    	CALL KeypadWaitPress
    	CALL KeypadInput        ;(AL)
    	CALL KeypadWaitRelease
    	
    	CMP AL,01D              ;wait until press 1
    	JNE LL
        
        POP SI
        POP DX
        POP AX
        RET	
PasswordCorrect ENDP
 
 
;handle incorrect password
PasswordInCorrect PROC Near
        PUSH AX
        PUSH DX
        PUSH SI
        
        INC PasswordInCorrectCounter    ;increase incorrect counter
        
        CALL LCD_CLEAR
        
        LEA SI,StringPasswordInCorrect
        CALL LCD_PRINTSTR
        
        MOV DL,2
    	MOV DH,1
    	CALL LCD_SET_CUR        ;in row2
    	
    	LEA SI,StringPasswordInCorrect2
    	CALL LCD_PRINTSTR
    KK:
        CALL KeypadWaitPress
    	CALL KeypadInput        ;(AL)
    	CALL KeypadWaitRelease
    	
    	CMP AL,01D              ;wait until press 1
    	JNE KK
    	
    	POP SI
    	POP DX
    	POP AX
    	RET     
PasswordInCorrect ENDP

;//////////////////////////////////////////Alarm

;return (AL=1,AL=0):::(1:Alarm Active>
AlarmCheck PROC Near
        PUSH AX
        
        CMP PasswordInCorrectCounter,PasswordMAXInCorrect
        JB AlarmExit
                        
        CALL LCD_CLEAR
        LEA SI,StringAlarm
        CALL LCD_PRINTSTR                
        
        MOV AL,01H
    AlarmLoop:              
        OUT AlarmPort,AL
        NOT AL
        
        MOV CX,10000D
        CALL DELAY
        
        JMP AlarmLoop
        
        RET                     ;return with AL=1
    
    
    AlarmExit:
        POP AX
        MOV AL,00H              ;return AL=0
        RET 
    
AlarmCheck ENDP


;//////////////////////////////////////////Keypad

KeypadInit PROC Near
        PUSH AX
        PUSH DX
        
        MOV AL,10010010B
        MOV DX,KeypadPortR
        OUT DX,AL
        
        
        POP DX
        POP AX
        RET
KeypadInit ENDP


;for read a complete number
;stop reading with enter(0X0F)
;return AX
KeypadRead PROC Near
        PUSH BX
        PUSH CX
        PUSH DX
        MOV AX,0000H        ;save temp
        
        MOV DL,1
    	MOV DH,5
    	CALL LCD_SET_CUR
        
    LoopRead:
        PUSH AX             ;save number 
        
        CALL KeypadWaitPress
        CALL KeypadInput    ;AL
        CALL KeypadWaitRelease
        
        CMP AL,0FH
        JE KeypadReadExit
        
        MOV CX,0000H
        MOV CL,AL           ;CL=input
        
        POP AX              ;reload number
        MOV BX,10D
        MUL BX              ;in (DX,AX)
        
        ;MOV AX,DX           ;in AX
        MOV DH,00H
        ADD AX,CX           ;add input in main  
        
        CALL LCD_CLEAR
        CALL PrintBin
        JMP LoopRead
    
    KeypadReadExit:
        POP AX              ;reload number
        POP DX
        POP CX
        POP BX
        RET    
KeypadRead ENDP



;find press key and return
;<AL> unless return (AL=0XFF)
KeypadInput PROC Near
        PUSH AX
        PUSH BX
        
    	IN AL,KeypadPortB 	;Read B
    	MOV AH,AL           ;AH=B,AL=A
    	IN AL,KeypadPortA   ;Read A
    	
    	CMP AL,0FFH         ;check key in A
    	JNE CheckA          ;go find in A
    	CMP AH,0FFH         ;check key in B
    	JNE CheckB          ;go find in B
    	
        JMP NotFind         ;no key not press
    
    CheckA:	
    	MOV CX,CountA
    	MOV SI,00H
    LoopA:
        MOV BL,KeypadA[SI]  ;check in array
        TEST AL,BL
        JZ FindKeyA
        INC SI              ;not find go next 
        LOOP LoopA
    
        JMP NotFind         ;not find
    
    CheckB:    
        MOV CX,CountB
        MOV SI,00H
    LoopB:
        MOV BL,KeypadB[SI]  
        TEST AH,BL
        JZ FindKeyB
        INC SI
        LOOP LoopB
        
        JMP NotFind         ;not find
    	
    
    FindKeyA:
        POP AX
        MOV AL,NumA[SI]
        JMP ReadKeypadExit
        
    FindKeyB:
        POP AX
        MOV AL,NumB[SI]
        JMP ReadKeypadExit
    
    NotFind:
        POP AX
        MOV AL,0FFH
        JMP ReadKeypadExit
    
    ReadKeypadExit:
        POP BX
    	RET

KeypadInput ENDP 


;wait until press a key of keypad
KeypadWaitPress PROC Near
        PUSH AX
    CC:
        IN AL,KeypadPortB   ;read portB
        MOV AH,AL           ;AH=PortB, AL=PortA
        IN AL,KeypadPortA   ;read portA
        AND AL,AH
        
        CMP AL,0FFH
        JE CC
        
        POP AX
        RET
KeypadWaitPress ENDP


; wait until press key release
KeypadWaitRelease PROC Near
        PUSH AX
    AA:    
        IN AL,KeypadPortA
        CMP AL,0FFH
        JNE AA
    
    BB:
        IN AL,KeypadPortB
        CMP AL,0FFH
        JNE BB 
          
        POP AX
        RET
KeypadWaitRelease ENDP

;//////////////////////////////////////////Print 
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
;		LCD function library.(CORE)  ;
;                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PROC DELAY
;input: CX, this value controls the delay. CX=50 means 1ms
;output: none
	JCXZ @DELAY_END
	@DEL_LOOP:
	LOOP @DEL_LOOP	
	@DELAY_END:
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
