;******************************************************************************
;                                                                             *
;    Filename:                                                                *
;    Date:                                                                    *
;    File Version:                                                            *
;                                                                             *
;    Author:       G. Babski                                                  *
;    Company:      None                                                       *
;    License:                                                                 *
;******************************************************************************
;                                                                             *
;    Files Required: P18F2539.INC                                             *
;                                                                             *
;******************************************************************************

	LIST P=18F2539		;directive to define processor
	#include <P18F2539.INC>	;processor specific variable definitions

;******************************************************************************
;   Configuration:
    CONFIG	OSC = HSPLL			; 5MHz x PLL = 20MHz
	CONFIG	WDT = OFF			; Disable watchdog timer
	CONFIG	BOR = OFF			; Disable brownout reset
	CONFIG  LVP = OFF			; Disable low voltage programming
	CONFIG  PWRT = ON			; Enable power up timer
	CONFIG  DEBUG = OFF			; Disable debug

;******************************************************************************
;   Variable definitions:

		CBLOCK	0x080
		TempAvailable           ;Indicates that a temperature reading has started (adc)
		NumH                    ;High byte input for binary to ascii conversion
		NumL                    ;Low byte input for binary to ascii conversion
		ButtonValue             ;The value of the pressed key
		SetpointAvailable       ;Indicates that a new setpoint should be sent to LCD
		SetpointH               ;The setpoint (10-bit) high byte
		SetpointL               ;The setpoint (10-bit) low byte
		TenK    				;Holds 10000's digit
		Thou    				;Holds 1000's digit
		Hund     				;Holds 100's digit
		Tens    				;Holds 10's digit
		Ones    				;Holds 1's digit
		templcd			        ;Temp store for LCD 4 bit mode
		count1			        ;Used in delay routine
		counta			        ;Used in delay routine
		countb			        ;Used in delay routine
		SubH                    ;Used for subtraction
		SubL                    ;Used for subtraction
		SpiH                    ;Used for 10-bit to 12-bit conversion
		SpiL                    ;Used for 10-bit to 12-bit conversion
		ENDC

LCD_CTRL_PORT	Equ	LATC
LCD_DATA_PORT	Equ	LATB
LCD_RS			Equ	RC0			;LCD handshake lines
LCD_E			Equ	RC1

;******************************************************************************
;Reset vector
		ORG	0x0000

		goto	Init			;go to start of main code

;******************************************************************************
;High priority interrupt vector
		ORG	0x0008

		bra	HighInt				;go to high priority interrupt routine

;******************************************************************************
;High priority interrupt routine
HighInt:
		btfsc   INTCON, TMR0IF  ; Test TIMER0 Interrupt flag
	    goto    intTMR0IF       ; goto correct code otherwise
		btfsc   INTCON, RBIF  	; Test RB Interrupt flag
	    goto    intRBIF       	; goto correct code otherwise
	    retfie                  ; return.

; Timer0 interrupt handler
intTMR0IF:                      ; Handle timer0 interrupt
		bsf		ADCON0, 2       ; Start ADC conversion
		setf	TempAvailable   ; Tell main loop to check for ADC completion
		movlw	0xB3            ; Set timer0 to fire in ~ 1s, since temp display is informative, not critical
		movwf	TMR0H           ; FFFF - B3B4 = 4C4B = 19543 timer ticks
		movlw	0xB4            ; We should adjust these up to compensate for instructions executed in interrupt.
		movwf	TMR0L           ; Always write TMR0H first then TMR0L for simultaneous update.
	    bcf     INTCON, TMR0IF  ; Clear TIMER0 interrupt flag.
		retfie	FAST

; PORTB interrupt on change handler
intRBIF:
		movff	PORTB, ButtonValue  ; Load PORTB - clears mismatch for interrupt on change
		movlw	b'11100000'
		andwf	ButtonValue, 1		; Clear lower 5 bits - not part of key input
		movlw	b'00000000'
		xorwf	ButtonValue, 0      ; Test if RB5:RB7 clear - this happens when a button is released
		bz		intRBIFOut          ; Ignore - we only handle the key down events

		MOVLW	b'00100000'         
		XORWF	ButtonValue, 0      ; Test if button 1 was pressed
		BNZ		But2                ; Not button 1, try 2
		movlw	d'1'                ; We will add 0.1 C to set point
		goto	AddDelta
But2:
        MOVLW	b'01000000'         
		XORWF	ButtonValue, 0      ; Test if button 2 was pressed
		BNZ		But3                ; Not button 2, try 3
		movlw	d'10'               ; We will add 1 C to set point
		goto	AddDelta
But3:
        MOVLW	b'01100000'         
		XORWF	ButtonValue, 0      ; Test if button 3 was pressed
		BNZ		But4                ; Not button 3, try 4
		movlw	d'100'              ; We will add 10 C to set point
		goto	AddDelta
But4:
        MOVLW	b'10000000'         
		XORWF	ButtonValue, 0      ; Test if button 4 was pressed
		BNZ		But5                ; Not button 4, try 5
		movlw	d'1'                ; We will subtract 0.1 C from set point
		goto	SubDelta
But5:
        MOVLW	b'10100000'         
		XORWF	ButtonValue, 0      ; Test if button 5 was pressed
		BNZ		But6                ; Not button 5, try 6
		movlw	d'10'               ; We will subtract 1 C from set point
		goto	SubDelta
But6:
        MOVLW	B'11000000'         
		XORWF	ButtonValue, 0      ; Test if button 6 was pressed
		BNZ		intRBIFOut          ; Not button 6, should never happen. Oh well - lets just ignore this :)
		movlw	d'100'              ; We will subtract 10 C from set point
		goto	SubDelta

AddDelta:
		addwf	SetpointL, 1        ; Add delta (in W) to SetpointL
		btfsc   STATUS,C            ; If a bit was carried
		incf	SetpointH, 1        ; Add it to SetpointH

		movlw	0xE7                ; Represents max temp = 99.9 C
		movwf	SubL
		movlw	0x03
		movwf	SubH
		movf	SetpointL, W
		subwf	SubL                ; SubL = SubL - SetpointL
		movf	SetpointH, W
		btfss	STATUS, C           ; If we needed to borrow a bit
		decf	SubH                ; Borrow it from SubH
		subwf	SubH                ; SubH = SubH - SetpointH

		btfss	SubH, 7             ; If Sub >= 0 then Setpoint <= 99.9 C
		goto 	AddOut              ; Everything is OK!
		movlw	0xE7                ; Else force Setpoint = 99.9 C
		movwf	SetpointL
		movlw	0x03
		movwf	SetpointH
AddOut:
		clrf	SetpointAvailable   ; Tell main loop to display new Setpoint
		goto	SendToDAC
SubDelta:
		subwf	SetpointL, 1        ; SetpointL = SetpointL - W
		btfss   STATUS,C            ; If we needed to borrow a bit
		decf	SetpointH, 1        ; Borrow it from SetpointH
		btfss	SetpointH, 7        ; If Setpoint >=0 
		goto 	SubOut              ; Everything is OK!
		clrf	SetpointL           ; Else force Setpoint = 0.0 C
		clrf	SetpointH
SubOut:
		clrf	SetpointAvailable   ; Tell main loop to display new Setpoint

SendToDAC:
        movff   SetpointH, SpiH     ; We need to rotate right by 2 bits
        movff	SetpointL, SpiL     ; Our value is 10-bit, DAC wants 12-bit
        bcf		STATUS, C           ; Clear carry (so rotate pulls 0 in on right)
        rlcf	SpiL                ; Rotate low byte once
		rlcf	SpiH                ; Rotate high byte once - carry has top bit of low byte
        bcf		STATUS, C           ; Clear carry (so rotate pulls 0 in on right)
        rlcf	SpiL                ; Rotate low byte once
		rlcf	SpiH                ; Rotate high byte once - carry has top bit of low byte
		
		BCF		LATC, RC2			; Tells device that command is coming
		movf	SpiH, W             ; Load high data byte
		iorlw	b'01110000'         ; Set DAC config (top 4 bits)
		movwf   SSPBUF              ; Send via SPI
SpiWait1:
		btfss   SSPSTAT,BF			; Has data been received and sent?
		bra     SpiWait1			; Loop if not received yet
  		movf    SSPBUF,W			; Empty the receive buffer - required to prevent overflow flag

		movf	SpiL, W             ; Load low data byte
		movwf   SSPBUF              ; Send via SPI
SpiWait2:
		btfss   SSPSTAT,BF			; Has data been received and sent?
		bra     SpiWait2			; Loop if not received yet
  		movf    SSPBUF,W			; Empty the receive buffer - required to prevent overflow flag

		bsf		LATC, RC2		    ; Tells device that command is ended & DAC can latch output voltage
intRBIFOut:
	    bcf     INTCON, RBIF        ; Clear RB interrupt flag.

		retfie	FAST

;******************************************************************************
;Start of main program
; The main program code is placed here.

Init:
		clrf	LATB                ; Clear PORTB (all zero)
		movlw	b'11100000'         ; Configure PORTB, RB5:RB7 inputs (keypad), RB4 ouput (unused), RB0:RB3 outputs (LCD)
		movwf	TRISB

		clrf	LATC                ; Clear PORTC (all zero)
		bsf		LATC, RC2           ; Set RC2 of PORTC - this is the DAC CS flag which is idle high
		movlw	b'11010000'         ; Configure PORTC
		movwf	TRISC

;SPI Mode 1,1
		clrf	SSPCON1
		movlw	b'00110000'		; Set up SPI - Master, Fosc/4
		movwf	SSPCON1
		clrf	SSPSTAT
		movlw	b'10000000'		; Sample on end, Rising Clock, SPI MODE 1,1
		movwf	SSPSTAT

		clrf	SetpointAvailable
		clrf	SetpointH
		clrf	SetpointL

		MOVLW	b'11001111'     ; ADC 
		MOVWF	ADCON1
		MOVLW	b'10000001'     ; ADC
		MOVWF	ADCON0
		BSF		ADCON0, 2       ; Start first ADC conversion
		setf	TempAvailable   ; Tell main loop to expect a temperature value

	    bsf     RCON, IPEN      ; Enable priority levels
	    bsf     INTCON, GIEH    ; Enable high priority interrupts

	    bcf     INTCON, TMR0IF  ; Clear the TIMER0 interrupt flag
	    bsf     INTCON, TMR0IE  ; Enable the TIMER0 interrupt
	    bcf     INTCON, RBIF	; Clear the RB interrupt flag
	    bsf     INTCON, RBIE	; Enable the RB interrupt

        ; Need to switch to a table lookup
        ; Intent - set LCD static text
		call	Delay100
		call	LCD_Init
		movlw	'C'
		call	LCD_Char
		movlw	'u'
		call	LCD_Char
		movlw	'r'
		call	LCD_Char
		movlw	'r'
		call	LCD_Char
		movlw	'e'
		call	LCD_Char
		movlw	'n'
		call	LCD_Char
		movlw	't'
		call	LCD_Char
		movlw	':'
		call	LCD_Char
		movlw	d'14'
		call	LCD_Line1W
		movlw	b'11011111'
		call	LCD_Char
		movlw	'C'
		call	LCD_Char

		call	LCD_Line2
		movlw	'S'
		call	LCD_Char
		movlw	'e'
		call	LCD_Char
		movlw	't'
		call	LCD_Char
		movlw	'p'
		call	LCD_Char
		movlw	'o'
		call	LCD_Char
		movlw	'i'
		call	LCD_Char
		movlw	'n'
		call	LCD_Char
		movlw	't'
		call	LCD_Char
		movlw	':'
		call	LCD_Char
		movlw	d'14'
		call	LCD_Line2W
		movlw	b'11011111'
		call	LCD_Char
		movlw	'C'
		call	LCD_Char

		movlw	0xB3            ; Set timer0 to fire in ~ 1s, since first ADC is already under way
		movwf	TMR0H
		movlw	0xB4
		movwf	TMR0L
 		movlw   b'10000111'     ; Configure and enable TIMER0.
	    movwf   T0CON

;	*** main code goes here ***
Main:
		tstfsz	SetpointAvailable   ; Is new setpoint available?
		goto	CheckTemp           ; No - skip setpoint update
		movff	SetpointH, NumH     ; Yes - load setpoint
		movff	SetpointL, NumL
		setf	SetpointAvailable   ; Clear setpoint available
		call	BinToBcd16Bit       ; Convert setpoint from binary to ascii
		movlw	d'10'               
		call	LCD_Line2W          ; Move to column 10 in line 2 on LCD
		call	ShowTemp            ; Display value in TenK, Thou, Hund, Tens and Ones as temperature
        
CheckTemp:
		tstfsz	TempAvailable       ; Is new temperature being converted?
		btfsc	ADCON0, 2           ; Is conversion done?
		goto	Main                ; No (either question) - do main loop again
		setf	TempAvailable       ; Yes - clear temperature availability
		movff	ADRESH, NumH        ; Load temperature from ADC    
		movff	ADRESL, NumL
		call	BinToBcd16Bit       ; Convert temperature from binary to ascii
		movlw	d'10'
		call	LCD_Line1W          ; Move to column 10 in line 1 on LCD
		call	ShowTemp            ; Display value in TenK, Thou, Hund, Tens and Ones as temperature

		goto	Main			    ; Infinite Loop

; Initialise LCD - No arguments
LCD_Init
		movlw	0x20			;Set 4 bit mode
		call	LCD_Cmd
		call	Delay255

		movlw	0x28			;Set display character mode
		call	LCD_Cmd
		call	Delay255

		movlw	0x06			;Set display shift
		call	LCD_Cmd
		call	Delay255

		movlw	0x0c			;Set display on/off and cursor command
		call	LCD_Cmd
		call	Delay255

		call	LCD_Clr			;clear display
		call	Delay255
		retlw	0x00

; LCD command set routine - arguments: W = command
LCD_Cmd
		movwf	templcd
		swapf	templcd,	w	;send upper nibble
		andlw	0x0f			;clear upper 4 bits of W
		movwf	LCD_DATA_PORT
		bcf		LCD_CTRL_PORT, LCD_RS	;RS line to 0
		call	Pulse_e			;Pulse the E line high

		movf	templcd,	w
		andlw	0x0f			;clear upper 4 bits of W
		movwf	LCD_DATA_PORT
		bcf		LCD_CTRL_PORT, LCD_RS	;RS line to 0
		call	Pulse_e			;Pulse the E line high
		call 	Delay5
		retlw	0x00

; LCD show digit routine - arguments: W = digit (binary) 0-9
LCD_CharD	addlw	0x30
; LCD show character routine - arguments: W = character (ascii)
LCD_Char
		movwf	templcd         ; Save original W
		swapf	templcd,	w	; Swap nibbles - to send upper nibble first
		andlw	0x0f			; Clear upper 4 bits of W
		movwf	LCD_DATA_PORT   ; Send lower bits to PORTB
		bsf		LCD_CTRL_PORT, LCD_RS	;RS line to 1
		call	Pulse_e			;Pulse the E line high

		movf	templcd,	w   ; Load original message
		andlw	0x0f			; Clear upper 4 bits of W
		movwf	LCD_DATA_PORT   ; Send lower bits to PORTB
		bsf		LCD_CTRL_PORT, LCD_RS	;RS line to 1
		call	Pulse_e			;Pulse the E line high
		call 	Delay5
		call	Delay255
		retlw	0x00

; LCD line 1 reset routine - No arguments
LCD_Line1	movlw	0x80			;move to 1st row, first column
		call	LCD_Cmd
		retlw	0x00

; LCD line 2 reset routine - No arguments
LCD_Line2	movlw	0xc0			;move to 2nd row, first column
		call	LCD_Cmd
		retlw	0x00

; LCD line 1 column move routine - arguments: W = column
LCD_Line1W	addlw	0x80			;move to 1st row, column W
		call	LCD_Cmd
		retlw	0x00

; LCD line 2 column move routine - arguments: W = column
LCD_Line2W	addlw	0xc0			;move to 2nd row, column W
		call	LCD_Cmd
		retlw	0x00

; LCD clear display routine - No arguments
LCD_Clr		movlw	0x01			;Clear display
		call	LCD_Cmd
		retlw	0x00

Delay255	movlw	0xff		;delay 255 mS
		goto	d0
Delay100	movlw	d'100'		;delay 100mS
		goto	d0
Delay50		movlw	d'50'		;delay 50mS
		goto	d0
Delay20		movlw	d'20'		;delay 20mS
		goto	d0
Delay5		movlw	0x05		;delay 5.000 ms (20 MHz clock)
d0		movwf	count1
d1		movlw	0xC7			;delay 1mS
		movwf	counta
		movlw	0x05
		movwf	countb
Delay_0
		decfsz	counta, f
		goto	$+2
		decfsz	countb, f
		goto	Delay_0

		decfsz	count1	,f
		goto	d1
		retlw	0x00

Pulse_e	bsf	LCD_CTRL_PORT, LCD_E
		nop
		nop
		bcf	LCD_CTRL_PORT, LCD_E
		nop
		nop
		retlw	0x00

ShowTemp
		TSTFSZ	TenK
		goto ShowError
		TSTFSZ	Thou
		goto ShowError
		movf	Hund, W
		call	LCD_CharD
		movf	Tens, W
		call	LCD_CharD
		movlw	'.'
		call	LCD_Char
		movf	Ones, W
		call	LCD_CharD
		RETURN

ShowError
		movlw	'E'
		call	LCD_Char	
		movlw	'r'
		call	LCD_Char	
		movlw	'r'
		call	LCD_Char	
		movlw	' '
		call	LCD_Char	
		return

;----------------------------------------------------------
; BinToBcd16Bit - Converts a 16 bit 
;        number to BCD for display.
;
; Usage: Place value to be converted in 
;    NumH:NumL
;
; Vars:    
;        _ui_TenK    - Holds 10000's digit
;        _ui_Thou    - Holds 1000's digit
;        _ui_Hund     - Holds 100's digit
;        _ui_Tens    - Holds 10's digit
;        _ui_Ones    - Holds 1's digit
;
; *** NOTE *** This is the PIC18 Version. 
;
; Original code :
; addwf Ones,f ; B0 += 10
; decf Tens,f ; B1 -= 1
; btfss 3,0
;
; Modified code :
; decf Tens,f ; B1 -= 1
; addwf Ones,f ; B0 += 10
; btfss STATUS, C, ACCESS
;
; Do it for all digits...+
;----------------------------------------------------------
BinToBcd16Bit
       swapf   NumH,w 
       andlw   0x0F             
       addlw   0xF0             
       movwf   Thou 
       addwf   Thou,f 
       addlw   0xE2 
       movwf   Hund 
       addlw   0x32 
       movwf   Ones 

       movf    NumH,w 
       andlw   0x0F 
       addwf   Hund,f 
       addwf   Hund,f 
       addwf   Ones,f 
       addlw   0xE9 
       movwf   Tens 
       addwf   Tens,f 
       addwf   Tens,f 

       swapf   NumL,w 
       andlw   0x0F 
       addwf   Tens,f 
       addwf   Ones,f 

       rlcf     Tens,f 
       rlcf     Ones,f 
       comf     Ones,f 
       rlcf     Ones,f 

       movf     NumL,w 
       andlw    0x0F 
       addwf    Ones,f 
       rlcf     Thou,f 

       movlw   0x07 
       movwf   TenK 

       movlw   0x0A
Lb1: 
       ;addwf   Ones,f 
       decf     Tens,f 
       addwf    Ones,f 
       btfss   STATUS,C ; Much easier to read! Good coding practice!
        goto   Lb1 
Lb2: 
       ;addwf  Tens,f 
       decf    Hund,f 
       addwf   Tens,f 
       btfss   STATUS,C
        goto   Lb2 
Lb3: 
       ;addwf   Hund,f 
       decf    Thou,f 
       addwf   Hund,f 
       btfss   STATUS,C
        goto   Lb3 
Lb4: 
       ;addwf   Thou,f 
       decf    TenK,f 
       addwf   Thou,f 
       btfss   STATUS,C
        goto   Lb4 

       return

		END
