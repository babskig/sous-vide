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
;Configuration bits
;Microchip has changed the format for defining the configuration bits, please 
;see the .inc file for futher details on notation.  Below are a few examples.

;   Configuration:
    CONFIG	OSC = HSPLL			; 5MHz x PLL = 20MHz
	CONFIG	WDT = OFF			; Disable watchdog timer
	CONFIG	BOR = OFF			; Disable brownout reset
	CONFIG  LVP = OFF			; Disable low voltage programming
	CONFIG  PWRT = ON			; Enable power up timer
	CONFIG  DEBUG = OFF			; Disable debug

;******************************************************************************
;Variable definitions
; These variables are only needed if low priority interrupts are used. 
; More variables may be needed to store other special function registers used
; in the interrupt routines.

		CBLOCK	0x080
		WREG_TEMP	;variable used for context saving 
		STATUS_TEMP	;variable used for context saving
		BSR_TEMP	;variable used for context saving
		TempAvailable
		NumH
		NumL
		ButtonValue
		SetpointAvailable
		SetpointH
		SetpointL
		TenK    					;Holds 10000's digit
		Thou    					;Holds 1000's digit
		Hund     					;Holds 100's digit
		Tens    					;Holds 10's digit
		Ones    					;Holds 1's digit
		templcd			;temp store for 4 bit mode
		templcd2
		count1			;used in delay routine
		counta			;used in delay routine
		countb			;used in delay routine
		SubH
		SubL
		SpiH
		SpiL
		ENDC

LCD_CTRL_PORT	Equ	LATC
LCD_DATA_PORT	Equ	LATB
LCD_RS			Equ	RC0			;LCD handshake lines
LCD_E			Equ	RC1

;******************************************************************************
;Reset vector
; This code will start executing when a reset occurs.

		ORG	0x0000

		goto	Init		;go to start of main code

;******************************************************************************
;High priority interrupt vector
; This code will start executing when a high priority interrupt occurs or
; when any interrupt occurs if interrupt priorities are not enabled.

		ORG	0x0008

		bra	HighInt		;go to high priority interrupt routine

;******************************************************************************
;Low priority interrupt vector and routine
; This code will start executing when a low priority interrupt occurs.
; This code can be removed if low priority interrupts are not used.

		ORG	0x0018

		movff	STATUS,STATUS_TEMP	;save STATUS register
		movff	WREG,WREG_TEMP		;save working register
		movff	BSR,BSR_TEMP		;save BSR register

;	*** low priority interrupt code goes here ***


		movff	BSR_TEMP,BSR		;restore BSR register
		movff	WREG_TEMP,WREG		;restore working register
		movff	STATUS_TEMP,STATUS	;restore STATUS register
		retfie

;******************************************************************************
;High priority interrupt routine
; The high priority interrupt code is placed here to avoid conflicting with
; the low priority interrupt vector.

HighInt:

;	*** high priority interrupt code goes here ***
		btfsc   INTCON, TMR0IF  ; Test TIMER0 Interrupt flag
	    goto    intTMR0IF       ;  and goto correct code otherwise

		btfsc   INTCON, RBIF  	; Test RB Interrupt flag
	    goto    intRBIF       	;  and goto correct code otherwise

	    retfie                  ;  return.
	
intTMR0IF:
		BSF		ADCON0, 2
		setf	TempAvailable
		movlw	0xB3
		movwf	TMR0H
		movlw	0xB4
		movwf	TMR0L
	    bcf     INTCON, TMR0IF  ; Clear TIMER0 interrupt flag.

		retfie	FAST

intRBIF:
		movff	PORTB, ButtonValue
		movlw	b'11100000'
		andwf	ButtonValue, 1		; Store in Value
		movlw	b'00000000'
		xorwf	ButtonValue, 0
		BZ		intRBIFOut

		bcf		STATUS,C
		rrncf	ButtonValue
		rrncf	ButtonValue
		rrncf	ButtonValue
		rrncf	ButtonValue
		rrncf	ButtonValue

		MOVLW	d'1'
		XORWF	ButtonValue, 0
		BNZ		But2
		movlw	d'1'
		goto	AddDelta
But2:
		MOVLW	d'2'
		XORWF	ButtonValue, 0
		BNZ		But3
		movlw	d'10'
		goto	AddDelta
But3:
		MOVLW	d'3'
		XORWF	ButtonValue, 0
		BNZ		But4
		movlw	d'100'
		goto	AddDelta
But4:
		MOVLW	d'4'
		XORWF	ButtonValue, 0
		BNZ		But5
		movlw	d'1'
		goto	SubDelta
But5:
		MOVLW	d'5'
		XORWF	ButtonValue, 0
		BNZ		But6
		movlw	d'10'
		goto	SubDelta
But6:
		MOVLW	d'6'
		XORWF	ButtonValue, 0
		BNZ		intRBIFOut
		movlw	d'100'
		goto	SubDelta

AddDelta:
		addwf	SetpointL, 1
		btfsc   STATUS,C
		incf	SetpointH, 1

		movlw	0xE7
		movwf	SubL
		movlw	0x03
		movwf	SubH
		movf	SetpointL, W
		subwf	SubL
		movf	SetpointH, W
		btfss	STATUS, C
		decf	SubH
		subwf	SubH

		btfss	SubH, 7
		goto 	AddOut
		movlw	0xE7
		movwf	SetpointL
		movlw	0x03
		movwf	SetpointH
AddOut:
		clrf	SetpointAvailable
		goto	SendToDAC
SubDelta:
		subwf	SetpointL, 1
		btfss   STATUS,C
		decf	SetpointH, 1
		btfss	SetpointH, 7
		goto 	SubOut
		clrf	SetpointL
		clrf	SetpointH
SubOut:
		clrf	SetpointAvailable

SendToDAC:
		movf	SetpointH, W
		movwf	SpiH
		rlncf	SpiH
		rlncf	SpiH
		movf	SetpointL, W
		movwf	SpiL
		bcf		STATUS, C
		rlcf	SpiL
		btfsc	STATUS, C
		bsf		SpiH, 1
		bcf		STATUS, C
		rlcf	SpiL
		btfsc	STATUS, C
		bsf		SpiH, 0

		BCF		LATC, RC2			; Tells device that command is coming
		movf	SpiH, W
		iorlw	b'01110000'
		movwf   SSPBUF
SpiWait1:
		btfss   SSPSTAT,BF			; has data been received?
		bra     SpiWait1			; loop if not received yet
  		movf    SSPBUF,W			; empty the receive buffer

		movf	SpiL, W
		movwf   SSPBUF
SpiWait2:
		btfss   SSPSTAT,BF			; has data been received?
		bra     SpiWait2			; loop if not received yet
  		movf    SSPBUF,W			; empty the receive buffer

		BSF		LATC, RC2		; Tells device that command is ended
intRBIFOut:
	    bcf     INTCON, RBIF  ; Clear interrupt flag.

		retfie	FAST

;******************************************************************************
;Start of main program
; The main program code is placed here.

Init:
		CLRF	LATB
		MOVLW	b'11100000'
		MOVWF	TRISB

		CLRF	LATC
		BSF		LATC, RC2
		MOVLW	b'11010000'
		MOVWF	TRISC

;SPI Mode 1,1
		clrf	SSPCON1
		movlw	b'00110010'		; Set up SPI - Master
		movwf	SSPCON1
		clrf	SSPSTAT
		movlw	b'10000000'		; Sample on end, Rising Clock
		movwf	SSPSTAT


		clrf	SetpointAvailable
		clrf	SetpointH
		clrf	SetpointL

		MOVLW	b'11001111'
		MOVWF	ADCON1
		MOVLW	b'10000001'
		MOVWF	ADCON0
		BSF		ADCON0, 2
		setf	TempAvailable

	    bsf     RCON, IPEN      ; Enable priority levels, and
	    bsf     INTCON, GIEH    ;  high priority interrupts.

	    bcf     INTCON, TMR0IF  ; Clear the TIMER0 interrupt flag
	    bsf     INTCON, TMR0IE  ; Enable the TIMER0 interrupt
	    bcf     INTCON, RBIF	; Clear the RB interrupt flag
	    bsf     INTCON, RBIE	; Enable the RB interrupt

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

		movlw	0xB3
		movwf	TMR0H
		movlw	0xB4
		movwf	TMR0L
 		movlw   b'10000111'     ; Configure and enable TIMER0.
	    movwf   T0CON

;	*** main code goes here ***
Main:
		tstfsz	SetpointAvailable
		goto	CheckTemp
		movff	SetpointH, NumH
		movff	SetpointL, NumL
		setf	SetpointAvailable
		call	BinToBcd16Bit
		movlw	d'10'
		call	LCD_Line2W
		call	ShowTemp

CheckTemp:
		tstfsz	TempAvailable
		btfsc	ADCON0, 2
		goto	Main
		setf	TempAvailable
		movff	ADRESH, NumH
		movff	ADRESL, NumL
		call	BinToBcd16Bit
		movlw	d'10'
		call	LCD_Line1W
		call	ShowTemp

		goto	Main			; Infinite Loop

;******************************************************************************
;End of program

;Initialise LCD
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

; command set routine
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

LCD_CharD	addlw	0x30
LCD_Char
		movwf	templcd
		swapf	templcd,	w	;send upper nibble
		andlw	0x0f			;clear upper 4 bits of W
		movwf	LCD_DATA_PORT
		bsf		LCD_CTRL_PORT, LCD_RS	;RS line to 1
		call	Pulse_e			;Pulse the E line high

		movf	templcd,	w
		andlw	0x0f			;clear upper 4 bits of W
		movwf	LCD_DATA_PORT
		bsf		LCD_CTRL_PORT, LCD_RS	;RS line to 1
		call	Pulse_e			;Pulse the E line high
		call 	Delay5
		call	Delay255
		retlw	0x00

LCD_Line1	movlw	0x80			;move to 1st row, first column
		call	LCD_Cmd
		retlw	0x00

LCD_Line2	movlw	0xc0			;move to 2nd row, first column
		call	LCD_Cmd
		retlw	0x00

LCD_Line1W	addlw	0x80			;move to 1st row, column W
		call	LCD_Cmd
		retlw	0x00

LCD_Line2W	addlw	0xc0			;move to 2nd row, column W
		call	LCD_Cmd
		retlw	0x00

LCD_CurOn	movlw	0x0d			;Set display on/off and cursor command
		call	LCD_Cmd
		retlw	0x00

LCD_CurOff	movlw	0x0c			;Set display on/off and cursor command
		call	LCD_Cmd
		retlw	0x00

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
Delay5		movlw	0x05		;delay 5.000 ms (4 MHz clock)
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

ShowTemp2
		TSTFSZ	TenK
		movf	TenK, W
		call	LCD_CharD
		TSTFSZ	Thou
		movf	Thou, W
		call	LCD_CharD
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
