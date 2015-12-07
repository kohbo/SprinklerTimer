;
; Sprinkler Project.asm
;
; Created: 12/1/2015 10:21:57
; Author : Juan Menendez
;

;Relay Output Port B Pin 0 (J2 ADC0)
;Light Sensor Port A Pin 0 (J3 PA0)

			.include	<atxmega256a3budef.inc>
			.dseg
RanToday:	.byte	0					;area to store True/False if ran today
			.cseg
			.org	0x00
			jmp		start
			.org	TCF0_OVF_VECT
			JMP		lightISR
			.org	0xf6				;skip vector tables

start:		
			//init stack
			LDI		R16, low(RAMEND)
			STS		CPU_SPL, R16
			LDI		R16, high(RAMEND)
			STS		CPU_SPL, R16

			//global interrupt init
			LDI		R16, 0b0000110		;Enable high and medium level interrupts
			STS		PMIC_CTRL, R16
			SEI							;Enable global interrupt

			//init pins to relay
			LDI		R16, 0x1			;Set pin direction out
			STS		PORTB_DIR, R16		;*
			LDI		R16, 0				;Ensure pin is low
			STS		PORTB_OUT, R16		;*

			//init light sensor pin
			LDS		R16, PORTA_DIR		;Set light sensor pin as input
			ANDI	R16, 0xFE			;*
			STS		PORTA_DIR, R16		;*
			LDI		R16, 0b00000010		;Set pin to detect falling edge
			STS		PORTA_PIN0CTRL, R16	;*
			//LDI		R16, 0x0A			; Enable port med interrupt
			//STS		PORTA_INTCTRL, R16  ;*
			//LDI		R16, 0x01			;Enable interrupt for pin 0
			//STS		PORTA_INT0MASK, R16		;*
			
			LDI		R16, 0b00001111		;Enable extended power-save sleep
			STS		SLEEP_CTRL, R16		;*

			//Initialize LCD
			CALL		InitTheLCD				; Initialize the LCD
			CALL		LCDBackLightOn		
			CALL		LCDClearScreen
			CALL		StatScreen

			// Writing to screen procedure
			// SetLCDCurser
			// R0 = col; R1 = row
			// LCDWriteHexDigit
			// R0 = LCDData index
			
			//This macro adds one digit to the screen
			// @0 - set col
			// @1 - set row
			// @2 - set character
			.macro		PRINT
			LDI			R16, @0
			MOV			R0, R16
			LDI			R16, @1
			MOV			R1, R16
			Call		LCDSetCurser
			
			LDI			R16, @2
			MOV			R0, R16
			Call		LCDWriteHexDigit
			.endmacro
				
			//LDI		R16, 0xD8			; Disable CCP
			//STS		CPU_CCP, R16
			//LDI		R16, 0x02			; Set CLK_SYS to 32kHz
			//STS		CLK_CTRL, R16
			
			LDI		R16, 0xD8			; Disable CCP
			STS		CPU_CCP, R16
			LDI		R16, 0x3C			; Set Prescaler to CLK/256
			STS		CLK_PSCTRL, R16		
			
			//set up timer/counter
			LDI		R16, 0				; Set to normal mode
			STS		TCF0_CTRLB, R16
			LDI		R16, 0x0F			; Set err and ovf int to high
			STS		TCF0_INTCTRLA, R16
			LDI		R16, low(9766)		; Set period
			STS		TCF0_PER, R16
			LDI		R16, high(9766)
			STS		TCF0_PER+1, R16
			LDI		R16, 0x03			; Enable TCF0
			STS		TCF0_CTRLA, R16

			JMP		SysSleep

statDay:
			PUSH	R16
			PUSH	R0
			PUSH	R1
			
			//Print "DAY  "
			PRINT	42,0,13
			PRINT	48,0,10
			PRINT	54,0,34
			PRINT	60,0,36
			PRINT	66,0,36

			POP		R1
			POP		R0
			POP		R16
			RET

statNight:
			PUSH	R16
			PUSH	R0
			PUSH	R1
			
			//print "NIGHT"
			PRINT	42,0,23
			PRINT	48,0,18
			PRINT	54,0,16
			PRINT	60,0,17
			PRINT	66,0,29

			POP		R1
			POP		R0
			POP		R16
			RET

statTrue:	
			PUSH	R16
			PUSH	R0
			PUSH	R1
			
			//print "TRUE "
			PRINT	72,1,29
			PRINT	78,1,27
			PRINT	84,1,30
			PRINT	90,1,14
			PRINT	96,1,36

			POP		R1
			POP		R0
			POP		R16
			RET

statFalse:
			PUSH	R16
			PUSH	R0
			PUSH	R1
			
			//print "FALSE"
			PRINT	72,1,15
			PRINT	78,1,10
			PRINT	84,1,21
			PRINT	90,1,28
			PRINT	96,1,14

			POP		R1
			POP		R0
			POP		R16
			RET

lightISR:	
			PUSH	R16
			LDS		R16, PORTA_IN		; check it light pin set to 1
			ANDI	R16, 0x01			; *
			CPI		R16, 0x01			; *
			BRNE	ClrRTFlag			; If not day, clear ran today flag and do nothing else
			CALL	statDay				; Show day status on LCD
			LDS		R16, RanToday		; Check if ran today flag set
			CPI		R16, 1
			BREQ	closeValve			; ensure valve is closed if ran today

SetRTFlag:	LDI		R16, 1				; Set the RanToday flag
			STS		RanToday, R16
			CALL	statTrue
			POP		R16						

openValve:	LDI		R16, 0x1
			STS		PORTB_OUT, R16
			JMP		ltISRdone

closeValve:	LDI		R16, 0x0
			STS		PORTB_OUT, R16
			JMP		ltISRdone

ClrRTFlag:	CALL	statNight
			CALL	statFalse
			CLR		R16
			STS		RanToday, R16
			POP		R16

ltISRdone:	POP		R16
			RETI

SysSleep:	//SLEEP
			rjmp		SysSleep

// ******************************************************
// *					LCD Module						*
// ******************************************************

// ***********************************************************************************
// *								LCD Font Section                                 *
// ***********************************************************************************
				LCDData:
				LCDData0:		.DB			0x3E,0x45,0x49,0x51,0x3E,0x00 ; digit 0
				LCDData1:		.DB			0x00,0x00,0x7F,0x00,0x00,0x00 ; digit 1
				LCDData2:		.DB			0x32,0x49,0x49,0x49,0x06,0x00 ; digit 2
				LCDData3:		.DB			0x41,0x49,0x49,0x49,0x36,0x00 ; digit 3
				LCDData4:		.DB			0x0F,0x08,0x7E,0x08,0x08,0x00 ; digit 4
				LCDData5:		.DB			0x2F,0x49,0x49,0x49,0x31,0x00 ; digit 5
				LCDData6:		.DB			0x3E,0x49,0x49,0x49,0x32,0x00 ; digit 6
				LCDData7:		.DB			0x41,0x21,0x11,0x09,0x07,0x00 ; digit 7
				LCDData8:		.DB			0x36,0x49,0x49,0x49,0x36,0x00 ; digit 8
				LCDData9:		.DB			0x06,0x09,0x09,0x09,0x7E,0x00 ; digit 9
				LCDDataA:		.DB			0x7C,0x12,0x11,0x12,0x7C,0x00 ; digit A - 10
				LCDDataB:		.DB			0x7F,0x49,0x49,0x49,0x36,0x00 ; digit B
				LCDDataC:		.DB			0x3E,0x41,0x41,0x41,0x41,0x00 ; digit C
				LCDDataD:		.DB			0x7F,0x41,0x41,0x41,0x3E,0x00 ; digit D
				LCDDataE:		.DB			0x7F,0x49,0x49,0x41,0x41,0x00 ; digit E
				LCDDataF:		.DB			0x7F,0x09,0x09,0x01,0x00,0x00 ; digit F - 15
				LCDDataG:		.DB			0x3E,0x41,0x41,0x49,0x3A,0x00 ; Digit G
				LCDDataH:		.DB			0x7F,0x08,0x08,0x08,0x7F,0x00 ; Digit H
				LCDDataI:		.DB			0x41,0x41,0x7F,0x41,0x41,0x00 ; Digit I
				LCDDataJ:		.DB			0x21,0x41,0x3F,0x01,0x01,0x00 ; Digit J
				LCDDataK:		.DB			0x7F,0x08,0x14,0x22,0x41,0x00 ; Digit K - 20
				LCDDataL:		.DB			0x7F,0x40,0x40,0x40,0x40,0x00 ; Digit L
				LCDDataM:		.DB			0x7F,0x02,0x04,0x02,0x7F,0x00 ; Digit M
				LCDDataN:		.DB			0x7F,0x04,0x08,0x10,0x7F,0x00 ; Digit N
				LCDDataO:		.DB			0x3E,0x41,0x41,0x41,0x3E,0x00 ; Digit O
				LCDDataP:		.DB			0x7F,0x09,0x09,0x09,0x06,0x00 ; Digit P - 25
				LCDDataQ:		.DB			0x3E,0x41,0x41,0x21,0x5E,0x00 ; Digit Q
				LCDDataR:		.DB			0x7E,0x09,0x19,0x29,0x46,0x00 ; Digit R
				LCDDataS:		.DB			0x26,0x49,0x49,0x49,0x32,0x00 ; Digit S
				LCDDataT:		.DB			0x01,0x01,0x7F,0x01,0x01,0x00 ; Digit T
				LCDDataU:		.DB			0x3F,0x40,0x40,0x40,0x3F,0x00 ; Digit U - 30
				LCDDataV:		.DB			0x1F,0x20,0x40,0x20,0x1F,0x00 ; Digit V
				LCDDataW:		.DB			0x3F,0x40,0x20,0x40,0x3F,0x00 ; Digit W
				LCDDataX:		.DB			0x63,0x14,0x08,0x14,0x63,0x00 ; Digit X
				LCDDataY:		.DB			0x03,0x04,0x78,0x04,0x03,0x00 ; Digit Y
				LCDDataZ:		.DB			0x61,0x51,0x49,0x45,0x43,0x00 ; Digit Z - 35
				Space:			.DB			0x00,0x00,0x00,0x00,0x00,0x00 ; Space
				Ball:			.db			0x3C,0x7E,0xFF,0xFF,0x7E,0x3C 
				Box:			.db			0xFF,0xFF,0xFF,0xFF,0xFF,0xFF
				RightArrow:		.db			0x08,0x08,0x2A,0x1C,0x08,0x00 ; Digit ->
				EndLCDData:


// This routine tests the LCD by writting the numbers 0 through F on the top row of the LCD.

LCDTest:
				PUSH		R16						; ddd
				PUSH		R0

				LDI			R16,0					; row. 
				MOV			R1,R16
				LDI			R16,0					; colm
				MOV			R0,R16
				RCALL		LCDSetCurser

				LDI			R16,23					; R0 = 15
				MOV			R0,R16
loop2:												; while R0 >= 0

				RCALL		LCDWriteHexDigit		; Write the hex digit in R0
				DEC			R0						;  R0 = R0 - 1
				BRNE		loop2					; end while
				RCALL		LCDWriteHexDigit		; Write the last hex digit R0 == 0
	
				POP			R0
				POP			R16
				RET

statScreen:		
			PUSH	R16
			PUSH	R0
			PUSH	R1
			
			//print "Time -> "
			PRINT	0,0,29
			PRINT	6,0,18
			PRINT	12,0,22
			PRINT	18,0,14
			PRINT	24,0,36
			PRINT	30,0,39
			PRINT	36,0,36

			//print "Ran Today -> "
			PRINT	0,1,27
			PRINT	6,1,10
			PRINT	12,1,23
			PRINT	18,1,36
			PRINT	24,1,29
			PRINT	30,1,24
			PRINT	36,1,13
			PRINT	42,1,10
			PRINT	48,1,34
			PRINT	54,1,36
			PRINT	60,1,39
			PRINT	66,1,36


			POP		R1
			POP		R0
			POP		R16
			RET
		
// ***********************************************************************************
// *                  LCD Public functions. Call these functions but do not modefy   *
// ***********************************************************************************

// Performs all LCD initialization.
InitTheLCD:		//CALL		SetCpuClockTo32MInt
				CALL		LCDSetupUsartSpiPins
				CALL		LCDSetupSPIOnUARTD0
				CALL		LCDReset
				CALL		LCDInit
				RET

// Possitions the curser in the LCD. 
// input:	R0 = colm			(0 ... 131)
//			R1 = row  (page)	(0 ... 3)
//			
// each page is a block of 8 rows.
// each letter is a 8 X 6 matrix with the image in the top left 7 X 5 corner.
// Leave the bottom row and the rightmost column blank for spacing. 

LCDSetCurser:	MOV			R16,R0					; set the MSB of the colm address
				ANDI		R16,0xF0				; Code is 0001xxxx
				SWAP		R16
				ORI			R16,0x10
				RCALL		LCDWriteCmd

				MOV			R16,R0					; set the LSB of the colm address
				ANDI		R16,0x0F				; code is 0000xxxx
				RCALL		LCDWriteCmd

				MOV			R16,R1					; set the row (page)
				ANDI		R16,0x0F				; code is 1011xxxx
				ORI			R16,0xB0
				RCALL		LCDWriteCmd
				RET

// Writes the byte in R16 to the current colm of the current row of the LCD.
// The next colomn is incremented automatically.
// Algo: make A0 line high then send the byte to the LCD.
// Input: R16 = byte with bit pattern to display.

LCDWriteData:
				PUSH		R17					
				LDI			R17,0b00000001			; LCD_AO high (D0 <- 1)
				STS			PORTD_OUTSET,R17		;			*
				CALL		LCDSendByte				; Send the byte
				POP			R17
				RET

// Draws a figure that has 6 colms at the location of the curser.
//
// Z = address of the first of the 6 bytes to draw.
//

LCDDraw6ColmFig:
				PUSH		R16
				LPM			R16,Z+					; write( LCDData[Z++] )
				RCALL		LCDWriteData			;			*
				LPM			R16,Z+					; write( LCDData[Z++] )
				RCALL		LCDWriteData			;			*
				LPM			R16,Z+					; write( LCDData[Z++] )
				RCALL		LCDWriteData			;			*
				LPM			R16,Z+					; write( LCDData[Z++] )
				RCALL		LCDWriteData			;			*
				LPM			R16,Z+					; write( LCDData[Z++] )
				RCALL		LCDWriteData			;			*
				LPM			R16,Z+					; write( LCDData[Z++] )
				RCALL		LCDWriteData			;			*
				POP			R16
				RET


// clears the whole LCD screen.
// Algo: Traverses each block of the screen writting a 00 bit pattern.
LCDClearScreen:
				PUSH		R16
				PUSH		R17
				PUSH		R18
				PUSH		R0
				PUSH		R1

				CLR			R0						; colm
				LDI			R16,3
				MOV			R1,R16					; row (page)
LCDwhile2:	
				CALL		LCDSetCurser

				CLR			R16
				LDI			R17,132
LCDwhile3:		CALL		LCDWriteData
				DEC			R17
				BRNE		LCDwhile3
		
				DEC			R1
				BRGE		LCDwhile2
				POP			R1
				POP			R0
				POP			R18
				POP			R17
				POP			R16
				RET

LCDBackLightOn:
				PUSH		R16
				LDI			R16,0b00010000			; E4 <- 1 (LCD back light on)
				STS			PORTE_OUTSET,R16		
				POP			R16
				RET

LCDBackLightOff:
				PUSH		R16
				LDI			R16,0b00010000			; E4 <- 0 (LCD back light off)
				STS			PORTE_OUTCLR,R16		
				POP			R16
				RET
LCDReverseOn:
				PUSH		R16
				LDI			R16,0xA7				; cmd = A7 (Reverse on)
				RCALL		LCDWriteCmd
				POP			R16
				RET
LCDReverseOff:
				PUSH		R16				
				LDI			R16,0xA6				; cmd = A6 (Reverse off)
				RCALL		LCDWriteCmd
				POP			R16
				RET

LCDOn:
				PUSH		R16				
				LDI			R16,0xAF				; cmd = AF (LCD on)
				RCALL		LCDWriteCmd
				POP			R16
				RET

LCDOff:
				PUSH		R16				
				LDI			R16,0xAE				; cmd = AE (LCD off)
				RCALL		LCDWriteCmd
				POP			R16
				RET

// ***********************************************************************************
// *    LCD Private functions. Do not call these functions directly. Do not modefy   *
// ***********************************************************************************



// Writes the byte in R16 to the LCD as a command.
// Algo: make A0 line low then send the byte to the LCD.
// Input: R16 = the byte command.

LCDWriteCmd:
													
				PUSH		R17
				LDI			R17,0b00000001			; LCD_AO low (D0 <- 0)
				STS			PORTD_OUTCLR,R17		;			*
				CALL		LCDSendByte				; Send the byte
				POP			R17
				RET

// Send the byte in R16 to the LCD. 

LCDSendByte:
				PUSH		R17						
				LDI			R17,0b00001000			; Make CS low (F3 <- 0)
				STS			PORTF_OUTCLR,R17		;			*
wcmd1:
				LDS			R17, USARTD0_STATUS		; loop until the data buffer is clear
				SBRS		R17,5
				RJMP		wcmd1
				STS			USARTD0_DATA,R16		; Send the byte to the LCD
wcmd2:
				LDS			R17, USARTD0_STATUS		; loop until the transmit is complete
				SBRS		R17,6
				RJMP		wcmd2
				CBR			R17,6
				STS			USARTD0_STATUS,R17		; CLEAR TRANSMIT COMPLETE
				POP			R17
				RET

;   wait a little, short delay used in RESET of the ST7565r
;	for lcd5 added an outer loop to increase the delay by a factor of 4.
;
wlittle:
		PUSH r17
		PUSH r18
		ldi r18,4
agab:
		ldi r17,85
agaa:
		nop
		nop
		nop
		dec r17
		brne agaa
		dec r18
		brne agab
		POP r18
		POP r17
		ret



// Initialize the LCD
LCDInit:
				LDI			R16, 0xA0				; cmd = A0 (adc normal)
				CALL		LCDWriteCmd				;			*
				LDI			R16, 0xA6				; cmd = A6 (display in normal mode)
				CALL		LCDWriteCmd
				LDI			R16, 0xC8				; cmd = C8 (reverse scan)
				CALL		LCDWriteCmd
				LDI			R16, 0xA2				; cmd = A2 (lcd bias)
				CALL		LCDWriteCmd
				CALL		wlittle					; wants a small delay here
				LDI			R16, 0x2F				; cmd = 2F (power control)		
				CALL		LCDWriteCmd
				LDI			R16, 0xF8				; cmd = F8 (set  booster ratio)
				CALL		LCDWriteCmd
				LDI			R16, 0x00				; cmd = 00 (booster ratio 2x ... 4x)
				CALL		LCDWriteCmd
				LDI			R16, 0x21				; cmd = 21 (resister ratio)
				CALL		LCDWriteCmd
													; SHOULD CHECK 30 <-< 40 for contrast, called volume
													; in ST7565
				LDI			R16, 0x1F				; cmd = 1F (set contrast ???)
				CALL		LCDWriteCmd
				LDI			R16,0xAF				; cmd = AF (LCD on)
				CALL		LCDWriteCmd
				RET



; Setup the Pins used for the SPI on USART D0

; A3 = Reset/
; F3 = CS/
; D0 = AO of the LCD
; D1 = XCK
; D3 = TX
; E4 = back light (1 = on, 0 = off)

LCDSetupUsartSpiPins:
				PUSH		R16

				LDI			R16,0b00001000			;set usart-spi ports
				STS			PORTA_DIRSET,R16		;A3 out for Reset
				STS			PORTA_OUTSET,R16		;   high
				STS			PORTF_DIRSET,R16		;F3 out for CS
				STS			PORTF_OUTSET,R16		;   high
				LDI			R16,0b00001011
				STS			PORTD_DIRSET,R16		;D0,1,3 out for  D0=A0,D1=xck,D3=TX
				STS			PORTD_OUTSET,R16		;   high
				LDI			R16,0b00010000			;set usart-spi ports
				STS			PORTE_DIRSET,R16		;E4 out  for backlite
				STS			PORTE_OUTSET,R16		;   on

				POP			R16
				RET

; Reset the LCD.  
; Algo: Make CS/ low then Reset/ low then wait 1 ms then Reset/ high.
LCDReset:
				PUSH		R16
				LDI			R16,0b00001000
				STS			PORTF_OUTCLR,R16		; F3 = 0 (cs_bar low = active)
				STS			PORTA_OUTCLR,R16		; A3 = 0 (reset_bar low = start reset)
				CALL		wlittle					; delay 1 ms
				STS			PORTA_OUTSET,R16		; A3 = 1 (reset_bar high).
				POP			R16
				RET

; Set up master spi on UARTD0
; USART initialization should use the following sequence: 
; 1.    Set the TxD pin value high, and optionally set the XCK pin low.
; 2.    Set the TxD and optionally the XCK pin as output. DONE ABOVE
; 3.    Set the baud rate and frame format. 
; 4.    Set the mode of operation (enables XCK pin output in synchronous mode). 
; 5.    Enable the transmitter or the receiver, depending on the usage. 

LCDSetupSPIOnUARTD0:
				PUSH		R16
				
				LDI			R16, 0b01000000			; Step 1&2. invert xck
				STS			PORTD_PIN1CTRL,R16		; This is part of "SPI MODE 3"
				
				LDI			R16,0b00000010			; xck
				STS			PORTD_OUTCLR,R16	

				LDI			R16, 0b00001111			; Step 3. set BSEL USART xck to 0x0F
				STS			USARTD0_BAUDCTRLA,R16

				LDI			R16, 0b11000011			; Step 4.
				STS			USARTD0_CTRLC,R16		; MASTER,MSB FIRST, hafl of MODE 3, BIT0 ???, 

				LDI			R16, 0b00011000			; Step 5.
				STS			USARTD0_CTRLB,R16		; TX & RX ENABLE

				POP			R16
				RET



// Displays the number in the least significant 4 bits of R0.
// The bit patterns for each digit is stored in the table LCDData in program memory.
// Each digit has 6 columns for the table has 6 bytes per digit. 
// Algo:	The digit value is multiplied by 6 to produce a byte offset from the start 
// of the table. Then the offset is added to the start of the table and that address points 
// to the first of the 6 bytes for that digit. Each of the 6 byte is sent to the LCD to display. 
// Input:	R0 = digit to display (00 ... 0F)

LCDWriteHexDigit:
				PUSH		R16
				PUSH		R17
				PUSH		R1
				PUSH		ZL	
				PUSH		ZH	
				PUSH		R0

				LDI			ZL,low(LCDData << 1)	; Z = LCDData			
				LDI			ZH,high(LCDData << 1)	;			*

				MOV			R16,R0					; Clear the MSB of R0 just in case 
				LDI			R17,6					; Z = LCDData + (digit * 6)
				MUL			R16,R17					;			* 
				MOV			R16,R0					;			*
				ADD			ZL,R16					;			*
				CLR			R16						;			*
				ADC			ZH,R16					;			*
				
				CALL		LCDDraw6ColmFig

				POP			R0
				POP			ZH
				POP			ZL
				POP			R1
				POP			R17
				POP			R16		

				RET

SetCpuClockTo32MInt:
				LDS			R16,OSC_CTRL			; Enable the 32M Hz oscilator
				ORI			R16,0b00000010			;			*
				STS			OSC_CTRL,R16			;			*
while1:
				LDS			R16,OSC_STATUS			; Wait until its stable
				ANDI		R16,0x02				;			*
				BREQ		while1					;			*

				LDI			R16,0xD8				; Connect the 23 MHz OSC to the system clock
				OUT			CPU_CCP,R16				;			*
				LDI			R16,0x01				;			*
				STS			CLK_CTRL,R16			;			*

				LDI			R16,0xD8				; Reset the prescale stages A,B,C back to 1
				OUT			CPU_CCP,R16				;			*
				LDI			R16,0x00				;			*
				STS			CLK_PSCTRL,R16			;			*

				LDI			R16,0xD8				; Select the internal 32.768 KHz source
				OUT			CPU_CCP,R16				; for the RC32M DFLL
				LDS			R16,OSC_DFLLCTRL		;			*
				ANDI		R16,0b11111101			;			*
				STS			OSC_DFLLCTRL,R16		;			*

				LDS			R16,DFLLRC32M_CTRL		; Enable the DFLL for the RC32MHz
				ORI			R16,0x01				;			*
				STS			DFLLRC32M_CTRL,R16		;			*

				RET