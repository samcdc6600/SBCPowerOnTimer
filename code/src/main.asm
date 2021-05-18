	;; SBCPowerOnTimer
	;; Note that we use a callee saved convention for registers and that a
	;; function should only save a register if it uses it. Of course if a
	;; function takes an argument then the register will need to be saved by
	;; the caller if it is used by the caller for something else. Arguments
	;; should start from r16 unless they can't (for example when using Z.)
	;; We note that AT-PAn -> LCD-Dn for n = 0 up to 7 and where -> means is
	;; connected to. We note also that AT-PC7 -> LCD-RS, AT-PC6 -> LCD-RW
	;; and AT-PC5 -> LCD-E.
	
	.nolist			; Don't include in the .lst file if we ask the
				; assembler to generate one.
	.include "./include/m16Adef.inc" 


	;; =====================================================================
	;; ============================ Definitions ============================
	;; The assembler won't automatically place a null byte after strings.
	;; It may place one as padding however.

	;; =====================================================================
	;; ============================= Constants =============================
	.set	setDDRToAllOutputs	= 0b11111111
	.set	setDDRToAllInputs	= 0b00000000
	;; ======================= LCD Related Constants =======================
	.set	lcdLine1	= 0b00000000
	.set	lcdLine2Half	= 0x7f ; 127, lcdLine2 should equal this after right shift.
	.set	lcdLine2	= 0b11111111 ; Must be ff as we invert to get lcdLine1.
	;; The Display data RAM (DDRAM) is 80 x 8 bits.
	;; 0x28 = 40 (0x29 because the ATmega16 doesn't have brgt (branch if
	;; greater than) so we must use brsh (branch if same or higher.)
	.set	halfDDRAMSize	= 0x29
	;; .set	DDRAMSize	= 0x50
	.set	displayWidth	= 0b00010000 ; The screen is 16 characters long.
	;; == Display control siginals E (enable), RW (read write), RS (register select.) ==
	.set	enable		  = 0b00100000
	.set	enableRead	  = 0b01000000
	.set	enableWrite	  = 0b00000000
	.set	registerSelectOn	= 0b10000000
	.set	registerSelectOff	= 0b00000000
	;; == Display commands (the position of the first 1 indicates the command.) ==
	;; DL (data length? = 8 bits), N (number of lines = 2), F (character dimensions = 5 * 8),
	.set	functionSet_data  = 0b00111000 ; that is bits 4, 3 and 2.
	;; D (display on/off = on), C (cursor on/off = on), B (cursor blinking = on).
	.set	displayOn_data    = 0b00001111 ; That is bits 2, 1 and 0
	;; Sets DDRAM address so that the cursor is positioned at the head of the second line.
	.set	setDDRAMAddressTo2ndLine	  = 0b11000000
	;; S/C (display shift (1) / cursor move), R/L (shift to the right (1) / shift to the left)
	.set	shiftDisplayLeft	= 0b00011000
	.set	shiftDisplayRight	= 0b00011100


	;; ======================== Start data segment! ========================
	;; =====================================================================
	.dseg
	;; Sould only be changed by SWITCH_LCD_LINE (after INIT_LCD.)
	currentLCDLine:		.byte 1 ; 0 for first line ff for second line.
	;; Stores the length of the longest line on the display.
	;; Reserve 1 byte (current max line len can't be more than 40 bytes.)
	currentMaxLineLen:	.byte 1
	;; Stores the current number of characters scrolled in from the right.
	currentScrollLen:	.byte 1

	
	;; ======================== Start code segment! ========================
	;; =====================================================================
	.cseg			
	;; =====================================================================
	;; ========================== Interrupt Vector =========================
	jmp   RESET	; Reset Handler
	jmp   EXT_INT0	; IRQ0 Handler
	jmp   EXT_INT1	; IRQ1 Handler
	jmp   TIM2_COMP	; Timer2 Compare Handler
	jmp   TIM2_OVF	; Timer2 Overflow Handler
	jmp   TIM1_CAPT	; Timer1 Capture Handler
	jmp   TIM1_COMPA	; Timer1 CompareA Handler
	jmp   TIM1_COMPB	; Timer1 CompareB Handler
	jmp   TIM1_OVF	; Timer1 Overflow Handler
	jmp   TIM0_OVF	; Timer0 Overflow Handler
	jmp   SPI_STC	; SPI Transfer Complete Handler
	jmp   USART_RXC	; USART RX Complete Handler
	jmp   USART_UDRE	; UDR Empty Handler
	jmp   USART_TXC	; USART TX Complete Handler
	jmp   ADC	; ADC Conversion Complete Handler
	jmp   EE_RDY	; EEPROM Ready Handler
	jmp   ANA_COMP	; Analog Comparator Handler
	jmp   TWSI	; Two-wire Serial Interface Handler
	jmp   EXT_INT2	; IRQ2 Handler
	jmp   TIM0_COMP	; Timer0 Compare Handler
	jmp   SPM_RDY	; Store Program Memory Ready Handler;


	;; helloStr:	.db "- When little worlds collide! -", 0, 0 ; Extra 0 so bytes are even.
	;; helloStr2nd:	.db "IFeelThoseFeelings", 0, 0
	helloStr:	.db "I Love You So I Do", 0, 0 ; Extra 0 so bytes are even.
	helloStr2nd:	.db "~~~~~", 0, 0

	
	;; =====================================================================
	;; ========================= Main Code Section =========================
MAIN:
	ldi	r30, low(2*helloStr) ; Load address of string.
	ldi	r31, high(2*helloStr)	
	call	WRITE_TO_LCD
	
	call	SWITCH_LCD_LINE
	ldi	r30, low(2*helloStr2nd) ; Load address of string.
	ldi	r31, high(2*helloStr2nd)
	call	WRITE_TO_LCD

	
START_OF_MAIN:
	;; call	CLEAR_LCD
	ldi	r16, 0b00111111
	ldi	r17, 0b00011101	; Maybe have a menu option to aulter this vlaue?
	call	BUSY_WAIT
	call	SCROLL_LCD
	rjmp	START_OF_MAIN


	;; https://stackoverflow.com/questions/48645379/avr-xyz-registers
	;; X Y and Z registers are actually pairs of r27:r26, r29:r28 and
	;; r31:r30 registers. Each of them can be used as indirect pointers to SRAM:
	;; ld r16, X
	;;
	;; with post-increment, or pre-decrement:
	;; ld r16, -Y
	;; st Z+, r16
	;;
	;; But only Y and Z can be used with displacment
	;; ldd r16, Y + 10
	;; std Z + 5, r16
	;;
	;; and only Z can be used to indirect read the flash memory, and no pre-decrement or displacement are available :
	;; lpm r16, Z+
	;; lpm r17, Z
	;; WRITE_TO_LCD takes two arguments that are passed via r30 and r31.
	;; They are respectively the low and high bytes of the string address
WRITE_TO_LCD:
	push	r16
	push	r17
	push	r18		; Used to count str len, where str is pointed to by Z.
	
	clr	r18
START_WRITE_TO_LCD:
	lpm	r16, Z+
	inc	r18
	ldi	r17, 0b0
	cp	r16, r17		; Have we hit the null byte?
	breq    END_WRITE_TO_LCD
	cpi	r18, halfDDRAMSize	; Have we reached the end of the display ram for this line.
	brge	END_WRITE_TO_LCD
	;; Output character command.
	out	PortA, r16
	ldi	r16, low(registerSelectOn)	; Set RS control signal
	out	PortC, r16
	;; Set E and RS control siginals.
	ldi	r16, (low(registerSelectOn) | low(enable))
	out	PortC, r16
	ldi	r17, 0b00000001	; Set up args for BUSY_WAIT
	ldi	r16, 0b00000001
	call	BUSY_WAIT
	ldi	r16, low(registerSelectOn)	; Clear E control signal
	out	PortC, r16
	jmp	START_WRITE_TO_LCD
END_WRITE_TO_LCD:
	dec	r18		; We inc'ed for '\0'
	ldi	r30, low(2*currentMaxLineLen)
	ldi	r31, high(2*currentMaxLineLen)
	ld	r16, Z
	cp	r18, r16
	brlo	SKIP_CURRENT_MAX_LINE_LEN_UPDATE	; Branch if lower
	st	Z, r18		; Store str len at line2CurrentStrLen.
SKIP_CURRENT_MAX_LINE_LEN_UPDATE:
	
AFTER_SET_LCD_LINE_LEN:
	pop	r18
	pop	r17
	pop	r16
	ret


SWITCH_LCD_LINE:
	push	r16
	push	r30
	push	r31
	
	ldi	r16, low(setDDRAMAddressTo2ndLine) ; Switch to second line :)
	call	SEND_LCD_INSTRUCTION
	
	ldi	r30, low(2*currentLCDLine)
	ldi	r31, high(2*currentLCDLine)
	ld	r18, Z
	com	r18		; One's complement (invert all bits.)
	st	Z, r18		; Store current LCD line (r18.)
	
	pop	r31
	pop	r30
	pop	r16
	ret


SCROLL_LCD:
	push	r16
	push	r30
	push	r31
	;; Load current max line len (the length of the longest currently displayed line.)
	ldi	r30, low(2*currentMaxLineLen)
	ldi	r31, high(2*currentMaxLineLen)
	ld	r16, Z
	dec	r16		; No branch if less than or equal.
	cpi	r16, displayWidth	; Will scroll if both lines are of length 0 (This shouldn't be the case!)
	brlo	NO_SCROLL_NEEDED
	call	SCROLL_LCD_PROPER

NO_SCROLL_NEEDED:
	
	pop	r31
	pop	r30
	pop	r16
	ret


	;; Takes Z (2*currentMaxLineLen) as an argument.
SCROLL_LCD_PROPER:
	push	r16
	push	r17
	
	ld	r16, Z		; Load *currentMaxLineLen
	ldi	r30, low(2*currentScrollLen)
	ldi	r31, high(2*currentScrollLen)
	ld	r17, Z		; Load *currentScrollLen into r17
	subi	r16, displayWidth ; We've already made sure r16 < displayWidth in SCROLL_LCD.
	cp	r17, r16
	breq	FAST_SCROLL_BACK ; We've scrolled to the end of currentMaxLineLen.

	ldi	r16, shiftDisplayLeft
	call	SEND_LCD_INSTRUCTION

	inc	r17
	jmp	SCROLL_LCD_PROPER_RET

FAST_SCROLL_BACK:
	ldi	r16, shiftDisplayRight
	call	SEND_LCD_INSTRUCTION
	dec	r17
	cpi	r17, 0b0
	brne	FAST_SCROLL_BACK

SCROLL_LCD_PROPER_RET:
	st	Z, r17
	pop	r17
	pop	r16
	ret

;; 	;; Load current max line len (the length of the longest currently displayed line.)
;; 	ldi	r30, low(2*currentMaxLineLen)
;; 	ldi	r31, high(2*currentMaxLineLen)
;; 	ld	r16, Z
;; 	subi	r16, screenLen	; Account for screen size.
;; 	;; Load current scroll len (how many characters we have scrolled so far.)
;; 	ldi	r30, low(2*currentScrollLen)
;; 	ldi	r31, high(2*currentScrollLen)
;; 	ld	r17, Z
;; 	;; ldi	r16, 0b00011111
;; 	cp	r17, r16	; Cmp (*currentMaxLineLenTo - *currentScrollLen)
;; 	breq	FAST_FORWARD

;; 	;; push	r16		; r16 holds *currentMaxLineLen
;; 	ldi	r16, shiftDisplayLeft
;; 	call	SEND_LCD_INSTRUCTION
;; 	;; pop	r16
;; 	jmp	STAY_SLOW

;; FAST_FORWARD:
;; 	ldi	r17, DDRAMSize
;; 	sub	r17, r16

;; SCROLL_LCD_FOR:			; r17 > 0
;; 	ldi	r16, shiftDisplayLeft
;; 	call	SEND_LCD_INSTRUCTION
;; 	dec	r17
;; 	cpi	r17, 0b0
;; 	brne	SCROLL_LCD_FOR
;; 	ldi	r17, 0xff	; Will wrape around when incremented.
;; STAY_SLOW:

;; 	inc	r17
;; 	st	Z, r17

;; 	pop	r31
;; 	pop	r30
;; 	pop	r17
;; 	pop	r16
;; 	ret


;; SCROLL_LCD:
	
;; 	push	r16
;; 	push	r17
;; 	push	r30
;; 	push	r31

;; 	;; Load current max line len (the length of the longest currently displayed line.)
;; 	ldi	r30, low(2*currentMaxLineLen)
;; 	ldi	r31, high(2*currentMaxLineLen)
;; 	ld	r16, Z
;; 	subi	r16, screenLen	; Account for screen size.
;; 	;; Load current scroll len (how many characters we have scrolled so far.)
;; 	ldi	r30, low(2*currentScrollLen)
;; 	ldi	r31, high(2*currentScrollLen)
;; 	ld	r17, Z
;; 	;; ldi	r16, 0b00011111
;; 	cp	r17, r16	; Cmp (*currentMaxLineLenTo - *currentScrollLen)
;; 	breq	FAST_FORWARD

;; 	;; push	r16		; r16 holds *currentMaxLineLen
;; 	ldi	r16, shiftDisplayLeft
;; 	call	SEND_LCD_INSTRUCTION
;; 	;; pop	r16
;; 	jmp	STAY_SLOW

;; FAST_FORWARD:
;; 	ldi	r17, DDRAMSize
;; 	sub	r17, r16

;; SCROLL_LCD_FOR:			; r17 > 0
;; 	ldi	r16, shiftDisplayLeft
;; 	call	SEND_LCD_INSTRUCTION
;; 	dec	r17
;; 	cpi	r17, 0b0
;; 	brne	SCROLL_LCD_FOR
;; 	ldi	r17, 0xff	; Will wrape around when incremented.
;; STAY_SLOW:

;; 	inc	r17
;; 	st	Z, r17

;; 	pop	r31
;; 	pop	r30
;; 	pop	r17
;; 	pop	r16
;; 	ret
;; LOOP:
;; 	jmp LOOP


;; READ_LCD_ADDRESS_COUNTER:	; Returns the LCD address counter in r17.
;; 	push	r16

;; 	ldi	r17, setDDRToAllInputs
;; 	call	SET_DDRA	; Set port A to all inputs.
;; 	ldi	r17, (low(registerSelectOff) | low(enableRead) | low(enableRead))
;; 	out	PortC, r17

;; 	ldi	r16, 0b00000011
;; 	ldi	r17, 0b00000011
;; 	call	BUSY_WAIT
;; 	in	r17, PortA
;; 	;; Reset LCD settings back to what they were after init (this will reset
;; 	;; the cursor position) and set the direction of port A back to output.
;; 	ldi	r16, low(functionSet_data)
;; 	call	SEND_LCD_INSTRUCTION
;; 	ldi	r16, low(setDDRToAllOutputs)
;; 	call	SET_DDRA	; SET_DDRA doesn't change r16

;; 	pop	r16
;; 	ret
	
	
INIT:
	call	DISABLE_JTAG
	ldi	r16, low(setDDRToAllOutputs)
	call	SET_DDRA	; SET_DDRA doesn't change r16
	call	SET_DDRC
	call	INIT_LCD
	ret


	;; DISABLE_JTAG dissables the JTAG interface (PC7 - PC2) in software.
	;; This allows the use of the PC7 - PC2 pins as general IO while still
	;; allowing the chip to be programmed as we have not set any fuses.
	;; The manual says to set the JTD bit (bit 7 according to the manual (pg
	;; 236) but that does not seem to work.) Post #3 on this site:
	;; https://www.avrfreaks.net/forum/how-disable-jtag-c
	;; said to use 0x80 and that does work. From the manual "In order to
	;; avoid unintentional disabling or enabling of the JTAG interface, a
	;; timed sequence must be followed when changing this bit: The
	;; application software must write this bit to the desired value twice
	;; within four cycles to change its value."
DISABLE_JTAG:			;===============================================
	push	r16
	ldi	r16, 0x80
	out	MCUCSR, r16
	out	MCUCSR, r16
	pop	r16
	ret


	;; Set the data direction registers. These may be changed latter
	;; depending on what is attached to the pins.
SET_DDRA:
	out	DDRA, r16
	ret


SET_DDRC:
	out	DDRC, r16
	ret	

	
INIT_LCD:
	;; Output display function set command.
	;; Set display parameters (DL (bus width), N (number of lines),
	;; F (font size)). We set the state of the data pins (port A) high first.
	ldi  r16, low(functionSet_data) ; SEND_LCD_INSTRUCTION takes r16 as an argument.
	call SEND_LCD_INSTRUCTION
	call TURN_ON_DISPLAY
	;; It will simplify the code to know the current LCD line (there may be
	;; a way to find this our from the LCD controller, however we don't know
	;; of any.)
	ldi	r30, low(2*currentLCDLine) ; Load currentLCDLine into Z.
	ldi	r31, high(2*currentLCDLine)
	ldi	r16, lcdLine1
	st	Z, r16		; Set *currentLCDLine to the first line (lcdLine1.)
	
	ldi	r30, low(2*currentMaxLineLen) ; Load currentMaxLineLen into Z.
	ldi	r31, high(2*currentMaxLineLen)
	ldi	r16, 0b0
	st	Z, r16		; Set *currentMaxLineLen to 0.
	
	ldi	r30, low(2*currentScrollLen) ; Load currentScrollLen into Z.
	ldi	r31, high(2*currentScrollLen)
	ldi	r16, 0b0
	st	Z, r16		; Set *currentScrollLen to 0 (we havent scrolled the display yet.)
	ret


SEND_LCD_INSTRUCTION:
	out	PortA, r16
	;; Clear control signals (high 3 bit's of port C.)
	ldi	r16, 0b0
	out	PortC, r16
	;; toggle enable bit to send instruction.
	ldi	r16, low(Enable)
	out	PortC, r16	; Send instruction to display.
	;; ldi	r16, 0b00000011	; Add a small delay to make sure instruction is sent.
	;; ldi	r17, 0b00000001
	call	BUSY_WAIT
	ldi	r16, 0b0
	out	PortC, r16	; Stop sending.
	ret


	;; Not that this function also turns on the cursor.
TURN_ON_DISPLAY:
	push	r16
	;; Output display on command.
	ldi	r16, low(displayOn_data)
	out	PortA, r16
	;; Clear control siginals.
	ldi	r16, 0b0
	out	PortC, r16
	;; toggle enable bit to send instruction.
	ldi	r16, low(Enable)
	out	PortC, r16	; Send instruction to display.
	ldi	r16, 0b0
	out	PortC, r16	; Stop sending.
	pop	r16
	ret


	;; BUSY_WAIT takes two arguments that are passed via r16 and r17.
	;; It loops ~ r16 ^ 2 * r17 times.
BUSY_WAIT:			;===============================================
	push	r31
	push	r30
	push	r29
	clr	r31
BUSY_WAIT_LOOP_0_START:
	inc	r31

	clr	r30
BUSY_WAIT_LOOP_1_START:
	inc	r30

	clr	r29
BUSY_WAIT_LOOP_2_START:
	inc	r29

	cp	r29, r16
	brne	BUSY_WAIT_LOOP_2_START

	cp	r30, r16
	brne	BUSY_WAIT_LOOP_1_START
	
	cp	r31, r17
	brne	BUSY_WAIT_LOOP_0_START
	pop	r29	
	pop	r30
	pop	r31
	ret


	;; =====================================================================
	;; =============================== ISRs ================================
RESET:
	ldi	r16, high(RAMEND)	; Program starts here.
	out	SPH, r16		; Set Stack Pointer to top of RAM.
	ldi	r16, low(RAMEND)
	out	SPL, r16
	; Stack set up now perform the remaining initialisation tasks
	call	INIT
	sei			; Enable interrupts.
	call	MAIN
EXT_INT0:	; IRQ0 Handler
EXT_INT1:	; IRQ1 Handler
TIM2_COMP:	; Timer2 Compare Handler
TIM2_OVF:	; Timer2 Overflow Handler
TIM1_CAPT:	; Timer1 Capture Handler
TIM1_COMPA:	; Timer1 CompareA Handler
TIM1_COMPB:	; Timer1 CompareB Handler
TIM1_OVF:	; Timer1 Overflow Handler
TIM0_OVF:	; Timer0 Overflow Handler
SPI_STC:	; SPI Transfer Complete Handler
USART_RXC:	; USART RX Complete Handler
USART_UDRE:	; UDR Empty Handler
USART_TXC:	; USART TX Complete Handler
ADC:		; ADC Conversion Complete Handler
EE_RDY:		; EEPROM Ready Handler
ANA_COMP:	; Analog Comparator Handler
TWSI:		; Two-wire Serial Interface Handler
EXT_INT2:	; IRQ2 Handler Note that another second has passed
	;; inc	secondCount	; Time flies :O :'(
	reti
TIM0_COMP:	; Timer0 Compare Handler
SPM_RDY:	; Store Program Memory Ready Handler;
