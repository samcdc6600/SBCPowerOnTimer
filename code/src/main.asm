	;; SBCPowerOnTimer
	;; Note that we use a callee saved convention for registers and that a
	;; function should only save a register if it uses it.
	;; We note that AT-PAn -> LCD-Dn for n = 0 up to 7 and where -> means is
	;; connected to. We not also that AT-PC7 -> LCD-RS, AT-PC6 -> LCD-RW
	;; and AT-PC5 -> LCD-E.
	
	.nolist			; Don't include in the .lst file if we ask the
				; assembler to generate one.
	.include "./include/m16Adef.inc" 



	;; =====================================================================
	;; ============================ Definitions ============================

	;; =====================================================================
	;; ============================= Constants =============================
	.equ	DDRportC = 0b11111111
	.equ	DDRportA = 0b11111111
	;; ======================= LCD Related Constants =======================
	;; == Display control siginals E (enable), RW (read write), RS (register select) ==
	.equ	Enable		  = 0b00100000
	.equ	ReadWrite	  = 0b01000000
	.equ	RegisterSelect	  = 0b10000000
	;; == Display commands (the position of the first 1 indicates the command.) ==
	;; DL (data length? = 8 bits), N (number of lines = 2), F (character dimensions = 5 * 8),
	.equ	functionSet_data  = 0b00111000 ; that is bits 4, 3 and 2.
	;;  D (display on/off = on), C (cursor on/off = on), B (cursor blinking = on).
	.equ	displayOn_data    = 0b00001111 ; That is bits 2, 1 and 0

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


	;; =====================================================================
	;; ========================= Main Code Section =========================
MAIN:
START_OF_MAIN:

	rjmp START_OF_MAIN
	
INIT:
	call	DISABLE_JTAG
	;; call	BUSY_WAIT
	;; call	BUSY_WAIT
	call	SET_DDRS
	call	INIT_LCD
	;; clr	secondCount	; Clear the regs we store time in
	;; clr	minuteCount
	;; clr	hourCount

	;; ldi	r16, low(ddrSecondPins)
	;; out	DDRD, r16	; Set the Data Direction Register for Port D (seconds)
	;; out	PortD, secondCount ; Set second pins low

	;; ldi	r16, low(ddrMinutePins)
	;; out	DDRC, r16	; Set the Data Direction Register for port C (minutes)
	;; out	PortC, minuteCount ; Set minute pins low

	; Port A is shared between hours and the time adjustment indicator LEDs
	;; ldi	r16, low(ddrHourAndAdjustLEDIndicatorPins)
	;; out	DDRA, r16	; Set the Data Direction Register for port A (hours)
	;; out	PortA, hourCount	; Set hour pins low

	;; ldi	r16, low(ddrAdjustPins)
	;; Set the Data Direction Register for Port B (time adjustment controls
	;; and time signal)
	;; out	DDRB, r16	; Also note that PB2 is used for our time signal
	;; ldi	r16, low(adjustPinsPullups)
	; Set pull up's high (except for PB2 (time signal) which is active high)
	;; out	PortB, r16
	
	;; ldi	r16, low(int2En)
	;; out	GICR, r16	; Set external interrupt 2 to be enabled
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
SET_DDRS:
	push	r16
	ldi	r16, low(DDRportA)
	out	DDRA, r16
	ldi	r16, low(DDRportC)
	out	DDRC, r16
	pop	r16

	
INIT_LCD:
	call SET_LCD_FUNCTIONS
	call TURN_ON_DISPLAY
	ret


SET_LCD_FUNCTIONS:
	push	r16
	;; Output display function set command.
	;; Set display parameters (DL (bus width), N (number of lines),
	;; F (font size)). We set the state of the data pins (port A) high first.
	ldi	r16, low(functionSet_data)
	out	PortA, r16
	;; Clear control signals (high 3 bit's of port C.)
	ldi	r16, 0b0
	out	PortC, r16
	;; toggle enable bit to send instruction.
	ldi	r16, low(Enable)
	out	PortC, r16	; Send instruction to display.
	;; call	BUSY_WAIT
	ldi	r16, 0b0
	out	PortC, r16	; Stop sending.
	pop	r16
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
	;; call	BUSY_WAIT
	ldi	r16, 0b0
	out	PortC, r16	; Stop sending.
	pop	r16
	ret


BUSY_WAIT:			;===============================================
	clr	r31
	clr	r30
	clr	r29
BUSY_WAIT_LOOP_0_START:
	inc	r31
	
BUSY_WAIT_LOOP_1_START:
	inc	r30
	
BUSY_WAIT_LOOP_2_START:
	inc	r29
	
	cpi	r29, 0b11110000
	brne	BUSY_WAIT_LOOP_2_START

	cpi	r30, 0b11110000
	brne	BUSY_WAIT_LOOP_1_START
	
	cpi	r31, 0b11110000
	brne	BUSY_WAIT_LOOP_0_START
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
