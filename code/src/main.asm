	;; SBCPowerOnTimer
	;; Note that we use a callee saved convention for registers and that a
	;; function should only save a register if it uses it. Of course if a
	;; function takes an argument then the register will need to be saved by
	;; the caller if it is used by the caller for something else. Arguments
	;; should start from r16.
	;; We note that AT-PAn -> LCD-Dn for n = 0 up to 7 and where -> means is
	;; connected to. We not also that AT-PC7 -> LCD-RS, AT-PC6 -> LCD-RW
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
	.equ	setDDRToAllOutputs = 0b11111111
	;; ======================= LCD Related Constants =======================
	;; == Display control siginals E (enable), RW (read write), RS (register select.) ==
	.equ	enable		  = 0b00100000
	.equ	readWrite	  = 0b01000000
	.equ	registerSelect	  = 0b10000000
	;; == Display commands (the position of the first 1 indicates the command.) ==
	;; DL (data length? = 8 bits), N (number of lines = 2), F (character dimensions = 5 * 8),
	.equ	functionSet_data  = 0b00111000 ; that is bits 4, 3 and 2.
	;; D (display on/off = on), C (cursor on/off = on), B (cursor blinking = on).
	.equ	displayOn_data    = 0b00001111 ; That is bits 2, 1 and 0
	;; Sets DDRAM address so that the cursor is positioned at the head of the second line.
	.equ	setDDRAMAddress	  = 0b11000000

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


	helloStr:	.db "- Hello Girls! -", 0, 0
	helloStr2nd:	.db "IFeelThoseThings", 0, 0

	
	;; =====================================================================
	;; ========================= Main Code Section =========================
MAIN:
	ldi	r30, low(2*helloStr)
	ldi	r31, high(2*helloStr)
	
	call	WRITE_TO_LCD




	push	r16
	;; Output display function set command.
	;; Set display parameters (DL (bus width), N (number of lines),
	;; F (font size)). We set the state of the data pins (port A) high first.
	ldi	r16, low(setDDRAMAddress)
	out	PortA, r16
	;; Clear control signals (high 3 bit's of port C.)
	ldi	r16, 0b0
	out	PortC, r16
	;; toggle enable bit to send instruction.
	ldi	r16, low(Enable)
	out	PortC, r16	; Send instruction to display.
	ldi	r16, 0b0
	out	PortC, r16	; Stop sending.
	pop	r16

	


	ldi	r30, low(2*helloStr2nd)
	ldi	r31, high(2*helloStr2nd)
	call	WRITE_TO_LCD
	
START_OF_MAIN:
	;; call	CLEAR_LCD
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
START_WRITE_TO_LCD:

	lpm	r16, Z+
	
	ldi	r17, 0b0
	cp	r16, r17		; Have we hit the null byte?
	breq    END_WRITE_TO_LCD
	;; Output character command.
	out	PortA, r16
	ldi	r16, low(registerSelect)	; Set RS control signal
	out	PortC, r16
	;; Set E and RS control siginals.
	ldi	r16, (low(registerSelect) | low(enable))
	out	PortC, r16
	push	r17		; BUSY_WAIT takes r17 and r16 as arguments.
	push	r16
	ldi	r17, 0b00000001	; Set up args for BUSY_WAIT
	ldi	r16, 0b00000001
	call	BUSY_WAIT
	pop	r16
	pop	r17
	ldi	r16, low(registerSelect)	; Clear E control signal
	out	PortC, r16
	jmp	START_WRITE_TO_LCD
END_WRITE_TO_LCD:
	pop	r17
	pop	r16
	ret
	
	
	
INIT:
	call	DISABLE_JTAG
	call	SET_DDRS
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
SET_DDRS:
	push	r16
	ldi	r16, low(setDDRToAllOutputs)
	out	DDRA, r16
	out	DDRC, r16
	pop	r16

	
INIT_LCD:
	call SET_LCD_FUNCTION
	call TURN_ON_DISPLAY
	ret


SET_LCD_FUNCTION:
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
