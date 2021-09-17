	;; SBCPowerOnTimer
	;; Note that we use a callee saved convention for registers and that a
	;; subroutine should only save a register if it uses it. Of course if a
	;; subroutine takes an argument then the register will need to be saved
	;; by the caller if it is used by the caller for something else. 
	;; Arguments should start from r16 unless they can't (for example when
	;; using Z.) We note that AT-PAn -> LCD-Dn for n = 0 up to 7 and
	;; where -> means is connected to. We note also that AT-PC7 -> LCD-RS,
	;; AT-PC6 -> LCD-RW and AT-PC5 -> LCD-E.
	
	.nolist			; Don't include in the .lst file if we ask the
				; assembler to generate one.
	.include "./include/m16Adef.inc" 


	;; =====================================================================
	;; ============================ Definitions ============================
	;; The assembler won't automatically place a null byte after strings.
	;; It may place one as padding however.

	;; =====================================================================
	;; ============================= Constants =============================
	.set	TRUE	= 0b00000001
	.set	FALSE	= 0b00000000
	;; ==================== General PORT related things ====================
	.set	allHigh			= 0b11111111
	.set	allLow			= 0b00000000
;	.set	portBPullupDownValues	= 0b00000000
	;; =============================== Inputs ==============================
	.set	enterButton		= 0b00000001 ; -> PB0
	.set	upButton		= 0b00000010 ; -> PB1
	.set	downButton		= 0b00001000 ; -> PB3
	.set	leftButton		= 0b00010000 ; -> PB4
	.set	rightButton		= 0b00100000 ; -> PB5
	;	.set	int2Input		= 0b00000100 ; Int 2 is PB2
;	.set	notInt2Input		= 0b11111011 ; For when we don't want int2.
	.set	int2Pin			= 0b00000100 ; For when we don't want int2 (follow with neg.)
	;; ============================== Outputs ==============================
	.set	enRelay			= 0b10000000 ; Relay is connected to PD7.
	;; =================== For Interrupt on Clock Signal ===================
	;; To enable int2 (pin PB2)
	.equ	int2En = 0b00100000
	;; ======================= LCD Related Constants =======================
	.set	lcdLine1	= 0b00000000
	.set	lcdLine2Half	= 0x7f ; 127, lcdLine2 should equal this after right shift.
	.set	lcdLine2	= 0b11111111 ; Must be ff as we invert to get lcdLine1.
	.set	mainMenuSelectionStrLen = 0b00000001 ; Length of mainMenuSelectionStr.
	;; The Display data RAM (DDRAM) is 80 x 8 bits.
	;; 0x28 = 40 (0x29 because the ATmega16 doesn't have brgt (branch if
	;; greater than) so we must use brsh (branch if same or higher.)
	.set	halfDDRAMSize	= 0x29
	;; .set	DDRAMSize	= 0x50
	.set	displayWidth	= 0b00010000 ; The screen is 16 characters long.
	;; ==| Display control siginals E (enable), RW (read write), RS (register select.) |==
	.set	enable		  = 0b00100000
	.set	enableRead	  = 0b01000000
	.set	enableWrite	  = 0b00000000
	.set	registerSelectOn	= 0b10000000
	.set	registerSelectOff	= 0b00000000
	;; ==| Display commands (the position of the first 1 indicates the command.) |==
	;; DL (data length? = 8 bits), N (number of lines = 2), F (character dimensions = 5 * 8),
	.set	functionSetData  = 0b00111000 ; that is bits 4, 3 and 2.
	;; D (display on/off = on), C (cursor on/off = on), B (cursor blinking = on).
	.set	displayOnData    = 0b00001100 ; That is bits 2, 1 and 0
	;; Sets DDRAM address so that the cursor is positioned at the head of the second line.
	.set	setDDRAMAddressTo2ndLine	  = 0b11000000
	;; S/C (display shift (1) / cursor move), R/L (shift to the right (1) / shift to the left)
	.set	shiftDisplayLeft	= 0b00011000
	.set	shiftDisplayRight	= 0b00011100
	;; Clear display
	.set	clearDisplay	= 0b00000001
	;; =============================== Timing ==============================
	.equ	arcMinute = 0b00111100 ; Number of seconds in minute.
	.equ	buttonPressDelaySquaredComp = 0b00111111
	.equ	buttonPressDelayLinearComp  = 0b00011011
	 ;; .equ	buttonPressDelayLinearComp = 0b00111101
	;; ================================ Menu ===============================
	.set	maxMainMenuItems	= 0b00000101 ; There are 5 items in our main menu.


	;; ======================== Start Data Segment! ========================
	;; =====================================================================
	.dseg
	;; ==| LCD related variables |==
	;; Sould only be changed by SWITCH_TO_SECOND_LCD_LINE (after INIT_LCD.)
	;; currentLCDLine:		.byte 1 ; 0 for first line ff for second line.
	;; Stores the length of the longest line on the display.
	;; Reserve 1 byte (current max line len can't be more than 40 bytes.)
	currentMaxLineLen:	.byte 1
	;; Stores the current number of characters scrolled in from the right.
	currentScrollLen:	.byte 1
	;; Stores the current second (in the current minute.)
	secondsInCurrentMinute:	.byte 1
	;; ==| Backlight related variables |==
	;; Should be set to TRUE if the back light is off and FALSE otherwise.
	backLightOff:		.byte 1
	;; Stores the number of minutes since a button was pressed (used to
	;; automatically turn off the screen back light.)
	minutesIdle:		.byte 1
	
	
	;; ======================== Start Code Segment! ========================
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


	;; ============================== Strings ==============================
	;; ========================= Main Menu Strings =========================
	helloStr:	.db "- When little worlds collide! -", 0, 0 ; Extra 0 so bytes are even.
	helloStr2nd:	.db "- This line is longer then it should be! -", 0, 0

	mainMenuSelectionStr:	.db "~", 0 ; "~" becomes a right facing arrow!
	mainMenuSetTimeStr:	.db "Set Time", 0 ; Item 1.
	mainMenuSetDateStr:	.db "Set Date", 0 ; Item 2.
	mainMenuSetActivationTimeStr:	.db "Set Activation Time", 0 ; Item 3.
	mainMenuDeleteActivationTimeStr:	.db "Delete Activation Time", 0 ; Item 4.
	mainMenuSetBrightnessStr:	.db "Set Brightness", 0 ; Item 5.

	;; ======================== Main Menu Jump Table =======================
;	mainMenuItemsJumpTable:	.dw UPDATE_MENU____DISPLAY_ITEM_1, UPDATE_MENU____DISPLAY_ITEM_2, UPDATE_MENU____DISPLAY_ITEM_3, UPDATE_MENU____DISPLAY_ITEM_4, UPDATE_MENU____DISPLAY_ITEM_5
	;; helloStr:	.db "I Love You So I Do", 0, 0 ; Extra 0 so bytes are even.
	;; helloStr2nd:	.db "~~~~~", 0, 0

	
	;; =====================================================================
	;; ========================= Main Code Section =========================
MAIN:
	;; ldi	r30, low(2*helloStr) ; Load address of string.
	;; ldi	r31, high(2*helloStr)
	;; call	WRITE_TO_LCD
	
	;; call	SWITCH_TO_SECOND_LCD_LINE
	;; ldi	r30, low(2*helloStr2nd) ; Load address of string.
	;; ldi	r31, high(2*helloStr2nd)
	;; call	WRITE_TO_LCD

;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;; CALL RUTINE TO SET TIME HERE. DON'T RETURN UNTIL TIME SET!!!!!!!!!!!!!!!!!!!
;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	ldi	r30, low(2*helloStr) ; Load address of string.
	ldi	r31, high(2*helloStr)
	call	WRITE_TO_LCD
	
	call	SWITCH_TO_SECOND_LCD_LINE		;
	ldi	r30, low(2*helloStr2nd) ; Load address of string.
	ldi	r31, high(2*helloStr2nd)
	call	WRITE_TO_LCD


;	ldi	r16, 0b00011111	
;	ldi	r17, 0b00011111
;	call	BUSY_WAIT
	
	
MAIN____START_OF_MAIN:
	call	GET_BUTTON_STATE
	cpi	r16, 0
	breq	MAIN____DISPLAY_TIME_AND_NEXT_ACTIVATION
	
;	call	SET_PortD_HIGH_OR_LOW ;TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP

	call	MAIN_MENU
	
	rjmp	MAIN____START_OF_MAIN

	
MAIN____DISPLAY_TIME_AND_NEXT_ACTIVATION:
;	ldi	r16, enRelay
	;; call	SET_PortD_HIGH_OR_LOW
	

	
;; 	ldi	r16, 0b01000000
;; 	ldi	r17, 0b00100111	; Maybe have a menu option to aulter this vlaue?
;; 	call	BUSY_WAIT

;; 	ldi	r30,	low(2*secondsInCurrentMinute)
;; 	ldi	r31,	high(2*secondsInCurrentMinute)
;; 	ld	r16,	Z
;; 	call	ODD_OR_EVEN
;; 	ldi	r17,	TRUE
;; 	cp	r16, r17
;; 	brne	NO_SCROLL	
;; 	call	SCROLL_LCD
;; NO_SCROLL:
	rjmp	MAIN____START_OF_MAIN


	;; Returns inverted state of Port B (Port B pull up's are set but we
	;; want 1's for button presses) with int2 (PB2) masked out.
GET_BUTTON_STATE:; Enter, up, down, left and right are all connnected to Port B.
	in	r16, PinB	  ; Load A.
	ori	r16, low(int2Pin) ; Mask out clock signal (set to high.)
	com	r16		  ; One's complement (inverts all bits.)
	ret


MAIN_MENU:
	push	r17
	push	r18
	push	r19	; Used as menuPos counter.
	push	r20	; Used as last menuPos counter.
	push	r30
	push	r31

	clr	r19		; Set menuPos counter to 0.
	mov	r20, r19	; Set last menuPos counter to r19 (menuPos).

MAIN_MENU____MENU_LOOP:
	;; Add delay affter call to GET_BUTTON_STATE (is also called before
	;; calling this rutine.)
	call SCROLL_LCD
	ldi	r17, buttonPressDelaySquaredComp
	ldi	r16, buttonPressDelayLinearComp
	call	BUSY_WAIT
	
	call	GET_BUTTON_STATE
	mov	r18, r16	; Check up and down button state. ==============
	ldi	r17, upButton
	and	r18, r17
	cp	r18, r17
	brne	MAIN_MENU____CHECK_DOWN_BUTTON ; Doesn't set any flags.

	inc	r19			       ; Note that the up button was pressed.
	cpi	r19, maxMainMenuItems ; Check to make sure r19 hasn't gone past maxMainMenuItems -1.
	brne	MAIN_MENU____CHECK_IF_MENU_POS_COUNTER_CHANGED
	clr	r19

MAIN_MENU____CHECK_DOWN_BUTTON:
	mov	r18, r16
	ldi	r17, downButton
	and	r18, r17
	cp	r18, r17
	brne	MAIN_MENU____CHECK_IF_MENU_POS_COUNTER_CHANGED

	cpi	r19, 0b0	; Make sure r19 won't go below 0.
	breq	MAIN_MENU____SET_MENU_POS_COUNTER_TO_MAX
	dec	r19		; Note that the down button was pressed.
	jmp	MAIN_MENU____CHECK_IF_MENU_POS_COUNTER_CHANGED
MAIN_MENU____SET_MENU_POS_COUNTER_TO_MAX:
	ldi	r19, maxMainMenuItems
	dec	r19		; MaxMainMenuIterms is 1 past our max value.

MAIN_MENU____CHECK_IF_MENU_POS_COUNTER_CHANGED:
	;; Here we check r19 aginst the menu pos counter value that was stored
	;; in memory last time MAIN_MENU____MENU_LOOP was iterated over. If it
	;; changed we use r19 to index into a jump table. Otherwise we loop
	;; again.
	cp	r19, r20
	breq	MAIN_MENU____MENU_LOOP
	mov	r20, r19	; Update last menuPos counter.
	mov	r16, r19	; Set up arg for UPDATE_MENU.

	call	UPDATE_MENU

	;; 	call	SCROLL_LCD
	;; ldi	r17, buttonPressDelaySquaredComp ; Add delay before scrolling after updating menu and add delay before next button read.
	;; ldi	r16, buttonPressDelayLinearComp
	;; call	BUSY_WAIT


	jmp	MAIN_MENU____MENU_LOOP

	pop	r31
	pop	r30
	pop	r20
	pop	r19
	pop	r18
	pop	r17
	ret


UPDATE_MENU:
	push	r17
	;; push	r18
	push	r30
	push	r31

	call	CLEAR_LCD
	
	;; TMP=============================================================================================================================================
	;; TMP=============================================================================================================================================
	;; ldi	TCNT0, 0b0	  ; Load timer 0 with 0.
	;; ldi	TCCR0, 0b00000011 ; Set clock source for timer 0 to clk / 64.
	;; TMP=============================================================================================================================================
	;; TMP=============================================================================================================================================
	
	call	CLEAR_SCROLL_STATE ; Set currentMaxLineLen and currentScrollLen to 0.
	;; Load address of jump table and add offset into current menu item selected.
	ldi	r30, low(UPDATE_MENU____JUMP_TABLE)
	add	r30, r16
	ldi	r31, high(UPDATE_MENU____JUMP_TABLE)
	ijmp			; PC <- Z

UPDATE_MENU____JUMP_TABLE:
	rjmp	UPDATE_MENU____DISPLAY_ITEM_1
	rjmp	UPDATE_MENU____DISPLAY_ITEM_2
	rjmp	UPDATE_MENU____DISPLAY_ITEM_3
	rjmp	UPDATE_MENU____DISPLAY_ITEM_4
	rjmp	UPDATE_MENU____DISPLAY_ITEM_5

UPDATE_MENU____DISPLAY_ITEM_1:
	ldi	r30, low(2*mainMenuSelectionStr) ; Current selection arrow.
	ldi	r31, high(2*mainMenuSelectionStr)
	call	WRITE_TO_LCD
	
	ldi	r30, low(2*mainMenuSetTimeStr) ; Current selection.
	ldi	r31, high(2*mainMenuSetTimeStr)
	ldi	r16, mainMenuSelectionStrLen ; R16 is added onto the length of the string at Z.
	call	UPDATE_CURRENT_MAX_LINE_LEN ; Update currentMaxLineLen.
	call	WRITE_TO_LCD
	
	call	SWITCH_TO_SECOND_LCD_LINE
	ldi	r30, low(2*mainMenuSetDateStr) ; Below selection.
	ldi	r31, high(2*mainMenuSetDateStr)
	ldi	r16, 0		; Set arg back to 0.
	call	UPDATE_CURRENT_MAX_LINE_LEN ; Update currentMaxLineLen.
	call	WRITE_TO_LCD
	
	jmp	UPDATE_MENU____EXIT_MENU_UPDATE
	
UPDATE_MENU____DISPLAY_ITEM_2:
	ldi	r30, low(2*mainMenuSelectionStr)
	ldi	r31, high(2*mainMenuSelectionStr)
	call	WRITE_TO_LCD

	ldi	r30, low(2*mainMenuSetBrightnessStr)
	ldi	r31, high(2*mainMenuSetBrightnessStr)
	ldi	r16, mainMenuSelectionStrLen
	call	UPDATE_CURRENT_MAX_LINE_LEN
	call	WRITE_TO_LCD

	call	SWITCH_TO_SECOND_LCD_LINE
	ldi	r30, low(2*mainMenuSetTimeStr)
	ldi	r31, high(2*mainMenuSetTimeStr)
	ldi	r16, 0
	call	UPDATE_CURRENT_MAX_LINE_LEN
	call	WRITE_TO_LCD
	
	jmp	UPDATE_MENU____EXIT_MENU_UPDATE
	
UPDATE_MENU____DISPLAY_ITEM_3:
	ldi	r30, low(2*mainMenuSelectionStr)
	ldi	r31, high(2*mainMenuSelectionStr)
	call	WRITE_TO_LCD

	ldi	r30, low(2*mainMenuDeleteActivationTimeStr)
	ldi	r31, high(2*mainMenuDeleteActivationTimeStr)
	ldi	r16, mainMenuSelectionStrLen
	call	UPDATE_CURRENT_MAX_LINE_LEN
	call	WRITE_TO_LCD

	call	SWITCH_TO_SECOND_LCD_LINE
	ldi	r30, low(2*mainMenuSetBrightnessStr)
	ldi	r31, high(2*mainMenuSetBrightnessStr)
	ldi	r16, 0
	call	UPDATE_CURRENT_MAX_LINE_LEN
	call	WRITE_TO_LCD
	
	jmp	UPDATE_MENU____EXIT_MENU_UPDATE
	
UPDATE_MENU____DISPLAY_ITEM_4:
	ldi	r30, low(2*mainMenuSelectionStr)
	ldi	r31, high(2*mainMenuSelectionStr)
	call	WRITE_TO_LCD

	ldi	r30, low(2*mainMenuSetActivationTimeStr)
	ldi	r31, high(2*mainMenuSetActivationTimeStr)
	ldi	r16, mainMenuSelectionStrLen
	call	UPDATE_CURRENT_MAX_LINE_LEN
	call	WRITE_TO_LCD

	call	SWITCH_TO_SECOND_LCD_LINE
	ldi	r30, low(2*mainMenuDeleteActivationTimeStr)
	ldi	r31, high(2*mainMenuDeleteActivationTimeStr)
	ldi	r16, 0
	call	UPDATE_CURRENT_MAX_LINE_LEN
	call	WRITE_TO_LCD
	
	jmp	UPDATE_MENU____EXIT_MENU_UPDATE
	
UPDATE_MENU____DISPLAY_ITEM_5:
	ldi	r30, low(2*mainMenuSelectionStr)
	ldi	r31, high(2*mainMenuSelectionStr)
	call	WRITE_TO_LCD

	ldi	r30, low(2*mainMenuSetDateStr)
	ldi	r31, high(2*mainMenuSetDateStr)
	ldi	r16, mainMenuSelectionStrLen
	call	UPDATE_CURRENT_MAX_LINE_LEN
	call	WRITE_TO_LCD

	call	SWITCH_TO_SECOND_LCD_LINE
	ldi	r30, low(2*mainMenuSetActivationTimeStr)
	ldi	r31, high(2*mainMenuSetActivationTimeStr)
	ldi	r16, 0
	call	UPDATE_CURRENT_MAX_LINE_LEN
	call	WRITE_TO_LCD
	
	jmp	UPDATE_MENU____EXIT_MENU_UPDATE

UPDATE_MENU____EXIT_MENU_UPDATE:

	pop	r31
	pop	r30
	;; pop	r18
	pop	r17
	ret


CLEAR_SCROLL_STATE:
	push	r16
	push	r30
	push	r31

	ldi	r30, low(2*currentMaxLineLen) ; Load currentMaxLineLen into Z.
	ldi	r31, high(2*currentMaxLineLen)
	ldi	r16, 0b0
	st	Z, r16		; Set *currentMaxLineLen to 0.
	ldi	r30, low(2*currentScrollLen) ; Load currentScrollLen into Z.
	ldi	r31, high(2*currentScrollLen)
	ldi	r16, 0b0
	st	Z, r16		; Set *currentScrollLen to 0 (we havent scrolled the display yet.)
	
	pop	r31
	pop	r30
	pop	r16
	ret


UPDATE_CURRENT_MAX_LINE_LEN:
	push	r16
	push	r17
	push	r30
	push	r31
	
	clr	r17		; Clear string len count.
	add	r17, r16	; Add R16 to the length of the string.

UPDATE_CURRENT_MAX_LINE_LEN____COUNTING:
	lpm	r16, Z+		; Load str character from flash.
	cpi	r16, FALSE	; Check if we've hit the null byte.
	breq	UPDATE_CURRENT_MAX_LINE_LEN____UPDATE_MAX_LINE_LEN ; We've reached the end of the line.
	inc	r17
	jmp	UPDATE_CURRENT_MAX_LINE_LEN____COUNTING

UPDATE_CURRENT_MAX_LINE_LEN____UPDATE_MAX_LINE_LEN:
	ldi	r30, low(2*currentMaxLineLen)
	ldi	r31, high(2*currentMaxLineLen)
	ld	r16, Z		; Load *currentScrollLen from mem into r16.
	cp	r16, r17
	brsh	UPDATE_CURRENT_MAX_LINE_LEN____EXIT ; Don't update currentMaxLineLen. (brsh = Branch if Same or Higher)
	st	Z, r17

	;; call	SET_PortD_HIGH_OR_LOW 

UPDATE_CURRENT_MAX_LINE_LEN____EXIT:

	pop	r31
	pop	r30
	pop	r17
	pop	r16
	ret


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
WRITE_TO_LCD____START_WRITE_TO_LCD:
	lpm	r16, Z+		; Load from flash.
	inc	r18
;	ldi	r17, 0b0
	cpi	r16, FALSE		; Have we hit the null byte?
	breq    WRITE_TO_LCD____END_WRITE_TO_LCD
	cpi	r18, halfDDRAMSize	; Have we reached the end of the display ram for this line.
	brge	WRITE_TO_LCD____END_WRITE_TO_LCD
	;; Output character command.
	out	PortA, r16
	ldi	r16, (low(registerSelectOn) | low(enable))
	out	PortC, r16
	ldi	r17, 0b00000001	; Set up args for BUSY_WAIT
	ldi	r16, 0b00000001
	call	BUSY_WAIT
	ldi	r16, low(registerSelectOn)	; Clear E control signal
	out	PortC, r16
	jmp	WRITE_TO_LCD____START_WRITE_TO_LCD
WRITE_TO_LCD____END_WRITE_TO_LCD:
	
	pop	r18
	pop	r17
	pop	r16
	ret


SWITCH_TO_SECOND_LCD_LINE:
	push	r16
	push	r30
	push	r31
	
	ldi	r16, low(setDDRAMAddressTo2ndLine) ; Switch to second line :)
	call	SEND_LCD_INSTRUCTION
	
	;; ldi	r30, low(2*currentLCDLine)
	;; ldi	r31, high(2*currentLCDLine)
	;; ld	r18, Z
	;; com	r18		; One's complement (inverts all bits.)
	;; st	Z, r18		; Store current LCD line (r18.)
	
	pop	r31
	pop	r30
	pop	r16
	ret


SCROLL_LCD:
	push	r16
	push	r30
	push	r31

	;; TMP=============================================================================================================================================
	;; TMP=============================================================================================================================================
	;; cpi	TIFR, 0x01
	;; brne	SCROLL_LCD____NO_SCROLL_NEEDED
	;; ldi	TIFR, 0x0
	;; TMP=============================================================================================================================================
	;; TMP=============================================================================================================================================
	
	;; Load current max line len (the length of the longest currently displayed line.)
	ldi	r30, low(2*currentMaxLineLen)
	ldi	r31, high(2*currentMaxLineLen)
	ld	r16, Z
	dec	r16		; No branch if less than or equal.
	cpi	r16, displayWidth	; Will scroll if both lines are of length 0 (This shouldn't be the case!)
	brlo	SCROLL_LCD____NO_SCROLL_NEEDED
	call	SCROLL_LCD_PROPER

SCROLL_LCD____NO_SCROLL_NEEDED:
	
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
	breq	SCROLL_LCD_PROPER____FAST_SCROLL_BACK ; We've scrolled to the end of *currentMaxLineLen.

	ldi	r16, shiftDisplayLeft
	call	SEND_LCD_INSTRUCTION

	inc	r17		; Inc *currentScrollLen
	jmp	SCROLL_LCD_PROPER____SCROLL_LCD_PROPER_RET

SCROLL_LCD_PROPER____FAST_SCROLL_BACK:
	ldi	r16, shiftDisplayRight
	call	SEND_LCD_INSTRUCTION
	dec	r17
	cpi	r17, 0b0
	brne	SCROLL_LCD_PROPER____FAST_SCROLL_BACK

SCROLL_LCD_PROPER____SCROLL_LCD_PROPER_RET:
	st	Z, r17		; Store *currentScrollLen
	pop	r17
	pop	r16
	ret


CLEAR_LCD:
	push	r16
	push	r17
	;; ==============   -~   C L E A R   D I S P L A Y   ~-   ==============
	;; /| RS | R/W(hat) | DB7 | DB6 | DB5 | DB4 | DB3 | DB2 | DB1 | DB0 |\
	;; {|---------------------------------------------------------------|}
	;; \| 0  | 0        | 0   | 0   | 0   | 0   | 0   | 0   | 0   | 1   |/

	ldi	r16, low(clearDisplay) ; clear display command
	out	PortA, r16
	;; Set E and RS control siginals.
	ldi	r16, (low(registerSelectOff) | low(enable))
	out	PortC, r16
	ldi	r17, 0b00000001	; Set up args for BUSY_WAIT
	ldi	r16, 0b00000001
	call	BUSY_WAIT
	ldi	r16, low(registerSelectOff)	; Clear E control signal
	out	PortC, r16
	;; We need to wait a while before we can write to the LCD again (we wait
	;; here since we anticipate that we will almost exclusivly be calling
	;; this rutine before WRITE_TO_LCD and because it simplifies the code.)
	ldi	r17, 0b0000111
	ldi	r16, 0b0000111
	call	BUSY_WAIT

	pop	r17
	pop	r16
	ret
	
	
INIT:
	call	DISABLE_JTAG
	
	ldi	r16, low(allHigh) ; Set the following DDRs to outputs.
	call	SET_DDRA	; SET_DDRA doesn't change r16.
	call	SET_DDRC
	call	SET_DDRD
	; Set portD to all low (the relay is connected to this port.)
	ldi	r16, low(allLow)
	call	SET_PortD_HIGH_OR_LOW
	
	ldi	r16, low(allLow) ; Set the following DDRs to inputs.
	;; ldi	r17, low(portBPullupDownValues)
	call	SET_DDRB	 ; Port B is used only for inputs.
	;ldi	r16, low(allLow) ; Set to all pull down resistors for portB.
	ldi	r16, low(allHigh) ; Set pull up resistors.
	call	SET_PortB_HIGH_OR_LOW

	call	INIT_LCD
	call	ENABLE_INT2
	call	ENABLE_TIMER0

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


	;; Set the data direction registers and set pull up/down resistors.
	;; These may be changed latter depending on what is attached to the
	;; pins.
SET_DDRA:
	out	DDRA, r16
	ret


SET_DDRB:
	out	DDRB, r16
	ret


SET_DDRC:
	out	DDRC, r16
	ret


SET_DDRD:
	out	DDRD, r16
	ret


	;; The SET_PortX_HIGH_OR_LOW rutines can be used to set pull up/down
	;; resistors when the port is set as an input or they can be used set
	;; the output of the port high / low
SET_PortA_HIGH_OR_LOW:
	out	PortA, r16
	ret


SET_PortB_HIGH_OR_LOW:
	out	PortB, r16
	ret


SET_PortC_HIGH_OR_LOW:
	out	PortC, r16
	ret


SET_PortD_HIGH_OR_LOW:
	out	PortD, r16
	ret

	
INIT_LCD:
	;; Output display function set command.
	;; Set display parameters (DL (bus width), N (number of lines),
	;; F (font size)). We set the state of the data pins (port A) high first.
	ldi	r16, functionSetData ; SEND_LCD_INSTRUCTION takes r16 as an argument.
	call	SEND_LCD_INSTRUCTION
	call	TURN_ON_DISPLAY
	call	CLEAR_SCROLL_STATE
	ret


SEND_LCD_INSTRUCTION:
	push	r17
	
	out	PortA, r16
	;; Clear control signals (high 3 bit's of port C.)
	ldi	r16, 0b0
	out	PortC, r16
	;; toggle enable bit to send instruction.
	ldi	r16, low(Enable)
	out	PortC, r16	; Send instruction to display.
	ldi	r16, 0b00000001	; Add a small delay to make sure instruction is sent.
	ldi	r17, 0b00000001
	call	BUSY_WAIT
	ldi	r16, 0b0
	out	PortC, r16	; Stop sending.

	pop	r17
	ret


	;; Not that this function also turns on the cursor.
TURN_ON_DISPLAY:
	push	r16
	;; Output display on command.
	ldi	r16, low(displayOnData)
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
BUSY_WAIT____LOOP_0_START:
	inc	r31

	clr	r30
BUSY_WAIT____LOOP_1_START:
	inc	r30

	clr	r29
BUSY_WAIT____LOOP_2_START:
	inc	r29

	cp	r29, r16
	brne	BUSY_WAIT____LOOP_2_START

	cp	r30, r16
	brne	BUSY_WAIT____LOOP_1_START
	
	cp	r31, r17
	brne	BUSY_WAIT____LOOP_0_START
	pop	r29	
	pop	r30
	pop	r31
	ret


ENABLE_INT2:
	push	r16
	ldi	r16, low(int2En)
	out	GICR, r16	; Set external interrupt 2 to be enabled
	pop	r16
	ret


ENABLE_TIMER0:
	push	r16
	
	;ldi	r16, TOIE0	; Timer/Counter0 Overflow Interrupt Enable flag.
	ldi	r16, 0x1
	out	TIMSK, r16	; Enable Timer0 overflow interrupts. TIMSK is the Timer / Counter Interrupt Mask Register.

	ldi	r16, 0x0	; We are counting from 0.
	out	TCNT0, r16	; Set up the Timer/Counter Register 0 with an initial value.

	;ldi	r16, CS00	; Set up r16 with flags for 1024 prescaler.
	;ori	r16, CS02
	ldi	r16, 0x00000101
	out	TCCR0, r16	; Start timer0 with a 1024 prescaler. TCCR0 = Timer/Counter Control Register 0.
	
	pop	r16
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
	push	r16
	push	r30
	push	r31

	;; ldi	r16, 0xff
	;; call	SET_PortD_HIGH_OR_LOW

	ldi	r30, low(2*minutesIdle)
	ldi	r31, high(2*minutesIdle)
	ld	r16, Z
	inc	r16
	st	Z, r16
	call	SET_PortD_HIGH_OR_LOW
;; 	;; ld	r16, Z
;; 	cpi	r16, 0x0
;; 	breq	TIME0_OVF____SET
;; 	;; ldi	r16, 0x0
;; 	;; st	Z, r16
;; 	;; ================= Below 2 are tmp
;; 	ldi	r16, 0x00
;; 	call	SET_PortD_HIGH_OR_LOW
	
;; 	jmp	TIME0_OVF____RESET_COUNTER
;; TIME0_OVF____SET:
;; 	;; ldi	r16, 0x1
;; 	;; st	Z, r16
;; 	;; ================= Below 2 are tmp
;; 	ldi	r16, 0xff
;; 	call	SET_PortD_HIGH_OR_LOW
	
;; TIME0_OVF____RESET_COUNTER:
	
	ldi	r16, 0x0
	out	TCNT0, r16	; Set Timer/Counter Register 0 back to 0.

	pop	r31
	pop	r30
	pop	r16
	reti
SPI_STC:	; SPI Transfer Complete Handler
USART_RXC:	; USART RX Complete Handler
USART_UDRE:	; UDR Empty Handler
USART_TXC:	; USART TX Complete Handler
ADC:		; ADC Conversion Complete Handler
EE_RDY:		; EEPROM Ready Handler
ANA_COMP:	; Analog Comparator Handler
TWSI:		; Two-wire Serial Interface Handler
EXT_INT2:	; IRQ2 Handler Note that another second has passed
;	ldi	r16, low(allHigh) ;
	;call	SET_PortD_HIGH_OR_LOW
	;; push	r30
	;; push	r31
	;; push	r16
	;; ldi	r30,	low(2*secondsInCurrentMinute)
	;; ldi	r31,	high(2*secondsInCurrentMinute)
	;; ld	r16,	Z
	;; We don't want to spend too long in this ISR so we check
	;; secondsInCurrentMinut arcMinute elsewhere
	;; inc	r16,
	;; st	Z, r16
	;; pop	r16
	;; pop	r31
	;; pop	r30
	reti
	
TIM0_COMP:	; Timer0 Compare Handler
SPM_RDY:	; Store Program Memory Ready Handler;

