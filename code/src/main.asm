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
	.set	backButton		= 0b01000000 ; -> PB6
	;	.set	int2Input		= 0b00000100 ; Int 2 is PB2
;	.set	notInt2Input		= 0b11111011 ; For when we don't want int2.
	.set	int2Pin			= 0b00000100
					  
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
	;; ==| Display Commands (the position of the first 1 indicates the command.) |==
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
	;; || CS02  | CS01  | CS00  |                                         ||
	;; ||=======|=======|=================================================||
	;; || 0     | 0     | 0     | No clock source (Timer / Counter stopped)|
	;; || 0     | 0     | 1     | clk (no pre-scaling)                    ||
	;; || 0     | 1     | 0     | clk / 8                                 ||
	;; || 0     | 1     | 1     | clk / 64                                ||
	;; || 1     | 0     | 0     | clk / 256                               ||
	;; || 1     | 0     | 1     | clk / 1024                              ||
	;; || ...     ...     ...     ...                                     ||
	;; =====================================================================
	;; 1000,000 / (256x195) = 20.032... (the TIM0_OVF ISR is run roughly
	;; 20 times a second. This isn't an exact value however see TIM0_OVF...)
	.equ	clock0PreScaler = 0b00000100
	;; (255 -195) = 60 (3C). TIM0_OVF is run when timerCounterRegister overflows.
	.equ	timerCounterRegisterInitialVal = 0x3C
	;; ;; Add 1 and divide by the number of times per second TIM0_OVF is called
	;; ;; to get the number of times per second we scroll.
	;; .equ	lcdScrollInterfaceTimerProperMax = 0x15
	.equ	lcdScrollTimerMax = 0x11
	.equ	arcMinute = 0b00111100 ; Number of seconds in minute.
	.equ	buttonPressDelaySquaredComp = 0b00111111
	.equ	buttonPressDelayLinearComp  = 0b00011011
	 ;; .equ	buttonPressDelayLinearComp = 0b00111101
	;; ================================ Menu ===============================
	.set	maxMainMenuItems	= 0b00000101 ; There are 5 items in our main menu.


	;; ======================== Start Data Segment! ========================
	;; =====================================================================
	.dseg
	;; ==| LCD Related Variables |==
	;; Sould only be changed by SWITCH_TO_SECOND_LCD_LINE (after INIT_LCD.)
	;; currentLCDLine:		.byte 1 ; 0 for first line ff for second line.
	;; Stores the length of the longest line on the display.
	;; Reserve 1 byte (current max line len can't be more than 40 bytes.)
	currentMaxLineLen:	.byte 1
	;; Stores the current number of characters scrolled in from the right.
	currentScrollLen:	.byte 1
	;; Stores the current second (in the current minute.)
	secondsInCurrentMinute:	.byte 1
	;; ==| Backlight Related Variables |==
	;; Should be set to TRUE if the back light is off and FALSE otherwise.
	backLightOff:		.byte 1
	scrollLCD:		.byte 1
	scrollLCDTimer:		.byte 1
	;; Stores the number of minutes since a button was pressed (used to
	;; automatically turn off the screen back light.)
	;; minutesIdle:		.byte 1
	;; interfaceTimer:		.byte 1
	;; ;; Is set to interfaceTimer each time CHECK_IF_SCROLL_LCD_TIME_YET is
	;; ;; run and it does on equal interfaceTimer. If updated
	;; ;; lcdScrollInterfaceTimerProper is incremented (unless it reaches
	;; ;; lcdScrollInterfaceTimerProperMax, in which case it is set back to 0.)
	;; ;; Note that lcdScrollInterfaceTimerProper is incremented before the compare.
	;; lcdScrollInterfaceTimer:	.byte 1
	;; lcdScrollInterfaceTimerProper:	.byte 1
	;; ==| Time And Date Related Variables |==
	currentYear:		.byte 1
	currentMonth:		.byte 1
	currentDay:		.byte 1
	currentHour:		.byte 1
	currentMinute:		.byte 1
	currentSecond:		.byte 1
	
	
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
	;; Main menu str's.
	mainMenuSelectionStr:	.db "~", 0 ; "~" becomes a right facing arrow!
	mainMenuSetTimeStr:	.db "Set Time", 0 ; Item 1.
	mainMenuSetDateStr:	.db "Set Date", 0 ; Item 2.
	mainMenuSetActivationTimeStr:	.db "Set Activation Time", 0 ; Item 3.
	mainMenuDeleteActivationTimeStr:	.db "Delete Activation Time", 0 ; Item 4.
	mainMenuSetBrightnessStr:	.db "Set Brightness", 0 ; Item 5.
	;; Set time str's.
	setTimeSetHoursStr:	.db "   <Set Hour>", 0
	setTimeSetMinutesStr:	.db "  <Set Minute>", 0
	setTimeSetSecondsStr:	.db " <Set  Seconds>", 0
	;; Character number offset.
	charNumberOffset:	.db "0", 0

	
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


	;; ldi	r16, 0b00011111	
	;; ldi	r17, 0b00011111
	;; call	BUSY_WAIT
	
	
MAIN____START_OF_MAIN:
	call	GET_BUTTON_STATE

	call	SET_PORT_D_HIGH_OR_LOW
	;; ldi	r16, 0b11111111	
	;; ldi	r17, 0b11111111
	;; call	BUSY_WAIT
	
	cpi	r16, 0
	breq	MAIN____DISPLAY_TIME_AND_NEXT_ACTIVATION
	
;	call	SET_PORT_D_HIGH_OR_LOW ;TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP

	call	MAIN_MENU
	
	rjmp	MAIN____START_OF_MAIN

	
MAIN____DISPLAY_TIME_AND_NEXT_ACTIVATION:
;	ldi	r16, enRelay
	;; call	SET_PORT_D_HIGH_OR_LOW
	

	
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
	ori	r16, int2Pin
	;; andi	r16, comInt2Pin ; Mask out clock signal (set to high.)
	com	r16		  ; One's complement (inverts all bits.) Pull up's are set on pins.
	ret


MAIN_MENU:
	push	r17
	push	r18
	push	r19	; Used as menuPos counter.
	push	r20	; Used as last menuPos counter.
	push	r30
	push	r31

	;; push	r16
	;; ldi	r17, buttonPressDelaySquaredComp ; Add delay as button was just pressed.
	;; ldi	r16, buttonPressDelayLinearComp
	;; call	BUSY_WAIT
	;; pop	r16

	clr	r19		; Set menuPos counter to 0.
	mov	r20, r19	; Set last menuPos counter to r19 (menuPos).

MAIN_MENU____MENU_LOOP:
	;; Add delay affter call to GET_BUTTON_STATE (is also called before
	;; calling this rutine.)
	;call SCROLL_LCD
	
	call	GET_BUTTON_STATE
	call	SET_PORT_D_HIGH_OR_LOW ; TMP=======TMP=========TMP==========TMP`=`=`=`=`==`TMP=`=`=`=`=`=`=TMP`=`=`=`=`=`=`=TMP`=`=`=`=`TMP=`=`=`=`=`=`TMP
	;; Check enter button state. ==============
	mov	r18, r16
	ldi	r17, enterButton
	and	r18, r17
	cp	r18, r17
	brne	MAIN_MENU____CHECK_UP_BUTTON ; Has the user used the enter button?

	;; Setup arg for ENTER_SUB_MENU (last menuPos counter). We don't want to
	;; take into consideration the simultanious pressing of the up and or
	;; down buttons with the enter button, hence the use of last menuPos
	;; counter.
	mov	r16, r20
	call	ENTER_SUB_MENU

	rjmp	MAIN_MENU____UPDATE_MAIN_MENU_THEN_TO_MENU_LOOP
	
MAIN_MENU____CHECK_UP_BUTTON:
	mov	r18, r16	; Prior and clobbers r18.
	ldi	r17, upButton
	and	r18, r17
	cp	r18, r17
	brne	MAIN_MENU____CHECK_DOWN_BUTTON ; Doesn't set any flags.

	inc	r19			       ; Note that the up button was pressed.
	cpi	r19, maxMainMenuItems ; Check to make sure r19 hasn't gone past maxMainMenuItems -1.
	brne	MAIN_MENU____CHECK_IF_MENU_POS_COUNTER_CHANGED
	clr	r19

MAIN_MENU____CHECK_DOWN_BUTTON:
	mov	r18, r16	; Prior and clobbers r18.
	ldi	r17, downButton
	and	r18, r17	; r18 contains ret val of GET_BUTTON_STATE.
	cp	r18, r17
	brne	MAIN_MENU____CHECK_IF_MENU_POS_COUNTER_CHANGED

	cpi	r19, 0b0	; Make sure r19 won't go below 0.
	breq	MAIN_MENU____SET_MENU_POS_COUNTER_TO_MAX
	dec	r19		; Note that the down button was pressed.
	rjmp	MAIN_MENU____CHECK_IF_MENU_POS_COUNTER_CHANGED
MAIN_MENU____SET_MENU_POS_COUNTER_TO_MAX:
	ldi	r19, maxMainMenuItems
	dec	r19		; MaxMainMenuIterms is 1 past our max value.

MAIN_MENU____CHECK_IF_MENU_POS_COUNTER_CHANGED:
	;; Here we check r19 aginst the menu pos counter value that was stored
	;; in memory last time MAIN_MENU____MENU_LOOP was iterated over. If it
	;; changed we use r19 to index into a jump table. Otherwise we loop
	;; again.
	cp	r19, r20
	breq	MAIN_MENU____TO_MENU_LOOP
	mov	r20, r19	; Update last menuPos counter.
	mov	r16, r19	; Set up arg for UPDATE_MENU.

MAIN_MENU____UPDATE_MAIN_MENU_THEN_TO_MENU_LOOP:
	call	UPDATE_MAIN_MENU

MAIN_MENU____TO_MENU_LOOP:	; We have to call SCROLL_LCD before the next loop.

	push	r17
	push	r16
	ldi	r17, buttonPressDelaySquaredComp ; Add delay before scrolling after updating menu and add delay before next button read.
	ldi	r16, buttonPressDelayLinearComp
	call	BUSY_WAIT
	pop	r16
	pop	r17
	
	call	SCROLL_LCD
	rjmp	MAIN_MENU____MENU_LOOP

	pop	r31
	pop	r30
	pop	r20
	pop	r19
	pop	r18
	pop	r17
	ret


UPDATE_MAIN_MENU:
	push	r17
	push	r30
	push	r31

	call	CLEAR_LCD
	call	CLEAR_SCROLL_STATE ; Set currentMaxLineLen and currentScrollLen to 0.
	;; Load address of jump table and add offset into current menu item selected.
	ldi	r30, low(UPDATE_MAIN_MENU____JUMP_TABLE)
	add	r30, r16
	ldi	r31, high(UPDATE_MAIN_MENU____JUMP_TABLE)
	ijmp			; PC <- Z

UPDATE_MAIN_MENU____JUMP_TABLE:
	rjmp	UPDATE_MAIN_MENU____DISPLAY_ITEM_1
	rjmp	UPDATE_MAIN_MENU____DISPLAY_ITEM_2
	rjmp	UPDATE_MAIN_MENU____DISPLAY_ITEM_3
	rjmp	UPDATE_MAIN_MENU____DISPLAY_ITEM_4
	rjmp	UPDATE_MAIN_MENU____DISPLAY_ITEM_5

UPDATE_MAIN_MENU____DISPLAY_ITEM_1:
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
	
	rjmp	UPDATE_MAIN_MENU____EXIT_MENU_UPDATE
	
UPDATE_MAIN_MENU____DISPLAY_ITEM_2:
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
	
	rjmp	UPDATE_MAIN_MENU____EXIT_MENU_UPDATE
	
UPDATE_MAIN_MENU____DISPLAY_ITEM_3:
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
	
	rjmp	UPDATE_MAIN_MENU____EXIT_MENU_UPDATE
	
UPDATE_MAIN_MENU____DISPLAY_ITEM_4:
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
	
	rjmp	UPDATE_MAIN_MENU____EXIT_MENU_UPDATE
	
UPDATE_MAIN_MENU____DISPLAY_ITEM_5:
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
	
	rjmp	UPDATE_MAIN_MENU____EXIT_MENU_UPDATE

UPDATE_MAIN_MENU____EXIT_MENU_UPDATE:

	pop	r31
	pop	r30
	pop	r17
	ret


;; Some of these might not really be menues. However the name ENTER_SUB_MENU
;; seems to make enough sense.
ENTER_SUB_MENU:			;  last menuPos counter is passed in via r16.
	push	r30
	push	r31

	call	CLEAR_LCD

	ldi	r30, low(ENTER_SUB_MENU____JUMP_TABLE)
	add	r30, r16
	ldi	r31, high(ENTER_SUB_MENU____JUMP_TABLE)
	ijmp			; PC <- Z

ENTER_SUB_MENU____JUMP_TABLE:
	rjmp	ENTER_SUB_MENU____JUMP_TABLE____ENTER_SUB_MENU_1
	rjmp	ENTER_SUB_MENU____JUMP_TABLE____ENTER_SUB_MENU_2
	rjmp	ENTER_SUB_MENU____JUMP_TABLE____ENTER_SUB_MENU_3
	rjmp	ENTER_SUB_MENU____JUMP_TABLE____ENTER_SUB_MENU_4
	rjmp	ENTER_SUB_MENU____JUMP_TABLE____ENTER_SUB_MENU_5

ENTER_SUB_MENU____JUMP_TABLE____ENTER_SUB_MENU_1:
	call	SET_TIME
	rjmp	ENTER_SUB_MENU____EXIT
	
ENTER_SUB_MENU____JUMP_TABLE____ENTER_SUB_MENU_2:
	call	SET_BRIGHTNESS
	rjmp	ENTER_SUB_MENU____EXIT
	
ENTER_SUB_MENU____JUMP_TABLE____ENTER_SUB_MENU_3:
	call	DELETE_ACTIVATION_TIME
	rjmp	ENTER_SUB_MENU____EXIT
	
ENTER_SUB_MENU____JUMP_TABLE____ENTER_SUB_MENU_4:
	call	SET_ACTIVATION_TIME
	rjmp	ENTER_SUB_MENU____EXIT
	
ENTER_SUB_MENU____JUMP_TABLE____ENTER_SUB_MENU_5:
	call	SET_DATE

ENTER_SUB_MENU____EXIT:	
	
	pop	r31
	pop	r30
	ret


SET_TIME:
	push	r16
	push	r17
	push	r18
	;; R19 is used to note which of the temporary time fields (hours,
	;; minutes and seconds) have been modified so far. We will refere to r19
	;; as "SET_TIME_STATUS". We set SET_TIME_STATUS to 0x00 for not
	;; modified, 0x01 for hours modified, 0x02 for hours and minutes
	;; modified and 0x03 for hours, minutes and seconds modified. When
	;; SET_TIME_STATUS is set to 0x03 we reset the decade counters and
	;; update the the time value stored in SRAM with the new time value and
	;; return from the subroutine.
	push	r19
	push	r20		; R20 is used to store hours temporarily.
	push	r21		; R21 is used to store minutes temporarily.
	push	r22		; R22 is used to store seconds temporarily.
	push	r30
	push	r31

	;; Returns current hours, minutes and seconds via r20, r21 and r22
	call	SET_TIME_GET_TIME	; respectively.

	ldi	r19, 0b0	; 0 for set hours str.
SET_TIME____DISPLAY_CURRENT_TIME_FIELD_AND_VALUE:
	mov	r16, r19	; R19 (SET_TIME_STATUS) tells which (h, m or s).
	call	SET_TIME_PRINT_CURRENT_TIME_FIELD
	
SET_TIME____GET_BUTTON_STATE:
	ldi	r17, buttonPressDelaySquaredComp ; Add delay before next button read.
	ldi	r16, buttonPressDelayLinearComp
	call	BUSY_WAIT

	call	GET_BUTTON_STATE
	mov	r18, r16
	ldi	r17, enterButton
	and	r18, r17
	;; mov	r16, r18
	;; call	SET_PORT_D_HIGH_OR_LOW	
	cp	r18, r17
	brne	SET_TIME____CHECK_UP_BUTTON
	;; =====================================================================
	;; ENTER PRESSED. Update SET_TIME_STATUS. ==============================
	rjmp	SET_TIME____CHECK_SET_AND_MAYBE_SET_TIME_STATUS

SET_TIME____CHECK_UP_BUTTON:
	mov	r18, r16
	ldi	r17, upButton
	and	r18, r17
	cp	r18, r17
	brne	SET_TIME____CHECK_DOWN_BUTTON
	;; =====================================================================
	;; UP BUTTON PRESSED (increment currently selected field.) =============

	rjmp	SET_TIME____DISPLAY_TIME_FIELD

SET_TIME____CHECK_DOWN_BUTTON:
	mov	r18, r16
	ldi	r17, downButton
	and	r18, r17
	cp	r18, r17
	brne	SET_TIME____CHECK_BACK_BUTTON
	;; =====================================================================
	;; DOWN BUTTON PRESSED (decrement currently selected field.) ===========

	rjmp	SET_TIME____DISPLAY_TIME_FIELD

SET_TIME____CHECK_BACK_BUTTON:
	mov	r18, r16
	ldi	r17, backButton
	and	r18, r17
	cp	r18, r17
	brne	SET_TIME____GET_BUTTON_STATE ; No relevant button was pressed, loop again.
	;; =====================================================================
	;; BACK BUTTON PRESSED (decrement currently selected field.) ===========
	
	subi	r19, 0b01
	brpl	SET_TIME____DISPLAY_TIME_FIELD ; Branch if plus [if (N = 0) then PC <- PC + k + 1].
	;; R19 went negative and therefore we're updating the previous field (or
	;; exiting without updating the time.)
	rjmp	SET_TIME____EXIT

	;; Update time fields on display and loop back to SET_TIME____GET_BUTTON_STATE.
SET_TIME____DISPLAY_TIME_FIELD:
	;	Temp to test displaying number vvvvvv.
	inc	r20		;	Temp to test displaying number.
	mov	r16, r20	;	Temp to test displaying number.
	;	Temp to test displaying number ^^^^^^.
	call	DISPLAY_NUMBER
	rjmp	SET_TIME____GET_BUTTON_STATE

	;; Here we check SET_TIME_STATUS. If it is 0x03 we follow the procedure
	;; layed our in the comment at that start of this rutine. Otherwise we
	;; increment SET_TIME_STATUS, update the LCD to reflect SET_TIME_STATUSs
	;; new value and read the button state again.
SET_TIME____CHECK_SET_AND_MAYBE_SET_TIME_STATUS:
	inc	r19		; We're updating the next time field (or exiting and updating the time.)
	cpi	r19, 0x03
	brne	SET_TIME____DISPLAY_CURRENT_TIME_FIELD_AND_VALUE
	;; Enter was pressed while setting seconds... Update time and return.
	ldi	r30, low(2*currentHour)
	ldi	r31, high(2*currentHour)
	st	Z, r20		; Set hours.
	ldi	r30, low(2*currentMinute)
	ldi	r31, high(2*currentMinute)
	st	Z, r21		; Set minutes.
	ldi	r30, low(2*currentSecond)
	ldi	r31, high(2*currentSecond)
	st	Z, r22		; Set seconds.
	;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	;; Reset decade counters!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SET_TIME____EXIT:		; Jump here if we are exiting without updating the time.
	
	pop	r31
	pop	r30
	pop	r22
	pop	r21
	pop	r20
	pop	r19
	pop	r18
	pop	r17
	pop	r16
	ret


		;; Returns current hours, minutes and seconds via r20, r21 and r22
SET_TIME_GET_TIME:			; respectively.
	push	r30
	push	r31

	;; Load current time into ret registers (r20 (h), r21 (m), r22 (s)).
	ldi	r30, low(2*currentHour)
	ldi	r31, high(2*currentHour)
	ld	r20, Z		; Load current hour.
	ldi	r30, low(2*currentMinute)
	ldi	r31, high(2*currentMinute)
	ld	r21, Z		; Load current minute.
	ldi	r30, low(2*currentSecond)
	ldi	r31, high(2*currentSecond)
	ld	r22, Z		; Load current second.

	pop	r31
	pop	r31
	ret


	;; R16 should contain a value in the range [0, 2]. A different string
	;; will be printed for each value in the range. These strings are for
	;; hours (0), minutes (1) and seconds (2). The behaviour of this rutine
	;; is undefined for any values outside of the aformentioned range.
SET_TIME_PRINT_CURRENT_TIME_FIELD:
	push	r30
	push	r31

	call	CLEAR_LCD
	call	CLEAR_SCROLL_STATE
	cpi	r16, 0
	brne	SET_TIME_PRINT_CURRENT_TIME_FIELD____CHECK_MINUTES
	;; Print str for set hours...
	ldi	r30, low(2*setTimeSetHoursStr)
	ldi	r31, high(2*setTimeSetHoursStr)
	ldi	r16, 0 		; R16 is added onto the length of the string at Z.
	call	UPDATE_CURRENT_MAX_LINE_LEN
	call	WRITE_TO_LCD
	;; call	SWITCH_TO_SECOND_LCD_LINE
	;; ldi	r30, low(2*mainMenuSelectionStr)
	;; ldi	r31, high(2*mainMenuSelectionStr)
	;; ldi	r16, 0 		; R16 is added onto the length of the string at Z.
	;; call	UPDATE_CURRENT_MAX_LINE_LEN
	;; call	WRITE_TO_LCD
	rjmp	SET_TIME_PRINT_CURRENT_TIME_FIELD____EXIT
	
SET_TIME_PRINT_CURRENT_TIME_FIELD____CHECK_MINUTES:
	cpi	r16, 1
	brne	SET_TIME_PRINT_CURRENT_TIME_FIELD____CHECK_SECONDS
	;; Print str for set minutes...
	ldi	r30, low(2*setTimeSetMinutesStr)
	ldi	r31, high(2*setTimeSetMinutesStr)
	ldi	r16, 0 		; R16 is added onto the length of the string at Z.
	call	UPDATE_CURRENT_MAX_LINE_LEN
	call	WRITE_TO_LCD
;; call	SWITCH_TO_SECOND_LCD_LINE
	;; ldi	r30, low(2*mainMenuSelectionStr)
	;; ldi	r31, high(2*mainMenuSelectionStr)
	;; ldi	r16, 0 		; R16 is added onto the length of the string at Z.
	;; call	UPDATE_CURRENT_MAX_LINE_LEN
	;; call	WRITE_TO_LCD
	rjmp	SET_TIME_PRINT_CURRENT_TIME_FIELD____EXIT

SET_TIME_PRINT_CURRENT_TIME_FIELD____CHECK_SECONDS:
	;; Print str for set seconds...
	ldi	r30, low(2*setTimeSetSecondsStr)
	ldi	r31, high(2*setTimeSetSecondsStr)
	ldi	r16, 0 		; R16 is added onto the length of the string at Z.
	call	UPDATE_CURRENT_MAX_LINE_LEN
	call	WRITE_TO_LCD
;; call	SWITCH_TO_SECOND_LCD_LINE
	;; 	ldi	r30, low(2*mainMenuSelectionStr)
	;; ldi	r31, high(2*mainMenuSelectionStr)
	;; ldi	r16, 0 		; R16 is added onto the length of the string at Z.
	;; call	UPDATE_CURRENT_MAX_LINE_LEN
	;; call	WRITE_TO_LCD
	
SET_TIME_PRINT_CURRENT_TIME_FIELD____EXIT:
	;; The current value for the field is displayed on the second line.
	call	SWITCH_TO_SECOND_LCD_LINE

	pop	r31
	pop	r30
	ret


	;; The value of r16 and r17 taken as one number is output to
	;; the LCD as a number.
DISPLAY_NUMBER:
	push	r18
	push	r19
	push	r22
	push	r16
	push	r17
	push	r20
	push	r21
	push	r30
	push	r31

	;; push	r22
	;; push	r23
	;; push	r24
	;; push	r25
	;; push	r16
	;; push	r17

;; 	clc			; Clear carry.
;; 	ldi	r22, 0b0		; Clear Mod10.
;; ;	mov	r1, r16

;; 	ldi	r24, 0b00010000
;; DIVIDE_LOOP:
;; 	ldi	r22, 0b0
;; ;	rol	r1	; Value
;; 	rol	r22	; Mod10
;; 	mov	r23, r22

;; 	sec		; Set carry flag.
;; 	subi	r23, 0b1010 ; Mod10 - 10. (r22 = dividend - divisor).
;; 	; Branch if carry clear. (Branch if dividend < divisor).
;; 	brcc DISPLAY_NUMBER____IGNORE_RESULT
;; 	inc	r23
;; 	mov	r22, r23

;; DISPLAY_NUMBER____IGNORE_RESULT:
;; 	dec	r24
;; 	brne	DIVIDE_LOOP

;; 	ldi	r25, 0b00000011
;; 	clc
;; 	adc	r22, r25
	
		ldi	r16,	0b0111
		ldi	r17,	0b0

		clr	r18		; Clear low byte of mod10.
		clr	r19		; Clear high byte of mod10.

		clc			; Clear carry.
		ldi	r22, 0b10000	; Loop counter (start at 16.)
	
	DISPLAY_NUMBER____DIV_LOOP:	
		;; Rotate quotient and remainder.
		rol	r16		; AKA low byte of value.
		rol	r17		; AKA high byte of value.
		rol	r18		; AKA low byte of mod10.
		rol	r19		; AKA high byte of mod10.

		;;  r19:r18 = dividend - divisor.
		mov	r20, r18	; We must save r18 and r19 for latter.
		mov	r21, r19
		sec			; Set carry flag.
		sbci	r20, 0b1010	; Sub with carry 10 from low byte of mod10.
		sbci	r21, 0b0	; Sub with carry 0 from high byte of mod10.

		brcc	DISPLAY_NUMBER____IGNORE_RESULT	; Brcc (branch if carry
					; cleared). Branch if dividend < divisor.
		mov	r18, r20	; Clobber mod10 with new mod10.
		mov	r19, r21

	DISPLAY_NUMBER____IGNORE_RESULT:
		dec	r22
		brne	DISPLAY_NUMBER____DIV_LOOP ; If(Z != 1).


	;; ldi	r22, 0b0

	;; ldi	r30,	low(2*charNumberOffset)
	;; ldi	r31,	high(2*charNumberOffset)
	;; ld	r16,	Z
	
	;; add	r22, r16
	;; mov	r17, r22

	ldi	r30,	low(2*charNumberOffset)
	ldi	r31,	high(2*charNumberOffset)
	lpm	r22,	Z
	;; add	r22,	r16
	add	r22,	r18
	;; mov	r16,	r22


	;; ldi	r22, 0b11001110 ; = ho Katakana character.
	
	;; Output character command.
	out	PortA, r22
	ldi	r22, (low(registerSelectOn) | low(enable))
	out	PortC, r22
	ldi	r17, 0b00000001	; Set up args for BUSY_WAIT
	ldi	r22, 0b00000001
	call	BUSY_WAIT
	ldi	r22, low(registerSelectOn)	; Clear E control signal
	out	PortC, r22
	
	;; pop	r17
	;; pop	r16
	;; pop	r25
	;; pop	r24
	;; pop	r23
	;; pop	r22

	pop	r31
	pop	r30
	pop	r21
	pop	r20
	pop	r17
	pop	r16
	pop	r22
	pop	r19
	pop	r18
	
	ret


SET_BRIGHTNESS:
	ret


DELETE_ACTIVATION_TIME:
	ret


SET_ACTIVATION_TIME:
	ret

SET_DATE:
	ret


CLEAR_SCROLL_STATE:
	push	r16

	;; The next two groups of instructions (not including the two pushs) 
	;; ensure that we wait roughly a full character scroll time before
	;; scrolling again.
	;; Reset Timer/ Counter Register 0 back to 0.
	ldi	r16, timerCounterRegisterInitialVal
	out	TCNT0, r16	; Set Timer/Counter Register 0 back to 0.
	
	push	r30
	push	r31

	;; Reset scrollLCDTimer.
	ldi	r30, low(2*scrollLCDTimer)
	ldi	r31, high(2*scrollLCDTimer)
	ldi	r16, 0b0
	st	Z, r16

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


	;; The value in r16 is added to the length of the string pointer to by
	;; r30:r31 (assuming that's the right endianness.) The calculated length
	;; is stored in currentMaxLineLen.
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
	rjmp	UPDATE_CURRENT_MAX_LINE_LEN____COUNTING

UPDATE_CURRENT_MAX_LINE_LEN____UPDATE_MAX_LINE_LEN:
	ldi	r30, low(2*currentMaxLineLen)
	ldi	r31, high(2*currentMaxLineLen)
	ld	r16, Z		; Load *currentScrollLen from mem into r16.
	cp	r16, r17
	brsh	UPDATE_CURRENT_MAX_LINE_LEN____EXIT ; Don't update currentMaxLineLen. (brsh = Branch if Same or Higher)
	st	Z, r17

	;; call	SET_PORT_D_HIGH_OR_LOW 

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
	;; and only Z can be used to indirect read the flash memory, and no pre-decrement or displacement are available:
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
	rjmp	WRITE_TO_LCD____START_WRITE_TO_LCD
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
	
	;; Load current max line len (the length of the longest currently displayed line.)
	ldi	r30, low(2*currentMaxLineLen)
	ldi	r31, high(2*currentMaxLineLen)
	ld	r16, Z
	dec	r16		; No branch if less than or equal.
	cpi	r16, displayWidth	; Will scroll if both lines are of length 0 (This shouldn't be the case!)
	brlo	SCROLL_LCD____NO_SCROLL_NEEDED
	
	;; Scrolling is needed. Check if it's time to scroll yet.
				; ======================== SUSPECT CODE! ===========
	call	CHECK_IF_SCROLL_LCD_TIME_YET ; =========== SUSPECT CODE! ===========
	cpi	r16, TRUE
	brne	SCROLL_LCD____NO_SCROLL_NEEDED ; ========= SUSPECT CODE! ===========
				; ======================== SUSPECT CODE! ===========

	ldi	r30, low(2*currentMaxLineLen) ; SCROLL_LCD_PROPER needs currentMaxLineLen in r16.
	ldi	r31, high(2*currentMaxLineLen)
	ld	r16, Z
	call	SCROLL_LCD_PROPER

SCROLL_LCD____NO_SCROLL_NEEDED:
	
	pop	r31
	pop	r30
	pop	r16
	ret


	;; Returns TRUE if the rutine has been called AND seen that
	;; interfaceTimer has changed since the last call
	;; lcdScrollInterfaceTimerProper times.
	;; In retrospect we should have used a didicated timer for this because
	;; if CHECK_IF_SCROLL_LCD_TIME_YET is not called at least once between
	;; each timer interrupt then interfaceTimer
CHECK_IF_SCROLL_LCD_TIME_YET:
	push	r17
	push	r30
	push	r31
	
	ldi	r30, low(2*scrollLCD)
	ldi	r31, high(2*scrollLCD)
	ld	r16, Z
	ldi	r17, FALSE
	st	Z, r17		; Set scrollLCD back to false.

	pop	r31
	pop	r30
	pop	r17
	ret
	

	;; Takes r16 as an argument should contain *currentMaxLineLen.
SCROLL_LCD_PROPER:
	push	r16
	push	r17
	push	r30
	push	r31

	ldi	r30, low(2*currentScrollLen)
	ldi	r31, high(2*currentScrollLen)
	ld	r17, Z		; Load *currentScrollLen into r17
	subi	r16, displayWidth ; We've already made sure r16 > displayWidth in SCROLL_LCD.
	cp	r17, r16
	breq	SCROLL_LCD_PROPER____FAST_SCROLL_BACK ; We've scrolled to the end of *currentMaxLineLen.
	;; Scroll LCD (one char in from right.) vvv   vvv   vvv
	ldi	r16, shiftDisplayLeft
	call	SEND_LCD_INSTRUCTION
	inc	r17		; Inc *currentScrollLen
	;; Scroll LCD                           ^^^   ^^^   ^^^
	rjmp	SCROLL_LCD_PROPER____SCROLL_LCD_PROPER_RET
	
	;; Scroll LCD (back to start.) vvv   vvv   vvv
;; SCROLL_LCD_PROPER____FAST_SCROLL_BACK_START:
	;; push	r17		; Add delay before scoll back.
	;; ldi	r17, 0b00000001
	;; ldi	r16, 0b00000001
	;; call	BUSY_WAIT
	;; pop	r17
SCROLL_LCD_PROPER____FAST_SCROLL_BACK:
	ldi	r16, shiftDisplayRight
	call	SEND_LCD_INSTRUCTION
	dec	r17
	cpi	r17, 0b0
	brne	SCROLL_LCD_PROPER____FAST_SCROLL_BACK
	;; push	r17		; Add delay after scroll back.
	;; ldi	r17, 0b00000001
	;; ldi	r16, 0b00000001
	;; call	BUSY_WAIT
	;; pop	r17
	;; Scroll LCD                  ^^^   ^^^   ^^^
	
SCROLL_LCD_PROPER____SCROLL_LCD_PROPER_RET:
	st	Z, r17		; Store *currentScrollLen

	pop	r31
	pop	r30
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
	call	SET_PORT_D_HIGH_OR_LOW
	
	ldi	r16, low(allLow) ; Set the following DDRs to inputs.
	;; ldi	r17, low(portBPullupDownValues)
	call	SET_DDRB	 ; Port B is used only for inputs.
	;ldi	r16, low(allLow) ; Set to all pull down resistors for portB.
	ldi	r16, low(allHigh) ; Set pull up resistors.
	call	SET_PORT_B_HIGH_OR_LOW

	call	INIT_TIME_AND_DATE

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
SET_PORT_A_HIGH_OR_LOW:
	out	PortA, r16
	ret


SET_PORT_B_HIGH_OR_LOW:
	out	PortB, r16
	ret


SET_PORT_C_HIGH_OR_LOW:
	out	PortC, r16
	ret


SET_PORT_D_HIGH_OR_LOW:
	out	PortD, r16
	ret


INIT_TIME_AND_DATE:		; We initialize all time and date values to 0 here.
	push	r16
	push	r17
	push	r30
	push	r31
	
	ldi	r17, 0b0
	ldi	r30, low(2*currentYear)
	ldi	r31, high(2*currentYear)
	st	Z, r17		; Zero years.
	ldi	r30, low(2*currentMonth)
	ldi	r31, high(2*currentMonth)
	st	Z, r17		; Zero months.
	ldi	r30, low(2*currentDay)
	ldi	r31, high(2*currentDay)
	st	Z, r17		; Zero days.
	ldi	r30, low(2*currentHour)
	ldi	r31, high(2*currentHour)
	st	Z, r17		; Zero hours.
	ldi	r30, low(2*currentMinute)
	ldi	r31, high(2*currentMinute)
	st	Z, r17		; Zero minutes.
	ldi	r30, low(2*currentSecond)
	ldi	r31, high(2*currentSecond)
	st	Z, r17		; Zero seconds.

	pop	r31
	pop	r30
	pop	r17
	pop	r16
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
	ldi	r16, Enable
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
	push	r30
	push	r31

	ldi	r30, low(2*scrollLCDTimer)
	ldi	r31, high(2*scrollLCDTimer)
	clr	r16
	st	Z, r16		; Set scrollLCDTimer to a defined initial value.

	;; ldi	r30, low(2*scrollLCD)
	;; ldi	r31, high(2*scrollLCD)
	;; ldi	r16, FALSE
	;; st	Z, r16		; Set scrollLCD to a defined initial value.
	
	; Enable Timer0 overflow interrupts.
	ldi	r16, 0x01
	out	TIMSK, r16	;TIMSK = Timer / Counter Interrupt Mask Register.
	;; Set up the TCNT0 register 0 with an initial value.
	ldi	r16, timerCounterRegisterInitialVal
	out	TCNT0, r16	; TCNT0 = Timer/Counter.
	;; Start timer0 with the prescaler that corresponds to clock0PreScaler.
	ldi	r16, clock0PreScaler
	out	TCCR0, r16	; TCCR0 = Timer/Counter Control Register 0.

	pop	r31
	pop	r30
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
	;; We put this code before the other push's because the counter will
	;; still be counting when the ISR is called and thus the sooner we
	;; update TNCT0 the better. We would try to subtract the number of
	;; unaccounted for cycles from timerCounterRegisterInitialVal but we
	;; couldn't find any good information on how long it takes to start an
	;; ISR on the ATmega16.
	ldi	r16, timerCounterRegisterInitialVal
	out	TCNT0, r16	; Set Timer/Counter Register 0 back to 0.
	push	r30
	push	r31

	ldi	r30, low(2*scrollLCDTimer)
	ldi	r31, high(2*scrollLCDTimer)
	ld	r16, Z		; Load scrollLCDTimer.
	inc	r16		; Inc first to account for off by one error.
	cpi	r16, lcdScrollTimerMax
	brne	TIM0_OVF____EXIT ; Branch if scrollLCDTimer != lcdScrollTimerMax.

	ldi	r16, TRUE	; Note that it is time to scroll.
	ldi	r30, low(2*scrollLCD)
	ldi	r31, high(2*scrollLCD)
	st	Z, r16
	clr	r16		; Reset scrollLCDTimer.

TIM0_OVF____EXIT:
	ldi	r30, low(2*scrollLCDTimer)
	ldi	r31, high(2*scrollLCDTimer)
	st	Z, r16		; Save scrollLCDTimer.

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
	;call	SET_PORT_D_HIGH_OR_LOW
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

