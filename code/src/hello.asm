;; Turn on an LED which is connnected to PC0

	.include "./include/m16Adef.inc" 

	ldi r16, 0b00000001
	out DDRC, r16
	out PortC, r16
Start:
	rjmp Start
