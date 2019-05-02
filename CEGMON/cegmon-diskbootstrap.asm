;=================================================================
; [$FC00] FLOPPY DISK BOOTSTRAP
;=================================================================
; **** This entry point must not move!

BOOTSTRP
	JSR	BOOT-OFFSET
	JMP	(ZPFD)

	JSR	BOOT-OFFSET
	JMP	NEWMON
					; COMMENTS taken from PEEK(65) V5#1 (Jan'84)
BOOT	LDY	#0			; Select Data Direction Register A
	STY	DISK+1
	STY	DISK			; Assign Port A as all INPUT
	LDX	#4			; Select I/O Port A
	STX	DISK+1
	STY	DISK+3			; Select Data Direction Register B
	DEY				; Get an FF
	STY	DISK+2			; Assign Port B as all OUTPUT
	STX	DISK+3			; Select I/O Port B
	STY	DISK+2			; Write Port B = all high (FF)

	LDA	#$FB			; Set step direction line to 'IN'
	BNE	LFC33			; Skip for first pass

LFC2A	LDA	#2			; Test for 'Track 0' true
	BIT	DISK			; Read Port-A & mask with TRKO bit
	BEQ	LFC4D			; True - exit this loop
	LDA	#$FF			; Else, set step dir line to 'OUT'
LFC33	STA	DISK+2			; Set step direction to given value
	JSR	LFCA5-OFFSET		; Wait 12 clock cycles	
	AND	#$F7			; Select 'STEP' function 

	STA	DISK+2			;
	JSR	LFCA5-OFFSET		; Wait 12 clock cycles
	ORA	#8			; Turn off 'STEP' function
	STA	DISK+2			;
	LDX	#$18			; Wait 30,000 clock cyles
	JSR	WAIT-OFFSET		;  (30 OR 15 ms)
	BEQ	LFC2A			; Loop back for more steps

LFC4D	LDX	#$7F			; Lower the head
	STX	DISK+2			;
	JSR	WAIT-OFFSET		; Wait about 150,000 cycles

LFC55	LDA	DISK			; Wait for the index hole
	BMI	LFC55			;

LFC5A	LDA	DISK			; Wait until the index hole
	BPL	LFC5A			; is gone

	LDA	#3			; Reset the ACIA
	STA	DISK+$10		;
	LDA	#$58			; Select:
					; Receive interrupt disabled,
					; Xmit interrupt disabled,
					; 8 Bits, Even Parity, 1 Stop Bit, /1 clock
	STA	DISK+$10		;
	JSR	DISKIN-OFFSET		; Get a byte from the disk
	STA	ZPFE			; Store as load address hi·and save it in X
	TAX				;
	JSR	DISKIN-OFFSET		; Get another byte
	STA	ZPFD			; Store as load address low
	JSR	DISKIN-OFFSET		; Get a third byte
	STA	ZPFF			; Store it as /I of pages to load
	LDY	#0			; Clear index register

LFC7B	JSR	DISKIN-OFFSET		; Get a data byte
	STA	(ZPFD),Y		; Save it at current location
	INY				; Bump index
	BNE	LFC7B			; Loop until a page is full
	INC	ZPFE			; When a page is full, incr addr hi
	DEC	ZPFF			; decr the # of pages to load
	BNE	LFC7B			; Loop until all pages are done
	STX	ZPFE			; Then, restore addr hi
	LDA	#$FF			; Lift the head
	STA	DISK+2			;
	RTS				; Done! Page Zero is loaded

;------ Timed Wait Routine - Wait 1250 * X + 11 machine cycles

WAIT	LDY	#$F8			; 2   - Get a 248, decimal
LFC93	DEY				; 2   - Inner loop - wait 1240
	BNE	LFC93			; 2/3 - machine cycles
	EOR	ZPFF,X			; 4   - waste 4 cycles
	DEX				; 2   - Wait X * 1250 cycles
	BNE	WAIT			; 2/3 - Loop until done
	RTS				

;------ INPUT CHAR FROM DISK

DISKIN	LDA	DISK+$10		; Wait for ACIA receive flag
	LSR				;
	BCC	DISKIN			;
	LDA	DISK+$11		; It's there, get the byte
LFCA5	RTS
