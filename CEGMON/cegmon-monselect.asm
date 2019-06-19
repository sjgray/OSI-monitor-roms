;=================================================================
; CEGMON MONITOR - CUSTOM CODE - MONITOR ROM SELECTION MENU
;=================================================================
; This is custom code that replaces the OSI DISK BOOTSTRAP (about
; 166 bytes). It is included when OPTDISK=3.
;
; There is ONE patch:
; INITIALIZATION:
;   "JSR CUSTOM" is called via the RESET routine in place of
;   JSR INITMEM. You should call INITMEM unless you have your own
;   init routine. You need to RTS when done. 
;
; NOTES:
;   This code takes up almost the entire space that the Disk Bootstrap
;   previously used.
;
;-----------------------------------------------------------------
; NEW FEATURES
;-----------------------------------------------------------------
;
; This code takes advantage of the extra BK0 and BK1 lines of the
; 600D control register. We will connect these lines to the upper
; 2 address lines of an 8K ROM in the monitor rom socket. This code
; will be placed in the first 2K of the ROM and when the machine
; boots, BK0 and BK1 will be zero thereby selecting this ROM.
; On boot a selection menu will appear with 4 options. The USER can
; then choose 1,2,3, or 4 which will set the appropriate BK lines,
; and then that selected ROM will be RUN as if the machine had just
; booted.
;
; CEGMON is used as the base, so selecting OPTION 1 from the menu
; will us this monitor rom, but without the disk bootstrap option.
; You will get a "C/W/M" prompt. Answering anything else will
; re-display the monitor select menu.
;
;-----------------------------------------------------------------
; "SCD" Register
;-----------------------------------------------------------------
; The SCD Register is used to configure the machine and include
; the BK lines as well as screen width, colour and sound settings.
;
; The SCD Register bits are:
; 0 - Width. How many columns? (0=32, 1=64)
;     Note: This is physical width not actual characters per line
; 1 - Enable Colour? (0=No, 1=Yes)
; 2 - BK0
; 3 - BK1
; 4 - DAC Enable? (0=No, 1=Yes)

;=================================================================
; CUSTOM code Entry Point
;=================================================================
; Init Memory, reset video mode

CUSTOM	JSR 	INITMEM		; Initialize memory
	LDA 	#0		; 32-column, no colour, no sound
	STA 	SCDREG		; write it to the control register

;=================================================================
; Display NEW Power-on MENU
;=================================================================
; This prints the power-on selection menu

RESTART	LDY 	#0
POMLOOP	LDA	POMENU,Y	; Prompt
	BEQ	POMMENU		; Is it a zero? Yes, we're done.

	JSR	JUMPOUT		; Print It
	INY			; next character
	BNE	POMLOOP		; No, loop for more

;=================================================================
; Get User's ROM SELECTION option
;=================================================================
; This waits for the User to make a selection then jumps to the
; appropriate code to start the machine.

POMMENU
	JSR	JUMPIN		; Accept a key

POM1	CMP	#'1'		; Is it "1" (ROM BANK 0 - THIS BANK!!!)
	BNE	POM2
	RTS			; Return to normal CEGMON startup

POM2	CMP	#'2'		; Is it "2"?
	BNE	POM3
	LDA	#%00000100	; Control Register (ROM BANK 1)
	BNE	POMSELECT

POM3	CMP	#'3'		; Is it "3"?
	BNE	POM4
	LDA	#%00001000	; Control Register (ROM BANK 2)
	BNE	POMSELECT

POM4	CMP	#'4'		; Is it "4"?
	BNE	RESTART		; No, start over
	LDA	#%00001100	; Control Register (ROM BANK 3)

;=================================================================
; DO IT!
;=================================================================
; Copy code to zero page
POMSELECT
	
	LDX	#0
JCLOOP	LDY	JUMPCODE,X	; Read byte from JUMPCODE table
	STY	INBUF,X		; Store it in the keyboard buffer
	INX
	CPX	#8		; 8 bytes to copy
	BNE 	JCLOOP

	STA	INBUF+1		; Store the selection after the LDA opcode
	JMP	INBUF		; JUMP to the code - We never return
	BRK

;=================================================================
; ROM Switching Code for Zero Page
;=================================================================

JUMPCODE !BYTE $A2,$00		; LDA #$xx  - Load the selection (User choice replaces xx)
	 !BYTE $8D,$00,$D8	; STA $D800 - Store it in the control register
	 !BYTE $6C,$FC,$FF	; JMP ($FFFC) - Cold Reset

;=================================================================
; Power-On Menu Text
;=================================================================

POMENU	!BYTE $1A				; Clear Screen
	!TEXT "MONITOR SELECT"
	!BYTE $0D,$0A,$0A			;<CR><LF><LF>
	!TEXT "1) CEGMON-MS"
	!BYTE $0D,$0A				;<CR><LF>
	!TEXT "2) CEGMON"
	!BYTE $0D,$0A				;<CR><LF>
	!TEXT "3) SYN600"
	!BYTE $0D,$0A				;<CR><LF>
	!TEXT "4) SJGMON"
	!BYTE $0D,$0A,$0A			;<CR><LF><LF>
	!TEXT "(1-4)? "
	!BYTE $00				; Marker for END of menu
