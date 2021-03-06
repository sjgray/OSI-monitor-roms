;=================================================================
; CEGMON MONITOR - CUSTOM CODE - BASIC ROM SELECTION MENU
;=================================================================
; This is custom code that replaces the OSI DISK BOOTSTRAP (about
; 166 bytes). It is included when OPTDISK=4.
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
; 600D control register. OSI already has jumpers to allow the BASIC
; ROM sockets to be switched to 8K, allowing you to select which of
; the BASIC ROM sockets are active. We could install 4 different
; BASIC versions, or any other firmware that we want.
; On boot a selection menu will appear with 4 options. The USER can
; then choose 1,2,3, or 4 which will set the appropriate BK lines,
; and then continue booting to the regular "C/W/M" prompt. Answering
; anything else will re-display the BASIC select menu.
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

ROM1	CMP	#'1'		; Is it "1"?
	BNE	ROM2
	LDA	#%00000000	; Control Register (ROM BANK 0)
	BEQ	ROMSELECT

ROM2	CMP	#'2'		; Is it "2"?
	BNE	ROM3
	LDA	#%00000100	; Control Register (ROM BANK 1)
	BNE	ROMSELECT

ROM3	CMP	#'3'		; Is it "3"?
	BNE	ROM4
	LDA	#%00001000	; Control Register (ROM BANK 2)
	BNE	ROMSELECT

ROM4	CMP	#'4'		; Is it "4"?
	BNE	RESTART		; No, start over
	LDA	#%00001100	; Control Register (ROM BANK 3)

;=================================================================
; DO IT!
;=================================================================
; Copy code to zero page
ROMSELECT
	STA 	SCDREG		; Activate the socket
	RTS			; Return to CEGMON


;=================================================================
; Power-On Menu Text
;=================================================================

POMENU	!BYTE $1A				; Clear Screen
	!TEXT "SELECT BASIC SOCKET:"
	!BYTE $0D,$0A,$0A			;<CR><LF><LF>
	!TEXT "(1-4)? "
	!BYTE $00				; Marker for END of menu
