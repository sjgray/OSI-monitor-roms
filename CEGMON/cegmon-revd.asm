;=================================================================
; CEGMON MONITOR - CUSTOM CODE - SUPPORT FOR 600 REV D
;=================================================================
; This is custom code that replaces the OSI DISK BOOTSTRAP (about
; 166 bytes). It is included when OPTDISK=0 and OPTCUST=1.
;
; There are TWO patches:
; INITIALIZATION:
;   "JSR CUSTOM" is called via the RESET routine in place of
;   JSR INITMEM. You should call INITMEM unless you have your own
;   init routine. You need to RTS when done. 
;
; PRINTING:
;   The printing routines are patched to JMP to CUSTKEYS at the
;   point where the CR check is done. You should also do a CR check
;   in your code. When you are done you must JMP back to the
;   appropriate spot depending on if you want the key processed
;   when you are done.
;
; NOTES:
;   This code takes up almost the entire space that the Disk Bootstrap
;   previously used.
;
;-----------------------------------------------------------------
; NEW FEATURES
;-----------------------------------------------------------------
;
; CTRL-W..... Switch to "wide" characters   (24/32)
; CTRL-N......Switch to "narrow" characters (48/64)
; CTRL-V......Composite Video Mode (black and white)
; CTRL-R......RGB Video Mode
; CTRL-U......Clear Colour Memory to whatever colour is at $D400.
;
;-----------------------------------------------------------------
; "SCD" Register
;-----------------------------------------------------------------
;
; The SCD Register bits are:
; 0 - Width. How many columns? (0=32, 1=64)
;     Note: This is physical width not actual characters per line
; 1 - Enable Colour? (0=No, 1=Yes)
; 2 - BK0
; 3 - BK1
; 4 - DAC Enable? (0=No, 1=Yes)
;
;-----------------------------------------------------------------
; Additional memory usage:
;-----------------------------------------------------------------
;
; PWIDTH = $FD			; Pysical screen width (not sure if this location is safe!!!!!)
; SHADOW = $0234		; Shadow SCD Register (Screen,Colour,DAC)
;
;-----------------------------------------------------------------

CUSTOM
	JSR INITMEM		; Initialize low memory
	JSR WIN1		; Set Default Screen,Colour, Window

!IF OPT630=1 {
	LDA #OPTRGB		; Default Colour
	JSR CLEARC		; Clear Colour Mem
}
	RTS


;*****************************************************************
; Custom KEY Handler
;*****************************************************************
; The key checker routine is patched to jump here. The patch
; replaces the test for CR, so we must check here as well, then
; we can check our new custom keys. If the KEY is not handled
; we must jump to STORKEY to finish up the normal KEY handling.

CUSTKEYS
	CMP #$0D		; Is it CR? (CARRIAGE RETURN)
	BNE CCSKIP1		; No, skip ahead
	JMP DOCR		; Yes, do it

CCSKIP1 CMP #$12		; Is it CTRL-R? (RGB Mode)
	BEQ CTRLR		; Yes, do it

CCSKIP2 CMP #$16		; Is it CTRL-V? (VIDEO Mode - B/W)
	BEQ CTRLV		; Yes, do it

CCSKIP3 CMP #$15		; Is it CTRL-U? (COLOUR CLEAR)
	BEQ CTRLU		; Yes, do it

CCSKIP4 CMP #$17		; Is it CTRL-W (Wide Video Mode - 24/32 columns)
	BEQ CTRLW		; Yes, do it

CCSKIP5 CMP #$0E		; Is it CTRL-N (Narrow Video Mode - 48/64 columns)
	BEQ CTRLN		; Yes, do it

;	CMP #$16		; Is it CTRL-X (Toggle Width)
;	BEQ CTRLX		; Yes, Do it

CCDONE	JMP STORKEY		; We didnt find our custom key. Pass it on. We're done!


;*****************************************************************
; CTRL-W - Set 24/32 Column Mode (WIDE Characters)
;*****************************************************************
; Removed due to lack of space
;
;CTRLX	LDA SHADOW
;	AND #%00000001
;	BEQ XWIDE
;	JSR WIN1
;	JMP RUBOUT2
;XWIDE	JSR WIN2
;	JMP RUBOUT2

;*****************************************************************
; CTRL-W - Set 24/32 Column Mode (WIDE Characters)
;*****************************************************************

CTRLW	JSR WIN1		; Select Window#1
	JMP RUBOUT2		; We're done

;*****************************************************************
; CTRL-N - Set 48/64 Column Mode (NARROW Character)
;*****************************************************************

CTRLN	JSR WIN2		; Set the CEGMON Window#2
	JMP RUBOUT2		; We're done

;*****************************************************************
; CTRL-V - Normal Video Mode (Monochrome)
;*****************************************************************

CTRLV	LDA SHADOW
	AND #%11111101		; Clear BIT 1
	JMP SETSCD		; Update real and shadow registers

;*****************************************************************
; CTRL-R - Enable Colour Mode (RGB)
;*****************************************************************

CTRLR	LDA SHADOW
	ORA #%00000010		; Set BIT 1

;*****************************************************************
; SETSCD - Update Shadow Reg and Hardware SCD Register
;*****************************************************************

SETSCD	STA SHADOW		; Update the shadow register
	STA SCDREG		; Write to the real register
	JMP PULLYXA		; We're done


;*****************************************************************
; CTRL-U - Clear Colour
;*****************************************************************

CTRLU	JSR CLEARS		; Clear to same as first bye of colour ram
	JMP PULLYXA		; We're done

;*****************************************************************
; Clear Colour Memory
;*****************************************************************
; CLEARS - Clears Colour Memory to Same as top left
; CLEARC - Clears Colour Memory to colour in A register

CLEARS  LDA $D400		; Read colour from first colour location
CLEARC	LDX #0			; Entry here with colour in A

CMLOOP	STA $D400,X		; set colour memory page 1
	STA $D500,X		; set colour memory page 2
	STA $D600,X		; set colour memory page 3
	STA $D700,X		; set colour memory page 4
	INX			; Next index position
	BNE CMLOOP		; Loop back for more
	RTS			; We're done!


;*****************************************************************
; Set CEGMON Window
;*****************************************************************
; These are the Window definition bytes. Five bytes are copied to
; the GEGMON Windowing parameter table, 1 byte is for the screen
; width, and 1 for the SCD Control Register (ie: Screen width,
; colour, DAC).

WIN1	LDX #0			; Window #1
	!BYTE $2C
WIN2	LDX #7			; Window #2	
SETWIN	LDY #0			; # of bytes to copy
SWLOOP	LDA WINDOWS,X		; Get window parameter
	STA SWIDTH,Y		; Write it to Window Parameter memory
	INX			; Next byte in table
	INY			; Count of bytes left to copy
	CPY #5
	BMI SWLOOP		; Loop back for more

	LDA WINDOWS,X		; Get the screen width (32 or 64)
	STA PWIDTH		; Store it	
	INX
	LDA WINDOWS,X		; Get SCD Mode
	STA SHADOW		; Save to Shadow Register
	STA SCDREG		; Write to real SCD Register

	LDA #0			; 
	STA INBUF		; put it in buffer
;	STA $01F8		; This seems to mimic $0200

	JSR SCNCLR		; Clear the screen
	JSR CURHOME		; Home the cursor
	STX CURDIS
	RTS

;*****************************************************************
; Windows Table
;*****************************************************************
; Format: WW=Window Width-1,TL=TopL,TH=TopH,BL=BaseL,BH=BaseH,
;         SW=Screen Width,MD=SCD Mode
; 
;             WW ,TL ,TH ,BL ,BH ,SW, MD
WINDOWS	!BYTE $17,$85,$D0,$65,$D3,$20,$00	; OFFSET 0 > 24 columns
	!BYTE $2F,$8B,$D0,$4B,$D3,$40,$01	; OFFSET 7 > 48 columns