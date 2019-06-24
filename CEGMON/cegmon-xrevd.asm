;=================================================================
; CEGMON MONITOR - CUSTOM CODE - SUPPORT FOR 600 REV D
;=================================================================
; This is custom code for external ROM Space.
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
;   appropriate spot depending on if you want the key processed or
;   not, when you are done.
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
; New:
; CTRL-X......Toggle 24/48 (32/64) Column Mode
; CTRL-I......Increment first Colour then fill Colour RAM
; CTRL-Y......Fill Screen with character at first byte
; CTRL-T......Increment first Character then Fill Character RAM

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
; 5 - User-Defined
;
;-----------------------------------------------------------------
; Additional memory usage:
;-----------------------------------------------------------------
;
; PWIDTH = $FD			; Pysical screen width (not sure if this location is safe!!!!!)
; SHADOW = $0234		; Shadow SCD Register (Screen,Colour,DAC)
;
;-----------------------------------------------------------------

;#################################################################
; START OF CUSTOM CODE
;#################################################################
; Start of code. 

;*****************************************************************
; Jump Table
;*****************************************************************
; When OPTXJMP=1 we include a small jump table at the beginning so
; that the MONITOR ROM always has a fixed enty point to the code.
; This helps me in testing because I don't need to burn two EPROMS
; every time there is a change.

!IF OPTXJMP = 1 {
XCUSTOM		JMP CUSTOM
XCUSTKEYS	JMP CUSTKEYS
		BRK
		BRK
}

;*****************************************************************
; Custom Init
;*****************************************************************

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
	BNE CCRANGE		; No, skip ahead
	JMP DOCR		; Yes, do it

CCRANGE CMP #$1F		; Is it in the range we need to check?
	BPL CCDONE		; No, it's higher, so exit

CCSKIP1 CMP #$12		; Is it CTRL-R? (RGB Mode)
	BEQ CTRLR		; Yes, do it

CCSKIP2 CMP #$16		; Is it CTRL-V? (VIDEO Mode - B/W)
	BEQ CTRLV		; Yes, do it

CCSKIP3 CMP #$15		; Is it CTRL-U? (COLOUR CLEAR)
	BEQ CTRLU		; Yes, do it

CCSKIP4 CMP #$17		; Is it CTRL-W? (Wide Video Mode - 24/32 columns)
	BEQ CTRLW		; Yes, do it

CCSKIP5 CMP #$0E		; Is it CTRL-N? (Narrow Video Mode - 48/64 columns)
	BEQ CTRLN		; Yes, do it

CCSKIP6	CMP #$16		; Is it CTRL-X? (Toggle Width)
	BEQ CTRLX		; Yes, Do it

CCSKIP7	CMP #$09		; Is it CTRL-I? (Increment Colour)
	BEQ CTRLI		; Yes, Do it

CCSKIP8 CMP #$19		; Is it CTRL-Y? (Fill screen)
	BEQ CTRLY

CCSKIP9 CMP #$14		; Is it CTRL-T? (Increment then Fill screen)
	BEQ CTRLT

;*****************************************************************
; Exit and pass KEY on to CEGMON
;*****************************************************************

CCDONE	JMP STORKEY		; We didnt find our custom key. Pass it on. We're done!


;*****************************************************************
; CTRL-X - Toggle 24/48 Column Mode
;*****************************************************************

CTRLX	LDA SHADOW
	AND #%00000001		; mask off all bits but 0
	BEQ CTRLN		; If bit0 is 0 then do Narrow

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

CTRLU	JSR CLEARS		; Clear Colour with value from first byte of Colour RAM
	JMP PULLYXA		; We're done

;*****************************************************************
; CTRL-I - Increment then Clear Colour
;*****************************************************************

CTRLI	JSR CLEARI		; Increment Colour then Clear Colour RAM
	JMP PULLYXA		; We're done

;*****************************************************************
; CTRL-Y - Fill Screen
;*****************************************************************

CTRLY	JSR FILLS		; Clear Screen with value from first byte of Screen RAM
	JMP PULLYXA		; We're done

;*****************************************************************
; CTRL-T - Increment then Fill Screen
;*****************************************************************

CTRLT	JSR FILLI		; Increment Character then Fill Screen RAM
	JMP PULLYXA		; We're done

;*****************************************************************
; Clear Colour Memory
;*****************************************************************
; CLEARS - Clears Colour Memory to Same as top left
; CLEARC - Clears Colour Memory to colour in A register
; CLEARI - Increment top left then Clear Colour Memory

CLEARI	INC COLOUR		; Increment the colour
CLEARS  LDA COLOUR		; Read colour from first colour location
CLEARC	LDX #0			; Entry here with colour in A

CMLOOP	STA COLOUR,X		; set Colour memory page 1
	STA COLOUR+$100,X	; set Colour memory page 2
	STA COLOUR+$200,X	; set Colour memory page 3
	STA COLOUR+$300,X	; set colour memory page 4

!IF SIZE=1 {
	STA COLOUR+$400,X	; set Colour memory page 5 (2K Video systems)
	STA COLOUR+$500,X	; set Colour memory page 6
	STA COLOUR+$600,X	; set Colour memory page 7
	STA COLOUR+$700,X	; set Colour memory page 8
}
	INX			; Next index position
	BNE CMLOOP		; Loop back for more
	RTS			; We're done!

;*****************************************************************
; Clear Screen Memory
;*****************************************************************
; FILLS - Clears Screen Memory to Same as top left
; FILLC - Clears Screen Memory to colour in A register
; FILLI - Increment top left then Clear Screen Memory

FILLI	INC SCREEN		; Increment the colour
FILLS	LDA SCREEN		; Read colour from first colour location
FILLC	LDX #0			; Entry here with colour in A

FCLOOP	STA SCREEN,X		; set Screen memory page 1
	STA SCREEN+$100,X	; set Screen memory page 2
	STA SCREEN+$200,X	; set Screen memory page 3
	STA SCREEN+$300,X	; set Screen memory page 4
!IF SIZE=1 {
	STA SCREEN+$400,X	; set Screen memory page 5 (2K Video systems)
	STA SCREEN+$500,X	; set Screen memory page 6
	STA SCREEN+$600,X	; set Screen memory page 7
	STA SCREEN+$700,X	; set Screen memory page 8
}
	INX			; Next index position
	BNE FCLOOP		; Loop back for more
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