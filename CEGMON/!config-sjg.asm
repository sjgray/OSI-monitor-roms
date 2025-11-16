;=================================================================
; CEGMON CUSTOMIZABLE FILE
;=================================================================
; This is a CEGMON configuration file. It sets specific VARIABLES
; to create custom binaries for various machines and ROM space.

;=================================================================
; CONFIGURE MACHINE AND DISPLAY TYPE
;=================================================================
; MACHINE  NAME      NMI   IRQ	ESC  
; 0        C1P/SBII  $130  $1C0	YES
; 1        UK101     $130  $1C0 NO
; 2        C2/C4     $130  $1C0 YES
;-----------------------------------------------------------------
; DISPLAY  DESCRIPTION  WIDTH  SIZE  START  COLS  ROWS NOTES
; 0        Std  24x24   32     0     133    24    24   C1/SB 
; 1	   Std  48x14   64     0     140    48    14   C1-II/600 RevD/UK101
; 2        Std  64x28   64     1     128    64    28   C2/C4
; 3        Tall 24x26   32     0     69     24    26   C1/SB
; 4        Full 32x28   32     0     64     32    28   Modified C1/SB
;-----------------------------------------------------------------   
; Edit the following for your desired features/options:

MACHINE = 0	; 0 to 2      - Determines Machine hardware config
DISPLAY = 0	; 0 to 4      - Determines Video Display Parameters

OPTEMACS= 0	; 0=No, 1=Yes - Enable EMACS-like Editing keys
OPTBANNR= 5	; 0 to 5      - Banner#
OPTINIT = 1	; Custom Init Code? 0=No, 1=Yes
OPTDISK = 1	; What goes in Disk Bootstrap area?
		; 0=Nothing
		; 1=Disk Bootstrap	(standard CEGMON bootstrap)
		; 2=Rev D Support Code 	(SJG)
		; 3=Monitor ROM Menu	(SJG)
		; 4=BASIC ROM Menu	(SJG)

OPTXROM = 2	; Put additional code in External ROM @ $8000-$9FFF
		; Do not set both OPTDISK and OPTXROM at the same time!
		; 0=No
		; 1=Reserved
		; 2=Rev D Support (extended)

OPTXJMP = 1	; Use a Jump table at the start of XROM? 0=No, 1=Yes

OPTWIDTH= 1	; Patch screen width calculations
		; 0=No  - Hard coded per DISPLAY setting
		; 1=Yes - Use PWIDTH memory location in RAM (requires custom routine to initialize)

OPTKEYS = 1	; 0=No, 1=Yes - Patch for Custom Key Handler
OPT630  = 1	; 0=No, 1=Yes - Colour
OPTRGB  = 4	; 0 to 15     - Default RGB value for Colour

;=================================================================
; SET OUTPUT FILE
;=================================================================
; This line sets the binary output filename. You can customize this
; for your specific configuration file.

!TO "SJGMON16.bin",plain

;=================================================================
; INCLUDE MAIN CODE
;=================================================================
; This includes the main CEGMON code. Do not edit or remove this line!

!SOURCE "cegmon-main.asm"

