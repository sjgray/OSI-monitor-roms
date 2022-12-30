Ohio Scientific Monitor ROMs Project
====================================

Last update: Dec 29, 2022

This is a repository of OSI Monitor ROMs.
The goal is to gather known monitor roms, disassemble each back to readable source,
with meaningful symbols, label names, and descriptive comments, and to be
able to re-assemble it with the ACME assembler. Once the ROM is understood
we want to be able to customize each with enhancements or additional features,
for example supporting Superboard II Rev D (C1P Series II) selectable video
modes, colour printing etc.

Included Monitors
-----------------

* CEGMON   - In progress
* WEMON    - In progress
* SYN600   - Queued
* NEWMON   - Queued
* BAER MON - Queued
* PCMON2   - Queued
* DABUG    - Queued
* Aardvark - Queued

MONITORS
========

CEGMON - Chkiantz, Elen, and Graves Monitor
------
* /Original Source/ folder was a disassembly found on the web. For reference only!
* /Docs/ contains the manual and installation instructions.
* /Binaries/ contains know versions for various systems.
* /NewBinaries/ contains newly assembled versions from my updated source
* NOTE: CEGMON will not function correctly with US ROMs. You must use the UK-101 ROMs!

WEMON - Watford Electronics Monitor
-----
* Only the Binary and Manual were available.
* /Docs/ contains the Manual and a review
* /Binaries/ contains original binaries for various systems.
* /Disassembly/ folder is my own disassembly of the binary using CBM-Transfer.
  The included ASM-PROJ file will enable you to generate various output formats.
* /Experimental/ is a special "split" version.


SYN600 - Original OSI Monitor for multiple systems
------
* Split Binaries can be assembled as required


NEW MONITOR - By Roger Cuthbert - Supplied with UK-101 machines
-----------
* Binary and Manual


BAER MON By Wolfgang Baer
--------
* Binary and Manual (German)


PCMON2
------
* Binary only - Found in a Superboard I purchased. 
* Appears to be a variation of the SYN600 ROM.


DABUG - Unknown Monitor for C1
-----
* Binary Only


Aardvark - Monitor for C1 and Superboard II. The version for C4 has not been found yet
--------
* Binary for C1
* Binary for Superboard II
* Basic Disassemblies for both


###########
## BASIC ##
###########

* UK-101 BASIC ROMs can be found in the /BASIC folder.
* Needed for certain Monitor ROMs


CURRENT STATUS
==============

SYSTEMS:  (S) =Superboard II
          (SD)=Superboard II Rev D / C1P Series II
          (UK)=UK-101
          (C2)=C2P/C4P

FEATURES: (ASM) = Assembles?, (LBL)=Meaningful Labels, (COM)=Commented
          (CUST) = Customizable? (VID)=Selectable Video Options?

STATUS:   (Y)es, (No), (P)artial

MONITOR  SYSTEM ASM LBL COM CUST VID
-------  ------ --- --- --- ---- ---
CEGMON   UK       Y   Y   P  Y    Y
CEGMON   S        Y   Y   P  Y    Y
CEGMON   SD       Y   Y   P  Y    Y
GECMON   C2	  Y   Y   P  Y    N

WEMON    UK       Y   P   P  N    N
WEMON    S        Y   P   P  N    N
WEMON    SD       Y   P   P  N    N
WEMON    C2       N   P   P  N    N

All other MONITORS have not been investigated yet.
If you have other Monitor ROMs please contact me to have them added!

Steve