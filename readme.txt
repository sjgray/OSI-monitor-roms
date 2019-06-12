Ohio Scientific Monitor ROMs Project
====================================

This is a repository of OSI Monitor ROMs.
The goal gather known monitor roms, disassemble each back to readable source,
with meaningful symbols, label names, and descriptive comments, and to be
able to re-assemble it with the ACME assembler. Once the ROM is understood
we want to be able to customize each with enhancements or additional features,
for example supporting Superboard II Rev D (C1P Series II) selectable video
modes, colour printing etc.

Current Status:
===============

SYSTEMS:  (S)=Superboard II, (SD)=Superboard II Rev D / C1P Series II (UK)=UK-101, C2=C2P/C4P
FEATURES: (ASM) = Assembles?, (LBL)=Meaningful Labels, (COM)=Commented
          (CUST) = Customizable? (VID)=Selectable Video Options?
STATUS:   (Y)es, (No), (P)artial

MONITOR  SYSTEM ASM LBL COM CUST VID
-------  ------ --- --- --- ---- ---
CEGMON   UK       Y   Y   P  Y    Y
CEGMON   S        Y   Y   P  Y    Y
CEGMON   SD       Y   Y   P  Y    Y
GECMON   C2	  Y   Y   P  Y    N

WEMON    UK       N   P   P  N    N
WEMON    S        N   P   P  N    N
WEMON    SD       N   P   P  N    N
WEMON    C2       N   P   P  N    N


BASIC
=====

CEGMON will not function correctly with US ROMs. You must use the UK-101 ROMs!
UK-101 BASIC ROMs can be found in the /BASIC folder.

Steve