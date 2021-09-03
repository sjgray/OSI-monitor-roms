rem assemble various OSI roms from individual parts  (pc-dos batch file)
rem
rem---- C2 Monitor, with ROM BASIC support in lower half, and OS65D support in upper half.

copy /b zeros+c2_key_FD00+c2_65v_FE00+c2_hdm_FF00+zeros+c2_key_FD00+c2_65v_FE00+c2_cwm_FF00 c2_cwmhigh.rom

rem---- C2 Monitor, with OS65D support in lower half, and ROM BASIC support in upper half.

copy /b zeros+c2_key_FD00+c2_65v_FE00+c2_cwm_FF00+zeros+c2_key_FD00+c2_65v_FE00+c2_hdm_FF00 c2_hdmhigh.rom

copy /b c2_hdm_FF00+c2_key_FD00+c2_65v_FE00+c2_cwm_FF00+c1_disk_FC00+c1_key_FD00+c1_65v_FE00+c1_dcwm_FF00 synmon.rom

rem---- C1 Monitor:

copy /b c1_disk_FC00+c2_key_FD00+c2_65v_FE00+c2_hdm_FF00+c1_disk_FC00+c2_key_FD00+c2_65v_FE00+c2_hdm_FF00 c1.rom
    
