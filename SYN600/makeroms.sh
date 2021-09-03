#!/bin/sh

# C2 Monitor, with ROM BASIC support in lower half, and OS65D support
# in upper half.

cat zeros c2_key_FD00 c2_65v_FE00 c2_hdm_FF00 \
    zeros c2_key_FD00 c2_65v_FE00 c2_cwm_FF00 >c2_cwmhigh.rom

# C2 Monitor, with OS65D support in lower half, and ROM BASIC support
# in upper half.

cat zeros c2_key_FD00 c2_65v_FE00 c2_cwm_FF00 \
    zeros c2_key_FD00 c2_65v_FE00 c2_hdm_FF00  >c2_hdmhigh.rom

cat c2_hdm_FF00 c2_key_FD00 c2_65v_FE00 c2_cwm_FF00 \
    c1_disk_FC00 c1_key_FD00 c1_65v_FE00 c1_dcwm_FF00 >synmon.rom

#C1 Monitor:
#
#cat c1_disk_FC00 c2_key_FD00 c2_65v_FE00 c2_hdm_FF00 \
#    c1_disk_FC00 c2_key_FD00 c2_65v_FE00 c2_hdm_FF00 > c1.rom
    
