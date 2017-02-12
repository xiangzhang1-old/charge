# Usage
charge [-tex/-normal] [-vaspkit/-splitdos] [-spin/-nonspin]. Default is -normal -splitdos -spin.
-tex produces .tex tabular output. -vaspkit uses vaspkit to get integrated pdos, while -splitdos uses vtstscripts/split_dos and Python to integrate it.

# Features
occupancy of s,px,dx2-y2... orbitals (requires: vaspkit || split_dos & numpy ; lm-DOSCAR and POSCAR)
Bader/RWIGS charge (requires: bader ; CHGCAR )
greps energy
supports non-spin-polarized (-nonspin) calculation, supports TeX tabular+math output

# History
Written Xiang Zhang 02/Aug/2015
Modified Xiang Zhang 17/Aug/2015: Reformatted options, added splitdos approach due to vaspkit bugs.
Modified Xiang Zhang 05/23/2016: Reformatted options with module-based charge paradigm.

