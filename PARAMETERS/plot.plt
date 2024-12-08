set terminal postscript eps enhanced color font "Times-Roman,20"
set output 'deformations.eps'
set title  " {/= 32 Deformations}"
set yrange [   -0.4: 0.4]
#set xrange [   5.0:  30.0]
set xlabel "{/= 32 A}"
set ylabel "{/= 32 {{/Symbol b}} }"
plot "betapar.dat" u 2:5:6 w e ps 1 lw 1 lt rgb "black" t "SLO", \
"betapar.dat" u 2:7:8 w e ps 1 lw 1 lt rgb "blue" t "SMLO", \
"betapar.dat" u 2:9 w p  ps 1  lt rgb "green" t "deflib", \
"betapar.dat" u 2:10 w p  ps 1  lt rgb "red" t "prittichenko", \
"betapar.dat" u 2:11 w p  ps 1  lt rgb "olive" t "yushkov", \
"betapar.dat" u 2:12:13 w e  ps 1  lt rgb "yellow" t "ramman", \
"betapar.dat" u 2:14:15 w e  ps 1  lt rgb "grey" t "SLO approx", \
"betapar.dat" u 2:16:17 w e  ps 1  lt rgb "brown" t "SMLO approx"

set output 'deformationsAbs.eps'
set title  " {/= 32 Absolute values of deformations}"
set yrange [   0: 0.6]
#set xrange [   5.0:  30.0]
set xlabel "{/= 32 A}"
set ylabel "{/= 32 {|{/Symbol b}|} }"
plot "betapar.dat" u 2:(sqrt($5**2)):6 w e ps 1 lw 2 lt rgb "black" t "SLO", \
"betapar.dat" u 2:(sqrt($7**2)):8 w e ps 1 lw 2 lt rgb "blue" t "SMLO", \
"betapar.dat" u 2:(sqrt($9**2)) w p  ps 1  lt rgb "green" t "deflib", \
"betapar.dat" u 2:(sqrt($10**2)) w p  ps 1  lt rgb "red" t "prittichenko", \
"betapar.dat" u 2:(sqrt($11**2)) w p  ps 1  lt rgb "olive" t "yushkov", \
"betapar.dat" u 2:12:13 w e  ps 1  lt rgb "yellow" t "ramman", \
"betapar.dat" u 2:(sqrt($14**2)):15 w e  ps 1  lt rgb "grey" t "SLO approx", \
"betapar.dat" u 2:(sqrt($16**2)):17 w e  ps 1  lt rgb "brown" t "SMLO approx"

set output 'deformationsAbs_first.eps'
set title  " {/= 32 Absolute values of deformations}"
set yrange [   0: 0.6]
#set xrange [   5.0:  30.0]
set xlabel "{/= 32 A}"
set ylabel "{/= 32 {|{/Symbol b}|} }"
plot "betapar_first.dat" u 2:(sqrt($5**2)):6 w e ps 1 lw 2 lt rgb "black" t "SLO", \
"betapar_first.dat" u 2:(sqrt($7**2)):8 w e ps 1 lw 2 lt rgb "blue" t "SMLO", \
"betapar_first.dat" u 2:(sqrt($9**2)) w p  ps 1  lt rgb "green" t "deflib", \
"betapar_first.dat" u 2:(sqrt($10**2)) w p  ps 1  lt rgb "red" t "prittichenko", \
"betapar_first.dat" u 2:(sqrt($11**2)) w p  ps 1  lt rgb "olive" t "yushkov", \
"betapar_first.dat" u 2:12:13 w e  ps 1  lt rgb "yellow" t "ramman", \
"betapar_first.dat" u 2:(sqrt($14**2)):15 w e  ps 1  lt rgb "grey" t "SLO approx", \
"betapar_first.dat" u 2:(sqrt($16**2)):17 w e  ps 1  lt rgb "brown" t "SMLO approx"

set output 'ErSLOcomp.eps'
set title  " {/= 32 GDR energies SLO}"
set yrange [   5.0: 30.0]
#set xrange [   5.0:  30.0]
set xlabel "{/= 32 A}"
set ylabel "{/= 32 {E_{r}} }"
plot "gdrSLOapartest.dat" u 2:5:6 w e ps 1 lw 1 lt rgb "black" t "exp", \
"gdrSLOapartest.dat" u 2:7:8 w e ps 1 lw 1 lt rgb "red" t "exp", \
"gdrSLOapartest.dat" u 2:9 w p  ps 1  lt rgb "blue" t "sys", \
"gdrSLOapartest.dat" u 2:11 w p  ps 1  lt rgb "green" t "sys1"

set output 'ErSMLOcomp.eps'
set title  " {/= 32 GDR energies SMLO}"
set yrange [   5.0: 30.0]
#set xrange [   5.0:  30.0]
set xlabel "{/= 32 A}"
set ylabel "{/= 32 {E_{r}} }"
plot "gdrSMLOapartest.dat" u 2:5:6 w e ps 1 lw 1 lt rgb "black" t "exp", \
"gdrSMLOapartest.dat" u 2:7:8 w e ps 1 lw 1 lt rgb "red" t "exp", \
"gdrSMLOapartest.dat" u 2:9 w p  ps 1  lt rgb "blue" t "sys", \
"gdrSMLOapartest.dat" u 2:11 w p  ps 1  lt rgb "green" t "sys1"


set key at screen 0.85, 0.85
set output 'deformations.eps'
set title  " {/= 32 Deformations}"
set yrange [   -0.4: 0.4]
#set xrange [   5.0:  30.0]
set xlabel "{/= 32 A}"
set ylabel "{/= 32 {{/Symbol b}} }"
plot "betapar.dat" u 2:5:6 w e ps 1 lw 1 lt rgb "black" t "SLO", \
"betapar.dat" u 2:7:8 w e ps 1 lw 1 lt rgb "blue" t "SMLO", \
"betapar.dat" u 2:9 w p  ps 1  lt rgb "green" t "deflib", \
"betapar.dat" u 2:10 w p  ps 1  lt rgb "red" t "prittichenko", \
"betapar.dat" u 2:11 w p  ps 1  lt rgb "olive" t "yushkov", \
"betapar.dat" u 2:12:13 w e  ps 1  lt rgb "yellow" t "ramman", \
"betapar.dat" u 2:14:15 w e  ps 1  lt rgb "grey" t "SLO approx", \
"betapar.dat" u 2:16:17 w e  ps 1  lt rgb "brown" t "SMLO approx"

set key at screen 0.85, 0.85
set output 'deformations_first.eps'
set title  " {/= 32 Deformations}"
set yrange [   -0.4: 0.4]
#set xrange [   5.0:  30.0]
set xlabel "{/= 32 A}"
set ylabel "{/= 32 {{/Symbol b}} }"
plot "betapar_first.dat" u 2:5:6 w e ps 1 lw 1 lt rgb "black" t "SLO", \
"betapar_first.dat" u 2:7:8 w e ps 1 lw 1 lt rgb "blue" t "SMLO", \
"betapar_first.dat" u 2:9 w p  ps 1  lt rgb "green" t "deflib", \
"betapar_first.dat" u 2:10 w p  ps 1  lt rgb "red" t "prittichenko", \
"betapar_first.dat" u 2:11 w p  ps 1  lt rgb "olive" t "yushkov", \
"betapar_first.dat" u 2:12:13 w e  ps 1  lt rgb "yellow" t "ramman", \
"betapar_first.dat" u 2:14:15 w p  ps 1  lt rgb "grey" t "SLO approx", \
"betapar_first.dat" u 2:16:17 w p  ps 1  lt rgb "brown" t "SMLO approx"


quit
