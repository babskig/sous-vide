.PS                            # Pic input begins with .PS
include(pstricks.m4)
cct_init                       # Read in macro definitions and set defaults

# Usual defs...
qrt=dimen_/4;
hlf=dimen_/2;
dim=dimen_;
mm=1/25.4;
pi=atan2(0,-1);

{line right_ 3*dim; dot(at last line.center); arrow up_ hlf
  right_; "$V_\mathrm{CC}$" above_}
resistor(down_); dot
{Q1: bi_tr(up_,R) with .C at Here }
capacitor(right_); llabel(,C,)
Con1:dot
resistor(up_); rlabel(,R,); dot
move right_ 2*dim
resistor(down_); dot;
{Q2: bi_tr(up_) with .C at Here}
capacitor(left_); rlabel(,C,)
Con2:dot
resistor(up_); llabel(,R,); dot
line from Q1.B to Con2
line from Q2.B to Con1
line from Q1.E to Q2.E
dot(at last line.center); arrow down_ hlf
right_; "$V_\mathrm{EE}$" below_

"\tiny BJTOscillator" at (3*dim, -2.5*dim) rjust_ 
.PE                            # Pic input ends

