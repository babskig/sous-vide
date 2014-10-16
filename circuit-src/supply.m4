.PS
include(pgf.m4)
cct_init(SIdefaults) # initialise and use metric unit
linethick_(.5) # line thickness
define(`dimen_', 10) # component size
elen = dimen_*3/2
Origin: Here
  # go up and draw the source and the in label
  source(up_ elen, AC); llabel(,220V,)
  line right_ elen/2
T1:  transformer(down_ elen,,,W,) with .P1 at Here; 
  line from T1.P2 to Origin
  blen = elen*1/2
W: T1.TS+(elen/2,0)
N: W+(blen,blen)
S: W+(blen,-blen)
E: S+(blen,blen)
  diode(from W to N); dot
  diode(from S to E); dot
  diode(from W to S); dot
  diode(from N to E)
  dot(at W)
  line from N to T1.S1
  line from S to T1.S2
  line from W to W+(0,-elen*3)
  line right_ elen
  capacitor(down_ elen*2 from E,C); rlabel(,2200 \mu,); dot
GroundOrigin: Here
  capacitor(down_ elen,C); rlabel(,2200 \mu,)
  line right_ elen/2
Neg:  nterm("7909",1,1,1,,,N) with .W1 at Here
  line right_ elen/2 from (GroundOrigin,E)
Pos:  nterm("7809",1,,1,1,,N) with .W1 at Here
  line from Pos.S1 to Neg.N1
  dot(at (Pos.S1,GroundOrigin))

  dot(at Neg.W1)
  line down_ elen*2/3
  diode(from Here to (Neg.E1,Here))
  line up_ elen*2/3; dot

  dot(at Pos.E1)
  line up_ elen*2/3
  diode(from Here to (Pos.W1,Here))
  line down_ elen*2/3; dot

  capacitor(down_ elen*2 from Pos.E1,C); llabel(,10 \mu,); dot
  capacitor(down_ elen,C); llabel(,10 \mu,); dot

  line right_ elen from Pos.E1; dot
  capacitor(down_ elen*2,); llabel(,0.1 \mu,); dot
  capacitor(down_ elen,); llabel(,0.1 \mu,); dot

  dot(at Neg.E1+(2*elen,0))
  diode(up_ elen); dot
  diode(up_ elen*2); dot
  {line left_ elen}
  line right_ elen/2; dot
  line down_ elen*2/3; dot
  {capacitor(down_ elen*4/3,C); llabel(,220 \mu,); dot}
Pos5:  nterm("7805",1,,1,1,,N) with .W1 at Here
  diode(up elen*2/3 from Pos5.E1); dot
  capacitor(down_ elen*4/3 from Pos5.E1,C); llabel(,10 \mu
,); dot
  line from Pos5.S1 to (Pos5.S1,GroundOrigin);dot
  dot(at Pos5.E1)
  line right_ elen; dot
  { capacitor(down_ elen*4/3,); llabel(,0.1 \mu
,); dot }
  line right_ elen; dot
  dot(at (Here,GroundOrigin))
  diode(up_ elen*4/3)
  line right_ elen; dot; llabel(,+5V,)
GroundEnd: (Here,GroundOrigin)

  line from (Pos5.W1,E) to (GroundEnd,E); dot; llabel(,+9V,)
  line from Neg.E1 to (GroundEnd,Neg.E1); dot; llabel(,-9V,)
  line from GroundOrigin to (GroundEnd,GroundOrigin); dot; llabel(,\hbox{GND},)

  # draw the out label
#  dot; "out" above
#  { # save the current position
#    # go down and draw the capacitor
#    capacitor(down_ to (Here,Origin)); rlabel(,C_{load},)
#    # draw the 0 label
#    dot; "0" below
#  }
#  # draw a forward horizontal line
#  line right_ elen
  # go down and draw the resistor
#  resistor(down_ Here.y-Origin.y,,E); llabel(,R_{load},)
.PE
