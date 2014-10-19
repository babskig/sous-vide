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
  line from W to W+(0,-elen*2)
  line right_ elen
  capacitor(down_ elen from E,C); llabel(,2200 \mu,); dot
GroundOrigin: Here
  capacitor(down_ elen,C); llabel(,2200 \mu,)
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

  capacitor(down_ elen from Pos.E1,C); llabel(,10 \mu,); dot
  capacitor(down_ elen,C); llabel(,10 \mu,); dot

  line right_ elen from Pos.E1; dot
  capacitor(down_ elen,); llabel(,0.2 \mu,); dot
  capacitor(down_ elen,); llabel(,0.2 \mu,); dot

  dot(at Neg.E1+(2*elen,0))
  diode(up_ elen); dot
  diode(up_ elen); dot
  dot(at Here+(elen/2,0))
GroundEnd: (Here,GroundOrigin)

  line from Pos.E1 to (GroundEnd,Pos.E1); dot; rlabel(,+9V,)
  line from Neg.E1 to (GroundEnd,Neg.E1); dot; rlabel(,-9V,)
  line from GroundOrigin to (GroundEnd,GroundOrigin); dot; rlabel(,\hbox{GND},)

SecondOrigin:
  source(up_ elen from Origin+(0,-elen*4), AC); llabel(,220V,)
  line right_ elen/2
T2:  transformer(down_ elen,,,W,) with .P1 at Here; 
  line left_ elen/2 from T2.P2
W2: T2.TS+(elen/2,0)
N2: W2+(blen,blen)
S2: W2+(blen,-blen)
E2: S2+(blen,blen)
  diode(from W2 to N2); dot
  diode(from S2 to E2); dot
  diode(from W2 to S2); dot
  diode(from N2 to E2)
  dot(at W2)
  line from N2 to T2.S1
  line from S2 to T2.S2
  line from W2 to W2+(0,-elen)
Ground5: Here
  capacitor(down_ elen from E2,C); llabel(,2200 \mu,); dot

Pos5:  nterm("7805",1,,1,1,,N) with .W1 at E2
  capacitor(down_ elen from Pos5.E1,C); llabel(,10 \mu,); dot
  line from Pos5.S1 to (Pos5.S1,Ground5);dot
  dot(at Pos5.E1)
  {
    line up_ elen*2/3
    diode(from Here to (Pos5.W1,Here))
    line from Here to Pos5.W1
  }
  dot(at Here+(elen,0))
  capacitor(down_ elen,); llabel(,0.2 \mu,);dot
  dot(at Here+(elen,0))
  diode(up_ elen)
  dot
  dot(at Here+(elen/2,0))
GroundEnd5: Here
  line from Pos5.E1 to GroundEnd5; rlabel(,+5V,)

  dot(at (GroundEnd5,Ground5))

  line from Ground5 to Here; rlabel(,\hbox{GND},); dot

.PE
