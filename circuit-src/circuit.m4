.PS
include(pgf.m4)
include(libcct.m4)
cct_init(SIdefaults) # initialise and use metric unit
linethick_(.5) # line thickness
define(`dimen_', 10) # component size
elen = dimen_*3/2
Origin: Here
  # go up and draw the source and the in label
  source(up_ elen, AC); llabel(,V_{in},); dot; "in" above
  line(right_ elen*1/2)
  transformer(down_, , W)
  # turn right and draw the diode
  diode(right_ elen); llabel(,D,)
  # draw the out label
  dot; "out" above
  { # save the current position
    # go down and draw the capacitor
    capacitor(down_ to (Here,Origin)); rlabel(,C_{load},)
    # draw the 0 label
    dot; "0" below
  }
  # draw a forward horizontal line
  line right_ elen*1/2
  # go down and draw the resistor
  resistor(down_ Here.y-Origin.y,,E); llabel(,R_{load},)
  # draw a backward horizontal segment
  line to Origin
.PE