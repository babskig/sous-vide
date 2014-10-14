.PS                            # Pic input begins with .PS
include(svg.m4)
cct_init                       # Read in macro definitions and set defaults

elen = 0.75                    # Variables are allowed; default units are inches
Origin: Here                   # Position names are capitalized
   source(up_ elen, AC); llabel(-,220V,+)
   resistor(right_ elen);  rlabel(,R,)
   dot
   {                           # Save the current position and direction
      capacitor(down_ to (Here,Origin))     #(Here,Origin) = (Here.x,Origin.y)
      rlabel(+,v,-); llabel(,C,)
      dot
      }                        # Restore position and direction
   line right_ elen*2/3
   inductor(down_ Here.y-Origin.y); rlabel(,L,); b_current(i)
   line to Origin
.PE                            # Pic input ends

