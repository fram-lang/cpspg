  $ cat <<EOF | ./Locations.exe
  > startpos
  >  startpos
  >   startpos
  > endpos
  >  endpos
  >   endpos
  > loc
  >  loc
  >   loc
  > EOF
  1:1
  2:2
  3:3
  4:7
  5:8
  6:9
  7:1-7:4
  8:2-8:5
  9:3-9:6
