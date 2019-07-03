#only works on polygons with 3 or more sides
# see http://www.mathwords.com/a/area_regular_polygon.htm for reference
#need to find actual derviation somewhere else
poly_prop = function(nsides){

  rads = (360/nsides)*(pi/180)
  ultima = (nsides* sin(rads) )/8
  return(ultima)

}
