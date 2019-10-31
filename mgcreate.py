# mgcreate.py
# From Kinser 2018
# These functions create images used in several different applications

import numpy as np
from scipy.signal import cspline2d
import scipy.misc as sm
from PIL import Image, ImageDraw

# create a 512x512 image with several blocks of increasing fuzziness
def FuzzBlocks():
    data = np.zeros((512,512))
    k = 0.0
    for v in range( 7 ):
        for h in range( 7 ):
            # create the image
            temp = np.zeros((64,64))
            temp[16:48,16:48] = 1
            if k > 0:
                temp = cspline2d( temp, k )
            # place in large array
            vv = (v+1)*64-32
            hh = (h+1)*64-32
            data[vv:vv+64,hh:hh+64] = temp + 0
            k += 1
    return data

# create an square annulus
def SquareAnnulus( D=512, L1=64, L2=48 ):
    """Frame, box radii
    Creates an image array with a square annulus
    D = frame size:  answ is D x D
    L1 = half width (height) of outer annulus box
    L2 = half width (height) of inner annulus box
    returns: array"""
    answ = np.zeros( (D,D) )
    k1 = int(D/2 - L1)
    k2 = int(D/2 + L1)
    answ[k1:k2,k1:k2] = 1 # outer
    k1 = int(D/2 - L2)
    k2 = int(D/2 + L2)
    answ[k1:k2,k1:k2] = 0 # inner
    return answ

# soft square annulus
def SoftSquareAnnulus( D=512, L1=64, L2=48, sm=25 ):
    """Frame, box radii, smooth
    Creates an image array with a smoothed square annulus
    D = frame size:  answ is D x D
    L1 = half width (height) of outer annulus box
    L2 = half width (height) of inner annulus box
    sm = smoothing factor
    returns: array"""
    answ = SquareAnnulus( D, L1, L2 )
    answ = cspline2d( answ, sm )
    return answ

# create a checkerboard image
def Checkerboard( D=512, ndv=16 ):
    """Frame, # of divisions
    Creates a checkerboard image array
    D = frame size
    ndv = number of divisions
    returns: array """
    answ = np.zeros( (D,D) )
    step = int( D/ndv )
    k = 0
    for i in range( 0, D, step ):
        for j in range( 0, D, step ):
            if k%2==0:
                answ[i:i+step,j:j+step] = 1
            k += 1
        k +=1
    return answ

# create an image with a right triangle
def Triangle( Dvh, p1, p2, p3 ):
    """Frame, points
    Dvh: a tuple that is the frame size
    p1: a tuple that has the point to the left/right of right corner
    p2: a tuple that is the point that is vertical to the right corner
    p3: a tuple that is the location of the right corner
    returns array """
    im = Image.new('L', Dvh)
    draw = ImageDraw.Draw(im)
    pp1 = p1[::-1]; pp2 = p2[::-1]; pp3 = p3[::-1]
    points = (pp1,pp2,pp3)
    draw.polygon( points,fill=1 )
    answ = sm.fromimage( im )
    return answ

# create an image that is a homeplate
def Homeplate( ):
    """
    Creates a homeplate image without adjustable parameters"""
    answ = np.zeros( (512,512) )
    answ[128:384,128:384] = 1
    t1 = Triangle( (512,512),(310,128),(384,128),(384,256))
    t2 = Triangle( (512,512),(384,256),(384,383),(310,383))
    answ = answ - t1 - t2
    return answ

def Circle( size, loc,rad):
    """frame, center, radius
    size is (v,h) of size of array
    loc is (v,h) of circle location
    rad is integer of radius
    returns array with a solid circle"""
    b1,b2 = np.indices( size )
    b1,b2 = b1-loc[0], b2-loc[1]
    mask = b1*b1 + b2*b2
    mask = ( mask <= rad*rad ).astype(int)
    return mask

def Ellipse( size, loc,a,b):
    """frame, center, a,b
    size is (v,h) of size of array
    loc is (v,h) of circle location
    (x/a)^2 + (y/b)^2 = 1
    returns array with a solid ellipse"""
    b1,b2 = np.indices( size )
    b1,b2 = b1-loc[0], b2-loc[1]
    mask = (b1/a)**2 + (b2/b)**2
    mask = ( mask <= 1 ).astype(int)
    return mask

def Ring( VH, vh, rad ):
    a = Circle( VH, vh, rad )
    b = Circle( VH, vh, rad-2 )
    answ = a - b
    return answ

def RandomCircles( VH, N, rad1, rad2 ):
    mxrad = max( (rad1,rad2) ) # maximum
    answ = np.zeros( VH, int )
    vrange = VH[0] - 2*mxrad
    hrange = VH[1] - 2*mxrad
    for i in range( N ):
        v = mxrad + np.random.rand()*vrange
        h = mxrad + np.random.rand()*hrange
        rad = np.random.rand()*(rad2-rad1)+rad1
        temp = Circle( VH, (v,h), rad )
        answ = answ | temp
    return answ

def RandomRings( VH, N, rad1, rad2 ):
    mxrad = max( (rad1,rad2) ) # maximum
    answ = np.zeros( VH, int )
    vrange = VH[0] - 2*mxrad
    hrange = VH[1] - 2*mxrad
    for i in range( N ):
        v = mxrad + np.random.rand()*vrange
        h = mxrad + np.random.rand()*hrange
        rad = np.random.rand()*(rad2-rad1)+rad1
        temp1 = Circle( VH, (v,h), rad )
        temp2 = Circle( VH, (v,h), rad-2 )
        answ = answ | (temp1-temp2)
    return answ

def Wedge( vh, t1, t2 ):
    """in degrees"""
    ans = np.zeros( vh )
    ndx = np.indices( vh ).astype(float)
    ndx[0] = ndx[0] - vh[0]/2
    ndx[1] = ndx[1] - vh[1]/2
    # watch out for divide by zero
    mask = ndx[0] == 0
    ndx[0] = (1-mask)*ndx[0] + mask*1e-10
    # compute the angles
    ans = np.arctan( ndx[1] / ndx[0] )
    # mask off the angle
    ans = ans + np.pi/2    # scales from 0 to pi
    mask = ans >= t1/180.* np.pi
    mask2 = ans < t2/180.* np.pi
    mask = np.logical_and( mask, mask2).astype(int)
    V,H = vh
    mask[V/2,H/2] = 0 # zap center which does not appear in all filters
    return mask

def Plop( data, VH, back=0):
    # vmax, hmax are size of frame
    ans = np.zeros( VH, float ) + back
    vmax, hmax = VH
    # plops the data in the center
    # get center of blob
    V,H = data.shape
    vctr, hctr = V//2, H//2
    vactr, hactr = vmax//2, hmax//2
    # compute the limits for the answ
    valo = vactr - vctr
    if valo<0: valo = 0
    vahi = vactr + vctr
    if vahi>=vmax: vahi = vmax
    halo = hactr - hctr
    if halo<0: halo = 0
    hahi = hactr + hctr
    if hahi>=hmax: hahi = hmax
    # compute limits of incoming
    vblo = vctr - vactr
    if vblo<=0: vblo = 0
    vbhi = vctr + vactr
    if vbhi>=V: vbhi= V
    hblo = hctr - hactr
    if hblo<=0: hblo = 0
    hbhi = hctr + hactr
    if hbhi>=H: hbhi = H
    #print vctr,hctr
    #print valo,vahi, halo,hahi,vblo,vbhi, hblo,hbhi
    if vahi-valo != vbhi-vblo:
        vbhi = vblo+vahi-valo
    if hahi-halo != hbhi-hblo:
        hbhi = hblo+hahi-halo
    ans[valo:vahi, halo:hahi] = data[vblo:vbhi, hblo:hbhi] + 0
    return ans

def KaiserMask( shape, center, r1, r2 ):
    # returns a mask that you can use over and over again.
    di, dj = center	# location of the center of the window
    v,h = shape
    theta = 2. * np.pi
    Iot = 1.0 + theta/4. + 2.*theta/64. + 3.*theta/2304.
    # compute radii
    vindex = np.multiply.outer( np.arange(v), np.ones(h) )
    hindex = np.multiply.outer( np.ones(v), np.arange(h) )
    a = (di-vindex).astype(float)
    b = dj-hindex
    r = np.sqrt( a*a + b*b)
    del a,b
    # inside r1, and outside r2 are easy
    mask = np.zeros( shape, float )
    mask = ( r<r1 ).astype(int)
    # work on the ring between
    b = np.logical_and( (r<r2), (r>r1) )
    m = (r-r1)/(r2-r1)
    m = m*b
    a = theta * np.sqrt( 1.-m*m)
    a = 1.0 + a/4.0 + 2.0*a/64.0 + 3.0*a/2304.0
    a = a / Iot
    a = a * ( r< r2)
    a = a * (r>=r1).astype(int)
    mask = mask + a
    return mask

def Rect2Polar( data ):
    V,H = data.shape
    answer= np.zeros((V,H,2))
    answer[:,:,0] = np.hypot( data.real, data.imag) #r
    answer[:,:,1] = np.arctan2( data.imag, data.real ) #theta
    return answer

def Polar2Rect( data ):
    V,H,N = data.shape
    answer = np.zeros((V,H),complex)
    answer.real = data[:,:,0] * np.cos( data[:,:,1] )
    answer.imag = data[:,:,0] * np.sin( data[:,:,1] )
    return answer

def Zernike( VH, rad, m,n ):
	rp = np.zeros( VH )
	horz = np.ones(VH[1])
	rr = (np.arange(rad)/float(rad))
	if m==0 and n==0: r = 1
	if abs(m)==1 and n==1:
		r = rr
	if m==0 and n==2:
		r = 2*(rr**2) -1
	if abs(m)==2 and n==2:
		r = rr**2
	if abs(m)==1 and n==3:
		r = 3*rr**3 - 2*rr
	if abs(m)==3 and n==3:
		r = rr**3
	if m==0 and n==4:
		r = 6 *rr**4 - 6*rr**2 +1
	if abs(m)==2 and n==4:
		r = 4*rr**4 - 3*rr**2
	if abs(m)==4 and n==4:
		r = rr**4
	if abs(m)==1 and n==5:
		r = 10 *rr**5 - 12* rr**3 + 3*rr
	if abs(m)==3 and n==5:
		r = 5 * rr**5 - 4 * rr**3
	if abs(m)==5 and n==5:
		r = rr**5
	if abs(m)==0 and n==6:
		r = 20*rr**6 - 30*rr**4 + 12*rr**2 - 1
	if abs(m)==2 and n==6:
		r = 15 * rr**6 - 20*rr**4 + 6*rr**2
	if abs(m)==4 and n==6:
		r = 6 *rr**6 - 5*  rr**4
	if abs(m)==6 and n==6:
		r = rr**6
	rp[:rad] = np.outer(r,horz)
	if m <0:
		rp *= np.sin( m * np.arange(VH[1]) /float(VH[1]) * 2 *np.pi )
	else:
		rp *= np.cos( m * np.arange(VH[1]) /float(VH[1]) * 2 *np.pi )
	ctr = int(VH[0]/2),int(VH[1]/2)
	answ = rpolar.IRPolar( rp, ctr )
	return answ
