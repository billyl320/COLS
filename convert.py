import scipy.misc as sm
import mgcreate as mgc
import scipy.ndimage as nd
import numpy as np
import os
import math

#convert image to square centered at center of image
#assuming input has the shape as black
def convert(fname):
    adata, temp = bin_edges(fname)
    #plopping into square centered at center of image with max of original image
    d = max(adata.shape)*2
    pic = mgc.Plop(adata, (d, d), 0)
    #swapping black and white
    new = pic
    #new = 1-new
    # find the center of mass
    v = nd.center_of_mass(new)
    v = (int(v[0]), int(v[1]))
    #finding cetner of new image
    n = (d/2, d/2)
    #shift in horizontal
    hori = v[1]-n[1]
    #shift in vertical
    vert = v[0]-n[0]
    #shift the image
    ultima = nd.shift(new, (-vert, -hori), cval=0)
    return ultima

#finding minimum encompassing circle - needs to be binary (1 and 0)
def enc_circ(pic):
    ultima = pic + 0
    v,h = ultima.shape
    z = np.ones((v,h))+0
    z[v//2,h//2] = 0
    dist = nd.distance_transform_edt(z)
    vals = dist*ultima
    r = vals.max()
    r = math.ceil(r)
    ultima = ultima[(v//2 - r) : r + v//2, h//2 -r : r + h//2]
    return ultima

#performs needed setup for other functions
#also provides binary edges info
#must use first
def bin_edges (fname, thresh=7):
    #read in image as bw
    adata = sm.imread(fname, flatten=True)
    #apply sobel edge detection
    b = nd.sobel(adata+0., axis=0)
    c = nd.sobel(adata+0., axis=1)
    edg = abs(b) + abs(c)
    #save to normalize values
    sm.imsave('dud.jpg', edg)
    s = sm.imread('dud.jpg', flatten=False)
    #collect binary histogram
    hist2, bins2 = np.histogram( ((s<thresh)+0.0).ravel(),2,[0,2])
    #apply thresholding
    edg = edg > thresh
    edg = edg + 0.0
    #apply binary fill holes
    shape = nd.binary_fill_holes(edg) + 0.0
    return shape, hist2


#getting all .pgm's and .png's
def GetPicNames( indir ):
    a = os.listdir( indir )
    pgmnames= []
    for t in a:
        if '.pgm' in t:
            pgmnames.append( indir + '/' + t )
        if '.png' in t:
            pgmnames.append( indir + '/' + t )
        if '.jpg' in t:
            pgmnames.append( indir + '/' + t )
    return pgmnames

#obtaining images
def GetAllImages( dirs ):
    mgs = []
    for d in dirs:
        mgnames = GetPicNames( d )
        for j in mgnames:
            b = convert(j)
            c = enc_circ(b)
            sm.imsave('circ_shape/'+j, c)
            mgs.append( c )
    return mgs

#getting all images as is (for CNN)
#changed slightly from poly convert as these images are color
def GetAllImagesCNN( dirs ):
    mgs = []
    for d in dirs:
        mgnames = GetPicNames( d )
        for j in mgnames:
            a, temp = bin_edges(j)
            sm.imsave("temp.png",1-a)
            temp=sm.imread('temp.png', flatten=False)
            adata = sm.imread("temp.png", flatten=True)
            V, H = adata.shape
            bdata = adata[0:V:10,0:H:10]
            new = (bdata>128) + 0
            new = 1 - new
            mgs.append( new )
    return mgs

#histogram conversion for 256 intensities.
def GSHistogram(dirs):
    imgs = []
    imgs.append(GetAllImages(dirs)) #images obtained from directory
    hist = np.zeros( (len(imgs[0]),2) )
    #get histogram values
    for i in range(0,(len(imgs[0]))):
        temp = imgs[0][i].ravel()
        temp = (temp < 256/2) + 0
        hist[i] = np.array(np.histogram(temp, bins=range(0, 3))[0])
    #return vals
    return hist

#gets binary image histogram
def BinaryHist(dirs):
    imgs = []
    imgs.append(GetAllImages(dirs))
    hist = np.zeros( (len(imgs[0]),2) )
    #get histogram values
    for i in range(0,(len(imgs[0]))):
        hist[i][0] = imgs[0][i].sum()
        hist[i][1] = (imgs[0][i].shape[0]*imgs[0][i].shape[1]) - hist[i][0]
    #return vals
    return hist

#save histogram as .txt where first column is white counts
#and second column is black counts
def GSHistTXT(tname, dirs):
    #obtain histogram
    hist = GSHistogram(dirs)
    #save as txt
    np.savetxt(tname, hist, delimiter=',', header="white,black", comments='')

#save histogram as .txt where first column is white counts
#and second column is black counts
def BinaryHistTXT(tname, dirs):
    #obtain histogram
    hist = BinaryHist(dirs)
    #save as txt
    np.savetxt(tname, hist, delimiter=',', header="white,black", comments='')

#-------------------------------------------------------------------------------
#the following functions only obtain the image histograms as the images are
#input.   No other changes are made other than converting the imags
#to binary ones
#-------------------------------------------------------------------------------

#convert image to square centered at center of image
#assuming input has the shape as black
def convert_old(fname):
    adata = sm.imread(fname, flatten=True)
    pic = adata
    #swapping black and white
    new = (pic>128) + 0
    new = 1 - new
    return new

#obtaining images
def GetAllImages2( dirs ):
    mgs = []
    for d in dirs:
        mgnames = GetPicNames( d )
        for j in mgnames:
            mgs.append( convert_old(j) )
    return mgs

#gets binary image histogram
def BinaryHist2(dirs):
    imgs = []
    imgs.append(GetAllImages2(dirs))
    hist = np.zeros( (len(imgs[0]),2) )
    #get histogram values
    for i in range(0,(len(imgs[0]))):
        hist[i][0] = imgs[0][i].sum()
        hist[i][1] = (imgs[0][i].shape[0]*imgs[0][i].shape[1]) - hist[i][0]
    #return vals
    return hist

#save histogram as .txt where first column is white counts
#and second column is black counts
def BinaryHistTXT_old(tname, dirs):
    #obtain histogram
    hist = BinaryHist2(dirs)
    #save as txt
    np.savetxt(tname, hist, delimiter=',', header="white,black", comments='')


#
