#converting images for analysis in R
#importing custom module for analysis
import convert as cvt

#desired directories
#note that each class should be separated into different directories.
#however, for the fucntion to work, multiple directories should be specified.
#thus, an empty folder is utilized for this task
#the empty folder is called "none"

#----------------------------------------------------------
#capsule class

caps = ["caps", "none"]

#name of .txt file
name = 'caps_temp.txt'

#converting images
cvt.BinaryHistTXT(name, caps)


#----------------------------------------------------------
#circle class

circ = ["circle1", "none"]

#name of .txt file
name = 'circ1_temp.txt'

#converting images
cvt.BinaryHistTXT(name, circ)


circ = ["circle2", "none"]

#name of .txt file
name = 'circ2_temp.txt'

#converting images
cvt.BinaryHistTXT(name, circ)

circ = ["circle3", "none"]

#name of .txt file
name = 'circ3_temp.txt'

#converting images
cvt.BinaryHistTXT(name, circ)


#
