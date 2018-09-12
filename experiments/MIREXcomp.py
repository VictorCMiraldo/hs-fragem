import os
from music21 import *
import fnmatch

path = "/Users/irisren/Dropbox/MIREX/patternmid"

for root, dirs, files in os.walk(path):
    for CurrentFileName in files:
        address = os.path.join(root, CurrentFileName)
        pit=[]
        
        if fnmatch.fnmatch(CurrentFileName, "sonata04*.mid"):