# imports - standard imports
import os

def makedirs(path, exists_ok = False):
    try:
        os.makedirs(path)
    except OSError as e:
        if not os.path.isdir(path) or (not exists_ok and e.errno == errno.EEXISTS):
            raise e
