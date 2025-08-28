# imports - standard imports
import os

# imports - module imports
import utils
import const

def write(response, dirname):
    parameters = response.headers['Content-Disposition'].decode('utf-8')
    filename   = parameters.split('=')[-1]

    dircache   = const.path['CACHEDIR']
    dirdata    = os.path.join(dircache, dirname)

    utils.makedirs(dirdata, exists_ok = True)

    filepath   = os.path.join(dirdata, filename)

    if not os.path.exists(filepath) and not os.path.isfile(filepath):
        with open(filepath, 'wb') as f:
            buffrr = response.body

            f.write(buffrr)
