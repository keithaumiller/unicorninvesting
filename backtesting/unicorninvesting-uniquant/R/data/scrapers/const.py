# imports - standard imports
import os

path             = dict()
path['CACHEDIR'] = os.getenv('UNIQUANT_CACHEDIR', os.path.join(os.path.expanduser('~'), '_uniquant'))
