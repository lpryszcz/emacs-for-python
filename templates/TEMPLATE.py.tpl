#!/bin/env python 
import sys
import os
sys.path.insert(0, os.path.expanduser("~/.pythonlib/"))
import logging as log
from argparse import ArgumentParser

__DESCRIPTION__ = """ 
(>>>POINT<<<)
(jhcepas [at] gmail.com) (>>>DATE<<<)
"""

log.basicConfig(level=log.INFO, \
                    format="%(levelname)s - %(message)s" )

if __name__ == "__main__":
    parser = ArgumentParser(description=__DESCRIPTION__)
    # name or flags - Either a name or a list of option strings, e.g. foo or -f, --foo.
    # action - The basic type of action to be taken when this argument is encountered at the command line. (store, store_const, store_true, store_false, append, append_const, version)
    # nargs - The number of command-line arguments that should be consumed. (N, ? (one or default), * (all 1 or more), + (more than 1) )
    # const - A constant value required by some action and nargs selections. 
    # default - The value produced if the argument is absent from the command line.
    # type - The type to which the command-line argument should be converted.
    # choices - A container of the allowable values for the argument.
    # required - Whether or not the command-line option may be omitted (optionals only).
    # help - A brief description of what the argument does.
    # metavar - A name for the argument in usage messages.
    # dest - The name of the attribute to be added to the object returned by parse_args().

    parser.add_argument("-d", "--delimiter", dest="species_delimiter", 
                        type=str, default = "_",
                        help="""delimiter used to split sequence names.""")
    

    args = parser.parse_args()
    log.info("Everything OK!")
