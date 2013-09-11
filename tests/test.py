from __future__ import print_function
import os
import sys
import glob

def main():
    test_path = os.path.dirname(os.path.realpath(__file__))
    parser_path = test_path + "/../main.native"
    files = glob.glob(test_path + "/*.jlite")
    for f in files:
        print
        os.system(parser_path + " " + f)

if __name__ == "__main__":
    main()
