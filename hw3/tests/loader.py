#!/usr/bin/python3

import sys

def main():
    if len(sys.argv) != 2:
        print('Please provide a single input file')
        sys.exit(1)

    filename = sys.argv[1]

    in_file = open(filename)
    out_file = open(filename + '.lsp', 'w')
    mapping = str.maketrans(' #$@.*+', '0123456')

    out_file.write('(setq {} \'('.format(filename))

    for line in in_file:
        out_file.write('(' + ' '.join(line.translate(mapping)) + ')')
        out_file.write('\n              ')

    out_file.write('))')

if __name__ == '__main__':
    main()
