RANGES = '''
00A8	00AA	00AD	00AF	00B2-00B5
00B7-00BA	00BC-00BE	00C0-00D6	00D8-00F6	00F8-00FF
0100-167F	1681-180D	180F-1FFF		
200B-200D	202A-202E	203F-2040	2054	2060-206F
2070-218F	2460-24FF	2776-2793	2C00-2DFF	2E80-2FFF
3004-3007	3021-302F	3031-D7FF		
F900-FD3D	FD40-FDCF	FDF0-FE44	FE47-FFFD	
10000-1FFFD	20000-2FFFD	30000-3FFFD	40000-4FFFD	50000-5FFFD
60000-6FFFD	70000-7FFFD	80000-8FFFD	90000-9FFFD	A0000-AFFFD
B0000-BFFFD	C0000-CFFFD	D0000-DFFFD	E0000-EFFFD
'''

CONT_RANGES = '''
0300-036F	1DC0-1DFF	20D0-20FF	FE20-FE2F
'''

def parse_range(s):
    if '-' in s:
        lo, hi = s.split('-')
        return (int(lo, 16), int(hi, 16))
    else:
        return (int(s, 16), int(s, 16))

def emit(ranges):
    cols = 4
    for base in range(0, len(ranges), cols):
        for r in ranges[base:base+cols]:
            print('0x%05x,0x%05x, ' % r, end='')
        print()

print('Init:')
emit(list(sorted(parse_range(s) for s in RANGES.split())))
print('Cont:')
emit(list(sorted(parse_range(s) for s in CONT_RANGES.split())))
