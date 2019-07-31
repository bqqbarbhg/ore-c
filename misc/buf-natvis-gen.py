import os
import re

base = os.path.abspath(os.path.dirname(__file__) + '/..')

try:
    os.mkdir(root + '/proj')
    print('Created /proj directory')
except:
    pass

buf_types = set()

re_typedef = re.compile(r'.*typedef\s+buf_type\(.*\)\s+(\w+)\s*;')

def process_file(name):
    with open(name) as file:
        for line in file:
            line = line.strip()
            m = re_typedef.match(line)
            if m:
                buf_types.add(m.group(1))

for (root, dirs, files) in os.walk(base + '/src'):
    for file in files:
        process_file(os.path.join(root, file))

natvis_prefix = '''
<?xml version="1.0" encoding="utf-8"?> 
<AutoVisualizer xmlns="http://schemas.microsoft.com/vstudio/debugger/natvis/2010">
'''.strip('\r\n')
natvis_type = '''
  <Type Name="{name}">
    <Expand>
      <Item Name="size">size</Item>
      <Item Name="capacity">capacity</Item>
      <ArrayItems>
        <Size>size</Size>
        <ValuePointer>data</ValuePointer>
      </ArrayItems>
    </Expand>
  </Type>
'''.strip('\r\n')
natvis_suffix = '''
</AutoVisualizer>
'''.strip('\r\n')

def emit_natvis():
    lines = []
    names = sorted(buf_types)
    lines.append(natvis_prefix)
    for name in names:
        lines.append(natvis_type.format(name=name))
    lines.append(natvis_suffix)
    return '\n'.join(lines) + '\n'

dst = os.path.realpath(base + '/proj/buf_types.natvis')

natvis = emit_natvis()
with open(dst, 'w') as file:
    file.write(natvis)

print(f'Wrote {len(buf_types)} buf types to {dst}')
