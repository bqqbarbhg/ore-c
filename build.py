#!/usr/bin/env python3

import os
import subprocess
from collections import defaultdict

repo_root = os.path.dirname(os.path.realpath(__file__))

def proj_path(path):
    abs_path = os.path.join(repo_root, path)
    return os.path.relpath(abs_path)

src_root = proj_path('src')
build_root = proj_path('build')

if not os.path.exists(build_root):
    print('build/')
    os.mkdir(build_root)

def source_path(path):
    return os.path.relpath(path, src_root)

def build_path(source, ext):
    base, _ = os.path.splitext(source)
    return os.path.join(build_root, base + ext)

def get_deps(source):
    yield os.path.join(src_root, source)
    try:
        with open(build_path(source, '.d')) as f:
            data = f.read().strip()
            for dep in data.split()[1:]:
                if dep == '\\': continue
                dep = proj_path(dep)
                if dep:
                    yield dep
    except:
        pass

def get_sources():
    for (root, dirs, files) in os.walk(src_root):
        for f in files:
            if f.endswith('.c'):
                yield source_path(os.path.join(root, f))

date_cache = { }

def get_date(path):
    try:
        cached = date_cache.get(path)
        if cached: return cached
        date = os.path.getmtime(path)
        date_cache[path] = date
        return date
    except:
        return 0

src_to_build = []
obj_files = []

for src in get_sources():
    path = os.path.join(src_root, src)
    date = 0
    for dep in get_deps(src):
        date = max(date, get_date(dep))
    obj_file = build_path(src, '.o')
    obj_date = get_date(obj_file)
    if date > obj_date or not obj_date:
        src_to_build.append(src)
    obj_files.append(obj_file)

for src in sorted(src_to_build):
    print(src)
    path = os.path.join(src_root, src)
    obj_file = build_path(src, '.o')
    dep_file = build_path(src, '.d')
    subprocess.run(['gcc', '-c', path, '-o', obj_file])
    subprocess.run(['gcc', path, '-MF', dep_file, '-MM'])

if src_to_build:
    print('ore-c')
    exe_path = os.path.join(build_root, 'ore-c')
    subprocess.run(['gcc', '-o', exe_path] + obj_files)

