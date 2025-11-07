#!/usr/bin/env python3

from datetime import date
from pathlib import Path
from subprocess import run
from tempfile import TemporaryDirectory
from shutil import copyfile

repo = Path(__file__).parent.resolve()
build = repo / 'build'

all_changes_committed = not run(['git', 'status', '--porcelain'], cwd=repo, check=True, capture_output=True).stdout
assert all_changes_committed

current_commit = run(['git', 'rev-parse', 'HEAD'], cwd=repo, check=True, capture_output=True).stdout.decode()[0:8]

def copyto(src, dst):
  copyfile(src, dst / src.name)

with TemporaryDirectory() as tmp:
  release = Path(tmp)
  run(['make'], cwd=repo, check=True)
  run(['git', 'fetch', 'origin'], cwd=repo, check=True)
  run(['git', 'worktree', 'add', '--detach', release, 'origin/deploy'], cwd=repo, check=True)
  copyto(build / 'main.wasm', release)
  copyto(build / 'index2.html', release)
  run(['git', 'add', '.', release], cwd=release, check=True)
  run(['git', 'commit', '-m', f'Deploy {date.today()} from {current_commit}'], cwd=release, check=True)
  run(['git', 'push', 'origin', 'HEAD:deploy'], cwd=release, check=True)
  run(['git', 'worktree', 'remove', release], cwd=repo, check=True)
