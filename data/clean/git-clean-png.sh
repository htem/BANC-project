#!/bin/bash

# Navigate to the repository root
cd ..

# Define the size limit in bytes (100KB)
SIZE_LIMIT=102400

# Use Python to process the files
python3 - <<EOF
import subprocess
import sys

def get_current_pngs():
    result = subprocess.run(['git', 'ls-tree', '-r', 'HEAD', '--name-only'], capture_output=True, text=True)
    return set(line.strip() for line in result.stdout.split('\n') if line.strip().endswith('.png'))

def get_large_pngs(size_limit, current_pngs):
    cmd = ['git', 'rev-list', '--objects', '--all']
    result = subprocess.run(cmd, capture_output=True, text=True)
    objects = result.stdout.split('\n')

    large_pngs = set()
    for obj in objects:
        if not obj.strip():
            continue
        sha = obj.split()[0]
        cmd = ['git', 'cat-file', '-s', sha]
        size_result = subprocess.run(cmd, capture_output=True, text=True)
        size = int(size_result.stdout.strip())

        if size >= size_limit:
            cmd = ['git', 'cat-file', '-p', sha]
            content_result = subprocess.run(cmd, capture_output=True)
            if content_result.stdout.startswith(b'\x89PNG'):
                cmd = ['git', 'rev-list', '--objects', '--all', '--filter=blob:limit=1', sha]
                path_result = subprocess.run(cmd, capture_output=True, text=True)
                path = path_result.stdout.split()[-1]
                if path.endswith('.png') and path not in current_pngs:
                    large_pngs.add(path)

    return large_pngs

current_pngs = get_current_pngs()
large_pngs = get_large_pngs($SIZE_LIMIT, current_pngs)

with open('large_pngs.txt', 'w') as f:
    for png in large_pngs:
        f.write(png + '\n')

if large_pngs:
    print(f"Found {len(large_pngs)} large PNG files.")
else:
    print("No PNG files larger than 100KB found in the repository that are not currently committed.")
EOF

# Remove the files from Git history
if [ -s large_pngs.txt ]; then
  # Create the filter branch command
  filter_command="git filter-branch --force --index-filter '"

  while IFS= read -r file; do
    filter_command+="git rm --cached --ignore-unmatch \"${file}\"; "
  done < large_pngs.txt

  filter_command+="' --prune-empty --tag-name-filter cat -- --all"

  # Execute the command
  eval "$filter_command"

  # Clean up refs and garbage collect
  git for-each-ref --format='delete %(refname)' refs/original | git update-ref --stdin
  git reflog expire --expire=now --all
  git gc --prune=now --aggressive

  echo "Large PNG files have been removed from history. Files removed:"
  cat large_pngs.txt
else
  echo "No PNG files larger than 100KB found in the repository that are not currently committed."
fi

# Clean up temporary file
rm large_pngs.txt