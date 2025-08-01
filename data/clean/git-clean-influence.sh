#!/bin/bash

# Navigate to the repository root
cd ..

# Create a list of large CSV files over 10MB
git rev-list --objects --all | \
  git cat-file --batch-check='%(objecttype) %(objectname) %(objectsize) %(rest)' | \
  sed -n 's/^blob //p' | \
  awk '$2 >= 1048576 && $3 ~ /^data\/influence\/.*\.csv$/ {print $3}' > large_csvs.txt

# Remove the files from Git history
if [ -s large_csvs.txt ]; then
  # Create the filter branch command
  filter_command="git filter-branch --force --index-filter '"
  
  while IFS= read -r file; do
    filter_command+="git rm --cached --ignore-unmatch \"$file\"; "
  done < large_csvs.txt
  
  filter_command+="' --prune-empty --tag-name-filter cat -- --all"
  
  # Execute the command
  eval "$filter_command"
  
  # Clean up refs and garbage collect
  git for-each-ref --format='delete %(refname)' refs/original | git update-ref --stdin
  git reflog expire --expire=now --all
  git gc --prune=now --aggressive
  
  echo "Large CSV files have been removed from history. Files removed:"
  cat large_csvs.txt
else
  echo "No CSV files larger than 10MB found in the specified path."
fi

# Clean up temporary file
rm large_csvs.txt
