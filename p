#!/bin/bash

open_url=false

while getopts ":o" opt; do
  case ${opt} in
    o ) # process option o
      open_url=true
      ;;
    \? )
      echo "Invalid option: $OPTARG" 1>&2
      ;;
  esac
done
shift $((OPTIND -1))

git add .

# Check if commit message is provided
commit_message="$*"

# If no commit message is provided, use the default message with current time
if [ -z "$commit_message" ]; then
    commit_message="God bless me on $(date +"%Y-%m-%d") at $(date +"%H:%M:%S")"
fi

# Commit with the provided or default commit message
git commit -m "$commit_message"

git push

# Get the URL from the git remote
remote_url=$(git remote get-url origin)

# Check if the open_url command is available
if $open_url ; then
    # Open the URL obtained from git remote
    open "$remote_url"
fi

# to update:
# cp ./pousse ./bash_files/pousse.sh