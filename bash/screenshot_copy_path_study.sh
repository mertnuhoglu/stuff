#!/bin/bash
dest="$HOME/prj/study"

save_folder="img"   # actual relative path
folder="./assets"   # relative path for logseq repos

file_name="scs$(date +%Y%m%d)_$(date +%H%M%S).png"
save_path="${dest}/${save_folder}/${file_name}"
relative_path="${folder}/${file_name}"
printf %s $relative_path | pbcopy
screencapture -i "${save_path}"
/Applications/ImageOptim.app/Contents/MacOS/ImageOptim "${save_path}"  


