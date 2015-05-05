mkdir newfiles
FILES="*.m4a
*.mp4
*.flv"
for f in $FILES; do ffmpeg -i "$f" -codec:v copy -codec:a libmp3lame -q:a 2 newfiles/"${f%.m4a}.mp3"; done

