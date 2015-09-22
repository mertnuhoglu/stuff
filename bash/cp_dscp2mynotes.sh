#!/bin/sh
for file in /Users/mertnuhoglu/projects/dewey/dscp/*.*; do
	filename=${file##*/}
	cp -a $file /Users/mertnuhoglu/Dropbox/mynotes/dscp_${filename}
done
