#!/bin/bash

input_filename="$1"
output_filename="$2"
cachefilename="$3"

if [ ! -f "$input_filename" ]; then
        exit 1
fi

mkdir -p "$(dirname "$output_filename")"


if ! convert "$input_filename" -trim png:- | pngquant --skip-if-larger - > "$output_filename"; then
        convert "$input_filename" -trim png:- | pngquant - > "$output_filename"
fi

if [ "${cachefilename}" != "" ]; then
	#A simple array assignment works: Note that the first element of a bash array can be addressed by just the name without the [0] index, ie, $md5 contains only the 32 chars of the md5sum. 
	md5=($(md5sum "$output_filename")) 

	Rscript -e "a<-readRDS('$cachefilename');a\$image_file_digest<-'$md5';saveRDS(a,file='$cachefilename', compress='xz')"
fi
