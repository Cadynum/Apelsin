#!/bin/bash
ICON="apelsin"
for i in 16 22 24 32 48 64 128 256; do
FP="share/icons/${i}x${i}/apps"
mkdir -p "$FP"
inkscape --export-png="$FP/$ICON.png" \
	 	 --export-width=$i \
		 --export-height=$i \
		 "$ICON.svg"
done
cp "share/icons/16x16/apps/apelsin.png" .
