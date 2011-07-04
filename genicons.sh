#!/bin/bash
ICON="apelsin"
for i in 16 22 24 32 48 64 128 256; do
inkscape --export-png="data/$ICON$i.png" \
	 	 --export-width=$i \
		 --export-height=$i \
		 "$ICON.svg"
done
