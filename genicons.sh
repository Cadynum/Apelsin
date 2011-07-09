#!/bin/bash
ICON="apelsin"
for i in 16 22 24 32 48 64 128 256; do
DDIR="data/hicolor/${i}x${i}/apps"
mkdir -p $DDIR
inkscape --export-png="$DDIR/$ICON.png" \
	 	 --export-width=$i \
		 --export-height=$i \
		 "$ICON.svg"
done

