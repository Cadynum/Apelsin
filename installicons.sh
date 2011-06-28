#!/bin/sh
ICON="apelsin"
for i in 16 22 24 32 48 64 128 256; do
	 cp "$ICON$i.png" "/usr/share/icons/hicolor/${i}x${i}/apps/$ICON.png"
done
xdg-icon-resource forceupdate --theme hicolor &> /dev/null
