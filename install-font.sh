#!/bin/sh

# don't know where to put this yet

cd tmp
curl -L -O "https://github.com/be5invis/Iosevka/releases/download/v1.14.2/01-iosevka-1.14.2.zip"
unzip 01-iosevka-1.14.2.zip
cd ttf
mkdir -p /usr/local/share/emacs/fonts
cp *.ttf /usr/local/share/emacs/fonts
