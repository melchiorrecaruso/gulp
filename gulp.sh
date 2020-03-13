#!/bin/sh
echo "*** BUILD GULP DEB PACKCAGE ***"

mkdir build

lazbuild -B -q -q gulp.lpi
lazbuild -B -q -q gulp-ux.lpi
lazbuild -B -q -q gulp-diff.lpi

gzip -7 -k -n install/gulp-cli.man

cd build

equivs-build gulp.equivs

cd ..

mv build/gulp_0.4_amd64.deb     gulp_0.4_amd64.deb

rm -f -r build

echo "*** END ***"




