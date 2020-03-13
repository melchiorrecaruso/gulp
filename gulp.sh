#!/bin/sh
echo "*** BUILD GULP-CLI DEB PACKCAGE ***"

mkdir build

lazbuild -B -q -q gulp.lpi
lazbuild -B -q -q gulpux.lpi
lazbuild -B -q -q gulpdiff.lpi

cd install

gzip -7 -k -n gulp-cli.man

mv gulp-cli.man.gz  gulp-cli.1.gz

equivs-build gulp-cli.equivs

rm gulp-cli.1.gz

cd ..
cd build

rm -f -r bin
rm -f -r lib

echo "*** END ***"




