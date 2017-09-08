#!/bin/sh
echo "*** BUILD GULP DEB PACKCAGE ***"

mkdir build

lazbuild -B -q -q gulp.lpi
lazbuild -B -q -q gulpx.lpi
lazbuild -B -q -q gdiff.lpi

gzip -7 -k -n install/gulp.man

mv build/x86_64-linux/bin/gulp  build/gulp
mv build/x86_64-linux/bin/gulpx build/gulpx
mv build/x86_64-linux/bin/gdiff build/gdiff
mv install/gulp.man.gz          build/gulp.1.gz
cp install/CHANGELOG            build/CHANGELOG 
cp install/LICENSE              build/LICENSE 
cp install/README               build/README 
cp install/gulp.equivs          build/gulp.equivs 

cd build

equivs-build gulp.equivs

cd ..

mv build/gulp_0.4_amd64.deb     gulp_0.4_amd64.deb

rm -f -r build

echo "*** END ***"




