#!/bin/sh
echo "*** BUILD GULP DEBIAN PACKCAGE ***"

lazbuild -q -q gulp.lpi
lazbuild -q -q gulpx.lpi
lazbuild -q -q gdiff.lpi

gzip -9 -k -n docs/gulp.man

mv build/x86_64-linux/bin/gulp  gulp
mv build/x86_64-linux/bin/gulpx gulpx
mv build/x86_64-linux/bin/gdiff gdiff
mv docs/gulp.man.gz             gulp.1.gz

equivs-build gulp.equivs

rm -f gulp
rm -f gulpx
rm -f gdiff
rm -f gulp.1.gz
