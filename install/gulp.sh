#!/bin/sh
echo "*** BUILD GULP DEBIAN PACKCAGE ***"

lazbuild -B -q -q gulp.lpi
lazbuild -B -q -q gulpx.lpi
lazbuild -B -q -q gdiff.lpi

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
