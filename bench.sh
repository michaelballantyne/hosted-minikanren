echo "build master branch"
git checkout master
raco make bench/base/bench.rkt
raco make bench/ee/bench.rkt

sleep 100

echo "master branch base 1"
racket bench/base/bench.rkt

sleep 100

echo "master branch base 2"
racket bench/base/bench.rkt

sleep 100

echo "master branch base 3"
racket bench/base/bench.rkt

sleep 100

echo "master branch ee 1"
racket bench/ee/bench.rkt

sleep 100

echo "master branch ee 2"
racket bench/ee/bench.rkt

sleep 100

echo "master branch ee 3"
racket bench/ee/bench.rkt

sleep 100


echo "build opt branch"
git checkout opt
raco make bench/base/bench.rkt
raco make bench/ee/bench.rkt

sleep 100


echo "opt branch base 1"
racket bench/base/bench.rkt

sleep 100

echo "opt branch base 2"
racket bench/base/bench.rkt

sleep 100

echo "opt branch base 3"
racket bench/base/bench.rkt

sleep 100

echo "opt branch ee 1"
racket bench/ee/bench.rkt

sleep 100

echo "opt branch ee 2"
racket bench/ee/bench.rkt

sleep 100

echo "opt branch ee 3"
racket bench/ee/bench.rkt


