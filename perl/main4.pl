use strict; use warnings;
sub dangerous { my ($v1,$v2)=@_; return int($v1 / $v2); }
sub foo { my ($ar,$c)=@_; return $c==0 ? dangerous($ar->[0], $c) : foo($ar, $c-1) }
my @array = (0) x 1000;
print "The result is ", foo(\@array, 6), "\n";

