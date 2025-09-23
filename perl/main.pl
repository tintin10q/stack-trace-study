use strict; use warnings;
sub dangerous { my ($array, $index) = @_; return $array->[$index + 2]; }
sub foo  { my ($array, $index) = @_; return dangerous($array, $index); }
sub foo1 { my ($array, $index) = @_; return foo($array, $index * 3); }
sub foo2 { my ($array, $index) = @_; return foo1($array, $index + 137); }
sub foo3 { my ($array, $index) = @_; return foo2($array, $index - 1); }
sub foo4 { my ($array, $index) = @_; return foo3($array, $index * 137); }
sub foo5 { my ($array, $index) = @_; return foo4($array, $index + 20); }
sub foo6 { my ($array, $index) = @_; return foo5($array, int($index / 3)); }
my @array = (0) x 1000;
print foo6(\@array, 50), "\n";

