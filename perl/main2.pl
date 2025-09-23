use strict;
use warnings FATAL => 'uninitialized';

sub dangerous {
    my ($array_ref, $index) = @_;
    return $array_ref->[$index]; # undef -> fatal when used later
}

sub foo {
    my ($array_ref, $counter) = @_;
    if ($counter == 0) {
        return dangerous($array_ref, $counter + 9137);
    } else {
        return foo($array_ref, $counter - 1);
    }
}

my @array = (0) x 1000;
my $result = foo(\@array, 6);
print "The result is $result\n";
