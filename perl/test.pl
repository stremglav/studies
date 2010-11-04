#!/usr/bin/perl
my $x=1;
$x = $x++ + ++$x;
print $x."\n";

print(print "A", print "B", print "C");
print "\n";

my $x=0;
(1)?$x=1:$x=2;
print $x."\n";
