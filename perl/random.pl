#!/usr/bin/perl
use Data::Dumper;

my $AAA = 10000000;
my $i=0;
my $c=0;
while ($i < $AAA) {
  $c += int (rand (2));
  $i++;
}

print $c."\n";
printf "proc - %2g \n\n", (50 - (($c/$AAA) * 100));
