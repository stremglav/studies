#!/usr/bin/perl 
  
use strict;
use warnings;
use Frontier::Client;
use Data::Dumper;

my $dd = {"aaa" => "P 1 ragmatic Project Automation"};
print Dumper($dd);
  
my $url  = "http://vovkas.info:3000/web_service/api";
my $client = Frontier::Client->new('url' => $url);

print Dumper($client->call('FindAllProducts'));
print Dumper($client->call('FindProductById', 1));
print Dumper($client->call('FindProducts', [{'title' => "qqq'qqq"},
                                            {'id'=>"1"},
                                            {'title' => "P 3 ragmatic Unit Testing (C#)"},
                                            {'price' => [-1, -1]}]));
