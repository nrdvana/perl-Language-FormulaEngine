#! /usr/bin/env perl
use Test2::V0;
use Language::FormulaEngine;

my %vals= (
	v => 1.1,
);
my @tests= (
	[
		[ 'base(55,13,5)' => '00043' ],
		[ 'base(33,2)'    => '100001' ],
	],[
		[ 'format_fraction(35,2)' => '17 1/2' ],
		[ 'format_fraction(12.4)' => '12 2/5' ],
		[ 'format_fraction(-12.4)' => '-12 2/5' ],
		[ 'format_fraction(.4)'   => '2/5' ],
		[ 'format_fraction(2,10)' => '1/5' ],
		[ 'format_fraction(0.400)' => '2/5' ],
		[ 'format_fraction(-0.400)' => '-2/5' ],
		[ 'format_fraction(1)' => '1' ],
		[ 'format_fraction(-1)' => '-1' ],
		[ 'format_fraction(0)' => '0' ],
	],[
		[ 'parse_fraction("1")' => '1' ],
		[ 'parse_fraction("0.5")' => '0.5' ],
		[ 'parse_fraction("1/2")' => '0.5' ],
		[ 'parse_fraction("1 1/2")' => '1.5' ],
		[ 'parse_fraction("1.5 1/1")' => '2.5' ],
	],[
		[ 'roundup(v * 2)' => '3' ],
		[ 'roundup(v * 2, 0)' => '3' ],
		[ 'roundup(v * 2, 1)' => '2.2' ],
		[ 'roundup(v * 2, 2)' => '2.2' ],
	],[
		[ 'rounddown(v * 2)' => '2' ],
		[ 'rounddown(v * 2, 0)' => '2' ],
		[ 'rounddown(v * 2, 1)' => '2.2' ],
		[ 'rounddown(v * 2, 2)' => '2.2' ],
	]
);
my $engine= Language::FormulaEngine->new();
for my $fn_group (@tests) {
	my ($name)= $fn_group->[0][0] =~ /^(\w+)/;
	subtest $name => sub {
		is( $engine->evaluate($_->[0]), $_->[1], $_->[0] )
			for @$fn_group;
	};
};

done_testing;
