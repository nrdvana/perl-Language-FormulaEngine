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
		is( $engine->evaluate($_->[0], \%vals), $_->[1], $_->[0] )
			for @$fn_group;
	};
};

done_testing;
