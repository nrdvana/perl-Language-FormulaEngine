#! /usr/bin/env perl
use Test2::V0;
use Language::FormulaEngine;
use Math::Trig;

my %vals= (
	v => 1.1,
);
my @tests= (
	[
		# No need to write extensive tests for these as they just pass-through to Perl
		[ 'abs(-4)'    => 4 ],
		[ 'acos(1)'    => 0 ],
		[ 'acot(1)'    => Math::Trig::pi()/4 ],
		[ 'asin(0)'    => 0 ],
		[ 'atan(0)'    => 0 ],
		[ 'atan2(1,0)' => 0 ],
		[ 'cos(0)'     => 1 ],
		[ 'degrees(0)' => 0 ],
		[ 'exp(0)'     => 1 ],
	],[
		[ 'average(1,2,3)' => 2 ],
	],[
		[ 'base(55,13,5)' => '00043' ],
		[ 'base(33,2)'    => '100001' ],
	],[
		[ 'char(50)'      => 2 ],
		[ 'char(0)'       => "\0" ],
	],[
		[ 'choose(3,2,4,6)' => 6 ],
	],[
		[ 'ceiling(4.5)'    => 5 ],
		[ 'ceiling(2.5,2)'  => 4 ],
		[ 'ceiling(2.5,-2)' => 2 ],
		[ 'ceiling(-2.5,2)' => -2 ],
		[ 'ceiling(-2.5,-2)'=> -4 ],
	],[
		[ 'clean("F OO")'     => 'F OO' ],
		[ 'clean(char(0))'    => '' ],
		[ 'clean(char(1))'    => '' ],
		[ 'clean(char(9))'    => '' ],
		[ 'clean(char(10))'   => '' ],
		[ 'clean(char(13))'   => '' ],
		[ 'clean(char(127))'  => '' ],
	],[
		[ 'code("A")'     => 65 ],
	],[
		[ 'fact(1)'       => 1 ],
		[ 'fact(3)'       => 6 ],
		[ 'fact(0)'       => 1 ],
	],[
		[ 'floor(4.5)'    => 4 ],
		[ 'floor(2.5,2)'  => 2 ],
		[ 'floor(2.5,-2)' => 4 ],
		[ 'floor(-2.5,2)' => -4 ],
		[ 'floor(-2.5,-2)'=> -2 ],
	],[
		[ 'fixed(12345.678)'       => '12,345.68' ],
		[ 'fixed(12,4)'            => '12.0000' ],
		[ 'fixed(15,-1)'           => '20' ],
		[ 'fixed(12345.678,2,0)'   => '12345.68' ],
		[ 'fixed(12345.678,2,".")' => '12.345,68' ],
	],[
		[ 'IFERROR(2+2, "ERR")'   => 4 ],
		[ 'IFERROR(2+"a", "ERR")' => 'ERR' ],
		[ 'IFERROR(IFERROR(2+"a", "ERR"+1), IFERROR("b"-1, "ERR2"))' => 'ERR2' ],
	],[
		[ 'IFS(0, 0, "", 1, " ", 2)' => 2 ],
		[ 'IFS(0+1 > 1, 3, 1, 4)'    => 4 ],
		[ 'IFERROR(IFS(0, 1), 9)'    => 9 ],
	],[
		[ 'roundup(v * 2)'    => '3' ],
		[ 'roundup(v * 2, 0)' => '3' ],
		[ 'roundup(v * 2, 1)' => '2.2' ],
		[ 'roundup(v * 2, 2)' => '2.2' ],
	],[
		[ 'rounddown(v * 2)'    => '2' ],
		[ 'rounddown(v * 2, 0)' => '2' ],
		[ 'rounddown(v * 2, 1)' => '2.2' ],
		[ 'rounddown(v * 2, 2)' => '2.2' ],
	]
);
my $engine= Language::FormulaEngine->new();
for my $fn_group (@tests) {
	my ($name)= $fn_group->[0][0] =~ /^(\w+)/;
	subtest $name => sub {
		is( $engine->evaluate($_->[0], \%vals), $_->[1], 'interpret '.$_->[0] )
			for @$fn_group;
		is( $engine->compile($_->[0])->(\%vals), $_->[1], 'compile '.$_->[0] )
			for @$fn_group;
	};
};

done_testing;
