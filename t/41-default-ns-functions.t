#! /usr/bin/env perl
use Test2::V0;
use Language::FormulaEngine;
use Language::FormulaEngine::Error ':all';
use Math::Trig;
use Try::Tiny;
use DateTime;

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
		[ 'date(2019,1,1)' => DateTime->new(year => 2019, month => 1, day => 1) ],
		[ 'date("x")'      => undef, object { prop blessed => ErrInval } ],
	],[
		[ 'datedif("2018-01-10","2019-01-01","m")'   => 11 ],
#		[ 'datedif("2019-01-10","2018-01-01","m")'   => undef, object { prop blessed => ErrInval } ],
	],[
		[ 'day("2019-01-10")' => 10 ],
		[ 'day("x")'          => undef, object { prop blessed => ErrInval } ],
	],[
		[ 'days("2019-01-01", "2019-02-01")' => -31 ],
		[ 'days("2020-01-01", "2019-01-01")' => 365 ],
	],[
		[ 'eomonth("2019-01-01")'         => DateTime->new(year => 2019, month => 1, day => 31) ],
		[ 'day(eomonth("2019-01-01",1))'  => 28 ],
		[ 'day(eomonth("2019-01-01",-2))' => 30 ],
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
		[ 'hour("2019-01-02")'      => 0 ],
		[ 'hour("2019/07/07 4:32")' => 4 ],
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
		for (@$fn_group) {
			my ($code, $val, $err)= @$_;
			my ($eval_val, $eval_err, $compile_val, $compile_err);
			try {; $eval_val= $engine->evaluate($code, \%vals); } catch {; $eval_err= $_; };
			try {; $compile_val= $engine->compile($code)->(\%vals); } catch {; $compile_err= $_; };
			if (!defined $err) {
				is( $eval_val, $val, 'interpret '.$code );
				diag $eval_err if defined $eval_err;
				is( $compile_val, $val, 'compile '.$code );
				diag $compile_err if defined $compile_err;
			}
			else {
				is( $eval_err, $err, 'interpret '.$code );
				is( $compile_err, $err, 'compile '.$code );
			}
		}
	};
};

done_testing;
