#! /usr/bin/env perl
use Test2::V0;
use Try::Tiny;
use Language::FormulaEngine;
use Language::FormulaEngine::JSCompiler;

my @tests= (
	{
		name => 'Default Compiler',
		args => {},
		tests => [
			[ 'foo+5',
				q|(arguments[0]["foo"]+5)|
			],
			[ 'foo+1*baz+bar/6/7/8',
				q|(arguments[0]["foo"]+(1*arguments[0]["baz"])+(((arguments[0]["bar"]/6)/7)/8))|
			],
			[ 'IF(foo,bar,baz)',
				q|(arguments[0]["foo"]?arguments[0]["bar"]:arguments[0]["baz"])|
			],
		],
		js_dep => '',
	},
	{
		name => 'With custom var access',
		args => {
			variables_via_method => 'get_var'
		},
		tests => [
			[ 'foo+5',
				q|(arguments[0].get_var("foo")+5)|
			],
		],
		js_dep => '',
	},
);

for my $outer (@tests) {
	subtest $outer->{name} => sub {
		my $fe= Language::FormulaEngine->new(
			compiler => Language::FormulaEngine::JSCompiler->new($outer->{args}),
			namespace => 'Language::FormulaEngine::Namespace::JSDefault',
		);
		for (@{ $outer->{tests} }) {
			my ($str, $code, $err_regex)= @$_;
			subtest qq{"$str"} => sub {
				ok( my $tree= $fe->parser->parse($str), 'parse succeeded' ) or diag $fe->parser->error;
				if (defined $code) {
					ok( (my $gencode= $fe->compiler->jsgen($tree)), 'compile succeeded' );
					is( $gencode, $code, 'correct code generated' );
				}
				else {
					my $gencode;
					like( dies { $gencode= $fe->compiler->jsgen($tree) }, $err_regex, 'correct error message' )
						or diag $gencode;
				}
				done_testing;
			};
		}
		is( $fe->compiler->dependency_code, $outer->{js_dep}, 'dependency code' );
		done_testing;
	};
}
done_testing;
