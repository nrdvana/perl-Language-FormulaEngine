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
				q|(this.foo+5)|
			],
			[ 'foo+1*baz+bar/6/7/8',
				q|(this.foo+(1*this.baz)+(((this.bar/6)/7)/8))|
			],
			[ 'IF(foo,bar,baz)',
				q|(this.foo?this.bar:this.baz)|
			],
		],
		js_dep => '',
	},
	{
		name => 'With custom var access',
		args => {
			js_function_prefix => 'FE_',
			gen_var_access => sub { 'FE_get_var('.$_[0]->jsgen_string_literal($_[1]->symbol_name).',arguments)' },
		},
		tests => [
			[ 'foo+5',
				q|(FE_get_var("foo",arguments)+5)|
			],
		],
		js_dep => '',
	},
);

for my $outer (@tests) {
	subtest $outer->{name} => sub {
		my $fe= Language::FormulaEngine->new(compiler => Language::FormulaEngine::JSCompiler->new($outer->{args}));
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
