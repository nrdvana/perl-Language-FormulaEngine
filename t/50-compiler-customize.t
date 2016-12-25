#! /usr/bin/env perl
use strict;
use warnings;
use Test::More;
use Data::Dumper;
use Log::Any::Adapter 'TAP';
use Log::Any '$log';
use Try::Tiny;

use_ok( 'Language::FormulaEngine' ) or BAIL_OUT;

my $vars= { baz => 42 };
package MyContext {
	use Moo;
	extends 'Language::FormulaEngine::Compiler';
	sub fn_customfunc { shift; return "arguments are ".join(', ', @_)."\n"; }
};
my $engine= Language::FormulaEngine->new(compiler => MyContext->new);
my $formula= $engine->compile( 'CustomFunc(baz,2,3)' );
is( $formula->($vars), "arguments are 42, 2, 3\n", 'correct result' );

done_testing;
