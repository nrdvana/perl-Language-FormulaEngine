#! /usr/bin/env perl
use strict;
use warnings;
use Test::More;
use Data::Dumper;
use Log::Any::Adapter 'TAP';
use Log::Any '$log';
use Try::Tiny;

use_ok( 'Language::FormulaEngine::Scanner' ) or BAIL_OUT;

# capture error message of code that should die
sub error_of(&) { my $sub= shift; try { $sub->(); 'No Exception Thrown' } catch { $_ } }

my %_escape_mapping= ("\0" => '\0', "\n" => '\n', "\r" => '\r', "\t" => '\t', "\f" => '\f', "\b" => '\b', "\a" => '\a', "\e" => '\e', "\\" => '\\' );
sub escape_char { exists $_escape_mapping{$_[0]}? $_escape_mapping{$_[0]} : sprintf((ord $_[0] <= 0xFF)? "\\x%02X" : "\\x{%X}", ord $_[0]); }
sub escape_str { my $str= shift; $str =~ s/([^\x20-\x7E])/escape_char($1)/eg; $str; }

sub test_scanner {
	my @tests= (
		[ ''
		],
		[ " \t\r\n\t   \r\n"
		],
		[ "foo",
			[ ident => 'foo', 0, 0 ],
		],
		[ "foo bar",
			[ ident => 'foo', 0, 0 ],
			[ ident => 'bar', 0, 4 ],
		],
		[ "1.57e-28+34",
			[ num => 1.57e-28, 0, 0 ],
			[ '+' => '+',      0, 8 ],
			[ num => 34,       0, 9 ],
		],
		[ "12A_1e-5,foOO(bar,34,baz)",
			[ num => '12',       0, 0 ],
			[ ident => 'A_1e',   0, 2 ],
			[ '-' => '-',        0, 6 ],
			[ num => 5,          0, 7 ],
			[ ',' => ',',        0, 8 ],
			[ ident => 'foOO',   0, 9 ],
			[ '(' => '(',        0, 13 ],
			[ ident => 'bar',    0, 14 ],
			[ ',' => ',',        0, 17 ],
			[ num => 34,         0, 18 ],
			[ ',' => ',',        0, 20 ],
			[ ident => 'baz',    0, 21 ],
			[ ')' => ')',        0, 24 ],
		],
		[ ">=<=>>==!===<>\x{2260}\x{2264}\x{2265}",
			[ '>=' => '>=', 0, 0 ],
			[ '<=' => '<=', 0, 2 ],
			[ '>'  => '>',  0, 4 ],
			[ '>=' => '>=', 0, 5 ],
			[ '==' => '=',  0, 7 ],
			[ '!=' => '!=', 0, 8 ],
			[ '==' => '==', 0, 10 ],
			[ '!=' => '<>', 0, 12 ],
			[ '!=' => "\x{2260}", 0, 14 ],
			[ '<=' => "\x{2264}", 0, 15 ],
			[ '>=' => "\x{2265}", 0, 16 ],
		],
		[ '(a, b, c)',
			[ '(' => '(',   0, 0 ],
			[ ident => 'a', 0, 1 ],
			[ ',' => ',',   0, 2 ],
			[ ident => 'b', 0, 4 ],
			[ ',' => ',',   0, 5 ],
			[ ident => 'c', 0, 7 ],
			[ ')' => ')',   0, 8 ],
		],
		[ q{ "foo"'foo'"foo\" },
			[ str => 'foo',   0, 1 ],
			[ str => 'foo',   0, 6 ],
			[ str => 'foo\\', 0, 11 ],
		],
	);

	for (@tests) {
		my ($str, @tokens)= @$_;
		subtest '"'.escape_str($str).'"' => sub {
			my $p= new_ok( 'Language::FormulaEngine::Scanner', [ input => $str ], 'new scanner' );
			$p->next_token;
			my $i= 1;
			for (@tokens) {
				is( $p->token_type,  $_->[0], "token $i type '$_->[0]'" );
				is( $p->token_value, $_->[1], "token $i value" );
				my ($row, $col)= $p->token_row_col;
				is( $row, $_->[2], "token $i row" );
				is( $col, $_->[3], "token $i col" );
				is( $p->consume_token, $_->[1] );
				$i++;
			}
			is( $p->token_type, 'eof' );
			like( error_of {; $p->consume_token; }, qr/EOF/, 'consume at eof dies' );
			done_testing;
		};
	}
	done_testing;
}

test_scanner();
