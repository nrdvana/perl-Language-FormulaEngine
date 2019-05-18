package Language::FormulaEngine::Namespace::Default;
our @ISA= ( 'Language::FormulaEngine::Namespace::V0' );

=head1 DESCRIPTION

This is a L<namespace|Language::FormulaEngine::Namespace> containing many spreadsheet-like
functions.  It aims for spreadsheet similarity rather than compatibility; the goal to give
users of the FormulaEngine a familiar environmet rather than to try duplicating all features
and misfeatures Excel.

=head2 Core Grammar Functionality

These are the methods that implement the infix operators.

=over

=item C<< sum( num1, num2 ... ) >>

(Since V0)

=item C<< negative( num1 ) >>

(Since V0)

=item C<< mul( num1, num2, ... ) >>

(Since V0)

=item C<< div( numerator, denominator ) >>

(Since V0)

=item C<< if( condition, val_if_true, val_if_false ) >>

This works for both numbers and strings.

(Since V0)

=item C<< and( bool1, bool2, ... ) >>

This applies perl-ish boolean semantics to each argument, and returns a numeric 0 or 1.
No arguments are evaluated after the first false value.

(Since V0)

=item C<< or( bool1, bool2, ... ) >>

This applies perl-ish boolean semantics to each argument, and returns a numeric 0 or 1.
No arguments are evaluated after the first true value.

(Since V0)

=item C<< not( bool1 ) >>

This applies perl-ish boolean semantics to the argument and returns numeric 1 or 0.

=item C<< compare( val1, op, val2, ...op, val ) >>

This compares two or more values against the 6 canonical operators
C<< "<", "<=", ">", ">=", "==", "!=" >> and returns 0 or 1.

It uses numeric comparison if both sides of an operator C<looks_like_number>, and uses string
comparison otherwise.

=back

=cut

package Language::FormulaEngine::Namespace::V0;
use parent 'Language::FormulaEngine::Namespace';
use strict;
use warnings;
use List::Util ();
use Math::Trig ();
use POSIX ();

*fn_sum= *List::Util::sum0;
sub asperl_sum {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->asperl($_), @{$node->parameters};
	return '( '.join(' + ', @arg_code).' )';
}

sub fn_negative {
	@_ == 1 or die "Can only negate a single value, not a list\n";
	return -$_[0];
}
sub asperl_negative {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->asperl($_), @{$node->parameters};
	@arg_code == 1 or die "Can only negate a single value, not a list\n";
	return '(-('.$arg_code[0].'))';
}

*fn_mul= *List::Util::product;
sub asperl_mul {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->asperl($_), @{$node->parameters};
	return '( '.join(' * ', @arg_code).' )';
}

sub fn_div {
	@_ == 2 or die "div() takes exactly two arguments\n";
	$_[1] or die "division by zero\n";
	return $_[0] / $_[1];
}
sub asperl_div {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->asperl($_), @{$node->parameters};
	return '( '.join(' / ', @arg_code).' )';
}

sub eval_if { # customize eval_ to provide lazy evaluation of arguments
	my ($self, $node)= @_;
	@{$node->parameters} == 3 or die "IF(test, when_true, when_false) requires all 3 parameters\n";
	my $bool= $node->parameters->[0]->evaluate($self);
	return $node->parameters->[$bool? 1 : 2]->evaluate($self);
}
sub asperl_if {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->asperl($_), @{$node->parameters};
	@arg_code == 3 or die "IF(test, when_true, when_false) requires all 3 parameters\n";
	return '( '.$arg_code[0].'? '.$arg_code[1].' : '.$arg_code[2].' )';
}

sub eval_and { # customize eval_ to provide lazy evaluation of arguments
	my ($self, $node)= @_;
	$_->evaluate($self) or return 0
		for @{ $node->parameters };
	return 1;
}
sub asperl_and {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->asperl($_), @{$node->parameters};
	return '( ('.join(' and ', @arg_code).')? 1 : 0)';
}

sub eval_or {
	my ($self, $node)= @_;
	$_->evaluate($self) and return 1
		for @{ $node->parameters };
	return 0;
}
sub asperl_or {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->asperl($_), @{$node->parameters};
	return '( ('.join(' or ', @arg_code).')? 1 : 0)';
}

sub fn_not {
	@_ == 1 or die "Too many arguments to 'not'\n";
	return $_[0]? 1 : 0;
}
sub asperl_not {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->asperl($_), @{$node->parameters};
	@arg_code == 1 or die "Too many arguments to 'not'\n";
	return '('.$arg_code[0].'? 0 : 1)';
}

sub fn_compare {
	my $left= shift;
	while (@_) {
		my $op= shift;
		my $right= shift;
		my $numeric= looks_like_number($left) && looks_like_number($right);
		if ($op eq '==' or $op eq '!=') {
			return 0 unless ($numeric? ($left == $right) : ($left eq $right)) == ($op eq '==');
		}
		elsif ($op eq '>=' or $op eq '<') {
			return 0 unless ($numeric? ($left >= $right) : ($left ge $right)) == ($op eq '>=');
		}
		elsif ($op eq '<=' or $op eq '>') {
			return 0 unless ($numeric? ($left <= $right) : ($left le $right)) == ($op eq '<=');
		}
		else {
			die "Unhandled operator '$op' in compare()\n";
		}
		$left= $right;
	}
	return 1;
}

=head2 Math Functions

=over

=item abs( number )

Return absolute value of number

=item acos( ratio )

Return angle in radians of the ratio adjacent/hypotenuse.

=item acot( ratio )

Return angle in radians of the ratio adjacent/opposite.

=item asin( ratio )

Return angle in radians of the ratio opposite/hypotenuse.

=item atan( ratio )

Return angle in radians of the ratio opposite/adjacent.

=item atan2( x, y )

Same as atan, but without division, so x=0 returns PI/2 instead of division error.

=item average( num1, ... )

Return sum of numbers divided by number of arguments

=item base( num1, radix, min_length=0 )

Return number converted to different base, with optional leading zeroes to reach min_length.

=item round( number, digits=0 )

Round NUMBER to DIGITS decimal places of precision.  Uses the IEEE
5-round-to-even algorithm that C gives us.  DIGITS defaults to 0,
making it round to the nearest integer.

Dies if you attempt to round something that isn't a number.

=item roundup( number, digits=0 )

Like L</round>, but always round up.

=item rounddown( number, digits=0 )

Like L</round>, but always round down.

=back

=cut

*fn_abs= *CORE::abs;
*fn_acos= *Math::Trig::acos;
*fn_acot= *Math::Trig::acot;
*fn_asin= *Math::Trig::asin;
*fn_atan= *Math::Trig::atan;
sub fn_atan2 {
	# Perl differs in argument order from popular spreadsheet programs
	Math::Trig::atan2($_[1], $_[0])
}
sub fn_average {
	List::Utils::sum0(@_) / @_;
}
sub fn_base {
	my ($num, $radix, $min_length)= @_;
	my $digits= '';
	while ($num > 0) {
		use integer;
		($num, my $digit)= ($num / $radix, $num % $radix);
		$digits= chr(($digit < 10? 48 : 65) + $digit) . $digits;
	}
	my $pad= ($min_length||0) - length $digits;
	return $pad > 0? '0'x$pad . $digits : $digits;
}

sub fn_round {
	my ($num, $digits)= @_;
	use warnings FATAL => 'numeric';
	$digits= 0 unless defined $digits;
	sprintf("%.*lf", $digits, $num);
}

our $epsilon= 5e-14; # fudge factor for avoiding floating point rounding errors
sub fn_roundup {
	my ($num, $digits)= @_;
	use warnings FATAL => 'numeric';
	$digits= 0 unless defined $digits;
	return POSIX::ceil($num * 10.0**$digits - $epsilon) * 0.1**$digits;
}

sub fn_rounddown {
	my ($num, $digits)= @_;
	use warnings FATAL => 'numeric';
	$digits= 0 unless defined $digits;
	return POSIX::floor($num * 10.0**$digits + $epsilon) * 0.1**$digits;
}

=head2 String Functions

=over

=item upper( string )

Return uppercase version of STRING.

=item lower( string )

Return lowercase version of STRING.

=item substr( string, offset, length=max )

Same as perl's builtin.

=item concat, concatenate( string, ... )

Returns all arguments concatenated as a string

=item textjoin, join( separator, string, ... )

Same as perl's builtin.

=back

=cut

*fn_upper= *CORE::uc;
sub asperl_upper {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->asperl($_), @{$node->parameters};
	@arg_code == 1 or die "Function 'upper' can only take one argument\n";
	return "uc($arg_code[0])";
}

*fn_lower= *CORE::lc;
sub asperl_lower {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->asperl($_), @{$node->parameters};
	@arg_code == 1 or die "Function 'lower' can only take one argument\n";
	return "lc($arg_code[0])";
}

*fn_substr= *CORE::substr;

*fn_length= *CORE::length;

sub fn_concatenate {
	join '', @_;
}
*fn_concat= *fn_concatenate;

*fn_join= *CORE::join;

1;
