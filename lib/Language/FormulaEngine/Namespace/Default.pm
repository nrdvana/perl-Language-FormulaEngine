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
use Scalar::Util ();
use POSIX ();

*fn_sum= *List::Util::sum0;
sub perlgen_sum {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->perlgen($_), @{$node->parameters};
	return '( '.join(' + ', @arg_code).' )';
}

sub fn_negative {
	@_ == 1 or die "Can only negate a single value, not a list\n";
	return -$_[0];
}
sub perlgen_negative {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->perlgen($_), @{$node->parameters};
	@arg_code == 1 or die "Can only negate a single value, not a list\n";
	return '(-('.$arg_code[0].'))';
}

*fn_mul= *List::Util::product;
sub perlgen_mul {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->perlgen($_), @{$node->parameters};
	return '( '.join(' * ', @arg_code).' )';
}

sub fn_div {
	@_ == 2 or die "div() takes exactly two arguments\n";
	$_[1] or die "division by zero\n";
	return $_[0] / $_[1];
}
sub perlgen_div {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->perlgen($_), @{$node->parameters};
	return '( '.join(' / ', @arg_code).' )';
}

sub nodeval_if { # customize nodeval_ to provide lazy evaluation of arguments
	my ($self, $node)= @_;
	@{$node->parameters} == 3 or die "IF(test, when_true, when_false) requires all 3 parameters\n";
	my $bool= $node->parameters->[0]->evaluate($self);
	return $node->parameters->[$bool? 1 : 2]->evaluate($self);
}
sub perlgen_if {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->perlgen($_), @{$node->parameters};
	@arg_code == 3 or die "IF(test, when_true, when_false) requires all 3 parameters\n";
	return '( '.$arg_code[0].'? '.$arg_code[1].' : '.$arg_code[2].' )';
}

sub nodeval_and { # customize nodeval_ to provide lazy evaluation of arguments
	my ($self, $node)= @_;
	$_->evaluate($self) or return 0
		for @{ $node->parameters };
	return 1;
}
sub perlgen_and {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->perlgen($_), @{$node->parameters};
	return '( ('.join(' and ', @arg_code).')? 1 : 0)';
}

sub nodeval_or {
	my ($self, $node)= @_;
	$_->evaluate($self) and return 1
		for @{ $node->parameters };
	return 0;
}
sub perlgen_or {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->perlgen($_), @{$node->parameters};
	return '( ('.join(' or ', @arg_code).')? 1 : 0)';
}

sub fn_not {
	@_ == 1 or die "Too many arguments to 'not'\n";
	return $_[0]? 0 : 1;
}
sub perlgen_not {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->perlgen($_), @{$node->parameters};
	@arg_code == 1 or die "Too many arguments to 'not'\n";
	return '('.$arg_code[0].'? 0 : 1)';
}

sub fn_compare {
	my $left= shift;
	while (@_) {
		my $op= shift;
		my $right= shift;
		my $numeric= Scalar::Util::looks_like_number($left) && Scalar::Util::looks_like_number($right);
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

=head2 Utility Functions

=over

=item choose( offset, val1, val2, val3, ... )

Given a 1-based offset, return the value of the Nth parameter.

=back

=cut

sub fn_choose {
	$_[0] > 0 and $_[0] < @_ or die "CHOSE() selector out of bounds ($_[0])";
	return $_[$_[0]];
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

=item ceiling( number, step=1 )

Round a number up to the next multiple of C<step>.  If step is negative, this rounds away from
zero in the negative direction.

=item cos( angle )

Cosine of C<angle> in radians

=item cot( ratio )

Return the angle for the triangle ratio adjacent/opposite.

=item degrees( angle_in_radians )

Convert radians to degrees

=item exp( power )

Return base of the natural log raised to the specified power.

=item fact( n )

Compute factorial of C<n>.  (C<< 1 * 2 * 3 * ... n >>)

=item floor( number, step=1 )

Round a number down to the previous multiple of C<step>.  If step is negative, this rounds
toward zero in the positive direction.

=item round( number, digits=0 )

Round NUMBER to DIGITS decimal places of precision.  Uses the IEEE
5-round-to-even algorithm that C gives us.  DIGITS defaults to 0,
making it round to the nearest integer.

Dies if you attempt to round something that isn't a number.

=item roundup( number, digits=0 )

Like L</round>, but always round up.  See also L</ceiling>.

=item rounddown( number, digits=0 )

Like L</round>, but always round down.  See also L</floor>.

=back

=cut

*fn_abs= *CORE::abs;
*fn_acos= *Math::Trig::acos;
*fn_acot= *Math::Trig::acot;
*fn_asin= *Math::Trig::asin;
*fn_atan= *Math::Trig::atan;

sub fn_atan2 {
	# Perl differs in argument order from popular spreadsheet programs
	atan2($_[1], $_[0])
}

sub fn_average {
	List::Util::sum0(@_) / @_;
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

*fn_cos= *CORE::cos;
*fn_degrees= *Math::Trig::rad2deg;
*fn_exp= *CORE::exp;
sub fn_fact {
	my $n= int($_[0]);
	return 1 unless $n;
	$n > 0 or die "Can't compute factorial of negative number '$n'";
	List::Util::product(1 .. $n);
}
sub fn_round {
	my ($num, $digits)= @_;
	use warnings FATAL => 'numeric';
	my $scale= 0.1 ** ($_[1] || 0);
	return POSIX::round($num / $scale) * $scale;
}

our $epsilon= 5e-14; # fudge factor for avoiding floating point rounding errors
sub fn_ceiling {
	my ($num, $step)= @_;
	use warnings FATAL => 'numeric';
	$step= 1 unless defined $step;
	return POSIX::ceil($num / $step - $epsilon) * $step;
}

sub fn_floor {
	my ($num, $step)= @_;
	use warnings FATAL => 'numeric';
	$step= 1 unless defined $step;
	return POSIX::floor($num / $step + $epsilon) * $step;
}
sub fn_roundup {
	use warnings FATAL => 'numeric';
	fn_ceiling($_[0], 0.1 ** ($_[1] || 0));
}
sub fn_rounddown {
	use warnings FATAL => 'numeric';
	fn_floor($_[0], 0.1 ** ($_[1] || 0));
}
*fn_sin= *CORE::sin;

=head2 String Functions

=over

=item char( codepoint_value )

Return a unicode character.

=item clean( string )

Returns C<string> after removing all non-printable characters (defined as C<< [:^print:] >> )

=item code( string )

Opposite of L</char>, known as C<ord()> in other languages.  Returns the unicode codepoint
number of the first character of the string.

=item find( needle, haystack, from_offset=1 )

Return the character offset of C<needle> from start of C<haystack>, beginning the search at
from_offset.  All offsets are 1-based.

=item fixed( number, decimals=2, no_commas=false )

Return the number formatted with a fixed number of decimal places.  By default, it gets commas
added in the USA notation, but this can be disabled.

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

*fn_char= *CORE::chr;
sub fn_clean {
	my $str= shift;
	$str =~ s/[[:^print:]]+//g;
	$str;
}
*fn_code= *CORE::ord;
*fn_upper= *CORE::uc;
*fn_lower= *CORE::lc;
*fn_substr= *CORE::substr;
*fn_length= *CORE::length;

sub fn_concatenate {
	join '', @_;
}
*fn_concat= *fn_concatenate;
*fn_join= *CORE::join;

sub fn_find {
	my ($needle, $haystack, $ofs)= @_;
	$ofs= 1 unless $ofs && $ofs > 0;
	return index($haystack, $needle, $ofs) + 1;
}

sub fn_fixed {
	my ($number, $places, $comma)= @_;
	$places= 2 unless defined $places;
	$comma= ',' unless defined $comma && (!$comma or $comma eq '.');
	$number= $places > 0? sprintf("%.*f", $places, $number) : fn_round($number, $places);
	if ($comma) {
		$number =~ s/\./,/ if $comma eq '.';
		my $tmp= reverse substr($number, 0, $places > 0? -($places+1) : length $number);
		$tmp =~ s/(\d\d\d)(?=\d)/$1$comma/g;
		substr($number, 0, $places > 0? -($places+1) : length $number)= reverse $tmp;
	}
	return $number;
}

=head2 DateTime Functions

Date math is implemented using the L<DateTime> module.  Strings are coerced into dates using
the L<DateTime::Format::Flexible> module for any parameter where a spreadsheet function would
normally expect a date value.  "Since 1900" date serial numbers are not used at all.

=over

=item date( year, month, day )

Convert a (year,month,day) triplet into a date.

=item datedif( start_date, end_date, unit )

Calculate difference bwteen two dates.  Unit can be one of: C<"Y"> (whole years), C<"M"> (whole
months), C<"D"> (whole days).  Dates can be parsed from any string resembling a date.

=item day( start_date )

Returns the day number of a date

=item days( start_date, end_date )

Returns number of days difference between start and end date.

=item eomonth( start_date, months )

Calculate the date of End-Of-Month at some offset from the start date.

=back

=cut

sub _date {
	return $_[0] if ref($_[0])->isa('DateTime');
	DateTime::Format::Flexible->parse_datetime($_[0]) or die "Not a date: $_[0]\n"
}
sub fn_date {
	my ($y, $m, $d)= @_;
	DateTime->new(year => $y, month => $m, day => $d);
}

sub fn_datedif {
	my ($start, $end, $unit)= @_;
	$unit= uc($unit || '');
	if ($unit eq 'Y') { return _date($end)->delta_md(_date($start))->in_years }
	if ($unit eq 'M') { return _date($end)->delta_md(_date($start))->in_months }
	if ($unit eq 'D') { return _date($end)->delta_days(_date($start))->in_days }
	die "Unsupported datedif unit '$unit'\n";
}
sub fn_day {
	_date($_[0])->day
}
sub fn_days {
	my ($start, $end)= @_;
	return _date($end)->delta_days(_date($start))->in_days
}
sub fn_eomonth {
	my ($start, $end)= @_;
	_date($start)->clone->add(months => $end+1)->truncate(to => 'month')->subtract(days => 1);
}

1;
