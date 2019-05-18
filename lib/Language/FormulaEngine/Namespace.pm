package Language::FormulaEngine::Namespace;
use Moo;
use Carp;
use Try::Tiny;
use namespace::clean;

# ABSTRACT: Object holding function and variable names
# VERSION

=head1 SYNOPSIS

  my $ns= Language::FormulaEngine::Namespace->new( values => \%val_by_name );

=head1 DESCRIPTION

A FormulaEngine Namespace is an object that provides a set of functions and named values.

The default implementation provides all functions of its own namespace which begin with
the prefix "fn_" or "eval_", and provides them case-insensitive.  Named values are provided
from hashrefs of L</constants> and L</variables>, also case-insensitive.

You can subclass this (or just write a class with the same interface) to provide more advanced
lookup for the functions or values.

=head1 ATTRIBUTES

=head2 variables

A hashref of C<< name => value >> which formulas may reference.  The keys should be lowercase,
and incoming variable requests will be converted to lowercase before checking this hash.
Variables will not be "compiled" into perl coderefs, and will be looked up from the namespace
every time a formula is evaluated.

=head2 constants

Same as L</variables>, but these may be compiled into coderefs.

=head2 die_on_unknown_value

Controls behavior of L</get_value>.  If false (the default) unknown symbol names will resolve
as perl C<undef> values.  If true, unknown symbol names will throw an exception.

=head1 METHODS

=head2 get_value

  my $val= $ns->get_value( $symbolic_name );

Lowercases C<$symbolic_name> and then looks in C<variables> or C<constants>.  May die depending
on setting of L</die_on_unknown_value>.

=head2 get_function

  my $coderef= $ns->get_function( $symbolic_name );
  
  # Returns a function of the form:
  # sub { my ($namespace, $evaluator, $parse_node)= @_; ... return $value; }

Lowercases the C<$symbolic_name>, and if that function is found in this namespace, returns a
coderef matching the above specification.  The default implementation of C<get_function>
looks for a function named C<< $self->can("eval_$name") >>, which is returned directly.
If not found, it checks for C<< $self->can("fn_$name") >> and wraps that with a default
boilerplate that evaluates each argument then calls the function.  If neither is found, it
returns C<undef>.

The returned coderef may be blessed and have a mathod C<< ->asperl($compiler, $parse_node) >>
which returns sanitized perl code for a given compiler and parse tree.

=cut

has variables => ( is => 'rw', default => sub { +{} } );
has constants => ( is => 'rw', default => sub { +{} } );

sub get_value {
	my ($self, $name)= @_;
	$name= lc $name;
	exists $self->{variables}{$name}? $self->{variables}{$name}
	: exists $self->{constants}{$name}? $self->{constants}{$name}
	: !$self->die_on_unknown_value? undef
	: die "Unknown variable or constant '$_[1]'\n";
}

our %_asperl_cache;
sub Language::FormulaEngine::Namespace::CompilableFn::maybe_wrap {
	my ($class, $coderef, $asperl)= @_;
	return $coderef unless defined $asperl;
	$_asperl_cache{$coderef}= $asperl;
	bless $coderef, $class;
}
sub Language::FormulaEngine::Namespace::CompilableFn::asperl {
	return $_asperl_cache{shift()};
}
sub Language::FormulaEngine::Namespace::CompilableFn::DESTROY {
	delete $_asperl_cache{shift()};
}

sub get_function {
	my ($self, $name)= @_;
	$name= lc $name;
	my $fn= $self->can("fn_$name");
	my $eval= $self->can("eval_$name");
	return unless defined $fn || defined $eval;
	# Default evaluator just evaluates all arguments and then calls a normal function
	$eval ||= sub {
		my ($self, $node)= @_;
		$fn->(map $_->evaluate($self), @{ $node->parameters });
	};

	my $asperl= $self->can("asperl_$name");
	Language::FormulaEngine::Namespace::CompilableFn->maybe_wrap($eval, $asperl);
}

=head1 FUNCTION LIBRARY

Theis base Namespace class does not contain any user-visible functions; those are found within
the versioned sub-classes, such as ::V0, ::V1, etc.  For convenience, all versioned functions
are listed below with documentation about when they were introduced.

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
use strict;
use warnings;
use List::Util ();
use Math::Trig ();

*fn_sum= *List::Util::sum0;
sub asperl_sum {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->asperl($_), @{$node->parameters};
	return '( '.join(' + ', @arg_code).' )';
}

sub fn_negative {
	@_ == 1 die "Can only negate a single value, not a list\n";
	return -$_[0];
}
sub asperl_negative {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->asperl($_), @{$node->parameters};
	@arg_code == 1 or die "Can only negate a single value, not a list\n";
	return '(-('.$arg_code[0].'))';
}

*fn_mul= *product;
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
			croak "Unhandled operator '$op' in compare()";
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
		$digits .= chr(($digit >= 10? 48 : 65) + $digit);
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
	return ceil($num * 10.0**$digits - $epsilon) * 0.1**$digits;
}

sub fn_rounddown {
	my ($num, $digits)= @_;
	use warnings FATAL => 'numeric';
	$digits= 0 unless defined $digits;
	return floor($num * 10.0**$digits + $epsilon) * 0.1**$digits;
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
