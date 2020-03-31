package Language::FormulaEngine::Namespace::JSDefault;
use Moo;
extends 'Language::FormulaEngine::Namespace';
use strict;
use Try::Tiny;
use List::Util ();
use Math::Trig ();
use Scalar::Util ();
use Language::FormulaEngine::Error ':all';
use namespace::clean;

# ABSTRACT: JavaScript implementation and generators for functions of default namespace
# VERSION

=head1 DESCRIPTION

=cut

sub _collect_function_info {
	my ($self, $name)= @_;
	my $js= $self->can("js_$name");
	my $gen= $self->can("jsgen_$name");
	return
		($js? ( js => $js ) : ()),
		($gen? ( js_generator => $gen ) : ()),
		$self->maybe::next::method($name);
}

=head2 Core Grammar Functionality

These are the methods that implement the infix operators.

=over

=item C<< sum( num1, num2 ... ) >>

=item C<< negative( num1 ) >>

=item C<< mul( num1, num2, ... ) >>

=item C<< div( numerator, denominator ) >>

=item C<< and( bool1, bool2, ... ) >>

This applies perl-ish boolean semantics to each argument, and returns a numeric 0 or 1.
No arguments are evaluated after the first false value.

=item C<< or( bool1, bool2, ... ) >>

This applies perl-ish boolean semantics to each argument, and returns a numeric 0 or 1.
No arguments are evaluated after the first true value.

=item C<< not( bool1 ) >>

This applies perl-ish boolean semantics to the argument and returns numeric 1 or 0.

=item C<< compare( val1, op, val2, ...op, val ) >>

This compares two or more values against the 6 canonical operators
C<< "<", "<=", ">", ">=", "==", "!=" >> and returns 0 or 1.

It uses numeric comparison if both sides of an operator C<looks_like_number>, and uses string
comparison otherwise.

=back

=cut

sub js_sum {
	'var t=0;for(var i=0;i<arguments.length;i++){t+=arguments[i]}'
	.'return t;'
}
sub jsgen_sum {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->jsgen($_), @{$node->parameters};
	return '('.join('+', @arg_code).')';
}

sub js_negative {
	'if(arguments.length>0){throw "Can only negate a single value, not a list"}'
	.'return -arguments[0];'
}
sub jsgen_negative {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->jsgen($_), @{$node->parameters};
	@arg_code == 1 or die "Can only negate a single value, not a list\n";
	return '(-('.$arg_code[0].'))';
}

sub js_mul {
	'var t=1;for(var i=0;i<arguments.length;i++){t*=arguments[i]}'
	.'return t;'
}
sub jsgen_mul {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->jsgen($_), @{$node->parameters};
	return '('.join('*', @arg_code).')';
}

sub js_div {
	'var t=arguments[0];for(var i=1;i<arguments.length;i++){t/=arguments[i]}'
	.'return t'
}
sub jsgen_div {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->jsgen($_), @{$node->parameters};
	return '('.join('/', @arg_code).')';
}

sub js_and {
	'for(var i=0;i<arguments.length;i++){if(!arguments[i])return 0}'
	.'return 1'
}
sub jsgen_and {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->jsgen($_), @{$node->parameters};
	return '(('.join('&&', @arg_code).')?1:0)';
}

sub js_or {
	'for(var i=0;i<arguments.length;i++){if(arguments[i])return 1}'
	.'return 0'
}
sub jsgen_or {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->jsgen($_), @{$node->parameters};
	return '(('.join('||', @arg_code).')?1:0)';
}

sub js_not {
	q|if(arguments.length!=1){throw "Too many arguments to 'not'"}|
	.'return arguments[0]?0:1'
}
sub jsgen_not {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->jsgen($_), @{$node->parameters};
	@arg_code == 1 or die "Too many arguments to 'not'\n";
	return '('.$arg_code[0].'?0:1)';
}

sub js_compare {
	'var left=arguments[0],i=1;'
	# In Excel, numbers always compare less than strings.  Add a prefix to enforce consistent comparisons.
	.'left=(typeof left=="number"?"a":"b")+left;'
	.'while(i<arguments.length){'
	.  'var op=arguments[i++],right=arguments[i++];'
	.  'right=(typeof right=="number"?"a":"b")+right;'
	.  'if(op=="=="){if(left!=right)return 0}'
	.  'else if(op=="!="){if(left==right)return 0}'
	.  'else if(op==">="){if(left<right)return 0}'
	.  'else if(op=="<="){if(left>right)return 0}'
	.  'else if(op==">"){if(left<=right)return 0}'
	.  'else if(op=="<"){if(left>=right)return 0}'
	.  'else{throw "Unhandled operator "+op+"in compare()"}'
	.  'left=right'
	.'}'
	.'return 1'
}

=head2 Utility Functions

=over

=item C<< choose( offset, val1, val2, val3, ... ) >>

Given a 1-based offset, return the value of the Nth parameter.

=item C<< if( condition, val_if_true, val_if_false ) >>

If C<condition> is "true" (Perl interpretation) return C<val_if_true>, else C<val_if_false>.

=item C<< iferror( value_maybe_error, alternate_value ) >>

If C<value_maybe_error> does not throw an exception, return it, else return the
C<alternate_value>.

=item C<< ifs( condition1, value1, condition2, value2, ... ) >>

A simplified sequence of IF functions.  If C<condition1> is true, it returns C<value1>, else if
C<condition2> is true it returns C<value2>, and so on.  If no condition is true it dies.  (use
a final true condition and value to provide a default)

=item C<< na() >>

Throw an NA exception.

=back

=cut

sub js_choose {
	'if(!(arguments.length>0&&arguments[0]>0&&arguments[0]<arguments.length){throw "CHOSE() selector out of bounds ("+arguments[0]+")"}'
	.'return arguments[arguments[0]];'
}

sub js_if {
	'return arguments[arguments[0]?1:2]'
}
sub jsgen_if {
	my ($self, $compiler, $node)= @_;
	my @arg_code= map $compiler->jsgen($_), @{$node->parameters};
	@arg_code == 3 or die "IF(test, when_true, when_false) requires all 3 parameters\n";
	return '('.$arg_code[0].'?'.$arg_code[1].':'.$arg_code[2].')';
}

1;
