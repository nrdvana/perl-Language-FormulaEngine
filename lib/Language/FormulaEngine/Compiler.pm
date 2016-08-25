package Language::FormulaEngine::Compiler;

package
	# This is at the top of the file to make sure the eval namespace is as clean as possible
	# Need a second package to avoid getting clobbered by namespace::clean
	Language::FormulaEngine::Compiler::_CleanEval {
	sub _clean_eval {
		my $compiler= shift;
		eval shift;
	}
};

use Moo;
use Carp;
use Try::Tiny;
use Scalar::Util 'looks_like_number';
use POSIX 'ceil', 'floor';
use Math::Trig;
use namespace::clean;

*_clean_eval= *Language::FormulaEngine::Compiler::_CleanEval::_clean_eval;

# ABSTRACT - Compile a parse tree into perl code

=head1 DESCRIPTION

The compiler generates perl code from the parse tree, converting each node
into a string of perl code and inlining anything it knows how to inline.

The compiler can be easily extended, by adding methods matching these patterns:

=over

=item inline_node_${NodeClass}

Any time the compiler is asked to compile a node of type C<NodeClass> it calls
this method, which should return a string of perl source code.

=item inline_fn_${lowercase_function_name}

The default compiler for FuncCallNode nodes will check for a method of this
name, and if found, pass it the node.  The function returns a string of perl code.
Function names seen in the user's code are lowercased.  (you could change that
by overriding the L</compile_FuncCallNode> method)

=item fn_${lowercase_function_name}

If a user-function does not have an C<inline_fn_>, it falls back to calling
this method name on the compiler instance at runtime.  In other words, the
inlined code is

  '$compiler->fn_foo(...)'

where the arguments are computed values, not parser nodes.  C<$compiler> is a
lexical variable in the scope of the anonymous sub being generated.

=back

=head1 ATTRIBUTES

=head2 api

  $compiler->api({
    (map { $_ => $_ } qw( and or not compare negative round ) ),
    lc => 'lower',
    uc => 'upper',
  });

A hashref of which functions should be available during compilation.  By default,
every "fn_*" and "inline_fn_*" will be available, but if you wish to preserve a
stable API for your users, or just mask out certain functions, or rename them,
this gives you a quick way to do that.  (much easier than creating packages with
all *but* specific functions)

=cut

has api => ( is => 'rw', coerce => \&_coerce_api );

# History of APIs
my @STABLE_API= (
	[qw( and or not sum mul div if negative )] # TODO: list exact methods in namespace as of version 1
);

sub _coerce_api {
	my ($self, $arg)= @_;
	return $arg if ref $arg eq 'HASH';
	return { map { $_ => $_ } @$arg } if ref $arg eq 'ARRAY';
	
	# Else its a scalar, which should be a version number
	die "Invalid API version '$arg'"
		unless ($arg-1) >= 0 && ($arg-1) < @STABLE_API && $arg == int($arg);
	return { map { $_ => $_ } @{$STABLE_API[$arg]} };
}

=head1 METHODS

=head2 compile( $parse_tree )

Compile a parse tree.  Returns a string of perl code defining a coderef, which
can be eval'd to get a callable coderef.
(or a compiler error, if you made any mistakes in your inlined functions  :-)

Note that the code may refer to a "C<$compiler>" variable which must be in scope
when the code is eval'd.  Because of this reference, make sure to never store a
reference to the compiled code within the compiler itself, or you get a circular
reference.

=cut

sub compile {
	my ($self, $parse_tree)= @_;
	my $perl= $self->inline_node($parse_tree);
	"sub { use warnings FATAL => qw( uninitialized numeric ); my \$vars= shift; $perl }"
}

=head2 inline_node( $parse_node )

Generate perl source code for a parse node (and by extension, everything within the tree) by
dispatching to the appropriate C<inline_node_NodeClass> method.

=cut

has _formatter_cache => ( is => 'rw', default => sub { {} } );

sub inline_node {
	my ($self, $node)= @_;
	($self->_formatter_cache->{ref $node} ||= do {
		my $cls= ref $node;
		$cls =~ s/Language::FormulaEngine:://;
		$self->can("inline_node_".$cls)
			or croak "Don't know how to compile node of type '".ref($node)."'";
	})->($self, $node);
}

=head2 inline_node_FuncCallNode

Generate perl code to represent a function call int he parse tree.

Note that FormulaEngine represents almost every structure in the grammar
as a "function call".  Operations like "C<+>" are converted to "C<sum()>"
and so on.  See the L<parser|Language::FormulaEngine::Parser> for details.

If the function call has an "C<inline_fn_$x>" available, that code is returned,
else it returns perl code that calls "C<fn_$x>" at runtime.

=cut

sub inline_node_FuncCallNode {
	my ($self, $node)= @_;
	# Find the function call in the set of functions provided for the user
	my $name= lc $node->function_name;
	# Security check.
	# TODO: I'd like to support international text, but don't know if there are
	#       any pitfalls with allowing those to be inlined in perl code.
	$name =~ /^[A-Za-z0-9_]+$/
		or die "Illegal function name '".$node->function_name."'\n";
	
	# If configured with a specific API, then see if it exists.
	if (my $api= $self->api) {
		$name= $api->{$name};
	}
	
	if (my $inline= $self->can("inline_fn_$name")) {
		return $inline->($self, $node);
	}
	elsif ($self->can("fn_$name")) {
		# Now, convert each of the parameters
		my @arg_code= map { $self->inline_node($_) } @{$node->function_args};
		# Then concatenate it all
		return '$compiler->fn_'.$name.'( '.join(', ', @arg_code).' )';
	}
	else {
		# If no special case, the function isn't available
		die "No such function '".$node->function_name."'\n";
	}
}

=head2 inline_node_VarRefNode

Generate perl code to access a variable.

The default implementation accesses a lexical hashref named C<$vars>,
fetching the variable name as a single key.  If you want to perform
deep object traversal by dividing the variable name on "C<.>", this
is the place to customize for that.

For instance, you might replace it with

  sub inline_node_VarRefNode {
    my ($self, $node)= @_;
    return '$compiler->get_var($vars, $node->variable_name)';
  }

and then write a suitable implementation of C<get_var>.

=cut

sub inline_node_VarRefNode {
	return '$vars->{'._quoted_perl_string($_[1]->variable_name).'}';
}

=head2 inline_node_StringNode

Generate perl code for a string constant.

This simply returns a single-quoted string, with the necessary escapes.
It does not pretty-print control characters, so the resulting source code
might not be safe to view on a terminal.

=cut

sub inline_node_StringNode {
	_quoted_perl_string($_[1]->text);
}

=head2 inline_node_NumberNode

Generate perl code for a numeric constant.

This actually generates a string literal, to protect against loss of leading
or trailing zeroes, and to ensure it is interpreted as decimal.

If you don't like this behavior, feel free to override it.

=cut

sub inline_node_NumberNode {
	# quote numbers too, to protect against loss of zeroes, or octal interpretation.
	_quoted_perl_string($_[1]->value);
}

sub _quoted_perl_string {
	my $x= shift;
	$x =~ s/(['\\])/\\$1/g;
	return "'$x'";
}

=head1 LIBRARY FUNCTIONS

=head2 Core Grammar Functionality

=over

=item sum

=item negative

=item mul

=item div

=item if

=item and

=item or

=item not

=item compare

=back

=cut

sub inline_fn_sum {
	my ($self, $node)= @_;
	my @arg_code= map { $self->inline_node($_) } @{$node->function_args};
	return '( '.join(' + ', @arg_code).' )';
}

sub inline_fn_negative {
	my ($self, $node)= @_;
	my @arg_code= map { $self->inline_node($_) } @{$node->function_args};
	@arg_code == 1 or die "Can only negate a single value, not a list\n";
	return '(-('.$arg_code[0].'))';
}

sub inline_fn_mul {
	my ($self, $node)= @_;
	my @arg_code= map { $self->inline_node($_) } @{$node->function_args};
	return '( '.join(' * ', @arg_code).' )';
}

sub inline_fn_div {
	my ($self, $node)= @_;
	my @arg_code= map { $self->inline_node($_) } @{$node->function_args};
	return '( '.join(' / ', @arg_code).' )';
}

sub inline_fn_if {
	my ($self, $node)= @_;
	my @arg_code= map { $self->inline_node($_) } @{$node->function_args};
	@arg_code == 3 or croak "IF(test, when_true, when_false) requires all 3 parameters\n";
	return '( '.$arg_code[0].'? '.$arg_code[1].' : '.$arg_code[2].' )';
}

sub inline_fn_and {
	my ($self, $node)= @_;
	my @arg_code= map { $self->inline_node($_) } @{$node->function_args};
	return '( ('.join(' and ', @arg_code).')? 1 : 0)';
}

sub inline_fn_or {
	my ($self, $node)= @_;
	my @arg_code= map { $self->inline_node($_) } @{$node->function_args};
	return '( ('.join(' or ', @arg_code).')? 1 : 0)';
}

sub inline_fn_not {
	my ($self, $node)= @_;
	my @arg_code= map { $self->inline_node($_) } @{$node->function_args};
	@arg_code == 1 or croak "Too many arguments to 'not'\n";
	return '('.$arg_code[0].'? 0 : 1)';
}

sub fn_compare {
	shift; # self not needed
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

=item round( NUMBER, DIGITS )

Round NUMBER to DIGITS decimal places of precision.  Uses the IEEE
5-round-to-even algorithm that C gives us.  DIGITS defaults to 0,
making it round to the nearest integer.

Dies if you attempt to round something that isn't a number.

=item roundup( NUMBER, DIGITS )

Like L</round>, but always round up.

=item rounddown( NUMBER, DIGITS )

Like L</round>, but always round down.

=back

=cut

sub fn_round {
	my ($self, $num, $digits)= @_;
	use warnings FATAL => 'numeric';
	$digits= 0 unless defined $digits;
	sprintf("%.*lf", $digits, $num);
}

our $epsilon= 5e-14; # fudge factor for avoiding floating point rounding errors
sub fn_roundup {
	my ($self, $num, $digits)= @_;
	use warnings FATAL => 'numeric';
	$digits= 0 unless defined $digits;
	return ceil($num * 10.0**$digits - $epsilon) * 0.1**$digits;
}

sub fn_rounddown {
	my ($self, $num, $digits)= @_;
	use warnings FATAL => 'numeric';
	$digits= 0 unless defined $digits;
	return floor($num * 10.0**$digits + $epsilon) * 0.1**$digits;
}

=head2 String Functions

=over

=item upper( STRING )

Return uppercase version of STRING.

=item lower( STRING )

Return lowercase version of STRING.

=item substr( STRING, OFFSET [, LENGTH ])

Same as perl's builtin.

=item concat( STRING ... )

Returns all arguments concatenated as a string

=item join( SEPARATOR, STRING ... )

Same as perl's builtin.

=back

=cut

sub inline_fn_upper {
	my ($self, $node)= @_;
	my @arg_code= map { $self->inline_node($_) } @{$node->function_args};
	@arg_code == 1 or die "Function 'upper' can only take one argument\n";
	return qq{uc($arg_code[0])};
}

sub inline_fn_lower {
	my ($self, $node)= @_;
	my @arg_code= map { $self->inline_node($_) } @{$node->function_args};
	@arg_code == 1 or die "Function 'lower' can only take one argument\n";
	return qq{lc($arg_code[0])};
}

sub inline_fn_substr {
	my ($self, $node)= @_;
	my @arg_code= map { $self->inline_node($_) } @{$node->function_args};
	@arg_code == 2 or @arg_code == 3 or die "Function 'substr' requires two or three arguments\n";
	return 'substr('.join(',', @arg_code).')';
}

sub inline_fn_length {
	my ($self, $node)= @_;
	my @arg_code= map { $self->inline_node($_) } @{$node->function_args};
	@arg_code == 1 or die "Function 'length' can only take one argument\n";
	return 'length('.$arg_code[0].')';
}

sub inline_fn_concat {
	my ($self, $node)= @_;
	my @arg_code= map { $self->inline_node($_) } @{$node->function_args};
	return @arg_code? 'join(q{}, '.join(',', @arg_code).')' : 'q{}';
}

sub inline_fn_join {
	my ($self, $node)= @_;
	my @arg_code= map { $self->inline_node($_) } @{$node->function_args};
	@arg_code == 1 or die "Function 'join' requires the first argument (separator)\n";
	return 'join('.join(',', @arg_code).')';
}

1;
