package Language::FormulaEngine::Parser;
use Moo;
use Carp;
use Try::Tiny;
use List::Util qw( min max );
use Log::Any '$log';
use namespace::clean;

our $VERSION= '0.00_1';

# ABSTRACT - Create parse tree from scanner tokens

=head1 SYNOPSIS

  my $scanner= Language::FormulaEngine::Scanner->new(input => $string);
  my $parser= Language::FormulaEngine::Parser->new();
  $parser->parse($scanner);
  
  # To subclass:
  package Foo {
    use Moo;
    extends 'Language::FormulaEngine::Parser';
    
    # for changes to parser, override method corresponding to parse rule
    sub parse_..._expr { ... }
  }

=head1 DESCRIPTION

This class parses tokens from a scanner into a parse tree of
Language::FormulaEngine::Parser::Node objects.

=head1 ATTRIBUTES

=head2 scanner

This is automatically assigned during the call to L</parse>.
The only real point of retaining the scanner object after a parse is if
it failed and you want to inspect the scanner's input context.

=head2 parse_tree

After a successful call to L</parse>, this contains the resulting parse tree.

=head2 functions

After a successful call to L</parse>, this contains the set of functions
referenced by any of the parse nodes.  You can use this to quickly determine
whether all functions in the formula are supported by your compilr object.

=head2 variables

After a successful call to L</parse>, this contains the set of symbolic values
which were referenced by any parse rule.

=cut

has scanner      => ( is => 'rw' );
has parse_tree   => ( is => 'rw' );
has functions    => ( is => 'rw' );
has variables    => ( is => 'rw' );

=head1 METHODS

=head2 parse

  $parse_tree= $parser->parse( $input );

Read tokens from C<$input> to build a parse tree.  Returns the parse tree, which is
composed of L</"Parse Nodes">.

Input can be a L<Scanner|Language::FormulaEngine::Scanner> object, or a plain scalar
which gets passed to L</new_scanner>.

Most nodes of the parse tree are function call nodes, because basic operations
like C<x*y> are converted to a function as C<mul(x,y)>.

=cut

sub parse {
	my ($self, $scanner)= @_;
	$self->functions({});
	$self->variables({});
	$self->parse_tree(undef);
	$scanner= $self->new_scanner($scanner)
		if !ref $scanner;
	$self->scanner($scanner);
	$self->_next_token;
	$self->parse_tree($self->parse_expr);
	# It is an error if there was un-processed input.
	$self->{_token_type} eq 'eof'
		or die "Unexpected '$self->{_token_value}' near \"".$self->scanner->token_context."\"\n";

	return $self->parse_tree;
}

=head2 new_scanner

  my $scanner= $parser->new_scanner( $input );

The scanner is a separate object, but the parser almost always needs paired with
a specific scanner class, so this provides an easy linkage to create the appropriate
scanner object for any given parser.

=cut

sub new_scanner {
	my ($self, $input)= @_;
	require Language::FormulaEngine::Scanner;
	return Language::FormulaEngine::Scanner->new(input => $input);
}

sub _next_token {
	my $self= shift;
	@{$self}{'_token_type','_token_value'}= $self->scanner->next_token;
}

my %_str_escapes= ("\0" => '\0', "\n" => '\n', "\r" => '\r', "\t" => '\t', "\f" => '\f', "\b" => '\b', "\a" => '\a', "\e" => '\e', "\\" => '\\' );
sub str_escape_char { exists $_str_escapes{$_[0]}? $_str_escapes{$_[0]} : sprintf((ord $_[0] <= 0xFF)? "\\x%02X" : "\\x{%X}", ord $_[0]); }
sub str_escape { my $str= shift; $str =~ s/([^\x20-\x7E])/str_escape_char($1)/eg; $str; }

=head2 Grammar

The grammar of the default parser is as follows:

  expr      ::= or_expr
  or_expr   ::= and_expr ( 'or' and_expr )*
  and_expr  ::= not_expr ( 'and' not_expr )*
  not_expr  ::= ( 'not' | '!' ) cmp_expr | cmp_expr
  cmp_expr  ::= sum_expr ( ( '=' | '==' | '<>' | '\u2260' | '<' | '<=' | '>' | '>=' ) sum_expr )*
  sum_expr  ::= prod_expr ( ('+' | '-') prod_expr )*
  prod_expr ::= ( unit_expr ('*' | '/') )* unit_expr
  unit_expr ::= '-' unit_expr | ident '(' list ')' | '(' (expr|list) ')' | ident | num | str
  list      ::= expr ( ',' expr )* ','?

C<ident>, C<num>, C<str>, and all the punctuation symbols are tokens that come from the scanner.

These are implemented as function calls which consume tokens via L</next_token>,
and return parse nodes:

=over

=item parse_expr

=item parse_or_expr

=item parse_and_expr

=item parse_not_expr

=item parse_cmp_expr

=item parse_sum_expr

=item parse_prod_expr

=item parse_unit_expr

=item parse_list

=back

=cut

sub parse_expr { shift->parse_or_expr; }

sub parse_or_expr {
	my $self= shift;
	my $first= $self->parse_and_expr;
	return $first unless $self->{_token_type} eq 'or';
	my @or_expr= $first;
	while ($self->{_token_type} eq 'or') {
		$self->_next_token;
		push @or_expr, $self->parse_and_expr;
	}
	return $self->new_call('or', \@or_expr);
}

sub parse_and_expr {
	my $self= shift;
	my $first= $self->parse_not_expr;
	return $first unless $self->{_token_type} eq 'and';
	my @and_expr= $first;
	while ($self->{_token_type} eq 'and') {
		$self->_next_token;
		push @and_expr, $self->parse_not_expr;
	}
	return $self->new_call('and', \@and_expr);
}

sub parse_not_expr {
	my $self= shift;
	if ($self->{_token_type} eq 'not' or $self->{_token_type} eq '!') {
		$self->_next_token;
		return $self->new_call('not', [ $self->parse_cmp_expr ]);
	}
	return $self->parse_cmp_expr;
}

my %_cmp_ops= map { $_ => 1 } qw( > < >= <= != == );
sub parse_cmp_expr {
	my $self= shift;
	my $first= $self->parse_sum_expr;
	return $first unless $_cmp_ops{$self->{_token_type}};
	my @expr= $first;
	while ($_cmp_ops{$self->{_token_type}}) {
		push @expr, $self->new_string($self->{_token_type});
		$self->_next_token;
		push @expr, $self->parse_sum_expr;
	}
	return $self->new_call('compare', \@expr);
}

sub parse_sum_expr {
	my $self= shift;
	my $first= $self->parse_prod_expr;
	return $first unless $self->{_token_type} eq '+' or $self->{_token_type} eq '-';
	my @sum_expr= $first;
	while ($self->{_token_type} eq '+' or $self->{_token_type} eq '-') {
		my $negate= $self->scanner->consume_token eq '-';
		my $operand= $self->parse_prod_expr;
		push @sum_expr, $negate? $operand->get_negative : $operand;
	}
	return $self->new_call('sum', \@sum_expr);
}

sub parse_prod_expr {
	my $self= shift;
	my $value= $self->parse_unit_expr;
	while ($self->{_token_type} eq '*' or $self->{_token_type} eq '/') {
		my $op= $self->scanner->consume_token;
		my $right= $self->parse_unit_expr;
		$value= $self->new_call( $op eq '*'? 'mul' : 'div', [ $value, $right ] );
	}
	return $value;
}

sub parse_unit_expr {
	my $self= shift;
	my $negate= 0;
	my $expr;

	if ($self->{_token_type} eq '-') {
		$self->_next_token;
		return $self->parse_unit_expr->get_negative;
	}

	if ($self->{_token_type} eq '(') {
		$self->_next_token;
		my $args= $self->parse_list;
		die "Expected ')' near \"".$self->scanner->token_context."\"\n"
			if $self->{_token_type} ne ')';
		die "Expected expression before ')' near \"".$self->scanner->token_context."\"\n"
			unless @$args;
		$self->_next_token;
		return @$args > 1? $self->new_call('list', $args) : $args->[0];
	}
	
	if ($self->{_token_type} eq 'num') {
		return $self->new_number($self->consume_token);
	}
	
	if ($self->{_token_type} eq 'str') {
		return $self->new_string($self->consume_token);
	}

	if ($self->{_token_type} eq 'ident') {
		my $id= $self->scanner->consume_token;
		if ($self->{_token_type} eq '(') {
			$self->_next_token;
			my $args= $self->parse_list;
			die "Expected ')' near \"".$self->scanner->token_context."\"\n"
				if $self->{_token_type} ne ')';
			$self->scanner->consume_token;
			return $self->new_call($id, $args);
		}
		else {
			return $self->new_variable($id);
		}
	}
	
	die "Unexpected '".$self->{_token_value}."' near \"".$self->scanner->token_context."\"\n";
}

sub parse_list {
	my $self= shift;
	my @args= $self->parse_expr;
	while ($self->{token_type} eq ',') {
		$self->consume_token;
		push @args, $self->parse_expr;
	}
	return \@args;
}

=head2 Parse Nodes

There are classes for each of the types of nodes that the parser creates.
These classes are not based on hashrefs or Moo, since they are intended to
be extremely light-weight.  There is a method to construct each type of node,
rather than using a constructor on the class.  If you subclass the parser you
are encouraged to create your own node classes rather than sub-classing
existing ones, and then you can simply override the C<new_*> methods to get
the existing code to use your objects.  These node classes are also defined
as part of the Parser package rather than a standalone C<Parser/Node.pm>.

Each node has a method "to_canonical" which renders the node back to source code.

As a special case for removing redundant negative signs and operations, each
node supports the method "get_negative" which returns another node which is the
simplest representation of "-($node)".

=over

=item Node

This is the base class for nodes, and its C<get_negative> returns a function
of C<FuncCall( 'negative', $node )>.

=cut 

{ package Language::FormulaEngine::Parser::Node;
	sub get_negative {
		my $self= shift;
		bless [ 'negative', [ $self ] ], 'Language::FormulaEngine::Node::FuncCall';
	}
}

=item Call, new_call

Represents a call to a named subroutine (function).
This is the most common kind of node, as most constructs in this grammar are
treated as named functions.  It has attributes of C<fn_name> and C<args>.

  $node= $parser->new_call( $fn_name, \@arg_nodes );

Calling C<get_negative> on a function named 'negative' will simply un-wrap the arguments.

=cut

{ package Language::FormulaEngine::Parser::Node::Call;
	our @ISA= 'Language::FormulaEngine::Parser::Node';

	sub fn_name { $_[0][0] }
	sub args    { $_[0][1] }

	sub get_negative {
		my $self= shift;
		if ($self->fn_name eq 'negative') {
			# the negative of a negative is the original
			return $_[0]->args->[0];
		}
		$self->SUPER::get_negative();
	}

	sub to_canonical {
		my $self= shift;
		return $self->fn_name . '( ' .join(', ', map { $_->to_canonical } @{$self->args}). ' )';
	}
}

sub new_call {
	my ($self, $fn, $args)= @_;
	$self->functions->{$fn}++; # record dependency on this function
	bless [ $fn, $args ], 'Language::FormulaEngine::Parser::Node::Call';
}

=item Variable, new_variable

A reference to a symbolic variable (or constant).
It has an attribute C<symbol>.

  $node= $parser->new_variable($symbol);

=cut

{ package Language::FormulaEngine::Parser::Node::Variable;
	our @ISA= 'Language::FormulaEngine::Parser::Node';

	sub symbol { ${$_[0]} }

	sub to_canonical { $_[0]->symbol }
}

sub new_variable  {
	my ($self, $symbol)= @_;
	$self->variables->{$symbol}++; # record dependency on this variable
	bless \$symbol, 'Language::FormulaEngine::Parser::Node::Variable'
}

=item String, new_string

A string literal.  It has an attribute C<text> holding the raw value.
Call C<to_canonical> to see it escaped with backslashes.

=cut

{ package Language::FormulaEngine::Parser::Node::String;
	our @ISA= 'Language::FormulaEngine::Parser::Node';

	sub text { ${$_[0]} }

	sub to_canonical {
		return Language::FormulaEngine::Parser::str_escape(${$_[0]});
	}
}

sub new_string {
	my ($self, $text)= @_;
	bless \$text, 'Language::FormulaEngine::Parser::Node::String'
}

=item Number, new_number

A numeric literal, with attribute C<value> holding the numeric value.
No translation is performed by the default implementation of C<new_number>;
this is the method to override if you want to support octal or something.

=cut

{ package Language::FormulaEngine::Parser::Node::Number;
	our @ISA= 'Language::FormulaEngine::Parser::Node';

	sub value { ${$_[0]} }

	sub get_negative { # for numbers, we return a new number with the sign flipped
		my ($self)= @_;
		my $ret= -$self->value;
		return bless \$ret, ref($self);
	}

	sub to_canonical {
		$_[0]->value
	}
}

sub new_number {
	my ($self, $value)= @_;
	bless \$value, 'Language::FormulaEngine::Parser::Node::Number'
}

=back

=cut

1;
