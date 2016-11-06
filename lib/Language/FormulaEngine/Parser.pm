package Language::FormulaEngine::Parser;
use Moo;
use Carp;
use Try::Tiny;
use List::Util qw( min max );
use Log::Any '$log';
use namespace::clean;

=head1 SYNOPSIS

  my $parser= Language::FormulaEngine::Parser->new(input => $text);
  $parser->parse;
  
  # To subclass:
  package Foo {
    use Moo;
    extends 'Language::FormulaEngine::Parser';
    
    # for small changes to scanner, change tokenizer coderef
    has '+_scanner_tokenizer' => ( deault => \&... );
    # for major changes to scanner, override next_token method
    sub next_token { ... }  # for major changes to scanner
    
    # for changes to parser, override method corresponding to parse rule
    sub parse_..._expr { ... }
  }

=head1 DESCRIPTION

This is both a scanner and a parser, but the logic is divided into distinct
scanning vs. parsing functions to make it easy to subclass.

=head2 PARSER

The parser can be invoked by calling L</parse>, or just by accessing the
L</parse_tree> attribute.  It will consume tokens via the L</next_token>
method as needed.  The default implementation will throw an exception unless
all tokens are consumed (resulting in a final token with type 'eof').

=cut

has input        => ( is => 'rw' );

has parse_tree   => ( is => 'lazy' );
has functions    => ( is => 'rw', default => sub { {} } );
has variables    => ( is => 'rw', default => sub { {} } ); 

=head2 parse

  $parse_tree= $parser->parse;

Read tokens from scanner to build a parse tree.  Returns the parse tree, which is
composed of L<Language::FormulaEngine::Parser::Node> objects.
Most nodes of the parse tree are function call nodes, because basic operations
like 'x*y' are converted to a function as 'mul(x,y)'.

=cut

sub _build_parse_tree {
	my $self= shift;
	my $tree= $self->parse_expr;

	# It is an error if there was un-processed input.
	$self->{token_type} eq 'eof'
		or die "Unexpected '$self->{token_value}' near \"".$self->token_context."\"\n";

	return $tree;
}

*parse= *_build_parse_tree;

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
	return $first unless $self->{token_type} eq 'or';
	my @or_expr= $first;
	while ($self->{token_type} eq 'or') {
		$self->next_token;
		push @or_expr, $self->parse_and_expr;
	}
	return $self->new_func_call('or', \@or_expr);
}

sub parse_and_expr {
	my $self= shift;
	my $first= $self->parse_not_expr;
	return $first unless $self->{token_type} eq 'and';
	my @and_expr= $first;
	while ($self->{token_type} eq 'and') {
		$self->next_token;
		push @and_expr, $self->parse_not_expr;
	}
	return $self->new_func_call('and', \@and_expr);
}

sub parse_not_expr {
	my $self= shift;
	if ($self->{token_type} eq 'not' or $self->{token_type} eq '!') {
		$self->next_token;
		return $self->new_func_call('not', [ $self->parse_cmp_expr ]);
	}
	return $self->parse_cmp_expr;
}

my %_cmp_ops= map { $_ => 1 } @_CMP_OPS;
sub parse_cmp_expr {
	my $self= shift;
	my $first= $self->parse_sum_expr;
	return $first unless $_cmp_ops{$self->{token_type}};
	my @expr= $first;
	while ($_cmp_ops{$self->{token_type}}) {
		push @expr, $self->new_string($self->{token_type});
		$self->next_token;
		push @expr, $self->parse_sum_expr;
	}
	return $self->new_func_call('compare', \@expr);
}

sub parse_sum_expr {
	my $self= shift;
	my $first= $self->parse_prod_expr;
	return $first unless $self->{token_type} eq '+' or $self->{token_type} eq '-';
	my @sum_expr= $first;
	while ($self->{token_type} eq '+' or $self->{token_type} eq '-') {
		my $negate= $self->consume_token eq '-';
		my $operand= $self->parse_prod_expr;
		push @sum_expr, $negate? $operand->get_negative : $operand;
	}
	return $self->new_func_call('sum', \@sum_expr);
}

sub parse_prod_expr {
	my $self= shift;
	my $value= $self->parse_unit_expr;
	while ($self->{token_type} eq '*' or $self->{token_type} eq '/') {
		my $op= $self->consume_token;
		my $right= $self->parse_unit_expr;
		$value= $self->new_func_call( $op eq '*'? 'mul' : 'div', [ $value, $right ] );
	}
	return $value;
}

sub parse_unit_expr {
	my $self= shift;
	my $negate= 0;
	my $expr;

	if ($self->{token_type} eq '-') {
		$self->next_token;
		return $self->parse_unit_expr->get_negative;
	}

	if ($self->{token_type} eq '(') {
		$self->next_token;
		my $args= $self->parse_list;
		die "Expected ')' near \"".$self->token_context."\"\n"
			if $self->{token_type} ne ')';
		die "Expected expression before ')' near \"".$self->token_context."\"\n"
			unless @$args;
		$self->next_token;
		return @$args > 1? $self->new_func_call('list', $args) : $args->[0];
	}
	
	if ($self->{token_type} eq 'num') {
		return $self->new_number($self->consume_token);
	}
	
	if ($self->{token_type} eq 'str') {
		return $self->new_string($self->consume_token);
	}

	if ($self->{token_type} eq 'ident') {
		my $id= $self->consume_token;
		if ($self->{token_type} eq '(') {
			$self->next_token;
			my $args= $self->parse_list;
			die "Expected ')' near \"".$self->token_context."\"\n"
				if $self->{token_type} ne ')';
			$self->consume_token;
			return $self->new_func_call($id, $args);
		}
		else {
			return $self->new_variable($id);
		}
	}
	
	die "Unexpected '".$self->{token_value}."' near \"".$self->token_context."\"\n";
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
These classes are not based on hashrefs or Moo.  There is a method to construct
each type of node, rather than using a constructor on the class.  This allows
the method to be subclassed and hook logic into the creation of the nodes.

As a special case for removing redundant negative signs and operations, each
node supports the method "get_negative" which returns another node which is the
simplest representation of "-($node)".

=over

=item Node

This is the base class for nodes, and its C<get_negative> returns a function
of C<FuncCallNode( 'negative', $node )>.

=cut 

package Language::FormulaEngine::Parser::Node {
	sub get_negative {
		my $self= shift;
		bless [ 'negative', [ $self ] ], 'Language::FormulaEngine::FuncCallNode';
	}
}

=item FuncCallNode, new_func_call

This is the most common kind of node, as most things in this grammar are treated
as function calls.  It is simply a function name and a list of arguments.

  my $node= $parser->new_func_call( $name, \@arg_nodes );
  print $node->function_name;
  print @{ $node->function_args };

Calling get_negative on a function named 'negative' will simply un-wrap the arguments.

=cut

package Language::FormulaEngine::Parser::FuncCallNode {
	our @ISA= 'Language::FormulaEngine::Parser::Node';

	sub function_name { $_[0][0] }
	sub function_args { $_[0][1] }

	sub get_negative {
		my $self= shift;
		if ($self->function_name eq 'negative') {
			# the negative of a negative is the original
			return $_[0]->function_args->[0];
		}
		$self->SUPER::get_negative();
	}

	sub to_canonical {
		my $self= shift;
		return $self->function_name . '( ' .join(', ', map { $_->to_canonical } @{$self->function_args}). ' )';
	}
}

sub new_func_call {
	my ($self, $fn, $args)= @_;
	$self->functions->{$fn}++; # record dependency on this function
	bless [ $fn, $args ], 'Language::FormulaEngine::Parser::FuncCallNode';
}

=item VarRefNode, new_variable

A variable reference.  Call $node->variable_name to get the variable's identifier.

  my $node= $parser->new_variable($name);

=cut

package Language::FormulaEngine::Parser::VarRefNode {
	our @ISA= 'Language::FormulaEngine::Parser::Node';

	sub variable_name { ${$_[0]} }

	sub to_canonical { $_[0]->variable_name }
}

sub new_variable  {
	my ($self, $ident)= @_;
	$self->variables->{$ident}++; # record dependency on this variable
	bless \$ident, 'Language::FormulaEngine::Parser::VarRefNode'
}

=item StringNode, new_string

A string literal.  Call $node->text to retrieve the value.

=cut

package Language::FormulaEngine::Parser::StringNode {
	use strict;
	use warnings;
	use parent -norequire => 'Language::FormulaEngine::ParseNode';

	sub text { ${$_[0]} }

	sub text_quoted {
		my $str= $_[0]->text;
		$str =~ s/(['\\])/\\$1/g;  # escape single quotes and backslashes
		"'$str'";
	}

	sub to_canonical {
		my $self= shift;
		return Language::FormulaEngine::looks_like_symbol($self->text)? $self->text : $self->text_quoted;
	}
}

sub new_string {
	my ($self, $text)= @_;
	bless \$text, 'Language::FormulaEngine::Parser::StringNode'
}

=item NumberNode, new_number

A numeric literal.  Call $node->value to get the scalar containing the un-processed number.
Note that if you want to support octal or something, the C<new_number> method is a good
place for the conversion.

=cut

package Language::FormulaEngine::Parser::NumberNode {
	
	use strict;
	use warnings;
	use parent -norequire => 'Language::FormulaEngine::ParseNode';

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
	bless \$value, 'Language::FormulaEngine::Parser::NumberNode'
}

=back

=cut

1;
