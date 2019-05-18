package Language::FormulaEngine::Parser;
use Moo;
use Carp;
use Try::Tiny;
use List::Util qw( min max );
use Log::Any '$log';
use Language::FormulaEngine::Parser::ContextUtil
	qw( calc_text_coordinates format_context_string format_context_multiline );
use namespace::clean;

our $VERSION= '0.00_1';

# ABSTRACT - Create parse tree from an input string

=head1 SYNOPSIS

  my $parse_tree= Language::FormulaEngine::Parser->parse($string)->parse_tree;

=head1 DESCRIPTION

This class scans tokens from an input string and builds a parse tree.  In compiler terminology,
it is both a Scanner and Parser.  It performs a top-down recursive descent parse, because this
is easy and gives good error messages.  It only parses strings, but leaves room for subclasses
to implement streaming.  By default, the parser simply applies a Grammar to the input, without
checking whether the functions variables exist, but can be subclassed to do more detailed
analysis during the parse.

The generated parse tree is made up of Function nodes (each infix operator is converted to a
named function) and each Function node may contain Symbols, Strings, Numbers, and other
Function nodes.  The parse tree can be passed to the Evaluator for instant execution, or passed
to the Compiler to generate an optimized perl coderef.  The parse tree is lightweight, and does
not include token/context information; this could also be added by a subclass.

=head1 PUBLIC API

=head2 parse

This is both a constructor and main method of the class.  It returns a new Parser object which
holds the final state of the parse, which may have succeeded or failed.  Inspect the various
attributes to find out what happened.

=head2 parse_tree

This holds the generated parse tree, or C<undef> if the parse failed.  See L</"PARSE NODES">.

=head2 error

This is C<undef> if the parse succeeded, else an error message describing the syntax that ended
the parse.

=head2 functions

A set (hashref) of all function names encountered during the parse.

=head2 symbols

A set (hashref) of all non-function symbols encountered.  (i.e. variables, but I'm not calling
them that because this language is intended for read-only behavior)

=cut

has parse_tree   => ( is => 'rw' );
has error        => ( is => 'rw' );
has functions    => ( is => 'rw' );
has symbols      => ( is => 'rw' );

sub parse {
	my ($proto, $input)= @_;
	my $class= ref $proto || $proto;
	my $self= $class->new(
		(ref $proto? %$proto : ()),
		functions  => {},
		symbols    => {},
		parse_tree => undef,
		error      => undef,
		input      => $input,
	);
	pos( $self->{input} )= 0;
	try {
		$self->next_token;
		my $tree= $self->parse_expr;
		# It is an error if there was un-processed input.
		$self->token_type eq '0'
			or die sprintf('Unexpected %s "%s" near %s',
				$self->token_type, $self->token_value, $self->token_context);
		$self->parse_tree($tree);
	} catch {
		$self->error($_);
	};
	return $self;
}

sub _str_escape {
	my $str= shift;
	$str =~ s/'/''/g;
	"'$str'";
}

sub deparse {
	my ($self, $node)= @_;
	return $node unless ref $node;
	return _str_escape($node->text) if $node->can('text');
	return $node->symbol if $node->can('symbol');
	return $node->function_name . '( ' .join(', ', map $self->deparse($_), @{$node->parameters}). ' )'
		if $node->can('function_name');
	croak "Don't know how to deparse node type ".ref($node);
}

=head1 EXTENSIBLE API

These methods and attributes are documented for purposes of subclassing the parser.

=head2 input

The input string being scanned.
Code within the parser should access this as C<< $self->{input} >> for efficiency.

=head2 input_pos

Shortcut for C<< pos($self->{input}) >>.

=head2 token_type

Type of current token scanned from C<input>.
Code within the parser should access this as C<< $self->{token_type} >> for efficiency.

=head2 token_value

Value of current token scanned from C<input>, with escape sequences and etc resolved to a
sensible perl value.
Code within the parser should access this as C<< $self->{token_value} >> for efficiency.

=head2 token_pos

An offset within C<input> where this token started.
Code within the parser should access this as C<< $self->{token_pos} >> for efficiency.

=cut

has input           => ( is => 'rw', required => 1 );
sub input_pos          { pos( shift->{input} ) }
sub token_type         { shift->{token_type} }
sub token_value        { shift->{token_value} }
sub token_pos          { shift->{token_pos} }

=head2 next_token

Advance to the next token, replacing the values of C<token_> variables and updating
C<input_pos>.  Returns the token_type, of which all are true except EOF which has a
type of C<0>, so this also means the function returns true if it parsed a token and
false if it reached EOF.  It dies if no token could be parsed.
If you call next_token again after the eof token, it throws an exception.

This method is a wrapper around L</scan_token>. Override that method to add new token types.

=cut

sub next_token {
	my $self= shift;
	
	# If already reached end of input, throw an exception.
	die "Can't call next_token after end of input"
		if '0' eq ($self->{token_type}||'');
	
	# Detect the next token
	my ($type, $val, $pos0, $pos1)= ('','');
	while ($type eq '') {
		$pos0= pos($self->{input}) || 0;
		($type, $val)= $self->scan_token;
		$pos1= pos($self->{input}) || 0;
		# Check for end of buffer, even if it matched.
		if ($pos1 >= length $self->{input}) {
			#pos($self->{input})= $pos0; # rewind to start of token before growing buffer
			#if ($self->_grow_buffer) {
			#	$log->trace("grow buffer succeeded");
			#	$type= '';
			#	next;
			#}
			#pos($self->{input})= $pos1; # restore actual position\
			# If we didn't get a token or are ignoring this final token, then return the EOF token
			if (!defined $type || $type eq '') {
				$type= 0;
				$val= '';
				$pos0= $pos1;
				last;
			}
		}
		defined $type
			or die "Unknown syntax at ".$self->token_context."\n";
		$pos1 > $pos0
			or croak "Tokenizer consumed zero characters";
	}
	@{$self}{'token_type','token_value','token_pos'}= ($type,$val,$pos0);
	return $type, $val;
}

=head2 scan_token

Pattern-match the next token, and either return C<< $type => $value >> or an empty list if
the syntax is invalid.  This is intended to be overridden by subclasses.

=head2 consume_token

  return $self->consume_token if $self->{token_type} eq $desired_type;

This is a shorthand for returning the current C<token_value> while also calling C<next_token>.

=head2 token_context

  my $text= $self->token_context(%options);

Default behavior generates a string like:

  "'blah blah' on line 15, char 12"

Passing C<< token_context(multiline => 1) >> generates a string like

  "Expected something else at line 15, char 16\n" .
  "blah blah blah token blah blah\n" .
  "               ^^^^^\n"

Multiline additionally takes arguments as described in
L<Language::FormulaEngine::Parser::ContextUtil/format_context_multiline>.

=cut

sub consume_token {
	my $self= shift;
	croak "Can't consume EOF"
		if $self->{token_type} eq '0';
	my $val= $self->{token_value};
	$self->next_token;
	return $val;
}

sub token_context {
	my ($self, %args)= @_;
	return format_context_multiline($self->{input}, $self->{token_pos}||0, pos($self->{input})||0, \%args)
		if delete $args{multiline};
	return format_context_string($self->{input}, $self->{token_pos}||0, pos($self->{input})||0);
}

=head1 GRAMMAR

=head2 Parse Rules

The default grammar implements the following rules:

  expr      ::= or_expr
  or_expr   ::= and_expr ( 'or' and_expr )*
  and_expr  ::= not_expr ( 'and' not_expr )*
  not_expr  ::= ( 'not' | '!' ) cmp_expr | cmp_expr
  cmp_expr  ::= sum_expr ( ( '=' | '==' | '<>' | '\u2260' | '<' | '<=' | '>' | '>=' ) sum_expr )*
  sum_expr  ::= prod_expr ( ('+' | '-') prod_expr )*
  prod_expr ::= ( unit_expr ('*' | '/') )* unit_expr
  unit_expr ::= '-' unit_expr | Identifier '(' list ')' | '(' (expr|list) ')' | Identifier | Number | String
  list      ::= expr ( ',' expr )* ','?

C<ident>, C<num>, C<str>, and all the punctuation symbols are tokens.

The parser uses a Recursive Descent algorithm implemented as the following method calls.
Each method consumes tokens from C<< $self >> and return a L</"PARSE NODES">:

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
	return $self->new_call('or', \@or_expr);
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
	return $self->new_call('and', \@and_expr);
}

sub parse_not_expr {
	my $self= shift;
	if ($self->{token_type} eq 'not' or $self->{token_type} eq '!') {
		$self->next_token;
		return $self->new_call('not', [ $self->parse_cmp_expr ]);
	}
	return $self->parse_cmp_expr;
}

my %_cmp_ops= map { $_ => 1 } qw( > < >= <= != == );
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
	return $self->new_call('compare', \@expr);
}

sub parse_sum_expr {
	my $self= shift;
	my $first= $self->parse_prod_expr;
	return $first unless $self->{token_type} eq '+' or $self->{token_type} eq '-';
	my @sum_expr= $first;
	while ($self->{token_type} eq '+' or $self->{token_type} eq '-') {
		my $negate= $self->consume_token eq '-';
		my $operand= $self->parse_prod_expr;
		push @sum_expr, $negate? $self->get_negative($operand) : $operand;
	}
	return $self->new_call('sum', \@sum_expr);
}

sub parse_prod_expr {
	my $self= shift;
	my $value= $self->parse_unit_expr;
	while ($self->{token_type} eq '*' or $self->{token_type} eq '/') {
		my $op= $self->consume_token;
		my $right= $self->parse_unit_expr;
		$value= $self->new_call( $op eq '*'? 'mul' : 'div', [ $value, $right ] );
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
		die "Expected ')' near ".$self->token_context."\n"
			if $self->{token_type} ne ')';
		die "Expected expression before ')' near ".$self->token_context."\n"
			unless @$args;
		$self->next_token;
		return @$args > 1? $self->new_call('list', $args) : $args->[0];
	}
	
	if ($self->{token_type} eq 'Number') {
		return $self->new_number($self->consume_token);
	}
	
	if ($self->{token_type} eq 'String') {
		return $self->new_string($self->consume_token);
	}
	
	if ($self->{token_type} eq 'Identifier') {
		my $id= $self->consume_token;
		if ($self->{token_type} eq '(') {
			$self->next_token;
			my $args= $self->parse_list;
			die "Expected ')' near ".$self->token_context."\n"
				if $self->{token_type} ne ')';
			$self->consume_token;
			return $self->new_call($id, $args);
		}
		else {
			return $self->new_variable($id);
		}
	}
	
	if ($self->{token_type} eq '0') {
		die "Expected expression component near (end of input)";
	}
	
	die "Unexpected token $self->{token_type} '$self->{token_value}' near ".$self->token_context."\n";
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

=head2 Token Types

=over

=item C<'Number'>

All the common decimal representations of integers and floating point numbers
which perl can parse.  Optional decimals and decimal point followed by decimals
and optional exponent, ending at either the end of the input or a non-alphanumeric.

=item C<'String'>

A single-quoted or double-quoted string, treating a double occurrence of the quote
character to mean a literal quote character.  ("Pascal style")

=item Keywords...

Keywords include the "word" tokens like 'OR', but also every text literal seen in a parse rule
such as operators and punctuation.
The C<token_type> of the keyword is the canonical version of the keyword, and the C<token_value>
is the actual text that was captured.  The pattern matches the longest keyword possible.

=item C<'Identifier'>

Any alpha (or underscore) followed by any run of alphanumerics,
(including underscore and period).

=back

=cut

our (@CMP_OPS, @MATH_OPS, @LOGIC_OPS, @LIST_OPS);
BEGIN {
	@CMP_OPS= (qw(  =  ==  !=  <>  >  >=  <  <=  ), "\x{2260}", "\x{2264}", "\x{2265}");
	@MATH_OPS= qw(  +  -  *  /  );
	@LOGIC_OPS= qw(  and  or  not  !  );
	@LIST_OPS= ( ',', '(', ')' );
	my %keywords= (
		(map { $_ => $_ } @CMP_OPS, @MATH_OPS, @LOGIC_OPS, @LIST_OPS),
		'=' => '==', '<>' => '!=', "\x{2260}" => '!=',
		"\x{2264}" => '<=', "\x{2265}" => '>=',
	);
	my $kw_regex= join '|', map { "\Q$_\E" }
		sort { length($b) <=> length($a) } # longest keywords get priority
		keys %keywords;
	
	my $scan_token= eval q%
		sub {
			my $self= shift;
			
			# Ignore whitespace
			if ($self->{input} =~ /\G(\s+)/gc) {
				return '' => ''; # empty string causes next_token to loop
			}
			
			# Check for numbers
			if ($self->{input} =~ /\G([0-9]*\.?[0-9]+(?:[eE][+-]?[0-9]+)?)\b/gc) {
				return Number => $1;
			}
			
			# Check for any keyword, and convert the type to the canonical (lowercase) name.
			if ($self->{input} =~ /\G(%.$kw_regex.q%)/gc) {
				return $keywords{lc $1} => $1;
			}
			
			# Check for identifiers
			if ($self->{input} =~ /\G([A-Za-z_][A-Za-z0-9_.]*)\b/gc) {
				return Identifier => $1;
			}
			
			# Single or double quoted string, using Pascal-style repeated quotes for escaping
			if ($self->{input} =~ /\G(?:"((?:[^"]|"")*)"|'((?:[^']|'')*)')/gc) {
				my $str= defined $1? $1 : $2;
				$str =~ s/""/"/g if defined $1;
				$str =~ s/''/'/g if defined $2;
				return String => $str;
			}
			return;
		}
	% or die $@;
	no strict 'refs';
	*scan_token= $scan_token;
}

=head2 Parse Nodes

The parse tree takes a minimalist approach to node classification.  In this default
implementation, numbers are represented as plain perl scalars, strings and symbolic references
are represented as blessed scalar refs, and function calls are represented as blessed Lisp-style
arrayrefs.

A blessed node only needs to support one method: C<< ->evaluate($namespace) >>.

The class name of the blessed nodes should be ignored.  A function is anything which
C<< can("function_name") >>, a string is anything which C<< can("string_value") >> and a
symbolic reference is anything which C<< can("symbolic_name") >>.  Also, the blessed strings
automatically stringify to their value, behaving almost like plain perl scalars.

Subclasses of Parser should implemnt new node types as needed.  You probable also need to
update L</deparse>.

The parser rules create nodes by methods on the Parser class, for easy subclassing.

=over

=item new_call

  $node= $parser->new_call( $function_name, $parameters );

Generate a node for a function call.  The returned node has attributes C<function_name>
and C<parameters>

=cut

sub Language::FormulaEngine::Parser::Node::Call::function_name { $_[0][0] }
sub Language::FormulaEngine::Parser::Node::Call::parameters { $_[0][1] }
sub Language::FormulaEngine::Parser::Node::Call::evaluate {
	my ($self, $namespace)= @_;
	my $name= $self->function_name;
	my $fn= $namespace->get_function($name) or die "Unknown function '$name'\n";
	$fn->($namespace, $self);
}

sub new_call {
	my ($self, $fn, $params)= @_;
	$self->functions->{$fn}++; # record dependency on this function
	bless [ $fn, $params ], 'Language::FormulaEngine::Parser::Node::Call';
}

=item new_symbol

  $node= $parser->new_symbol($symbol_name);

A reference to a symbolic value (i.e. variable or constant).
It has one attribute C<symbol_name>.

=cut

sub Language::FormulaEngine::Parser::Node::Symbol::symbol_name { ${$_[0]} }
sub Language::FormulaEngine::Parser::Node::Symbol::evaluate {
	my ($self, $namespace)= @_;
	$namespace->get_value($$self);
}

sub new_variable  {
	my ($self, $name)= @_;
	$self->symbols->{$name}++; # record dependency on this variable
	bless \$name, 'Language::FormulaEngine::Parser::Node::Symbol';
}

=head3 new_string

  $node= $parser->new_string($string_value);

A string literal.  It has an attribute C<string_value> holding the raw value.

=cut

sub Language::FormulaEngine::Parser::Node::String::string_value { ${$_[0]} }
sub Language::FormulaEngine::Parser::Node::String::evaluate { ${$_[0]} }

sub new_string {
	my ($self, $text)= @_;
	bless \$text, 'Language::FormulaEngine::Parser::Node::String'
}

=item new_number

  $plain_scalar= $parser->new_number($value);

A numeric constant.  It has an attribute C<number_value> holding the raw value.

=cut

sub Language::FormulaEngine::Parser::Node::Number::number_value { ${$_[0]} }
sub Language::FormulaEngine::Parser::Node::Number::evaluate { ${$_[0]} }

sub new_number {
	my $value= $_[1]+0;
	bless \$value, 'Language::FormulaEngine::Parser::Node::Number'
}

=item get_negative

Utility method to get the "opposite of" a parse node.  By default, this wraps it with the
function C<'negative'>, unless it already was that function then it unwraps the parameter.
It performs simple negation on numbers.

=back

=cut

sub get_negative {
	my ($self, $node)= @_;
	return $self->new_number(-$node->number_value) if $node->can('number_value');
	return $node->parameters->[0] if $node->can('function_name') and $node->function_name eq 'negative';
	return $self->new_call('negative', [$node]);
}

1;
