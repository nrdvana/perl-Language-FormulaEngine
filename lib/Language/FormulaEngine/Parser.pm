package Language::FormulaEngine::Parser;
use Moo;
use Carp;
use Try::Tiny;
use List::Util qw( min max );
use Log::Any '$log';
use namespace::clean;

=head1 DESCRIPTION

This is both a scanner and a parser, but the logic is divided into distinct
scanning vs. parsing functions to make it easy to subclass.

=head2 PARSER

=cut

has input        => ( is => 'rw' );

has parse_tree   => ( is => 'lazy' );
has functions    => ( is => 'rw', default => sub { {} } );
has variables    => ( is => 'rw', default => sub { {} } ); 

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

These are implemented as function calls which consume C<input>, and return parse nodes:

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

my @_CMP_OPS= (qw(  =  ==  !=  <>  >  >=  <  <=  ), "\x{2260}", "\x{2264}", "\x{2265}");
my @_MATH_OPS= qw(  +  -  *  /  );
my @_LOGIC_OPS= qw(  and  or  not  !  );
my @_LIST_OPS= ( ',', qw/ (  ) / );
my %_KEYWORDS= (
	(map { $_ => $_ } @_CMP_OPS, @_MATH_OPS, @_LOGIC_OPS, @_LIST_OPS),
	'=' => '==', '<>' => '!=', "\x{2260}" => '!=', "\x{2264}" => '<=', "\x{2265}" => '>='
);

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

=head1 SCANNER

The scanner methods consume input to generate the next value for token_type and token_value.
These are always fields of the Parser object, and the accessor methods are read-only.

(Why not return token objects? because updating hash fields is much more efficient
than dynamically allocating objects for each token and calling methods throughout
the parser.)

The scanner must also maintain enough buffer context to support the L</token_context> and
L</token_context_tty> methods.

=head2 Scanner Internals

Scanner implementations are not required to use these attributes, but they are the
basis for the default implementation.

=over

=item _scanner_buffer

A string of input where the current token was found.

=item _scanner_buffer_row_col

0-based line number and 0-based column number of the start of the token buffer,
used for calculating token_row_col.

=item _scanner_token_ofs

Character offset of the start of the token within the _scanner_buffer.

=item _scanner_ofs

Character offset of the first character beyond the end of the token within the
_scanner_buffer.

=back

=cut

has _scanner_tokenizer      => ( is => 'rw', init_arg => undef, default => sub { \&_default_tokenizer } );
has _scanner_buffer         => ( is => 'rw', init_arg => undef, default => sub { '' } );
has _scanner_buffer_row_col => ( is => 'rw', init_arg => undef, default => sub { [0,0] } );
has _scanner_token_ofs      => ( is => 'rw', init_arg => undef, default => sub { 0 } );
has _scanner_ofs            => ( is => 'rw', init_arg => undef, default => sub { 0 } );

=head2 Current-Token Attributes

These attributes should be written directly as hash fields.
The read-only accessor is provided for convenience.

=over

=item token_type

A string describing the type of the current token.  The default types
are 'ident', 'num', 'eof', and each punctuation symbol is its own type; i.e.
types of '+', '-', '*', '/'.

=item token_value

Returns the text value of the current token.  Punctuation have identical type
and value.  For 'ident' and 'num', token_value is the identifier or number.

=cut

has token_type         => ( is => 'ro', init_arg => undef );
has token_value        => ( is => 'ro', init_arg => undef );

=head2 token_row_col

  my ($row, $col)= $parser->token_row_col();

Returns the 0-based row number and column number of the current token
as a two-element list.
(calculated from scanner_buffer_row_col and scanner_ofs)

=cut

sub token_row_col {
	my $self= shift;
	my ($row, $col)= @{ $self->_scanner_buffer_row_col };
	my $buf= $self->_scanner_buffer;
	my $ofs= $self->_scanner_token_ofs // $self->_scanner_ofs;
	# If there are any newlines from the start of the buffer to the start of the token...
	my $line_end= rindex($buf, "\n", $ofs-1);
	if ($line_end >= 0) {
		# ...then add up the number of newlines and re-calculate the column
		#my $line_start= $line_end+1;
		$row+= (substr($buf, 0, $line_end+1) =~ /\n/);
		$col= $ofs - ($line_end+1);
	}
	else {
		$col += $ofs;
	}
	return ($row, $col);
}

=head2 token_context

  die "Expected something else at ".$parser->token_context()."\n";
  # same as:
  die "Expected something else at 'blah blah' on line 15, char 12\n";

Returns a single-string view of where the token occurs in the buffer.
This is useful for single-line "die" messages.

=cut

sub token_context {
	my $self= shift;
	my $buf= $self->_scanner_buffer;
	my $ofs= $self->_scanner_token_ofs // $self->_scanner_ofs;
	# If we don't have a buffer, there's nothing to show, so print "end of input".
	length($buf // '') > $ofs
		or return '(end of input)';
	my $context= substr($buf, $ofs, 20);
	$context =~ s/\n.*//s; # remove subsequent lines
	my ($row, $col)= $self->token_row_col();
	return sprintf '"%s" at line %d char %d', $context, $row+1, $col+1;
}

=head2 token_context_tty

  print $parser->token_context_tty();
  # is like:
  print "blah blah blah token blah blah\n";
  print "               ^^^^^\n";
  print " (line 15, char 16)\n";

More advanced view of the input string, printed on two lines with the second
marking the token within its context.  This is only useful with a fixed-width
font in a multi-line context.

This method also supports various options for formatting.

=cut

sub token_context_tty {
	my ($self, %args)= @_;
	my $buf= $self->scanner_buffer;
	my ($token_ofs, $scanner_ofs)= ($self->_scanner_token_ofs, $self->_scanner_ofs);
	my ($prefix, $token, $suffix)= ('','','');
	# If we don't have a buffer, there's nothing to show, so print "end of input".
	if (!length($buf)) {
		$suffix= '(end of input)';
	}
	# If no token, then can't underline it
	elsif (!defined $token_ofs) {
		$prefix= substr($buf, 0, $scanner_ofs);
		$suffix= substr($buf, $scanner_ofs);
	}
	else {
		$prefix= substr($buf, 0, $token_ofs);
		$token=  substr($buf, $token_ofs, $scanner_ofs-$token_ofs);
		$suffix= substr($buf, $scanner_ofs);
	}
	
	# Truncate prefix and suffix at line endings
	$prefix =~ s/.*\n//s;
	$suffix =~ s/\n.*//s;
	# Limit lengths of prefix and suffix and token
	my $max_width= $args{max_width} // 78;
	if (length($prefix) + length($token) > $max_width) {
		my $min_token= min(length($token), $args{min_token} // 30);
		# truncate prefix, or token, or both
		if (length($prefix) > $max_width - $min_token) {
			substr($prefix, 0, -($max_width - $min_token))= '';
		}
		if (length($prefix) + length($token) > $max_width) {
			substr($token, -($max_width - length($prefix) - length($token)))= ''; 
		}
	}
	if (length($prefix) + length($token) + length($suffix) > $max_width) {
		substr($suffix, -($max_width - length($prefix) - length($token)))= '';
	}
	my ($row, $col)= $self->token_row_col();
	return sprintf "%s%s%s\n%s%s\n (line %d char %d)\n",
		$prefix, $token, $suffix,
		' ' x length($prefix), '^' x length($token),
		$row+1, $col+1;
}

sub next_token {
	my $self= shift;
	
	# If already reached end of input, throw an exception.
	return 0
		if 'eof' eq ($self->{token_type}//'');
	
	# Clear the current token
	undef @{$self}{'token_type','token_value','_scanner_token_ofs'};
	# Detect the next token
	while (1) { # loop while type is ''
		if ($self->{_scanner_ofs} >= length($self->{_scanner_buffer})) {
			unless ($self->_scanner_grow_buffer) {
				$self->{token_type}= 'eof';
				$self->{token_value}= '';
				return 0;
			}
		}
		(my ($consumed, $type, $val)= $self->_scanner_tokenizer->($self, substr($self->{_scanner_buffer}, $self->{_scanner_ofs})))
			or die "Unknown syntax at ".$self->token_context."\n";
		$consumed > 0
			or croak "Tokenizer consumed zero characters";
		defined $type
			or croak "Tokenizer did not return token_type";
		$self->{_scanner_ofs}+= $consumed;
		if ($type ne '') { # whitespace isn't returned
			$self->{_scanner_token_ofs}= $self->{_scanner_ofs} - $consumed;
			@{$self}{'token_type','token_value'}= ($type,$val);
			return 1;
		}
	}
}

sub consume_token {
	my $self= shift;
	croak "Can't consume EOF"
		if $self->{token_type} eq 'eof';
	my $val= $self->{token_value};
	$self->next_token;
	return $val;
}

sub _scanner_grow_buffer {
	my $self= shift;
	# The default implementation just loads the "input" string as the scanner buffer.
	# Subclasses could read blocks from a file handle, or etc.
	if (!length $self->{_scanner_buffer} && length $self->input) {
		$self->{_scanner_buffer}= $self->input;
		return 1;
	}
	return 0;
}

my @default_rules= map { _coerce_scanner_rule($_) } (
	[ 'Whitespace',
		qr/(\s+)/,
		sub { return length($_[1]), '', ''; }
	],
	[ 'Numeric literal',
		qr/([0-9]*\.?[0-9]+(?:[eE][+-]?[0-9]+)?)(?:[^A-Za-z0-9_.]|$)/,
		sub { return length($_[1]), 'num', $_[1]; }
	],
	[ 'Keyword',
		do { my $kw_regex= join('|', map { "\Q$_\E" } sort { length($b) <=> length($a) } keys %_KEYWORDS); qr/($kw_regex)/i },
		sub { my $kw= $_KEYWORDS{lc($_[1])}; return length($_[1]), $kw, $_[1]; }
	],
	[ 'Identifier',
		qr/([A-Za-z0-9_.]+)/i,
		sub { return length($_[1]), 'ident', $_[1]; }
	],
	[ 'String',
		qr/("[^"]*"|'[^']*')/,
		sub {
			my $str= substr($_[1], 1, -1);
			return length($_[1]), 'str', $str;
		}
	]
);
my $default_rules_regex= _combine_rule_regexes(\@default_rules);
sub _default_tokenizer {
	my $self= shift;
	
	# Compare against all regexes at once
	my @cap= ($_[0] =~ $default_rules_regex)
		or return; # throws syntax error
	
	for my $rule (@default_rules) {
		my @rule_cap= splice @cap, 0, $rule->{capture_count};
		if (grep { defined } @rule_cap) {
			return $rule->{handler}->($self, @rule_cap);
		}
	}
	croak "BUG: combined rule regex matched, but no captures were collected";
}

sub _default_tokenizer_debug {
	my $self= shift;
	
	# Compare against all regexes at once
	my @cap= ($_[0] =~ $default_rules_regex);
	$log->tracef("captured %s", \@cap);
	@cap or return; # throws syntax error
	
	for my $rule (@default_rules) {
		my @rule_cap= splice @cap, 0, $rule->{capture_count};
		if (grep { defined } @rule_cap) {
			my @ret= $rule->{handler}->($self, @rule_cap);
			$log->debugf("found %s '%s', consumed %d", $rule->{name}, $ret[2], $ret[0]);
			return @ret;
		}
	}
	croak "BUG: combined rule regex matched, but no captures were collected";
}

sub _combine_rule_regexes {
	my $rules= shift;
	my $re= join '|', map { $_->{pattern} } @$rules;
	return qr/^(?:$re)/;
}

sub _coerce_scanner_rules {
	my $thing= shift;
	ref $thing eq 'ARRAY' or croak "Expected arrayref of scanner rules";
	[ map { _coerce_scanner_rule($_) } @$thing ];
}

sub _coerce_scanner_rule {
	my $thing= shift;
	ref $thing or croak "Expected arrayref or hashref for scanner rule";
	if (ref $thing eq 'ARRAY') {
		@$thing == 3 and !ref $thing->[0] and ref($thing->[1]) eq 'Regexp' and ref $thing->[2] eq 'CODE'
			or croak 'Arrayref scanner rule must be [$name, qr/pattern/, \&handler ]';
		$thing= { name => $thing->[0], pattern => $thing->[1], handler => $thing->[2] };
	}
	elsif (ref $thing eq 'HASH') {
		defined $thing->{pattern} or croak "Scanner rule is missing pattern";
		defined $thing->{handler} or croak "Scanner rule is missing handler";
		$thing->{name} //= "$thing->{pattern}";
	}
	else {
		croak "Can't coerce ".ref($thing)." to scanner rule";
	}
	"" =~ /|$thing->{pattern}/;
	$thing->{capture_count}= $#+;
	$thing;
}

=head1 PARSER OBJECT

The parser object takes a stream of tokens returned from a Scanner, and builds
a parse tree.  The tree is composed of
Language::FormulaEngine::ParseNode objects.  Most nodes
of the parse tree are function call nodes, because basic operations like 'x*y'
are converted to a function as 'mul(x,y)'.

=head2 parse( $scanner )

Read tokens from scanner to build a parse tree.  Returns a hashref of

  {
  parse_tree => $FormulaEngine_ParseNode,
  fn_deps    => \%set_of_function_names,
  var_deps   => \%set_of_var_names,
  sym_deps   => \%set_of_literals,
  }

=cut

use Moo;
use Carp;
use Try::Tiny;

sub info { $_[0]{info} }




