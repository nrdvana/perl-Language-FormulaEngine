package Language::FormulaEngine::Scanner;
use Moo;
use Carp;
use Try::Tiny;
use Log::Any '$log';
use namespace::clean;

# ABSTRACT - Break a string into tokens

=head1 SYNOPSIS

  my $scanner= Language::FormulaEngine::Scanner->new(input => "...");
  my ($token_type, $token_value)= $scanner->next_token
    or die "Syntax error at ".$scanner->token_context;
  
  # To subclass:
  package Foo {
    use Moo;
    extends 'Language::FormulaEngine::Scanner';
    
    # To change the set of operators/keywords:
    sub _build_keywords { ... }
    
    # To make changes to regexes of existing rules:
    sub _build_rule_... {
      my $rule= shift->SUPER::_build_rule_...;
      $rule->{pattern}= ...;
      return $rule;
    }
    
    # To add new token types:
    sub _build_rules {
      my $rules= shift->SUPER::_build_rules();
      push @$rules, { name => ..., pattern => ..., handler => ... };
      return $rules;
    }
    
    # For more complex parsing than can be done with regexes,
    # build your own tokenizer:
    sub _build__tokenizer {
      return sub {
        my ($scanner, $input)= @_;
        ...;
        return $chars_consumed, $type, $value;
      };
    }
    
    # For context-sensitive scanner states, swap the tokenizer at runtime
    my $other_tokenizer= __PACKAGE__->_build_tokenizer_for_rules(\@other_rules);
    sub _build_rule_foo {
       { name => "Foo", pattern => ..., handler => sub {
           shift->_tokenizer($other_tokenizer);
           return length(shift), '', '';
       }}
    }
  }

=head1 DESCRIPTION

This scanner class consumes an input string to create tokens consisting of
a "type" and "value".  The scanner also keeps track of the position of the
current token within the input to provide error context.

The scanner does not support input streams, only a single string, though it
has placeholders for subclasses to add that functionality.  (It seems unlikely
for a formula to be more then a few hundred characters, so I opted for
simplicity.)

=head1 SCANNER PATTERNS

The default scanner implements the following patterns:

=over

=item Numeric Literal

All the common decimal representations of integers and floating point numbers
which perl can parse.  Optional decimals and decimal point followed by decimals
and optional exponent, ending at either the end of the input or a non-alphanumeric.

=item Keyword

Keywords include punctuation.  The "token_type" of the keyword is the canonical
version of the keyword, and the "token_value" is the actual text that was
captured.  The pattern matches the longest keyword possible.

=item Identifier

Any alpha (or underscore) followed by any run of alphanumerics,
(including underscore or period).

=item String

A single-quoted or double-quoted string, without any escape codes.

=cut

my @_CMP_OPS= (qw(  =  ==  !=  <>  >  >=  <  <=  ), "\x{2260}", "\x{2264}", "\x{2265}");
my @_MATH_OPS= qw(  +  -  *  /  );
my @_LOGIC_OPS= qw(  and  or  not  !  );
my @_LIST_OPS= ( ',', '(', ')' );

sub keywords {
	return {
		(map { $_ => $_ } @_CMP_OPS, @_MATH_OPS, @_LOGIC_OPS, @_LIST_OPS),
		'=' => '==', '<>' => '!=', "\x{2260}" => '!=',
		"\x{2264}" => '<=', "\x{2265}" => '>=',
	};
}

sub _build_rules {
	my $class= shift;
	my $keywords= $class->keywords;
	my $kw_regex= join '|', map { "\Q$_\E" }
		sort { length($b) <=> length($a) } # longest keywords get priority
		keys %$keywords;
	return [ # order matters, for priority
		{ name => 'Whitespace',
		  # Return an empty string to indicate "not a token"
		  match => sub { $_[0]{_buffer} =~ /\G(\s+)/gc ? ('' => '') : (); }
		},
		{ name => 'Numeric Literal',
		  match => sub { $_[0]{_buffer} =~ /\G([0-9]*\.?[0-9]+(?:[eE][+-]?[0-9]+)?)/gc ? (num => $1) : () }
		},
		{ name => 'Keyword',
		  match => sub { $_[0]{_buffer} =~ /\G($kw_regex)/gc ? ( $keywords->{lc $1} => $1 ) : () }
		},
		{ name => 'Identifier',
		  match => sub { $_[0]{_buffer} =~ /\G([A-Za-z_][A-Za-z0-9_.]*)/gc ? ( ident => $1 ) : () }
		},
		{ name => 'String Literal',
		  match => sub {
				# Single or double quoted string, using Pascal-style repeated quotes for escaping
				return () unless $_[0]{_buffer} =~ /\G(?:"((?:[^"]|"")*)"|'((?:[^']|'')*)')/gc;
				my $str= defined $1? $1 : $2;
				$str =~ s/""/"/g if defined $1;
				$str =~ s/''/'/g if defined $2;
				return str => $str;
			}
		},
	];
}

=back

=head1 ATTRIBUTES

=head2 input

A string of input to parse.

=head2 rules

An arrayref of rules which will be checked in sequence against the input
buffer.  Each rule is of the form:

  {
    name => "Name of tokenizer",
    match => sub {
      my $self= shift;
      if ($self->{_buffer} =~ /\G some pattern /gcx) {
        return token_type => $token_value;
      } else {
        return ();
      }
    }
  }

When writing a custom rule, make sure your regex begins with \G (to match
starting from the current position) and sets C</gc> flags.

The matcher should return a pair of token type and token value if it matches,
and an empty list otherwise.

=head2 token_type

A string describing the type of the current token.  The default types
are C<'ident'>, C<'num'>, C<'eof'>, and one for each keyword and operator.
Attribute is undef until the first time L</next_token> is called.

=head2 token_value

Returns the text value of the current token.  Punctuation have identical type
and value.  For 'ident' and 'num', token_value is the identifier or number.
Attribute is undef until the first time L</next_token> is called.

=cut

has input              => ( is => 'rw' );
has rules              => ( is => 'rw', builder => 1 );
has token_type         => ( is => 'ro', init_arg => undef );
has token_value        => ( is => 'ro', init_arg => undef );

=head2 token_row_col

  my ($line, $col)= $scanner->token_row_col();

Returns the 0-based line number and character number of the current token
as a two-element list.
(calculated from L</_buffer_row_col> and L</_buffer_pos>)

=cut

sub token_row_col {
	my $self= shift;
	my ($row, $col)= @{ $self->_buffer_row_col };
	my $buf= $self->_buffer;
	my $ofs= $self->_token_pos;
	$ofs= $self->_buffer_pos unless defined $ofs;
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

  die "Expected something else at ".$scanner->token_context()."\n";
  # same as:
  die "Expected something else at 'blah blah' on line 15, char 12\n";

Returns a single-string view of where the token occurs in the buffer.
This is useful for single-line "die" messages.

=cut

sub token_context {
	my $self= shift;
	my $buf= $self->_buffer;
	my $ofs= $self->_token_pos;
	$ofs= $self->_buffer_pos unless defined $ofs;
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
	my $buf= $self->_buffer;
	my ($token_pos, $scanner_pos)= ($self->_token_pos, $self->_buffer_pos);
	my ($prefix, $token, $suffix)= ('','','');
	# If we don't have a buffer, there's nothing to show, so print "end of input".
	if (!length($buf)) {
		$suffix= '(end of input)';
	}
	# If no token, then can't underline it
	elsif (!defined $token_pos) {
		$prefix= substr($buf, 0, $scanner_pos);
		$suffix= substr($buf, $scanner_pos);
	}
	else {
		$prefix= substr($buf, 0, $token_pos);
		$token=  substr($buf, $token_pos, $scanner_pos-$token_pos);
		$suffix= substr($buf, $scanner_pos);
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

=head1 METHODS

=head2 next_token

  my ($token_type, $token_value)= $scanner->next_token;
  # -or-
  if ($scanner->next_token) {
    # can now use ->token_type and ->token_value
  } else {
    # syntax error
  }

Call next_token to load the next C<token_type> and C<token_value>.
Returns an empty list if the input doesn't match any token pattern.
C<token_context> gets updated regardless of whether a token was parsed.
End of input generates an 'eof' token.  If you call next_token again
after the eof token, it throws an exception.

=cut

sub next_token {
	my $self= shift;
	
	# If already reached end of input, throw an exception.
	die "Can't call next_token after end of input"
		if 'eof' eq ($self->{token_type}||'');
	
	# Detect the next token
	my ($type, $val, $pos0, $pos1)= ('','');
	find_token: while ($type eq '') {
		# Clear the current token
		undef @{$self}{'token_type','token_value','_token_pos'};
		
		$pos0= pos $self->{_buffer} || 0;
		($type, $val)= $self->_find_token;
		$pos1= pos $self->{_buffer} || 0;
		
		$log->tracef('pos0=%d pos1=%d type=%s val=%s buflen=%d', $pos0, $pos1, $type, $val, length $self->{_buffer});
		
		# Check for end of buffer, even if it matched.
		if ($pos1 >= length $self->{_buffer}) {
			pos $self->{_buffer} = $pos0; # rewind to start of token before growing buffer
			if ($self->_grow_buffer) {
				$log->trace("grow buffer succeeded");
				$type= '';
				next find_token;
			}
			# Else, can't grow buffer, so we're stuck here.
			# If we didn't get a token, or a token to ignore, then set the EOF token
			pos $self->{_buffer} = $pos1;
			if (!defined $type || $type eq '') {
				$type= 'eof';
				$val= '';
				last find_token;
			}
		}
		
		defined $type
			or die "Unknown syntax at ".$self->token_context."\n";
		$pos1 > $pos0
			or croak "Tokenizer consumed zero characters";
	}
	@{$self}{'token_type','token_value','_token_pos'}= ($type,$val,$pos0);
	return $type, $val;
}

sub _find_token {
	my $self= shift;
	for my $rule (@{ $self->rules }) {
		my ($type, $val)= $rule->{match}->($self) or next;
		$log->tracef('Matched %s type=%s value=%s _buffer_pos=%d', $rule->{name}, $type, $val, $self->_buffer_pos)
			if $log->is_trace;
		return $type, $val;
	}
	return;
}

=head2 C<consume_token>

  $token_value= $parser->consume_token;

Returns L</token_value> of current token, and calls L</next_token>.

=cut

sub consume_token {
	my $self= shift;
	croak "Can't consume EOF"
		if $self->{token_type} eq 'eof';
	my $val= $self->{token_value};
	$self->next_token;
	return $val;
}

=head1 Scanner Internals

Scanner implementations are not required to use these attributes, but they are the
basis for the default implementation.

=over

=item _buffer

A string of input where the current token was found.

=item _buffer_row_col

0-based line number and 0-based column number of the start of the token buffer,
used for calculating L</token_row_col>.

=item _buffer_pos

Character offset of the first character beyond the end of the token within the
C<_buffer>.  This is defined as C<<pos($self->{_buffer}>>, or in other words
we store the position using perl's regex internals.

=item _token_pos

Character offset of the start of the token within the C<_buffer>.
If L</token_type> is undefined then this attribute is also undefined.

=item _grow_buffer

This method is a placeholder for subclasses to do more advanced input reading.
Currently it just loads the entire L</input> attribute into L<_buffer>.
Returns true if it obtained more input, or false at the end of the input stream.

=cut

has _buffer         => ( is => 'rw', init_arg => undef, default => sub { '' } );
has _buffer_row_col => ( is => 'rw', init_arg => undef, default => sub { [0,0] } );
sub _buffer_pos        { pos($_[0]{_buffer})= $_[1] if @_ > 1; pos($_[0]{_buffer}) }
has _token_pos      => ( is => 'rw', init_arg => undef, default => sub { 0 } );

sub _grow_buffer {
	my $self= shift;
	# The default implementation just loads the "input" string as the scanner buffer.
	# Subclasses could read blocks from a file handle, or etc.
	# Don't forget to preserve pos() on the string.
	if (!length $self->{_buffer} && length $self->input) {
		$self->{_buffer}= $self->input;
		return 1;
	}
	return 0;
}

1;
