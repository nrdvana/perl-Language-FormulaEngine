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

sub _build_keywords {
	return {
		(map { $_ => $_ } @_CMP_OPS, @_MATH_OPS, @_LOGIC_OPS, @_LIST_OPS),
		'=' => '==', '<>' => '!=', "\x{2260}" => '!=',
		"\x{2264}" => '<=', "\x{2265}" => '>=',
	};
}

sub _build_rule_whitespace {
	return {
		name    => 'Whitespace',
		pattern => qr/(\s+)/,
		# Return an empty string to indicate "not a token"
		handler => sub { return length($_[1]), '', ''; },
	};
}
sub _build_rule_number {
	return {
		name    => 'Numeric Literal',
		pattern => qr/([0-9]*\.?[0-9]+(?:[eE][+-]?[0-9]+)?)/,
		handler => sub { return length($_[1]), 'num', $_[1]; },
	};
}
sub _build_rule_keyword {
	my $keywords= shift->_build_keywords;
	my $kw_regex= join '|', map { "\Q$_\E" }
		sort { length($b) <=> length($a) } # longest keywords get priority
		keys %$keywords;
	return {
		name    => 'Keyword',
		pattern => qr/($kw_regex)/i,
		handler => sub {
			my $kw= $keywords->{lc($_[1])};
			return length($_[1]), $kw, $_[1];
		},
	};
}
sub _build_rule_identifier {
	return {
		name    => 'Identifier',
		pattern => qr/([A-Za-z_][A-Za-z0-9_.]*)/,
		handler => sub { return length($_[1]), 'ident', $_[1]; }
	};
}
sub _build_rule_string {
	return {
		name    => 'String Literal',
		pattern => qr/("[^"]*"|'[^']*')/,
		handler => sub {
			my $str= substr($_[1], 1, -1);
			return length($_[1]), 'str', $str;
		}
	};
}

sub _build_rules {
	my $class= shift;
	return [ # order matters, for priority
		$class->_build_rule_whitespace,
		$class->_build_rule_number,
		$class->_build_rule_keyword,
		$class->_build_rule_identifier,
		$class->_build_rule_string,
	];
}

=back

You can change the scanner patterns by overriding the C<_build_rule_*> methods,
or overriding the _build_rules method, or create a custom L</_tokenizer> with your
own rule set.  Beware that the rule regexes are compiled into a single tokenizer
coderef ONCE PER PACKAGE, so if you want dynamic behavior (like a configurable
'rules' attribute per instance) you'll need to make additional changes to the
implementation.

=head1 ATTRIBUTES

=head2 input

A string of input to parse.

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
has token_type         => ( is => 'ro', init_arg => undef );
has token_value        => ( is => 'ro', init_arg => undef );

=head2 token_row_col

  my ($line, $col)= $scanner->token_row_col();

Returns the 0-based line number and character number of the current token
as a two-element list.
(calculated from L</_buffer_row_col> and L</_buffer_ofs>)

=cut

sub token_row_col {
	my $self= shift;
	my ($row, $col)= @{ $self->_buffer_row_col };
	my $buf= $self->_buffer;
	my $ofs= $self->_token_ofs // $self->_buffer_ofs;
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
	my $ofs= $self->_token_ofs // $self->_buffer_ofs;
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
	my ($token_ofs, $scanner_ofs)= ($self->_token_ofs, $self->_buffer_ofs);
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
	return 0
		if 'eof' eq ($self->{token_type}//'');
	
	# Detect the next token
	while (1) { # loop while type is ''
		# Clear the current token
		undef @{$self}{'token_type','token_value','_token_ofs'};
		
		# Check for end of buffer
		if ($self->{_buffer_ofs} >= length($self->{_buffer})) {
			unless ($self->_grow_buffer) {
				$self->{token_type}= 'eof';
				$self->{token_value}= '';
				return 0;
			}
		}
		(my ($consumed, $type, $val)= $self->_tokenizer->($self))
			or die "Unknown syntax at ".$self->token_context."\n";
		$consumed > 0
			or croak "Tokenizer consumed zero characters";
		defined $type
			or croak "Tokenizer did not return token_type";
		$self->{_buffer_ofs}+= $consumed;
		if ($type ne '') { # whitespace isn't returned
			$self->{_token_ofs}= $self->{_buffer_ofs} - $consumed;
			@{$self}{'token_type','token_value'}= ($type,$val);
			return 1;
		}
	}
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

=item _tokenizer

Set this to a coderef of your choice which obeys the following API:

  sub {
    my ($scanner, $input_string)= @_;
    ...
    return ($number_consumed_chars, $token_type, $token_value);
  }

The default value is built I<AND CACHED ONCE PER PACKAGE> by L</_build_tokenizer>
from the rules returned by L</_build_rules>.

=item _buffer

A string of input where the current token was found.

=item _buffer_row_col

0-based line number and 0-based column number of the start of the token buffer,
used for calculating L</token_row_col>.

=item _buffer_ofs

Character offset of the first character beyond the end of the token within the
C<_buffer>.  This is defined as C<<pos($self->{_buffer}>>, or in other words
we store the position using perl's regex internals.

=item _token_ofs

Character offset of the start of the token within the C<_buffer>.
If L</token_type> is undefined then this attribute is also undefined.

=item _grow_buffer

This method is a placeholder for subclasses to do more advanced input reading.
Currently it just loads the entire L</input> attribute into L<_buffer>.
Returns true if it obtained more input, or false at the end of the input stream.

=cut

has _tokenizer      => ( is => 'rw', lazy => 1, builder => 1, init_arg => undef );
has _buffer         => ( is => 'rw', init_arg => undef, default => sub { '' } );
has _buffer_row_col => ( is => 'rw', init_arg => undef, default => sub { [0,0] } );
sub _buffer_ofs        { pos($_[0]{_buffer})= $_[1] if @_ > 1; pos($_[0]{_buffer}) }
has _token_ofs      => ( is => 'rw', init_arg => undef, default => sub { 0 } );

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

sub _combine_rule_regexes {
	my $rules= shift;
	my $re= join '|', map { $_->{pattern} } @$rules;
	# Do "global" matching to preserve position, and don't reset position on match failure
	return qr/\G(?^gc:$re)/;
}

sub _build_tokenizer_for_rules {
	my ($class, $rules)= @_;
	for (@$rules) {
		defined $_->{pattern} or croak "Scanner rule is missing pattern";
		defined $_->{handler} or croak "Scanner rule is missing handler";
		$_->{name} //= "$_->{pattern}";
		"" =~ /|$_->{pattern}/; # Bind to empty string to count capture groups
		$_->{capture_count}= $#+;
	}
	my $token_regex= _combine_rule_regexes($rules);
	return sub {
		my $self= shift;
		
		# Compare against all regexes at once
		$self->{_buffer} =~ $token_regex
			or return; # throws syntax error
		my $ofs= 0;
		for my $rule (@$rules) {
			my $end_ofs= $ofs + $rule->{capture_count} - 1;
			if (grep { defined } @-[$ofs .. $end_ofs]) {
				return $rule->{handler}->($self, map { substr($self->{_buffer}, $-[$_], $+[$_]-$-[$_]+1) } $ofs .. $end_ofs);
			}
			$ofs+= $rule->{capture_count};
		}
		croak "BUG: combined rule regex matched, but no captures were collected";
	};
}

our %_tokenizer_cache;
sub _build__tokenizer {
	my $self= shift;
	$_tokenizer_cache{ref $self} //= $self->_build_tokenizer_for_rules($self->_build_rules);
}

1;
