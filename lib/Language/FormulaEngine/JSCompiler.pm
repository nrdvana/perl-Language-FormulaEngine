package Language::FormulaEngine::JSCompiler;
use Moo;
use Carp;
use Try::Tiny;
use namespace::clean;

# ABSTRACT: Compile a parse tree into JavaScript
# VERSION

=head1 DESCRIPTION

=head1 ATTRIBUTES

=head2 namespace

Namespace object to use for looking up functions, converting functions to JavaScript, and
symbolic constants.

=head2 error

After a failed call to C<compile>, this attribute holds the error message.

=head2 code_body

After compilation, this attribute holds the perl source code that was generated prior to being
wrapped with the coderef boilerplate.

=head2 dependencies

This is a set (hashref) of all non-native javascript functions that were referenced so far.

=head2 dependency_code

This contains the JavaScript code that declares all the functions that were referenced by
anything that was compiled.  The intent is to collect all common functions here so that this
block can be defined in the start of a javascript file, and then all other formaulas can
reference it.

=head2 js_function_prefix

This is a string which will be prefixed on each static function referenced by the compiled
code.  The default is C<< "FormulaEngine." >>, so a static function like C<compare> gets
written as C<< FormulaEngine.compare= function() { ... }; >>.  If the prefix does not contain
a dot, such as C<< "MyPrefix_" >> the function would be written as
C<< function MyPrefix_compare(){ ... } >>.

=head2 gen_var_access

   gen_var_access => sub { my ($compiler, $symbol_node)= @_; ... }

This attribute is a coderef or method name which will be called against the compiler object to
generate JavaScript to access a variable.

The default is to access attributes of C<this> assuming that the generated code was installed
as a method of an object.

=cut

has namespace => is => 'rw', trigger => 1;
has js_function_prefix => is => 'ro', default => 'FormulaEngine.';
has error => ( is => 'rw' );
has code_body => ( is => 'rw' );
has dependencies => is => 'rw', default => sub { {} };
sub dependency_code {
	my $self= shift;
	my $prefix= $self->js_function_prefix;
	my $set= $self->dependencies;
	if (index($prefix, '.') >= 0) {
		return join '', map "$prefix$_=function(){$set->{$_}};\n", keys %$set;
	} else {
		return join '', map "function $prefix$_(){$set->{$_}};\n", keys %$set;
	}
}

sub _trigger_namespace {
	my ($self, $newval)= @_;
	$self->_clear_js_generator_cache;
	$self->reset;
}

has gen_var_access => is => 'lazy', default => '_vars_from_this';
sub _vars_from_this {
	#my ($self, $node)= @_;
	'this.'.$_[1]->symbol_name;
}

has _js_generator_cache => ( is => 'lazy', clearer => 1, default => sub { {} } );

=head1 METHODS

=head2 compile( $parse_tree )

Compile a parse tree, returning an anonymous javascript function.  Any references to functions
will be immeditely looked up within the L</namespace>.  Any references to constants in the
L</namespace> will be inlined into the generated JavaScript.  Any other symbol is assumed to be
a variable, and will be looked up from the arguments to the function at runtime.  The
L</namespace> object controls how the variables are looked up.

=cut

sub compile {
	my ($self, $parse_tree)= @_;
	my $ret;
	$self->error(undef);
	$self->code_body(undef);
	try {
		$self->code_body($self->jsgen($parse_tree));
		$ret= 'function(){'.$self->code_body.'}';
	}
	catch {
		chomp unless ref $_;
		$self->error($_);
	};
	return $ret;
}

=head2 reset

Clear any results from the last compilation.  Returns C<$self>.

=cut

sub reset {
	my $self= shift;
	$self->dependencies({});
	$self->error(undef);
	$self->code_body(undef);
	$self;
}

=head2 jsgen( $parse_node )

Generate JavaScript for a parse node.

=cut

sub jsgen {
	my ($self, $node)= @_;
	if ($node->can('jsgen')) {
		return $node->jsgen($self);
	}
	elsif ($node->can('function_name')) {
		return $self->jsgen_function_node($node);
	}
	elsif ($node->can('symbol_name')) {
		return $self->jsgen_symbol_node($node);
	}
	elsif ($node->can('string_value')) {
		return $self->jsgen_string_literal($node->string_value);
	}
	elsif ($node->can('number_value')) {
		return $node->number_value+0;
	}
	else {
		die "Don't know how to compile node of type '".ref($node)."'\n";
	}
}

sub _validate_js_name {
	$_[1] =~ /^[A-Za-z_][A-Za-z0-9_]*(.[A-Za-z0-9_]+)*$/
		or die "'$_[1]' doesn't look safe enough for generated javascript\n";
}
sub _get_js_generator {
	my ($self, $name)= @_;
	my $info= $self->namespace->get_function($name)
		or die "No such function '$name'\n";
	# If a generator is given, nothing else to do.
	return $info->{js_generator} if $info->{js_generator};
	
	# Else need to create a generator for a static javascript function
	defined $info->{js_native}
		or die "Cannot compile function '$name'; no generator or native function given\n";
	# register the js_native as a dependency
	$self->dependencies->{$name}= $info->{js_native};
	$name= $self->js_function_prefix . $name;
	# For security, make reasonably sure that JavaScript will parse the function name
	# as a single piece.
	$self->_validate_js_name($name);
	# Create a generator that injects this function name
	return sub {
		$name . '(' . join(',', map $_[1]->perlgen($_), @{ $_[2]->parameters }) . ')'
	};
}

=head2 jsgen_function_node

Generate javascript for a Call node (representing a function call with argument list).

=cut

sub jsgen_function_node {
	my ($self, $node)= @_;
	my $name= $node->function_name;
	my $gen= $self->_js_generator_cache->{$name} ||= $self->_get_js_generator($name);
	return $gen->($self->namespace, $self, $node);
}

=head2 jsgen_symbol_node

Generate javascript for a Symbol node.  A symbol node might refer to a constant or a variable.
This queries the namespace to find out, and returns the appropriate JavaScript.

=cut

sub jsgen_symbol_node {
	my ($self, $node)= @_;
	my $name= $node->symbol_name;
	my $x= $self->namespace->get_constant($name);
	if (!defined $x) {
		# not a constant, treat as var access
		my $m= $self->gen_var_access;
		return $self->$m($node);
	}
	elsif (!ref $x) {
		# literal constant
		return $self->jsgen_literal($x);
	}
	elsif (ref $x eq 'SCALAR') {
		# literal javascript
		return $$x;
	}
	elsif (ref $x eq 'HASH' && (defined $x->{js_native} || $x->{js_generator})) {
		if (defined $x->{js_native}) {
			my $name= $self->js_function_prefix . $x->{js_native};
			# For security, make reasonably sure that JavaScript will parse the function name
			# as a single piece.
			$self->_validate_js_name($name);
			return $name.'()';
		}
		else {
			return $x->{js_generator}->($self->namespace, $self, $node);
		}
	} else {
		die "Can't interpret ".ref($x)." as a javascript literal";
	}
}

=head2 jsgen_string_literal

Generate a JavaScript string literal.  This wraps the string with double-quotes and escapes control
characters and C<["\\]> using hex-escape notation.  Unicode is passed through un-altered.

=cut

sub jsgen_string_literal {
	my ($self, $string)= @_;
	$string =~ s/([\0-\x1F\x7f"\\])/ sprintf("\\x%02x", ord $1) /gex;
	return qq{"$string"};
}

=head2 jsgen_literal

If the scalar can be exactly represented by a javascript numeric literal, this returns that
literal, else it wraps the string with qoutes using L</jsgen_string_literal>.

=cut

sub jsgen_literal {
	my ($self, $string)= @_;
	no warnings 'numeric';
	return ($string+0) eq $string? $string+0 : $self->jsgen_string_literal($string);
}

1;
