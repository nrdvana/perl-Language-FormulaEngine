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

=cut

has variables            => ( is => 'rw', default => sub { +{} } );
has constants            => ( is => 'rw', default => sub { +{} } );
has die_on_unknown_value => ( is => 'rw' );

=head1 METHODS

=head2 clone

  my $ns2= $ns1->clone(variables => \%different_vars);

Return a copy of the namespace, optionally with some attributes overridden.

=head2 clone_and_merge

  my $ns2= $ns1->clone_and_merge(variables => \%override_some_vars);

Return a copy of the namespace, with any new attributes merged into the existing ones.

=cut

sub clone {
	my $self= shift;
	my %attrs= @_==1 && ref $_[0] eq 'HASH'? %{$_[0]} : @_;
	$attrs{variables} ||= { %{ $self->variables } };
	$attrs{constants} ||= { %{ $self->constants } };
	$self->new( %$self, %attrs );
}

sub clone_and_merge {
	my $self= shift;
	my %attrs= @_==1 && ref $_[0] eq 'HASH'? %{$_[0]} : @_;
	$attrs{variables}= { %{ $self->variables }, %{ $attrs{variables}||{} } };
	$attrs{constants}= { %{ $self->constants }, %{ $attrs{constants}||{} } };
	$self->new( %$self, %attrs );
}

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
the versioned sub-classes, such as ::V0, ::V1, etc.
See L<Language::FormulaEngine::Namespace::Default> for a list of all versioned functions.

=cut

1;
