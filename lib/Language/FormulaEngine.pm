package Language::FormulaEngine;
use Moo;
use Carp;
use Try::Tiny;
use Module::Runtime 'require_module';

our $VERSION= '0.900';

# ABSTRACT - Extensible parser/compiler for simple expression language like used for spreadsheets

=head1 SYNOPSIS

  my $vars= { foo => 1, bar => 3.14159265358979, baz => 42 };
  
  my $engine= Language::FormulaEngine->new();
  my $formula= $engine->compile( 'if(foo, round(bar, 3), baz*100)' );
  print $formula->evaluate($vars);
  
  
  package MyContext {
    use Moo;
    extends 'Language::FormulaEngine::Compiler';
    sub fn_customfunc { shift; print "arguments are ".join(', ', @_)."\n"; }
  };
  my $engine= Language::FormulaEngine->new(compiler => MyContext->new);
  my $formula= $engine->compile( 'CustomFunc(baz,2,3)' );
  $formula->($vars); # prints "arguments are 42, 2, 3\n"

=head1 DESCRIPTION

This class is a parser and code generator for a simple-ish expression language.

The intent of this module is to help you write domain-specific languages for
"power users" who are not programmers (or maybe just not trusted) to give them the
ability to implement custom calculations or boolean logic in their system without
pestering you constantly for code changes or endless additional checkbox-activated
features.

The default language is pure-functional, in that each operation has exactly one return
value, and cannot modify variables.  There is no assignment, loops, or nested data
structures, though these can be added if you try hard enough.  (but if you want them,
there are L<some other modules|/"SEE ALSO"> you might consider.)

See L<Language::FormulaEngine::Parser> for the grammer details of the default language.
The default language is not Excel-compatible, but could be made so with a little
effort by someone who is intimately familiar with Excel.

The language is written with security in mind, and (until you start making changes)
should be safe for most uses, since the functional design promotes O(1) complexity
and shouldn't have side effects on the data structures you expose to the user.
It does use C<eval> though, so you should do an audit for yourself if you plan to
use it where security is a concern.

=head2 Features:

=over

=item *

Standard design with full scanner, parser, syntax tree, and compiler.

=item *

Generates perl coderefs for fast repeated execution

=item *

Provides metadata about what it compiled

=item *

Designed for extensibility

=item *

Light-weight, few dependencies, clean code

=item *

Top-down parse, which is easier to work with and gives helpful error messages,
though could get a bit slow if you extend the grammar too much.
(for simple grammars like this, it's pretty fast)

=back

=head1 ATTRIBUTES

=head2 parser

A parser for the language.  Responsible for tokenizing the input and building the
parse tree.  You can initialize this attribute with a class instance, a class name,
or arguments for the default parser.

Defaults to an instance of L<Language::FormulaEngine::Parser>.

=head2 compiler

A compiler for the parse tree.  Responsible for generating Perl code.
You can initialize this attribute with a class instance, a class name,
or arguments for the default parser.

The compiler object also holds the library of functions provided to the user.
Note that compiled formula may hold references to the compiler object, so never
store references to formula into the compiler object (or you end up with a memory
leak).

Defaults to an instance of L<Language::FormulaEngine::Compiler>

=cut

has parser           => ( is => 'lazy', builder => sub {}, coerce => sub { _coerce_instance($_[0], 'parse', 'Language::FormulaEngine::Parser')    } );
has compiler         => ( is => 'lazy', builder => sub {}, coerce => sub { _coerce_instance($_[0], 'compile', 'Language::FormulaEngine::Compiler')  } );

=head1 METHODS

=head2 compile

  my $formula= $fe->compile( $formula_text );
  # or to trap exceptions...
  my $formula= $fe->compile( $formula_text, $error_result );

Returns a L<Language::FormulaEngine::Formula> object for this
text.  May throw a variety of exceptions if the formula contains syntax errors
or references to missing functions.

If you specify the optional second argument and the formula has an error, the
variable passed as $error_result will receive the error and the function returns
undef instead of dying.

=cut
sub compile {
	# If they want to capture the error, wrap with try/catch
	if (@_ == 3) {
		my ($err, $formula);
		try {
			$formula= $_[0]->compile($_[1]);
		} catch {
			$err= $_;
			chomp($err) unless ref $err;
		};
		$_[2]= $err unless defined $formula;
		return $formula;
	}
	
	# else let exceptions fly.
	my ($self, $text)= @_;
	my $parse_result= $self->parser->parse($text);
	my $perl_code= $self->compiler->compile($parse_result->{parse_tree});
	my $coderef= $self->compiler->_clean_eval($perl_code)
		or croak "Generated invalid code:\n\t$perl_code\n\t$@";
	return Language::FormulaEngine::Formula->new(
		orig_text    => $text,
		var_deps     => $parse_result->{var_deps},
		parse_tree   => $parse_result->{parse_tree},
		perl_code    => $perl_code,
		coderef      => $coderef,
	);
}

sub _coerce_instance {
	my ($thing, $req_method, $default_class)= @_;
	return $thing if ref $thing and ref($thing)->can($req_method);
	
	my $class= !(defined $thing || ref $thing)? $default_class : $thing;
	use DDP;
	my $x= [ $thing, $req_method, $default_class, $class ];
	p $x;
	require_module($class)
		unless $class->can('new');
	
	my @args= !ref $thing? () : (ref($thing) eq 'ARRAY')? @$thing : ();
	return $class->new(@args);
}

=head1 CUSTOMIZING THE LANGUAGE

The module is called "FormulaEngine" in part because it is designed to be
customized. The functions are easy to extend, the variables are somewhat easy
to extend, the compilation can be extended after a little study of the API,
and the grammar itself can be extended with some effort.

If you are trying to addd LOTS of functionality, you might be starting with
the wrong module.  See the notes in the L</"SEE ALSO"> section.

=head2 Adding Functions

You can easily add custom functions to the language by subclassing the compiler
and ading methods with the appropriate name.  See the L<compiler documentation|Language::FormulaEngine::Compiler/DESCRIPTION>
for details.

=head2 Overriding Operators

You can also override the behavior of the basic operators by defining methods
on your compiler with names like 'C<fn_sum>', 'C<fn_mul>', 'C<fn_div>',
'C<fn_compare>' and so on.  In other words, each operator is an alias for a
named function.  If you define one of these methods, it will by called
instead of using the built-in default.  L<The parser|Language::FormulaEngine::Parser>
documents the functions for each operator.

=head2 Complex Data Structures

All variables are simple names looked up in a single hash, though the names can
contain "." to give the illusion of data structures.  I chose this default because
it is safe and fast.

If you really do want to allow the user to walk data structures, call methods,
allow "vmethods" like in L<template toolkit|Template>, or so on, you can simply
tie the hash that you pass as an argument to evaluate().  The other way is to
subclass the method in the compiler that inlines the "VarRefNode" objects.

=head2 Altering Syntax or Grammar

You can change how the text is tokenized by subclassing the L<parser|Language::FormulaEngine::Parser>.
(It is composed of separate Scanner and Parser steps, but as a single class since
if you want to subclass one you probably want to subclass both)

=cut

package Language::FormulaEngine::Formula;

=head1 FORMULA OBJECT

Formula has the following attributes/methods

=head2 orig_text

Read-only. The original text of the formula before parsing.

=head2 parse_tree

Read-only. The tree of ParseNode objects describing the syntax of the formula.

=head2 var_deps

Read-only. The set of variables this formula depends on.

=head2 perl_code

Read-only. The string of perl source code generated from this formula.

=head2 coderef

Read-only. An executable perl subroutine compiled from ->perl_code.

=cut

use Moo;
use Carp;
use Try::Tiny;
use overload '""' => \&to_string, 'bool' => \&to_bool;

has orig_text  => ( is => 'ro' );
has parse_tree => ( is => 'ro' );
has var_deps   => ( is => 'ro' );
has perl_code  => ( is => 'ro' );
has coderef    => ( is => 'ro' );

=head2 dependencies

Read-only.  For the PartTemplate API.  Same as var_deps, but returns an
arrayref instead of a set-hash.

=head2 dependency_list

List accessor for dependencies

=head2 have_dependencies

  if ($formula->have_dependencies(\%variables)) {
    ...

Convenience method to check whether all dependencies are present

=head2 evaluate( \%vars )

Evaluate the formula on the given set of variables.

=cut

sub get_dependencies {
	my ($self, $dep_set)= @_;
	if ($_[0]->var_deps) {
		$dep_set->{$_}= 1 for keys %{$_[0]->var_deps}
	}
}

sub dependency_list {
	return $_[0]->var_deps? keys %{$_[0]->var_deps} : ()
}

sub have_dependencies { # HOT method
	my ($self, $vars)= @_;
	my $deps= $self->var_deps;
	return 1 unless $deps;
	defined $vars->{$_} or return 0
		for keys %$deps;
	return 1;
}

sub evaluate { # HOT method
	my ($self, $vars)= @_;
	return $self->coderef->($vars);
}

=head2 to_string

This object stringifies into the original formula text.

=head2 to_bool

Formula objects are always true.  (needed because to_string can return '0'
for the formula of the constant zero.)

=cut

sub to_string {
	$_[0]->orig_text;
}
sub to_bool {
	1
}

=head1 SEE ALSO

=over

=item L<Language::Expr>

A bigger more featureful expression language; perl-like syntax and data structures.
Also much more complicated and harder to customize.
Can also compile to Javascript!

=item L<Math::Expression>

General-purpose language, including variable assignment and loops, arrays,
and with full attention to security.  However, grammar is not customizable at all,
and math-centric.

=item L<Math::Expression::Evaluator>

Very similar to this module, but no string support and not very customizable.
Supports assignments, and compilation.

=item L<Math::Expr>

Similar expression parser, but without string support.
Supports arbitrary customization of operators.
Not suitable for un-trusted strings, according to BUGS documentation.

=back

=cut

1;
