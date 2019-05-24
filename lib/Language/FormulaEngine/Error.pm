package Language::FormulaEngine::Error;
use Moo;
my @subclasses;
BEGIN {
	@subclasses= qw( ErrInval ErrNA ErrREF ErrNUM ErrNAME );
	# For each subclass name, export a function which can be used as the package
	# name or as the package constructor.
	for (@subclasses) {
		my $pkg= __PACKAGE__.'::'.$_;
		no strict 'refs';
		*$_= sub { @_? $pkg->new(@_) : $pkg }
	}
}
use Exporter 'import';
our @EXPORT_OK= @subclasses;
our %EXPORT_TAGS= ( all => \@EXPORT_OK );

# ABSTRACT: Exception objects for formula functions
# VERSION

=head1 DESCRIPTION

In keeping with the theme of spreadsheet formulas, this module collection provides exception
objects that can be used for similar exception handling.  These objects are intended to be
thrown using "die", but they can also be wrapped with a "trap" object so that they don't die
until used.  For example, in a spreadsheet the error is I<returned> from a function, but
anything that uses that value needs to generate a similar error.  That would require a lot of
argument checking and prevent using native Perl operations, but by returning an error wrapped
in a trap, any perl operation that attempts to use the trap will instead throw the exception
object.

=head2 ErrInval

The formula was given invalid inputs

=head2 ErrNA

The function encountered a condition where no value could be returned.  i.e. the function is
not defined for the supplied parameters, such as accessing elements beyond the end of an array.

=head2 ErrREF

The formula referenced a non-existent or nonsensical variable.

=head2 ErrNUM

The function expected a number (or specific range of number, like positive, integer, etc) but
was given something it couldn't convert.

=head2 ErrNAME

The formula uses an unknown function name.  This is thrown during compilation, or during
evaluation if the compile step is omitted.

=cut

has message => ( is => 'rw', required => 1 );

sub mine {
	return Language::FormulaEngine::ErrorMine->new($_[0]);
}

sub BUILDARGS {
	my $pkg= shift;
	return $_[0] if @_ == 1 && ref $_[0] eq 'HASH';
	# If odd number of arguments, and first is a scalar, then treat it as the message
	unshift @_, 'message' if @_ & 1 and !ref $_[0];
	return { @_ };
}

sub _fake_inc {
	(my $pkg= caller) =~ s,::,/,g;
	$INC{$pkg.'.pm'}= $INC{'Language/FormulaEngine/Error.pm'};
}

package Language::FormulaEngine::ErrorMine;
Language::FormulaEngine::Error::_fake_inc();


package Language::FormulaEngine::Error::ErrInval;
Language::FormulaEngine::Error::_fake_inc();
use Moo;
extends 'Language::FormulaEngine::Error';

package Language::FormulaEngine::Error::ErrNA;
Language::FormulaEngine::Error::_fake_inc();
use Moo;
extends 'Language::FormulaEngine::Error';

package Language::FormulaEngine::Error::ErrREF;
Language::FormulaEngine::Error::_fake_inc();
use Moo;
extends 'Language::FormulaEngine::Error';

package Language::FormulaEngine::Error::ErrNUM;
Language::FormulaEngine::Error::_fake_inc();
use Moo;
extends 'Language::FormulaEngine::Error::ErrInval';

package Language::FormulaEngine::Error::ErrNAME;
Language::FormulaEngine::Error::_fake_inc();
use Moo;
extends 'Language::FormulaEngine::Error';

1;
