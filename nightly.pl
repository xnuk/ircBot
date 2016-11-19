use strict;
use warnings;

die unless `curl --head "https://www.stackage.org/nightly"` =~ m|\nLocation: /(nightly-[\d-]+)|;
my $x=$1;
my $str;
{
	local $/ = undef;
	open(my $fh, "<", "stack.yaml") or die;
	$str = <$fh>;
	close $fh;
}
open(my $out, ">", "stack.yaml") or die;
open(my $in, "<", \$str) or die;
while(<$in>){
	s/^(resolver: )[^\s]*/$1$x/;
	print $out $_
}
close($out);
close($in);
