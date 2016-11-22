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

if(system("git diff --no-ext-diff --quiet --exit-code")){
	system("git add stack.yaml") ||
	system("git commit -m 'bump stack resolver version'") ||
	system("git fetch") ||
	system("git rebase origin master") ||
	system("git push");
	die "updated resolver to $x"
}
