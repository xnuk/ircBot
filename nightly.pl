use strict;
use warnings;

die unless `curl --head "https://www.stackage.org/nightly"` =~ m|\nLocation: /(nightly-[\d-]+)|;
`sed -ir 's/^(resolver: ).*\$/\\1$1/' stack.yaml`
