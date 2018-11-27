#!/usr/bin/perl -n

# Change [from] (signature param ...)
# to     <to>(signature param ...)</to>
# adding a <br> at the end of the line if the next line
# has data on it, due to the wiki not adding a newline automatically

BEGIN {
    die "usage: $0 from-tag to-tag files...\n" unless @ARGV > 2;
    $from = shift;
    $to = shift;
}

if (defined $last) {
    $last =~ s/$/<br>/ if /\S/;
    print $last;
}
if (s:^ \[$from\] (.+)$:<$to>\1</$to>:) {
    $last=$_;
} else {
    undef $last; print $_;
}

END { print $last if defined $last; }
