#!/usr/bin/perl -w
use strict;

# Copyright (C) 2009  Matt Keller <mattkeller at gmail dot com>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

# etags-update.pl: Updates a TAGS file for a given set of files.
#
# Requires ctags in PATH.
#
# Algorithm:
# 1) Ensure the given TAGS file and the files to be updated exist
# 2) Copy the TAGS file to a tempfile, filtering out the entries for files to be updated
# 3) Run 'ctags -a' to append entries to the tempfile for each file to be updated
# 4) Overwrite the original TAGS file with the tempfile

my $tmpname = '';

sub usage {
print <<END;
Usage: $0 <tags-file> <file-to-update> ...
END
exit;
}

sub error {
  my $msg = shift;
  print STDERR "$msg\n";
  exit 1;
}

my $ctags_bin = `which ctags`;
chomp($ctags_bin);
error("Cannot find 'ctags' in your shell's PATH") if ($ctags_bin eq '');

usage() if (scalar @ARGV < 2);

my $TAGS = shift @ARGV;
error("Cannot find TAGS file: $TAGS") if ( ! -f $TAGS );

my %files;
foreach my $f (@ARGV) {
  chomp($f);
  error("Cannot find file: $f.") if (! -f $f);
  $files{$f} = 1;
}

# Open a temp file (not a perfect tempfile name alg, but good enough)
$tmpname = "${TAGS}.${$}.tmp";
open(TMP, ">${tmpname}") or error("Cannot create temp file: $tmpname");

# Copy TAGS to the temp file, stripping the entries for unwanted files
my $sectionStart = 0;
my $ignoreSection = 0;
open(TAGS, "${TAGS}") or error("Cannot open $TAGS");
while(my $line = <TAGS>) {
  if ($line =~ m/\cL/) { $sectionStart = 1; }
  elsif ($sectionStart) {
    my $f = $line;
    $f =~ s/,\d*$//;
    chomp($f);
    if ($files{$f}) {
      #print "Stripping $f entries\n";
      $ignoreSection = 1;
    }
    else {
      $ignoreSection = 0;
      print TMP "\n$line";
    }
    $sectionStart = 0;
  }
  elsif ($ignoreSection) { next; }
  else {
    print TMP $line;
  }
}
close(TMP);
close(TAGS);

# Use 'etags --append' to add entries for updated files
for my $f (keys %files) {
  #print "Appending $f entries\n";
  if ((system("$ctags_bin -e -o $tmpname -a $f") >> 8) != 0) {
    error("Unable to append entries for $f");
  }
}

# Overwrite $TAGS with temp file
if ((system("mv $tmpname $TAGS") >> 8) != 0) {
  error("Unable to overwrite $TAGS");
}

END { unlink $tmpname if ( -f $tmpname ); }

