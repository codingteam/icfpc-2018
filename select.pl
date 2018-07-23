#!/usr/bin/env perl

use warnings;
use strict;

use feature qw(say switch);
use 5.012;

use Cwd qw(abs_path getcwd);

if ($#ARGV < 2) {
  say "Usage: $0 output.zip input1.zip input2.zip [input.zip...]";
  say "";
  say "Compares solutions from input files, and puts the best one into output.zip";

  exit 1;
}

my $KEYFILE=abs_path('privatekey');
my $KEY;
if (-f "$KEYFILE") {
  $KEY = `cat $KEYFILE`;
  chomp($KEY);
} else {
  say "Keyfile missing, won't be able to decrypt input zips! Aborting.";
  say "";
  say "Please put team's private ID into \`privatekey' file in the root";
  say "of the repository.";

  exit 1;
};

my $MODELS = abs_path('problemsF');
if (! -d $MODELS) {
  say "Models missing! Aborting.";
  say "";
  say "Please unpack problemsF.zip into problemsF directory in the root ";
  say "of the repository.";

  exit 1;
};

system "stack build";

my $OUTPUT=shift;
$OUTPUT = abs_path($OUTPUT);

my $repo = getcwd;

my @INPUT_DIRS = ();
for my $input (@ARGV) {
  my $tmp = `mktemp -d`;
  chomp($tmp);
  push @INPUT_DIRS, $tmp;

  $input = abs_path($input);
  say $input;

  chdir $tmp;
  system "unzip -P $KEY $input";

  chdir $repo;
}

my $OUTPUT_DIR = `mktemp -d`;
chomp($OUTPUT_DIR);
opendir(my $dh, $INPUT_DIRS[0]);
while (readdir $dh) {
  my $filename = $_;

  next if $filename !~ /\.nbt$/;

  my $arguments = "";
  for my $tmp (@INPUT_DIRS) {
    $arguments = "$arguments $tmp/$filename";
  }

  my $problem_type = substr($filename, 0, 2);
  my $model = "";
  given ($problem_type) {
    when ("FA") {
      $model = $filename =~ s/\.nbt/_tgt.mdl/r;
    }

    when ("FD") {
      $model = $filename =~ s/\.nbt/_src.mdl/r;
    }

    when ("FR") {
      $model = $filename =~ s/\.nbt/_src.mdl/r;
    }

    default {
      say "Unknown problem type \"$problem_type\"! Aborting.";
      exit 1;
    }
  };
  $model = "problemsF/$model";

  my $best = `stack exec icfpc2018-exe -- select $model $arguments`;
  chomp($best);
  system "cp -v $best $OUTPUT_DIR"
}
closedir $dh;

chdir $OUTPUT_DIR;
system "zip --encrypt -P $KEY $OUTPUT *";
chdir $repo;
