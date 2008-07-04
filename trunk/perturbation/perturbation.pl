#!/usr/bin/perl

use strict;

open WORDS, "/usr/share/dict/words" or die $!;

open PERTURBATIONS, "> perturbations.txt";

my @orig_words;
my %sorted_to_unsorted; # a hash of arrayrefs

my $num_words_with_perturbations = 0;
my $total_perturbations = 0;

foreach my $word (<WORDS>) {
	chomp $word;
	push @orig_words, $word;
	
	my $sorted_word = join( '', sort( split( //, $word )));
	
	if (! $sorted_to_unsorted{$sorted_word}) {
		$sorted_to_unsorted{$sorted_word} = [];
	}
	
	push @{ $sorted_to_unsorted{$sorted_word} }, $word;
}

foreach my $word (@orig_words) {
	chomp $word;
		
	next if $word !~ /m/;
	
	my $rb_word = $word;
	$rb_word =~ s/m//;
	$rb_word = $rb_word . 'rb';

	my $sorted_rb_word = join( '', sort( split( //, $rb_word )));
	
	if ( $sorted_to_unsorted{$sorted_rb_word} > 0 ) {
			$num_words_with_perturbations++;
			
			print PERTURBATIONS "$word perturbs to:\n";
			foreach my $perturbation ( @{ $sorted_to_unsorted{$sorted_rb_word} }) {
				$total_perturbations++;
				print PERTURBATIONS "\t$perturbation\n";
			}
			print "\n";
	}
}

print "$num_words_with_perturbations words have perturbations.  ";
print "There are $total_perturbations perturbations in all.\n";
