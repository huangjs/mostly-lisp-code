#!/usr/bin/perl -w

use strict;
use warnings;

my $kernel = -1;
my $degree = 3;
my $gamma  = 0.5;
my $coef   = 0;
my $b      = 0;
my $fac    = 1;

while (<STDIN>)
{
    chomp;

    if (/kernel_type\s+(\S+)/)
    {
        $kernel = 0 if ($1 eq 'linear');
        $kernel = 1 if ($1 eq 'polynomial');
        $kernel = 2 if ($1 eq 'rbf');
        $kernel = 3 if ($1 eq 'sigmoid');
    }
    elsif (/nr_class\s+(\d+)/)
    {
        die "Presently only binary classification is supported\n" if ($1 != 2);
    }
    else
    {
       $degree = $1 if (/degree\s+([+-]?(\d+\.\d+|\d+\.|\.\d+|\d+)([eE][+-]?\d+)?)/);
       $gamma  = $1 if (/gamma\s+([+-]?(\d+\.\d+|\d+\.|\.\d+|\d+)([eE][+-]?\d+)?)/);
       $coef   = $1 if (/coef0\s+([+-]?(\d+\.\d+|\d+\.|\.\d+|\d+)([eE][+-]?\d+)?)/);

       $b      = $1 if (/rho\s+([+-]?(\d+\.\d+|\d+\.|\.\d+|\d+)([eE][+-]?\d+)?)/);

       if (/label\s+([+-]?1)\s+([+-]?1)\s*/)
       {
	   die "Presently only binary classification is supported\n"
	       if (abs($1) != 1 || abs($2) != 1);

	   $fac = -1 if ($1 < $2);
       }

       last if (/^\s*SV/);
     }
}

$b = -$fac * $b;

if ($kernel == 0)
{
    print "k[x_,y_]:=Sum[x[[i]]*y[[i]],{i,1,Length[x]}];\n";
}
elsif ($kernel == 1)
{
    print "k[x_,y_]:=($gamma*Sum[x[[i]]*y[[i]],{i,1,Length[x]}]+$coef)^$degree;\n";
}
elsif ($kernel == 2)
{
    print "k[x_,y_]:=Exp[-$gamma*Sum[(x[[i]]-y[[i]])^2,{i,1,Length[x]}]];\n";
}
elsif ($kernel == 3)
{
    print "k[x_,y_]:=Tanh[$gamma*Sum[x[[i]]*y[[i]],{i,1,Length[x]}]+$coef];\n";
}
else
{
    die "Only kernels linear, polynomial, rbf and sigmoid are supported\n";
}

my @alphay;
my @xlist;
my $dim = 0;
my $n = 0;

while (<STDIN>)
{
    chomp;

    if (/^\s*([+-]?(\d+\.\d+|\d+\.|\.\d+|\d+)([eE][+-]?\d+)?)/)
    {
        $alphay[$n] = $fac * $1;

        while (/(\d+)\:([+-]?(\d+\.\d+|\d+\.|\.\d+|\d+)([eE][+-]?\d+)?)/g)
	{
	    if ($1 > 0)
	    {
	        $dim = $1 if ($1 > $dim);

	        $xlist[$n][$1-1] = $2;
	    }
	    else
	    {
	        print STDERR "Invalid column index $1\n";
	    }
	}

	$n++;
    }
}

exit(2) if ($dim == 0 || $n == 0);

print "b=$b;\n";
print 'alphay={',join(',',@alphay),"\};\n";

my @vectorlist;

for (my $i = 0; $i < $n; $i++)
{
    for (my $j = 0; $j < $dim; $j++)
    {
        $xlist[$i][$j] = 0 if (!(exists $xlist[$i][$j]) || $xlist[$i][$j] eq '');
    }

    my $vector = join(',',@{$xlist[$i]});

    push(@vectorlist, "\{$vector\}");
}

print 'xlist={',join(',',@vectorlist),"\};\n";
