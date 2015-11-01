#! /usr/bin/perl -w
#
# Generate a call graph from static analysis of a set of object files
# ... it used to work once in the mid 2000s on a Debian Sarge box.
# Needs objdump and c++filt (both parts of gnu binutils).
#
# Copyright (c) 2008 M. Joonas Pihlaja
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

use strict;
use warnings;

package sym;

# objdump symbol flags field
# ix	values or ' '
# 0	'!': local & global, 'l': local, 'g': global
# 1	'w': weak
# 2	'C': constructor
# 3	'W': warning
# 4	'I': indirect
# 5	'd': debugging, 'D': dynamic
# 6	'F': function, 'f': file, 'O': object

use constant LOCAL => 1;	# symbol is local
use constant GLOBAL => 2;	# symbol is global
use constant WEAK => 4;		# symbol is weak
use constant CONSTRUCTOR => 8;	# symbol is a constructor
use constant WARNING => 16;
use constant DEBUG => 32;
use constant DYNAMIC => 64;	# symbol is a dynamic symbol

# Mutually exclusive flags.
use constant FUNCTION => 128;
use constant FILE => 256;
use constant OBJECT => 512;

use constant INDIRECT => 1024;

use constant _ID => 0;		# globally unique id of this symbol.
use constant _FLAGS => 1;	# 
use constant _NAME => 2;	# 
use constant _VAL => 3;		# value of this symbol
use constant _SZ => 4;		# size of this symbol
use constant _SEC => 5;
use constant _USES => 6;
use constant _USEDBY => 7;

my $sym_id_seq = 0;

sub new {
    my $class = shift; $class = ref($class) || $class;
    my ($section, $name, $flags, $value, $size) = @_;
    my $self = [ $sym_id_seq++,
		 $flags,
		 $name,
		 $value,
		 $size,
		 $section,
		 {}, {} ];
    return bless $self, $class;
}
    
sub id { $_[0]->[_ID]; }
sub name { $_[0]->[_NAME]; }
sub value { $_[0]->[_VAL]; }
sub size { $_[0]->[_SZ]; }
sub section { $_[0]->[_SEC]; }
sub flags { return $_[0]->[_FLAGS] if @_ < 2;
	    $_[0]->[_FLAGS] & $_[1]; }
sub uses {
    my ($self,$sym) = @_;
    $self->[_USES]{"$sym"} = $sym if $sym;
    return $self->[_USES];
}

sub usedby {
    my ($self,$sym) = @_;
    $self->[_USEDBY]{"$sym"} = $sym if $sym;
    return $self->[_USEDBY];
}

package symtab;
sub new {
    my $class = shift; $class = ref($class) || $class;
    return bless {
	syms => {},		# maps symbol ids to symbols.
	global => {},		# global and non-local weak symbols by name.
	local => {}
    }, $class;
}

sub add {
    my ($self, $sym) = @_;
    #print $sym;
    #die "can't insert non-symbol" unless ref($sym) == "sym";
    $self->{syms}{$sym->id()} = $sym;
    my $name = $sym->name();
    if (0 == ($sym->flags() & sym::LOCAL)) {
	if (!exists($self->{global}{$name})) {
	    #print "<GLOBAL>";
	    $self->{global}{$name} = $sym;
	}
	else {
	    die "duplicate non-local symbol '".$sym->name()."'";
	}
    }
    else {
	#print "<LOCAL>";
	if (!exists($self->{local}{$name})) {
	    $self->{local}{$sym->name()} = $sym;
	}
	else {
	    #print STDERR "duplicate local symbol '".$sym->name()."'\n";
	}
    }
    return $self;
}

package addrmap;
sub new {
    my $class = shift; $class = ref($class) || $class;
    my ($vma) = @_;
    my $self = {
	vma => $vma,
	tree => {},
    };
    return bless $self, $class;
}

sub intcmp {
    $_[0] <=> $_[1];
}

sub symadd {
    my ($self, $sym) = @_;
    my $tree = $self->{tree};
    my $key = $sym->value;
    $tree->{$key} = [] unless $tree->{$key};
    push @{$tree->{$key}}, $sym;
}

sub find_by_ofs {
    my ($self, $ofs) = @_;
    my $tree = $self->{tree};
    my @syms = ();
    for (values %$tree) {
	for (@$_) {
	    if ($ofs >= $_->value && $ofs < $_->value+$_->size) {
		push @syms, $_;
	    }
	}
    }
    return @syms;
}

sub find_by_addr {
    my ($self, $addr) = @_;
    return $self->find_by_ofs($addr - $self->{vma});
}

package reloc;
sub new {
    my $class = shift; $class = ref($class) || $class;
    my ($type, $sec, $secofs, $symname) = @_;
    my $self = [ $sec, $secofs, $symname, [], $type ];
    return bless $self, $class;
}

sub section { return $_[0]->[0]; }
sub section_ofs { $_[0]->[1]; }
sub symbol_name { $_[0]->[2]; }
sub syms { $_[0]->[3]; }
sub type { $_[0]->[4]; }

package section;
sub new {
    my $class = shift; $class = ref($class) || $class;
    my ($obj, $name, $size, $vma, $lma, $fileofs) = @_;
    my $self = {
	obj => $obj,
	name => $name,
	size => $size,
	vma => $vma,
	lma => $lma,
	fileofs => $fileofs,
	flags => {},
	amap => addrmap->new($vma),
	relocs => {},		# maps a section offset to a reloc
    };
    return bless $self, $class;
}

sub flag {
    my ($self,$flag, $newval) = @_;
    my $val = $self->{flags}{$flag} || 0;
    if (defined($newval)) {
	$self->{flags}{$flag} = $newval;
    }
    return $val;
}

package obj;
sub new {
    my $class = shift; $class = ref($class) || $class;
    my $objname = shift;
    my $self = {
	name => $objname,
	symtab => symtab->new(),
	sections => {},
	fp => undef,
    };
    $self = bless $self,$class;
    return $self;
}

sub symtab { $_[0]->{symtab}; }

sub symadd {
    my ($self, $sym) = @_;
    $self->symtab->add($sym);
    return $self;
}

sub getsection {
    my ($self, $secname) = @_;
    $self->{sections}{$secname};
}

sub read_syms {
    my $self = shift;
    my $n = $self->{name};
    for (`objdump -t $n`) {
	if (/^([[:xdigit:]]+) (.......)\s+(\S+)\s+([[:xdigit:]]+)\s*(.*)$/) {
	    my $value = hex($1);
	    my $flags = decode_objdump_sym_flags($2);
	    my $secname = $3;
	    my $size = hex($4);
	    my $name = $5;
	    my $sec = $self->getsection($secname);

	    # Filter here the kinds of symbols we accept.  The main
	    # tenet is that only defined symbols are accepted at this
	    # point.
	    next if $secname eq '*UND*';
	    my $sec_is_loaded = !defined($sec) || $sec->flag('LOAD') || $sec->flag('ALLOC');
	    next unless $sec_is_loaded;
	    $name = $secname unless $name;

	    #print "$name $2 $flags";print "\n";
	    

	    # Ok, got a good one: either an absolute symbol, a section,
	    # or an honest to god symbol in a section.
	    my $sym = sym->new($sec, $name, $flags, $value, $size);
	    $self->symadd($sym);
	    my $xxx = $self->find_sym_global($name); $xxx = "NONONONONO" unless $xxx;
	    #print "Symbol $name $sec_is_loaded $sym $xxx\n";
	}
    }
}

sub decode_objdump_sym_flags {
    my $str = shift;
    my @f = split("", $str);
    push @f, " " while @f < 7;

    my $flags = 0;
    $flags |= sym::LOCAL if $f[0] eq "!";
    $flags |= sym::LOCAL if $f[0] eq "l";
    
    $flags |= sym::GLOBAL if $f[0] eq "!";
    $flags |= sym::GLOBAL if $f[0] eq "g";

    $flags |= sym::WEAK if $f[1] eq "w";
    $flags |= sym::CONSTRUCTOR if $f[2] eq "C";
    $flags |= sym::WARNING if $f[3] eq "W";
    $flags |= sym::INDIRECT if $f[4] eq "I";
    $flags |= sym::DEBUG if $f[5] eq "d";
    $flags |= sym::DYNAMIC if $f[5] eq "D";
    $flags |= sym::FUNCTION if $f[6] eq "F";
    $flags |= sym::OBJECT if $f[6] eq "O";
    $flags |= sym::FILE if $f[6] eq "f";
    return $flags;
}

sub read_sections {
    my $self = shift;
    my $n = $self->{name};
    my $get_flags = 0;
    my $sec = undef;
    for (`objdump -h $n`) {
        if (/^\s*(\d+)\s+(\S+)\s+([[:xdigit:]]+)\s+([[:xdigit:]]+)\s+([[:xdigit:]]+)\s+([[:xdigit:]]+)/) {
            my ($secno, $name, $size, $vma, $lma, $fileofs)= ($1,$2,hex($3),hex($4),hex($5),hex($6));
	    $sec = section->new($self, $name, $size, $vma, $lma, $fileofs);
	    $self->{sections}{$name} = $sec;
	    $get_flags = 1;
        }
	elsif ($get_flags) {
	    s/,/ /g;  s/\s+/ /g;
	    s/^\s*//; s/\s*$//;
	    for (split) {
		$sec->{flags}{$_} = 1;
	    }	    
	}
	else {
	    $get_flags = 0;
	}
    }
}

sub sec_read32 {
    my ($self, $secname, $secofs) = @_;
    my $buf = "";
    my $sec = $self->{sections}{$secname};
    die "no such section $secname" unless $sec;
    die "offset $secofs out of bounds [0,$sec->{size}) for section $secname"
	if $secofs < 0 || $secofs+4 > $sec->{size};
    seek($self->{fp}, $sec->{fileofs} + $secofs, 0) or die "failed to seek: $!";
    read($self->{fp}, $buf, 4) == 4
	or die "failed to read: $!";
    return unpack("l", $buf);
}

sub map_sections {
    my $self = shift;
    $self->{absmap} = addrmap->new(0x00000000);
    for (values %{$self->symtab->{syms}}) {
	my $sec = $_->section;
	my $amap = defined($sec) ? $sec->{amap} : $self->{absmap};
	$amap->symadd($_);
    }
}

sub find_syms_by_section_ofs {
    my ($self, $secname, $ofs) = @_;
    my $sec = $self->{sections}{$secname};
    my $amap = $sec ? $sec->{amap} : $self->{absmap};
    return $amap->find_by_ofs($ofs);
}

sub find_sym_global {
    my ($self, $name) = @_;
    $self->symtab->{global}{$name};
}

sub find_sym {
    my ($self, $name) = @_;
    my $s = $self->find_sym_global(@_);
    return $s if $s;
    $self->symtab->{local}{$name};
}

sub read_relocs {
    my $self = shift;
    my $n = $self->{name};
    my $sec = undef;
    for (`objdump -r $n`) {
	if (/^RELOCATION RECORDS FOR \[(\S+)\]:/) {
	    $sec = $self->{sections}{$1};
	    die "no such section $1" unless $sec;
	}
	elsif (/^([[:xdigit:]]+)\s+(\S+)\s+(\S+)/) {
	    die "no current section" unless $sec;
	    my ($ofs, $type, $symname) = (hex($1), $2, $3);
	    my $reloc = reloc->new($type, $sec, $ofs, $symname);
	    die "reloc slot at offset $ofs of section $sec->{name} already filled"
		if defined($sec->{relocs}{$ofs});
	    $sec->{relocs}{$ofs} = $reloc;
	}
    }
}

sub dprint {
    #print @_;
}

sub lookup {
    my ($search_order, $symname) = @_;
    my $done_locals = 0;
    my $weak_sym = undef;
    for my $obj (@$search_order) {
	dprint("\nLOOKUP IN ", $obj->{name});
	if (defined($obj)) {
	    my $sym;
	    if ($done_locals) {
		$sym = $obj->find_sym_global($symname);
	    }
	    else {
		$sym = $obj->find_sym($symname);
	    }
	    dprint(" (nope)") unless $sym;
	    if (defined($sym)) {
		if ($sym->flags() & sym::WEAK) {
		    if ($done_locals) {
			$weak_sym = $sym unless defined $weak_sym;
		    }
		}
		else {
		    dprint " resolved to symbol $sym";
		    return $sym;
		}
	    }
	}
	$done_locals = 1;
    }
    dprint " resolved to weak symbol $weak_sym" if $weak_sym;
    return $weak_sym;
}


sub resolve_relocs {
    my $self = shift;
    my $search_order = shift;
    for my $sec (values %{$self->{sections}}) {
	my $sec_is_loaded = !defined($sec) || $sec->flag('LOAD') || $sec->flag('ALLOC');
	next unless $sec_is_loaded;
	dprint "\nResolving relocations for section $sec->{name}\n";
	for my $reloc (values %{$sec->{relocs}}) {
	    my $reloc_ofs = $reloc->section_ofs;
	    my $reloc_type = $reloc->type;
	    my $symname = $reloc->symbol_name;

	    dprint "\n$reloc_ofs\t$reloc_type\t$symname";

	    my $sym = lookup($search_order, $symname);
	    next unless $sym;		# can't resolve symbol? 's okay.
	    dprint ".";
	    my $reloc_info = $self->sec_read32($sec->{name}, $reloc_ofs);

	    my $dst_sec = $sym->section;
	    next unless $dst_sec;
	    my $dst_ofs = $reloc_info;
	    dprint ".";

	    if ($reloc_type eq "R_386_32") {
		$dst_ofs += $sym->value;
		dprint " -> ".$dst_sec->{name}.":".$sym->name()."+".$dst_ofs;
	    }
	    elsif ($reloc_type eq "R_386_PC32") {
		die "destination section $dst_sec->{name} not a code section"
		    unless $dst_sec->flag('CODE');
		$dst_ofs += 4;
		$dst_ofs += $sym->value;
		dprint " -> ".$dst_sec->{name}.":".$sym->name()."+".$dst_ofs;
	    }
	    else {
		die "unknown reloc type $reloc_type";
	    }

	    my @dst_syms = $dst_sec->{amap}->find_by_ofs($dst_ofs);
	    for my $dstsym (@dst_syms) {
		my @refering_syms = $self->find_syms_by_section_ofs($sec->{name}, $reloc_ofs);
		for my $refsym (@refering_syms) {
		    $refsym->uses($dstsym);
		    dprint("\n".$refsym->name().' ===> '.$dstsym->name());
		    $dstsym->usedby($refsym);
		}
	    }
	}
	dprint "\n";
    }
}

sub read_objdump {
    my $self = shift;
    open $self->{fp}, '<', $self->{name} or die "can't open $self->{name}: $!";

    $self->read_sections;
    $self->read_syms;
    $self->map_sections;
    $self->read_relocs;
}

package main;

my @objnames = @ARGV;
my @objs = ();
for (@objnames) {
    my $obj = obj->new($_);
    $obj->read_objdump;
    push @objs, $obj;
}

for (@objs) {
    $_->resolve_relocs([$_, @objs]);
}

my ($nodes_arcs, $node_attrs) = form_graph(@objs);
emit_graph($nodes_arcs, $node_attrs);

my %cxx_names = ();
sub transform_name {
    my $name = shift;
    if (!exists($cxx_names{$name})) {
	if ($name =~ /^_/) {
	    my $a = `c++filt -p $name`;
	    $a=~s/^\s*//;
	    $a=~s/\s*$//;
	    $a=~s/::/::\\n/;
	    #print "'$name': '$a'\n";
	    $cxx_names{$name} = $a;
	}
	else {
	    $cxx_names{$name} = $name;
	}
    }
    $cxx_names{$name};
}

sub form_graph {
    my @objs = @_;
    my %arcs = ();
    for my $obj (@objs) {
	for my $sym (values %{$obj->{symtab}->{syms}}) {
	    next if $sym->flags() & sym::OBJECT;
	    next if ($sym->flags & sym::WEAK) && ((keys %{$sym->usedby()}) == 0);
	    my $name = transform_name($sym->name);
	    next if $name =~ /^std::/;
	    
	    for my $used (values %{$sym->uses()}) {
		next if $used->flags() & sym::OBJECT;
		my $name2 = transform_name($used->name);
		next if $name2 =~ /^std::/;
		next if $name2 eq $name;
		$arcs{$name}{$name2} = 1;
	    }
	}
    }
    return \%arcs;
}

sub emit_graph {
    my ($arcs, $attrs) = @_;
    print "digraph { ";
    print "center=1;\n";
    for my $from (keys %$arcs) {
	for my $to (keys %{$arcs->{$from}}) {
	    print "\t\"$from\" -> \"$to\"\n";
	}
    }
    print "}\n";
}
