use warnings;
use strict;
use Fcntl;
use 5.010;

BEGIN {
    use Exporter ();
    use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
    $VERSION     = '0.01';
    @ISA         = qw(Exporter);
    #Give a hoot don't pollute, do not export more than needed by default
    @EXPORT      = qw();
    @EXPORT_OK   = qw();
    %EXPORT_TAGS = ();
}

package ELF::File;

use Config;

my $ivSize = $Config{ivsize};

my %addrWidth = (
    1 => 32,
    2 => 64,
);
my %p_type = (
    0x00000000 => 'PT_NULL',
    0x00000001 => 'PT_LOAD',
    0x00000002 => 'PT_DYNAMIC',
    0x00000003 => 'PT_INTERP',
    0x00000004 => 'PT_NOTE',
    0x00000005 => 'PT_SHLIB',
    0x00000006 => 'PT_PHDR',
    0x00000007 => 'PT_TLS',
    0x60000000 => 'PT_LOOS',
    0x6FFFFFFF => 'PT_HIOS',
    0x70000000 => 'PT_LOPROC',
    0x7FFFFFFF => 'PT_HIPROC',
);
our %p_typeByName = map {$p_type{$_} => $_} keys %p_type;
my %sh_type = (
    0x0  => 'SHT_NULL',
    0x1  => 'SHT_PROGBITS',
    0x2  => 'SHT_SYMTAB',
    0x3  => 'SHT_STRTAB',
    0x4  => 'SHT_RELA',
    0x5  => 'SHT_HASH',
    0x6  => 'SHT_DYNAMIC',
    0x7  => 'SHT_NOTE',
    0x8  => 'SHT_NOBITS',
    0x9  => 'SHT_REL',
    0x0A => 'SHT_SHLIB',
    0x0B => 'SHT_DYNSYM',
    0x0E => 'SHT_INIT_ARRAY',
    0x0F => 'SHT_FINI_ARRAY',
    0x10 => 'SHT_PREINIT_ARRAY',
    0x11 => 'SHT_GROUP',
    0x12 => 'SHT_SYMTAB_SHNDX',
    0x13 => 'SHT_NUM',
    0x60000000 => 'SHT_LOOS'
    );
our %sh_typeByName = map {$sh_type{$_} => $_} keys %sh_type;
my %sh_flags = (
    0x1 => 'SHF_WRITE',
    0x2 => 'SHF_ALLOC',
    0x4 => 'SHF_EXECINSTR',
    0x10 => 'SHF_MERGE',
    0x20 => 'SHF_STRINGS',
    0x40 => 'SHF_INFO_LINK',
    0x80 => 'SHF_LINK_ORDER',
    0x100 => 'SHF_OS_NONCONFORMING',
    0x200 => 'SHF_GROUP',
    0x400 => 'SHF_TLS',
    0x0ff00000 => 'SHF_MASKOS',
    0xf0000000 => 'SHF_MASKPROC',
    0x4000000 => 'SHF_ORDERED',
    0x8000000 => 'SHF_EXCLUDE',
    );
our %sh_flagsByName = map {$sh_flags{$_} => $_} keys %sh_flags;
my %EI_OSABI = (
    0x00 => 'System V',
    0x01 => 'HP-UX',
    0x02 => 'NetBSD',
    0x03 => 'Linux',
    0x04 => 'GNU Hurd',
    0x06 => 'Solaris',
    0x07 => 'AIX',
    0x08 => 'IRIX',
    0x09 => 'FreeBSD',
    0x0A => 'Tru64',
    0x0B => 'Novell Modesto',
    0x0C => 'OpenBSD',
    0x0D => 'OpenVMS',
    0x0E => 'NonStop Kernel',
    0x0F => 'AROS',
    0x10 => 'Fenix OS',
    0x11 => 'CloudABI',
    0x12 => 'Stratus Technologies OpenVOS',
);
my %e_type = (
    0x00 => 'ET_NONE',
    0x01 => 'ET_REL',
    0x02 => 'ET_EXEC',
    0x03 => 'ET_DYN',
    0x04 => 'ET_CORE',
    0xFE00 => 'ET_LOOS',
    0xFEFF => 'ET_HIOS',
    0xFF00 => 'ET_LOPROC',
    0xFFFF => 'ET_HIPROC',
);
my %e_typeEng = (
    0x00 => 'No file type',
    0x01 => 'Relocatable file',
    0x02 => 'Executable file',
    0x03 => 'Shared object file',
    0x04 => 'Core file',
);
our %e_typeByName = map {$e_type{$_} => $_} keys %e_type;
my %e_machine = (
    0x00 => 'No specific instruction set',
    0x01 => 'AT&T WE 32100',
    0x02 => 'SPARC',
    0x03 => 'x86',
    0x04 => 'Motorola 68000 (M68k)',
    0x05 => 'Motorola 88000 (M88k)',
    0x06 => 'Intel MCU',
    0x07 => 'Intel 80860',
    0x08 => 'MIPS',
    0x09 => 'IBM_System/370',
    0x0A => 'MIPS RS3000 Little-endian',
    0x0B => 'Reserved for future use',
    0x0C => 'Reserved for future use',
    0x0D => 'Reserved for future use',
    0x0E => 'Hewlett-Packard PA-RISC',
    0x0F => 'Reserved for future use',
    0x13 => 'Intel 80960',
    0x14 => 'PowerPC',
    0x15 => 'PowerPC (64-bit)',
    0x16 => 'S390, including S390x',
    0x28 => 'ARM (up to ARMv7/Aarch32)',
    0x2A => 'SuperH',
    0x32 => 'IA-64',
    0x3E => 'amd64',
    0x8C => 'TMS320C6000 Family',
    0xB7 => 'ARM 64-bits (ARMv8/Aarch64)',
    0xF3 => 'RISC-V',
    0x101 => 'WDC 65C816',
);


package ELF::File::SegmentEntry;

=pod

=cut

sub new {
    my ($class, $elfFile, $entry, %params) = @_;

    $params{elf} = $elfFile;
    $params{entry} = $entry;

    my $self = bless \%params, $class;

    $self->_ReadEntry();
    return $self;
}

sub _ReadEntry {
    my ($self) = @_;
    my $elf = $self->{elf};

    $self->{p_type} = $elf->{get32}();
    $self->{p_flags} = $elf->{get32}() if !$elf->{is32Bit};
    $self->{p_offset} = $elf->{getPtr}();

    # Virtual address of the segment in memory.
    $self->{p_vaddr} = $elf->{getPtr}();

    # On systems where physical address is relevant, reserved for segment's
    # physical address.
    $self->{p_paddr} = $elf->{getPtr}();

    # Size in bytes of the segment in the file image.
    $self->{p_filesz} = $elf->{getPtr}();

    # Size in bytes of the segment in memory.
    $self->{p_memsz} = $elf->{getPtr}();
    $self->{p_flags} = $elf->{get32}() if $elf->{is32Bit};
    $self->{p_align} = $elf->{getPtr}();
}

sub FileSize {
    my ($self) = @_;

    return $self->{p_filesz};
}

sub uncoveredSub {
}


sub Describe {
    my ($self, %params) = @_;
    my $elf = $self->{elf};
    my $type = $p_type{$self->{p_type}} // '-- unknown type--';
    my $des = "Type: $type\n";

    if ($self->{p_type} == 1) {
        $des .= sprintf "Virtual load address:  0x%08x\n", $self->{p_vaddr}
            if $self->{p_vaddr} != $self->{p_paddr} || !$self->{p_vaddr};
        $des .= sprintf "Physical load address: 0x%08x\n", $self->{p_paddr}
            if $self->{p_vaddr} != $self->{p_paddr} || $self->{p_paddr};
        $des .= sprintf "Memory image size:     0x%08x\n", $self->{p_memsz};
        $des .= sprintf "Segment alignment:     0x%08x\n", $self->{p_align};
    }

    my $headLen = $params{head} // 0;
    my $tailLen = $params{tail} // 0;
    my $totalLen = $headLen + $tailLen;

    return $des if $totalLen <= 0 || !$self->{p_filesz};

    $params{width} //= 8;
    $params{width} = $elf->{ptrSize} * 8 if $params{width} eq 'ptr';

    my $spec = $params{spec} // $elf->{"spec$params{width}"};
    my $data = $self->Dump();
    my @segments;
    my $addrWidth = $elf->{ptrSize} * 2;

    if ($totalLen >= $self->{p_filesz}) {
        push @segments, [0, $self->{p_filesz} - 1];
    } else {
        push @segments, [0, $headLen] if $headLen;
        push @segments, [$self->{p_filesz} - $tailLen,  $tailLen]
             if $tailLen;
    }

    $params{width} /= 4;

    for my $span (@segments) {
        my @spanBytes = unpack "$spec*", substr $data, $span->[0], $span->[1];
        my $addr = $self->{p_paddr} || $self->{p_vaddr};
        my $index = 0;

        $addr += $span->[0];

        while ($index < @spanBytes) {
            $des .= sprintf "%0${addrWidth}x:", $addr;

            for (1 .. 16) {
                $des .= sprintf " %0$params{width}x", $spanBytes[$index];
                ++$addr;
                last if ++$index >= @spanBytes;
            }

            $des .= "\n";
        }
    }

    return $des;
}

sub GetBlockIter {
    my ($self, %params) = @_;
    state $maxBlockSize = 0x4000000; # 64 MiB block size limit

    $params{start} //= 0;
    $params{length} //= $self->{p_filesz};
    $params{size} //= 0x4000; # 16kiB

    die "GetBlockIter length parameter must be >= 0" if $params{length} < 0;
    die "GetBlockIter start parameter must be >= 0" if $params{start} < 0;
    die "GetBlockIter blksize parameter must be >= 0" if $params{blksize} < 0;

    my $segmentIndex = 0;
    my $currOffset = $self->{p_offset} + $params{start};
    my $endOffset = $currOffset + $params{length};
    my $elfFile = $self->{elf}{elfHandle};

    $currOffset += $params{start} if $params{start};
    $currOffset += $params{length};
    $params{blksize} = $maxBlockSize if $params{blksize} > $maxBlockSize;

    return sub {
        return undef if $currOffset >= $endOffset;

        my $blockSize = $params{size};
        my $remaining = $elfFile - $currOffset;

        $blockSize = $remaining if $blockSize > $remaining;
        seek $elfFile, $currOffset, Fcntl::SEEK_SET;
        read $elfFile, my $data, $self->{p_filesz};
        $currOffset += $blockSize;
        return $data;
    }
}

sub Dump {
    my ($self) = @_;
    my $elf = $self->{elf};
    my $elfFile = $elf->{elfHandle};

    seek $elfFile, $self->{p_offset}, Fcntl::SEEK_SET;
    read $elfFile, my $data, $self->{p_filesz};
    return $data;
}


package ELF::File::StringTable;

sub new {
    my ($class, $elfFile, $header, %params) = @_;

    $params{elf} = $elfFile;
    $params{header} = $header;

    my $self = bless \%params, $class;

    $self->_ReadEntries();
    return $self;
}

sub _ReadEntries {
    my ($self) = @_;
    my $elf = $self->{elf};
    my $header = $self->{header};
    my $startPos = tell $elf->{elfHandle};

    seek $elf->{elfHandle}, $header->{sh_offset}, Fcntl::SEEK_SET;
    read $elf->{elfHandle}, $self->{strings}, $header->{sh_size};
    seek $elf->{elfHandle}, $startPos, Fcntl::SEEK_SET;
}

sub GetString {
    my ($self, $index) = @_;
    my $substr = substr ($self->{strings}, $index);

    return $substr =~ /((?:(?!\x00).)*)/;
}


package ELF::File::SymbolTable;

sub new {
    my ($class, $elfFile, $header, %params) = @_;

    $params{elf} = $elfFile;
    $params{header} = $header;

    my $self = bless \%params, $class;

    $self->_ReadEntries();
    return $self;
}

sub _ReadEntries {
    my ($self) = @_;
    my $elf = $self->{elf};
    my $header = $self->{header};
    my $startPos = tell $elf->{elfHandle};
    my $entryOffset = $header->{sh_offset};
    my $sectionEnd = $entryOffset + $header->{sh_size};

    while ($entryOffset < $sectionEnd) {
        my %entry;

        seek $elf->{elfHandle}, $entryOffset, Fcntl::SEEK_SET;
        $entry{st_name} = $elf->{get32}();
        $entry{st_value} = $elf->{getPtr}();
        $entry{st_size} = $elf->{get32}();
        $entry{st_info} = $elf->{get8}();
        $entry{st_other} = $elf->{get8}();
        $entry{st_shndx} = $elf->{get16}();
        push @{$self->{entries}}, \%entry;

        $entryOffset += $header->{sh_entsize};
    }

    seek $elf->{elfHandle}, $startPos, Fcntl::SEEK_SET;
}


package ELF::File::ProgramHeader;

sub new {
    my ($class, $elfFile, %params) = @_;

    $params{elf} = $elfFile;

    my $self = bless \%params, $class;

    $self->_ReadEntries();
    return $self;
}

sub _ReadEntries {
    my ($self) = @_;
    my $elf = $self->{elf};
    my $phEntryBase = $elf->{e_phoff};

    for my $entry (1 .. $self->{elf}{e_phnum}) {
        seek $elf->{elfHandle}, $phEntryBase, Fcntl::SEEK_SET;
        push @{$self->{entries}}, ELF::File::SegmentEntry->new($elf);
        $phEntryBase += $elf->{e_phentsize};
    }
}


package ELF::File::SectionHeader;

sub new {
    my ($class, $elfFile, %params) = @_;

    $params{elf} = $elfFile;

    my $self = bless \%params, $class;

    $self->_ReadEntries();
    return $self;
}

sub _ReadEntries {
    my ($self) = @_;
    my $elf = $self->{elf};
    my $shEntryBase = $elf->{e_shoff};

    for my $entry (1 .. $elf->{e_shnum}) {
        seek $elf->{elfHandle}, $shEntryBase, Fcntl::SEEK_SET;

        my %header = (headerNum => $entry);

        # An offset to a string in the .shstrtab section that represents the
        # name of this section.
        $header{sh_name} = $elf->{get32}();
        $header{sh_type} = $elf->{get32}();
        $header{sh_flags} = $elf->{getPtr}();

        # Virtual address of the section in memory, for sections that are loaded
        $header{sh_addr} = $elf->{getPtr}();

        # Offset of the section in the file image
        $header{sh_offset} = $elf->{getPtr}();
        $header{sh_size} = $elf->{getPtr}();
        $header{sh_link} = $elf->{get32}();
        $header{sh_info} = $elf->{get32}();

        # Contains the required alignment of the section. This field must be a
        # power of two.
        $header{sh_addralign} = $elf->{getPtr}();

        # Contains the size, in bytes, of each entry, for sections that contain
        # fixed-size entries. Otherwise, this field contains zero.
        $header{sh_entsize} = $elf->{getPtr}();
        push @{$self->{entries}}, \%header;

        if ($header{sh_type} == 2) {
            warn "Multiple symbol tables found. Table at $header{sh_offset} ignored"
                if $elf->{symbolTable};
            $elf->{symbolTable} //= ELF::File::SymbolTable->new($elf, \%header);
        } elsif ($header{sh_type} == 3) {
            my $table = ELF::File::StringTable->new($elf, \%header);
            push @{$elf->{stringTables}}, $table;
            $elf->{sectionNames} = $table if $elf->{e_shstrndx} == $entry - 1;
        }

        $shEntryBase += $self->{elf}{e_shentsize};
    }

    if ($elf->{sectionNames}) {
        # Fix up section names
        my $strings = $elf->{sectionNames}{strings};

        for my $section (@{$self->{entries}}) {
            my $substr = substr ($strings, $section->{sh_name});
            ($section->{name}) = $substr =~ /((?:(?!\x00).)*)/;

            next;
        }
    }

}


package ELF::File;

use Exporter 'import';

our @ISA = qw(Exporter);
our @EXPORT_OK = (
    '$addrWidth',
    '%p_type',
    '%p_typeByName',
    '%sh_type',
    '%sh_typeByName',
    '%sh_flags',
    '%sh_flagsByName',
    '%EI_OSABI',
    '%e_type',
    '%e_typeEng',
    '%e_typeByName',
    '%e_machine',
);

sub new {
    my ($class, %params) = @_;

    my $self = bless \%params, $class;

    $self->_openFile($self->{filePath}) if $self->{filePath};
    return $self;
}

sub Describe {
    my ($self) = @_;
    my $des = "ELF file '$self->{filePath}'\n";

    $des .= sprintf "File type: %s\n",
        $e_typeEng{$self->{e_type}} // '-- unknown --';
    $des .= sprintf "Target machine: %s\n",
        $e_machine{$self->{e_machine}} // '-- unknown --';
    $des .= sprintf "Multi-byte fields are %s endien\n",
        $self->{isLE} ? 'little' : 'big';
    $des .= sprintf "Address and offset fields are %d bits wide\n",
        $addrWidth{$self->{EI_CLASS}};
    $des .= sprintf "Program Header table entries: %d\n",
        $self->{e_phnum} if $self->{e_phoff};
    $des .= sprintf "Section Header table entries: %d\n",
        $self->{e_shnum} if $self->{e_shoff};

    return $des;
}

sub GetSegment {
    my ($self, $index) = @_;

    return undef if $index < 0
        || $index >= $self->{e_phnum}
        || !$self->{progHeader};

    return $self->{progHeader}{entries}[$index];
}

sub GetSegments {
    my ($self) = @_;

    return @{$self->{progHeader}{entries}};
}

sub GetSectionIter {
    my ($self) = @_;
    my $sectionIndex = 0;

    return undef if !$self->{secHeader};

    return sub {
        return undef if $sectionIndex > @{$self->{secHeader}{entries}};

        return $self->{secHeader}{entries}[$sectionIndex++];
    }
}

sub GetSegmentIter {
    my ($self) = @_;
    my $segmentIndex = 0;

    return sub {
        return undef if $segmentIndex > @{$self->{progHeader}{entries}};

        return $self->{progHeader}{entries}[$segmentIndex++];
    }
}

sub _openFile {
    my ($self, $filePath) = @_;
    my @eiList = qw{EI_CLASS EI_DATA EI_VERSION EI_OSABI EI_ABIVERSION};

    $self->{filePath} = $filePath;
    open $self->{elfHandle}, '<:raw', $filePath
        or die "Can't open '$filePath': $!";

    read $self->{elfHandle}, $self->{magic}, 4;
    read $self->{elfHandle}, $self->{$_}, 1 or die "Bad ELF format" for @eiList;
    $self->{magic} = join '', unpack 'a[4]', $self->{magic};
    die "'$filePath' is not an ELF file" if $self->{magic} ne "\x7FELF";

    $self->{$_} = unpack 'C', $self->{$_} for @eiList;
    $self->{isLE} = $self->{EI_DATA} == 1;

    $self->{spec8} = 'C';

    if ($self->{EI_DATA} == 1) {
        # little endian data
        $self->{spec16} = 'S<';
        $self->{spec32} = 'L<';
        $self->{spec64} = 'Q<';
    } elsif ($self->{EI_DATA} == 2) {
        # big endian data
        $self->{spec16} = 'S>';
        $self->{spec32} = 'L>';
        $self->{spec64} = 'Q>';
    } else {
        die "Bad endieness in ELF file: $self->{EI_DATA}";
    }

    $self->{get8} = sub{
        read $self->{elfHandle}, my $rawValue, 1;
        return unpack 'C', $rawValue;
        };
    $self->{get16} = sub{
        read $self->{elfHandle}, my $rawValue, 2;
        return unpack $self->{spec16}, $rawValue;
        };
    $self->{get32} = sub{
        read $self->{elfHandle}, my $rawValue, 4;
        return unpack $self->{spec32}, $rawValue;
        };
    $self->{get64} = sub{
        read $self->{elfHandle}, my $rawValue, 8;
        return unpack $self->{spec64}, $rawValue;
        };
    $self->{as16} = sub{
        my ($data) = @_;
        return unpack $self->{spec16}, $data;
        };
    $self->{as32} = sub{
        my ($data) = @_;
        return unpack $self->{spec32}, $data;
        };
    $self->{as64} = sub{
        my ($data) = @_;
        return unpack $self->{spec64}, $data;
        };
    $self->{asPtr} = sub{
        my ($data) = @_;
        return unpack $self->{specPtr}, $data;
        };

    $self->{is32Bit} = $self->{EI_CLASS} == 1;

    if ($self->{EI_CLASS} == 1) {
        # 32 bit native word size
        $self->{ptrSize} = 4;
        $self->{getPtr} = $self->{get32};
    } elsif ($self->{EI_CLASS} == 2) {
        # 64 bit native word size
        $self->{ptrSize} = 8;
        $self->{getPtr} = $self->{get64};
    } else {
        die "Bad word size in ELF file: $self->{EI_CLASS}";
    }

    # Skip padding bytes
    seek $self->{elfHandle}, 7, Fcntl::SEEK_CUR;

    $self->{e_type} = $self->{get16}();
    $self->{e_machine} = $self->{get16}();
    $self->{e_version} = $self->{get32}();
    $self->{e_entry} = $self->{getPtr}();
    $self->{e_phoff} = $self->{getPtr}();
    $self->{e_shoff} = $self->{getPtr}();
    $self->{e_flags} = $self->{get32}();
    $self->{e_ehsize} = $self->{get16}();
    $self->{e_phentsize} = $self->{get16}();
    $self->{e_phnum} = $self->{get16}();
    $self->{e_shentsize} = $self->{get16}();
    $self->{e_shnum} = $self->{get16}();
    $self->{e_shstrndx} = $self->{get16}();
    $self->{progHeader} = ELF::File::ProgramHeader->new($self)
        if $self->{e_phoff};
    $self->{secHeader} = ELF::File::SectionHeader->new($self)
        if $self->{e_shoff};
}

1;

=pod

=head1 NAME

ELF::File

=head1 VERSION

version 0.0001

=head1 SYNOPSIS

    use ELF::File;

    my $elfFile = ELF::File->new(filePath => 'sample.out');

    for my $phIndex (0 .. $elfFile->ProgHeaderSize() - 1) {
        print $elfFile->GetProgHeaderEntry($phIndex)->Describe();
    }

    my $segData = $elfFile->GetProgHeaderEntry(0)->Dump();

=head1 DESCRIPTION

ELF::File parses an ELF (Executable and Linkable Format) file and provides
access to interesting parts of the it. It can be used to pick out the memory
loadable segments of an ELF file for use in creating a raw image file to load
into a flash memory, or calculate the initial memory footprint for an
application.

ELF::File is OS, 32/64 bit and endien agnostic so it can be used to correctly
parse files from foreign systems (but see Caveats below).

=head1 USAGE

=head2 ELF::File

The ELF::File object manages parsing the ELF file and provides access to
various internal objects

=over 4

=item C<new(%params)>

C<new(...)> creates a new ELF::File object and parses the Program Header table
and the Section Header table.

=over 4

=item filePath

B<filePath> is the path name of the ELF file to be processed.

=back

=item C<Describe()>

C<Describe()> returns a multi-line string describing the ELF file header
contents.

=item C<GetSectionIter()>

Return a section iterator that can be used as:

    my $nextSection = $elfFile->GetSectionIter();
    
    while (my $section = $nextSection->()) {
    }
    
=item C<GetSegmentIter()>

Return a segment iterator that can be used as:

    my $nextSegment = $elfFile->GetSegmentIter();
    
    while (my $Segment = $nextSegment->()) {
    }

=back

=head2 ELF::File::SegmentEntry

A SegmentEntry object holds the information for a segment entry from the Program
Header Table. If a segment is loaded into memory the SegmentEntry object will
cache the contents of the segment.

=head2 ELF::File::StringTable

String table objects contain strings that are referenced by other structures in
the file. For example the section name string table contains the names of
sections. There may be multiple string tables in the file.

=over 4

=item C<GetString(B<index>)>

Return the string starting at the given index into the string table.

=back

=head2 ELF::File::ProgramHeader

The Program header object wraps up the Section Header table and its section
entries. ELF::File's C<GetSegments()> method returns a ProgramHeader object
which can be treated as an array reference to give indexed access to the segment
entries:

    my $segments = $elfFile->GetSegments();
    print ${$segments}[1]->Describe();

=head2 ELF::File::SectionHeader

The Section header object wraps up the Section Header table and its section
entries. Sections describe the contents of the payload data in the ELF file.
Each byte in the file may belong to at most one section and all the data in any
one section is contiguous.

=over 4

=item C<Load(B<index>)>

Returns the binary image for the section referenced by the entry given by
C<B<index>>. C<undef> is returned if index is invalid or there is no data
associated with the entry.

=back

=head1 CAVEATS

Returned size and address values are translated to the running Perl's native
representation which could cause trouble when parsing files > 2GB for a perl
using 32 bit representation for integers.

The author doesn't have access to a big endian system for testing the module in
a big endien environment. Please report any endien related issues to the author
(see below) and include machine and ELF file details if possible.

=head1 AUTHOR

    Peter Jaquiery
    CPAN ID: GRANDPA
    grandpa@cpan.org

=head1 COPYRIGHT & LICENSE

This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

The full text of the license can be found in the LICENSE file included with this
module.

=head1 BUG REPORTS

Bug reports may be submitted using
=head1 SEE ALSO

L<ELF::Writer|https://metacpan.org/pod/ELF::Writer> is a complement to
C<ELF::File> but is otherwise unrelated.

The ELF file format is described somewhat in
L<http://refspecs.linuxbase.org/elf/elf.pdf>. Note that this document doesn't
cover 32/64 bit differences.

L<Wikipedia's ELF article|https://en.wikipedia.org/wiki/Executable_and_Linkable_Format>
gives a good overview of the ELF file format and has useful links to further
documentation.

=cut

