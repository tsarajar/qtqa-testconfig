#!/usr/bin/env perl
#############################################################################
##
## Copyright (C) 2011 Nokia Corporation and/or its subsidiary(-ies).
## All rights reserved.
## Contact: Nokia Corporation (qt-info@nokia.com)
##
## This file is part of the test suite of the Qt Toolkit.
##
## $QT_BEGIN_LICENSE:LGPL$
## GNU Lesser General Public License Usage
## This file may be used under the terms of the GNU Lesser General Public
## License version 2.1 as published by the Free Software Foundation and
## appearing in the file LICENSE.LGPL included in the packaging of this
## file. Please review the following information to ensure the GNU Lesser
## General Public License version 2.1 requirements will be met:
## http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
##
## In addition, as a special exception, Nokia gives you certain additional
## rights. These rights are described in the Nokia Qt LGPL Exception
## version 1.1, included in the file LGPL_EXCEPTION.txt in this package.
##
## GNU General Public License Usage
## Alternatively, this file may be used under the terms of the GNU General
## Public License version 3.0 as published by the Free Software Foundation
## and appearing in the file LICENSE.GPL included in the packaging of this
## file. Please review the following information to ensure the GNU General
## Public License version 3.0 requirements will be met:
## http://www.gnu.org/copyleft/gpl.html.
##
## Other Usage
## Alternatively, this file may be used in accordance with the terms and
## conditions contained in a signed written agreement between you and Nokia.
##
##
##
##
##
## $QT_END_LICENSE$
##
#############################################################################

use strict;
use warnings;
use 5.008;

#==============================================================================
package QtQATest;

use File::Basename;
use File::Spec;
use FindBin;
use Cwd qw(abs_path getcwd);
use Getopt::Long;
use Data::Dumper;
use Carp qw(croak);

use constant CONFDIR => $FindBin::RealBin;

use constant TOKEN_LITERAL     => "literal";
use constant TOKEN_PROPERTY    => "property";
use constant TOKEN_ENVIRONMENT => "environment";

use constant SOURCE_FILE        =>  "file";
use constant SOURCE_ENVIRONMENT =>  "environment";
use constant SOURCE_DEFAULT     =>  "default value";

sub new
{
    my $class = shift;
    my %opts = @_;
    $opts{properties} = {};
    if (!$opts{confdir}) {
        $opts{confdir} = CONFDIR;
    }
    bless \%opts, $class;
}

# Transform a Pulse project or stage name into a filesystem-safe handle
sub _handleize
{
    my $self = shift;
    my $out = shift;

    $out =~ s/[^a-zA-Z0-9_\-+.]/_/g;

    return $out;
}

# Return first line (chomped) of the given file, or die.
sub _filecontents
{
    my $self = shift;
    my $filename = shift;

    my $realfilename = $self->_follow_symlinks($filename);

    my $fh;
    open $fh, "<$realfilename" or croak "open $realfilename: $!\n"
        .(($filename ne $realfilename) ? "  ($realfilename is dest of symlink $filename)\n" : "");
    my $value = <$fh>;
    close $fh;

    if (defined($value)) {
        chomp $value;
    }

    return $value;
}

# Read environment variables from the given directory.
sub _readenvironment
{
    my $self = shift;
    my $directory = shift;

    eval {
        my $newdirectory = $self->_follow_symlinks($directory);
        $directory = $newdirectory if $newdirectory;
    };

    # Directory is allowed to not exist.
    if (! -e $directory) {
        return;
    }

    if (! -d $directory) {
        die "$directory exists but isn't a directory";
    }

    foreach my $file (glob "$directory/*") {
        next unless -f $file;

        my $environment = basename $file;
        my $value = $self->_filecontents($file);

        if ($environment =~ /^(PULSE|QTQA)_(.*)$/i) {
            my $prefix = $1;
            my $rest = lc $2;
            die "$file is named $environment\n"
                ."Please don't set `${prefix}_FOO' values with the environment directory; "
                ."if you want to set a property called $rest then use the properties directory";
        }

        $self->{environment}->{$environment} = {
            value       =>  $value,
            source      =>  SOURCE_FILE,
            filename    =>  $file,
        };
    }
}

# Read properties from the given directory.
sub _readproperties
{
    my $self = shift;
    my $directory = shift;
    my %opts = @_;

    eval {
        my $newdirectory = $self->_follow_symlinks($directory);
        $directory = $newdirectory if $newdirectory;
    };

    # Directory is allowed to not exist.
    if (! -e $directory) {
        return;
    }

    if (! -d $directory) {
        die "$directory exists but isn't a directory";
    }

    foreach my $file (glob "$directory/*") {
        next unless -f $file;

        my $property = basename $file;
        my $value = $self->_filecontents($file);

        my ($environment) = $self->_property_to_environment($property);
        if ($self->{useenv} && exists($ENV{$environment})) {
            warn "environment variable $environment overrides the value in $file";
        }
        else {
            if (exists($ENV{$environment})) {
                warn "environment variable $environment is set but will have no effect";
                if (!$self->{warnedenv}) {
                    $self->{warnedenv} = 1;
                    warn "If you want environment variables to be able to override configuration, "
                        ."you must run with the `--use-env' option";
                }
            }
            if (my $p = $self->{properties}->{$property}) {
                if (defined($p->{value}) && defined($value) && ($p->{value} ne $value) && $opts{warn_redefine}) {
                    warn "Property `$property' was redefined\n"
                        ."  New definition:      `$value' (from ".SOURCE_FILE." $file)\n"
                        ."  Previous definition: `".$p->{value}."' (from ".$p->{source}.
                            (($p->{source} eq SOURCE_FILE) ? " ".$p->{filename} : "").")"
                    ;
                }
            }
            $self->{properties}->{$property} = {
                value       =>  $value,
                source      =>  SOURCE_FILE,
                filename    =>  $file,
            };
        }
    }
}

sub _readenvironmentproperties
{
    my $self = shift;
    foreach my $key (keys %ENV) {
        $key = uc $key;
        next unless ($key =~ /^(?:PULSE|QTQA)_/i);

        # This environment variable is a property.
        # However, there's no unambiguous way to map it back to the property name,
        # since replacing . with _ is lossy.
        # To give the nicest looking results, we'll try to match against the properties we've read
        # from disk, plus a few other known property names
        my @known_properties = keys %{$self->{properties}};
        push @known_properties, "base.dir";
        my $property;
        OUTER: foreach my $propertyname (@known_properties) {
            my @this_env = $self->_property_to_environment($propertyname);
            foreach my $this_env (@this_env) {
                if ($this_env eq $key) {
                    $property = $propertyname;
                    last OUTER;
                }
            }
        }

        # if there's no matches, we'll just leave _ as-is, instead of trying to guess
        # which ones should be dots.
        if (!$property) {
            $property = $key;
            $property =~ s/^(?:PULSE|QTQA)_//;
            $property = lc $property;
        }

        if (!$self->{properties}->{$property} || $self->{useenv}) {
            $self->{properties}->{$property} = {
                value   =>  $ENV{$key},
                source  =>  SOURCE_ENVIRONMENT,
                env     =>  $key,
            }
        }
    }
}

# Read an optional value from any one of the given files.
# The first file which exists is used.
# If none of the files exist, an empty string is returned.
# Returns the value and the filename which was used.
sub _readoptionfile
{
    my $self = shift;
    my @filenames = @_;

    my $out = "";
    my $resolvedfilename;

    foreach my $filename (@filenames) {
        if (-f $filename) {
            $out = $self->_filecontents($filename);
            $resolvedfilename = $filename;
            last;
        }
    }

    if (wantarray) {
       return ($out, $resolvedfilename);
    }
    else {
        return $out;
    }
}

sub _parserepository
{
    my $self = shift;
    my $repository = shift;

    # repository string is this format:
    #
    #  uri/of/git/repo#gitbranch directoryname
    #
    # With the corresponding command(s) being:
    #
    #  git clone uri/of/git/repo directoryname
    #  cd directoryname
    #  git checkout -b local origin/gitbranch
    #

    my @split = split(/ +/, $repository);
    if (scalar(@split) != 2) {
        die "expected 2 space-separated elements, but got ".scalar(@split);
    }

    my $gitrepository = $split[0];
    my $gitlocaldirectory = $split[1];
    my $gitbranch;

    if ($gitrepository =~ /#/) {
        @split = split(/#/, $gitrepository);
        if (scalar(@split) != 2) {
            die "repository part `$gitrepository' should have 0 or 1 `#' characters";
        }
        $gitrepository = $split[0];
        $gitbranch = $split[1];
    }

    $self->{gitrepository} = $gitrepository;
    $self->{gitlocaldirectory} = $gitlocaldirectory;
    $self->{gitbranch} = $gitbranch;
}

# Follow msysgit's emulated symbolic links
sub _follow_symlinks
{
    my $self = shift;
    my $path = shift;

    # On Unix we can follow symlinks normally...
    if ($^O ne "MSWin32") {
        eval { $path = abs_path($path) };
        return $path;
    }

    my $existing_part = $path;
    while (! -e $existing_part) {
        my $new_existing_part = dirname($existing_part);
        last if ($new_existing_part eq $existing_part);
        $existing_part = $new_existing_part;
    }
    if (! -e $existing_part) {
        return $path;
    }

    my $existing_part_quoted = quotemeta($existing_part);
    my $rest = $path;
    $rest =~ s/^$existing_part_quoted//;

    eval { $existing_part = abs_path($existing_part) };

    # Given the path:
    #
    #  projects/Qt3d/stages/foobar
    #
    # ...where the `stages' part is a symlink, we now have
    #
    # $existing_part = projects/Qt3d/stages
    # $rest = /foobar


    # If $existing_part is a directory, it's not a symlink
    return $path if (-d $existing_part);

    my $fh;
    open $fh, "<$existing_part" or return $path;
    my $value = <$fh>;
    close $fh;

    chomp $value;

    if (!$value) {
        return $path;
    }

    # OK, now $existing_part is _possibly_ a symlink
    my $symlink_src = $existing_part;
    my $symlink_dest;
    eval { $symlink_dest = dirname(abs_path($symlink_src))."/$value" };
    if ($@) {
        return $path;
    }
    if (! -e $symlink_dest) {
        return $path;
    }

    my $created_cache = !exists($self->{_symlink_cache});
    if (exists($self->{_symlink_cache}->{$symlink_dest})) {
        die "cyclic reference in symbolic links";
    }
    $self->{_symlink_cache}->{$symlink_dest} = 1;

    my $out;
    eval {
        $out = $self->_follow_symlinks($symlink_dest);
    };
    my $error = $@;
    delete $self->{_symlink_cache} if $created_cache;

    if ($error) {
        die "while processing $symlink_dest (linked to from $symlink_src):\n $error";
    }

    $out .= $rest;

    eval { $out = abs_path($out) };

    return $out;
}

sub _format_parse_error {
    my $self = shift;
    my %opts = @_;

    my $input = $opts{input};
    my $errorposition = $opts{errorposition};
    my $errorstring = $opts{errorstring};

    my $out = $input;
    $out .= "\n";
    $out .= sprintf("%${errorposition}s^\n","");
    $out .= "error: $errorstring";

    return $out;
}

# Tokenize a property string and return a description of the tokens.
sub _tokenize_property
{
    my $self = shift;
    my $property = shift;

    my @tokens;
    my @chars;
    if (defined($property->{value})) {
        @chars = split(//, $property->{value});
    }

    use constant STATE_READING_LITERAL  =>  "reading_literal";
    use constant STATE_READING_ENVIRONMENT_CURLYBRACE =>  "reading_env_curlybrace";
    use constant STATE_READING_PROPERTY_PARENTHESIS =>  "reading_property_parenthesis";
    use constant STATE_SUBSTITUTION_MARKER  =>  "subst_marker";
    use constant STATE_ESCAPE_CHARACTER =>  "escape_character";

    my $current_token = "";
    my $state = STATE_READING_LITERAL;
    my $i = -1;

    while (@chars) {
        my $c = shift @chars;
        ++$i;


        if ($state eq STATE_READING_LITERAL)
        {
            if ($c eq '\\') {
                $state = STATE_ESCAPE_CHARACTER;
            }
            elsif ($c eq '$') {
                $state = STATE_SUBSTITUTION_MARKER;
            }

            # Are we still reading a literal?
            if ($state eq STATE_READING_LITERAL) {
                $current_token .= $c;
            }
            elsif ($current_token ne "") {
                push @tokens, {
                    type    =>  TOKEN_LITERAL,
                    data    =>  $current_token,
                };
                $current_token = "";
            }
        }

        elsif ($state eq STATE_READING_ENVIRONMENT_CURLYBRACE)
        {
            if ($c eq '}')
            {
                $state = STATE_READING_LITERAL;
                push @tokens, {
                    type    =>  TOKEN_ENVIRONMENT,
                    data    =>  $current_token,
                };
                $current_token = "";
            }
            else
            {
                $current_token .= $c;
            }
        }

        elsif ($state eq STATE_READING_PROPERTY_PARENTHESIS)
        {
            if ($c eq ')')
            {
                $state = STATE_READING_LITERAL;
                push @tokens, {
                    type    =>  TOKEN_PROPERTY,
                    data    =>  $current_token,
                };
                $current_token = "";
            }
            else
            {
                $current_token .= $c;
            }
        }

        elsif ($state eq STATE_SUBSTITUTION_MARKER) {
            if ($c eq '{') {
                $state = STATE_READING_ENVIRONMENT_CURLYBRACE;
            }
            elsif ($c eq '(') {
                $state = STATE_READING_PROPERTY_PARENTHESIS;
            }
            else {
                die $self->_format_parse_error(
                    input           =>  $property->{value},
                    errorposition   =>  $i,
                    errorstring     =>
                        "expected { or (, got $c\n"
                        ."(maybe you need \\\$$c instead of \$$c ?)"
                );
            }
        }


        elsif ($state eq STATE_ESCAPE_CHARACTER) {
            if (!($c =~ /[\\\$]/)) {
                die $self->_format_parse_error(
                    input           =>  $property->{value},
                    errorposition   =>  $i-1,
                    errorstring     =>
                        "useless use of \\\n"
                        ."(I saw \\$c; if you wanted $c then put $c; if you wanted \\$c then put \\\\$c)"
                );
            }
            push @tokens, {
                type    =>  TOKEN_LITERAL,
                data    =>  $c,
            };
            $current_token = "";
            $state = STATE_READING_LITERAL;
        }


        else {
            die $self->_format_parse_error(
                input           =>  $property->{value},
                errorposition   =>  $i,
                errorstring     => "internal error: parser in unknown state $state",
            );
        }
    }



    # We're done; we should have always finished on STATE_READING_LITERAL
    if ($state eq STATE_READING_LITERAL) {
        if ($current_token ne "") {
            push @tokens, {
                type    =>  TOKEN_LITERAL,
                data    =>  $current_token,
            };
            $current_token = "";
        }
    }
    elsif ($state eq STATE_READING_ENVIRONMENT_CURLYBRACE) {
        die $self->_format_parse_error(
            input           =>  $property->{value},
            errorposition   =>  $i+1,
            errorstring     => "got end of string, expected `}'",
        );
    }
    elsif ($state eq STATE_READING_PROPERTY_PARENTHESIS) {
        die $self->_format_parse_error(
            input           =>  $property->{value},
            errorposition   =>  $i+1,
            errorstring     => "got end of string, expected `)'",
        );
    }
    elsif ($state eq STATE_SUBSTITUTION_MARKER) {
        die $self->_format_parse_error(
            input           =>  $property->{value},
            errorposition   =>  $i+1,
            errorstring     =>
                "got end of string, expected `(' or `{'\n"
                ."(last character in string was \$, did you mean \\\$ ?)"
        );
    }
    elsif ($state eq STATE_ESCAPE_CHARACTER) {
        die $self->_format_parse_error(
            input           =>  $property->{value},
            errorposition   =>  $i+1,
            errorstring     =>
                "got end of string, expected some character\n"
                ."(last character in string was \\, did you mean \\\\ ?)"
        );
    }
    else {
        die $self->_format_parse_error(
            input           =>  $property->{value},
            errorposition   =>  $i+1,
            errorstring     =>
                "internal error at end of string: parser ended in unexpected state $state"
        );
    }


    my %out = %{$property};
    $out{tokens} = \@tokens;
    return \%out;
}

sub _relative_filename
{
    my $self = shift;
    my $filename = shift;

    my $prefix = $self->{confdir};
    $filename =~ s|^$prefix/?||;

    return $filename;
}

sub _tokenize_or_die
{
    my $self = shift;
    my %opts = @_;

    my $key = $opts{key};
    my $thing = $opts{thing};
    my $type = $opts{type};

    my $tokenized_thing;
    eval { $tokenized_thing = $self->_tokenize_property($thing) };
    if ($@) {
        die sprintf("while parsing %s `%s' (sourced from %s%s):\n%s",
            $type,
            $key,
            $thing->{source},
            ($thing->{source} eq SOURCE_FILE) ? (" ".$self->_relative_filename($thing->{filename})) : "",
            $@
        );
    }

    return $tokenized_thing;
}

sub _resolve_property
{
    my $self = shift;
    my %opts = @_;

    my $property = $opts{to_resolve};
    my $unresolved_properties = $opts{unresolved};
    my $input_properties = $opts{input};

    my @tokens = @{$property->{tokens}};
    if (scalar(@tokens) == 0) {
        return "";
    }

    my $resolved = undef;
    foreach my $token (@tokens) {
        if ($token->{type} eq TOKEN_LITERAL) {
            defined($resolved) or $resolved = "";
            $resolved .= $token->{data};
        }
        elsif ($token->{type} eq TOKEN_PROPERTY) {
            my $data = $token->{data};
            if (exists $input_properties->{$data}) {
                defined($resolved) or $resolved = "";
                $resolved .= $input_properties->{$data}->{value};
            }
            elsif (exists $unresolved_properties->{$data}) {
                $resolved = undef;
                last;
            }
            else {
                # property does not exist at all - just use an empty value.
                defined($resolved) or $resolved = "";
            }
        }
        elsif ($token->{type} eq TOKEN_ENVIRONMENT) {
            my $data = $token->{data};
            if (exists $ENV{$data}) {
                $resolved .= $ENV{$data};
            }
            else {
                defined($resolved) or $resolved = "";
            }
        }
    }

    return $resolved;
} 

# Look through all properties and environment variables and resolve $(references)
# to the referenced property.
# Also removes escape characters.
sub _resolve_property_references
{
    my $self = shift;

    my $all_properties = $self->{properties};
    my $resolved_properties;
    my $unresolved_properties;

    foreach my $key (keys %{$all_properties}) {
        my $property = $all_properties->{$key};

        # We don't touch properties which come from anywhere but files.
        # The reason is that properties set in Pulse will come from the
        # environment, and may have been parsed once already by that point.
        # If we parsed them here, they would potentially be parsed twice,
        # and there would be much confusion about where they should be parsed,
        # how many escape characters need to be used, etc.
        if ($property->{source} ne SOURCE_FILE) {
            $resolved_properties->{$key} = $property;
            next;
        }

        $unresolved_properties->{$key} = $self->_tokenize_or_die(
            key     =>  $key,
            thing   =>  $property,
            type    =>  "property",
        );
    }

    my $all_environment = $self->{environment};
    my $unresolved_environment;
    my $resolved_environment;

    foreach my $key (keys %{$all_environment}) {
        my $environment = $all_environment->{$key};

        $unresolved_environment->{$key} = $self->_tokenize_or_die(
            key     =>  $key,
            thing   =>  $environment,
            type    =>  "environment variable",
        );
    }

    # Check tokenization here if unsure :-)
    #print STDERR Dumper($unresolved_properties, $unresolved_environment);

    # To avoid the code complexity of generating a dependency graph between
    # properties, we'll simply use an inefficient approach of repeatedly iterating
    # through all unresolved properties until it looks like we can't do any more.
    my $did_something = 1;
    while ($did_something && (scalar keys %{$unresolved_properties})) {
        $did_something = 0;

        foreach my $key (keys %{$unresolved_properties}) {
            my $to_resolve = $unresolved_properties->{$key};
            my $resolved = $self->_resolve_property(
                to_resolve  =>  $to_resolve,
                unresolved  =>  $unresolved_properties,
                input       =>  $resolved_properties,
            );
            if (defined($resolved)) {
                $to_resolve->{value} = $resolved;
                $resolved_properties->{$key} = $to_resolve;
                delete $unresolved_properties->{$key};
                $did_something = 1;
            }
        }
    }


    # If there are still unresolved properties, then we didn't manage to resolve
    # anything at the last pass - there must be cyclic dependencies.
    if (scalar keys %{$unresolved_properties}) {
        my $property_description = "";
        my $sep = "";
        foreach my $key (keys %{$unresolved_properties}) {
            $property_description .= $sep;
            $sep = ", ";

            my $property = $unresolved_properties->{$key};

            $property_description .= "$key";
            $property_description .= " (from ".$property->{source}.(
                ($property->{source} eq SOURCE_FILE)
                    ? (" ".$property->{filename})
                    : ""
            );
            $property_description .= ")";
        }
        die "cyclic dependency detected between these properties: $property_description";
    }

    # Resolve environment;
    # these can refer to properties, but can't refer to each other.
    foreach my $key (keys %{$unresolved_environment}) {
        my $to_resolve = $unresolved_environment->{$key};

        my $resolved = $self->_resolve_property(
            to_resolve  =>  $to_resolve,
            unresolved  =>  {},
            input       =>  $resolved_properties,
        );

        # This should never happen... (cyclic dependencies are not possible here)
        defined($resolved) or die "internal error: resolving environment `$key' failed";

        $to_resolve->{value} = $resolved;
        $resolved_environment->{$key} = $to_resolve;
        delete $unresolved_environment->{$key};
    }

    $self->{properties} = $resolved_properties;
    $self->{environment} = $resolved_environment;
}

sub _setdefaultproperties
{
    my $self = shift;
    my $properties = $self->{properties};

    # There are a few properties which Pulse sets every time.
    # If we're running the script locally, we should set these for the user if
    # the value is predictable.  Otherwise, any references to these from other
    # properties would be replaced with an empty string.
    if (!exists($properties->{'base.dir'}) && !exists($properties->{base_dir})) {
        $properties->{'base.dir'} = {
            value   =>  File::Spec->canonpath(getcwd),
            source  =>  SOURCE_DEFAULT,
        };
    }
}

# Return true if it looks like we're in Pulse.
sub _in_pulse
{
    my $self = shift;

    if ($self->{force_in_pulse}) {
        return 1;
    }

    return exists($ENV{PULSE_BUILD_NUMBER}) && exists($ENV{PULSE_BUILD_REASON});
}

sub _read_inheritance_tree
{
    my $self = shift;

    my $dir = shift;
    
    my %seen;
    my @out;

    my $thisdir;
    eval { $thisdir = $self->_follow_symlinks($dir) };
    if (!$thisdir || ! -d $thisdir) {
        return @out;
    }

    unshift @out, $thisdir;
    $seen{$thisdir} = 1;

    my $inherits = "$thisdir/inherits";
    while (-e($inherits) || -l($inherits)) {
        eval { $thisdir = $self->_follow_symlinks($inherits) };
        if ($@) {
            die "Broken inheritance link `$inherits': $@";
        }
        if (! -e($thisdir) && ! -l($thisdir)) {
            die "$inherits points to non-existent target $thisdir";
        }
        if ($seen{$thisdir}) {
            die "Cyclic inheritance detected between ".join(", ",(sort keys %seen));
        }
        $seen{$thisdir} = 1;
        unshift @out, $thisdir;
        $inherits = "$thisdir/inherits";
    }

    return @out;
}

sub _readconfig
{
    my $self = shift;

    my $project = $self->{project};
    my $stage = $self->{stage};
    my $confdir = $self->{confdir};

    my @stagedirs;

    my $thisprojectdir = $self->_follow_symlinks("$confdir/projects/".$self->_handleize($project));
    if (! -d $thisprojectdir) {
        die "project directory $thisprojectdir doesn't exist or isn't a directory";
    }
    my @projectdirs = $self->_read_inheritance_tree($thisprojectdir);

    #print STDERR "projectdirs now ".Dumper(\@projectdirs)."\n";

    my $stagehandle = $self->_handleize($stage);
    foreach my $projectdir (@projectdirs) {
        my $stagedir;
        eval { $stagedir = $self->_follow_symlinks($projectdir."/stages/".$self->_handleize($stage)) };
        if (!$@ && $stagedir && (-d $stagedir)) {
            push @stagedirs, $self->_read_inheritance_tree($stagedir);
        }
    }
    @stagedirs or die "Could not find a stage directory for $stage (looked at "
        .join(", ", (map { "$_/stages/$stagehandle" } @projectdirs)).")";

    # It should be anything different than '0'
    # This option will ignore all failures
    my ($forcesuccess, $forcesuccessfile) = $self->_readoptionfile(
        (map { "$_/forcesuccess" } (reverse(@projectdirs, @stagedirs))),
    );
    $self->{forcesuccess} = $forcesuccess;
    $self->{forcesuccessfile} = $forcesuccessfile;

    my $overridedir = $self->_follow_symlinks("$confdir/manual_overrides");

    # Note: order is important here.
    # stage properties should override project properties.
    # manual_override should override these.
    # environment properties should override everything.
    foreach my $projectdir (@projectdirs) {
        $self->_readproperties("$projectdir/properties");
        $self->_readenvironment("$projectdir/environment");
    }
    foreach my $stagedir (@stagedirs) {
        $self->_readproperties("$stagedir/properties");
        $self->_readenvironment("$stagedir/environment");
    }

    if (! $self->_in_pulse) {
        my @warns;
        {
            local $SIG{__WARN__} = sub {
                push @warns, (split /\n/, $_[0]);
            };
            $self->_readproperties("$overridedir/properties", warn_redefine => 1);
            $self->_readenvironment("$overridedir/environment", warn_redefine => 1);
        }
        if (@warns) {
            print STDERR
                 "# Warning: because this script is not being run in Pulse, the test environment\n"
                ."# differs slightly.  Details:\n"
                ."# ".join("\n# ", @warns)
                ."\n";
        }
    }
    $self->_readenvironmentproperties;

    $self->_setdefaultproperties;

    $self->_resolve_property_references;

    my ($repository, $repositoryfile) = $self->_readoptionfile(
        (map { "$_/repository" } (reverse(@projectdirs, @stagedirs))),
    );
    eval { $self->_parserepository($repository) } if $repository;
    if ($@) {
        die "while parsing $repositoryfile: $@\ncontents of file: $repository\n\n";
    }

    my $script = $self->_readoptionfile(
        (map { "$_/script" } (reverse(@projectdirs, @stagedirs))),
    );
    if (!$script) {
        die "no `script' file exists in any of (".join(", ", (@stagedirs, @projectdirs))."), so I don't know what to run";
    }

    $self->{script} = $script;
}

sub _property_to_environment
{
    my $self = shift;

    my $out = shift;

    # If we read the property from environment, ensure we return the same
    # env var used to set it.
    if (exists $self->{properties}{$out}) {
        my $p = $self->{properties}{$out};
        if ($p->{source} eq SOURCE_ENVIRONMENT) {
            return $p->{env};
        }
    }

    $out = uc $out;
    $out =~ tr/./_/;
    $out =~ tr/-/_/;

    return ("QTQA_$out", "PULSE_$out");
}

sub _gitcommands
{
    my $self = shift;
    my @out;

    my $gitrepository = $self->{gitrepository};
    my $gitlocaldirectory = $self->{gitlocaldirectory};
    my $gitbranch = $self->{gitbranch};
    if ($gitrepository) {
        push @out, "git clone $gitrepository $gitlocaldirectory";
        if ($gitbranch) {
            push @out, "git --git-dir=$gitlocaldirectory/.git --work-tree=$gitlocaldirectory checkout -b $gitbranch origin/$gitbranch";
        }
    }

    return @out;
}

sub _set_environment_shell_fragment
{
    my $self = shift;
    my %opts = @_;
    my $key = $opts{key};
    my $value = $opts{value};

    my $setpart = "$key=";
    $setpart = "set $key=" if ($^O eq "MSWin32");

    if ($^O ne "MSWin32") {
        if ($value =~ /[^a-zA-Z0-9_\-+.]/) {
            $value =~ s/'/'"'"'/g;
            $value = "'$value'";
        }
    }

    return $setpart.$value;
}

sub dryrun
{
    my $self = shift;

    eval { $self->_readconfig };
    if ($@) {
        $self->_maybe_die("While attempting to do a dry run of ".$self->{project}." stage ".$self->{stage}.":\n $@");
        return;
    }

    my $properties = $self->{properties};
    my $environment = $self->{environment};

    my $lastbit = "export";
    $lastbit = "" if ($^O eq "MSWin32");

    foreach my $key (sort keys %{$properties}) {
        my ($pulse_key) = $self->_property_to_environment($key);

        $lastbit .= " $pulse_key" if ($^O ne "MSWin32");

        my $shellbit = $self->_set_environment_shell_fragment(
            key     =>  $pulse_key,
            value   =>  $properties->{$key}->{value},
        );

        print "$shellbit\n";
    }

    foreach my $key (sort keys %{$environment}) {
        $lastbit .= " $key" if ($^O ne "MSWin32");

        my $shellbit = $self->_set_environment_shell_fragment(
            key     =>  $key,
            value   =>  $environment->{$key}->{value},
        );

        print "$shellbit\n";
    }

    print "$lastbit\n" if $lastbit;

    foreach my $command ($self->_gitcommands) {
        print "$command\n";
    }

    print $self->{script}."\n";
}

sub _exe
{
    my $self = shift;

    my $cmd_with_args = join " ", @_;
    print "+ $cmd_with_args\n";

    system(@_);

    if ($? == -1) {
        die "could not fork to run `$cmd_with_args': $!";
    }
    elsif (($^O ne "MSWin32") && ($? & 127)) {
        my $error = sprintf("`$cmd_with_args' died with signal %d", ($? & 127));
        die $error;
    }
    elsif ($? == 0) {
    }
    else {
        die "`$cmd_with_args' exited with code ".($? >> 8);
    }
}

sub _run_commands
{
    my $self = shift;

    foreach my $command ($self->_gitcommands) {
        $self->_exe($command);
    }

    $self->_exe($self->{script});
}

sub _maybe_die
{
    my $self = shift;
    my $error = shift;
    if (!$self->{forcesuccess}) {
        die $error;
    }
    else {
        print STDERR "$error\n";
        print STDERR "Normally I would now fail.  However, `forcesuccess' was set in "
            .$self->{forcesuccessfile}.".\n"
            ."Therefore I'm going to exit successfully.\n"
            ."This probably indicates that this test is known to not yet be stable.\n"
        ;
    }
}

sub run
{
    my $self = shift;

    eval { $self->_readconfig };
    if ($@) {
        $self->_maybe_die("While attempting to do a run of ".$self->{project}." stage ".$self->{stage}.": $@");
        return;
    }

    my $properties = $self->{properties};
    my $environment = $self->{environment};

    foreach my $key (sort keys %{$properties}) {
        my ($pulse_key) = $self->_property_to_environment($key);
        $ENV{$pulse_key} = $properties->{$key}->{value};
    }
    foreach my $key (sort keys %{$environment}) {
        $ENV{$key} = $environment->{$key}->{value};
    }

    eval { $self->_run_commands };
    if ($@) {
        $self->_maybe_die($@);
        return;
    }
}

#==============================================================================
package main;

use FindBin;
use Getopt::Long;

use constant CONFDIR => $FindBin::RealBin;

sub usage {
    print

q{Usage: test.pl --project project-name --stage stage-name \
  [ --dry-run ] [ --use-env ]

 Run the test procedure for the given project and stage on the local machine.

 This script should be run from within the source repository of the tested product.

 Configuration will be read from }.CONFDIR.q{
 and run a test using the environment shown in that directory.

 When running the script within the Pulse CI tool, the `--project' and
 `--stage' options should be omitted.  The appropriate values will be read from
 the environment.

OPTIONS:

  --dry-run     Don't run anything; just output what would be run, and the environment
                which would be used.

  --use-env     Allow locally set environment variables to override values from disk.
                This option can be used to experiment with new property values
                without having to make the changes on disk first.

USING CUSTOM SETTINGS:

  If a property is already set in the environment, then it takes precedence over the
  configuration in }.CONFDIR.q{ .

  For instance, if `incredibuild.enabled' is set to `1' in this repository, but
  you don't have incredibuild available, you can disable it by setting the
  QTQA_INCREDIBUILD_ENABLED environment variable to `0'.

  The script will warn about all properties which have been overridden in the
  environment in this way.

EXAMPLES:

 If you want to reproduce what the "QtBase master Integration" rule does on your
 local Linux machine, the test procedure would go like this:

    $ git clone git://gitorious.org/qt/qtbase
    $ cd qtbase
    $ }.CONFDIR.q{/test.pl --project "QtBase master Integration" --stage "linux-g++-32 Ubuntu 10.04 x86"
};
}

sub main {
    my $project;
    my $stage;
    my $project_env = $ENV{PULSE_PROJECT};
    my $stage_env = $ENV{PULSE_STAGE};
    my $dryrun;
    my $useenv;
    my $help;

    if (!GetOptions(
        "project=s" =>  \$project,
        "stage=s"   =>  \$stage,
        "dry-run"   =>  \$dryrun,
        "use-env"   =>  \$useenv,
        "help"      =>  \$help,
    )) {
        usage;
        exit 2;
    }

    if ($help) {
        usage;
        exit 0;
    }

    if (!$project && !$project_env) {
        print STDERR "--project was omitted and PULSE_PROJECT environment variable wasn't set.\n";
        usage;
        exit 2;
    }

    if (!$stage && !$stage_env) {
        print STDERR "--stage was omitted and PULSE_STAGE environment variable wasn't set.\n";
        usage;
        exit 2;
    }

    if ($project) {
        if ($project_env) {
            warn "`--project' option overrides PULSE_PROJECT environment variable. "
                ."Please pick just one method of specifying the project.";
        }
        $ENV{PULSE_PROJECT} = $project;
    }
    else {
        $project = $project_env;
    }

    if ($stage) {
        if ($stage_env) {
            warn "`--stage' option overrides PULSE_STAGE environment variable. "
                ."Please pick just one method of specifying the project.";
        }
        $ENV{PULSE_STAGE} = $stage;
    }
    else {
        $stage = $stage_env;
    }

    my $test = QtQATest->new(
        project =>  $project,
        stage   =>  $stage,
        useenv  =>  $useenv,
    );

    if ($dryrun) {
        $test->dryrun;
    }
    else {
        $test->dryrun;
        print "========== TO REPRODUCE THIS BUILD, COPY THE ABOVE ===============\n";

        $test->run;
    }
}

if (!caller) {
    main;
}

1;

