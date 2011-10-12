#!/usr/bin/env perl
use strict;
use warnings;

# This test does a dry run of all the real production data and checks
# for any errors which can be detected in a dry run.

use Test::More qw(no_plan);
use FindBin;
use File::Basename;
use IO::Capture::Stdout;

do "$FindBin::Bin/../test.pl" or die "do test.pl: $!";

sub clean_pulse_environment
{
    foreach my $key (keys %ENV) {
        if ($key =~ /^PULSE_/i) {
            delete $ENV{$key};
        }
    }
}

sub test_stage
{
    my %opts = @_;
    my $confdir = $opts{confdir};
    my $project = $opts{project};
    my $stage = $opts{stage};

    clean_pulse_environment;

    # Pretend we are in Pulse to get as accurate as possible results.
    $ENV{PULSE_BUILD_NUMBER} = '123';
    $ENV{PULSE_BUILD_REASON} = 'faked by test script';

    my $test = PulseTest->new(
        project =>  $project,
        stage   =>  $stage,
        confdir =>  $confdir,
    );

    my $error;
    my $capture = IO::Capture::Stdout->new;
    $capture->start;
    eval { $test->dryrun };
    $error = $@;
    $capture->stop;

    print STDERR $error if $error;

    ok(!$@);
}

sub find_all_stages
{
    my %opts = @_;
    my $dir = $opts{dir};
    my $stages = $opts{stages};
    $stages = {} unless $stages;

    my $test = PulseTest->new;

    my $stagedir = $test->_follow_symlinks("$dir/stages");

    foreach my $stage (glob "$stagedir/*") {
        (-d $stage) or next;
        $stages->{basename($stage)} = 1;
    }

    if (-e "$dir/inherits") {
        my $parent = $test->_follow_symlinks("$dir/inherits");
        find_all_stages(
            dir     =>  $parent,
            stages  =>  $stages,
        );
    }

    return sort keys %{$stages};
}

sub test_project
{
    my %opts = @_;
    my $confdir = $opts{confdir};
    my $project = $opts{project};

    my $projectdir = "$confdir/projects/$project";

    my @stages = find_all_stages(dir => $projectdir);

    foreach my $stage (@stages) {
        test_stage(
            confdir =>  $confdir,
            project =>  $project,
            stage   =>  $stage,
        );
    }
}

sub test_projects
{
    my %opts = @_;
    
    my $testdata = $opts{testdata};

    foreach my $project (glob "$testdata/projects/*") {
        (-d $project) or next;

        test_project(
            confdir => $testdata,
            project => basename($project),
        );
    }
}

test_projects(testdata => "$FindBin::Bin/..");

