#!/usr/bin/env perl
use strict;
use warnings;

use Test::More tests => 101;
use FindBin;
use File::Basename;
use IO::Capture::Stdout;
use Text::Diff;

do "$FindBin::Bin/../test.pl" or die "do test.pl: $!";

sub debug
{
    print STDERR @_ unless 1;
}

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

    my $expectfail = 1;
    my $expectedfile = "$confdir/expected/projects/$project/stages/$stage.fail";
    if (! -e $expectedfile) {
        $expectfail = 0;
        $expectedfile = "$confdir/expected/projects/$project/stages/$stage";
    }
    my $expectedcontent = "";

    # For convenience, we don't die if the file doesn't exist...
    # This allows us to get the diff of newly created testdata.
    if (-e $expectedfile) {
        my $fh;
        open $fh, "<$expectedfile" or die "open $expectedfile: $!";
        while (my $line = <$fh>) {
            if ($^O eq "MSWin32") {
                # Convert Unix-style dry run to Windows-style
                if ($line =~ /=/) {
                    if ($line =~ s/='(.+)'$/=$1/) {
                        $line =~ s/'"'"'/'/g;
                    }
                    $line = "set $line";
                }
                if ($line =~ /^export /) {
                }
                else {
                    $expectedcontent .= $line;
                }
            }
            else {
                $expectedcontent .= $line;
            }
        }
        close $fh;
    }


    clean_pulse_environment;

    # Override PulseTest's default setting of base.dir so we get predictable output
    $ENV{PULSE_BASE_DIR} = "BASE_DIR";

    # For this test case, force PulseTest to think we are in Pulse
    if ($confdir =~ /14_no_manual_overrides/) {
        $ENV{PULSE_BUILD_NUMBER} = "123";
        $ENV{PULSE_BUILD_REASON} = "faked by test script";
    }

    debug "    $project $stage:\n";

    my $test = PulseTest->new(
        project =>  $project,
        stage   =>  $stage,
        confdir =>  $confdir,
    );

    my $error;
    my $oldstderr = *STDERR;
    my $capture = IO::Capture::Stdout->new;

    *STDERR = *STDOUT;
    $capture->start;
    eval { $test->dryrun };
    $error = $@;
    $capture->stop;
    *STDERR = $oldstderr;

    my $content = "";
    if ($expectfail) {
        $content = $error;
    }
    else {
        ok(!$error);
        while (my $line = $capture->read) {
            $content .= $line;
        }
        if (!$content && $error) {
            $content = $error;
        }
    }

    # For purposes of comparison, drop any absolute paths
    $content =~ s| at [^ ]+test\.pl line \d+| at SCRIPT line LINE|g;
    $content =~ s/$confdir/CONFDIR/g;

    my $diff;
    if ($content ne $expectedcontent) {
        $diff = diff \$expectedcontent, \$content, {
            FILENAME_A  =>  $expectedfile,
            FILENAME_B  =>  "actual_output",
            STYLE => "Unified"
        };
    }

    if ($diff) {
        print STDERR "dry run for `$project' stage `$stage' differed from expected:\n$diff\n";
    }

    ok(!$diff);
}

sub test_project
{
    my %opts = @_;
    my $confdir = $opts{confdir};
    my $project = $opts{project};

    my $projectdir = "$confdir/projects/$project";
    my $expecteddir = "$confdir/expected/projects/$project";

    my %teststages;
    foreach my $stage (glob "$expecteddir/stages/*") {
        (-f $stage) or next;
        my $stagename = basename($stage);
        $stagename =~ s/\.fail$//;
        $teststages{$stagename} = 1;
    }
    foreach my $stage (glob "$projectdir/stages/*") {
        (-d $stage) or next;
        my $stagename = basename($stage);
        $teststages{$stagename} = 1;
    }

    foreach my $stage (keys %teststages) {
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

my $testdatadir = "$FindBin::Bin/testdata";

debug "$testdatadir:\n";

foreach my $testdata (glob "$testdatadir/*") {
    (-d $testdata) or next;
    debug "  $testdata:\n";
    test_projects(testdata => $testdata);
}

