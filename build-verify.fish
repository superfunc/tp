# Copyright (c) 2017, Josh Filstrup
# Licensed under MIT(see LICENSE file for details)
#
# A script used by travis to verify a build, including:
# - Build the code
# - Run tests

function runcmd
    if eval $argv >>$ROOT/build_log.txt 2>&1
        echo '✓ ' $argv
    else
        echo '✗ ' $argv
        exit 1
    end
end

set IDRISCC $argv
set ROOT (pwd)

runcmd "which $IDRISCC"
echo '-------------------------------------'
echo 'Idris compiler found!'
echo '-------------------------------------'

runcmd "$IDRISCC --clean tp.ipkg"
runcmd "$IDRISCC --build tp.ipkg"
echo '-------------------------------------'
echo 'Compilation ran successfully!'
echo '-------------------------------------'

runcmd "$IDRISCC --testpkg tp.ipkg | grep -v 'failed'"
echo '-------------------------------------'
echo 'Tests ran successfully!'
echo '-------------------------------------'
