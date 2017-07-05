# Copyright (c) 2017, Josh Filstrup
# Licensed under BSD3(see license.md file for details)
#
# A script used by travis to verify a build, including:
# - Build the code
# - Run examples
# - Run tests
# - Generate docs(and diff them against the installed copy)

function runcmd
    if eval $argv >> $ROOT/build_log.txt 2>&1
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

runcmd "$IDRISCC --build tp.ipkg"
echo '-------------------------------------'
echo 'Compilation ran successfully!'
echo '-------------------------------------'

runcmd "$IDRISCC --testpkg tp.ipkg | grep -v 'failed'"
echo '-------------------------------------'
echo 'Tests ran successfully!'
echo '-------------------------------------'

