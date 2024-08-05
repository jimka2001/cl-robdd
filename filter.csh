#!/bin/csh -f
foreach file ($argv)
    # git filter-repo --invert-paths --path $file
    git filter-repo --force --invert-paths --path $file
end
