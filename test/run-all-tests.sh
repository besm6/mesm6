#!/bin/bash

#
# All tests.
#
tests="uj vtm_vzm_v1m j+m_utm vlm utc_wtc vjm mtj"

#
# Delete log file from previous run.
#
rm -f run.log

#
# Run all tests that have *.oct file present.
#
ntests=0
npassed=0
nfailed=0
for t in $tests
do
    echo -n "Run $t"
    dir=$t
    tag=`basename $t`

    if [ ! -f $dir/$tag.oct ]; then
        echo -e " - \033[1;31mNo OCT file!\033[m"
        continue
    fi

    echo ==== >> run.log
    make -C$dir run >> run.log

    let ntests++
    grep -q 'Test PASS' $dir/output.trace
    if [ $? = 0 ]; then
        echo -e " - \033[0;32mPASS\033[m"
        let npassed++
    else
        echo -e " - \033[1;31mFAIL\033[m"
        let nfailed++
    fi
done
echo "Done."
echo "---"

echo "Tests total: $ntests, passed: $npassed, failed: $nfailed"
