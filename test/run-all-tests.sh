#!/bin/bash

#
# Tests in assembler.
#
tests="uj vtm_vzm_v1m j+m_utm vlm utc_wtc vjm mtj xta_uza_u1a atx ati_ita"
tests+=" addr0 aax_aox_aex arx its sti xts stx asn_asx acx_anx apx_aux"
tests+=" stack ntr_rte yta e+n_e-n_e+x_e-x a+x_a-x_x-a amx avx"

# TODO:
tests+=" mul_div"

#
# Tests in Pascal.
#
tests+=" hello"

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
    echo -n "Test $t "
    dir=$t
    tag=`basename $t`

    if [ ! -f $dir/$tag.oct ]; then
        echo -e "- \033[1;31mNo OCT file!\033[m"
        continue
    fi

    echo ==== >> run.log
    rm -f $dir/output.trace
    make -C$dir test >> run.log
    make_status=$?

    let ntests++
    grep -q 'Test PASS' $dir/output.trace
    if [ $? = 0 -a $make_status = 0 ]; then
        echo -e "- \033[0;32mPASS\033[m"
        let npassed++
    else
        echo -e "- \033[1;31mFAIL\033[m"
        let nfailed++
    fi
done
echo "---"

echo "Tests total: $ntests, passed: $npassed, failed: $nfailed"
