app=`ls -a *.tar.gz | awk -F"-" '{ print $1 }'`
vsn=`ls -a *.tar.gz | awk -F"-" '{ print $2 }' | awk -F".tar" '{print $1 }'`
tar -xvzf ${app}-${vsn}.tar.gz
./erts-*/bin/erl -boot releases/${vsn}/start -config releases/${vsn}/sys -noshell
