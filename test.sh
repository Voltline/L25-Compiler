for f in test/*.l25; 
    do echo "== $f =="; 
    ./l25cc "$f" -emit-ir -o /tmp/$(basename $f.l25).ll; echo "exit=$?"; 
done