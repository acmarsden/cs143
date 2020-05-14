#|//bin/bash

rm -f our.txt their.txt
./mylexer teststringlength.cl > our.txt
./theirlexer teststringlength.cl > their.txt
diff our.txt their.txt

