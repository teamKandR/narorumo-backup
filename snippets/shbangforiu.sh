#!/bin/bash

for fn in *py *cgi; do
  sed -i '1i #!/l/python2.6/bin/python ' $fn
done
