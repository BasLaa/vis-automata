#! /bin/bash

cat > ~/.netrc << EOF
machine api.heroku.com
  login baslaarakker@gmail.com
  password -
machine git.heroku.com
  login baslaarakker@gmail.com
  password -
EOF

heroku container:login