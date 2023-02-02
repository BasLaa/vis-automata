#! /bin/bash

cat > ~/.netrc << EOF
machine api.heroku.com
  login baslaarakker@gmail.com
  password b5c4fdf1-44a7-4815-9576-319253f0b204
machine git.heroku.com
  login baslaarakker@gmail.com
  password b5c4fdf1-44a7-4815-9576-319253f0b204
EOF

heroku container:login