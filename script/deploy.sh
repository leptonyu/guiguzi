#!/bin/sh
PRG=`readlink "$0"`
if [ -z "$PRG" ]; then
  PRG=$0
fi
ROOT=`dirname "$PRG"`
cd "$ROOT/.."

if uname | grep -qvi linux; then
  echo not linux
  exit 1
fi

function copy_lib(){
  #Validate the inputs
  [[ $# < 2 ]] && useage

  #Check if the paths are vaild
  [[ ! -e $1 ]] && echo "Not a vaild input $1" && exit 1 
  [[ -d $2 ]] || echo "No such directory $2 creating..."&& mkdir -p "$2"

  #Get the library dependencies
  echo "Collecting the shared library dependencies for $1..."
  deps=$(ldd $1 | awk 'BEGIN{ORS=" "}$1\
  ~/^\//{print $1}$3~/^\//{print $3}'\
   | sed 's/,$/\n/')
  echo "Copying the dependencies to $2"

  #Copy the deps
  for dep in $deps
  do
      echo "Copying $dep to $2"
      mkdir -p "$(dirname $2/$dep)"
      cp "$dep" "$2/$dep"
  done

  echo "Done!"
}

function mk_dockerfile(){
cat << EOF > $BIN/Dockerfile
FROM scratch
COPY . /
ENTRYPOINT ["/$NAME"]
EOF
}

NAME=captcha-server
VERSION=`stack ls dependencies --no-external | grep "^captcha-server " | awk '{print $2}'`
BIN=.stack-work/bin
[[ -d "$BIN" ]] && rm -rf "$BIN"
stack install --local-bin-path=$BIN --ghc-options='-O2 -threaded' $@ \
  && copy_lib $BIN/$NAME $BIN \
  && mk_dockerfile \
  && docker build -t icymint/$NAME:$VERSION $BIN \
  && rm -rf "$BIN"