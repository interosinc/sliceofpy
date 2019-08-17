FROM haskell
RUN cabal new-update
RUN mkdir -p /src/sliceofpy
ADD . /src/sliceofpy
WORKDIR /src/sliceofpy
RUN cabal new-build
