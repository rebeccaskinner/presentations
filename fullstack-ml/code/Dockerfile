FROM p7hb/docker-haskell
RUN mkdir /build
ADD . /build
WORKDIR /build
RUN stack setup
RUN stack build
CMD ["stack exec example"]
