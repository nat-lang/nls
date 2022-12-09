FROM haskell:9.0.2

WORKDIR /app

COPY src src
COPY exe exe
COPY nls.cabal .
COPY package.yaml .
COPY stack.yaml .
COPY stack.yaml.lock .

RUN stack install --local-bin-path /usr/local/bin

CMD ["nls"]