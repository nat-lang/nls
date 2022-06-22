FROM node:14-buster as node
FROM haskell:8.10.7

WORKDIR /app

COPY --from=node /usr/local/lib/node_modules /usr/local/lib/node_modules
COPY --from=node /usr/local/bin /usr/local/bin

COPY package.json package.json
COPY tsconfig.json tsconfig.json

ENV PATH "$PATH:/root/.ghcup/bin:$PATH"

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh \
    && ghcup install hls
