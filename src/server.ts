import * as ws from "ws";
import * as http from "http";
import * as url from "url";
import * as net from "net";
import * as rpc from "@codingame/monaco-jsonrpc";
import * as server from "@codingame/monaco-jsonrpc/lib/server";
import * as lsp from "vscode-languageserver";
import cors from 'cors';
import fs from "fs";
import path from "path";

import express from "express";

const fragmentDir = path.join('/', 'app', 'fragments');

process.on('uncaughtException', function (err: any) {
  console.error('Uncaught Exception: ', err.toString());
  if (err.stack) {
    console.error(err.stack);
  }
});

export function launch(socket: rpc.IWebSocket) {
  const reader = new rpc.WebSocketMessageReader(socket);
  const writer = new rpc.WebSocketMessageWriter(socket);

  const socketConnection = server.createConnection(reader, writer, () => socket.dispose());
  console.log(process.env)
  const serverConnection = server.createServerProcess(
    'haskell',
    'haskell-language-server-8.10.7',
    ['--lsp', '--debug', '--cwd=/app/fragments'],
    { env: process.env }
  );

  server.forward(socketConnection, serverConnection, message => {
    console.log(message);

    if (rpc.isRequestMessage(message)) {
      if (message.method === lsp.InitializeRequest.type.method) {
        const initializeParams = message.params as lsp.InitializeParams;
        initializeParams.processId = process.pid;
      }
    }
    return message;
  });
}

// init/plugins
const app = express();
app.use(cors());
app.use(express.json());

// start the server
const httpServer = app.listen(3003);

// routes
app.get('/:filename', (req, res) => {
  const buffer = fs.readFileSync(path.join(fragmentDir, req.params.filename));
  res.json(buffer.toString());
});
app.post('/:filename', (req, res) => {
  fs.writeFileSync(path.join(fragmentDir, req.params.filename), req.body.content);
  res.send('POST request to the homepage');
});

// create the web socket
const wss = new ws.Server({
  noServer: true,
  perMessageDeflate: false
});

httpServer.on('upgrade', (request: http.IncomingMessage, socket: net.Socket, head: Buffer) => {
  const pathname = request.url ? url.parse(request.url).pathname : undefined;

  if (pathname === '/') {
    wss.handleUpgrade(request, socket, head, webSocket => {
      const socket: rpc.IWebSocket = {
        send: content => webSocket.send(content, error => {
          if (error) throw error;
        }),
        onMessage: cb => webSocket.on('message', cb),
        onError: cb => webSocket.on('error', cb),
        onClose: cb => webSocket.on('close', cb),
        dispose: () => webSocket.close()
      };
    
      // launch the HLS when the web socket is opened
      if (webSocket.readyState === webSocket.OPEN) launch(socket);
      else webSocket.on('open', () => launch(socket));
    });
  }
});