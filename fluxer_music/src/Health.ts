// SPDX-License-Identifier: AGPL-3.0-or-later

import {createServer, type Server} from 'node:http';

/** Serves GET /healthz on loopback: 200 while the gateway is ready, 503 otherwise. */
export function startHealthServer(port: number, isReady: () => boolean): Server {
	const server = createServer((req, res) => {
		if (req.method === 'GET' && req.url === '/healthz') {
			res.writeHead(isReady() ? 200 : 503).end();
			return;
		}
		res.writeHead(404).end();
	});
	server.listen(port, '127.0.0.1');
	return server;
}
