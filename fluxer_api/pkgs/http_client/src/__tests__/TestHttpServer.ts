// SPDX-License-Identifier: AGPL-3.0-or-later

import {createServer, type IncomingMessage, type Server, type ServerResponse} from 'node:http';

type RouteHandler = (req: IncomingMessage, res: ServerResponse) => void | Promise<void>;

export interface TestServer {
	url: string;
	port: number;
	server: Server;
	close: () => Promise<void>;
	setHandler: (handler: RouteHandler) => void;
}

export async function createTestServer(): Promise<TestServer> {
	let currentHandler: RouteHandler = (_req, res) => {
		res.writeHead(404);
		res.end('Not Found');
	};
	const server = createServer((req, res) => {
		Promise.resolve(currentHandler(req, res)).catch((error) => {
			res.writeHead(500);
			res.end(String(error));
		});
	});
	return new Promise((resolve, reject) => {
		server.on('error', reject);
		server.listen(0, '127.0.0.1', () => {
			const address = server.address();
			if (!address || typeof address === 'string') {
				reject(new Error('Failed to get server address'));
				return;
			}
			const port = address.port;
			const url = `http://127.0.0.1:${port}`;
			resolve({
				url,
				port,
				server,
				close: () =>
					new Promise<void>((resolveClose, rejectClose) => {
						server.close((err) => {
							if (err) {
								rejectClose(err);
							} else {
								resolveClose();
							}
						});
					}),
				setHandler: (handler: RouteHandler) => {
					currentHandler = handler;
				},
			});
		});
	});
}

export function readRequestBody(req: IncomingMessage): Promise<string> {
	return new Promise((resolve, reject) => {
		const chunks: Array<Buffer> = [];
		req.on('data', (chunk: Buffer) => chunks.push(chunk));
		req.on('end', () => resolve(Buffer.concat(chunks).toString('utf-8')));
		req.on('error', reject);
	});
}
