// SPDX-License-Identifier: AGPL-3.0-or-later

import {createServer, type Socket} from 'node:net';
import {expect} from 'vitest';

interface SmtpMessage {
	from: string;
	to: string;
	body: string;
}

export async function createRecordingSmtpServer({
	firstDelivery,
}: {
	firstDelivery: 'reject' | 'hold' | 'hold-and-reject';
}) {
	const attempts: Array<SmtpMessage> = [];
	const delivered: Array<SmtpMessage> = [];
	const sockets = new Set<Socket>();
	const protocolErrors: Array<string> = [];
	const entered = Promise.withResolvers<void>();
	const released = Promise.withResolvers<void>();
	const responses = new Set<Promise<void>>();
	const smtp = createServer((socket) => {
		sockets.add(socket);
		socket.on('close', () => sockets.delete(socket));
		socket.on('error', (error) => protocolErrors.push(error.message));
		socket.setEncoding('utf8');
		let buffer = '';
		let from = '';
		let to = '';
		let body: Array<string> | null = null;
		socket.write('220 localhost ESMTP\r\n');
		socket.on('data', (chunk: string) => {
			buffer += chunk;
			let end: number;
			while ((end = buffer.indexOf('\r\n')) !== -1) {
				const line = buffer.slice(0, end);
				buffer = buffer.slice(end + '\r\n'.length);
				if (body !== null) {
					if (line !== '.') {
						body.push(line.startsWith('..') ? line.slice(1) : line);
						continue;
					}
					const raw = body.join('\r\n');
					const decoded = Buffer.from(
						raw
							.replace(/=\r\n/g, '')
							.replace(/=([\dA-F]{2})/g, (_match, hex: string) => String.fromCharCode(Number.parseInt(hex, 16))),
						'latin1',
					).toString('utf8');
					const message = {from, to, body: decoded};
					attempts.push(message);
					body = null;
					const first = attempts.length === 1;
					const response = (async () => {
						entered.resolve();
						if (first && firstDelivery !== 'reject') await released.promise;
						if (first && firstDelivery !== 'hold') {
							socket.write('550 Delivery rejected\r\n');
							return;
						}
						delivered.push(message);
						socket.write('250 Message accepted\r\n');
					})();
					responses.add(response);
					continue;
				}
				if (line.startsWith('EHLO ') || line.startsWith('HELO ')) socket.write('250 localhost\r\n');
				else if (line.startsWith('MAIL FROM:')) {
					from = line.slice('MAIL FROM:'.length);
					socket.write('250 Sender accepted\r\n');
				} else if (line.startsWith('RCPT TO:')) {
					to = line.slice('RCPT TO:'.length);
					socket.write('250 Recipient accepted\r\n');
				} else if (line === 'DATA') {
					body = [];
					socket.write('354 End with a dot\r\n');
				} else if (line === 'QUIT') socket.end('221 Goodbye\r\n');
				else if (line === 'RSET') socket.write('250 Reset\r\n');
				else {
					protocolErrors.push(line);
					socket.end('500 Unexpected command\r\n');
				}
			}
		});
	});
	await new Promise<void>((resolve) => smtp.listen(0, '127.0.0.1', resolve));
	const address = smtp.address();
	if (!address || typeof address === 'string') throw new Error('SMTP listener has no TCP address');
	return {
		port: address.port,
		attempts,
		delivered,
		entered: entered.promise,
		release: released.resolve,
		async close() {
			released.resolve();
			try {
				await Promise.all(responses);
			} finally {
				for (const socket of sockets) socket.destroy();
				await new Promise<void>((resolve, reject) => smtp.close((error) => (error ? reject(error) : resolve())));
			}
			expect(protocolErrors).toEqual([]);
		},
	};
}
