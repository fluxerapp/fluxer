// SPDX-License-Identifier: AGPL-3.0-or-later

import type {NatsConnection} from '@nats-io/transport-node';
import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';

interface FakeRequest {
	subject: string;
	body: Record<string, unknown>;
	timeout: number | undefined;
}

export class FakeNatsConnectionManager implements INatsConnectionManager {
	private closed: boolean;
	readonly requests: Array<FakeRequest> = [];
	connectCalls = 0;
	drained = false;

	constructor(
		private readonly reply: (
			subject: string,
			body: Record<string, unknown>,
			timeout: number | undefined,
		) => string | Uint8Array | Error | Record<string, unknown> | Array<unknown>,
		options: {startConnected?: boolean} = {},
	) {
		this.closed = !options.startConnected;
	}

	async connect(): Promise<void> {
		this.connectCalls += 1;
		this.closed = false;
	}

	getConnection(): NatsConnection {
		if (this.closed) throw new Error('not connected');
		return {
			request: async (subject: string, data: Uint8Array, options?: {timeout?: number}) => {
				const body = JSON.parse(new TextDecoder().decode(data)) as Record<string, unknown>;
				this.requests.push({subject, body, timeout: options?.timeout});
				const reply = this.reply(subject, body, options?.timeout);
				if (reply instanceof Error) throw reply;
				let encoded: Uint8Array;
				if (reply instanceof Uint8Array) {
					encoded = reply;
				} else {
					encoded = new TextEncoder().encode(typeof reply === 'string' ? reply : JSON.stringify(reply));
				}
				return {data: encoded};
			},
		} as unknown as NatsConnection;
	}

	async drain(): Promise<void> {
		this.drained = true;
		this.closed = true;
	}

	isClosed(): boolean {
		return this.closed;
	}
}
