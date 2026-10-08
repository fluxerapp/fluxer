// SPDX-License-Identifier: AGPL-3.0-or-later

import http from 'node:http';
import https from 'node:https';
import net, {type LookupFunction} from 'node:net';
import type {Duplex} from 'node:stream';

const HAPPY_EYEBALLS_ATTEMPT_DELAY_MS = 250;
const HAPPY_EYEBALLS_CONNECT_TIMEOUT_MS = 30_000;

interface ConnectAddress {
	readonly address: string;
	readonly family: 4 | 6;
}

type ConnectCallback = (error: Error | null, socket: net.Socket | null) => void;
type Dial = (options: net.TcpNetConnectOpts) => net.Socket;

interface RaceOptions {
	readonly attemptDelayMs?: number;
	readonly dial?: Dial;
	readonly keepAlive?: boolean;
	readonly keepAliveInitialDelay?: number;
	readonly noDelay?: boolean;
}

type CreatedConnectionCallback = (error: Error | null, stream: Duplex) => void;

interface HappyEyeballsConnectOptions {
	readonly host?: string | null;
	readonly hostname?: string | null;
	readonly keepAlive?: boolean;
	readonly keepAliveInitialDelay?: number;
	readonly lookup?: LookupFunction;
	readonly noDelay?: boolean;
	readonly port?: number | string | null;
}

class HappyEyeballsConnectError extends AggregateError {
	public readonly code: string | undefined;
	public readonly syscall = 'connect';

	public constructor(errors: ReadonlyArray<Error>) {
		super(errors, `Could not connect to any resolved address (${errors.map((error) => error.message).join('; ')})`);
		this.name = 'HappyEyeballsConnectError';
		this.code = (errors[0] as NodeJS.ErrnoException | undefined)?.code;
	}
}

function connectFailure(errors: ReadonlyArray<Error>): Error {
	return errors.length === 1 ? errors[0] : new HappyEyeballsConnectError(errors);
}

function connectTimeoutError(): NodeJS.ErrnoException {
	const error: NodeJS.ErrnoException = new Error('Connecting to the resolved addresses timed out');
	error.code = 'ETIMEDOUT';
	return error;
}

function interleaveFamilies(addresses: ReadonlyArray<ConnectAddress>): ReadonlyArray<ConnectAddress> {
	const first = addresses[0]?.family;
	const preferred = addresses.filter((address) => address.family === first);
	const other = addresses.filter((address) => address.family !== first);
	const ordered: Array<ConnectAddress> = [];
	for (let index = 0; index < Math.max(preferred.length, other.length); index += 1) {
		if (index < preferred.length) {
			ordered.push(preferred[index]);
		}
		if (index < other.length) {
			ordered.push(other[index]);
		}
	}
	return ordered;
}

export function connectToFirstAnsweringAddress(
	addresses: ReadonlyArray<ConnectAddress>,
	port: number,
	options: RaceOptions,
	callback: ConnectCallback,
): void {
	const attemptDelayMs = options.attemptDelayMs ?? HAPPY_EYEBALLS_ATTEMPT_DELAY_MS;
	const dial = options.dial ?? net.connect;
	const ordered = interleaveFamilies(addresses);
	const pending = new Set<net.Socket>();
	const errors: Array<Error> = [];
	let next = 0;
	let settled = false;
	let attemptTimer: NodeJS.Timeout | undefined;
	const finish = (error: Error | null, winner: net.Socket | null): void => {
		if (settled) {
			return;
		}
		settled = true;
		clearTimeout(attemptTimer);
		clearTimeout(deadline);
		for (const socket of pending) {
			if (socket !== winner) {
				socket.destroy();
			}
		}
		pending.clear();
		callback(error, winner);
	};
	const startNext = (): void => {
		clearTimeout(attemptTimer);
		if (settled || next >= ordered.length) {
			return;
		}
		const {address, family} = ordered[next];
		next += 1;
		const socket = dial({
			family,
			host: address,
			keepAlive: options.keepAlive,
			keepAliveInitialDelay: options.keepAliveInitialDelay,
			noDelay: options.noDelay,
			port,
		});
		pending.add(socket);
		const onError = (error: Error): void => {
			pending.delete(socket);
			errors.push(error);
			if (settled) {
				return;
			}
			if (next < ordered.length) {
				startNext();
			} else if (pending.size === 0) {
				finish(connectFailure(errors), null);
			}
		};
		socket.once('error', onError);
		socket.once('connect', () => {
			socket.removeListener('error', onError);
			finish(null, socket);
		});
		if (next < ordered.length) {
			attemptTimer = setTimeout(startNext, attemptDelayMs);
		}
	};
	const deadline = setTimeout(() => {
		errors.push(connectTimeoutError());
		finish(connectFailure(errors), null);
	}, HAPPY_EYEBALLS_CONNECT_TIMEOUT_MS);
	deadline.unref();
	if (ordered.length === 0) {
		finish(connectTimeoutError(), null);
		return;
	}
	startNext();
}

function connectThroughLookup(options: HappyEyeballsConnectOptions, callback: ConnectCallback): void {
	const lookup = options.lookup as LookupFunction;
	const hostname = options.host ?? options.hostname ?? '';
	lookup(hostname, {all: true}, (error, resolved) => {
		if (error != null) {
			callback(error, null);
			return;
		}
		const addresses = (resolved as unknown as ReadonlyArray<{address: string; family: number}>).map(
			(entry): ConnectAddress => ({address: entry.address, family: entry.family === 6 ? 6 : 4}),
		);
		connectToFirstAnsweringAddress(
			addresses,
			Number(options.port),
			{
				keepAlive: options.keepAlive,
				keepAliveInitialDelay: options.keepAliveInitialDelay,
				noDelay: options.noDelay,
			},
			callback,
		);
	});
}

export class HappyEyeballsHttpAgent extends http.Agent {
	public override createConnection(
		options: HappyEyeballsConnectOptions,
		callback?: CreatedConnectionCallback,
	): Duplex | null | undefined {
		if (options.lookup == null || callback == null) {
			return super.createConnection(options as http.ClientRequestArgs, callback);
		}
		connectThroughLookup(options, (error, socket) => {
			if (error != null || socket == null) {
				callback(error ?? connectTimeoutError(), undefined as unknown as Duplex);
				return;
			}
			callback(null, socket);
		});
		return undefined;
	}
}

export class HappyEyeballsHttpsAgent extends https.Agent {
	public override createConnection(
		options: HappyEyeballsConnectOptions,
		callback?: CreatedConnectionCallback,
	): Duplex | null | undefined {
		if (options.lookup == null || callback == null) {
			return super.createConnection(options as http.ClientRequestArgs, callback);
		}
		connectThroughLookup(options, (error, socket) => {
			if (error != null || socket == null) {
				callback(error ?? connectTimeoutError(), undefined as unknown as Duplex);
				return;
			}
			callback(null, super.createConnection({...options, socket} as http.ClientRequestArgs) as Duplex);
		});
		return undefined;
	}
}
