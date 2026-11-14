// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LookupAddress, LookupAllOptions} from 'node:dns';
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

interface RaceOptions {
	readonly connectionSignal?: AbortSignal;
	readonly keepAlive?: boolean;
	readonly keepAliveInitialDelay?: number;
	readonly noDelay?: boolean;
}

type CreatedConnectionCallback = (error: Error | null, stream: Duplex) => void;
type ConnectionCompletion = (...args: [error: Error, stream: undefined] | [error: null, stream: Duplex]) => void;
type LookupAll = (
	hostname: string,
	options: LookupAllOptions,
	callback: (error: NodeJS.ErrnoException | null, addresses: Array<LookupAddress>) => void,
) => void;

interface HappyEyeballsConnectOptions extends RaceOptions {
	readonly host?: string | null;
	readonly hostname?: string | null;
	readonly lookup?: LookupFunction;
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

function connectToFirstAnsweringAddress(
	addresses: ReadonlyArray<ConnectAddress>,
	port: number,
	options: RaceOptions,
	callback: ConnectCallback,
): void {
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
		options.connectionSignal?.removeEventListener('abort', onAbort);
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
	const onAbort = (): void => {
		const reason: unknown = options.connectionSignal?.reason;
		const error = reason instanceof Error ? reason : new DOMException('The operation was aborted', 'AbortError');
		finish(error, null);
	};
	const startNext = (): void => {
		clearTimeout(attemptTimer);
		if (settled || next >= ordered.length) {
			return;
		}
		const {address, family} = ordered[next];
		next += 1;
		const socket = net.connect({
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
			attemptTimer = setTimeout(startNext, HAPPY_EYEBALLS_ATTEMPT_DELAY_MS);
		}
	};
	const deadline = setTimeout(() => {
		errors.push(connectTimeoutError());
		finish(connectFailure(errors), null);
	}, HAPPY_EYEBALLS_CONNECT_TIMEOUT_MS);
	deadline.unref();
	options.connectionSignal?.addEventListener('abort', onAbort, {once: true});
	if (options.connectionSignal?.aborted === true) {
		onAbort();
		return;
	}
	if (ordered.length === 0) {
		finish(connectTimeoutError(), null);
		return;
	}
	startNext();
}

function connectThroughLookup(
	options: HappyEyeballsConnectOptions,
	lookup: LookupFunction,
	callback: ConnectCallback,
): void {
	const lookupAll = lookup as LookupAll;
	const hostname = options.host ?? options.hostname ?? '';
	lookupAll(hostname, {all: true}, (error, resolved) => {
		if (error != null) {
			callback(error, null);
			return;
		}
		const addresses = resolved.map(
			(entry): ConnectAddress => ({address: entry.address, family: entry.family === 6 ? 6 : 4}),
		);
		connectToFirstAnsweringAddress(
			addresses,
			Number(options.port),
			{
				connectionSignal: options.connectionSignal,
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
		const complete = callback as ConnectionCompletion;
		connectThroughLookup(options, options.lookup, (error, socket) => {
			if (error != null || socket == null) {
				complete(error ?? connectTimeoutError(), undefined);
				return;
			}
			complete(null, socket);
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
		const complete = callback as ConnectionCompletion;
		connectThroughLookup(options, options.lookup, (error, socket) => {
			if (error != null || socket == null) {
				complete(error ?? connectTimeoutError(), undefined);
				return;
			}
			complete(null, super.createConnection({...options, socket} as http.ClientRequestArgs) as Duplex);
		});
		return undefined;
	}
}
