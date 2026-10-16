// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ForegroundDemotionHandover,
	type ForegroundDemotionHandoverHost,
} from '@app/features/gateway/transport/ForegroundDemotionHandover';
import type {GatewayDispatchDelivery, GatewaySocket} from '@app/features/gateway/transport/GatewaySocket';
import {describe, expect, test} from 'vitest';

const ACCOUNT_KEY = 'primary.test::100';
const EXPECTED_USER_ID = '100';
const DEMOTION_BUFFER_MAX_BYTES = 16 * 1024 * 1024;

class FakeSocket {
	connected = true;
	readonly failures: Array<unknown> = [];
	readonly tokens: Array<string> = [];
	readonly resets: Array<boolean> = [];
	private readonly listeners = new Map<string, Array<(payload: never) => void>>();

	on(event: string, listener: (payload: never) => void): void {
		const existing = this.listeners.get(event) ?? [];
		existing.push(listener);
		this.listeners.set(event, existing);
	}

	removeAllListeners(event: string): void {
		this.listeners.delete(event);
	}

	isDispatchActive(): boolean {
		return true;
	}

	completeDispatchProcessing(): void {}

	failDispatchProcessing(_receipt: unknown, error: unknown): void {
		this.failures.push(error);
		this.connected = false;
	}

	isConnected(): boolean {
		return this.connected;
	}

	reset(discardSession: boolean): void {
		this.resets.push(discardSession);
	}

	setToken(token: string): void {
		this.tokens.push(token);
	}

	deliver(delivery: GatewayDispatchDelivery): void {
		for (const listener of this.listeners.get('dispatch') ?? []) {
			(listener as (payload: GatewayDispatchDelivery) => void)(delivery);
		}
	}

	resume(): void {
		this.connected = true;
	}

	asGatewaySocket(): GatewaySocket {
		return this as unknown as GatewaySocket;
	}
}

class FakeHost implements ForegroundDemotionHandoverHost {
	readonly applied: Array<string> = [];
	retiringTokenPersists = true;
	private token: string | null = null;

	applySnapshotDispatch(_accountKey: string, _expectedUserId: string, delivery: GatewayDispatchDelivery): void {
		this.applied.push(delivery.type);
	}

	persistRetiringToken(_accountKey: string, token: string): Promise<boolean> {
		if (!this.retiringTokenPersists) {
			return Promise.resolve(false);
		}
		this.token = token;
		return Promise.resolve(true);
	}

	persistAccountToken(_accountKey: string, token: string): Promise<boolean> {
		this.token = token;
		return Promise.resolve(true);
	}

	getAccountToken(): string | null {
		return this.token;
	}
}

function delivery(type: string, retainedByteSize = 8): GatewayDispatchDelivery {
	return {
		type,
		data: {},
		retainedByteSize,
		receipt: {sequence: 1, generation: 0},
	} as unknown as GatewayDispatchDelivery;
}

function authSessionChange(newToken: unknown, retainedByteSize = 8): GatewayDispatchDelivery {
	return {
		type: 'AUTH_SESSION_CHANGE',
		data: {new_token: newToken},
		retainedByteSize,
		receipt: {sequence: 1, generation: 0},
	} as unknown as GatewayDispatchDelivery;
}

function createHarness(): {socket: FakeSocket; host: FakeHost; handover: ForegroundDemotionHandover} {
	const socket = new FakeSocket();
	const host = new FakeHost();
	const handover = new ForegroundDemotionHandover(host);
	handover.begin({accountKey: ACCOUNT_KEY, expectedUserId: EXPECTED_USER_ID, socket: socket.asGatewaySocket()});
	return {socket, host, handover};
}

describe('foreground demotion handover', () => {
	test('a demotion that stayed within its bounds applies its buffered dispatches and reports success', async () => {
		const {host, socket, handover} = createHarness();

		socket.deliver(delivery('MESSAGE_CREATE'));
		socket.deliver(delivery('MESSAGE_UPDATE'));

		await expect(handover.consume(ACCOUNT_KEY)).resolves.toBe(true);
		expect(host.applied).toEqual(['MESSAGE_CREATE', 'MESSAGE_UPDATE']);
	});

	test('a demotion whose buffer overflowed reports failure even once the socket is connected again', async () => {
		const {host, socket, handover} = createHarness();

		socket.deliver(delivery('MESSAGE_CREATE'));
		socket.deliver(delivery('MESSAGE_UPDATE', DEMOTION_BUFFER_MAX_BYTES));
		socket.resume();

		await expect(handover.consume(ACCOUNT_KEY)).resolves.toBe(false);
		expect(host.applied).toEqual([]);
	});

	test('a rollback after a buffer overflow refuses to restore the retained session', async () => {
		const {socket, handover} = createHarness();

		socket.deliver(delivery('MESSAGE_CREATE', DEMOTION_BUFFER_MAX_BYTES));
		socket.resume();

		const rollback = await handover.rollback(ACCOUNT_KEY);

		expect(rollback).toEqual({
			outcome: 'restore',
			socket: socket.asGatewaySocket(),
			deliveries: [],
			persistedAuthToken: null,
			canRestore: false,
		});
	});
});

describe('foreground demotion handover token rotation', () => {
	test('a rotation the host refuses to persist fails the demotion rather than retiring the stale token', async () => {
		const {host, socket, handover} = createHarness();
		host.retiringTokenPersists = false;

		socket.deliver(delivery('MESSAGE_CREATE'));
		socket.deliver(authSessionChange('rotated-token'));

		await expect(handover.consume(ACCOUNT_KEY)).resolves.toBe(false);
		expect(socket.resets).toEqual([true]);
		expect(host.applied).toEqual([]);
	});

	test('a rotation carrying an unusable token fails the dispatch and drops what was buffered', async () => {
		const {host, socket, handover} = createHarness();

		socket.deliver(delivery('MESSAGE_CREATE'));
		socket.deliver(authSessionChange(''));

		expect(socket.failures).toHaveLength(1);
		expect((socket.failures[0] as Error).name).toBe('ForegroundDemotionAuthTokenError');

		await expect(handover.consume(ACCOUNT_KEY)).resolves.toBe(false);
		expect(host.applied).toEqual([]);
	});
});
