// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type BackgroundDispatchHandoverEnd,
	BackgroundDispatchHandoverOutcome,
} from '@app/features/gateway/transport/BackgroundGatewayDispatchQueue';
import {
	ForegroundPromoteCoordinator,
	type ForegroundPromoteHost,
	type WarmForegroundCandidate,
} from '@app/features/gateway/transport/ForegroundPromoteCoordinator';
import type {GatewayDispatchDelivery, GatewaySocket} from '@app/features/gateway/transport/GatewaySocket';
import {describe, expect, test} from 'vitest';

const ACCOUNT_KEY = 'secondary.test::200';
const WARM_PROMOTE_BUFFER_MAX_BYTES = 16 * 1024 * 1024;
const WARM_PROMOTE_BUFFER_MAX_ENTRIES = 1024;

function delivery(type: string, retainedByteSize = 8): GatewayDispatchDelivery {
	return {
		type,
		data: {},
		retainedByteSize,
		receipt: {sequence: 1, generation: 0},
	} as unknown as GatewayDispatchDelivery;
}

class FakeCandidate implements WarmForegroundCandidate {
	isBackgroundConnectionReady = true;
	readyResult = true;
	socket: GatewaySocket | null = {} as GatewaySocket;
	idleHook: (() => void) | null = null;
	lastHandoverEnd: BackgroundDispatchHandoverEnd | null = null;
	private listener: ((item: GatewayDispatchDelivery) => void) | null = null;

	constructor(private readonly calls: Array<string>) {}

	beginBackgroundDispatchHandover(listener: (item: GatewayDispatchDelivery) => void): void {
		this.calls.push('beginHandover');
		this.listener = listener;
	}

	endBackgroundDispatchHandover(request: BackgroundDispatchHandoverEnd): void {
		this.calls.push(`endHandover:${request.outcome}`);
		this.lastHandoverEnd =
			request.outcome === BackgroundDispatchHandoverOutcome.RESTORED
				? {outcome: request.outcome, deliveries: [...request.deliveries]}
				: request;
		this.listener = null;
	}

	detachBackgroundSocket(): GatewaySocket | null {
		this.calls.push('detach');
		const socket = this.socket;
		this.socket = null;
		return socket;
	}

	waitForBackgroundConnectionReady(): Promise<boolean> {
		this.calls.push('waitReady');
		return Promise.resolve(this.readyResult);
	}

	waitForBackgroundDispatchIdle(): Promise<void> {
		this.calls.push('idle');
		this.idleHook?.();
		return Promise.resolve();
	}

	deliver(item: GatewayDispatchDelivery): void {
		this.listener?.(item);
	}
}

class FakeHost implements ForegroundPromoteHost {
	snapshotPrepared = true;
	commitAccepted = true;
	replayError: Error | null = null;
	hydrateError: Error | null = null;
	adoptedSockets: Array<GatewaySocket> = [];
	hydrateHook: (() => void) | null = null;

	constructor(
		readonly calls: Array<string>,
		public candidate: WarmForegroundCandidate | null,
	) {}

	findWarmCandidate(): WarmForegroundCandidate | null {
		return this.candidate;
	}

	prepareForegroundSnapshot(): Promise<boolean> {
		this.calls.push('prepareSnapshot');
		return Promise.resolve(this.snapshotPrepared);
	}

	hydrateForegroundSnapshot(): Promise<void> {
		this.calls.push('hydrate');
		this.hydrateHook?.();
		return this.hydrateError === null ? Promise.resolve() : Promise.reject(this.hydrateError);
	}

	commitForegroundSnapshot(): boolean {
		this.calls.push('commit');
		return this.commitAccepted;
	}

	abortForegroundSnapshot(): void {
		this.calls.push('abortSnapshot');
	}

	finalizeForegroundSnapshot(): void {
		this.calls.push('finalizeSnapshot');
	}

	stopBackgroundConnection(): void {
		this.calls.push('stopBackground');
	}

	prepareForegroundReplay(): void {
		this.calls.push('prepareReplay');
	}

	adoptForegroundSocket(_accountKey: string, socket: GatewaySocket): void {
		this.calls.push('adopt');
		this.adoptedSockets.push(socket);
	}

	replayForegroundDispatch(_accountKey: string, item: GatewayDispatchDelivery): void {
		this.calls.push(`replay:${item.type}`);
		if (this.replayError !== null) {
			throw this.replayError;
		}
	}

	finalizeForeground(): void {
		this.calls.push('finalizeForeground');
	}

	removeBackgroundConnection(): void {
		this.calls.push('removeBackground');
	}

	discardDetachedSocket(): void {
		this.calls.push('discardSocket');
	}

	discardForegroundConnection(): void {
		this.calls.push('discardForeground');
	}
}

function createHarness(): {calls: Array<string>; candidate: FakeCandidate; host: FakeHost} {
	const calls: Array<string> = [];
	const candidate = new FakeCandidate(calls);
	return {calls, candidate, host: new FakeHost(calls, candidate)};
}

describe('foreground promote coordinator', () => {
	test('a warm promotion hands the background socket over in one fixed order', async () => {
		const {calls, candidate, host} = createHarness();
		const promoted = candidate.socket;
		const coordinator = new ForegroundPromoteCoordinator(host);

		await coordinator.begin(ACCOUNT_KEY);
		candidate.deliver(delivery('MESSAGE_CREATE'));
		const outcome = await coordinator.complete(ACCOUNT_KEY);

		expect(outcome).toEqual({mode: 'warm'});
		expect(calls).toEqual([
			'beginHandover',
			'idle',
			'prepareSnapshot',
			'idle',
			'idle',
			'prepareReplay',
			'hydrate',
			'idle',
			'commit',
			`endHandover:${BackgroundDispatchHandoverOutcome.CONSUMED}`,
			'detach',
			'removeBackground',
			'adopt',
			'replay:MESSAGE_CREATE',
			'finalizeForeground',
			'finalizeSnapshot',
		]);
		expect(host.adoptedSockets).toEqual([promoted]);
	});

	test('a missing candidate goes cold without touching any handover', async () => {
		const {calls, host} = createHarness();
		host.candidate = null;
		const coordinator = new ForegroundPromoteCoordinator(host);

		await coordinator.begin(ACCOUNT_KEY);
		const outcome = await coordinator.complete(ACCOUNT_KEY);

		expect(outcome).toEqual({mode: 'cold', reason: 'no_candidate'});
		expect(calls).toEqual(['stopBackground']);
	});

	test('a candidate that never becomes ready goes cold without touching any handover', async () => {
		const {calls, candidate, host} = createHarness();
		candidate.isBackgroundConnectionReady = false;
		candidate.readyResult = false;
		const coordinator = new ForegroundPromoteCoordinator(host);

		await coordinator.begin(ACCOUNT_KEY);
		const outcome = await coordinator.complete(ACCOUNT_KEY);

		expect(outcome).toEqual({mode: 'cold', reason: 'candidate_not_ready_timeout'});
		expect(calls).toEqual(['waitReady', 'stopBackground']);
	});

	test('a candidate lost during the handover restores its buffered dispatches and aborts the snapshot once', async () => {
		const {calls, candidate, host} = createHarness();
		const buffered = delivery('MESSAGE_CREATE');
		const coordinator = new ForegroundPromoteCoordinator(host);
		candidate.idleHook = () => {
			candidate.deliver(buffered);
			candidate.isBackgroundConnectionReady = false;
		};

		await coordinator.begin(ACCOUNT_KEY);
		const outcome = await coordinator.complete(ACCOUNT_KEY);

		expect(outcome).toEqual({mode: 'cold', reason: 'candidate_lost'});
		expect(candidate.lastHandoverEnd).toEqual({
			outcome: BackgroundDispatchHandoverOutcome.RESTORED,
			deliveries: [buffered],
		});
		expect(calls.filter((call) => call === 'abortSnapshot')).toEqual(['abortSnapshot']);
		expect(calls.filter((call) => call.startsWith('endHandover'))).toEqual([
			`endHandover:${BackgroundDispatchHandoverOutcome.RESTORED}`,
		]);
		expect(calls).not.toContain('adopt');
	});

	test('an unavailable snapshot goes cold and restores the handover once', async () => {
		const {calls, candidate, host} = createHarness();
		host.snapshotPrepared = false;
		const coordinator = new ForegroundPromoteCoordinator(host);

		await coordinator.begin(ACCOUNT_KEY);
		const outcome = await coordinator.complete(ACCOUNT_KEY);

		expect(outcome).toEqual({mode: 'cold', reason: 'snapshot_unavailable'});
		expect(candidate.lastHandoverEnd).toEqual({
			outcome: BackgroundDispatchHandoverOutcome.RESTORED,
			deliveries: [],
		});
		expect(calls.filter((call) => call === 'abortSnapshot')).toEqual(['abortSnapshot']);
		expect(calls).not.toContain('detach');
	});

	test('a buffer overflow goes cold and loses the handover instead of restoring it', async () => {
		const {calls, candidate, host} = createHarness();
		const coordinator = new ForegroundPromoteCoordinator(host);

		await coordinator.begin(ACCOUNT_KEY);
		candidate.deliver(delivery('MESSAGE_CREATE', WARM_PROMOTE_BUFFER_MAX_BYTES));
		const outcome = await coordinator.complete(ACCOUNT_KEY);

		expect(outcome).toEqual({mode: 'cold', reason: 'buffer_capacity'});
		expect(candidate.lastHandoverEnd).toEqual({outcome: BackgroundDispatchHandoverOutcome.LOST});
		expect(calls.filter((call) => call === 'abortSnapshot')).toEqual(['abortSnapshot']);
		expect(calls).not.toContain('adopt');
	});

	test('exceeding the retained entry bound goes cold and loses the handover instead of restoring it', async () => {
		const {calls, candidate, host} = createHarness();
		const coordinator = new ForegroundPromoteCoordinator(host);

		await coordinator.begin(ACCOUNT_KEY);
		for (let index = 0; index <= WARM_PROMOTE_BUFFER_MAX_ENTRIES; index += 1) {
			candidate.deliver(delivery('MESSAGE_CREATE'));
		}
		const outcome = await coordinator.complete(ACCOUNT_KEY);

		expect(outcome).toEqual({mode: 'cold', reason: 'buffer_capacity'});
		expect(candidate.lastHandoverEnd).toEqual({outcome: BackgroundDispatchHandoverOutcome.LOST});
		expect(calls.filter((call) => call === 'abortSnapshot')).toEqual(['abortSnapshot']);
		expect(calls).not.toContain('adopt');
	});

	test('staying one delivery under the retained entry bound still promotes warm', async () => {
		const {calls, candidate, host} = createHarness();
		const coordinator = new ForegroundPromoteCoordinator(host);

		await coordinator.begin(ACCOUNT_KEY);
		for (let index = 0; index < WARM_PROMOTE_BUFFER_MAX_ENTRIES; index += 1) {
			candidate.deliver(delivery('MESSAGE_CREATE'));
		}
		const outcome = await coordinator.complete(ACCOUNT_KEY);

		expect(outcome).toEqual({mode: 'warm'});
		expect(candidate.lastHandoverEnd).toEqual({outcome: BackgroundDispatchHandoverOutcome.CONSUMED});
		expect(calls.filter((call) => call === 'replay:MESSAGE_CREATE')).toHaveLength(WARM_PROMOTE_BUFFER_MAX_ENTRIES);
	});

	test('a failure after the foreground socket is adopted discards the foreground connection', async () => {
		const {calls, candidate, host} = createHarness();
		const replayError = new Error('replay exploded');
		host.replayError = replayError;
		const coordinator = new ForegroundPromoteCoordinator(host);

		await coordinator.begin(ACCOUNT_KEY);
		candidate.deliver(delivery('MESSAGE_CREATE'));

		await expect(coordinator.complete(ACCOUNT_KEY)).rejects.toBe(replayError);
		expect(calls).toContain('discardForeground');
		expect(calls).not.toContain('discardSocket');
		expect(calls.filter((call) => call === 'abortSnapshot')).toEqual(['abortSnapshot']);
		expect(calls).toContain('stopBackground');

		host.candidate = null;
		await expect(coordinator.begin(ACCOUNT_KEY)).resolves.toBeUndefined();
	});

	test('a rollback restores the buffered dispatches and clears the promotion', async () => {
		const {calls, candidate, host} = createHarness();
		const buffered = delivery('MESSAGE_CREATE');
		const coordinator = new ForegroundPromoteCoordinator(host);

		await coordinator.begin(ACCOUNT_KEY);
		candidate.deliver(buffered);
		await coordinator.rollback(ACCOUNT_KEY);

		expect(candidate.lastHandoverEnd).toEqual({
			outcome: BackgroundDispatchHandoverOutcome.RESTORED,
			deliveries: [buffered],
		});
		expect(calls.filter((call) => call === 'abortSnapshot')).toEqual(['abortSnapshot']);
		expect(calls).not.toContain('detach');

		host.candidate = null;
		await expect(coordinator.begin(ACCOUNT_KEY)).resolves.toBeUndefined();
	});

	test('buffered dispatches replay into the promoted foreground in arrival order', async () => {
		const {calls, candidate, host} = createHarness();
		const coordinator = new ForegroundPromoteCoordinator(host);

		await coordinator.begin(ACCOUNT_KEY);
		candidate.deliver(delivery('MESSAGE_CREATE'));
		candidate.deliver(delivery('MESSAGE_UPDATE'));
		candidate.deliver(delivery('MESSAGE_DELETE'));
		const outcome = await coordinator.complete(ACCOUNT_KEY);

		expect(outcome).toEqual({mode: 'warm'});
		expect(calls.filter((call) => call.startsWith('replay:'))).toEqual([
			'replay:MESSAGE_CREATE',
			'replay:MESSAGE_UPDATE',
			'replay:MESSAGE_DELETE',
		]);
	});

	test('a candidate replaced in the registry mid-handover goes cold instead of promoting the stale one', async () => {
		const {calls, candidate, host} = createHarness();
		const coordinator = new ForegroundPromoteCoordinator(host);

		await coordinator.begin(ACCOUNT_KEY);
		host.candidate = new FakeCandidate(calls);
		const outcome = await coordinator.complete(ACCOUNT_KEY);

		expect(outcome).toEqual({mode: 'cold', reason: 'candidate_lost'});
		expect(candidate.socket).not.toBeNull();
		expect(host.adoptedSockets).toEqual([]);
		expect(calls).not.toContain('adopt');
	});

	test('a buffer overflow during snapshot hydration goes cold instead of promoting without its dispatches', async () => {
		const {calls, candidate, host} = createHarness();
		const coordinator = new ForegroundPromoteCoordinator(host);
		host.hydrateHook = () => {
			candidate.deliver(delivery('MESSAGE_CREATE', WARM_PROMOTE_BUFFER_MAX_BYTES));
		};

		await coordinator.begin(ACCOUNT_KEY);
		const outcome = await coordinator.complete(ACCOUNT_KEY);

		expect(outcome).toEqual({mode: 'cold', reason: 'buffer_capacity'});
		expect(candidate.lastHandoverEnd).toEqual({outcome: BackgroundDispatchHandoverOutcome.LOST});
		expect(calls).not.toContain('adopt');
	});

	test('a snapshot that fails to hydrate goes cold instead of failing the whole promotion', async () => {
		const {calls, candidate, host} = createHarness();
		host.hydrateError = new Error('snapshot hydration rejected');
		const coordinator = new ForegroundPromoteCoordinator(host);

		await coordinator.begin(ACCOUNT_KEY);
		const outcome = await coordinator.complete(ACCOUNT_KEY);

		expect(outcome).toEqual({mode: 'cold', reason: 'snapshot_unavailable'});
		expect(candidate.lastHandoverEnd).toEqual({
			outcome: BackgroundDispatchHandoverOutcome.RESTORED,
			deliveries: [],
		});
		expect(calls.filter((call) => call === 'abortSnapshot')).toEqual(['abortSnapshot']);
		expect(calls).toContain('stopBackground');
		expect(calls).not.toContain('adopt');

		host.candidate = null;
		await expect(coordinator.begin(ACCOUNT_KEY)).resolves.toBeUndefined();
	});

	test('a rejected snapshot commit aborts instead of adopting the background socket', async () => {
		const {calls, candidate, host} = createHarness();
		host.commitAccepted = false;
		const coordinator = new ForegroundPromoteCoordinator(host);

		await coordinator.begin(ACCOUNT_KEY);
		candidate.deliver(delivery('MESSAGE_CREATE'));

		await expect(coordinator.complete(ACCOUNT_KEY)).rejects.toThrow(
			`Foreground snapshot commit rejected ${ACCOUNT_KEY}`,
		);
		expect(host.adoptedSockets).toEqual([]);
		expect(calls).not.toContain('detach');
		expect(calls).not.toContain('adopt');
		expect(calls.filter((call) => call === 'abortSnapshot')).toEqual(['abortSnapshot']);
	});

	test('completing a promotion under another account key throws instead of driving it', async () => {
		const {calls, host} = createHarness();
		const coordinator = new ForegroundPromoteCoordinator(host);

		await coordinator.begin(ACCOUNT_KEY);

		await expect(coordinator.complete('third.test::300')).rejects.toThrow(
			`Foreground gateway promotion belongs to ${ACCOUNT_KEY}, not third.test::300`,
		);
		expect(calls).not.toContain('adopt');
		await expect(coordinator.complete(ACCOUNT_KEY)).resolves.toEqual({mode: 'warm'});
	});

	test('completing a promotion that never began throws', async () => {
		const {host} = createHarness();
		const coordinator = new ForegroundPromoteCoordinator(host);

		await expect(coordinator.complete(ACCOUNT_KEY)).rejects.toThrow(
			`No foreground gateway promotion is active for ${ACCOUNT_KEY}`,
		);
	});

	test('beginning a second promotion while one is active throws', async () => {
		const {host} = createHarness();
		const coordinator = new ForegroundPromoteCoordinator(host);

		await coordinator.begin(ACCOUNT_KEY);

		await expect(coordinator.begin('third.test::300')).rejects.toThrow(
			`Foreground gateway promotion is already active for ${ACCOUNT_KEY}`,
		);
	});
});
