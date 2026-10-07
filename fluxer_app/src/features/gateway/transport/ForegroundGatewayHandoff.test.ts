// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {SNAPSHOT_SCHEMA_EPOCH} from '@app/features/gateway/snapshot/SnapshotEntities';
import type {StateSnapshotCapture} from '@app/features/gateway/snapshot/SnapshotTypes';
import type {BackgroundGatewaySessionManager} from '@app/features/gateway/transport/BackgroundGatewaySessionManager';
import {
	type ForegroundDemotionRetirementReason,
	ForegroundGatewayHandoff,
} from '@app/features/gateway/transport/ForegroundGatewayHandoff';
import type {WarmForegroundCandidate} from '@app/features/gateway/transport/ForegroundPromoteCoordinator';
import type {GatewaySocket, GatewaySocketProperties} from '@app/features/gateway/transport/GatewaySocket';
import type {PooledGatewayConnection} from '@app/features/gateway/transport/PooledGatewayConnection';
import type {Account} from '@app/features/platform/state/AuthSession';
import {beforeEach, describe, expect, test, vi} from 'vitest';

const mocks = vi.hoisted(() => ({
	accounts: new Map<string, unknown>(),
	events: [] as Array<string>,
	adoptSocketError: null as Error | null,
	snapshotAvailable: true,
}));

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});

vi.mock('@lingui/react/macro', () => ({
	Trans: () => null,
	useLingui: () => ({i18n: {_: (descriptor: {message?: string}) => descriptor.message ?? '', locale: 'en'}}),
}));

vi.mock('@app/features/channel/state/Channels', () => ({default: {getChannel: () => undefined}}));

vi.mock('@app/features/navigation/state/Navigation', () => ({default: {channelId: null}}));

vi.mock('@app/features/messaging/state/AttachmentUrlRefresher', () => ({default: {reset: vi.fn()}}));

vi.mock('@app/features/platform/state/AuthSession', () => ({
	default: {getAccount: (accountKey: string) => mocks.accounts.get(accountKey) ?? null},
}));

vi.mock('@app/features/gateway/snapshot/SnapshotHydration', () => ({
	captureForegroundMentionCounts: () => new Map(),
	hydrateStoresFromSnapshotScope: () => Promise.resolve(),
}));

vi.mock('@app/features/gateway/snapshot/SnapshotSync', () => ({
	default: {
		flush: (storageKey: string) => {
			mocks.events.push(`snapshot:flush:${storageKey}`);
			return Promise.resolve();
		},
		demoteForeground: (storageKey: string) => {
			mocks.events.push(`snapshot:demote:${storageKey}`);
			return mocks.snapshotAvailable ? demotionCapture() : null;
		},
		captureDemotionSnapshot: () => null,
		prepareForegroundPromotion: () => false,
		commitForegroundPromotion: () => false,
		abortForegroundPromotion: (storageKey: string) => {
			mocks.events.push(`snapshot:abort:${storageKey}`);
		},
		finalizeForegroundPromotion: () => undefined,
	},
}));

vi.mock('@app/features/gateway/transport/BackgroundGatewaySession', () => ({
	BackgroundGatewaySession: class BackgroundGatewaySession {
		constructor(config: {account: Account}) {
			mocks.events.push(`session:new:${config.account.storageKey}`);
		}

		adoptSocket(): void {
			mocks.events.push('session:adopt');
			if (mocks.adoptSocketError !== null) {
				throw mocks.adoptSocketError;
			}
		}

		stop(): void {
			mocks.events.push('session:stop');
		}
	},
}));

const ACCOUNT_KEY = 'secondary.test::200';
const TARGET_KEY = 'primary.test::100';
const SESSION_ID = 'session-200';

const PROPERTIES: GatewaySocketProperties = {
	os: 'macos',
	browser: 'Fluxer Client',
	device: 'desktop',
	locale: 'en-US',
	user_agent: 'test',
	browser_version: '1',
	os_version: '1',
	build_version: '1',
};

function retainedAccount(): Account {
	return {
		storageKey: ACCOUNT_KEY,
		userId: '200',
		token: 'token-200',
		lastActive: 10,
		isValid: true,
		instance: {
			gatewayEndpoint: 'wss://gateway.secondary.test',
			features: {max_background_gateway_connections: 4},
		} as Account['instance'],
	};
}

function demotionCapture(): StateSnapshotCapture {
	return {entries: {}, cursor: {sessionId: SESSION_ID, schemaEpoch: SNAPSHOT_SCHEMA_EPOCH, updatedAt: 0}};
}

class FakeGatewaySocket {
	sessionId: string | null = SESSION_ID;
	connected = true;
	readonly resets: Array<boolean> = [];

	getSessionId(): string | null {
		return this.sessionId;
	}

	isConnected(): boolean {
		return this.connected;
	}

	reset(shouldReconnect: boolean): void {
		this.resets.push(shouldReconnect);
	}
}

class FakeForegroundConnection {
	foregroundAccountKey: string | null = ACCOUNT_KEY;
	handoverAvailable = true;
	consumeResult = true;
	socket: FakeGatewaySocket | null = new FakeGatewaySocket();

	beginForegroundDemotionHandover(accountKey: string): boolean {
		mocks.events.push(`foreground:begin-handover:${accountKey}`);
		return this.handoverAvailable;
	}

	consumeForegroundDemotionHandover(accountKey: string): Promise<boolean> {
		mocks.events.push(`foreground:consume-handover:${accountKey}`);
		return Promise.resolve(this.consumeResult);
	}

	rollbackForegroundDemotionHandover(accountKey: string): Promise<void> {
		mocks.events.push(`foreground:rollback-handover:${accountKey}`);
		return Promise.resolve();
	}

	foregroundSocketForAccount(): GatewaySocket | null {
		return this.socket as unknown as GatewaySocket | null;
	}

	detachForegroundSocket(): GatewaySocket | null {
		mocks.events.push('foreground:detach');
		return this.socket as unknown as GatewaySocket | null;
	}

	retireForAccountSwitch(): void {
		mocks.events.push('foreground:retire');
		this.foregroundAccountKey = null;
	}
}

class FakeBackgroundSessionManager {
	transportAvailable = true;
	cachedProperties: GatewaySocketProperties | null = PROPERTIES;
	readonly protectedAccountKeys: Array<string> = [];
	readonly adoptedAccountKeys: Array<string> = [];
	readonly candidates = new Map<string, WarmForegroundCandidate>();

	isTransportAvailable(): boolean {
		return this.transportAvailable;
	}

	preloadProperties(): Promise<GatewaySocketProperties | null> {
		return Promise.resolve(this.cachedProperties);
	}

	protect(accountKey: string): void {
		mocks.events.push(`background:protect:${accountKey}`);
		this.protectedAccountKeys.push(accountKey);
	}

	unprotect(accountKey: string): void {
		mocks.events.push(`background:unprotect:${accountKey}`);
		const index = this.protectedAccountKeys.indexOf(accountKey);
		if (index >= 0) {
			this.protectedAccountKeys.splice(index, 1);
		}
	}

	sync(): void {}

	invalidateSync(): void {}

	find(accountKey: string): WarmForegroundCandidate | null {
		return this.candidates.get(accountKey) ?? null;
	}

	adopt(accountKey: string): void {
		mocks.events.push(`background:adopt:${accountKey}`);
		this.adoptedAccountKeys.push(accountKey);
	}

	stop(accountKey: string): void {
		mocks.events.push(`background:stop:${accountKey}`);
	}
}

interface Harness {
	foreground: FakeForegroundConnection;
	background: FakeBackgroundSessionManager;
	handoff: ForegroundGatewayHandoff;
}

function createHarness(): Harness {
	const foreground = new FakeForegroundConnection();
	const background = new FakeBackgroundSessionManager();
	const handoff = new ForegroundGatewayHandoff(
		foreground as unknown as PooledGatewayConnection,
		background as unknown as BackgroundGatewaySessionManager,
	);
	return {foreground, background, handoff};
}

beforeEach(() => {
	mocks.events.length = 0;
	mocks.adoptSocketError = null;
	mocks.snapshotAvailable = true;
	mocks.accounts.clear();
	mocks.accounts.set(ACCOUNT_KEY, retainedAccount());
});

describe('foreground gateway demotion retirement', () => {
	const cases: Array<{reason: ForegroundDemotionRetirementReason; arrange: (harness: Harness) => void}> = [
		{
			reason: 'no_foreground',
			arrange: ({foreground}) => {
				foreground.foregroundAccountKey = null;
			},
		},
		{
			reason: 'handover_unavailable',
			arrange: ({foreground}) => {
				foreground.handoverAvailable = false;
			},
		},
		{
			reason: 'socket_lost',
			arrange: ({foreground}) => {
				foreground.consumeResult = false;
			},
		},
		{
			reason: 'account_unavailable',
			arrange: () => {
				mocks.accounts.delete(ACCOUNT_KEY);
			},
		},
		{
			reason: 'transport_disabled',
			arrange: ({background}) => {
				background.transportAvailable = false;
			},
		},
		{
			reason: 'capacity_disabled',
			arrange: () => {
				const account = retainedAccount();
				mocks.accounts.set(ACCOUNT_KEY, {
					...account,
					instance: {
						...account.instance,
						features: {max_background_gateway_connections: 0},
					},
				});
			},
		},
		{
			reason: 'endpoint_unavailable',
			arrange: () => {
				const account = retainedAccount();
				mocks.accounts.set(ACCOUNT_KEY, {...account, instance: {...account.instance, gatewayEndpoint: ''}});
			},
		},
		{
			reason: 'metadata_unavailable',
			arrange: ({background}) => {
				background.cachedProperties = null;
			},
		},
		{
			reason: 'snapshot_unavailable',
			arrange: () => {
				mocks.snapshotAvailable = false;
			},
		},
	];

	for (const {reason, arrange} of cases) {
		test(`a demotion retires the foreground gateway with ${reason}`, async () => {
			const harness = createHarness();
			arrange(harness);

			const outcome = await harness.handoff.demoteForegroundForAccountSwitch();

			expect(outcome).toEqual({
				mode: 'retired',
				accountKey: reason === 'no_foreground' ? null : ACCOUNT_KEY,
				reason,
			});
			expect(mocks.events).toContain('foreground:retire');
			expect(harness.background.adoptedAccountKeys).toEqual([]);
			expect(mocks.events).not.toContain('session:adopt');
		});
	}

	test('a live foreground socket that lost its session id is retired rather than retained', async () => {
		const harness = createHarness();
		(harness.foreground.socket as FakeGatewaySocket).sessionId = null;

		const outcome = await harness.handoff.demoteForegroundForAccountSwitch();

		expect(outcome).toEqual({mode: 'retired', accountKey: ACCOUNT_KEY, reason: 'socket_lost'});
		expect(harness.background.adoptedAccountKeys).toEqual([]);
	});

	test('a demotion that clears every gate retains the socket as a background session', async () => {
		const harness = createHarness();

		const outcome = await harness.handoff.demoteForegroundForAccountSwitch();

		expect(outcome).toEqual({mode: 'retained', accountKey: ACCOUNT_KEY, sessionId: SESSION_ID});
		expect(harness.background.adoptedAccountKeys).toEqual([ACCOUNT_KEY]);
		expect(mocks.events).toContain('foreground:retire');
	});
});

describe('foreground gateway demotion adoption failure', () => {
	test('a background session that refuses the socket is stopped and the socket discarded once', async () => {
		const harness = createHarness();
		mocks.adoptSocketError = new Error('adoption rejected');

		await expect(harness.handoff.demoteForegroundForAccountSwitch()).rejects.toThrow(
			`Failed to retain foreground gateway ${ACCOUNT_KEY}`,
		);

		expect(mocks.events.filter((event) => event === 'session:stop')).toEqual(['session:stop']);
		expect((harness.foreground.socket as FakeGatewaySocket).resets).toEqual([false]);
		expect(harness.background.adoptedAccountKeys).toEqual([]);
		expect(mocks.events).toContain('foreground:retire');
	});
});

describe('foreground gateway promotion rollback', () => {
	function throwingCandidate(): WarmForegroundCandidate {
		return {
			isBackgroundConnectionReady: true,
			beginBackgroundDispatchHandover: () => {
				throw new Error('handover refused');
			},
			endBackgroundDispatchHandover: () => undefined,
			detachBackgroundSocket: () => null,
			waitForBackgroundConnectionReady: () => Promise.resolve(true),
			waitForBackgroundDispatchIdle: () => Promise.resolve(),
		};
	}

	test('a promotion that fails to begin rolls the demotion back and releases both accounts', async () => {
		const harness = createHarness();
		harness.background.candidates.set(TARGET_KEY, throwingCandidate());

		await expect(harness.handoff.beginPromotion(TARGET_KEY)).rejects.toThrow('handover refused');

		expect(mocks.events).toContain(`foreground:rollback-handover:${ACCOUNT_KEY}`);
		expect(harness.background.protectedAccountKeys).toEqual([]);
	});

	test('a failed promotion leaves the coordinator free for the next account', async () => {
		const harness = createHarness();
		harness.background.candidates.set(TARGET_KEY, throwingCandidate());

		await expect(harness.handoff.beginPromotion(TARGET_KEY)).rejects.toThrow('handover refused');

		await expect(harness.handoff.beginPromotion('other.test::300')).resolves.toBeUndefined();
	});
});
