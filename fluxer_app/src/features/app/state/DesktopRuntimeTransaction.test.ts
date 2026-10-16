// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {type RuntimeConfigSnapshot, runtimeSnapshotFromDiscovery} from '@app/features/app/state/InstanceSnapshotStore';
import {desktopLocalApiEndpoint} from '@app/features/platform/DesktopLocalAppRuntime';
import {BOOTSTRAP_APP_PUBLIC} from '@app/features/platform/state/__fixtures__/RuntimeBootstrapFixture';
import type {
	DesktopRuntimeAbort,
	DesktopRuntimeCommit,
	DesktopRuntimeDeactivation,
	DesktopRuntimeFinalization,
	DesktopRuntimePreparation,
	DesktopRuntimeRollback,
} from '@fluxer/desktop_ipc/src/LocalAppRuntimeContract';
import {parseInstanceDiscoveryDocument} from '@fluxer/instance_bootstrap/src/Discovery';
import type {InstanceDiscoveryResponse} from '@fluxer/instance_bootstrap/src/Types';
import type {LimitConfigSnapshot} from '@fluxer/limits/src/LimitTypes';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

vi.mock('@app/features/platform/DesktopLocalAppRuntime', async (importOriginal) => ({
	...(await importOriginal<typeof import('@app/features/platform/DesktopLocalAppRuntime')>()),
	isDesktopLocalAppDocument: () => true,
}));

const EMPTY_LIMITS: LimitConfigSnapshot = {version: 1, traitDefinitions: [], rules: []};

function discoveryDocument(host: string): InstanceDiscoveryResponse {
	return {
		api_code_version: 1,
		endpoints: {
			api: `https://${host}/api`,
			api_client: `https://${host}/api`,
			api_public: `https://${host}/api`,
			gateway: `wss://gateway.${host}`,
			media: `https://media.${host}`,
			static_cdn: `https://cdn.${host}`,
			marketing: `https://${host}`,
			admin: `https://admin.${host}`,
			invite: `https://${host}/invite`,
			gift: `https://${host}/gift`,
			webapp: `https://app.${host}`,
			upload_relay: `https://upload.${host}`,
		},
		captcha: {provider: 'none'},
		features: {
			voice_enabled: false,
			stripe_enabled: false,
			self_hosted: true,
			presigned_attachment_uploads: false,
			emails_enabled: false,
			premium_enabled: false,
			stripe_serviceable: false,
			phone_verification_enabled: false,
		},
		gif: {provider: 'klipy', display_name: 'Klipy', attribution_required: false},
		sso: {enabled: false, enforced: false, display_name: null, redirect_uri: `https://${host}/sso`},
		registration: {mode: 'open', admin_registration_urls_enabled: true},
		community: {
			single_community: false,
			single_community_guild_id: null,
			direct_messages_disabled: false,
			guild_create_access: true,
		},
		services: {gif_enabled: true, youtube_enabled: false, bluesky_enabled: false},
		limits: EMPTY_LIMITS,
		push: {public_vapid_key: null},
		app_public: BOOTSTRAP_APP_PUBLIC,
	};
}

function snapshotFor(host: string): RuntimeConfigSnapshot {
	return runtimeSnapshotFromDiscovery(parseInstanceDiscoveryDocument(discoveryDocument(host)));
}

interface BridgeScript {
	readonly prepare?: (request: {preparationId: string; instanceKey: string}) => DesktopRuntimePreparation;
	readonly commit?: (request: {
		preparationId: string;
		instanceKey: string;
		baseRevision: number;
	}) => DesktopRuntimeCommit | Promise<DesktopRuntimeCommit>;
	readonly abort?: () => DesktopRuntimeAbort | Promise<DesktopRuntimeAbort>;
	readonly finalize?: (request: DesktopRuntimeFinalization) => DesktopRuntimeFinalization;
	readonly rollback?: () => DesktopRuntimeRollback;
	readonly deactivate?: () => DesktopRuntimeDeactivation;
	readonly baseRevision?: number;
	readonly baseActiveInstanceKey?: string | null;
}

function installBridge(host: string, script: BridgeScript = {}) {
	const calls: Array<[string, unknown]> = [];
	const document = discoveryDocument(host);
	const instanceKey = `https://${host}/api`;
	const baseRevision = script.baseRevision ?? 0;
	const baseActiveInstanceKey = script.baseActiveInstanceKey ?? null;
	const record = <T>(method: string, request: unknown, produce: () => T): T => {
		calls.push([method, request]);
		return produce();
	};
	const api = {
		prepare: async (request: {preparationId: string; instanceKey: string}) =>
			record('prepare', request, () =>
				script.prepare == null
					? {
							preparationId: request.preparationId,
							instanceKey,
							apiEndpoint: desktopLocalApiEndpoint(instanceKey),
							remoteApiEndpoint: instanceKey,
							document,
							baseRevision,
							baseActiveInstanceKey,
						}
					: script.prepare(request),
			),
		commit: async (request: {preparationId: string; instanceKey: string; baseRevision: number}) =>
			record('commit', request, () =>
				script.commit == null
					? {
							preparationId: request.preparationId,
							instanceKey: request.instanceKey,
							baseRevision: request.baseRevision,
							revision: request.baseRevision + 1,
						}
					: script.commit(request),
			),
		abort: async (request: {preparationId: string}) =>
			record('abort', request, () =>
				script.abort == null
					? {disposition: 'aborted' as const, revision: baseRevision, activeInstanceKey: baseActiveInstanceKey}
					: script.abort(),
			),
		finalize: async (request: DesktopRuntimeFinalization) =>
			record('finalize', request, () => (script.finalize == null ? request : script.finalize(request))),
		rollback: async (request: {preparationId: string; committedRevision: number}) =>
			record('rollback', request, () =>
				script.rollback == null
					? {revision: request.committedRevision + 1, activeInstanceKey: baseActiveInstanceKey}
					: script.rollback(),
			),
		deactivate: async (request: {rendererActiveInstanceKey: string}) =>
			record('deactivate', request, () =>
				script.deactivate == null
					? {revision: baseRevision + 1, deactivatedInstanceKey: request.rendererActiveInstanceKey}
					: script.deactivate(),
			),
	};
	(window as {electron?: unknown}).electron = {desktopRuntimeConfig: api};
	return {calls, instanceKey, snapshot: snapshotFor(host)};
}

type TransactionClient = typeof import('@app/features/app/state/DesktopRuntimeTransaction')['default'];

let transactions: TransactionClient;
let DesktopRuntimeTransactionError: typeof import('@app/features/app/state/DesktopRuntimeTransaction').DesktopRuntimeTransactionError;

beforeEach(async () => {
	vi.resetModules();
	const module = await import('@app/features/app/state/DesktopRuntimeTransaction');
	transactions = module.default;
	DesktopRuntimeTransactionError = module.DesktopRuntimeTransactionError;
});

afterEach(() => {
	delete (window as {electron?: unknown}).electron;
});

describe('prepare/commit/publish/finalize', () => {
	test('every bridge call carries the identity the preparation handed back', async () => {
		const bridge = installBridge('one.test');

		const prepared = await transactions.prepare(bridge.snapshot, null);
		expect(prepared.transportApiEndpoint).toBe(desktopLocalApiEndpoint(bridge.instanceKey));
		expect(prepared.snapshot.apiEndpoint).toBe(bridge.instanceKey);

		const committed = await transactions.commit(prepared);
		expect(committed.committedRevision).toBe(1);
		expect(committed.previousPublishedRevision).toBe(0);

		transactions.publish(committed);
		await transactions.finalize(committed);

		const preparationId = prepared.preparation.preparationId;
		expect(bridge.calls).toEqual([
			['prepare', {preparationId, instanceKey: bridge.instanceKey}],
			['commit', {preparationId, instanceKey: bridge.instanceKey, baseRevision: 0}],
			['finalize', {preparationId, instanceKey: bridge.instanceKey, baseRevision: 0, committedRevision: 1}],
		]);
	});

	test('a finalization acknowledgement that is not exact is refused', async () => {
		const bridge = installBridge('one.test', {
			finalize: (request) => ({...request, committedRevision: request.committedRevision + 1}),
		});

		const committed = await transactions.commit(await transactions.prepare(bridge.snapshot, null));
		transactions.publish(committed);

		await expect(transactions.finalize(committed)).rejects.toThrow(
			'Desktop runtime finalization acknowledgement is not exact',
		);
	});
});

describe('commit', () => {
	test('a commit that skips a revision is aborted instead of published', async () => {
		const bridge = installBridge('one.test', {
			commit: (request) => ({...request, revision: request.baseRevision + 2}),
		});
		const prepared = await transactions.prepare(bridge.snapshot, null);

		await expect(transactions.commit(prepared)).rejects.toThrow(
			'Desktop runtime commit does not match its preparation',
		);
		expect(bridge.calls.map(([method]) => method)).toEqual(['prepare', 'commit', 'abort']);
		expect(bridge.calls.at(-1)).toEqual(['abort', {preparationId: prepared.preparation.preparationId}]);
	});

	test('an abort that restored a different base instance is reported instead of the original failure', async () => {
		const bridge = installBridge('one.test', {
			baseActiveInstanceKey: 'https://base.test/api',
			commit: () => {
				throw new Error('main process refused the commit');
			},
			abort: () => ({disposition: 'rolled-back', revision: 2, activeInstanceKey: 'https://other.test/api'}),
		});
		const prepared = await transactions.prepare(bridge.snapshot, null);

		const failure = await transactions.commit(prepared).catch((error: unknown) => error);
		expect(failure).toBeInstanceOf(AggregateError);
		expect((failure as AggregateError).name).toBe('DesktopRuntimeTransactionRollbackError');
		expect((failure as AggregateError).errors.map((error: unknown) => (error as Error).message)).toEqual([
			'main process refused the commit',
			'Desktop runtime abort restored a different base instance',
		]);
	});

	test('an abort that lost the transaction is reported instead of the original failure', async () => {
		const bridge = installBridge('one.test', {
			commit: () => {
				throw new Error('main process refused the commit');
			},
			abort: () => ({disposition: 'absent', revision: 0, activeInstanceKey: null}),
		});
		const prepared = await transactions.prepare(bridge.snapshot, null);

		const failure = await transactions.commit(prepared).catch((error: unknown) => error);
		expect((failure as AggregateError).errors.map((error: unknown) => (error as Error).message)).toEqual([
			'main process refused the commit',
			'Desktop runtime abort lost its prepared transaction',
		]);
	});

	test('a failing commit whose rollback also fails carries both errors', async () => {
		const commitError = new Error('main process refused the commit');
		const rollbackError = new Error('abort channel is gone');
		const bridge = installBridge('one.test', {
			commit: () => {
				throw commitError;
			},
			abort: () => {
				throw rollbackError;
			},
		});
		const prepared = await transactions.prepare(bridge.snapshot, null);

		const failure = await transactions.commit(prepared).catch((error: unknown) => error);
		expect(failure).toBeInstanceOf(AggregateError);
		expect((failure as AggregateError).errors).toEqual([commitError, rollbackError]);
	});
});

describe('publish', () => {
	test('the same commit cannot be published twice', async () => {
		const bridge = installBridge('one.test');
		const committed = await transactions.commit(await transactions.prepare(bridge.snapshot, null));

		transactions.publish(committed);

		expect(() => transactions.publish(committed)).toThrow(DesktopRuntimeTransactionError);
		expect(() => transactions.publish(committed)).toThrow(
			'Desktop runtime commit was superseded before renderer publication',
		);
	});

	test('a rolled-back publication frees the revision for the next commit', async () => {
		const bridge = installBridge('one.test');
		const first = await transactions.commit(await transactions.prepare(bridge.snapshot, null));
		transactions.publish(first);
		transactions.rollbackPublished(first);

		expect(() => transactions.publish(first)).not.toThrow();
	});

	test('rolling back a publication that never happened is refused', async () => {
		const bridge = installBridge('one.test');
		const committed = await transactions.commit(await transactions.prepare(bridge.snapshot, null));

		expect(() => transactions.rollbackPublished(committed)).toThrow(
			'Published desktop runtime was superseded before renderer rollback',
		);
	});
});

describe('deactivate', () => {
	test('a deactivation that does not advance past the published revision is refused', async () => {
		const bridge = installBridge('one.test', {
			deactivate: () => ({revision: 1, deactivatedInstanceKey: 'https://one.test/api'}),
		});
		const committed = await transactions.commit(await transactions.prepare(bridge.snapshot, null));
		transactions.publish(committed);

		await expect(transactions.deactivate(bridge.instanceKey)).rejects.toThrow(
			'Desktop runtime deactivation returned a stale revision',
		);
	});

	test('a deactivation that cleared another instance is refused', async () => {
		const bridge = installBridge('one.test', {
			deactivate: () => ({revision: 9, deactivatedInstanceKey: 'https://other.test/api'}),
		});

		await expect(transactions.deactivate(bridge.instanceKey)).rejects.toThrow(
			'Desktop runtime deactivation cleared a different instance',
		);
	});

	test('a deactivation moves the published revision forward for later preparations', async () => {
		const bridge = installBridge('one.test', {
			deactivate: () => ({revision: 4, deactivatedInstanceKey: `https://one.test/api`}),
		});
		await transactions.deactivate(bridge.instanceKey);

		await expect(transactions.prepare(bridge.snapshot, null)).rejects.toThrow(
			'Desktop runtime preparation was superseded before validation',
		);
	});
});

describe('prepare', () => {
	test('a preparation answering a different identity is aborted and reported', async () => {
		const bridge = installBridge('one.test', {
			prepare: (request) => ({
				preparationId: `${request.preparationId}-other`,
				instanceKey: request.instanceKey,
				apiEndpoint: desktopLocalApiEndpoint(request.instanceKey),
				remoteApiEndpoint: request.instanceKey,
				document: discoveryDocument('one.test'),
				baseRevision: 0,
				baseActiveInstanceKey: null,
			}),
		});

		await expect(transactions.prepare(bridge.snapshot, null)).rejects.toThrow(
			'Desktop runtime returned a different preparation identity',
		);
		expect(bridge.calls.map(([method]) => method)).toEqual(['prepare', 'abort']);
	});

	test('a preparation whose transport does not match its own document is aborted', async () => {
		const bridge = installBridge('one.test', {
			prepare: (request) => ({
				preparationId: request.preparationId,
				instanceKey: request.instanceKey,
				apiEndpoint: desktopLocalApiEndpoint('https://elsewhere.test/api'),
				remoteApiEndpoint: request.instanceKey,
				document: discoveryDocument('one.test'),
				baseRevision: 0,
				baseActiveInstanceKey: null,
			}),
		});

		await expect(transactions.prepare(bridge.snapshot, null)).rejects.toThrow(
			'Desktop runtime preparation transport does not match its discovery document',
		);
		expect(bridge.calls.map(([method]) => method)).toEqual(['prepare', 'abort']);
	});

	test('an already aborted signal never reaches the bridge', async () => {
		const bridge = installBridge('one.test');

		await expect(transactions.prepare(bridge.snapshot, AbortSignal.abort())).rejects.toThrow();
		expect(bridge.calls.map(([method]) => method)).toEqual(['abort']);
	});
});
