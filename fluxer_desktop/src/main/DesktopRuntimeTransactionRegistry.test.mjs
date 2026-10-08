// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import {beforeEach, describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import {installElectronStub, installTestModuleStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

const PROTOCOL_KEY = '__fluxerDesktopRuntimeTransactionTestProtocol__';
const DISCOVERY_KEY = '__fluxerDesktopRuntimeTransactionTestDiscovery__';

installTestModuleStub(
	'@electron/main/LocalAppProtocol',
	`export const getDesktopLocalAppProtocol = () => globalThis[${JSON.stringify(PROTOCOL_KEY)}];`,
);
installTestModuleStub(
	'@electron/main/DesktopRuntimeDiscovery',
	`export const resolveDesktopRuntimePlan = async (request) => await globalThis[${JSON.stringify(DISCOVERY_KEY)}](request);`,
);

const REGISTRY_SOURCE_PATH = fileURLToPath(new URL('./DesktopRuntimeTransactionRegistry.ts', import.meta.url));

const {DesktopRuntimeTransactionRegistry} = await import(
	`data:text/javascript,${encodeURIComponent(
		esbuild.transformSync(readFileSync(REGISTRY_SOURCE_PATH, 'utf8'), {
			loader: 'ts',
			format: 'esm',
			platform: 'node',
			target: 'node22',
		}).code,
	)}`
);

const MAX_RUNTIME_PREPARATIONS = 8;

const ONE = 'https://one.test/api';
const TWO = 'https://two.test/api';
const BASE = 'https://base.test/api';

function createPlan(instanceKey) {
	return {
		instanceKey,
		document: {instance: instanceKey},
		endpoints: {
			apiEndpoint: instanceKey,
			apiPublicEndpoint: null,
			webAppEndpoint: null,
			mediaEndpoint: null,
			staticCdnEndpoint: null,
			uploadRelayEndpoint: null,
			gatewayEndpoint: null,
			inviteEndpoint: null,
			giftEndpoint: null,
		},
		selfHosted: true,
		desktopModulesEnabled: null,
	};
}

function createProtocol(activePlan = null) {
	const shutdown = new AbortController();
	let active = activePlan;
	return {
		activations: [],
		getActivePlan: () => active,
		findPlanForRoute: () => null,
		getShutdownSignal: () => shutdown.signal,
		activateRuntimePlan(plan) {
			active = plan;
			this.activations.push(plan.instanceKey);
		},
		deactivateRuntimePlan() {
			active = null;
			this.activations.push(null);
		},
	};
}

function createRendererDocument(name) {
	const watchers = new Set();
	const owner = {
		sender: name,
		frame: name,
		isCurrent: () => true,
		matchesEvent: (event) => event.document === owner,
		requireCurrent: () => undefined,
		watchInvalidation(onInvalidated) {
			const watcher = {onInvalidated, dispose: () => watchers.delete(watcher)};
			watchers.add(watcher);
			return watcher;
		},
	};
	return {owner, event: {document: owner}, liveWatchers: () => watchers.size};
}

const rendererDocuments = {capture: (event) => event.document};

function transactionRequest(preparation) {
	return {
		preparationId: preparation.preparationId,
		instanceKey: preparation.instanceKey,
		baseRevision: preparation.baseRevision,
	};
}

function committedRequest(preparation, committedRevision) {
	return {...transactionRequest(preparation), committedRevision};
}

function named(name) {
	return (error) => {
		assert.equal(error.name, name, `expected ${name}, got ${error.name}: ${error.message}`);
		return true;
	};
}

let protocol;
let discovered;
let document;
let registry;

beforeEach(() => {
	protocol = createProtocol();
	discovered = new Map([
		[ONE, createPlan(ONE)],
		[TWO, createPlan(TWO)],
	]);
	globalThis[PROTOCOL_KEY] = protocol;
	globalThis[DISCOVERY_KEY] = async ({input}) => {
		const plan = discovered.get(input);
		if (plan == null) {
			throw new Error(`no discovery fixture for ${input}`);
		}
		return plan;
	};
	document = createRendererDocument('document-one');
	registry = new DesktopRuntimeTransactionRegistry(rendererDocuments);
});

describe('DesktopRuntimeTransactionRegistry.commit', () => {
	test('a preparation cannot commit at a revision an earlier rollback already moved past', async () => {
		const basePlan = createPlan(BASE);
		protocol = createProtocol(basePlan);
		globalThis[PROTOCOL_KEY] = protocol;
		registry = new DesktopRuntimeTransactionRegistry(rendererDocuments);
		registry.initialize();
		const first = await registry.prepare(document.event, {preparationId: 'first', instanceKey: ONE});
		const second = await registry.prepare(document.event, {preparationId: 'second', instanceKey: ONE});
		assert.equal(second.baseRevision, first.baseRevision);
		assert.equal(second.baseActiveInstanceKey, BASE);
		registry.commit(document.event, transactionRequest(first));
		registry.abort(document.event, {preparationId: 'first'});
		assert.equal(protocol.getActivePlan(), basePlan);

		assert.throws(
			() => registry.commit(document.event, transactionRequest(second)),
			named('DesktopRuntimePreparationSupersededError'),
		);
		assert.equal(protocol.getActivePlan(), basePlan);
		assert.deepEqual(protocol.activations, [ONE, BASE]);
	});

	test('a preparation left behind by another commit is refused as superseded', async () => {
		registry.initialize();
		const first = await registry.prepare(document.event, {preparationId: 'first', instanceKey: ONE});
		const second = await registry.prepare(document.event, {preparationId: 'second', instanceKey: TWO});
		assert.equal(first.baseRevision, 0);
		assert.equal(second.baseRevision, 0);

		const commit = registry.commit(document.event, transactionRequest(first));
		assert.deepEqual(commit, {preparationId: 'first', instanceKey: ONE, baseRevision: 0, revision: 1});
		registry.finalize(document.event, committedRequest(first, 1));

		assert.throws(
			() => registry.commit(document.event, transactionRequest(second)),
			named('DesktopRuntimePreparationSupersededError'),
		);
		assert.equal(protocol.getActivePlan().instanceKey, ONE);
		assert.deepEqual(protocol.activations, [ONE]);
	});

	test('a commit whose base activation moved underneath it is refused on identity', async () => {
		registry.initialize();
		const prepared = await registry.prepare(document.event, {preparationId: 'first', instanceKey: ONE});
		assert.equal(prepared.baseActiveInstanceKey, null);

		protocol.activateRuntimePlan(createPlan(BASE));

		assert.throws(
			() => registry.commit(document.event, transactionRequest(prepared)),
			named('DesktopRuntimePreparationIdentityError'),
		);
		assert.equal(protocol.getActivePlan().instanceKey, BASE);
		assert.deepEqual(protocol.activations, [BASE]);
	});

	test('a second provisional commit is refused while the first is still open', async () => {
		registry.initialize();
		const first = await registry.prepare(document.event, {preparationId: 'first', instanceKey: ONE});
		const second = await registry.prepare(document.event, {preparationId: 'second', instanceKey: TWO});
		registry.commit(document.event, transactionRequest(first));

		assert.throws(
			() => registry.commit(document.event, transactionRequest(second)),
			named('DesktopRuntimeCommitInProgressError'),
		);
		assert.equal(protocol.getActivePlan().instanceKey, ONE);
	});
});

describe('DesktopRuntimeTransactionRegistry.abort', () => {
	test('aborting a committed preparation restores the exact plan it replaced', async () => {
		const basePlan = createPlan(BASE);
		protocol = createProtocol(basePlan);
		globalThis[PROTOCOL_KEY] = protocol;
		registry = new DesktopRuntimeTransactionRegistry(rendererDocuments);
		registry.initialize();
		const prepared = await registry.prepare(document.event, {preparationId: 'first', instanceKey: ONE});
		assert.equal(prepared.baseRevision, 1);
		assert.equal(prepared.baseActiveInstanceKey, BASE);
		registry.commit(document.event, transactionRequest(prepared));
		assert.equal(protocol.getActivePlan().instanceKey, ONE);

		const aborted = registry.abort(document.event, {preparationId: 'first'});

		assert.deepEqual(aborted, {disposition: 'rolled-back', revision: 3, activeInstanceKey: BASE});
		assert.equal(protocol.getActivePlan(), basePlan);
		assert.deepEqual(protocol.activations, [ONE, BASE]);
		assert.equal(document.liveWatchers(), 0);
	});

	test('aborting a committed preparation that replaced nothing deactivates the runtime again', async () => {
		registry.initialize();
		const prepared = await registry.prepare(document.event, {preparationId: 'first', instanceKey: ONE});
		registry.commit(document.event, transactionRequest(prepared));

		const aborted = registry.abort(document.event, {preparationId: 'first'});

		assert.deepEqual(aborted, {disposition: 'rolled-back', revision: 2, activeInstanceKey: null});
		assert.equal(protocol.getActivePlan(), null);
		assert.deepEqual(protocol.activations, [ONE, null]);
	});

	test('aborting a prepared-but-uncommitted preparation leaves the runtime alone', async () => {
		registry.initialize();
		await registry.prepare(document.event, {preparationId: 'first', instanceKey: ONE});

		const aborted = registry.abort(document.event, {preparationId: 'first'});

		assert.deepEqual(aborted, {disposition: 'aborted', revision: 0, activeInstanceKey: null});
		assert.deepEqual(protocol.activations, []);
		assert.deepEqual(registry.abort(document.event, {preparationId: 'first'}), {
			disposition: 'absent',
			revision: 0,
			activeInstanceKey: null,
		});
	});
});

describe('DesktopRuntimeTransactionRegistry.rollback', () => {
	test('rolling back a committed preparation restores the exact plan it replaced', async () => {
		const basePlan = createPlan(BASE);
		protocol = createProtocol(basePlan);
		globalThis[PROTOCOL_KEY] = protocol;
		registry = new DesktopRuntimeTransactionRegistry(rendererDocuments);
		registry.initialize();
		const prepared = await registry.prepare(document.event, {preparationId: 'first', instanceKey: ONE});
		const commit = registry.commit(document.event, transactionRequest(prepared));

		const rollback = registry.rollback(document.event, committedRequest(prepared, commit.revision));

		assert.deepEqual(rollback, {revision: commit.revision + 1, activeInstanceKey: BASE});
		assert.equal(protocol.getActivePlan(), basePlan);
		assert.throws(
			() => registry.rollback(document.event, committedRequest(prepared, commit.revision)),
			named('DesktopRuntimePreparationNotFoundError'),
		);
	});

	test('a finalized preparation can no longer be rolled back', async () => {
		registry.initialize();
		const prepared = await registry.prepare(document.event, {preparationId: 'first', instanceKey: ONE});
		const commit = registry.commit(document.event, transactionRequest(prepared));
		registry.finalize(document.event, committedRequest(prepared, commit.revision));

		assert.throws(
			() => registry.rollback(document.event, committedRequest(prepared, commit.revision)),
			named('DesktopRuntimePreparationNotFoundError'),
		);
		assert.equal(protocol.getActivePlan().instanceKey, ONE);
		assert.deepEqual(protocol.activations, [ONE]);
	});
});

describe('DesktopRuntimeTransactionRegistry.prepare', () => {
	test('a renderer cannot hold more preparations than the registry admits', async () => {
		registry.initialize();
		for (let index = 0; index < MAX_RUNTIME_PREPARATIONS; index += 1) {
			discovered.set(`https://instance-${index}.test/api`, createPlan(`https://instance-${index}.test/api`));
			const prepared = await registry.prepare(document.event, {
				preparationId: `preparation-${index}`,
				instanceKey: `https://instance-${index}.test/api`,
			});
			assert.equal(prepared.instanceKey, `https://instance-${index}.test/api`);
		}

		await assert.rejects(
			registry.prepare(document.event, {preparationId: 'overflow', instanceKey: ONE}),
			named('DesktopRuntimePreparationCapacityError'),
		);
		assert.equal(document.liveWatchers(), MAX_RUNTIME_PREPARATIONS);
		assert.deepEqual(registry.abort(document.event, {preparationId: 'overflow'}), {
			disposition: 'absent',
			revision: 0,
			activeInstanceKey: null,
		});
	});

	test('a duplicate preparation id is refused before it can shadow the live one', async () => {
		registry.initialize();
		await registry.prepare(document.event, {preparationId: 'first', instanceKey: ONE});

		await assert.rejects(
			registry.prepare(document.event, {preparationId: 'first', instanceKey: TWO}),
			named('DesktopRuntimePreparationAlreadyExistsError'),
		);
		assert.equal(document.liveWatchers(), 1);
	});

	test('a preparation belongs to the renderer document that opened it', async () => {
		registry.initialize();
		const prepared = await registry.prepare(document.event, {preparationId: 'first', instanceKey: ONE});
		const other = createRendererDocument('document-two');

		assert.throws(
			() => registry.commit(other.event, transactionRequest(prepared)),
			named('DesktopRuntimePreparationOwnershipError'),
		);
		assert.deepEqual(protocol.activations, []);
	});
});
