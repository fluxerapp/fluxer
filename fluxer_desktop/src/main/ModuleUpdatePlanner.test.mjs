// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Buffer} from 'node:buffer';
import {describe, test} from 'node:test';
import './LocalAppTestSupport.test.mjs';

const {parseModuleUpdateManifest} = await import('@electron/main/ModuleManifest');
const {MODULE_BOOT_ATTEMPT_ROLLBACK_THRESHOLD} = await import('@electron/main/ModuleStore');
const {ModuleUnreachableLaunchDecision, ModuleUpdatePlanner, ModuleUpdaterBlockReason} = await import(
	'@electron/main/ModuleUpdatePlanner'
);
const {parseModuleVersion} = await import('@electron/main/ModuleVersion');

const SHELL_VERSION = '2026.823.1';
const RELEASE_CHANNEL = 'canary';
const PLATFORM = 'darwin';
const ARCH = 'arm64';
const FETCHED_AT = '2026-08-23T00:00:00.000Z';
const RENDERER_SHA = 'a'.repeat(64);
const OVERLAY_SHA = 'b'.repeat(64);

const TARGET = {releaseChannel: RELEASE_CHANNEL, platform: PLATFORM, arch: ARCH};

function manifest({shellMinimum = SHELL_VERSION, modules = {}, required = null} = {}) {
	const entries = Object.entries(modules);
	return parseModuleUpdateManifest(
		Buffer.from(
			JSON.stringify({
				manifest_version: 1,
				release_channel: RELEASE_CHANNEL,
				platform: PLATFORM,
				arch: ARCH,
				build_version: SHELL_VERSION,
				pub_date: FETCHED_AT,
				metadata_version: 1,
				shell: {latest_version: SHELL_VERSION, minimum_version: shellMinimum},
				modules: Object.fromEntries(
					entries.map(([moduleName, entry]) => [
						moduleName,
						{
							sha256: entry.sha256,
							bytes: 1024,
							url: `https://api.invalid/dl/desktop/${entry.sha256}.br`,
							minimum_shell_version: entry.minimumShellVersion ?? null,
							maximum_shell_version: entry.maximumShellVersion ?? null,
						},
					]),
				),
				required_modules: required ?? entries.map(([moduleName]) => moduleName),
			}),
			'utf8',
		),
		TARGET,
	);
}

function createStore({
	committed = {},
	floor = {},
	rejected = {},
	lastManifestFetch = FETCHED_AT,
	bootAttempt = 0,
	missing = [],
	installedManifests = {},
} = {}) {
	const absent = new Set(missing);
	return {
		getState: () => ({
			committed,
			floor,
			rejected,
			last_manifest_fetch: lastManifestFetch,
			boot_attempt: bootAttempt,
		}),
		getCommitted: () => committed,
		isRejected: (moduleName, sha256) => rejected[moduleName] === sha256,
		isInstalled: async (moduleName, sha256) => !absent.has(`${moduleName}:${sha256}`),
		getInstalledManifest: async (moduleName, sha256) => installedManifests[`${moduleName}:${sha256}`] ?? null,
	};
}

function createPlanner(
	store,
	{shellVersion = SHELL_VERSION, hasOfflineRenderer = false, bundledRendererVersion = null} = {},
) {
	return new ModuleUpdatePlanner(
		store,
		parseModuleVersion(shellVersion, 'desktop shell version'),
		hasOfflineRenderer,
		bundledRendererVersion == null ? null : parseModuleVersion(bundledRendererVersion, 'bundled renderer version'),
	);
}

function bundledPlanner(store, bundledRendererVersion = SHELL_VERSION) {
	return createPlanner(store, {hasOfflineRenderer: true, bundledRendererVersion});
}

function installedRenderer(sha256, buildVersion) {
	return {[`fluxer_renderer:${sha256}`]: {build_version: buildVersion}};
}

describe('ModuleUpdatePlanner.isShellCompatible', () => {
	test('an entry with no shell bounds is compatible', () => {
		const planner = createPlanner(createStore());
		assert.equal(planner.isShellCompatible({minimumShellVersion: null, maximumShellVersion: null}), true);
	});

	test('the shell floor and ceiling are inclusive', () => {
		const planner = createPlanner(createStore());
		const bound = parseModuleVersion(SHELL_VERSION, 'bound');
		assert.equal(planner.isShellCompatible({minimumShellVersion: bound, maximumShellVersion: null}), true);
		assert.equal(planner.isShellCompatible({minimumShellVersion: null, maximumShellVersion: bound}), true);
	});

	test('a shell below the entry floor is incompatible', () => {
		const planner = createPlanner(createStore());
		assert.equal(
			planner.isShellCompatible({
				minimumShellVersion: parseModuleVersion('2026.824.0', 'floor'),
				maximumShellVersion: null,
			}),
			false,
		);
	});

	test('a shell above the entry ceiling is incompatible', () => {
		const planner = createPlanner(createStore());
		assert.equal(
			planner.isShellCompatible({
				minimumShellVersion: null,
				maximumShellVersion: parseModuleVersion('2026.823.0', 'ceiling'),
			}),
			false,
		);
	});

	test('a prerelease shell does not satisfy the release it precedes', () => {
		const planner = createPlanner(createStore(), {shellVersion: '2026.823.1-rc.1'});
		assert.equal(
			planner.isShellCompatible({
				minimumShellVersion: parseModuleVersion(SHELL_VERSION, 'floor'),
				maximumShellVersion: null,
			}),
			false,
		);
	});
});

describe('ModuleUpdatePlanner.shellUpdateRequired', () => {
	test('a manifest whose shell floor matches this shell asks for nothing', () => {
		const planner = createPlanner(createStore());
		assert.equal(planner.shellUpdateRequired(manifest({modules: {fluxer_renderer: {sha256: RENDERER_SHA}}})), false);
	});

	test('a shell floor above this shell requires a shell update', () => {
		const planner = createPlanner(createStore());
		assert.equal(
			planner.shellUpdateRequired(
				manifest({shellMinimum: '2026.824.0', modules: {fluxer_renderer: {sha256: RENDERER_SHA}}}),
			),
			true,
		);
	});

	test('a required module that outruns this shell requires a shell update', () => {
		const planner = createPlanner(createStore());
		assert.equal(
			planner.shellUpdateRequired(
				manifest({modules: {fluxer_renderer: {sha256: RENDERER_SHA, minimumShellVersion: '2026.824.0'}}}),
			),
			true,
		);
	});

	test('a required module capped below this shell requires a shell update', () => {
		const planner = createPlanner(createStore());
		assert.equal(
			planner.shellUpdateRequired(
				manifest({modules: {fluxer_renderer: {sha256: RENDERER_SHA, maximumShellVersion: '2026.823.0'}}}),
			),
			true,
		);
	});

	test('an optional module that outruns this shell does not require a shell update', () => {
		const planner = createPlanner(createStore());
		assert.equal(
			planner.shellUpdateRequired(
				manifest({
					modules: {
						fluxer_renderer: {sha256: RENDERER_SHA},
						fluxer_overlay: {sha256: OVERLAY_SHA, minimumShellVersion: '2026.824.0'},
					},
					required: ['fluxer_renderer'],
				}),
			),
			false,
		);
	});
});

describe('ModuleUpdatePlanner.plan', () => {
	test('a shell-incompatible optional module keeps its committed copy instead of updating', async () => {
		const store = createStore({committed: {fluxer_renderer: RENDERER_SHA, fluxer_overlay: OVERLAY_SHA}});
		const planner = createPlanner(store);
		const plan = await planner.plan(
			manifest({
				modules: {
					fluxer_renderer: {sha256: RENDERER_SHA},
					fluxer_overlay: {sha256: 'c'.repeat(64), minimumShellVersion: '2026.824.0'},
				},
				required: ['fluxer_renderer'],
			}),
		);

		assert.deepEqual(plan.items, []);
		assert.deepEqual(plan.base, {fluxer_renderer: RENDERER_SHA, fluxer_overlay: OVERLAY_SHA});
	});

	test('a shell-incompatible optional module that is not installed is dropped entirely', async () => {
		const store = createStore({
			committed: {fluxer_renderer: RENDERER_SHA, fluxer_overlay: OVERLAY_SHA},
			missing: [`fluxer_overlay:${OVERLAY_SHA}`],
		});
		const planner = createPlanner(store);
		const plan = await planner.plan(
			manifest({
				modules: {
					fluxer_renderer: {sha256: RENDERER_SHA},
					fluxer_overlay: {sha256: 'c'.repeat(64), maximumShellVersion: '2026.823.0'},
				},
				required: ['fluxer_renderer'],
			}),
		);

		assert.deepEqual(plan.items, []);
		assert.deepEqual(plan.base, {fluxer_renderer: RENDERER_SHA});
	});
});

describe('ModuleUpdatePlanner.evaluateUnreachableLaunch', () => {
	test('a store that never reached the manifest feed blocks with nothing-installed', async () => {
		const planner = createPlanner(createStore({committed: {fluxer_renderer: RENDERER_SHA}, lastManifestFetch: null}));

		assert.deepEqual(await planner.evaluateUnreachableLaunch(true, false), {
			kind: ModuleUnreachableLaunchDecision.BLOCK,
			reason: ModuleUpdaterBlockReason.NOTHING_INSTALLED,
		});
	});

	test('an empty committed set with no offline renderer blocks with nothing-installed', async () => {
		const planner = createPlanner(createStore({committed: {}}));

		assert.deepEqual(await planner.evaluateUnreachableLaunch(true, false), {
			kind: ModuleUnreachableLaunchDecision.BLOCK,
			reason: ModuleUpdaterBlockReason.NOTHING_INSTALLED,
		});
	});

	test('an offline renderer carries an empty committed set past the nothing-installed block', async () => {
		const planner = createPlanner(createStore({committed: {}}), {hasOfflineRenderer: true});

		assert.deepEqual(await planner.evaluateUnreachableLaunch(true, false), {
			kind: ModuleUnreachableLaunchDecision.LAUNCH,
		});
	});

	test('boot attempts at the rollback threshold block with rollback-exhausted', async () => {
		const planner = createPlanner(
			createStore({
				committed: {fluxer_renderer: RENDERER_SHA},
				bootAttempt: MODULE_BOOT_ATTEMPT_ROLLBACK_THRESHOLD,
			}),
		);

		assert.deepEqual(await planner.evaluateUnreachableLaunch(true, false), {
			kind: ModuleUnreachableLaunchDecision.BLOCK,
			reason: ModuleUpdaterBlockReason.ROLLBACK_EXHAUSTED,
		});
	});

	test('one boot attempt below the threshold still launches', async () => {
		const planner = createPlanner(
			createStore({
				committed: {fluxer_renderer: RENDERER_SHA},
				bootAttempt: MODULE_BOOT_ATTEMPT_ROLLBACK_THRESHOLD - 1,
			}),
		);

		assert.deepEqual(await planner.evaluateUnreachableLaunch(true, false), {
			kind: ModuleUnreachableLaunchDecision.LAUNCH,
		});
	});

	test('a pending security update blocks with unreachable-below-floor', async () => {
		const planner = createPlanner(createStore({committed: {fluxer_renderer: RENDERER_SHA}}));

		assert.deepEqual(await planner.evaluateUnreachableLaunch(false, true), {
			kind: ModuleUnreachableLaunchDecision.BLOCK,
			reason: ModuleUpdaterBlockReason.UNREACHABLE_BELOW_FLOOR,
		});
	});

	test('a committed set behind the floor blocks only while startup updates are enforced', async () => {
		const store = createStore({
			committed: {fluxer_renderer: RENDERER_SHA},
			floor: {fluxer_renderer: 'c'.repeat(64)},
		});
		const planner = createPlanner(store);

		assert.deepEqual(await planner.evaluateUnreachableLaunch(true, false), {
			kind: ModuleUnreachableLaunchDecision.BLOCK,
			reason: ModuleUpdaterBlockReason.UNREACHABLE_BELOW_FLOOR,
		});
		assert.deepEqual(await planner.evaluateUnreachableLaunch(false, false), {
			kind: ModuleUnreachableLaunchDecision.LAUNCH,
		});
	});

	test('a committed module that is no longer on disk blocks with unreachable-below-floor', async () => {
		const planner = createPlanner(
			createStore({
				committed: {fluxer_renderer: RENDERER_SHA},
				missing: [`fluxer_renderer:${RENDERER_SHA}`],
			}),
		);

		assert.deepEqual(await planner.evaluateUnreachableLaunch(false, false), {
			kind: ModuleUnreachableLaunchDecision.BLOCK,
			reason: ModuleUpdaterBlockReason.UNREACHABLE_BELOW_FLOOR,
		});
	});

	test('a healthy committed set launches while the feed is unreachable', async () => {
		const planner = createPlanner(
			createStore({
				committed: {fluxer_renderer: RENDERER_SHA},
				floor: {fluxer_renderer: RENDERER_SHA},
			}),
		);

		assert.deepEqual(await planner.evaluateUnreachableLaunch(true, false), {
			kind: ModuleUnreachableLaunchDecision.LAUNCH,
		});
	});
});

describe('ModuleUpdatePlanner with a renderer bundled in the shell', () => {
	const OLDER = '2026.822.9';
	const NEWER = '2026.824.1';

	test('a feed renderer no newer than the bundle is never planned or pending', async () => {
		const planner = bundledPlanner(createStore());
		const feed = manifest({modules: {fluxer_renderer: {sha256: RENDERER_SHA}}});

		assert.equal(planner.bundleCoversModule('fluxer_renderer', feed), true);
		assert.deepEqual(await planner.plan(feed), {items: [], base: {}});
	});

	test('a feed renderer newer than the bundle is downloaded but never required to launch', async () => {
		const planner = bundledPlanner(createStore(), OLDER);
		const feed = manifest({modules: {fluxer_renderer: {sha256: RENDERER_SHA}}});

		assert.equal(planner.bundleCoversModule('fluxer_renderer', feed), false);
		const plan = await planner.plan(feed);
		assert.deepEqual(
			plan.items.map((item) => [item.module, item.requirement]),
			[['fluxer_renderer', 'optional']],
		);
	});

	test('the bundle only ever stands in for the renderer module', async () => {
		const planner = bundledPlanner(createStore());
		const feed = manifest({modules: {fluxer_overlay: {sha256: OVERLAY_SHA}}});

		assert.equal(planner.bundleCoversModule('fluxer_overlay', feed), false);
		assert.equal((await planner.plan(feed)).items.length, 1);
	});

	test('a committed renderer older than or equal to the bundle is stale, a newer one is not', async () => {
		for (const [installedVersion, stale] of [
			[OLDER, true],
			[SHELL_VERSION, true],
			[NEWER, false],
		]) {
			const planner = bundledPlanner(
				createStore({
					committed: {fluxer_renderer: RENDERER_SHA},
					installedManifests: installedRenderer(RENDERER_SHA, installedVersion),
				}),
			);
			assert.equal(await planner.staleRendererModule(), stale ? RENDERER_SHA : null, installedVersion);
		}
	});

	test('a committed renderer whose installation is unreadable is stale', async () => {
		const planner = bundledPlanner(createStore({committed: {fluxer_renderer: RENDERER_SHA}}));

		assert.equal(await planner.staleRendererModule(), RENDERER_SHA);
	});

	test('without a bundle no committed renderer is ever stale', async () => {
		const planner = createPlanner(
			createStore({
				committed: {fluxer_renderer: RENDERER_SHA},
				installedManifests: installedRenderer(RENDERER_SHA, OLDER),
			}),
		);

		assert.equal(await planner.staleRendererModule(), null);
	});

	test('serving drops an older renderer module and keeps every other module', async () => {
		const planner = bundledPlanner(createStore({installedManifests: installedRenderer(RENDERER_SHA, OLDER)}));

		assert.deepEqual(await planner.selectServedModules({fluxer_renderer: RENDERER_SHA, fluxer_overlay: OVERLAY_SHA}), {
			modules: {fluxer_overlay: OVERLAY_SHA},
			renderer: {source: 'bundled', version: SHELL_VERSION, bundledVersion: SHELL_VERSION},
		});
	});

	test('serving keeps a renderer module newer than the bundle', async () => {
		const planner = bundledPlanner(createStore({installedManifests: installedRenderer(RENDERER_SHA, NEWER)}));
		const modules = {fluxer_renderer: RENDERER_SHA};

		assert.deepEqual(await planner.selectServedModules(modules), {
			modules,
			renderer: {source: 'module', version: NEWER, bundledVersion: SHELL_VERSION},
		});
	});

	test('a renderer floor never holds the launch of a shell that bundles its renderer', async () => {
		const planner = bundledPlanner(createStore({floor: {fluxer_renderer: RENDERER_SHA, fluxer_overlay: OVERLAY_SHA}}));

		assert.deepEqual(await planner.modulesBelowFloor(true), ['fluxer_overlay']);
	});

	test('a fresh install that never reached the feed launches the bundled renderer', async () => {
		const planner = bundledPlanner(createStore({lastManifestFetch: null}));

		assert.deepEqual(await planner.evaluateUnreachableLaunch(true, false), {
			kind: ModuleUnreachableLaunchDecision.LAUNCH,
		});
	});

	test('the bundle meets a Linux security minimum it is at least as new as', async () => {
		const planner = bundledPlanner(createStore());

		assert.equal(
			await planner.securityUpdateRequired({
				version: parseModuleVersion(SHELL_VERSION, 'minimum'),
				requiredModules: ['fluxer_renderer'],
			}),
			false,
		);
	});
});
