// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {loadTsModule} from './fixtures/TsModuleLoader.mjs';

const legacyHandlers = new Map();
const legacyStubs = {
	electron: {ipcMain: {handle: (channel, handler) => legacyHandlers.set(channel, handler)}},
	'@electron/main/PrivilegedRendererDocuments': {requirePrivilegedRendererDocumentSender: () => {}},
};
const engineModule = loadTsModule('@electron/main/GlobalShortcutsEngine');
const {GlobalShortcutsEngine, sanitizeGlobalShortcutsSyncPayload, portalDefinitionsFromStored, storedActionsFromSync} =
	engineModule;
const {LinuxPortalShortcutsManager} = loadTsModule('@electron/main/LinuxGlobalShortcutsPortal');
const legacy = loadTsModule('@electron/main/LegacyGlobalKeyHookIpc', {stubs: legacyStubs});
const {GLOBAL_SHORTCUT_ACTIONS} = loadTsModule('@electron/common/GlobalShortcutActions');
const {bindingFromCombo, GlobalShortcutMatcher} = loadTsModule('@electron/main/GlobalShortcutMatcher');
const native = loadTsModule('@electron/main/GlobalShortcutsNative', {
	stubs: {
		'node:module': {createRequire: () => () => ({})},
		'@electron/common/Logger': {createChildLogger: () => ({info: () => {}, warn: () => {}})},
		'@electron/main/MacTcc': {getTccStatus: () => 'granted'},
	},
});

function combo(overrides) {
	return {key: '', ctrl: false, alt: false, shift: false, meta: false, ...overrides};
}

function keyEvent(type, code, raw, mods = {}, native = {keycode: raw, keyName: code}) {
	return {
		type,
		code,
		key: null,
		rawKeycode: raw,
		ctrlKey: mods.ctrl === true,
		altKey: mods.alt === true,
		shiftKey: mods.shift === true,
		metaKey: mods.meta === true,
		native,
	};
}

class FakeHook {
	constructor(kind, harness) {
		this.kind = kind;
		this.harness = harness;
		this.stopped = false;
	}

	async start(onEvent) {
		this.harness.started.push(this.kind);
		this.pendingOnEvent = onEvent;
		const result = await (this.harness.startResults.shift() ?? 'ok');
		if (result === 'ok') this.onEvent = onEvent;
		return result;
	}

	stop() {
		this.stopped = true;
		this.harness.stopped.push(this.kind);
	}
}

function createSettings(initial = {}) {
	const state = {directInputEnabled: false, migrated: false, lastActions: [], ...initial};
	return {
		state,
		getDirectInputEnabled: () => state.directInputEnabled,
		setDirectInputEnabled: (enabled) => {
			state.directInputEnabled = enabled;
		},
		isMigrated: () => state.migrated,
		setMigrated: () => {
			state.migrated = true;
		},
		getLastActions: () => state.lastActions,
		setLastActions: (actions) => {
			state.lastActions = actions;
		},
	};
}

function createEngine({
	platform = 'linux',
	session = 'x11',
	inputHookMode = 'auto',
	keyboardReadable = false,
	settings = createSettings(),
	portalConsent = 'unset',
	portalOpen = {version: 2, appIdSource: 'registered', uniqueName: ':1.9', listed: []},
} = {}) {
	const harness = {
		started: [],
		stopped: [],
		startResults: [],
		hooks: [],
		sent: [],
		portals: [],
		timers: [],
		deviceWatches: [],
		consent: portalConsent,
	};
	const schedule = (callback, delayMs) => {
		const timer = {callback, delayMs, cancelled: false, cancel: () => (timer.cancelled = true)};
		harness.timers.push(timer);
		return timer;
	};
	harness.keyboardReadable = keyboardReadable;
	let engine = null;
	const linux = platform === 'linux' ? {session, sandboxed: false, desktop: 'gnome', inputHookMode} : null;
	const portal =
		linux !== null && session === 'wayland'
			? new LinuxPortalShortcutsManager({
					createPortal: (onEvent) => {
						const fake = {
							onEvent,
							closed: false,
							open: async () => portalOpen,
							bind: async () => ({
								outcome: 'bound',
								shortcuts: GLOBAL_SHORTCUT_ACTIONS.map((id) => ({id, description: id, triggerDescription: 'F13'})),
							}),
							configure: async () => {},
							close: () => {
								fake.closed = true;
							},
						};
						harness.portals.push(fake);
						return fake;
					},
					desktop: 'gnome',
					plasma5: false,
					portalAppId: 'app.fluxer.FluxerDesktop',
					getConsent: () => harness.consent,
					setConsent: (consent) => {
						harness.consent = consent;
					},
					getDefinitions: () => [],
					getParentWindow: () => '',
					onShortcut: (action, phase) => engine?.handlePortalShortcut(action, phase),
					onStatusChanged: () => engine?.notifyStatus(),
					schedule: () => ({cancel: () => {}}),
					now: () => 0,
					log: () => {},
				})
			: null;
	engine = new GlobalShortcutsEngine({
		platform,
		linux,
		settings,
		portal,
		createHookBackend: (kind) => {
			const hook = new FakeHook(kind, harness);
			harness.hooks.push(hook);
			return hook;
		},
		isDirectInputAvailable: () => harness.keyboardReadable,
		watchInputDevices: (onChange) => {
			const watch = {onChange, closed: false, close: () => (watch.closed = true)};
			harness.deviceWatches.push(watch);
			return watch;
		},
		schedule,
		log: () => {},
	});
	harness.engine = engine;
	harness.settings = settings;
	harness.portal = portal;
	harness.attach = (id) => engine.attachClient(id, (channel, payload) => harness.sent.push({id, channel, payload}));
	harness.events = (id) =>
		harness.sent
			.filter((entry) => entry.id === id && entry.channel === 'global-shortcut-event')
			.map((entry) => `${entry.payload.phase}:${entry.payload.sourceId}`);
	harness.captured = (id) =>
		harness.sent
			.filter((entry) => entry.id === id && entry.channel === 'global-shortcuts:capture')
			.map((entry) => entry.payload);
	harness.activeHook = () => harness.hooks.find((hook) => hook.onEvent && !hook.stopped);
	return harness;
}

function syncPayload(bindings, actions = []) {
	return sanitizeGlobalShortcutsSyncPayload({bindings, actions});
}

async function settle() {
	for (let index = 0; index < 20; index += 1) await Promise.resolve();
}

describe('sanitizeGlobalShortcutsSyncPayload', () => {
	test('accepts well formed payloads and filters unknown actions', () => {
		const payload = sanitizeGlobalShortcutsSyncPayload({
			bindings: [
				{
					sourceId: 'default:voice_toggle_mute',
					action: 'voice_toggle_mute',
					combo: combo({key: 'm', ctrl: true}),
				},
			],
			actions: [
				{
					action: 'voice_toggle_mute',
					description: 'Toggle mute',
					preferredCombo: combo({key: 'm', ctrl: true}),
				},
				{action: 'not_global', description: 'Nope', preferredCombo: null},
				{action: 'voice_push_to_talk', description: '   ', preferredCombo: null},
			],
		});
		assert.equal(payload.bindings.length, 1);
		assert.deepEqual(
			payload.actions.map((entry) => entry.action),
			['voice_toggle_mute'],
		);
		assert.deepEqual(storedActionsFromSync(payload.actions), [
			{action: 'voice_toggle_mute', description: 'Toggle mute', preferredTrigger: 'CTRL+m'},
		]);
	});

	test('ignores fields it does not know so older renderers still sync', () => {
		const payload = sanitizeGlobalShortcutsSyncPayload({
			bindings: [
				{sourceId: 'custom:a', action: 'voice_push_to_talk', hold: true, extra: 1, combo: combo({key: 'F13'})},
			],
			actions: [{action: 'voice_push_to_talk', description: 'Push to talk', hold: true, preferredCombo: null}],
			version: 3,
		});
		assert.deepEqual(payload, {
			bindings: [{sourceId: 'custom:a', action: 'voice_push_to_talk', combo: combo({key: 'F13'})}],
			actions: [{action: 'voice_push_to_talk', description: 'Push to talk', preferredCombo: null}],
		});
	});

	test('rejects malformed bindings', () => {
		assert.equal(sanitizeGlobalShortcutsSyncPayload(null), null);
		assert.equal(sanitizeGlobalShortcutsSyncPayload({bindings: {}, actions: []}), null);
		assert.equal(
			sanitizeGlobalShortcutsSyncPayload({bindings: [{sourceId: 'a', action: 'b', combo: {key: 1}}], actions: []}),
			null,
		);
		assert.equal(
			sanitizeGlobalShortcutsSyncPayload({
				bindings: [{sourceId: 'a', action: 'b', combo: combo({key: '', mouseButton: 99})}],
				actions: [],
			}),
			null,
		);
	});

	test('portal definitions always cover all ten actions with non-empty descriptions', () => {
		const definitions = portalDefinitionsFromStored([
			{action: 'voice_push_to_talk', description: 'Sprechen', preferredTrigger: 'F13'},
		]);
		assert.deepEqual(
			definitions.map((entry) => entry.id),
			[...GLOBAL_SHORTCUT_ACTIONS],
		);
		assert.ok(definitions.every((entry) => entry.description.length > 0));
		assert.deepEqual(
			definitions.find((entry) => entry.id === 'voice_push_to_talk'),
			{id: 'voice_push_to_talk', description: 'Sprechen', preferredTrigger: 'F13'},
		);
		assert.deepEqual(
			definitions.find((entry) => entry.id === 'voice_toggle_mute'),
			{
				id: 'voice_toggle_mute',
				description: 'Toggle mute',
			},
		);
	});
});

describe('portalDefinitionsFromStored safety', () => {
	test('never sends blank descriptions, duplicates or malformed triggers', () => {
		const definitions = portalDefinitionsFromStored([
			{action: 'voice_toggle_mute', description: '   ', preferredTrigger: 'CTRL+,'},
			{action: 'voice_toggle_mute', description: 'Again', preferredTrigger: 'F1'},
			{action: 'voice_disconnect', description: 'Leave', preferredTrigger: 'NUM+KP_1'},
			{action: 'voice_push_to_talk', description: 'Talk', preferredTrigger: 'SHIFT+F13'},
		]);
		const ids = definitions.map((entry) => entry.id);
		assert.equal(new Set(ids).size, GLOBAL_SHORTCUT_ACTIONS.length);
		assert.ok(definitions.every((entry) => entry.description.trim().length > 0));
		const byId = Object.fromEntries(definitions.map((entry) => [entry.id, entry]));
		assert.equal(byId.voice_disconnect.preferredTrigger, undefined);
		assert.equal(byId.voice_push_to_talk.preferredTrigger, 'SHIFT+F13');
		assert.equal(byId.voice_toggle_mute.description.trim().length > 0, true);
	});
});

describe('GlobalShortcutsEngine hook backends', () => {
	test('starts the hook only once a client has bindings and emits press/release to that client', async () => {
		const harness = createEngine({session: 'x11', settings: createSettings({migrated: true})});
		harness.attach(1);
		harness.attach(2);
		await harness.engine.refreshHooks();
		assert.deepEqual(harness.started, []);
		harness.engine.sync(
			1,
			syncPayload([{sourceId: 'custom:a', action: 'voice_push_to_talk', combo: combo({key: 'F13', code: 'F13'})}]),
		);
		await settle();
		assert.deepEqual(harness.started, ['x11']);
		assert.equal(harness.engine.getStatus().hooksActive, true);
		const hook = harness.activeHook();
		hook.onEvent(keyEvent('keydown', 'F13', 191));
		hook.onEvent(keyEvent('keyup', 'F13', 191));
		assert.deepEqual(harness.events(1), ['press:custom:a', 'release:custom:a']);
		assert.deepEqual(harness.events(2), []);
	});

	test('pause suppresses presses only', async () => {
		const harness = createEngine({session: 'x11', settings: createSettings({migrated: true})});
		harness.attach(1);
		harness.engine.sync(
			1,
			syncPayload([{sourceId: 'custom:a', action: 'voice_push_to_talk', combo: combo({key: 'F13', code: 'F13'})}]),
		);
		await settle();
		const hook = harness.activeHook();
		hook.onEvent(keyEvent('keydown', 'F13', 191));
		harness.engine.setPaused(1, true);
		hook.onEvent(keyEvent('keyup', 'F13', 191));
		hook.onEvent(keyEvent('keydown', 'F13', 191));
		assert.deepEqual(harness.events(1), ['press:custom:a', 'release:custom:a']);
	});

	test('removing a binding mid-hold releases it and stops the idle hook', async () => {
		const harness = createEngine({session: 'x11', settings: createSettings({migrated: true})});
		harness.attach(1);
		harness.engine.sync(
			1,
			syncPayload([{sourceId: 'custom:a', action: 'voice_push_to_talk', combo: combo({key: 'F13', code: 'F13'})}]),
		);
		await settle();
		harness.activeHook().onEvent(keyEvent('keydown', 'F13', 191));
		harness.engine.sync(1, syncPayload([]));
		await settle();
		assert.deepEqual(harness.events(1), ['press:custom:a', 'release:custom:a']);
		assert.deepEqual(harness.stopped, ['x11']);
		assert.equal(harness.engine.getStatus().hooksActive, false);
	});

	test('a failed start is reported and retried on the next sync', async () => {
		const harness = createEngine({platform: 'macos'});
		harness.startResults.push('permission');
		harness.attach(1);
		const payload = syncPayload([
			{sourceId: 'custom:a', action: 'voice_toggle_mute', combo: combo({key: 'm', meta: true})},
		]);
		harness.engine.sync(1, payload);
		await settle();
		assert.equal(harness.engine.getStatus().hookError, 'permission');
		assert.equal(harness.engine.getStatus().backend, 'macos');
		harness.engine.sync(1, payload);
		await settle();
		assert.deepEqual(harness.started, ['macos', 'macos']);
		assert.equal(harness.engine.getStatus().hookError, null);
		assert.equal(harness.engine.getStatus().hooksActive, true);
	});

	test('a backend dropped after its start resolves releases the presses it already sent', async () => {
		const harness = createEngine({session: 'x11', keyboardReadable: true, settings: createSettings({migrated: true})});
		let resolveStart;
		harness.startResults.push(new Promise((resolve) => (resolveStart = resolve)));
		harness.attach(1);
		harness.engine.sync(
			1,
			syncPayload([{sourceId: 'custom:a', action: 'voice_push_to_talk', combo: combo({key: 'F9', code: 'F9'})}]),
		);
		await settle();
		assert.deepEqual(harness.started, ['x11']);
		harness.hooks[0].pendingOnEvent(keyEvent('keydown', 'F9', 75));
		assert.deepEqual(harness.events(1), ['press:custom:a']);
		harness.engine.setDirectInputEnabled(true);
		resolveStart('ok');
		await settle();
		assert.deepEqual(harness.stopped, ['x11']);
		assert.deepEqual(harness.started, ['x11', 'evdev']);
		assert.deepEqual(harness.events(1), ['press:custom:a', 'release:custom:a']);
	});

	test('detaching a client stops the hook without touching other clients', async () => {
		const harness = createEngine({platform: 'windows'});
		harness.attach(1);
		harness.attach(2);
		const binding = [{sourceId: 'custom:a', action: 'voice_push_to_talk', combo: combo({key: 'F13', code: 'F13'})}];
		harness.engine.sync(1, syncPayload(binding));
		harness.engine.sync(2, syncPayload(binding));
		await settle();
		harness.engine.detachClient(1);
		await settle();
		assert.deepEqual(harness.stopped, []);
		harness.activeHook().onEvent(keyEvent('keydown', 'F13', 0x64));
		assert.deepEqual(harness.events(2), ['press:custom:a']);
		assert.deepEqual(harness.events(1), []);
	});
});

describe('GlobalShortcutsEngine Linux backend selection', () => {
	test('Wayland uses the portal and never starts a hook', async () => {
		const harness = createEngine({session: 'wayland', settings: createSettings({migrated: true})});
		harness.engine.start();
		harness.attach(1);
		harness.engine.sync(
			1,
			syncPayload([{sourceId: 'custom:a', action: 'voice_push_to_talk', combo: combo({key: 'F13', code: 'F13'})}]),
		);
		await settle();
		const status = harness.engine.getStatus();
		assert.equal(status.backend, 'portal');
		assert.equal(status.linux.portal.state, 'not-set-up');
		assert.equal(status.supportsMouseButtons, false);
		assert.deepEqual(harness.started, []);
	});

	test('portal shortcuts reach clients as portal sources', async () => {
		const harness = createEngine({
			session: 'wayland',
			portalConsent: 'granted',
			settings: createSettings({migrated: true}),
		});
		harness.engine.start();
		harness.attach(1);
		await settle();
		harness.portals[0].onEvent({type: 'activated', id: 'voice_push_to_talk'});
		harness.engine.setPaused(1, true);
		harness.portals[0].onEvent({type: 'deactivated', id: 'voice_push_to_talk'});
		harness.portals[0].onEvent({type: 'activated', id: 'voice_toggle_mute'});
		assert.deepEqual(harness.events(1), ['press:portal:voice_push_to_talk', 'release:portal:voice_push_to_talk']);
	});

	test('the first sync migrates readable keyboards to direct input and closes the portal', async () => {
		const harness = createEngine({session: 'wayland', keyboardReadable: true, portalConsent: 'granted'});
		harness.engine.start();
		harness.attach(1);
		await settle();
		assert.equal(harness.portals.length, 1);
		harness.engine.sync(
			1,
			syncPayload([{sourceId: 'custom:a', action: 'voice_push_to_talk', combo: combo({key: '', mouseButton: 3})}]),
		);
		await settle();
		assert.equal(harness.settings.state.migrated, true);
		assert.equal(harness.settings.state.directInputEnabled, true);
		assert.equal(harness.portals[0].closed, true);
		assert.equal(harness.engine.getStatus().backend, 'evdev');
		assert.deepEqual(harness.started, ['evdev']);
	});

	test('migration only marks itself done when no keyboard is readable', async () => {
		const harness = createEngine({session: 'wayland', keyboardReadable: false});
		harness.engine.start();
		harness.attach(1);
		harness.engine.sync(
			1,
			syncPayload([{sourceId: 'custom:a', action: 'voice_push_to_talk', combo: combo({key: 'F13', code: 'F13'})}]),
		);
		await settle();
		assert.equal(harness.settings.state.migrated, true);
		assert.equal(harness.settings.state.directInputEnabled, false);
		assert.equal(harness.engine.getStatus().backend, 'portal');
	});

	test('direct input is ignored when no keyboard is readable', () => {
		const harness = createEngine({
			session: 'x11',
			settings: createSettings({directInputEnabled: true, migrated: true}),
		});
		assert.equal(harness.engine.getStatus().backend, 'x11');
		assert.deepEqual(harness.engine.getStatus().linux.directInput, {available: false, enabled: true, locked: false});
	});

	test('direct input is probed again when a keyboard shows up after launch', async () => {
		const harness = createEngine({
			session: 'wayland',
			settings: createSettings({directInputEnabled: true, migrated: true}),
		});
		harness.attach(1);
		await settle();
		assert.equal(harness.engine.getStatus().backend, 'portal');
		harness.keyboardReadable = true;
		harness.engine.sync(
			1,
			syncPayload([{sourceId: 'custom:a', action: 'voice_push_to_talk', combo: combo({key: 'F13', code: 'F13'})}]),
		);
		await settle();
		assert.equal(harness.engine.getStatus().backend, 'evdev');
		assert.equal(harness.engine.getStatus().linux.directInput.available, true);
		assert.deepEqual(harness.started, ['evdev']);
		assert.equal(harness.portals[0]?.closed ?? true, true);
	});

	test('a keyboard plugged in after launch is picked up from the input device watch', async () => {
		const harness = createEngine({
			session: 'wayland',
			settings: createSettings({directInputEnabled: true, migrated: true}),
		});
		harness.engine.start();
		harness.attach(1);
		harness.engine.sync(
			1,
			syncPayload([{sourceId: 'custom:a', action: 'voice_push_to_talk', combo: combo({key: 'F13', code: 'F13'})}]),
		);
		await settle();
		assert.equal(harness.engine.getStatus().backend, 'portal');
		assert.equal(harness.deviceWatches.length, 1);
		const watch = harness.deviceWatches[0];
		watch.onChange();
		watch.onChange();
		assert.deepEqual(
			harness.timers.map((timer) => [timer.delayMs, timer.cancelled]),
			[
				[500, true],
				[500, false],
			],
		);
		harness.timers[1].callback();
		await settle();
		assert.equal(watch.closed, false);
		assert.deepEqual(harness.started, []);
		harness.keyboardReadable = true;
		watch.onChange();
		harness.timers[2].callback();
		await settle();
		assert.equal(watch.closed, true);
		assert.equal(harness.engine.getStatus().backend, 'evdev');
		assert.deepEqual(harness.started, ['evdev']);
		assert.equal(harness.deviceWatches.length, 1);
	});

	test('the input device watch only runs while direct input is on and unavailable', () => {
		const harness = createEngine({session: 'wayland', settings: createSettings({migrated: true})});
		harness.engine.start();
		assert.equal(harness.deviceWatches.length, 0);
		harness.engine.setDirectInputEnabled(true);
		assert.equal(harness.deviceWatches.length, 1);
		harness.deviceWatches[0].onChange();
		harness.engine.setDirectInputEnabled(false);
		assert.equal(harness.deviceWatches[0].closed, true);
		assert.equal(harness.timers[0].cancelled, true);
		harness.engine.setDirectInputEnabled(true);
		assert.equal(harness.deviceWatches.length, 2);
		harness.engine.dispose();
		assert.equal(harness.deviceWatches[1].closed, true);
	});

	test('a hook start attempt notices a keyboard that went away', async () => {
		const harness = createEngine({
			session: 'x11',
			keyboardReadable: true,
			settings: createSettings({directInputEnabled: true, migrated: true}),
		});
		assert.equal(harness.engine.getStatus().backend, 'evdev');
		harness.keyboardReadable = false;
		harness.attach(1);
		harness.engine.sync(
			1,
			syncPayload([{sourceId: 'custom:a', action: 'voice_push_to_talk', combo: combo({key: 'F13', code: 'F13'})}]),
		);
		await settle();
		assert.equal(harness.engine.getStatus().backend, 'x11');
		assert.deepEqual(harness.started, ['x11']);
	});

	test('a launch flag locks the direct input setting', () => {
		const harness = createEngine({session: 'wayland', inputHookMode: 'evdev', keyboardReadable: true});
		assert.equal(harness.engine.getStatus().linux.directInput.locked, true);
	});

	test('disabling direct input hands Wayland back to the portal', async () => {
		const harness = createEngine({
			session: 'wayland',
			keyboardReadable: true,
			settings: createSettings({directInputEnabled: true, migrated: true}),
		});
		harness.engine.start();
		harness.attach(1);
		assert.equal(harness.engine.getStatus().backend, 'evdev');
		harness.engine.setDirectInputEnabled(false);
		await settle();
		assert.equal(harness.engine.getStatus().backend, 'portal');
		assert.equal(harness.engine.getStatus().linux.portal.state, 'not-set-up');
	});

	test('launch modes off and native refuse backends', () => {
		assert.equal(createEngine({session: 'x11', inputHookMode: 'off'}).engine.getStatus().backend, 'none');
		assert.equal(createEngine({session: 'wayland', inputHookMode: 'native'}).engine.getStatus().backend, 'none');
		assert.equal(createEngine({session: 'x11', inputHookMode: 'native'}).engine.getStatus().backend, 'x11');
		assert.equal(
			createEngine({session: 'wayland', inputHookMode: 'evdev', keyboardReadable: true}).engine.getStatus().backend,
			'evdev',
		);
	});

	test('an unsupported portal reports backend none', async () => {
		const harness = createEngine({session: 'wayland', settings: createSettings({migrated: true})});
		harness.portal.deps.createPortal = () => ({
			open: async () => {
				throw new Error('unsupported:2');
			},
			bind: async () => ({outcome: 'denied'}),
			configure: async () => {},
			close: () => {},
		});
		harness.engine.start();
		harness.attach(1);
		await settle();
		assert.equal(harness.engine.getStatus().backend, 'none');
		assert.equal(harness.engine.getStatus().linux.portal.state, 'unsupported');
	});
});

describe('GlobalShortcutsEngine release on suspend', () => {
	test('releaseAllPressed releases hook holds without stopping the hook', async () => {
		const harness = createEngine({session: 'x11', settings: createSettings({migrated: true})});
		harness.attach(1);
		harness.engine.sync(
			1,
			syncPayload([{sourceId: 'custom:a', action: 'voice_push_to_talk', combo: combo({key: 'F13', code: 'F13'})}]),
		);
		await settle();
		const hook = harness.activeHook();
		hook.onEvent(keyEvent('keydown', 'F13', 191));
		harness.engine.releaseAllPressed();
		hook.onEvent(keyEvent('keyup', 'F13', 191));
		assert.deepEqual(harness.events(1), ['press:custom:a', 'release:custom:a']);
		assert.deepEqual(harness.stopped, []);
		hook.onEvent(keyEvent('keydown', 'F13', 191));
		assert.deepEqual(harness.events(1), ['press:custom:a', 'release:custom:a', 'press:custom:a']);
	});

	test('releaseAllPressed releases portal holds', async () => {
		const harness = createEngine({
			session: 'wayland',
			portalConsent: 'granted',
			settings: createSettings({migrated: true}),
		});
		harness.engine.start();
		harness.attach(1);
		await settle();
		harness.portals[0].onEvent({type: 'activated', id: 'voice_push_to_talk'});
		harness.engine.releaseAllPressed();
		harness.portals[0].onEvent({type: 'deactivated', id: 'voice_push_to_talk'});
		assert.deepEqual(harness.events(1), ['press:portal:voice_push_to_talk', 'release:portal:voice_push_to_talk']);
	});

	test('a release-all on lock lets the next portal press through', async () => {
		const harness = createEngine({
			session: 'wayland',
			portalConsent: 'granted',
			settings: createSettings({migrated: true}),
		});
		harness.attach(1);
		await settle();
		const portal = harness.portals[0];
		portal.onEvent({type: 'activated', id: 'voice_push_to_talk'});
		harness.engine.releaseAllPressed();
		portal.onEvent({type: 'activated', id: 'voice_push_to_talk'});
		assert.deepEqual(harness.events(1), [
			'press:portal:voice_push_to_talk',
			'release:portal:voice_push_to_talk',
			'press:portal:voice_push_to_talk',
		]);
	});

	test('a portal release is only sent for a press the client received', async () => {
		const harness = createEngine({
			session: 'wayland',
			portalConsent: 'granted',
			settings: createSettings({migrated: true}),
		});
		harness.attach(1);
		await settle();
		harness.engine.setPaused(1, true);
		harness.portals[0].onEvent({type: 'activated', id: 'voice_push_to_talk'});
		harness.engine.setPaused(1, false);
		harness.portals[0].onEvent({type: 'deactivated', id: 'voice_push_to_talk'});
		harness.engine.releaseAllPressed();
		assert.deepEqual(harness.events(1), []);
	});
});

describe('GlobalShortcutsEngine portal lifecycle', () => {
	test('the portal stays closed until a client of the new API calls in', async () => {
		const harness = createEngine({
			session: 'wayland',
			portalConsent: 'granted',
			settings: createSettings({migrated: true}),
		});
		harness.engine.start();
		await settle();
		assert.equal(harness.portals.length, 0);
		assert.equal(harness.portal.getState(), 'unknown');
		harness.attach(1);
		await settle();
		assert.equal(harness.portals.length, 1);
		assert.equal(harness.portal.getState(), 'bound');
	});

	test('check again re-probes an unsupported portal', async () => {
		const harness = createEngine({session: 'wayland', settings: createSettings({migrated: true})});
		const create = harness.portal.deps.createPortal;
		harness.portal.deps.createPortal = () => {
			harness.portal.deps.createPortal = create;
			return {
				open: async () => {
					throw new Error('unsupported:interface');
				},
				bind: async () => ({outcome: 'denied'}),
				configure: async () => {},
				close: () => {},
			};
		};
		harness.attach(1);
		await settle();
		assert.equal(harness.engine.getStatus().backend, 'none');
		await harness.engine.recheck();
		assert.equal(harness.engine.getStatus().backend, 'portal');
		assert.equal(harness.engine.getStatus().linux.portal.state, 'not-set-up');
	});

	test('set up on an unsupported portal checks again instead of binding', async () => {
		const harness = createEngine({session: 'wayland', settings: createSettings({migrated: true})});
		const create = harness.portal.deps.createPortal;
		harness.portal.deps.createPortal = () => {
			harness.portal.deps.createPortal = create;
			return {
				open: async () => {
					throw new Error('unsupported:interface');
				},
				bind: async () => ({outcome: 'denied'}),
				configure: async () => {},
				close: () => {},
			};
		};
		harness.attach(1);
		await settle();
		await harness.engine.setUp();
		assert.equal(harness.engine.getStatus().linux.portal.state, 'not-set-up');
		assert.equal(harness.consent, 'unset');
	});
});

describe('GlobalShortcutsEngine capture', () => {
	test('captures raw input on a hook backend for the requesting client only', async () => {
		const harness = createEngine({session: 'x11', settings: createSettings({migrated: true})});
		harness.attach(1);
		harness.attach(2);
		const captureId = await harness.engine.startCapture(1);
		assert.equal(typeof captureId, 'number');
		assert.deepEqual(harness.started, ['x11']);
		const hook = harness.activeHook();
		hook.onEvent({...keyEvent('keydown', 'PrintScreen', 107, {ctrl: true}), key: 'Print'});
		hook.onEvent({
			type: 'mousedown',
			button: 4,
			ctrlKey: false,
			altKey: false,
			shiftKey: false,
			metaKey: false,
			native: null,
		});
		assert.deepEqual(harness.captured(1), [
			{
				type: 'keydown',
				code: 'PrintScreen',
				key: 'Print',
				button: null,
				ctrl: true,
				alt: false,
				shift: false,
				meta: false,
			},
			{type: 'mousedown', code: null, key: null, button: 4, ctrl: false, alt: false, shift: false, meta: false},
		]);
		assert.deepEqual(harness.captured(2), []);
		harness.engine.stopCapture(2, captureId);
		hook.onEvent(keyEvent('keyup', 'PrintScreen', 107));
		assert.equal(harness.captured(1).length, 3);
		harness.engine.stopCapture(1, captureId);
		await settle();
		assert.deepEqual(harness.stopped, ['x11']);
		assert.equal(harness.timers[0].cancelled, true);
	});

	test('capture stops on its own after a minute and when the client goes away', async () => {
		const harness = createEngine({platform: 'windows'});
		harness.attach(1);
		assert.notEqual(await harness.engine.startCapture(1), null);
		assert.equal(harness.timers[0].delayMs, engineModule.GLOBAL_SHORTCUT_CAPTURE_TIMEOUT_MS);
		harness.timers[0].callback();
		await settle();
		assert.deepEqual(harness.stopped, ['windows']);
		assert.notEqual(await harness.engine.startCapture(1), null);
		harness.engine.detachClient(1);
		await settle();
		assert.deepEqual(harness.stopped, ['windows', 'windows']);
	});

	test('a hook blocked on permission starts on retry without a new sync', async () => {
		const harness = createEngine({platform: 'macos'});
		harness.startResults.push('permission');
		harness.attach(1);
		harness.engine.sync(
			1,
			syncPayload([{sourceId: 'custom:ptt', action: 'voice_push_to_talk', combo: combo({key: 'F13', code: 'F13'})}]),
		);
		await settle();
		assert.equal(harness.engine.getStatus().hookError, 'permission');
		assert.equal(harness.engine.hooksActive(), false);
		await harness.engine.retryBlockedHooks();
		assert.deepEqual(harness.started, ['macos', 'macos']);
		assert.equal(harness.engine.hooksActive(), true);
		const statuses = harness.sent.filter((entry) => entry.channel === 'global-shortcuts:status');
		assert.deepEqual(
			statuses.map((entry) => [entry.payload.hooksActive, entry.payload.hookError]),
			[
				[false, 'permission'],
				[true, null],
			],
		);
		harness.activeHook().onEvent(keyEvent('keydown', 'F13', 105));
		assert.deepEqual(harness.events(1), ['press:custom:ptt']);
	});

	test('a retry leaves a running hook and an idle engine alone', async () => {
		const running = createEngine({platform: 'macos'});
		running.attach(1);
		running.engine.sync(
			1,
			syncPayload([{sourceId: 'custom:a', action: 'voice_toggle_mute', combo: combo({key: 'm', meta: true})}]),
		);
		await settle();
		await running.engine.retryBlockedHooks();
		assert.deepEqual(running.started, ['macos']);
		assert.deepEqual(running.stopped, []);
		const idle = createEngine({platform: 'macos'});
		idle.attach(1);
		await idle.engine.retryBlockedHooks();
		assert.deepEqual(idle.started, []);
	});

	test('a retry while the permission is still missing stays blocked', async () => {
		const harness = createEngine({platform: 'macos'});
		harness.startResults.push('permission', 'permission');
		harness.attach(1);
		harness.engine.sync(
			1,
			syncPayload([{sourceId: 'custom:a', action: 'voice_toggle_mute', combo: combo({key: 'm', meta: true})}]),
		);
		await settle();
		await harness.engine.retryBlockedHooks();
		assert.equal(harness.engine.getStatus().hookError, 'permission');
		assert.equal(harness.engine.hooksActive(), false);
	});

	test('capture is refused on the portal and when the hook cannot start', async () => {
		const wayland = createEngine({session: 'wayland', settings: createSettings({migrated: true})});
		wayland.attach(1);
		assert.equal(await wayland.engine.startCapture(1), null);
		assert.deepEqual(wayland.started, []);
		const mac = createEngine({platform: 'macos'});
		mac.startResults.push('permission');
		mac.attach(1);
		assert.equal(await mac.engine.startCapture(1), null);
		assert.deepEqual(mac.started, ['macos']);
		assert.equal(await mac.engine.startCapture(2), null);
	});

	test('a late stop from a replaced capture leaves the newer capture running', async () => {
		const harness = createEngine({platform: 'windows'});
		harness.attach(1);
		const first = await harness.engine.startCapture(1);
		const secondStart = harness.engine.startCapture(1);
		harness.engine.stopCapture(1, first);
		const second = await secondStart;
		assert.notEqual(second, null);
		assert.notEqual(second, first);
		await settle();
		assert.deepEqual(harness.stopped, []);
		harness.activeHook().onEvent(keyEvent('keydown', 'F13', 100));
		assert.equal(harness.captured(1).length, 1);
		harness.timers[0].callback();
		await settle();
		assert.deepEqual(harness.stopped, []);
		harness.engine.stopCapture(1, second);
		await settle();
		assert.deepEqual(harness.stopped, ['windows']);
	});
});

describe('GlobalShortcutsNative translation', () => {
	function nativeKey(type, overrides) {
		return {type, ctrlKey: false, altKey: false, shiftKey: false, metaKey: false, ...overrides};
	}

	test('an injected Windows key without a scan code matches by virtual key', () => {
		const matcher = new GlobalShortcutMatcher();
		matcher.setBindings([bindingFromCombo('custom:a', 'voice_push_to_talk', combo({key: 'F13', code: 'F13'}))]);
		const identity = {keycode: 0x7c, keyName: 'F13', scanCode: 0, extended: false};
		const down = native.translateNativeHookEvent('windows', nativeKey('keydown', identity));
		const up = native.translateNativeHookEvent('windows', nativeKey('keyup', identity));
		assert.equal(down.code, 'F13');
		assert.equal(matcher.handleKey(down, true).length, 1);
		assert.equal(matcher.handleKey(up, true).length, 1);
	});

	test('a Windows modifier keydown sets its own flag so a Ctrl then Shift capture keeps both', () => {
		const ctrl = {keycode: 0xa2, keyName: 'ControlLeft', scanCode: 0x1d, extended: false};
		const shift = {keycode: 0xa0, keyName: 'ShiftLeft', scanCode: 0x2a, extended: false};
		const ctrlDown = native.translateNativeHookEvent('windows', nativeKey('keydown', ctrl));
		assert.equal(ctrlDown.ctrlKey, true);
		const shiftDown = native.translateNativeHookEvent('windows', nativeKey('keydown', {...shift, ctrlKey: true}));
		assert.deepEqual(
			[shiftDown.ctrlKey, shiftDown.shiftKey, shiftDown.altKey, shiftDown.metaKey],
			[true, true, false, false],
		);
		const shiftUp = native.translateNativeHookEvent('windows', nativeKey('keyup', {...shift, ctrlKey: true}));
		assert.equal(shiftUp.shiftKey, false);
		const x11 = native.translateNativeHookEvent(
			'x11',
			nativeKey('keydown', {keycode: 0xffe1, keyName: 'ShiftLeft', x11Keycode: 50}),
		);
		assert.equal(x11.shiftKey, false);
	});

	test('a Windows Ctrl then Shift capture saves a binding that ignores Ctrl alone', async () => {
		const harness = createEngine({platform: 'windows'});
		harness.attach(1);
		assert.notEqual(await harness.engine.startCapture(1), null);
		const hook = harness.activeHook();
		const ctrl = {keycode: 0xa2, keyName: 'ControlLeft', scanCode: 0x1d, extended: false};
		const shift = {keycode: 0xa0, keyName: 'ShiftLeft', scanCode: 0x2a, extended: false};
		hook.onEvent(native.translateNativeHookEvent('windows', nativeKey('keydown', ctrl)));
		hook.onEvent(native.translateNativeHookEvent('windows', nativeKey('keydown', {...shift, ctrlKey: true})));
		const last = harness.captured(1).at(-1);
		assert.deepEqual([last.code, last.ctrl, last.shift], ['ShiftLeft', true, true]);
		const binding = bindingFromCombo(
			'custom:a',
			'voice_push_to_talk',
			combo({key: 'Shift', code: last.code, ctrl: last.ctrl, shift: last.shift, modifierOnly: true}),
		);
		const matcher = new GlobalShortcutMatcher();
		matcher.setBindings([binding]);
		const ctrlOnly = native.translateNativeHookEvent('windows', nativeKey('keydown', ctrl));
		assert.deepEqual(matcher.handleKey(ctrlOnly, true), []);
	});

	test('evdev keys without a DOM code are dropped and every mapped key passes through', () => {
		const unmapped = native.translateNativeHookEvent('evdev', nativeKey('keydown', {keycode: 0x2ff, keyName: ''}));
		assert.equal(unmapped, null);
		const pause = native.translateNativeHookEvent('evdev', nativeKey('keydown', {keycode: 119, keyName: ''}));
		assert.equal(pause.code, 'Pause');
		const calculator = native.translateNativeHookEvent('evdev', nativeKey('keydown', {keycode: 140, keyName: ''}));
		assert.equal(calculator.code, 'LaunchApp2');
	});

	test('an old renderer media select registration matches the media select key', () => {
		const binding = legacy.legacyRegistrationToBinding({id: 'key:media', keyName: 'LaunchMediaPlayer'});
		assert.deepEqual(binding.trigger, {kind: 'key', layoutKey: 'LaunchMediaPlayer', code: 'MediaSelect'});
		const matcher = new GlobalShortcutMatcher();
		matcher.setBindings([binding]);
		const identity = {keycode: 0xb5, keyName: 'LaunchMediaPlayer', scanCode: 0x6d, extended: true};
		assert.equal(
			matcher.handleKey(native.translateNativeHookEvent('windows', nativeKey('keydown', identity)), true).length,
			1,
		);
	});

	test('an X11 keysym without a name falls back to the physical code', () => {
		const matcher = new GlobalShortcutMatcher();
		matcher.setBindings([
			bindingFromCombo('custom:a', 'voice_toggle_mute', combo({key: 'q', code: 'KeyQ', ctrl: true})),
		]);
		const event = native.translateNativeHookEvent(
			'x11',
			nativeKey('keydown', {keycode: 0x6ca, keyName: '', x11Keycode: 24, ctrlKey: true}),
		);
		assert.equal(event.key, null);
		assert.equal(event.code, 'KeyQ');
		assert.equal(matcher.handleKey(event, true).length, 1);
	});

	test('X11 Right Alt survives events whose Mod1 flag is clear on AltGr layouts', () => {
		const matcher = new GlobalShortcutMatcher();
		matcher.setBindings([
			bindingFromCombo(
				'custom:a',
				'voice_push_to_talk',
				combo({key: 'AltGraph', code: 'AltRight', modifierOnly: true}),
			),
		]);
		const down = native.translateNativeHookEvent(
			'x11',
			nativeKey('keydown', {keycode: 0xfe03, keyName: '', x11Keycode: 108}),
		);
		const click = native.translateNativeHookEvent('x11', {
			type: 'mousedown',
			button: 0,
			ctrlKey: false,
			altKey: false,
			shiftKey: false,
			metaKey: false,
		});
		const other = native.translateNativeHookEvent(
			'x11',
			nativeKey('keydown', {keycode: 0x71, keyName: 'Q', x11Keycode: 24}),
		);
		const up = native.translateNativeHookEvent(
			'x11',
			nativeKey('keyup', {keycode: 0xfe03, keyName: '', x11Keycode: 108}),
		);
		assert.deepEqual(matcher.handleKey(down, true), [
			{sourceId: 'custom:a', action: 'voice_push_to_talk', phase: 'press'},
		]);
		assert.deepEqual(matcher.handleMouse(click, true), []);
		assert.deepEqual(matcher.handleKey(other, true), []);
		assert.deepEqual(matcher.handleKey(up, true), [
			{sourceId: 'custom:a', action: 'voice_push_to_talk', phase: 'release'},
		]);
	});

	test('a left Alt hold still recovers from a lost key-up on X11', () => {
		const matcher = new GlobalShortcutMatcher();
		matcher.setBindings([
			bindingFromCombo('custom:a', 'voice_push_to_talk', combo({key: 'Alt', code: 'AltLeft', modifierOnly: true})),
		]);
		const down = native.translateNativeHookEvent(
			'x11',
			nativeKey('keydown', {keycode: 0xffe9, keyName: 'AltLeft', x11Keycode: 64}),
		);
		const other = native.translateNativeHookEvent(
			'x11',
			nativeKey('keydown', {keycode: 0x71, keyName: 'Q', x11Keycode: 24}),
		);
		assert.equal(matcher.handleKey(down, true).length, 1);
		assert.deepEqual(matcher.handleKey(other, true), [
			{sourceId: 'custom:a', action: 'voice_push_to_talk', phase: 'release'},
		]);
	});

	test('Windows unicode packets report the typed character, and a space packet is the Space key', () => {
		const matcher = new GlobalShortcutMatcher();
		matcher.setBindings([
			bindingFromCombo('custom:f13', 'voice_push_to_talk', combo({key: 'F13', code: 'F13'})),
			bindingFromCombo('custom:space', 'voice_toggle_mute', combo({key: ' ', code: 'Space'})),
			bindingFromCombo(
				'custom:shift',
				'voice_push_to_mute',
				combo({key: 'Shift', code: 'ShiftRight', modifierOnly: true}),
			),
		]);
		for (const character of ['d', '9', '6', '8']) {
			const identity = {keycode: 0xe7, keyName: 'Key231', scanCode: character.charCodeAt(0), extended: false};
			const down = native.translateNativeHookEvent('windows', nativeKey('keydown', identity));
			assert.equal(down.code, null);
			assert.equal(down.key, character.toUpperCase());
			assert.deepEqual(matcher.handleKey(down, true), []);
			assert.deepEqual(
				matcher.handleKey(native.translateNativeHookEvent('windows', nativeKey('keyup', identity)), true),
				[],
			);
		}
		const letter = new GlobalShortcutMatcher();
		letter.setBindings([bindingFromCombo('custom:d', 'voice_toggle_mute', combo({key: 'd', code: 'KeyD'}))]);
		const packetD = {keycode: 0xe7, keyName: 'Key231', scanCode: 0x64, extended: false};
		const packetE = {keycode: 0xe7, keyName: 'Key231', scanCode: 0x65, extended: false};
		assert.equal(
			letter.handleKey(native.translateNativeHookEvent('windows', nativeKey('keydown', packetD)), true).length,
			1,
		);
		assert.deepEqual(
			letter.handleKey(native.translateNativeHookEvent('windows', nativeKey('keyup', packetE)), true),
			[],
		);
		assert.equal(
			letter.handleKey(native.translateNativeHookEvent('windows', nativeKey('keyup', packetD)), true).length,
			1,
		);
		const packetSpace = {keycode: 0xe7, keyName: 'Space', scanCode: 0x20, extended: false};
		const spaceDown = native.translateNativeHookEvent('windows', nativeKey('keydown', packetSpace));
		assert.equal(spaceDown.code, 'Space');
		assert.deepEqual(
			matcher.handleKey(spaceDown, true).map((transition) => `${transition.phase}:${transition.sourceId}`),
			['press:custom:space'],
		);
		assert.deepEqual(
			matcher
				.handleKey(native.translateNativeHookEvent('windows', nativeKey('keyup', packetSpace)), true)
				.map((transition) => `${transition.phase}:${transition.sourceId}`),
			['release:custom:space'],
		);
	});

	test('injected Windows keys without a scan code resolve every virtual key the old names covered', () => {
		const expected = [
			[0x0c, 'Numpad5'],
			[0x15, 'KanaMode'],
			[0x1c, 'Convert'],
			[0x1d, 'NonConvert'],
			[0x5f, 'Sleep'],
			[0x6c, 'NumpadComma'],
			[0x92, 'NumpadEqual'],
			[0xa6, 'BrowserBack'],
			[0xa7, 'BrowserForward'],
			[0xa8, 'BrowserRefresh'],
			[0xa9, 'BrowserStop'],
			[0xaa, 'BrowserSearch'],
			[0xab, 'BrowserFavorites'],
			[0xac, 'BrowserHome'],
			[0xb4, 'LaunchMail'],
			[0xb5, 'MediaSelect'],
			[0xb6, 'LaunchApp1'],
			[0xb7, 'LaunchApp2'],
			[0x87, 'F24'],
			[0xde, 'Quote'],
			[0xe2, 'IntlBackslash'],
		];
		for (const [vk, code] of expected) {
			const identity = {keycode: vk, keyName: 'x', scanCode: 0, extended: false};
			assert.equal(native.translateNativeHookEvent('windows', nativeKey('keydown', identity)).code, code, code);
		}
	});
});

function loadNativeWith(modules, getTccStatus = () => 'granted') {
	return loadTsModule('@electron/main/GlobalShortcutsNative', {
		stubs: {
			'node:module': {createRequire: () => (name) => modules[name] ?? {loadError: null}},
			'@electron/common/Logger': {createChildLogger: () => ({info: () => {}, warn: () => {}})},
			'@electron/main/MacTcc': {getTccStatus},
		},
	});
}

function withPlatform(platform, run) {
	const original = Object.getOwnPropertyDescriptor(process, 'platform');
	Object.defineProperty(process, 'platform', {...original, value: platform});
	return Promise.resolve()
		.then(run)
		.finally(() => Object.defineProperty(process, 'platform', original));
}

describe('macOS Input Monitoring', () => {
	function macHookModule({preflight = false} = {}) {
		const instances = [];
		class InputHook {
			constructor() {
				this.stopped = false;
				instances.push(this);
			}
			start() {
				return true;
			}
			stop() {
				this.stopped = true;
			}
		}
		return {instances, module: {InputHook, hasAccessibilityPermission: () => preflight, loadError: null}};
	}

	test('the live status wins over a stale hook preflight', () =>
		withPlatform('darwin', () => {
			const hook = macHookModule({preflight: false});
			const loaded = loadNativeWith({'@fluxer/macos-input-hook': hook.module}, () => 'granted');
			assert.equal(loaded.hasMacInputMonitoringAccess(), true);
		}));

	test('the hook preflight is the fallback when the live status is unavailable', () =>
		withPlatform('darwin', () => {
			const granted = macHookModule({preflight: true});
			const unknown = () => 'not-determined';
			assert.equal(
				loadNativeWith({'@fluxer/macos-input-hook': granted.module}, unknown).hasMacInputMonitoringAccess(),
				true,
			);
			const throwing = () => {
				throw new Error('addon crashed');
			};
			assert.equal(
				loadNativeWith({'@fluxer/macos-input-hook': granted.module}, throwing).hasMacInputMonitoringAccess(),
				true,
			);
			const denied = macHookModule({preflight: false});
			assert.equal(
				loadNativeWith({'@fluxer/macos-input-hook': denied.module}, () => 'denied').hasMacInputMonitoringAccess(),
				false,
			);
		}));

	test('a live denial wins over a stale hook preflight', () =>
		withPlatform('darwin', () => {
			const stale = macHookModule({preflight: true});
			assert.equal(
				loadNativeWith({'@fluxer/macos-input-hook': stale.module}, () => 'denied').hasMacInputMonitoringAccess(),
				false,
			);
		}));

	test('a hook refused for permission starts once the permission is granted', () =>
		withPlatform('darwin', async () => {
			let status = 'denied';
			const hook = macHookModule({preflight: false});
			const loaded = loadNativeWith({'@fluxer/macos-input-hook': hook.module}, () => status);
			assert.equal(await loaded.createNativeHookBackend('macos').start(() => {}), 'permission');
			assert.equal(hook.instances.length, 0);
			status = 'granted';
			assert.equal(await loaded.createNativeHookBackend('macos').start(() => {}), 'ok');
			assert.equal(hook.instances.length, 1);
		}));

	test('other platforms never ask', () =>
		withPlatform('linux', () => {
			const loaded = loadNativeWith({}, () => {
				throw new Error('must not be read');
			});
			assert.equal(loaded.hasMacInputMonitoringAccess(), true);
		}));
});

describe('GlobalShortcutsNative backends', () => {
	function x11Module(start) {
		const instances = [];
		class InputHook {
			constructor(callback) {
				this.callback = callback;
				this.stopped = false;
				instances.push(this);
			}
			start() {
				return start(this);
			}
			stop() {
				this.stopped = true;
			}
		}
		return {instances, module: {InputHook, loadError: null}};
	}

	test('an asynchronous native start is awaited before the hook counts as running', async () => {
		let resolveStart;
		const x11 = x11Module(() => new Promise((resolve) => (resolveStart = resolve)));
		const loaded = loadNativeWith({'@fluxer/linux-input-hook': x11.module});
		const backend = loaded.createNativeHookBackend('x11');
		let settled = null;
		const pending = backend.start(() => {}).then((result) => (settled = result));
		await settle();
		assert.equal(settled, null);
		resolveStart();
		await pending;
		assert.equal(settled, 'ok');
		backend.stop();
		assert.equal(x11.instances[0].stopped, true);
	});

	test('a rejected native start stops the instance and surfaces the error', async () => {
		const x11 = x11Module(() => Promise.reject(new Error('XRecord extension not available')));
		const backend = loadNativeWith({'@fluxer/linux-input-hook': x11.module}).createNativeHookBackend('x11');
		await assert.rejects(
			backend.start(() => {}),
			/XRecord extension not available/,
		);
		assert.equal(x11.instances[0].stopped, true);
	});

	test('a synchronous native start still works', async () => {
		const x11 = x11Module(() => undefined);
		const backend = loadNativeWith({'@fluxer/linux-input-hook': x11.module}).createNativeHookBackend('x11');
		assert.equal(await backend.start(() => {}), 'ok');
		const failing = x11Module(() => false);
		const refused = loadNativeWith({'@fluxer/linux-input-hook': failing.module}).createNativeHookBackend('x11');
		assert.equal(await refused.start(() => {}), 'start-failed');
		assert.equal(failing.instances[0].stopped, true);
	});

	test('the session monitor never loads the portals module when portals are turned off', () => {
		const original = Object.getOwnPropertyDescriptor(process, 'platform');
		const previousEnv = process.env.FLUXER_DISABLE_LINUX_PORTALS;
		let constructed = 0;
		const portals = {
			GlobalShortcutsPortal: null,
			SessionStateMonitor: class {
				constructor() {
					constructed += 1;
				}
				close() {}
			},
			loadError: null,
		};
		Object.defineProperty(process, 'platform', {...original, value: 'linux'});
		try {
			process.env.FLUXER_DISABLE_LINUX_PORTALS = '1';
			const off = loadNativeWith({'@fluxer/linux-portals': portals});
			assert.equal(
				off.createLinuxSessionStateMonitor(() => {}),
				null,
			);
			assert.equal(constructed, 0);
			delete process.env.FLUXER_DISABLE_LINUX_PORTALS;
			const on = loadNativeWith({'@fluxer/linux-portals': portals});
			assert.notEqual(
				on.createLinuxSessionStateMonitor(() => {}),
				null,
			);
			assert.equal(constructed, 1);
		} finally {
			Object.defineProperty(process, 'platform', original);
			if (previousEnv === undefined) delete process.env.FLUXER_DISABLE_LINUX_PORTALS;
			else process.env.FLUXER_DISABLE_LINUX_PORTALS = previousEnv;
		}
	});
});

describe('LegacyGlobalKeyHookAdapter', () => {
	function legacyHarness({session = 'x11', portalConsent = 'unset'} = {}) {
		const harness = createEngine({session, portalConsent, settings: createSettings({migrated: true})});
		const adapter = new legacy.LegacyGlobalKeyHookAdapter();
		harness.engine.addConsumer(adapter);
		harness.legacySent = [];
		adapter.attach(7, (channel, payload) => harness.legacySent.push({channel, payload}));
		harness.adapter = adapter;
		harness.triggered = () =>
			harness.legacySent
				.filter((entry) => entry.channel === 'global-keybind-triggered')
				.map((entry) => `${entry.payload.type}:${entry.payload.id}`);
		return harness;
	}

	test('legacy registrations translate to bindings', () => {
		assert.deepEqual(
			legacy.legacyRegistrationToBinding({
				id: 'key:voice_toggle_mute:m',
				keycode: 50,
				keyName: 'M',
				physicalKeyName: 'M',
				ctrl: true,
			}),
			{
				sourceId: 'key:voice_toggle_mute:m',
				action: 'key:voice_toggle_mute:m',
				trigger: {kind: 'key', layoutKey: 'M', code: 'KeyM'},
				modifiers: {ctrl: true, alt: false, shift: false, meta: false},
			},
		);
		assert.deepEqual(legacy.legacyRegistrationToBinding({id: 'mouse:voice_push_to_talk:3', mouseButton: 3}).trigger, {
			kind: 'mouse',
			button: 3,
		});
		assert.equal(legacy.legacyRegistrationToBinding({id: 'key:x', keycode: 50}), null);
	});

	test('started legacy senders get raw events and triggered ids without the keysym collision', async () => {
		const harness = legacyHarness();
		harness.adapter.register(
			7,
			legacy.legacyRegistrationToBinding({id: 'key:voice_toggle_mute:m', keyName: 'M', ctrl: true}),
		);
		harness.adapter.acquire(7);
		await harness.engine.refreshHooks();
		const hook = harness.activeHook();
		hook.onEvent({...keyEvent('keydown', 'Digit2', 11, {ctrl: true}, {keycode: 0x32, keyName: '2'}), key: '2'});
		hook.onEvent({...keyEvent('keydown', 'KeyM', 58, {ctrl: true}, {keycode: 0x6d, keyName: 'M'}), key: 'M'});
		hook.onEvent({...keyEvent('keyup', 'KeyM', 58, {}, {keycode: 0x6d, keyName: 'M'}), key: 'M'});
		assert.deepEqual(harness.triggered(), ['keydown:key:voice_toggle_mute:m', 'keyup:key:voice_toggle_mute:m']);
		const raw = harness.legacySent.filter((entry) => entry.channel === 'global-key-event');
		assert.equal(raw.length, 3);
		assert.deepEqual(raw[1].payload, {
			type: 'keydown',
			keycode: 0x6d,
			keyName: 'M',
			ctrlKey: true,
			altKey: false,
			shiftKey: false,
			metaKey: false,
			backend: 'native',
		});
		harness.adapter.release(7);
		await harness.engine.refreshHooks();
		assert.deepEqual(harness.stopped, ['x11']);
	});

	function registerLegacy(harness) {
		legacyHandlers.clear();
		const sender = {
			id: 9,
			isDestroyed: () => false,
			send: (channel, payload) => harness.legacySent.push({channel, payload}),
		};
		legacy.registerLegacyGlobalKeyHookHandlers(harness.engine, harness.adapter, {
			platform: 'linux',
			sandboxed: false,
			username: 'u',
			hasInputMonitoringAccess: () => true,
			watchSender: () => {},
		});
		return (channel, ...args) => legacyHandlers.get(channel)({sender}, ...args);
	}

	test('an old renderer alone never opens the portal', async () => {
		const harness = legacyHarness({session: 'wayland', portalConsent: 'granted'});
		harness.engine.start();
		await settle();
		const invoke = registerLegacy(harness);
		assert.equal(await invoke('global-key-hook-start'), false);
		await invoke('global-key-hook-register', {id: 'key:voice_toggle_mute::F9:F9', keyName: 'F9'});
		assert.equal(harness.portals.length, 0);
		assert.deepEqual(harness.started, []);
	});

	test('the portal closes when the last new renderer goes away and reopens for the next one', async () => {
		const harness = legacyHarness({session: 'wayland', portalConsent: 'granted'});
		harness.attach(1);
		await settle();
		assert.equal(harness.engine.getStatus().linux.portal.state, 'bound');
		harness.portals[0].onEvent({type: 'activated', id: 'voice_push_to_talk'});
		harness.engine.detachClient(1);
		await settle();
		assert.equal(harness.portals[0].closed, true);
		assert.equal(harness.engine.getStatus().linux.portal.state, 'unknown');
		const invoke = registerLegacy(harness);
		assert.equal(await invoke('global-key-hook-start'), false);
		assert.equal(harness.portals.length, 1);
		harness.attach(2);
		await settle();
		assert.equal(harness.portals.length, 2);
		assert.equal(harness.engine.getStatus().linux.portal.state, 'bound');
	});

	test('an old renderer alone on Wayland is told it has access so it never offers a grant', async () => {
		const harness = legacyHarness({session: 'wayland', portalConsent: 'granted'});
		const invoke = registerLegacy(harness);
		assert.equal(await invoke('global-key-hook-start'), false);
		assert.equal((await invoke('linux-evdev-status')).hasAccess, true);
		assert.equal(harness.engine.getStatus().linux.portal.state, 'unknown');
	});

	test('a portal bound for a new renderer keeps an old renderer quiet without routing to it', async () => {
		const harness = legacyHarness({session: 'wayland', portalConsent: 'granted'});
		harness.attach(1);
		await settle();
		assert.equal(harness.engine.getStatus().linux.portal.state, 'bound');
		const invoke = registerLegacy(harness);
		assert.equal(await invoke('global-key-hook-start'), false);
		assert.equal((await invoke('linux-evdev-status')).hasAccess, true);
		harness.portals[0].onEvent({type: 'activated', id: 'voice_toggle_mute'});
		assert.deepEqual(harness.triggered(), []);
	});

	function unmigratedLegacyHarness(options) {
		const harness = createEngine(options);
		harness.legacySent = [];
		harness.adapter = new legacy.LegacyGlobalKeyHookAdapter();
		harness.engine.addConsumer(harness.adapter);
		return harness;
	}

	test('a legacy start alone migrates readable keyboards to direct input on Wayland', async () => {
		const harness = unmigratedLegacyHarness({session: 'wayland', keyboardReadable: true, portalConsent: 'granted'});
		harness.engine.start();
		await settle();
		const invoke = registerLegacy(harness);
		assert.equal(await invoke('global-key-hook-start'), true);
		assert.deepEqual(harness.started, ['evdev']);
		assert.equal(harness.settings.state.migrated, true);
		assert.equal(harness.settings.state.directInputEnabled, true);
		assert.equal(harness.engine.getStatus().backend, 'evdev');
		await invoke('global-key-hook-register', {id: 'key:voice_toggle_mute::F9:F9', keyName: 'F9'});
		assert.deepEqual(harness.started, ['evdev']);
		assert.equal((await invoke('linux-evdev-status')).hasAccess, true);
	});

	test('legacy calls leave the migration pending while no keyboard is readable', async () => {
		const harness = unmigratedLegacyHarness({session: 'wayland', keyboardReadable: false});
		harness.engine.start();
		await settle();
		const invoke = registerLegacy(harness);
		assert.equal(await invoke('global-key-hook-start'), false);
		await invoke('global-key-hook-register', {id: 'key:voice_toggle_mute::F9:F9', keyName: 'F9'});
		assert.equal(harness.settings.state.migrated, false);
		assert.equal(harness.settings.state.directInputEnabled, false);
		harness.keyboardReadable = true;
		assert.equal(await invoke('global-key-hook-start'), true);
		assert.equal(harness.settings.state.migrated, true);
		assert.deepEqual(harness.started, ['evdev']);
	});

	test('a migrated user who turned direct input off stays on the portal for legacy starts', async () => {
		const harness = unmigratedLegacyHarness({
			session: 'wayland',
			keyboardReadable: true,
			settings: createSettings({migrated: true}),
		});
		const invoke = registerLegacy(harness);
		assert.equal(await invoke('global-key-hook-start'), false);
		assert.equal(harness.settings.state.directInputEnabled, false);
		assert.deepEqual(harness.started, []);
	});

	test('a legacy registration also runs the migration', async () => {
		const harness = unmigratedLegacyHarness({session: 'x11', keyboardReadable: true});
		const invoke = registerLegacy(harness);
		await invoke('global-key-hook-register', {id: 'mouse:voice_push_to_talk::4', mouseButton: 4});
		assert.equal(harness.settings.state.migrated, true);
		assert.equal(harness.settings.state.directInputEnabled, true);
		assert.equal(harness.engine.getStatus().backend, 'evdev');
	});

	test('legacy evdev status never offers a prompt and grant is unsupported', () => {
		assert.deepEqual(legacy.buildLegacyLinuxEvdevStatus({platform: 'linux', sandboxed: true, username: 'u'}), {
			supported: true,
			hasAccess: true,
			canPrompt: false,
			sandboxed: true,
			username: 'u',
			totalEventDevices: 0,
			readableEventDevices: 0,
			inInputGroup: false,
		});
		assert.equal(
			legacy.buildLegacyLinuxEvdevStatus({platform: 'darwin', sandboxed: false, username: null}).hasAccess,
			true,
		);
		assert.deepEqual(
			{...legacy.LEGACY_LINUX_EVDEV_GRANT_RESULT},
			{success: false, needsRelogin: false, error: 'unsupported'},
		);
	});
});
