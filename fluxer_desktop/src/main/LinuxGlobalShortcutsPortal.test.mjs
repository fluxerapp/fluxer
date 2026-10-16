// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {loadTsModule} from './fixtures/TsModuleLoader.mjs';

const {LinuxPortalShortcutsManager, PORTAL_RECOVERY_BACKOFF_MS, PORTAL_TOGGLE_REARM_MS} = loadTsModule(
	'@electron/main/LinuxGlobalShortcutsPortal',
);
const {GLOBAL_SHORTCUT_ACTIONS} = loadTsModule('@electron/common/GlobalShortcutActions');

function listedAll(trigger = 'F13') {
	return GLOBAL_SHORTCUT_ACTIONS.map((id) => ({id, description: id, triggerDescription: trigger}));
}

class FakePortal {
	constructor(harness, onEvent) {
		this.harness = harness;
		this.onEvent = onEvent;
		this.closed = false;
		this.bindCalls = [];
		this.configureCalls = [];
	}

	async open() {
		this.harness.openCalls += 1;
		const next = this.harness.openResults.shift() ?? this.harness.defaultOpen;
		if (next instanceof Error) throw next;
		if (typeof next === 'function') return next(this);
		return next;
	}

	async bind(shortcuts, parentWindow) {
		this.bindCalls.push({shortcuts, parentWindow});
		this.harness.bindCalls.push({shortcuts, parentWindow});
		const next = this.harness.bindResults.shift() ?? {outcome: 'bound', shortcuts: listedAll()};
		if (next instanceof Error) throw next;
		return next;
	}

	async configure(parentWindow) {
		this.configureCalls.push(parentWindow);
		this.harness.configureCalls.push(parentWindow);
		const next = this.harness.configureResults.shift();
		if (next instanceof Error) throw next;
	}

	close() {
		this.closed = true;
	}

	emit(event) {
		this.onEvent(event);
	}
}

function createHarness({
	consent = 'unset',
	desktop = 'gnome',
	plasma5 = false,
	version = 2,
	listed = [],
	appIdSource = 'registered',
} = {}) {
	const harness = {
		consent,
		openCalls: 0,
		openResults: [],
		bindResults: [],
		bindCalls: [],
		configureCalls: [],
		configureResults: [],
		defaultOpen: {version, appIdSource, uniqueName: ':1.42', listed},
		portals: [],
		shortcuts: [],
		statusChanges: 0,
		timers: [],
		clock: 0,
	};
	harness.manager = new LinuxPortalShortcutsManager({
		createPortal: (onEvent) => {
			const portal = new FakePortal(harness, onEvent);
			harness.portals.push(portal);
			return portal;
		},
		desktop,
		plasma5,
		portalAppId: 'app.fluxer.FluxerDesktop',
		getConsent: () => harness.consent,
		setConsent: (next) => {
			harness.consent = next;
		},
		getDefinitions: () => GLOBAL_SHORTCUT_ACTIONS.map((id) => ({id, description: `Label ${id}`})),
		getParentWindow: () => 'x11:2a',
		onShortcut: (action, phase) => harness.shortcuts.push(`${phase}:${action}`),
		onStatusChanged: () => {
			harness.statusChanges += 1;
		},
		schedule: (callback, delayMs) => {
			const timer = {callback, delayMs, cancelled: false, cancel: () => (timer.cancelled = true)};
			harness.timers.push(timer);
			return timer;
		},
		now: () => harness.clock,
		log: () => {},
	});
	harness.latestPortal = () => harness.portals[harness.portals.length - 1];
	harness.runTimer = async () => {
		const timer = harness.timers.find((entry) => !entry.cancelled && !entry.ran);
		assert.ok(timer, 'expected a pending timer');
		timer.ran = true;
		timer.callback();
		await harness.manager.probe();
		await flush();
		return timer.delayMs;
	};
	return harness;
}

async function flush() {
	for (let index = 0; index < 10; index += 1) await Promise.resolve();
}

describe('LinuxPortalShortcutsManager events', () => {
	test('activations are deduplicated and unknown ids are ignored', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		await harness.manager.activate();
		const portal = harness.latestPortal();
		portal.emit({type: 'activated', id: 'voice_push_to_talk'});
		portal.emit({type: 'activated', id: 'voice_push_to_talk'});
		portal.emit({type: 'activated', id: 'not_an_action'});
		portal.emit({type: 'deactivated', id: 'voice_toggle_mute'});
		portal.emit({type: 'deactivated', id: 'voice_push_to_talk'});
		assert.deepEqual(harness.shortcuts, ['press:voice_push_to_talk', 'release:voice_push_to_talk']);
	});

	test('shortcuts-changed with empty triggers keeps the bound state', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		await harness.manager.activate();
		harness.latestPortal().emit({type: 'shortcuts-changed', shortcuts: listedAll('')});
		const status = harness.manager.getStatus();
		assert.equal(status.state, 'bound');
		assert.ok(status.shortcuts.every((entry) => entry.triggerDescription === null));
	});

	test('session loss releases held shortcuts and reopens with backoff while still reporting bound', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		await harness.manager.activate();
		const first = harness.latestPortal();
		first.emit({type: 'activated', id: 'voice_push_to_talk'});
		const changesBefore = harness.statusChanges;
		first.emit({type: 'session-lost', reason: 'portal-restarted'});
		assert.deepEqual(harness.shortcuts, ['press:voice_push_to_talk', 'release:voice_push_to_talk']);
		assert.equal(first.closed, true);
		assert.ok(harness.statusChanges > changesBefore);
		let status = harness.manager.getStatus();
		assert.equal(status.state, 'bound');
		assert.equal(status.recovering, true);
		assert.equal(status.canConfigure, false);
		await harness.manager.configure();
		assert.deepEqual(harness.configureCalls, []);
		first.emit({type: 'activated', id: 'voice_toggle_mute'});
		assert.equal(harness.shortcuts.length, 2);
		harness.openResults.push(new Error('error:bus'), new Error('error:timeout'));
		assert.equal(await harness.runTimer(), PORTAL_RECOVERY_BACKOFF_MS[0]);
		assert.equal(harness.manager.getState(), 'bound');
		assert.equal(harness.manager.getStatus().recovering, true);
		assert.equal(await harness.runTimer(), PORTAL_RECOVERY_BACKOFF_MS[1]);
		assert.equal(await harness.runTimer(), PORTAL_RECOVERY_BACKOFF_MS[2]);
		status = harness.manager.getStatus();
		assert.equal(status.state, 'bound');
		assert.equal(status.recovering, false);
		assert.equal(status.canConfigure, true);
		assert.equal(harness.openCalls, 4);
	});

	test('a reopen that lists new triggers pushes a status even when the state stays bound', async () => {
		const harness = createHarness({consent: 'granted', version: 1, desktop: 'kde', listed: listedAll()});
		await harness.manager.activate();
		harness.latestPortal().emit({type: 'session-lost', reason: 'portal-restarted'});
		harness.defaultOpen = {...harness.defaultOpen, listed: listedAll('F14')};
		const changesBefore = harness.statusChanges;
		await harness.runTimer();
		assert.equal(harness.bindCalls.length, 0);
		assert.ok(harness.statusChanges > changesBefore);
		const status = harness.manager.getStatus();
		assert.equal(status.recovering, false);
		assert.ok(status.shortcuts.every((entry) => entry.triggerDescription === 'F14'));
	});

	test('a recovery that runs out of backoff steps settles into an error and keeps retrying', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		await harness.manager.activate();
		harness.latestPortal().emit({type: 'session-lost', reason: 'portal-restarted'});
		for (const delay of PORTAL_RECOVERY_BACKOFF_MS) {
			assert.equal(harness.manager.getStatus().recovering, true);
			harness.openResults.push(new Error('error:bus'));
			assert.equal(await harness.runTimer(), delay);
		}
		const status = harness.manager.getStatus();
		assert.equal(status.state, 'error');
		assert.equal(status.error, 'portal-unavailable');
		assert.equal(status.recovering, false);
		assert.equal(await harness.runTimer(), 30000);
		assert.equal(harness.manager.getState(), 'bound');
	});

	test('the backoff caps at thirty seconds', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		await harness.manager.activate();
		harness.latestPortal().emit({type: 'session-lost', reason: 'bus-error'});
		const delays = [];
		for (let index = 0; index < 7; index += 1) {
			harness.openResults.push(new Error('down'));
			delays.push(await harness.runTimer());
		}
		assert.deepEqual(delays, [1000, 2000, 5000, 10000, 30000, 30000, 30000]);
	});

	test('a closed session reopens once, then a second close within a minute is an error', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		await harness.manager.activate();
		harness.latestPortal().emit({type: 'session-lost', reason: 'closed'});
		await harness.runTimer();
		assert.equal(harness.manager.getState(), 'bound');
		harness.clock += 1000;
		harness.latestPortal().emit({type: 'session-lost', reason: 'closed'});
		assert.equal(harness.manager.getState(), 'error');
		assert.equal(harness.manager.getStatus().error, 'session-closed');
		assert.equal(harness.timers.filter((timer) => !timer.cancelled && !timer.ran).length, 0);
	});

	test('deactivate releases held shortcuts, cancels recovery and ignores late events', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		await harness.manager.activate();
		const portal = harness.latestPortal();
		portal.emit({type: 'activated', id: 'voice_push_to_mute'});
		harness.manager.deactivate();
		assert.deepEqual(harness.shortcuts, ['press:voice_push_to_mute', 'release:voice_push_to_mute']);
		assert.equal(portal.closed, true);
		assert.equal(harness.manager.getState(), 'unknown');
		portal.emit({type: 'activated', id: 'voice_push_to_mute'});
		assert.equal(harness.shortcuts.length, 2);
	});

	test('a deactivate that lands before the queued startup never opens a session', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		const activation = harness.manager.activate();
		harness.manager.deactivate();
		await activation;
		await flush();
		assert.equal(harness.openCalls, 0);
		assert.equal(harness.bindCalls.length, 0);
		assert.equal(harness.manager.getState(), 'unknown');
	});

	test('check again is only offered where it can do something', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		await harness.manager.activate();
		assert.equal(harness.manager.getStatus().canRecheck, false);
		harness.manager.deactivate();
		assert.equal(harness.manager.getStatus().canRecheck, false);
	});

	test('configure only runs on version 2 portals', async () => {
		const v2 = createHarness({consent: 'granted', version: 2, listed: listedAll()});
		await v2.manager.activate();
		await v2.manager.configure();
		assert.deepEqual(v2.configureCalls, ['x11:2a']);
		const v1 = createHarness({consent: 'granted', version: 1, desktop: 'kde', listed: listedAll()});
		await v1.manager.activate();
		await v1.manager.configure();
		assert.deepEqual(v1.configureCalls, []);
	});
});

describe('LinuxPortalShortcutsManager configure and triggers', () => {
	test('a trigger change on a held shortcut releases it', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		await harness.manager.activate();
		const portal = harness.latestPortal();
		portal.emit({type: 'activated', id: 'voice_push_to_talk'});
		portal.emit({type: 'activated', id: 'voice_toggle_mute'});
		portal.emit({
			type: 'shortcuts-changed',
			shortcuts: [{id: 'voice_push_to_talk', description: null, triggerDescription: 'F14'}],
		});
		assert.deepEqual(harness.shortcuts, [
			'press:voice_push_to_talk',
			'press:voice_toggle_mute',
			'release:voice_push_to_talk',
		]);
		portal.emit({type: 'deactivated', id: 'voice_push_to_talk'});
		assert.equal(harness.shortcuts.length, 3);
	});

	test('a repeated activation of a toggle after a lost deactivation is a new press', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		await harness.manager.activate();
		const portal = harness.latestPortal();
		portal.emit({type: 'activated', id: 'voice_toggle_mute'});
		harness.clock += PORTAL_TOGGLE_REARM_MS + 1;
		portal.emit({type: 'activated', id: 'voice_toggle_mute'});
		portal.emit({type: 'deactivated', id: 'voice_toggle_mute'});
		portal.emit({type: 'deactivated', id: 'voice_toggle_mute'});
		assert.deepEqual(harness.shortcuts, [
			'press:voice_toggle_mute',
			'release:voice_toggle_mute',
			'press:voice_toggle_mute',
			'release:voice_toggle_mute',
		]);
	});

	test('autorepeat of a held toggle never re-arms it', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		await harness.manager.activate();
		const portal = harness.latestPortal();
		for (let index = 0; index < 10; index += 1) {
			portal.emit({type: 'activated', id: 'voice_toggle_deafen'});
			harness.clock += index === 0 ? 500 : 30;
		}
		harness.clock += 2000;
		portal.emit({type: 'deactivated', id: 'voice_toggle_deafen'});
		assert.deepEqual(harness.shortcuts, ['press:voice_toggle_deafen', 'release:voice_toggle_deafen']);
	});

	test('a hold shortcut never re-arms on a repeated activation', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		await harness.manager.activate();
		const portal = harness.latestPortal();
		portal.emit({type: 'activated', id: 'voice_push_to_talk'});
		harness.clock += 10 * PORTAL_TOGGLE_REARM_MS;
		portal.emit({type: 'activated', id: 'voice_push_to_talk'});
		assert.deepEqual(harness.shortcuts, ['press:voice_push_to_talk']);
	});

	test('a press after releaseAll is not mistaken for a repeat', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		await harness.manager.activate();
		const portal = harness.latestPortal();
		portal.emit({type: 'activated', id: 'voice_push_to_talk'});
		harness.manager.releaseAll();
		portal.emit({type: 'activated', id: 'voice_push_to_talk'});
		assert.deepEqual(harness.shortcuts, [
			'press:voice_push_to_talk',
			'release:voice_push_to_talk',
			'press:voice_push_to_talk',
		]);
	});

	test('releaseAll releases held shortcuts and ignores their later deactivation', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		await harness.manager.activate();
		const portal = harness.latestPortal();
		portal.emit({type: 'activated', id: 'voice_push_to_talk'});
		harness.manager.releaseAll();
		portal.emit({type: 'deactivated', id: 'voice_push_to_talk'});
		assert.deepEqual(harness.shortcuts, ['press:voice_push_to_talk', 'release:voice_push_to_talk']);
	});
});
