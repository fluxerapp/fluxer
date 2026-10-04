// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {loadTsModule} from './fixtures/TsModuleLoader.mjs';

const {
	LinuxPortalShortcutsManager,
	PORTAL_RECOVERY_BACKOFF_MS,
	PORTAL_NO_PORTAL_RETRY_WINDOW_MS,
	PORTAL_TOGGLE_REARM_MS,
} = loadTsModule('@electron/main/LinuxGlobalShortcutsPortal');
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

function recordStates(harness) {
	const states = [];
	const original = harness.manager.deps.onStatusChanged;
	harness.manager.deps.onStatusChanged = () => {
		const state = harness.manager.getState();
		if (states[states.length - 1] !== state) states.push(state);
		original();
	};
	return states;
}

async function runAllTimers(harness) {
	const delays = [];
	for (;;) {
		const pending = harness.timers.find((timer) => !timer.cancelled && !timer.ran);
		if (!pending) return delays;
		harness.clock += pending.delayMs;
		delays.push(await harness.runTimer());
	}
}

describe('LinuxPortalShortcutsManager consent', () => {
	test('does not open the portal at activation without consent, and a probe reports not-set-up', async () => {
		const harness = createHarness({consent: 'unset'});
		await harness.manager.activate();
		assert.equal(harness.openCalls, 0);
		assert.equal(harness.manager.getState(), 'unknown');
		await harness.manager.probe();
		assert.equal(harness.openCalls, 1);
		assert.equal(harness.manager.getState(), 'not-set-up');
		assert.equal(harness.bindCalls.length, 0);
	});

	test('a stored decline is reported without prompting', async () => {
		const harness = createHarness({consent: 'declined'});
		await harness.manager.activate();
		await harness.manager.probe();
		assert.equal(harness.manager.getState(), 'declined');
		assert.equal(harness.bindCalls.length, 0);
	});

	test('set up binds the full action set and stores consent', async () => {
		const harness = createHarness({consent: 'unset'});
		await harness.manager.activate();
		await harness.manager.setUp();
		assert.equal(harness.bindCalls.length, 1);
		assert.deepEqual(
			harness.bindCalls[0].shortcuts.map((entry) => entry.id),
			[...GLOBAL_SHORTCUT_ACTIONS],
		);
		assert.equal(harness.bindCalls[0].parentWindow, 'x11:2a');
		assert.equal(harness.consent, 'granted');
		const status = harness.manager.getStatus();
		assert.equal(status.state, 'bound');
		assert.equal(status.canConfigure, true);
		assert.equal(status.shortcuts.length, GLOBAL_SHORTCUT_ACTIONS.length);
		assert.equal(status.shortcuts[0].triggerDescription, 'F13');
	});

	for (const outcome of [{outcome: 'denied'}, {outcome: 'cancelled'}]) {
		test(`a ${outcome.outcome} bind stores the decline`, async () => {
			const harness = createHarness({consent: 'unset'});
			harness.bindResults.push(outcome);
			await harness.manager.activate();
			await harness.manager.setUp();
			assert.equal(harness.manager.getState(), 'declined');
			assert.equal(harness.consent, 'declined');
		});
	}

	for (const desktop of ['other', 'kde', 'hyprland']) {
		test(`a denied bind on ${desktop} means the backend cannot bind, not a user decline`, async () => {
			const harness = createHarness({consent: 'unset', desktop});
			harness.bindResults.push({outcome: 'denied'});
			await harness.manager.activate();
			await harness.manager.setUp();
			assert.equal(harness.manager.getState(), 'unsupported');
			assert.equal(harness.manager.getStatus().canRecheck, true);
			assert.equal(harness.consent, 'unset');
		});
	}

	test('try again after an old stored decline on a non-GNOME desktop settles on unsupported', async () => {
		const harness = createHarness({consent: 'declined', desktop: 'other'});
		harness.bindResults.push({outcome: 'denied'});
		await harness.manager.activate();
		await harness.manager.probe();
		assert.equal(harness.manager.getState(), 'declined');
		await harness.manager.setUp();
		assert.equal(harness.manager.getState(), 'unsupported');
		assert.equal(harness.consent, 'unset');
	});

	test('a cancelled bind on KDE is still a user decline', async () => {
		const harness = createHarness({consent: 'unset', desktop: 'kde'});
		harness.bindResults.push({outcome: 'cancelled'});
		await harness.manager.activate();
		await harness.manager.setUp();
		assert.equal(harness.manager.getState(), 'declined');
		assert.equal(harness.consent, 'declined');
	});

	test('a failed bind is an error that keeps consent untouched', async () => {
		const harness = createHarness({consent: 'unset'});
		harness.bindResults.push({outcome: 'failed', code: 3});
		await harness.manager.activate();
		await harness.manager.setUp();
		assert.equal(harness.manager.getState(), 'error');
		assert.equal(harness.manager.getStatus().error, 'bind-failed');
		assert.equal(harness.consent, 'unset');
	});

	test('a GNOME 48 bind with shortcuts but a garbage response code is bound', async () => {
		const harness = createHarness({consent: 'unset'});
		harness.bindResults.push({outcome: 'bound', shortcuts: listedAll('Ctrl+F13')});
		await harness.manager.activate();
		await harness.manager.setUp();
		assert.equal(harness.manager.getState(), 'bound');
		assert.equal(harness.consent, 'granted');
	});

	test('try again after a decline binds again', async () => {
		const harness = createHarness({consent: 'declined'});
		await harness.manager.activate();
		await harness.manager.probe();
		await harness.manager.setUp();
		assert.equal(harness.bindCalls.length, 1);
		assert.equal(harness.manager.getState(), 'bound');
	});
});

describe('LinuxPortalShortcutsManager startup bind policy', () => {
	test('version 2 binds at startup when consent was granted', async () => {
		const harness = createHarness({consent: 'granted', version: 2, listed: listedAll()});
		await harness.manager.activate();
		assert.equal(harness.bindCalls.length, 1);
		assert.equal(harness.manager.getState(), 'bound');
	});

	test('version 1 skips the bind when the list already covers every id', async () => {
		const harness = createHarness({consent: 'granted', version: 1, desktop: 'kde', listed: listedAll()});
		await harness.manager.activate();
		assert.equal(harness.bindCalls.length, 0);
		assert.equal(harness.manager.getState(), 'bound');
		assert.equal(harness.manager.getStatus().canConfigure, false);
	});

	test('version 1 on KDE with an empty list waits for the user', async () => {
		const harness = createHarness({consent: 'granted', version: 1, desktop: 'kde', listed: []});
		await harness.manager.activate();
		assert.equal(harness.bindCalls.length, 0);
		assert.equal(harness.manager.getState(), 'not-set-up');
		assert.equal(harness.consent, 'granted');
	});

	test('version 1 on KDE with a partial list binds', async () => {
		const harness = createHarness({consent: 'granted', version: 1, desktop: 'kde', listed: listedAll().slice(0, 3)});
		await harness.manager.activate();
		assert.equal(harness.bindCalls.length, 1);
	});

	test('version 1 on GNOME binds with an empty list', async () => {
		const harness = createHarness({consent: 'granted', version: 1, desktop: 'gnome', listed: []});
		await harness.manager.activate();
		assert.equal(harness.bindCalls.length, 1);
		assert.equal(harness.manager.getState(), 'bound');
	});

	test('Plasma 5 is unsupported and never opens the portal', async () => {
		const harness = createHarness({consent: 'granted', plasma5: true});
		await harness.manager.activate();
		await harness.manager.probe();
		await harness.manager.setUp();
		assert.equal(harness.openCalls, 0);
		assert.equal(harness.manager.getState(), 'unsupported');
		assert.equal(harness.manager.getStatus().canRecheck, false);
	});

	test('open failures map to unsupported, identity and error states', async () => {
		const cases = [
			['unsupported:no-portal', 'unsupported', null],
			['unsupported:2', 'unsupported', null],
			['identity:not-found', 'error', 'identity'],
			['error:timeout', 'error', 'timeout'],
			['error:bus', 'error', 'open-failed'],
		];
		for (const [message, state, error] of cases) {
			const harness = createHarness({consent: 'granted'});
			harness.openResults.push(new Error(message));
			await harness.manager.activate();
			assert.equal(harness.manager.getState(), state, message);
			assert.equal(harness.manager.getStatus().error, error, message);
			assert.equal(harness.manager.getStatus().canRecheck, true, message);
		}
	});

	test('an unregistered app id hides the portal app id', async () => {
		const harness = createHarness({consent: 'unset', appIdSource: 'unregistered'});
		await harness.manager.activate();
		await harness.manager.probe();
		assert.equal(harness.manager.getStatus().portalAppId, null);
	});
});

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

function deferred() {
	let resolve;
	let reject;
	const promise = new Promise((onResolve, onReject) => {
		resolve = onResolve;
		reject = onReject;
	});
	return {promise, resolve, reject};
}

describe('LinuxPortalShortcutsManager portal availability', () => {
	test('a missing portal at startup is retried for about thirty seconds without flickering', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		const noPortal = () => new Error('unsupported:no-portal');
		harness.openResults.push(noPortal(), noPortal(), noPortal(), noPortal(), noPortal(), noPortal());
		const states = recordStates(harness);
		await harness.manager.activate();
		assert.equal(harness.manager.getState(), 'unsupported');
		assert.equal(harness.manager.getStatus().recovering, true);
		const delays = await runAllTimers(harness);
		assert.deepEqual(delays, [1000, 2000, 5000, 10000, 12000]);
		assert.equal(
			delays.reduce((sum, delay) => sum + delay, 0),
			PORTAL_NO_PORTAL_RETRY_WINDOW_MS,
		);
		assert.equal(harness.manager.getState(), 'unsupported');
		assert.equal(harness.manager.getStatus().recovering, false);
		assert.deepEqual(states, ['probing', 'unsupported']);
	});

	test('a portal restart during the startup open is retried until it binds', async () => {
		for (const message of ['error:session-lost', 'error:timeout', 'error:bus', 'unsupported:2']) {
			const harness = createHarness({consent: 'granted', listed: listedAll()});
			harness.openResults.push(new Error(message), new Error(message));
			const settledStates = [];
			const original = harness.manager.deps.onStatusChanged;
			harness.manager.deps.onStatusChanged = () => {
				const status = harness.manager.getStatus();
				if (!status.recovering) settledStates.push(status.state);
				original();
			};
			await harness.manager.activate();
			assert.equal(harness.manager.getStatus().recovering, true, message);
			await runAllTimers(harness);
			assert.deepEqual(settledStates, ['probing', 'bound'], message);
			assert.equal(harness.manager.getState(), 'bound', message);
			assert.equal(harness.manager.getStatus().recovering, false, message);
			assert.equal(harness.bindCalls.length, 1, message);
		}
	});

	test('startup errors without consent are shown without retrying', async () => {
		const harness = createHarness({consent: 'unset'});
		harness.openResults.push(new Error('error:bus'));
		await harness.manager.activate();
		await harness.manager.probe();
		assert.equal(harness.manager.getState(), 'error');
		assert.equal(harness.manager.getStatus().error, 'open-failed');
		assert.equal(harness.manager.getStatus().recovering, false);
		assert.equal(harness.timers.length, 0);
	});

	test('startup errors that outlast the retry window settle', async () => {
		const harness = createHarness({consent: 'granted'});
		for (let index = 0; index < 8; index += 1) harness.openResults.push(new Error('error:timeout'));
		await harness.manager.activate();
		await runAllTimers(harness);
		const status = harness.manager.getStatus();
		assert.equal(status.state, 'error');
		assert.equal(status.error, 'timeout');
		assert.equal(status.recovering, false);
	});

	test('check again reopens an unsupported portal', async () => {
		const harness = createHarness({consent: 'unset'});
		harness.openResults.push(new Error('unsupported:interface'));
		await harness.manager.activate();
		await harness.manager.probe();
		assert.equal(harness.manager.getState(), 'unsupported');
		const states = recordStates(harness);
		await harness.manager.recheck();
		assert.deepEqual(states, ['probing', 'not-set-up']);
		assert.equal(harness.bindCalls.length, 0);
	});

	test('check again leaves a working session alone', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		await harness.manager.activate();
		await harness.manager.recheck();
		assert.equal(harness.openCalls, 1);
		assert.equal(harness.latestPortal().closed, false);
	});

	test('a missing portal that comes up during the retry window binds', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		harness.openResults.push(new Error('unsupported:no-portal'));
		await harness.manager.activate();
		await harness.runTimer();
		assert.equal(harness.manager.getState(), 'bound');
		assert.equal(harness.bindCalls.length, 1);
	});

	test('the portal appearing on the bus re-probes after the retries gave up', async () => {
		const harness = createHarness({consent: 'unset'});
		harness.openResults.push(new Error('unsupported:no-portal'));
		await harness.manager.activate();
		await harness.manager.probe();
		const failed = harness.latestPortal();
		assert.equal(failed.closed, false);
		assert.equal(harness.manager.getState(), 'unsupported');
		failed.emit({type: 'portal-available'});
		await harness.manager.probe();
		await flush();
		assert.equal(harness.manager.getState(), 'not-set-up');
		assert.equal(failed.closed, true);
		assert.equal(harness.timers.filter((timer) => !timer.cancelled && !timer.ran).length, 0);
	});

	test('an interface that is genuinely unsupported is final and ignores the bus', async () => {
		const harness = createHarness({consent: 'granted'});
		harness.openResults.push(new Error('unsupported:interface'));
		await harness.manager.activate();
		assert.equal(harness.manager.getState(), 'unsupported');
		assert.equal(harness.timers.length, 0);
		assert.equal(harness.latestPortal().closed, true);
	});

	test('a missing portal during recovery keeps backing off without leaving bound', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		await harness.manager.activate();
		harness.latestPortal().emit({type: 'session-lost', reason: 'portal-restarted'});
		harness.openResults.push(new Error('unsupported:no-portal'));
		await harness.runTimer();
		assert.equal(harness.manager.getState(), 'bound');
		assert.equal(harness.manager.getStatus().recovering, true);
		assert.equal(await harness.runTimer(), PORTAL_RECOVERY_BACKOFF_MS[1]);
		assert.equal(harness.manager.getState(), 'bound');
		assert.equal(harness.manager.getStatus().recovering, false);
	});

	test('a retry from an error state never passes through probing', async () => {
		const harness = createHarness({consent: 'unset'});
		harness.openResults.push(new Error('error:timeout'));
		await harness.manager.activate();
		await harness.manager.probe();
		assert.equal(harness.manager.getState(), 'error');
		const states = recordStates(harness);
		await harness.manager.setUp();
		assert.deepEqual(states, ['error', 'binding', 'bound']);
	});
});

describe('LinuxPortalShortcutsManager bind single flight', () => {
	test('a second set up joins the pending bind', async () => {
		const harness = createHarness({consent: 'unset'});
		const pending = deferred();
		harness.bindResults.push(pending.promise);
		await harness.manager.activate();
		await harness.manager.probe();
		const first = harness.manager.setUp();
		await flush();
		const second = harness.manager.setUp();
		await flush();
		pending.resolve({outcome: 'bound', shortcuts: listedAll()});
		await Promise.all([first, second]);
		assert.equal(harness.bindCalls.length, 1);
		assert.equal(harness.manager.getState(), 'bound');
	});

	test('set up during the startup bind joins it', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		const pending = deferred();
		harness.bindResults.push(pending.promise);
		const startup = harness.manager.activate();
		await flush();
		assert.equal(harness.manager.getState(), 'binding');
		const setUp = harness.manager.setUp();
		pending.resolve({outcome: 'bound', shortcuts: listedAll()});
		await Promise.all([startup, setUp]);
		assert.equal(harness.bindCalls.length, 1);
	});

	test('a session loss during a pending bind waits for the bind to settle', async () => {
		const harness = createHarness({consent: 'unset'});
		const pending = deferred();
		harness.bindResults.push(pending.promise);
		await harness.manager.activate();
		await harness.manager.probe();
		const setUp = harness.manager.setUp();
		await flush();
		const portal = harness.latestPortal();
		portal.emit({type: 'session-lost', reason: 'portal-restarted'});
		assert.equal(portal.closed, false);
		assert.equal(harness.timers.length, 0);
		pending.resolve({outcome: 'bound', shortcuts: listedAll()});
		await setUp;
		assert.equal(portal.closed, true);
		assert.equal(harness.consent, 'granted');
		assert.equal(await harness.runTimer(), PORTAL_RECOVERY_BACKOFF_MS[0]);
		assert.equal(harness.manager.getState(), 'bound');
		assert.equal(harness.bindCalls.length, 2);
	});

	test('a second close during a recovery bind ends in the session-closed error', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		await harness.manager.activate();
		harness.latestPortal().emit({type: 'session-lost', reason: 'closed'});
		const pending = deferred();
		harness.bindResults.push(pending.promise);
		const timer = harness.timers.find((entry) => !entry.cancelled && !entry.ran);
		timer.ran = true;
		timer.callback();
		await flush();
		harness.clock += 1000;
		harness.latestPortal().emit({type: 'session-lost', reason: 'closed'});
		pending.resolve({outcome: 'bound', shortcuts: listedAll()});
		await harness.manager.probe();
		await flush();
		assert.equal(harness.manager.getState(), 'error');
		assert.equal(harness.manager.getStatus().error, 'session-closed');
		assert.equal(harness.timers.filter((entry) => !entry.cancelled && !entry.ran).length, 0);
	});

	test('a rejected bind closes and reopens so the next set up works', async () => {
		const harness = createHarness({consent: 'unset'});
		harness.bindResults.push(new Error('error:no-session'));
		await harness.manager.activate();
		await harness.manager.probe();
		await harness.manager.setUp();
		assert.equal(harness.manager.getState(), 'error');
		assert.equal(harness.manager.getStatus().error, 'bind-failed');
		assert.equal(harness.portals[0].closed, true);
		assert.equal(harness.portals.length, 2);
		await harness.manager.setUp();
		assert.equal(harness.portals.length, 2);
		assert.equal(harness.portals[1].bindCalls.length, 1);
		assert.equal(harness.manager.getState(), 'bound');
	});
});

describe('LinuxPortalShortcutsManager configure and triggers', () => {
	test('a session loss that lands before the open continuation schedules recovery', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		harness.openResults.push((portal) => {
			portal.emit({type: 'session-lost', reason: 'portal-restarted'});
			return harness.defaultOpen;
		});
		await harness.manager.activate();
		assert.equal(harness.portals[0].closed, true);
		assert.equal(harness.bindCalls.length, 0);
		assert.equal(harness.manager.getStatus().recovering, true);
		assert.equal(harness.manager.getStatus().canConfigure, false);
		assert.equal(await harness.runTimer(), PORTAL_RECOVERY_BACKOFF_MS[0]);
		assert.equal(harness.portals.length, 2);
		assert.equal(harness.portals[1].bindCalls.length, 1);
		assert.equal(harness.manager.getState(), 'bound');
		assert.equal(harness.manager.getStatus().recovering, false);
	});

	test('a session loss from a replaced portal during a reopen is ignored', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		await harness.manager.activate();
		const stale = harness.latestPortal();
		harness.latestPortal().emit({type: 'session-lost', reason: 'portal-restarted'});
		harness.openResults.push(() => {
			stale.emit({type: 'session-lost', reason: 'bus-error'});
			return harness.defaultOpen;
		});
		await harness.runTimer();
		assert.equal(harness.manager.getState(), 'bound');
		assert.equal(harness.manager.getStatus().recovering, false);
		assert.equal(harness.latestPortal().closed, false);
	});

	test('a recovery reopen that lists nothing keeps publishing the last triggers until the bind lands', async () => {
		const harness = createHarness({consent: 'granted', listed: []});
		harness.bindResults.push({outcome: 'bound', shortcuts: listedAll('F13')});
		await harness.manager.activate();
		assert.equal(harness.manager.getState(), 'bound');
		const published = [];
		const original = harness.manager.deps.onStatusChanged;
		harness.manager.deps.onStatusChanged = () => {
			published.push(harness.manager.getStatus().shortcuts.map((entry) => entry.triggerDescription));
			original();
		};
		harness.latestPortal().emit({type: 'session-lost', reason: 'portal-restarted'});
		const pending = deferred();
		harness.bindResults.push(pending.promise);
		const timer = harness.timers.find((entry) => !entry.cancelled && !entry.ran);
		timer.ran = true;
		timer.callback();
		await flush();
		assert.equal(harness.manager.getState(), 'binding');
		assert.ok(published.length > 0);
		assert.ok(published.every((triggers) => triggers.every((trigger) => trigger === 'F13')));
		pending.resolve({outcome: 'bound', shortcuts: listedAll('F14')});
		await harness.manager.probe();
		await flush();
		const status = harness.manager.getStatus();
		assert.equal(status.state, 'bound');
		assert.equal(status.recovering, false);
		assert.ok(status.shortcuts.every((entry) => entry.triggerDescription === 'F14'));
	});

	test('a recovery that ends without a bind drops the retained triggers', async () => {
		const harness = createHarness({consent: 'granted', listed: []});
		harness.bindResults.push({outcome: 'bound', shortcuts: listedAll('F13')});
		await harness.manager.activate();
		harness.latestPortal().emit({type: 'session-lost', reason: 'portal-restarted'});
		harness.bindResults.push({outcome: 'cancelled'});
		await harness.runTimer();
		const status = harness.manager.getStatus();
		assert.equal(status.state, 'declined');
		assert.ok(status.shortcuts.every((entry) => entry.triggerDescription === null));
	});

	test('try again that binds with no triggers opens the system settings on version 2', async () => {
		const harness = createHarness({consent: 'declined'});
		harness.bindResults.push({outcome: 'bound', shortcuts: listedAll('')});
		await harness.manager.activate();
		await harness.manager.probe();
		await harness.manager.setUp();
		assert.equal(harness.manager.getState(), 'bound');
		assert.deepEqual(harness.configureCalls, ['x11:2a']);
	});

	test('no configure call on version 1 or Hyprland', async () => {
		for (const options of [
			{version: 1, desktop: 'gnome'},
			{version: 2, desktop: 'hyprland'},
		]) {
			const harness = createHarness({consent: 'declined', ...options});
			harness.bindResults.push({outcome: 'bound', shortcuts: listedAll('')});
			await harness.manager.activate();
			await harness.manager.probe();
			await harness.manager.setUp();
			assert.deepEqual(harness.configureCalls, [], options.desktop);
		}
	});

	test('a transient configure failure keeps the button', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		harness.configureResults.push(new Error('error:timeout'), new Error('error:bus'));
		await harness.manager.activate();
		await harness.manager.configure();
		await harness.manager.configure();
		assert.equal(harness.manager.getStatus().canConfigure, true);
		await harness.manager.configure();
		assert.equal(harness.configureCalls.length, 3);
	});

	test('a configure failure falls back to instructions', async () => {
		const harness = createHarness({consent: 'granted', listed: listedAll()});
		harness.configureResults.push(new Error('unsupported:version'));
		await harness.manager.activate();
		assert.equal(harness.manager.getStatus().canConfigure, true);
		await harness.manager.configure();
		assert.equal(harness.manager.getStatus().canConfigure, false);
		await harness.manager.configure();
		assert.equal(harness.configureCalls.length, 1);
	});

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
