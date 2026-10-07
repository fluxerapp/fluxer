// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

const {
	armOpenUrlForwarding,
	armSecondInstanceForwarding,
	getCommittedModuleFiles,
	observeMainWindow,
	onMainWindowCreated,
	onMainWindowReady,
	setCommittedModuleFiles,
	setOpenUrlSink,
	setSecondInstanceSink,
	signalMainWindowCreated,
	signalMainWindowReady,
} = await import('@electron/main/ModuleBootHandoff');

describe('ModuleBootHandoff', () => {
	test('the committed module file index starts empty', () => {
		assert.equal(getCommittedModuleFiles().size, 0);
	});

	test('the committed module file index is copied, not aliased', () => {
		const source = new Map([['assets/app.js', '/store/fluxer_renderer/abc/assets/app.js']]);
		setCommittedModuleFiles('/store', source);
		source.set('assets/late.js', '/store/fluxer_renderer/abc/assets/late.js');
		const published = getCommittedModuleFiles();
		assert.equal(published.size, 1);
		assert.equal(published.get('assets/app.js'), '/store/fluxer_renderer/abc/assets/app.js');
	});

	test('a listener registered before the signal receives the window', () => {
		const received = [];
		onMainWindowCreated((window) => {
			received.push(window);
		});
		onMainWindowCreated(() => {
			throw new Error('this listener throws');
		});
		onMainWindowCreated((window) => {
			received.push(window);
		});
		assert.deepEqual(received, []);
		const window = {id: 1, isDestroyed: () => false};
		signalMainWindowCreated(window);
		assert.deepEqual(received, [window, window]);
	});

	test('a listener registered after the signal fires immediately', () => {
		const received = [];
		onMainWindowCreated((window) => {
			received.push(window);
		});
		assert.equal(received.length, 1);
		assert.equal(received[0].id, 1);
	});

	test('a second signal is ignored', () => {
		const received = [];
		signalMainWindowCreated({id: 2, isDestroyed: () => false});
		onMainWindowCreated((window) => {
			received.push(window);
		});
		assert.equal(received[0].id, 1);
	});

	test('creating the window does not by itself mean the window became ready', () => {
		let ready = 0;
		onMainWindowReady(() => {
			ready += 1;
		});
		assert.equal(ready, 0);
	});

	test('the ready signal fires every waiting listener exactly once and survives a thrower', () => {
		let ready = 0;
		onMainWindowReady(() => {
			throw new Error('this listener throws');
		});
		onMainWindowReady(() => {
			ready += 1;
		});
		signalMainWindowReady();
		signalMainWindowReady();
		assert.equal(ready, 1);
	});

	test('a ready listener registered after the signal fires immediately', () => {
		let ready = 0;
		onMainWindowReady(() => {
			ready += 1;
		});
		assert.equal(ready, 1);
	});

	test('an observer sees the live window on every signal, not only the one the handoff latched', () => {
		const received = [];
		const stop = observeMainWindow((window) => {
			received.push(window);
		});
		assert.deepEqual(
			received.map((window) => window.id),
			[2],
			'the observer starts from the window that is live right now, not from the latched boot window',
		);
		const reopened = {id: 3, isDestroyed: () => false};
		signalMainWindowCreated(reopened);
		assert.deepEqual(
			received.map((window) => window.id),
			[2, 3],
			'closing and reopening the main window must hand the module poll the new window, or the poll never ticks again',
		);
		const latched = [];
		onMainWindowCreated((window) => {
			latched.push(window);
		});
		assert.deepEqual(
			latched.map((window) => window.id),
			[1],
			'the one shot boot handoff keeps its latch',
		);
		stop();
		signalMainWindowCreated({id: 4, isDestroyed: () => false});
		assert.deepEqual(
			received.map((window) => window.id),
			[2, 3],
		);
	});
});

const appRegistrations = [];
installElectronStub({
	app: {
		on: (eventName, listener) => {
			appRegistrations.push({eventName, listener, once: false});
		},
		once: (eventName, listener) => {
			appRegistrations.push({eventName, listener, once: true});
		},
	},
});

describe('ModuleBootHandoff second instance forwarding', () => {
	const registrations = appRegistrations;
	const forward = (argv) => {
		registrations[0].listener({}, argv, '/');
	};

	test('arming is idempotent, so the bootstrap and the main app cannot double dispatch one launch', () => {
		armSecondInstanceForwarding();
		armSecondInstanceForwarding();
		assert.deepEqual(
			registrations.map(({eventName}) => eventName),
			['second-instance'],
			'the bootstrap arms this the moment it takes the lock and the main app arms it again on the path where the bootstrap never ran',
		);
	});

	test('launches forwarded before a sink exists are replayed in order once it lands, thrower and all', () => {
		forward(['fluxer', 'fluxer://channels/1']);
		forward(['fluxer', 'boom']);
		forward(['fluxer', '--jump-list-task=new-dm']);
		const seen = [];
		setSecondInstanceSink((argv) => {
			seen.push(argv[1]);
			if (argv[1] === 'boom') {
				throw new Error('this sink throws');
			}
		});
		assert.deepEqual(
			seen,
			['fluxer://channels/1', 'boom', '--jump-list-task=new-dm'],
			'Electron never queues second-instance, so a launch that arrives while the splash is still updating is only recoverable from this buffer',
		);
	});

	test('a launch that arrives after the sink landed reaches it directly and nothing is replayed twice', () => {
		const seen = [];
		setSecondInstanceSink((argv) => {
			seen.push(argv[1]);
		});
		assert.deepEqual(seen, [], 'the buffer drained into the first sink, so a later sink must start empty');
		forward(['fluxer', 'fluxer://channels/2']);
		assert.deepEqual(seen, ['fluxer://channels/2']);
	});
});

describe('ModuleBootHandoff open-url forwarding', () => {
	const registrations = appRegistrations;
	const listenerFor = (eventName) =>
		registrations.find((registration) => registration.eventName === eventName).listener;
	const openUrl = (url) => {
		let prevented = false;
		listenerFor('open-url')(
			{
				preventDefault: () => {
					prevented = true;
				},
			},
			url,
		);
		return prevented;
	};

	test('arming is idempotent and listens for open-url and the launch fallback link', () => {
		armOpenUrlForwarding();
		armOpenUrlForwarding();
		assert.deepEqual(
			registrations
				.filter(({eventName}) => eventName !== 'second-instance')
				.map(({eventName, once}) => [eventName, once]),
			[
				['open-url', false],
				['ready', true],
			],
		);
	});

	test('links opened before the main app exists are buffered and replayed in order once its sink lands', () => {
		assert.equal(openUrl('fluxer://invite/abc'), true, 'macOS opens the link in a browser unless the event is claimed');
		listenerFor('ready')({}, {userInfo: {fallbackDeepLink: 'fluxer://channels/1'}});
		openUrl('fluxer://boom');
		const seen = [];
		setOpenUrlSink((url) => {
			seen.push(url);
			if (url === 'fluxer://boom') {
				throw new Error('this sink throws');
			}
		});
		assert.deepEqual(
			seen,
			['fluxer://invite/abc', 'fluxer://channels/1', 'fluxer://boom'],
			'open-url fires around will-finish-launching on a cold start, long before module convergence imports the main app',
		);
	});

	test('a link that arrives after the sink landed reaches it directly and nothing is replayed twice', () => {
		const seen = [];
		setOpenUrlSink((url) => {
			seen.push(url);
		});
		assert.deepEqual(seen, []);
		openUrl('fluxer://channels/2');
		assert.deepEqual(seen, ['fluxer://channels/2']);
	});
});
