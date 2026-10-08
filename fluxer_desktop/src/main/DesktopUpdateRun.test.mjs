// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const {DesktopUpdateRun} = await import('@electron/main/DesktopUpdateRun');

const LAUNCH_ATTEMPT = Object.freeze({committed: {fluxer_renderer: 'b'.repeat(64)}});

function harness({
	probes = [{shellLatestVersion: '2026.1008.2', shellNewer: false, modulesChanged: true}],
	canSelfUpdateShell = true,
	shellResults = [],
	installFails = false,
	settled = true,
} = {}) {
	const steps = [];
	const published = [];
	const failures = [];
	let probeIndex = 0;
	let shellIndex = 0;
	const quiet = () => undefined;
	const run = new DesktopUpdateRun({
		probe: async () => {
			const next = probes[Math.min(probeIndex, probes.length - 1)];
			probeIndex += 1;
			steps.push('probe');
			if (next instanceof Error) throw next;
			return next;
		},
		canSelfUpdateShell,
		runShellSelfUpdate: async () => {
			steps.push('shell-update');
			const result = shellResults[Math.min(shellIndex, shellResults.length - 1)];
			shellIndex += 1;
			return result;
		},
		installModules: async () => {
			steps.push('install');
			if (installFails) throw new Error('disk full');
			return LAUNCH_ATTEMPT;
		},
		takeover: {
			begin: async () => {
				steps.push('takeover');
			},
			restore: () => {
				steps.push('restore');
			},
			closeApp: async () => {
				steps.push('close');
			},
			reopen: (launchAttempt) => {
				steps.push(launchAttempt == null ? 'reopen' : 'reopen-updated');
			},
			reloadInPlace: async () => {
				steps.push('reload-in-place');
			},
		},
		publish: (check) => {
			published.push(check);
		},
		openDownloadsPage: async () => {
			steps.push('downloads-page');
		},
		reportFailure: (failure) => {
			failures.push(failure);
		},
		logger: {info: quiet, warn: quiet, error: quiet},
	});
	if (settled) run.markLaunchSettled();
	return {run, steps, published, failures};
}

describe('DesktopUpdateRun', () => {
	test('the background check only looks, it never downloads, closes or installs anything', async () => {
		const {run, steps, published} = harness();

		assert.deepEqual(await run.check(), {shellNewer: false, modulesChanged: true});

		assert.deepEqual(steps, ['probe']);
		assert.deepEqual(published, []);
	});

	test('a click takes the window over, checks, and only then closes the app and installs', async () => {
		const {run, steps, published} = harness();

		await run.start();

		assert.deepEqual(steps, ['probe', 'takeover', 'close', 'install', 'reopen-updated']);
		assert.deepEqual(published, [{shellNewer: false, modulesChanged: false}]);
	});

	test('a click while offline gives the same windows back instead of tearing the app down', async () => {
		const {run, steps, published} = harness({probes: [new Error('offline')]});

		await run.start();

		assert.deepEqual(steps, ['probe', 'takeover', 'restore']);
		assert.deepEqual(published, [], 'the icon stays so the next click can try again');
	});

	test('a click after the update was withdrawn gives the windows back and hides the icon', async () => {
		const {run, steps, published} = harness({
			probes: [{shellLatestVersion: '2026.1008.1', shellNewer: false, modulesChanged: false}],
		});

		await run.start();

		assert.deepEqual(steps, ['probe', 'takeover', 'restore']);
		assert.deepEqual(published, [{shellNewer: false, modulesChanged: false}]);
	});

	test('a shell update runs before any module download, and modules install only when the shell did not restart', async () => {
		const {run, steps} = harness({
			probes: [{shellLatestVersion: '2026.1008.2', shellNewer: true, modulesChanged: true}],
			shellResults: [{reason: 'install-failed', detail: 'nope'}],
		});

		await run.start();

		assert.deepEqual(steps, ['probe', 'takeover', 'close', 'shell-update', 'install', 'reopen-updated']);
	});

	test('a shell feed that lags the manifest stops offering the shell until the manifest moves on', async () => {
		const lagging = {shellLatestVersion: '2026.1008.2', shellNewer: true, modulesChanged: false};
		const {run, steps, published} = harness({
			probes: [lagging, lagging, {...lagging, shellLatestVersion: '2026.1008.3'}],
			shellResults: [{reason: 'no-update', detail: null}],
		});

		await run.start();
		assert.deepEqual(published, [{shellNewer: false, modulesChanged: false}]);

		assert.deepEqual(
			await run.check(),
			{shellNewer: false, modulesChanged: false},
			'the poll no longer shows the icon',
		);
		assert.deepEqual(await run.check(), {shellNewer: true, modulesChanged: false}, 'a newer manifest shell does');
		assert.deepEqual(steps, ['probe', 'takeover', 'close', 'shell-update', 'reopen', 'probe', 'probe']);
	});

	test('a shell update that failed on the network is retried by the next click', async () => {
		const shell = {shellLatestVersion: '2026.1008.2', shellNewer: true, modulesChanged: false};
		const {run, steps, published} = harness({
			probes: [shell],
			shellResults: [
				{reason: 'timed-out', detail: 'slow link'},
				{reason: 'check-failed', detail: 'offline'},
			],
		});

		await run.start();
		await run.start();

		assert.deepEqual(published, [
			{shellNewer: true, modulesChanged: false},
			{shellNewer: true, modulesChanged: false},
		]);
		assert.equal(steps.filter((step) => step === 'shell-update').length, 2);
		assert.equal(steps.includes('downloads-page'), false);
	});

	test('only a shell update that failed to install falls back to the downloads page', async () => {
		const shell = {shellLatestVersion: '2026.1008.2', shellNewer: true, modulesChanged: false};
		const {run, steps} = harness({probes: [shell], shellResults: [{reason: 'install-failed', detail: 'apply'}]});

		await run.start();
		steps.length = 0;
		await run.start();

		assert.deepEqual(steps, ['downloads-page']);
	});

	test('a shell that cannot update itself never offers the shell part', async () => {
		const {run} = harness({
			canSelfUpdateShell: false,
			probes: [{shellLatestVersion: '2026.1008.2', shellNewer: true, modulesChanged: false}],
		});

		assert.deepEqual(await run.check(), {shellNewer: false, modulesChanged: false});
	});

	test('a failed module install reopens the app on the installed set and keeps the icon', async () => {
		const {run, steps, published} = harness({installFails: true});

		await run.start();

		assert.deepEqual(steps, ['probe', 'takeover', 'close', 'install', 'reopen']);
		assert.deepEqual(published, [{shellNewer: false, modulesChanged: true}]);
	});

	test('a click before the renderer confirmed its launch does nothing', async () => {
		const {run, steps} = harness({settled: false});

		await run.start();

		assert.deepEqual(steps, []);
	});
});

describe('DesktopUpdateRun telling the user when a click did not update', () => {
	const SHELL_AND_MODULES = {shellLatestVersion: '2026.1008.2', shellNewer: true, modulesChanged: true};

	test('an update that installs reports nothing', async () => {
		const {run, failures} = harness();

		await run.start();

		assert.deepEqual(failures, []);
	});

	test('a check that fails gives the windows back and says so', async () => {
		const {run, steps, failures} = harness({probes: [new Error('offline')]});

		await run.start();

		assert.deepEqual(steps, ['probe', 'takeover', 'restore']);
		assert.deepEqual(failures, [{reason: 'check-failed', detail: 'offline'}]);
	});

	test('a shell update that does not finish is reported once the app is back', async () => {
		const {run, steps, failures} = harness({
			probes: [SHELL_AND_MODULES],
			shellResults: [{reason: 'timed-out', detail: 'the update did not finish'}],
		});

		await run.start();

		assert.equal(steps.at(-1), 'reopen-updated');
		assert.deepEqual(failures, [{reason: 'timed-out', detail: 'the update did not finish'}]);
	});

	test('a feed that has no newer shell after all is not a failure', async () => {
		const {run, failures} = harness({
			probes: [SHELL_AND_MODULES],
			shellResults: [{reason: 'no-update', detail: null}],
		});

		await run.start();

		assert.deepEqual(failures, []);
	});

	test('a module install that throws is reported with its cause', async () => {
		const {run, steps, failures} = harness({installFails: true});

		await run.start();

		assert.equal(steps.at(-1), 'reopen');
		assert.deepEqual(failures, [{reason: 'download-failed', detail: 'disk full'}]);
	});
});

describe('DesktopUpdateRun installing a renderer only update in place', () => {
	const MODULES_ONLY = {shellLatestVersion: '2026.1008.2', shellNewer: false, modulesChanged: true};
	const SHELL_TOO = {shellLatestVersion: '2026.1008.3', shellNewer: true, modulesChanged: true};

	test('a click after the background check found only modules never shows the splash or closes a window', async () => {
		const {run, steps, published, failures} = harness({probes: [MODULES_ONLY]});
		await run.check();

		await run.start();

		assert.deepEqual(steps, ['probe', 'probe', 'install', 'reload-in-place']);
		assert.deepEqual(published, [{shellNewer: false, modulesChanged: false}]);
		assert.deepEqual(failures, []);
	});

	test('a shell that turned newer since the background check still updates behind the splash', async () => {
		const {run, steps} = harness({
			probes: [MODULES_ONLY, SHELL_TOO],
			shellResults: [{reason: 'no-update', detail: null}],
		});
		await run.check();

		await run.start();

		assert.deepEqual(steps, ['probe', 'probe', 'takeover', 'close', 'shell-update', 'install', 'reopen-updated']);
	});

	test('a download that fails leaves the running app alone and says so', async () => {
		const {run, steps, published, failures} = harness({probes: [MODULES_ONLY], installFails: true});
		await run.check();

		await run.start();

		assert.deepEqual(steps, ['probe', 'probe', 'install']);
		assert.deepEqual(published, [{shellNewer: false, modulesChanged: true}]);
		assert.deepEqual(failures, [{reason: 'download-failed', detail: 'disk full'}]);
	});

	test('a click while offline changes nothing on screen', async () => {
		const {run, steps, failures} = harness({probes: [MODULES_ONLY, new Error('offline')]});
		await run.check();

		await run.start();

		assert.deepEqual(steps, ['probe', 'probe']);
		assert.deepEqual(failures, [{reason: 'check-failed', detail: 'offline'}]);
	});
});
