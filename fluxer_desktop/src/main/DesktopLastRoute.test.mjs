// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {existsSync, mkdtempSync, rmSync, writeFileSync} from 'node:fs';
import {tmpdir} from 'node:os';
import nodePath from 'node:path';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const {
	recordDesktopLastRoute,
	resolveDesktopLandingUrl,
	desktopLastRouteFilePath,
	seedDesktopLastRouteFromLegacyStorage,
} = await import('./DesktopLastRoute.ts');
const {isRestorableDesktopRoutePath} = await import('@fluxer/desktop_ipc/src/LastRouteContract');

const DEFAULT_LANDING = 'fluxer-app://app/channels/@me';

function withTempUserData(run) {
	const dir = mkdtempSync(nodePath.join(tmpdir(), 'fluxer-last-route-'));
	try {
		run(dir);
	} finally {
		rmSync(dir, {recursive: true, force: true});
	}
}

describe('DesktopLastRoute', () => {
	test('falls back to the default landing url when nothing is stored', () => {
		withTempUserData((dir) => {
			assert.equal(resolveDesktopLandingUrl(dir), DEFAULT_LANDING);
		});
	});

	test('round-trips a recorded route into a same-origin landing url', () => {
		withTempUserData((dir) => {
			recordDesktopLastRoute(dir, '/channels/123/456');
			assert.equal(resolveDesktopLandingUrl(dir), 'fluxer-app://app/channels/123/456');
		});
	});

	test('ignores an unsafe stored path and uses the default', () => {
		withTempUserData((dir) => {
			writeFileSync(desktopLastRouteFilePath(dir), JSON.stringify({path: '//evil.example'}));
			assert.equal(resolveDesktopLandingUrl(dir), DEFAULT_LANDING);
		});
	});

	test('refuses to persist an unsafe route', () => {
		withTempUserData((dir) => {
			recordDesktopLastRoute(dir, 'https://evil.example');
			assert.equal(resolveDesktopLandingUrl(dir), DEFAULT_LANDING);
		});
	});

	test('tolerates a corrupt store file', () => {
		withTempUserData((dir) => {
			writeFileSync(desktopLastRouteFilePath(dir), 'not json');
			assert.equal(resolveDesktopLandingUrl(dir), DEFAULT_LANDING);
		});
	});
});

describe('seedDesktopLastRouteFromLegacyStorage', () => {
	const LEGACY_SESSION = {token: 'legacy-token', userId: '1555292173950255104'};

	function legacyLocation(lastLocation) {
		return JSON.stringify({lastLocation, lastMobileLayoutState: null, __mps__: {version: 1}});
	}

	test('lands the first migrated boot on the last location of the old client', () => {
		withTempUserData((dir) => {
			seedDesktopLastRouteFromLegacyStorage(dir, {
				...LEGACY_SESSION,
				Location: legacyLocation('/channels/@me/1555292173950255104'),
			});
			assert.equal(resolveDesktopLandingUrl(dir), 'fluxer-app://app/channels/@me/1555292173950255104');
		});
	});

	test('keeps a route the new client already recorded', () => {
		withTempUserData((dir) => {
			recordDesktopLastRoute(dir, '/channels/123/456');
			seedDesktopLastRouteFromLegacyStorage(dir, {...LEGACY_SESSION, Location: legacyLocation('/channels/@me/789')});
			assert.equal(resolveDesktopLandingUrl(dir), 'fluxer-app://app/channels/123/456');
		});
	});

	test('does not seed a route for a signed-out old client', () => {
		withTempUserData((dir) => {
			seedDesktopLastRouteFromLegacyStorage(dir, {Location: legacyLocation('/channels/@me/789')});
			seedDesktopLastRouteFromLegacyStorage(dir, {token: '', userId: '', Location: legacyLocation('/channels/1/2')});
			assert.equal(existsSync(desktopLastRouteFilePath(dir)), false);
		});
	});

	test('ignores a missing, corrupt or unsafe legacy location', () => {
		withTempUserData((dir) => {
			seedDesktopLastRouteFromLegacyStorage(dir, LEGACY_SESSION);
			seedDesktopLastRouteFromLegacyStorage(dir, {...LEGACY_SESSION, Location: 'not json'});
			seedDesktopLastRouteFromLegacyStorage(dir, {...LEGACY_SESSION, Location: 'null'});
			seedDesktopLastRouteFromLegacyStorage(dir, {...LEGACY_SESSION, Location: legacyLocation(null)});
			seedDesktopLastRouteFromLegacyStorage(dir, {...LEGACY_SESSION, Location: legacyLocation('https://evil.example')});
			assert.equal(existsSync(desktopLastRouteFilePath(dir)), false);
		});
	});
});

describe('isRestorableDesktopRoutePath', () => {
	test('accepts ordinary in-app routes', () => {
		for (const value of ['/channels/@me', '/channels/123/456', '/channels/@me/789?jump=1', '/discover']) {
			assert.equal(isRestorableDesktopRoutePath(value), true, value);
		}
	});

	test('rejects unsafe or non-string values', () => {
		for (const value of [
			null,
			undefined,
			42,
			'',
			'channels/@me',
			'//evil.example',
			'https://evil.example',
			'/foo://bar',
			'/a/../../etc/passwd',
			'/a\\b',
			'/a\nb',
			`/${'a'.repeat(600)}`,
		]) {
			assert.equal(isRestorableDesktopRoutePath(value), false, String(value));
		}
	});
});
