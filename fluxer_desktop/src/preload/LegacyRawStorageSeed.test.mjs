// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import '../main/LocalAppTestSupport.test.mjs';

const {DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY, DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_VALUE} = await import(
	'../../../packages/desktop_ipc/src/LegacyHarvestContract.ts'
);
const {applyLegacyRawLocalStorage} = await import('./LegacyRawStorageSeed.ts');

const LEGACY_SKELETON_MEMORY = JSON.stringify({
	version: 9,
	chrome: {
		dmSidebar: {
			isMobile: false,
			friendsVisible: true,
			personalNotesVisible: true,
			premiumVisible: false,
			sectionVisible: true,
			channelRowCount: 3,
			channelSubtextFlags: [true, false, false],
		},
		guildRail: {
			inlineDmRowCount: 2,
			inlineDmUnreadFlags: [true],
			selectedInlineDmRowIndex: -1,
			outageVisible: false,
			fluxerVisible: true,
			favoritesVisible: true,
			discoveryVisible: true,
			addGuildVisible: true,
			downloadVisible: false,
			helpVisible: true,
			selectedItemIndex: -1,
			organizedItems: [
				{kind: 'guild', indicator: 'mention'},
				{kind: 'collapsed_folder', indicator: 'unread', childCount: 3, showIconWhenCollapsed: true},
				{
					kind: 'expanded_folder',
					indicator: 'none',
					childCount: 2,
					childIndicators: ['none', 'unread'],
					selectedChildIndex: 1,
				},
			],
			scrollTopPx: 48.2,
		},
	},
	nagbar: {
		rows: [
			{tone: 'alert', hasActions: true, dismissible: false},
			{tone: 'brand', hasActions: true, dismissible: true},
		],
	},
	channelHeader: {staffToolsVisible: true, updaterVisible: false, favoritesVisible: true},
	composer: {desktopActionCount: 4, mobileActionCount: 2, sendDividerVisible: false},
	messagePresentation: {
		compact: true,
		messageGutterPx: 12,
		fontSizePx: 15,
		groupSpacingPx: 8,
		compactAvatarsVisible: true,
		compactTimestampWidthPx: 60,
		viewportHeightPx: 474,
	},
});

function createStorage(initial = {}, refuseKey = null) {
	const entries = new Map(Object.entries(initial));
	return {
		entries,
		getItem: (key) => entries.get(key) ?? null,
		setItem: (key, value) => {
			if (refuseKey === key) throw new Error('quota exceeded');
			entries.set(key, value);
		},
	};
}

function installWindow(protocol, storage) {
	globalThis.window = {location: {protocol}, localStorage: storage};
	return storage;
}

function createRenderer(payload) {
	const calls = [];
	return {
		calls,
		sendSync: (channel) => {
			calls.push(channel);
			return payload;
		},
	};
}

describe('seeding the legacy localStorage onto the new origin', () => {
	test('does nothing outside the local app scheme', () => {
		const storage = installWindow('https:', createStorage());
		const renderer = createRenderer({token: 'tok'});
		applyLegacyRawLocalStorage(renderer);
		assert.deepEqual(renderer.calls, []);
		assert.equal(storage.entries.size, 0);
	});

	test('seeds every harvested string key and records the witness once', () => {
		const storage = installWindow('fluxer-app:', createStorage());
		const renderer = createRenderer({token: 'tok', theme: 'dark', count: 3});
		applyLegacyRawLocalStorage(renderer);
		assert.equal(storage.getItem('token'), 'tok');
		assert.equal(storage.getItem('theme'), 'dark');
		assert.equal(storage.getItem('count'), null);
		applyLegacyRawLocalStorage(renderer);
		assert.equal(renderer.calls.length, 1);
	});

	test('a harvested session marks the preboot session so the first frame is the signed-in shell', () => {
		const storage = installWindow('fluxer-app:', createStorage());
		applyLegacyRawLocalStorage(createRenderer({token: 'tok', userId: '42'}));
		assert.equal(storage.getItem('fluxer:gateway:preboot:session'), '1');
	});

	test('a half session or one the new origin already holds leaves the preboot marker alone', () => {
		const tokenOnly = installWindow('fluxer-app:', createStorage());
		applyLegacyRawLocalStorage(createRenderer({token: 'tok'}));
		assert.equal(tokenOnly.getItem('fluxer:gateway:preboot:session'), null);
		const current = installWindow('fluxer-app:', createStorage({token: 'the new token', userId: '7'}));
		applyLegacyRawLocalStorage(createRenderer({token: 'tok', userId: '42'}));
		assert.equal(current.getItem('fluxer:gateway:preboot:session'), null);
	});

	test('the legacy zoom level seeds the zoom mirror the first frame reads', () => {
		const storage = installWindow('fluxer-app:', createStorage());
		applyLegacyRawLocalStorage(createRenderer({'Accessibility:zoomLevel': '1.15'}));
		assert.equal(storage.getItem('fluxer:accessibility:zoom-preboot'), '115');
	});

	test('an older accessibility store seeds the zoom mirror and an out of range level is clamped', () => {
		const older = installWindow('fluxer-app:', createStorage());
		applyLegacyRawLocalStorage(createRenderer({Accessibility: JSON.stringify({zoomLevel: 1.25})}));
		assert.equal(older.getItem('fluxer:accessibility:zoom-preboot'), '125');
		const clamped = installWindow('fluxer-app:', createStorage());
		applyLegacyRawLocalStorage(createRenderer({'Accessibility:zoomLevel': '9'}));
		assert.equal(clamped.getItem('fluxer:accessibility:zoom-preboot'), '200');
	});

	test('no legacy zoom level or an existing mirror leaves the zoom mirror alone', () => {
		const absent = installWindow('fluxer-app:', createStorage());
		applyLegacyRawLocalStorage(
			createRenderer({
				'Accessibility:zoomLevel': 'large',
				Accessibility: JSON.stringify({__mps__: {version: 2}, zoomLevel: 1.5}),
			}),
		);
		assert.equal(absent.getItem('fluxer:accessibility:zoom-preboot'), null);
		const current = installWindow('fluxer-app:', createStorage({'fluxer:accessibility:zoom-preboot': '90'}));
		applyLegacyRawLocalStorage(createRenderer({'Accessibility:zoomLevel': '1.15'}));
		assert.equal(current.getItem('fluxer:accessibility:zoom-preboot'), '90');
	});

	test('a harvested session seeds a complete shell hint from the legacy skeleton memory', () => {
		const storage = installWindow('fluxer-app:', createStorage());
		applyLegacyRawLocalStorage(
			createRenderer({
				token: 'tok',
				userId: '42',
				SkeletonLayoutMemory: LEGACY_SKELETON_MEMORY,
				'fluxer:ui:sidebar-width': '306',
			}),
		);
		const hint = JSON.parse(storage.getItem('fluxer:ui:shell-hint'));
		assert.equal(typeof hint.t, 'number');
		assert.deepEqual(
			{...hint, t: 0},
			{
				v: 4,
				t: 0,
				p: '/channels/@me',
				a: '',
				mo: 0,
				ua: 0,
				fs: 0,
				sw: 306,
				sv: 'dm',
				ck: 'other',
				cb: 1,
				rf: 1,
				rv: 1,
				ru: [1, 0],
				rti: 0,
				ro: 0,
				ri: [
					[0, 2],
					[1, 1, 3, 1],
					[2, 0, 2, 1, 0, 1],
				],
				rgi: -1,
				rbm: 11,
				rbi: -1,
				rs: 48,
				gb: 0,
				ga: 250,
				gn: 0,
				gd: 0,
				gm: 0,
				sr: 3,
				sam: 3,
				ss: 1,
				sb: [1, 0, 0],
				cid: '',
				ch: 2,
				ct: 0,
				cnw: 0,
				ctw: 0,
				cda: -1,
				cma: -1,
				cfv: 1,
				cst: 1,
				cuv: 0,
				ml: 0,
				co: 4,
				com: 2,
				cdv: 0,
				pc: 1,
				pg: 12,
				pf: 15,
				ps: 8,
				pa: 1,
				pt: 60,
				pv: 474,
				nr: [
					[7, 1, 0],
					[5, 1, 1],
				],
				vh: 0,
				vc: 0,
				mr: 0,
				ms: 0,
				mn: 0,
			},
		);
	});

	test('a signed-out harvest or an existing hint seeds no shell hint', () => {
		const signedOut = installWindow('fluxer-app:', createStorage());
		applyLegacyRawLocalStorage(createRenderer({SkeletonLayoutMemory: LEGACY_SKELETON_MEMORY}));
		assert.equal(signedOut.getItem('fluxer:ui:shell-hint'), null);
		const current = installWindow('fluxer-app:', createStorage({'fluxer:ui:shell-hint': 'current'}));
		applyLegacyRawLocalStorage(
			createRenderer({token: 'tok', userId: '42', SkeletonLayoutMemory: LEGACY_SKELETON_MEMORY}),
		);
		assert.equal(current.getItem('fluxer:ui:shell-hint'), 'current');
	});

	test('a skeleton memory with an unknown rail item or nagbar tone keeps the generic rail and no nagbars', () => {
		const storage = installWindow('fluxer-app:', createStorage());
		applyLegacyRawLocalStorage(
			createRenderer({
				token: 'tok',
				userId: '42',
				SkeletonLayoutMemory: JSON.stringify({
					chrome: {guildRail: {organizedItems: [{kind: 'portal', indicator: 'none'}]}},
					nagbar: {rows: [{tone: 'sparkly', hasActions: false, dismissible: false}]},
				}),
			}),
		);
		const hint = JSON.parse(storage.getItem('fluxer:ui:shell-hint'));
		assert.equal(hint.ri.length, 6);
		assert.deepEqual(hint.nr, []);
		assert.equal(hint.pv, undefined);
	});

	test('never overwrites a value the new origin already holds', () => {
		const storage = installWindow('fluxer-app:', createStorage({token: 'the new token'}));
		applyLegacyRawLocalStorage(createRenderer({token: 'the legacy token'}));
		assert.equal(storage.getItem('token'), 'the new token');
	});

	test('ignores a witness carried inside the harvested corpus', () => {
		const storage = installWindow('fluxer-app:', createStorage());
		applyLegacyRawLocalStorage(createRenderer({[DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY]: 'stale', token: 'tok'}));
		assert.equal(
			storage.getItem(DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY),
			DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_VALUE,
		);
	});

	test('a refused preboot read leaves the witness unset so a later launch retries', () => {
		const storage = installWindow('fluxer-app:', createStorage());
		applyLegacyRawLocalStorage(createRenderer(null));
		assert.equal(storage.getItem(DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY), null);
	});

	test('a storage that refuses a write leaves the witness unset', () => {
		const storage = installWindow('fluxer-app:', createStorage({}, 'theme'));
		applyLegacyRawLocalStorage(createRenderer({token: 'tok', theme: 'dark'}));
		assert.equal(storage.getItem('token'), 'tok');
		assert.equal(storage.getItem(DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY), null);
	});
});
