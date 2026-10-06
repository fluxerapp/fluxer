// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {loadTsModule} from './fixtures/TsModuleLoader.mjs';

const desktopConfig = loadTsModule('@electron/common/DesktopConfig', {
	stubs: {
		'node:fs': {},
		'node:path': {},
		'electron-log': {debug: () => {}, info: () => {}, warn: () => {}, error: () => {}},
		'@electron/common/BuildChannel': {BUILD_CHANNEL: 'stable'},
	},
});

describe('sanitizePersistedGlobalShortcutsSettings', () => {
	test('keeps valid fields', () => {
		assert.deepEqual(
			desktopConfig.sanitizePersistedGlobalShortcutsSettings({
				portal_consent: 'granted',
				direct_input_enabled: true,
				migrated: true,
				last_actions: [{action: 'voice_push_to_talk', description: ' Push to talk ', preferredTrigger: 'F13'}],
			}),
			{
				portal_consent: 'granted',
				direct_input_enabled: true,
				migrated: true,
				last_actions: [{action: 'voice_push_to_talk', description: 'Push to talk', preferredTrigger: 'F13'}],
			},
		);
	});

	test('drops invalid fields and unknown or duplicate actions', () => {
		assert.deepEqual(
			desktopConfig.sanitizePersistedGlobalShortcutsSettings({
				portal_consent: 'maybe',
				direct_input_enabled: 'yes',
				last_actions: [
					{action: 'misc_help', description: 'Help', preferredTrigger: null},
					{action: 'voice_toggle_mute', description: '', preferredTrigger: null},
					{action: 'voice_toggle_mute', description: 'Toggle mute', preferredTrigger: 42},
					{action: 'voice_toggle_mute', description: 'Again', preferredTrigger: 'F1'},
				],
			}),
			{last_actions: [{action: 'voice_toggle_mute', description: 'Toggle mute', preferredTrigger: null}]},
		);
		assert.equal(desktopConfig.sanitizePersistedGlobalShortcutsSettings({portal_consent: 'nope'}), undefined);
		assert.equal(desktopConfig.sanitizePersistedGlobalShortcutsSettings('granted'), undefined);
	});

	test('defaults to an unset, disabled, unmigrated state', () => {
		assert.deepEqual(desktopConfig.getGlobalShortcutsSettings(), {
			portalConsent: 'unset',
			directInputEnabled: false,
			migrated: false,
			lastActions: [],
		});
	});
});
