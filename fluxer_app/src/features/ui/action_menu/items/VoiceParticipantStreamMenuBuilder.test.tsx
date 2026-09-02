// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {installVoiceMenuTestBootstrap} from '@app/features/ui/action_menu/items/__fixtures__/VoiceMenuTestBootstrap';
import type {VoiceParticipantMenuScreenShareSource} from '@app/features/ui/action_menu/items/VoiceParticipantMenuTypes';
import type {I18n} from '@lingui/core';
import {expect, test, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});
vi.mock('@lingui/react/macro', () => ({
	Trans: () => null,
	useLingui: () => ({i18n: {_: (descriptor: {message?: string}) => descriptor.message ?? '', locale: 'en'}}),
}));
vi.mock('@app/features/voice/components/ActiveScreenShareMenu', () => ({
	changeActiveScreenShare: vi.fn(async () => undefined),
	stopActiveScreenShare: vi.fn(async () => undefined),
}));
vi.mock('@app/features/voice/components/modals/ScreenSharePickerModal', () => ({
	openScreenSharePreviewPrivacyModal: vi.fn(),
}));
vi.mock('@app/features/voice/engine/MediaEngineFacade', () => ({
	default: {
		applyLocalAudioPreferencesForUser: vi.fn(),
		getVoiceStateByConnectionId: () => null,
		connectionId: null,
	},
}));
vi.mock('@app/features/voice/state/PopoutWindowManager', () => ({
	default: {openTilePopout: vi.fn()},
	isVoicePopoutSupported: () => false,
}));
vi.mock('@app/features/voice/state/StreamAudioPrefs', () => ({
	default: {setMuted: vi.fn(), setVolume: vi.fn()},
}));
vi.mock('@app/features/voice/state/VoiceSettings', () => ({
	default: {showMyOwnScreenShare: false, pauseOwnScreenSharePreviewOnUnfocus: false},
}));
vi.mock('@app/features/voice/commands/VoiceSettingsCommands', () => ({
	update: vi.fn(),
}));

installVoiceMenuTestBootstrap();

const {buildVoiceParticipantStreamMenu} = await import(
	'@app/features/ui/action_menu/items/VoiceParticipantStreamMenuBuilder'
);

const i18n = {
	locale: 'en',
	_: (descriptor: {message?: string}) => descriptor.message ?? '',
} as unknown as I18n;

interface MenuLeaf {
	label?: string;
	items?: Array<MenuLeaf>;
}

function streamMenu(source: VoiceParticipantMenuScreenShareSource): Array<{items: Array<MenuLeaf>}> {
	return buildVoiceParticipantStreamMenu({
		i18n,
		userId: '111',
		channelId: null,
		participantIdentity: 'user_111_conn',
		displayName: 'Alice',
		isCurrentUserConnectedToVoice: false,
		source,
		streamVolume: 100,
		isStreamMuted: false,
		showMyOwnScreenShare: false,
		pauseOwnScreenSharePreviewOnUnfocus: false,
		onClose: () => undefined,
	}) as unknown as Array<{items: Array<MenuLeaf>}>;
}

function findLeaf(groups: Array<{items: Array<MenuLeaf>}>, label: string): MenuLeaf | null {
	for (const group of groups) {
		for (const item of group.items) {
			if (item.label === label) return item;
			for (const child of item.items ?? []) {
				if (child.label === label) return child;
			}
		}
	}
	return null;
}

const OWN_STREAM_SOURCE: VoiceParticipantMenuScreenShareSource = {
	kind: 'screen-share',
	streamKey: 'stream-key',
	state: {kind: 'own'},
};

const WATCHED_REMOTE_STREAM_SOURCE: VoiceParticipantMenuScreenShareSource = {
	kind: 'screen-share',
	streamKey: 'stream-key',
	state: {kind: 'remote-watched', hasAudio: true, onStopWatching: () => undefined},
};

test('own stream keeps a More options submenu with the screen-share preferences', () => {
	const groups = streamMenu(OWN_STREAM_SOURCE);
	const moreOptions = findLeaf(groups, 'More options');
	expect(moreOptions).not.toBeNull();
	expect(findLeaf(groups, 'Show my screen share')).not.toBeNull();
	expect(findLeaf(groups, 'Report Problem')).toBeNull();
});

test('remote watched stream omits the now-empty More options submenu and keeps audio controls', () => {
	const groups = streamMenu(WATCHED_REMOTE_STREAM_SOURCE);
	expect(findLeaf(groups, 'More options')).toBeNull();
	expect(findLeaf(groups, 'Mute')).not.toBeNull();
	expect(findLeaf(groups, 'Stream volume')).not.toBeNull();
	expect(findLeaf(groups, 'Report Problem')).toBeNull();
});
