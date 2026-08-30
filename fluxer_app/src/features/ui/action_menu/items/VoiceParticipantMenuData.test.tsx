// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {installVoiceMenuTestBootstrap} from '@app/features/ui/action_menu/items/__fixtures__/VoiceMenuTestBootstrap';
import type {VoiceParticipantMenuSource} from '@app/features/ui/action_menu/items/VoiceParticipantMenuTypes';
import type {User} from '@app/features/user/models/User';
import {act, createElement} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, expect, test, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});
vi.mock('@lingui/react/macro', () => ({
	Trans: () => null,
	useLingui: () => ({i18n: {_: (descriptor: {message?: string}) => descriptor.message ?? '', locale: 'en'}}),
}));

installVoiceMenuTestBootstrap();

const {useVoiceParticipantMenuData} = await import('@app/features/ui/action_menu/items/VoiceParticipantMenuData');
const {default: EntranceSoundListenerPrefs} = await import('@app/features/voice/state/EntranceSoundListenerPrefs');
const {default: MediaEngine} = await import('@app/features/voice/engine/MediaEngineFacade');
const {observer} = await import('mobx-react-lite');

(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;

const TARGET_USER = {id: '111', username: 'target'} as unknown as User;
const PARTICIPANT_SOURCE: VoiceParticipantMenuSource = {kind: 'participant'};
const CAMERA_SOURCE: VoiceParticipantMenuSource = {kind: 'camera'};
const noop = (): void => undefined;

interface MenuLeaf {
	label?: string;
	checked?: boolean;
	value?: number;
	items?: Array<MenuLeaf>;
}

interface MenuGroup {
	items: Array<MenuLeaf>;
}

const shadowedMediaEngineKeys: Array<string> = [];
let root: Root | null = null;
let container: HTMLDivElement | null = null;

function shadowMediaEngine(key: string, value: unknown): void {
	Object.defineProperty(MediaEngine, key, {value, configurable: true, writable: true});
	shadowedMediaEngineKeys.push(key);
}

function findLeaf(groups: Array<MenuGroup>, label: string): MenuLeaf | null {
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

async function renderMenu(
	options: Parameters<typeof useVoiceParticipantMenuData>[0],
): Promise<{groups: () => Array<MenuGroup>}> {
	const latest: {groups: Array<MenuGroup>} = {groups: []};
	const Probe = observer(function Probe() {
		latest.groups = useVoiceParticipantMenuData(options).groups as unknown as Array<MenuGroup>;
		return null;
	});
	container = document.createElement('div');
	document.body.appendChild(container);
	root = createRoot(container);
	await act(async () => {
		root?.render(createElement(Probe));
	});
	return {groups: () => latest.groups};
}

afterEach(async () => {
	await act(async () => {
		root?.unmount();
	});
	root = null;
	container?.remove();
	container = null;
	for (const key of shadowedMediaEngineKeys.splice(0)) {
		delete (MediaEngine as unknown as Record<string, unknown>)[key];
	}
	EntranceSoundListenerPrefs.reset(TARGET_USER.id);
});

test('entrance-sound checkbox tracks the pref while the menu stays open', async () => {
	const menu = await renderMenu({
		user: TARGET_USER,
		surface: 'call-tile',
		source: PARTICIPANT_SOURCE,
		onClose: noop,
	});
	expect(findLeaf(menu.groups(), 'Mute entrance sound')?.checked).toBe(false);
	await act(async () => {
		EntranceSoundListenerPrefs.setMuted(TARGET_USER.id, true);
	});
	expect(findLeaf(menu.groups(), 'Mute entrance sound')?.checked).toBe(true);
	await act(async () => {
		EntranceSoundListenerPrefs.setVolume(TARGET_USER.id, 42);
	});
	expect(findLeaf(menu.groups(), 'Entrance sound volume')?.value).toBe(42);
});

test('disable video locally is offered for another user camera tile in the local room', async () => {
	shadowMediaEngine('connected', true);
	shadowMediaEngine('connectionId', 'self-connection');
	shadowMediaEngine('channelId', 'channel-1');
	shadowMediaEngine('getVoiceStateByConnectionId', (connectionId: string) =>
		connectionId === 'target-connection' ? {channel_id: 'channel-1', user_id: TARGET_USER.id} : null,
	);
	const menu = await renderMenu({
		user: TARGET_USER,
		connectionId: 'target-connection',
		surface: 'call-tile',
		source: CAMERA_SOURCE,
		onClose: noop,
	});
	expect(findLeaf(menu.groups(), 'Disable video locally')).not.toBeNull();
});

test('offers a single inline user-volume slider and no nested secondary submenu wrapper', async () => {
	const REMOVED_SUBMENU_LABELS = [
		'Media controls',
		'Device controls',
		'Display options',
		'User actions',
		'Relationship',
		'Relationship actions',
		'Moderation',
		'Moderation actions',
		'Advanced',
		'Advanced actions',
	];
	shadowMediaEngine('connected', true);
	shadowMediaEngine('connectionId', 'self-connection');
	shadowMediaEngine('channelId', 'channel-1');
	shadowMediaEngine('getVoiceStateByConnectionId', (connectionId: string) =>
		connectionId === 'target-connection' ? {channel_id: 'channel-1', user_id: TARGET_USER.id} : null,
	);
	const menu = await renderMenu({
		user: TARGET_USER,
		connectionId: 'target-connection',
		surface: 'participant-list',
		source: PARTICIPANT_SOURCE,
		onClose: noop,
	});
	const groups = menu.groups();
	const topLevelItems = groups.flatMap((group) => group.items);
	const topLevelSliders = topLevelItems.filter((item) => typeof item.value === 'number');
	expect(topLevelSliders).toHaveLength(1);
	expect(topLevelSliders[0]?.label).toBe('User volume');
	const submenuLabels = topLevelItems.filter((item) => Array.isArray(item.items)).map((item) => item.label);
	expect(submenuLabels.every((label) => label === 'Entrance sound')).toBe(true);
	for (const label of REMOVED_SUBMENU_LABELS) {
		expect(findLeaf(groups, label)).toBeNull();
	}
});
