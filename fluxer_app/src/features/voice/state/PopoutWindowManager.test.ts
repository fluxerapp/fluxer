// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import PopoutWindowManager, {
	getVoiceCallPopoutKey,
	getVoiceTilePopoutKey,
	type VoiceTilePopoutSource,
} from '@app/features/voice/state/PopoutWindowManager';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

interface PendingAlwaysOnTopCall {
	key: string;
	flag: boolean;
	resolve: () => void;
	reject: (error: Error) => void;
}

let pendingAlwaysOnTopCalls: Array<PendingAlwaysOnTopCall> = [];

function installElectronApi(): void {
	pendingAlwaysOnTopCalls = [];
	(window as unknown as {electron: unknown}).electron = {
		popoutFocus: () => Promise.resolve(true),
		popoutSetAlwaysOnTop: (key: string, flag: boolean) =>
			new Promise<boolean>((resolve, reject) => {
				pendingAlwaysOnTopCalls.push({key, flag, resolve: () => resolve(true), reject});
			}),
	};
}

function openCallPopout(channelId: string): string {
	expect(PopoutWindowManager.openCallPopout({channelId, guildId: null, title: channelId})).toBe(true);
	return getVoiceCallPopoutKey(channelId);
}

function openTilePopout(participantIdentity: string, source: VoiceTilePopoutSource): string {
	expect(
		PopoutWindowManager.openTilePopout({
			participantIdentity,
			source,
			userId: 'user-1',
			connectionId: 'connection-1',
			channelId: 'channel-1',
			guildId: null,
			title: participantIdentity,
		}),
	).toBe(true);
	return getVoiceTilePopoutKey(participantIdentity, source);
}

function generationOf(key: string): number {
	const descriptor = PopoutWindowManager.popouts[key];
	if (!descriptor) throw new Error(`Expected popout ${key} to be open`);
	return descriptor.generation;
}

beforeEach(() => {
	installElectronApi();
	PopoutWindowManager.closeAll();
});

afterEach(() => {
	PopoutWindowManager.closeAll();
	delete (window as unknown as {electron?: unknown}).electron;
});

describe('popout generations', () => {
	test('allocates a fresh generation every time a key is reopened', () => {
		const key = openCallPopout('channel-a');
		const firstGeneration = generationOf(key);
		PopoutWindowManager.close(key, firstGeneration);
		openCallPopout('channel-a');
		expect(generationOf(key)).not.toBe(firstGeneration);
	});

	test('ignores handleWindowClosed from a superseded window', () => {
		const key = openCallPopout('channel-a');
		const firstGeneration = generationOf(key);
		PopoutWindowManager.close(key, firstGeneration);
		openCallPopout('channel-a');
		const secondGeneration = generationOf(key);
		PopoutWindowManager.handleWindowClosed(key, firstGeneration);
		expect(PopoutWindowManager.isOpen(key)).toBe(true);
		PopoutWindowManager.handleWindowClosed(key, secondGeneration);
		expect(PopoutWindowManager.isOpen(key)).toBe(false);
	});

	test('ignores close from a superseded window', () => {
		const key = openCallPopout('channel-a');
		const firstGeneration = generationOf(key);
		PopoutWindowManager.close(key, firstGeneration);
		openCallPopout('channel-a');
		PopoutWindowManager.close(key, firstGeneration);
		expect(PopoutWindowManager.isOpen(key)).toBe(true);
	});

	test('ignores attachWindow from a superseded window', () => {
		const key = openCallPopout('channel-a');
		const staleGeneration = generationOf(key);
		PopoutWindowManager.close(key, staleGeneration);
		openCallPopout('channel-a');
		const staleChildWindow = {closed: false, focus: vi.fn(), close: vi.fn()};
		PopoutWindowManager.attachWindow(key, staleGeneration, staleChildWindow);
		PopoutWindowManager.close(key);
		expect(staleChildWindow.close).not.toHaveBeenCalled();
	});

	test('closes the child window attached for the current generation', () => {
		const key = openCallPopout('channel-a');
		const childWindow = {closed: false, focus: vi.fn(), close: vi.fn()};
		PopoutWindowManager.attachWindow(key, generationOf(key), childWindow);
		PopoutWindowManager.close(key);
		expect(childWindow.close).toHaveBeenCalledTimes(1);
	});
});

describe('always on top', () => {
	test('commits the pinned state only after the desktop call resolves', async () => {
		const key = openCallPopout('channel-a');
		PopoutWindowManager.setAlwaysOnTop(key, true);
		expect(pendingAlwaysOnTopCalls).toHaveLength(1);
		expect(PopoutWindowManager.isAlwaysOnTop(key)).toBe(false);
		pendingAlwaysOnTopCalls[0]?.resolve();
		await vi.waitFor(() => expect(PopoutWindowManager.isAlwaysOnTop(key)).toBe(true));
	});

	test('leaves the pinned state untouched when the desktop call rejects', async () => {
		const key = openCallPopout('channel-a');
		PopoutWindowManager.setAlwaysOnTop(key, true);
		pendingAlwaysOnTopCalls[0]?.reject(new Error('popout pin failed'));
		await vi.waitFor(() => expect(pendingAlwaysOnTopCalls).toHaveLength(1));
		expect(PopoutWindowManager.isAlwaysOnTop(key)).toBe(false);
	});

	test('serializes concurrent toggles and applies the last requested value', async () => {
		const key = openCallPopout('channel-a');
		PopoutWindowManager.setAlwaysOnTop(key, true);
		PopoutWindowManager.setAlwaysOnTop(key, false);
		expect(pendingAlwaysOnTopCalls).toHaveLength(1);
		expect(pendingAlwaysOnTopCalls[0]?.flag).toBe(true);
		pendingAlwaysOnTopCalls[0]?.resolve();
		await vi.waitFor(() => expect(pendingAlwaysOnTopCalls).toHaveLength(2));
		expect(pendingAlwaysOnTopCalls[1]?.flag).toBe(false);
		pendingAlwaysOnTopCalls[1]?.resolve();
		await vi.waitFor(() => expect(PopoutWindowManager.isAlwaysOnTop(key)).toBe(false));
	});

	test('toggles against the pending desired value rather than the committed one', () => {
		const key = openCallPopout('channel-a');
		PopoutWindowManager.toggleAlwaysOnTop(key);
		expect(pendingAlwaysOnTopCalls[0]?.flag).toBe(true);
		PopoutWindowManager.toggleAlwaysOnTop(key);
		expect(pendingAlwaysOnTopCalls).toHaveLength(1);
		PopoutWindowManager.toggleAlwaysOnTop(key);
		expect(pendingAlwaysOnTopCalls).toHaveLength(1);
	});

	test('drops in-flight operations when the popout is removed', async () => {
		const key = openCallPopout('channel-a');
		PopoutWindowManager.setAlwaysOnTop(key, true);
		PopoutWindowManager.close(key);
		pendingAlwaysOnTopCalls[0]?.resolve();
		await vi.waitFor(() => expect(pendingAlwaysOnTopCalls).toHaveLength(1));
		expect(PopoutWindowManager.isAlwaysOnTop(key)).toBe(false);
	});
});

describe('closeConnectionBoundPopouts', () => {
	test('keeps standalone user popouts and closes track-bound ones', () => {
		const callKey = openCallPopout('channel-a');
		const cameraKey = openTilePopout('participant-1', 'camera');
		const screenShareKey = openTilePopout('participant-1', 'screen_share');
		const userKey = openTilePopout('participant-2', 'user');
		PopoutWindowManager.closeConnectionBoundPopouts();
		expect(PopoutWindowManager.isOpen(callKey)).toBe(false);
		expect(PopoutWindowManager.isOpen(cameraKey)).toBe(false);
		expect(PopoutWindowManager.isOpen(screenShareKey)).toBe(false);
		expect(PopoutWindowManager.isOpen(userKey)).toBe(true);
	});
});
