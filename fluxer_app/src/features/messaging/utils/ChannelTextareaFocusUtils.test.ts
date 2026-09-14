// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {fileURLToPath} from 'node:url';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const rolloutMock = {enabled: true};
const dispatch = vi.fn();

vi.mock('@app/features/messaging/state/MessageKeyboardFocusRollout', () => ({default: rolloutMock}));
vi.mock('@app/features/platform/utils/ComponentBus', () => ({ComponentBus: {dispatch}}));

const {focusChannelTextareaFromKeybind} = await import('@app/features/messaging/utils/ChannelTextareaFocusUtils');

const CHANNEL_ID = '900000000000000001';

beforeEach(() => {
	rolloutMock.enabled = true;
	dispatch.mockReset();
});

afterEach(() => {
	vi.restoreAllMocks();
});

describe('focusChannelTextareaFromKeybind', () => {
	it('keeps keyboard mode on when the keybind focuses the composer in the experiment arm', () => {
		focusChannelTextareaFromKeybind(CHANNEL_ID);
		expect(dispatch).toHaveBeenCalledWith('FOCUS_TEXTAREA', {channelId: CHANNEL_ID, enterKeyboardMode: true});
	});

	it('dispatches the unchanged payload in the control arm', () => {
		rolloutMock.enabled = false;
		focusChannelTextareaFromKeybind(CHANNEL_ID);
		expect(dispatch).toHaveBeenCalledWith('FOCUS_TEXTAREA', {channelId: CHANNEL_ID});
	});

	it('is what the Tab bound chat_focus_textarea action runs', () => {
		const handlers = readFileSync(
			fileURLToPath(new URL('../../app/keybindings/keybind_manager/handlers/defaultHandlers.ts', import.meta.url)),
			'utf8',
		);
		const handler = handlers.match(/host\.register\('chat_focus_textarea'[\s\S]*?\n\t\}\);/)?.[0];
		expect(handler).toBeDefined();
		expect(handler).toMatch(/focusChannelTextareaFromKeybind\(channelId\)/);
		expect(handler).not.toMatch(/ComponentBus\.dispatch/);
	});
});
