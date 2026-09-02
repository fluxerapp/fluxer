// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import EntranceSoundPlaybackEngine from '@app/features/voice/engine/EntranceSoundPlaybackEngine';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {waitForVoiceJoinChimeSequence} from '@app/features/voice/engine/VoiceSelfJoinChime';
import {beforeEach, describe, expect, it, vi} from 'vitest';
import {handleEntranceSoundPlay} from './EntranceSoundPlay';

vi.mock('@app/features/voice/engine/EntranceSoundPlaybackEngine', () => ({
	default: {play: vi.fn(() => Promise.resolve())},
}));

vi.mock('@app/features/voice/engine/MediaEngineFacade', () => ({
	default: {connected: true, channelId: 'channel-1'},
}));

vi.mock('@app/features/voice/engine/VoiceSelfJoinChime', () => ({
	waitForVoiceJoinChimeSequence: vi.fn(),
}));

const payload = {
	user_id: 'user-1',
	channel_id: 'channel-1',
	guild_id: 'guild-1',
	sound_id: 'sound-1',
	hash: 'hash-1',
	url: 'https://example.invalid/entrance.mp3',
	duration_ms: 1200,
	content_type: 'audio/mpeg',
};

const context = {} as GatewayHandlerContext;

describe('handleEntranceSoundPlay', () => {
	beforeEach(() => {
		vi.mocked(EntranceSoundPlaybackEngine.play).mockClear();
		vi.mocked(waitForVoiceJoinChimeSequence).mockReset();
	});

	it('plays the entrance sound after the join chime started', async () => {
		vi.mocked(waitForVoiceJoinChimeSequence).mockResolvedValue('started');
		handleEntranceSoundPlay(payload, context);
		await vi.waitFor(() => expect(EntranceSoundPlaybackEngine.play).toHaveBeenCalledTimes(1));
		expect(waitForVoiceJoinChimeSequence).toHaveBeenCalledWith({userId: 'user-1', channelId: 'channel-1'});
	});

	it('plays the entrance sound when the join chime was unavailable', async () => {
		vi.mocked(waitForVoiceJoinChimeSequence).mockResolvedValue('unavailable');
		handleEntranceSoundPlay(payload, context);
		await vi.waitFor(() => expect(EntranceSoundPlaybackEngine.play).toHaveBeenCalledTimes(1));
	});

	it('drops the entrance sound when the join chime expired before starting', async () => {
		vi.mocked(waitForVoiceJoinChimeSequence).mockResolvedValue('expired-before-start');
		handleEntranceSoundPlay(payload, context);
		await Promise.resolve();
		await Promise.resolve();
		expect(EntranceSoundPlaybackEngine.play).not.toHaveBeenCalled();
	});

	it('drops the entrance sound when the channel changed while waiting', async () => {
		vi.mocked(waitForVoiceJoinChimeSequence).mockImplementation(() =>
			Promise.resolve().then(() => {
				vi.mocked(MediaEngine).channelId = 'channel-2';
				return 'started' as const;
			}),
		);
		handleEntranceSoundPlay(payload, context);
		await Promise.resolve();
		await Promise.resolve();
		await Promise.resolve();
		expect(EntranceSoundPlaybackEngine.play).not.toHaveBeenCalled();
		vi.mocked(MediaEngine).channelId = 'channel-1';
	});
});
