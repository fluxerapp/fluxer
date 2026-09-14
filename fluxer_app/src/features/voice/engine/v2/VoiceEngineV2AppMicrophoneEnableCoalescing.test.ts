// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {installVoiceMenuTestBootstrap} from '@app/features/ui/action_menu/items/__fixtures__/VoiceMenuTestBootstrap';
import type {Room} from 'livekit-client';
import {expect, test, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});
vi.mock('@lingui/react/macro', () => ({
	Trans: () => null,
	useLingui: () => ({i18n: {_: (descriptor: {message?: string}) => descriptor.message ?? '', locale: 'en'}}),
}));

installVoiceMenuTestBootstrap();

await import('@app/features/voice/engine/MediaEngineFacade');
const {VoiceEngineV2AppMediaExecutionAdapter} = await import(
	'@app/features/voice/engine/v2/VoiceEngineV2AppMediaExecutionAdapter'
);

interface FakeRoom {
	audioTrackPublications: Map<string, unknown>;
	localParticipant: {audioTrackPublications: Map<string, unknown>};
}

function createFakeRoom(): FakeRoom {
	const audioTrackPublications = new Map<string, unknown>();
	return {audioTrackPublications, localParticipant: {audioTrackPublications}};
}

function publishMicrophone(room: FakeRoom): void {
	room.audioTrackPublications.set('mic', {
		source: 'microphone',
		track: {mediaStreamTrack: {readyState: 'live'}},
	});
}

function unpublishMicrophone(room: FakeRoom): void {
	room.audioTrackPublications.delete('mic');
}

function asRoom(room: FakeRoom): Room {
	return room as unknown as Room;
}

function createDeferred(): {promise: Promise<void>; resolve: () => void} {
	let resolve = (): void => undefined;
	const promise = new Promise<void>((res) => {
		resolve = () => res();
	});
	return {promise, resolve};
}

function createAdapter(onEnable: (room: FakeRoom, channelId: string | null) => Promise<void>): {
	adapter: InstanceType<typeof VoiceEngineV2AppMediaExecutionAdapter>;
	calls: Array<{room: FakeRoom; channelId: string | null}>;
} {
	const adapter = new VoiceEngineV2AppMediaExecutionAdapter();
	const calls: Array<{room: FakeRoom; channelId: string | null}> = [];
	(
		adapter as unknown as {enableMicrophoneNow: (room: Room, channelId: string | null) => Promise<void>}
	).enableMicrophoneNow = async (room: Room, channelId: string | null): Promise<void> => {
		const fakeRoom = room as unknown as FakeRoom;
		calls.push({room: fakeRoom, channelId});
		await onEnable(fakeRoom, channelId);
	};
	return {adapter, calls};
}

test('an enable for a second room is not satisfied by an enable in flight for the first room', async () => {
	const roomA = createFakeRoom();
	const roomB = createFakeRoom();
	const gate = createDeferred();
	const {adapter, calls} = createAdapter(async (room) => {
		await gate.promise;
		publishMicrophone(room);
	});

	const first = adapter.enableMicrophone(asRoom(roomA), 'channel-a');
	await Promise.resolve();
	const second = adapter.enableMicrophone(asRoom(roomB), 'channel-b');
	gate.resolve();
	await Promise.all([first, second]);

	expect(calls.map((call) => call.channelId)).toEqual(['channel-a', 'channel-b']);
	expect(roomA.audioTrackPublications.size).toBe(1);
	expect(roomB.audioTrackPublications.size).toBe(1);
});

test('an enable is not satisfied by an in-flight enable whose publication was already unpublished', async () => {
	const room = createFakeRoom();
	const gate = createDeferred();
	const {adapter, calls} = createAdapter(async (target) => {
		publishMicrophone(target);
		await gate.promise;
	});

	const first = adapter.enableMicrophone(asRoom(room), 'channel-a');
	await Promise.resolve();
	unpublishMicrophone(room);
	const second = adapter.enableMicrophone(asRoom(room), 'channel-a');
	gate.resolve();
	await Promise.all([first, second]);

	expect(calls).toHaveLength(2);
	expect(room.audioTrackPublications.size).toBe(1);
});

test('a coalesced enable that already has a live publication does not enable again', async () => {
	const room = createFakeRoom();
	const gate = createDeferred();
	const {adapter, calls} = createAdapter(async (target) => {
		publishMicrophone(target);
		await gate.promise;
	});

	const first = adapter.enableMicrophone(asRoom(room), 'channel-a');
	await Promise.resolve();
	const second = adapter.enableMicrophone(asRoom(room), 'channel-a');
	gate.resolve();
	await Promise.all([first, second]);

	expect(calls).toHaveLength(1);
	expect(room.audioTrackPublications.size).toBe(1);
});

test('a rejection from the in-flight enable still propagates to the coalesced caller', async () => {
	const room = createFakeRoom();
	const gate = createDeferred();
	const {adapter, calls} = createAdapter(async () => {
		await gate.promise;
		throw new Error('enable failed');
	});

	const first = adapter.enableMicrophone(asRoom(room), 'channel-a');
	await Promise.resolve();
	const second = adapter.enableMicrophone(asRoom(room), 'channel-a');
	gate.resolve();

	await expect(first).rejects.toThrow('enable failed');
	await expect(second).rejects.toThrow('enable failed');
	expect(calls).toHaveLength(1);
});
