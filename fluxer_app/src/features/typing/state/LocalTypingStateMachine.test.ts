// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createLocalTypingSnapshot,
	LOCAL_TYPING_REMOTE_REFRESH_MS,
	type LocalTypingMachineEvent,
	type LocalTypingSnapshot,
	selectLocalTypingModel,
	transitionLocalTypingSnapshot,
} from '@app/features/typing/state/LocalTypingStateMachine';
import {describe, expect, it} from 'vitest';

function transition(snapshot: LocalTypingSnapshot, event: LocalTypingMachineEvent): LocalTypingSnapshot {
	return transitionLocalTypingSnapshot(snapshot, event);
}

describe('localTypingStateMachine', () => {
	it('shows local typing immediately and schedules the first remote send', () => {
		const snapshot = transition(createLocalTypingSnapshot(), {
			type: 'localTyping.started',
			channelId: 'channel-1',
			now: 1000,
		});

		expect(selectLocalTypingModel(snapshot)).toMatchObject({
			channelId: 'channel-1',
			localTyping: true,
			remotePending: true,
			remotePendingVersion: 1,
		});
	});

	it('cancels local typing and an unsent remote indicator when typing stops', () => {
		let snapshot = transition(createLocalTypingSnapshot(), {
			type: 'localTyping.started',
			channelId: 'channel-1',
			now: 1000,
		});
		snapshot = transition(snapshot, {
			type: 'localTyping.stopped',
			channelId: 'channel-1',
		});

		expect(selectLocalTypingModel(snapshot)).toMatchObject({
			channelId: null,
			localTyping: false,
			remotePending: false,
			remotePendingVersion: 1,
		});
	});

	it('keeps remote cooldown after local typing stops', () => {
		let snapshot = transition(createLocalTypingSnapshot(), {
			type: 'localTyping.started',
			channelId: 'channel-1',
			now: 1000,
		});
		snapshot = transition(snapshot, {
			type: 'localTyping.remoteSent',
			channelId: 'channel-1',
			now: 2500,
			pendingVersion: 1,
		});
		snapshot = transition(snapshot, {
			type: 'localTyping.stopped',
			channelId: 'channel-1',
		});

		expect(selectLocalTypingModel(snapshot)).toMatchObject({
			channelId: null,
			localTyping: false,
			remotePending: false,
			remoteCooldownChannelId: 'channel-1',
			remoteCooldownUntil: 2500 + LOCAL_TYPING_REMOTE_REFRESH_MS,
		});
	});

	it('shows local typing during remote cooldown without scheduling another send', () => {
		let snapshot = createLocalTypingSnapshot({
			remoteCooldownChannelId: 'channel-1',
			remoteCooldownUntil: 10000,
		});
		snapshot = transition(snapshot, {
			type: 'localTyping.started',
			channelId: 'channel-1',
			now: 5000,
		});

		expect(selectLocalTypingModel(snapshot)).toMatchObject({
			channelId: 'channel-1',
			localTyping: true,
			remotePending: false,
			remotePendingVersion: 0,
		});
	});

	it('schedules a new remote send after cooldown has elapsed', () => {
		let snapshot = createLocalTypingSnapshot({
			remoteCooldownChannelId: 'channel-1',
			remoteCooldownUntil: 10000,
		});
		snapshot = transition(snapshot, {
			type: 'localTyping.started',
			channelId: 'channel-1',
			now: 10000,
		});

		expect(selectLocalTypingModel(snapshot)).toMatchObject({
			channelId: 'channel-1',
			localTyping: true,
			remotePending: true,
			remotePendingVersion: 1,
		});
	});

	it('bumps the pending version while input remains active so the debounce can restart', () => {
		let snapshot = transition(createLocalTypingSnapshot(), {
			type: 'localTyping.started',
			channelId: 'channel-1',
			now: 1000,
		});
		snapshot = transition(snapshot, {
			type: 'localTyping.started',
			channelId: 'channel-1',
			now: 1200,
		});

		expect(selectLocalTypingModel(snapshot)).toMatchObject({
			channelId: 'channel-1',
			localTyping: true,
			remotePending: true,
			remotePendingVersion: 2,
		});
	});

	it('treats a channel switch as a fresh local session', () => {
		let snapshot = transition(createLocalTypingSnapshot(), {
			type: 'localTyping.started',
			channelId: 'channel-1',
			now: 1000,
		});
		snapshot = transition(snapshot, {
			type: 'localTyping.remoteSent',
			channelId: 'channel-1',
			now: 2500,
			pendingVersion: 1,
		});
		snapshot = transition(snapshot, {
			type: 'localTyping.started',
			channelId: 'channel-2',
			now: 3000,
		});

		expect(selectLocalTypingModel(snapshot)).toMatchObject({
			channelId: 'channel-2',
			localTyping: true,
			remotePending: true,
			remotePendingVersion: 2,
		});
	});
});
