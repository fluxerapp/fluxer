// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type AutoAckWindowConditions,
	createAutoAckWindowSnapshot,
	selectAutoAckWindowCommands,
	transitionAutoAckWindowSnapshot,
} from '@app/features/notification/state/NotificationAutoAckStateMachine';
import {describe, expect, it} from 'vitest';

function conditions(overrides: Partial<AutoAckWindowConditions> = {}): AutoAckWindowConditions {
	return {
		channelId: null,
		isAtBottom: false,
		canAutoAck: false,
		...overrides,
	};
}

function transition(overrides: Partial<AutoAckWindowConditions>) {
	return transitionAutoAckWindowSnapshot(createAutoAckWindowSnapshot(), {
		type: 'autoAck.conditionsChanged',
		conditions: conditions(overrides),
	});
}

describe('autoAckWindowStateMachine', () => {
	it('does not emit commands when there is no selected channel', () => {
		const snapshot = transition({});
		expect(snapshot.value).toBe('noChannel');
		expect(selectAutoAckWindowCommands(snapshot)).toEqual([]);
	});

	it('enables automatic ack only when the channel is visible at bottom', () => {
		const snapshot = transition({channelId: 'channel-1', isAtBottom: true, canAutoAck: true});

		expect(snapshot.value).toBe('enabled');
		expect(selectAutoAckWindowCommands(snapshot)).toEqual([{type: 'enable', channelId: 'channel-1'}]);
	});

	it('disables the current channel when auto-ack conditions fail', () => {
		const snapshot = transition({channelId: 'channel-1', isAtBottom: false, canAutoAck: true});

		expect(snapshot.value).toBe('disabled');
		expect(selectAutoAckWindowCommands(snapshot)).toEqual([{type: 'disable', channelId: 'channel-1'}]);
	});

	it('disables the previous channel before enabling a new channel', () => {
		const firstSnapshot = transition({channelId: 'channel-1', isAtBottom: true, canAutoAck: true});
		const secondSnapshot = transitionAutoAckWindowSnapshot(firstSnapshot, {
			type: 'autoAck.conditionsChanged',
			conditions: conditions({channelId: 'channel-2', isAtBottom: true, canAutoAck: true}),
		});

		expect(secondSnapshot.value).toBe('enabled');
		expect(selectAutoAckWindowCommands(secondSnapshot)).toEqual([
			{type: 'disable', channelId: 'channel-1'},
			{type: 'enable', channelId: 'channel-2'},
		]);
	});

	it('disables the previous channel when selection clears', () => {
		const firstSnapshot = transition({channelId: 'channel-1', isAtBottom: true, canAutoAck: true});
		const secondSnapshot = transitionAutoAckWindowSnapshot(firstSnapshot, {
			type: 'autoAck.conditionsChanged',
			conditions: conditions({channelId: null}),
		});

		expect(secondSnapshot.value).toBe('noChannel');
		expect(selectAutoAckWindowCommands(secondSnapshot)).toEqual([{type: 'disable', channelId: 'channel-1'}]);
	});

	it('disables automatic ack while the media viewer suppresses auto-ack', () => {
		const enabledSnapshot = transition({channelId: 'channel-1', isAtBottom: true, canAutoAck: true});
		const viewerOpenSnapshot = transitionAutoAckWindowSnapshot(enabledSnapshot, {
			type: 'autoAck.conditionsChanged',
			conditions: conditions({channelId: 'channel-1', isAtBottom: true, canAutoAck: false}),
		});

		expect(viewerOpenSnapshot.value).toBe('disabled');
		expect(selectAutoAckWindowCommands(viewerOpenSnapshot)).toEqual([{type: 'disable', channelId: 'channel-1'}]);
	});

	it('re-enables automatic ack at the bottom once the media viewer no longer suppresses it', () => {
		const viewerOpenSnapshot = transition({channelId: 'channel-1', isAtBottom: true, canAutoAck: false});
		const viewerClosedSnapshot = transitionAutoAckWindowSnapshot(viewerOpenSnapshot, {
			type: 'autoAck.conditionsChanged',
			conditions: conditions({channelId: 'channel-1', isAtBottom: true, canAutoAck: true}),
		});

		expect(viewerClosedSnapshot.value).toBe('enabled');
		expect(selectAutoAckWindowCommands(viewerClosedSnapshot)).toEqual([{type: 'enable', channelId: 'channel-1'}]);
	});
});
