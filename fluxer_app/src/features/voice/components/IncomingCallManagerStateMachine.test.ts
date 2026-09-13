// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createIncomingCallManagerSnapshot,
	type IncomingCallManagerSignals,
	resolveIncomingCallQueue,
	resolveIncomingRingCommand,
	selectIncomingCallManagerModel,
	shouldPlayIncomingRing,
	transitionIncomingCallManagerSnapshot,
} from '@app/features/voice/components/IncomingCallManagerStateMachine';
import {describe, expect, it} from 'vitest';

function signals(overrides: Partial<IncomingCallManagerSignals> = {}): IncomingCallManagerSignals {
	return {
		incomingCallIds: [],
		hasRingingCalls: false,
		isVoiceConnected: false,
		isVoiceConnecting: false,
		...overrides,
	};
}

describe('IncomingCallManagerStateMachine', () => {
	it('retains active queued calls before appending new incoming calls', () => {
		expect(resolveIncomingCallQueue(['b', 'a'], ['a', 'b', 'c'])).toEqual(['b', 'a', 'c']);
	});

	it('drops ended calls from the queue', () => {
		expect(resolveIncomingCallQueue(['a', 'b', 'c'], ['c'])).toEqual(['c']);
	});

	it('selects the first retained queued call as active', () => {
		let snapshot = createIncomingCallManagerSnapshot();
		snapshot = transitionIncomingCallManagerSnapshot(snapshot, {
			type: 'incomingCalls.update',
			signals: signals({incomingCallIds: ['a', 'b'], hasRingingCalls: true}),
		});
		snapshot = transitionIncomingCallManagerSnapshot(snapshot, {
			type: 'incomingCalls.update',
			signals: signals({incomingCallIds: ['b', 'c'], hasRingingCalls: true}),
		});

		expect(selectIncomingCallManagerModel(snapshot)).toMatchObject({
			callQueue: ['b', 'c'],
			activeCallId: 'b',
			shouldPlayIncomingRing: true,
		});
	});

	it('rings only while there are ringing calls and voice is not joining or connected', () => {
		expect(shouldPlayIncomingRing(signals({hasRingingCalls: true}))).toBe(true);
		expect(shouldPlayIncomingRing(signals({hasRingingCalls: true, isVoiceConnected: true}))).toBe(false);
		expect(shouldPlayIncomingRing(signals({hasRingingCalls: true, isVoiceConnecting: true}))).toBe(false);
		expect(shouldPlayIncomingRing(signals({hasRingingCalls: false}))).toBe(false);
	});

	it('starts the ring whenever ringing is wanted, enabled, and not already active', () => {
		expect(resolveIncomingRingCommand({shouldPlayIncomingRing: true, ringSoundEnabled: true, ringActive: false})).toBe(
			'start',
		);
	});

	it('restarts the ring when the ring sound is re-enabled mid-call', () => {
		expect(resolveIncomingRingCommand({shouldPlayIncomingRing: true, ringSoundEnabled: false, ringActive: false})).toBe(
			'none',
		);
		expect(resolveIncomingRingCommand({shouldPlayIncomingRing: true, ringSoundEnabled: true, ringActive: false})).toBe(
			'start',
		);
	});

	it('does not restart a ring that is already active', () => {
		expect(resolveIncomingRingCommand({shouldPlayIncomingRing: true, ringSoundEnabled: true, ringActive: true})).toBe(
			'none',
		);
	});

	it('stops the ring when ringing is no longer wanted', () => {
		expect(resolveIncomingRingCommand({shouldPlayIncomingRing: false, ringSoundEnabled: true, ringActive: true})).toBe(
			'stop',
		);
		expect(resolveIncomingRingCommand({shouldPlayIncomingRing: false, ringSoundEnabled: false, ringActive: true})).toBe(
			'stop',
		);
	});

	it('does nothing while idle without ringing calls', () => {
		expect(resolveIncomingRingCommand({shouldPlayIncomingRing: false, ringSoundEnabled: true, ringActive: false})).toBe(
			'none',
		);
	});
});
