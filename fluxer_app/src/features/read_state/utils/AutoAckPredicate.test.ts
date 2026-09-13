// SPDX-License-Identifier: AGPL-3.0-or-later

import {shouldAutoAck} from '@app/features/read_state/utils/AutoAckPredicate';
import {describe, expect, it} from 'vitest';

function conditions(overrides: Partial<Parameters<typeof shouldAutoAck>[0]> = {}): Parameters<typeof shouldAutoAck>[0] {
	return {
		channelActive: true,
		windowFocused: true,
		atBottom: true,
		textChatVisible: true,
		manualAck: false,
		blockingModalOpen: false,
		...overrides,
	};
}

describe('shouldAutoAck', () => {
	it('returns true when all positive conditions hold and nothing blocks', () => {
		expect(shouldAutoAck(conditions())).toBe(true);
	});

	it('returns false when a blocking modal is open', () => {
		expect(shouldAutoAck(conditions({blockingModalOpen: true}))).toBe(false);
	});

	it('returns false when not at the bottom', () => {
		expect(shouldAutoAck(conditions({atBottom: false}))).toBe(false);
	});

	it('returns false during manual ack', () => {
		expect(shouldAutoAck(conditions({manualAck: true}))).toBe(false);
	});

	it('returns false when the window is not focused', () => {
		expect(shouldAutoAck(conditions({windowFocused: false}))).toBe(false);
	});

	it('returns false when the text chat is not visible', () => {
		expect(shouldAutoAck(conditions({textChatVisible: false}))).toBe(false);
	});

	it('returns false when the channel is not active', () => {
		expect(shouldAutoAck(conditions({channelActive: false}))).toBe(false);
	});
});
