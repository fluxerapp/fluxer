// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createReadStateMentionSnapshot,
	type ReadStateMentionInput,
	type ReadStateMentionReason,
	resolveReadStateMention,
	selectReadStateMentionModel,
	transitionReadStateMentionSnapshot,
} from '@app/features/read_state/state/read_states/ReadStateMentionMachine';
import {describe, expect, it} from 'vitest';

function input(overrides: Partial<ReadStateMentionInput> = {}): ReadStateMentionInput {
	return {
		authorBlocked: false,
		hasUserMention: false,
		hasEveryoneMention: false,
		hasRoleMention: false,
		isPrivate: false,
		isMuted: false,
		...overrides,
	};
}

const mentionCases: Array<[Partial<ReadStateMentionInput>, ReadStateMentionReason, boolean]> = [
	[{authorBlocked: true, hasUserMention: true}, 'blocked', false],
	[{hasUserMention: true, hasEveryoneMention: true}, 'user', true],
	[{hasEveryoneMention: true, hasRoleMention: true}, 'everyone', true],
	[{hasRoleMention: true}, 'role', true],
	[{isPrivate: true, isMuted: false}, 'private', true],
	[{isPrivate: true, isMuted: true}, 'none', false],
	[{}, 'none', false],
];

describe('readStateMentionMachine', () => {
	it.each(mentionCases)('routes %# to %s', (overrides, reason, shouldMention) => {
		expect(resolveReadStateMention(input(overrides))).toEqual({reason, shouldMention});
	});

	it('updates the mention reason from later input', () => {
		const noneSnapshot = createReadStateMentionSnapshot(input());
		expect(selectReadStateMentionModel(noneSnapshot)).toEqual({reason: 'none', shouldMention: false});

		const roleSnapshot = transitionReadStateMentionSnapshot(noneSnapshot, {
			type: 'readStateMention.updated',
			input: input({hasRoleMention: true}),
		});

		expect(selectReadStateMentionModel(roleSnapshot)).toEqual({reason: 'role', shouldMention: true});
	});
});
