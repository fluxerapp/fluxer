// SPDX-License-Identifier: AGPL-3.0-or-later

import {maskThreadArtifacts} from '@app/api/channel/services/message/ThreadMessageResponses';
import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {ServerMessageFlags} from '@fluxer/constants/src/ThreadConstants';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {describe, expect, it} from 'vitest';

function message(overrides: Partial<MessageResponse>): MessageResponse {
	return {
		id: '10',
		channel_id: '1',
		author: {
			id: '2',
			username: 'a',
			discriminator: '0001',
			global_name: null,
			avatar: null,
			avatar_color: null,
			flags: 0,
		},
		type: MessageTypes.DEFAULT,
		flags: 0,
		content: '',
		timestamp: new Date(0).toISOString(),
		pinned: false,
		mention_everyone: false,
		tts: false,
		mentions: [],
		mention_roles: [],
		...overrides,
	};
}

describe('thread artifact masking', () => {
	it('drops thread created messages and clears thread bits including nested ones', () => {
		const masked = maskThreadArtifacts([
			message({id: '1', type: MessageTypes.THREAD_CREATED}),
			message({
				id: '2',
				flags: ServerMessageFlags.HAS_THREAD,
				referenced_message: message({id: '3', flags: ServerMessageFlags.FAILED_TO_MENTION_SOME_ROLES_IN_THREAD}),
			}),
			message({id: '4', referenced_message: message({id: '5', type: MessageTypes.THREAD_CREATED})}),
		]);
		expect(masked.map((m) => m.id)).toEqual(['2', '4']);
		expect(masked[0]?.flags).toBe(0);
		expect(masked[0]?.referenced_message?.flags).toBe(0);
		expect(masked[1]?.referenced_message).toBeNull();
	});
});
