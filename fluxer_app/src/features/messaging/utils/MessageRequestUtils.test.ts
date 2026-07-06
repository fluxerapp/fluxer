// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PollRequest} from '@fluxer/schema/src/domains/message/PollSchemas';
import {beforeAll, describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/messaging/state/ChatInputSettings', () => ({
	default: {convertEmoticons: false},
}));
vi.mock('@app/features/messaging/utils/EmoticonConversionUtils', () => ({
	convertEmoticonsToEmoji: (content: string) => content,
}));
vi.mock('@app/features/messaging/utils/UrlSanitizationUtils', () => ({
	maybeSanitizeOutgoingMessage: (content: string) => content,
}));

let buildMessageCreateRequest: typeof import('./MessageRequestUtils').buildMessageCreateRequest;

beforeAll(async () => {
	({buildMessageCreateRequest} = await import('./MessageRequestUtils'));
});

describe('buildMessageCreateRequest', () => {
	it('includes poll payloads without requiring text content', () => {
		const poll: PollRequest = {
			title: 'Lunch?',
			options: [{text: 'Pizza'}, {text: 'Sushi'}],
			duration_seconds: 3600,
			anonymous: false,
			allow_ranked_choice: false,
			allow_custom_answers: true,
		};

		expect(buildMessageCreateRequest({content: '', nonce: '123', poll})).toEqual({
			nonce: '123',
			poll,
		});
	});

	it('preserves poll option attachment references alongside message attachments', () => {
		const poll: PollRequest = {
			title: 'Pick a design',
			options: [{text: 'One', attachment_id: 0}, {text: 'Two'}],
			duration_seconds: 3600,
			anonymous: false,
			allow_ranked_choice: false,
			allow_custom_answers: false,
		};
		const attachments = [
			{id: '0', filename: 'one.png', title: 'one.png'},
			{id: '1', filename: 'two.png', title: 'two.png'},
		];

		expect(buildMessageCreateRequest({content: '', nonce: '123', attachments, poll})).toEqual({
			nonce: '123',
			attachments,
			poll,
		});
	});
});
