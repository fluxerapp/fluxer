// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {type ChannelID, createChannelID, createMessageID, createUserID, type MessageID} from '../../BrandedTypes';
import {harvestMessages} from './HarvestUserData';

const AUTHOR = createUserID(1000000000000000000n);

function makeRepository(refs: Array<{channelId: ChannelID; messageId: MessageID}>, missing = new Set<string>()) {
	let pages = 0;
	return {
		pages: () => pages,
		listMessagesByAuthor: async (_userId: typeof AUTHOR, limit: number, lastMessageId?: MessageID) => {
			pages++;
			const start = lastMessageId ? refs.findIndex((r) => r.messageId === lastMessageId) + 1 : 0;
			return refs.slice(start, start + limit);
		},
		getMessage: async (_channelId: ChannelID, messageId: MessageID) =>
			missing.has(messageId.toString()) ? null : {content: `body ${messageId.toString()}`, attachments: undefined},
	};
}

function refsAcross(channelCount: number, perChannel: number) {
	const out: Array<{channelId: ChannelID; messageId: MessageID}> = [];
	let id = 1500000000000000000n;
	for (let c = 0; c < channelCount; c++) {
		const channelId = createChannelID(2000000000000000000n + BigInt(c));
		for (let m = 0; m < perChannel; m++) {
			out.push({channelId, messageId: createMessageID(id)});
			id += 1n;
		}
	}
	return out;
}

describe('harvestMessages', () => {
	it('reads past a single page instead of stopping at one query', async () => {
		const refs = refsAcross(1, 2500);
		const repo = makeRepository(refs);
		const result = await harvestMessages(repo, AUTHOR, Date.now(), null);
		expect(result.totalMessages).toBe(2500);
		expect(repo.pages()).toBeGreaterThan(1);
	});

	it('groups every message under its own channel', async () => {
		const repo = makeRepository(refsAcross(3, 4));
		const result = await harvestMessages(repo, AUTHOR, Date.now(), null);
		expect(result.channelMessagesMap.size).toBe(3);
		for (const messages of result.channelMessagesMap.values()) {
			expect(messages).toHaveLength(4);
		}
		expect(result.totalMessages).toBe(12);
	});

	it('leaves out a message the repository cannot return', async () => {
		const refs = refsAcross(1, 5);
		const repo = makeRepository(refs, new Set([refs[2].messageId.toString()]));
		const result = await harvestMessages(repo, AUTHOR, Date.now(), null);
		expect(result.totalMessages).toBe(4);
	});

	it('returns an empty map for an account with no messages', async () => {
		const repo = makeRepository([]);
		const result = await harvestMessages(repo, AUTHOR, Date.now(), null);
		expect(result.totalMessages).toBe(0);
		expect(result.channelMessagesMap.size).toBe(0);
	});
});
