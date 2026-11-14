// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CacheDatabase} from '@electron/engine/CacheDatabase';
import type {AccountEngineMention} from '@fluxer/desktop_ipc/src/AccountEngineContract';
import {isRecord} from '@fluxer/record_utils/src/RecordGuards';

const MENTION_LOG_KEEP = 200;

function mentionCreatedAt(mention: AccountEngineMention): number {
	const parsed = Date.parse(mention.timestamp);
	return Number.isFinite(parsed) ? parsed : 0;
}

export class MentionLog {
	private readonly db: CacheDatabase;

	constructor(db: CacheDatabase) {
		this.db = db;
	}

	record(accountKey: string, mention: AccountEngineMention): void {
		this.db.insertMention(
			accountKey,
			mention.messageId,
			mentionCreatedAt(mention),
			JSON.stringify(mention),
			MENTION_LOG_KEEP,
		);
	}

	list(accountKey: string, limit: number): Array<AccountEngineMention> {
		if (limit <= 0) {
			return [];
		}
		const mentions: Array<AccountEngineMention> = [];
		for (const json of this.db.listMentions(accountKey, Math.min(limit, MENTION_LOG_KEEP))) {
			let value: unknown;
			try {
				value = JSON.parse(json);
			} catch (error) {
				if (error instanceof SyntaxError) {
					continue;
				}
				throw error;
			}
			if (!isRecord(value) || !isRecord(value.author)) {
				continue;
			}
			const {messageId, channelId, guildId, channelType, channelName, guildName, author, content, gated, timestamp} =
				value;
			const {id, username, globalName, avatar, discriminator} = author;
			if (
				typeof messageId !== 'string' ||
				typeof channelId !== 'string' ||
				(guildId !== null && typeof guildId !== 'string') ||
				(channelType !== null && typeof channelType !== 'number') ||
				(channelName !== null && typeof channelName !== 'string') ||
				(guildName !== null && typeof guildName !== 'string') ||
				typeof id !== 'string' ||
				typeof username !== 'string' ||
				(globalName !== null && typeof globalName !== 'string') ||
				(avatar !== null && typeof avatar !== 'string') ||
				(discriminator !== null && typeof discriminator !== 'string') ||
				typeof content !== 'string' ||
				(gated !== undefined && typeof gated !== 'boolean') ||
				typeof timestamp !== 'string'
			) {
				continue;
			}
			mentions.push({
				...value,
				messageId,
				channelId,
				guildId,
				channelType,
				channelName,
				guildName,
				author: {...author, id, username, globalName, avatar, discriminator},
				content,
				gated: gated ?? false,
				timestamp,
			});
		}
		return mentions;
	}
}
