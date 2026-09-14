// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {Logger} from '@app/api/Logger';
import {fileShaCache} from '@app/api/middleware/FileShaCache';
import {phraseBlocklistCache} from '@app/api/middleware/PhraseBlocklistCache';
import {urlBlocklistCache} from '@app/api/middleware/UrlBlocklistCache';
import {extractUrlCandidates} from '@app/api/utils/UrlNormalizer';
import {ContentBlockedError} from '@fluxer/errors/src/domains/content/ContentBlockedError';

export interface ModerationContext {
	userId: bigint | null;
	guildId: bigint | null;
	channelId: bigint | null;
	messageId: bigint | null;
	surface:
		| 'message_content'
		| 'message_attachment'
		| 'message_embed_unfurl'
		| 'avatar'
		| 'banner'
		| 'guild_icon'
		| 'guild_splash'
		| 'guild_banner'
		| 'emoji'
		| 'sticker'
		| 'app_asset'
		| 'profile_field'
		| 'webhook'
		| 'oauth_redirect';
}

class ContentModerationService {
	scanText(text: string | null | undefined, ctx: ModerationContext): void {
		if (!text) return;
		if (phraseBlocklistCache.containsBannedPhrase(text)) {
			Logger.warn(
				{surface: ctx.surface, userId: ctx.userId?.toString(), guildId: ctx.guildId?.toString()},
				'content_moderation.block phrase match in text',
			);
			throw new ContentBlockedError();
		}
		const urls = extractUrlCandidates(text);
		if (urls.length === 0) return;
		for (const url of urls) {
			if (urlBlocklistCache.isUrlOrDomainBanned(url)) {
				Logger.warn(
					{surface: ctx.surface, userId: ctx.userId?.toString(), guildId: ctx.guildId?.toString()},
					'content_moderation.block url match in text',
				);
				throw new ContentBlockedError();
			}
		}
	}

	scanUrl(url: string, ctx: ModerationContext): void {
		if (!url) return;
		if (urlBlocklistCache.isUrlOrDomainBanned(url)) {
			Logger.warn({surface: ctx.surface, userId: ctx.userId?.toString(), url}, 'content_moderation.block url match');
			throw new ContentBlockedError();
		}
	}

	scanHostname(host: string, ctx: ModerationContext): void {
		if (!host) return;
		if (urlBlocklistCache.isHostnameBanned(host)) {
			Logger.warn(
				{surface: ctx.surface, userId: ctx.userId?.toString(), host},
				'content_moderation.block hostname match',
			);
			throw new ContentBlockedError();
		}
	}

	scanFileBuffer(buffer: Buffer | Uint8Array, ctx: ModerationContext): string {
		const sha = createHash('sha256').update(buffer).digest('hex');
		this.scanSha256(sha, ctx);
		return sha;
	}

	scanSha256(sha: string, ctx: ModerationContext): void {
		if (fileShaCache.isBanned(sha)) {
			Logger.warn(
				{
					surface: ctx.surface,
					userId: ctx.userId?.toString(),
					guildId: ctx.guildId?.toString(),
					sha256: sha,
				},
				'content_moderation.block sha256 match',
			);
			throw new ContentBlockedError();
		}
	}
}

export const contentModerationService = new ContentModerationService();
