/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Config} from '~/Config';
import type {IAssetDeletionQueue} from '~/infrastructure/IAssetDeletionQueue';

const STICKER_EXTENSIONS = ['webp', 'gif'];

export class ExpressionAssetPurger {
	constructor(private readonly assetDeletionQueue: IAssetDeletionQueue) {}

	async purgeEmoji(id: string): Promise<void> {
		await this.queueAsset('emojis', id, this.buildEmojiCdnUrls(id));
	}

	async purgeSticker(id: string): Promise<void> {
		await this.queueAsset('stickers', id, this.buildStickerCdnUrls(id));
	}

	private async queueAsset(prefix: string, id: string, cdnUrls: Array<string>): Promise<void> {
		const uniqueUrls = Array.from(new Set(cdnUrls));
		const [primaryUrl, ...additionalUrls] = uniqueUrls;

		await this.assetDeletionQueue.queueDeletion({
			s3Key: `${prefix}/${id}`,
			cdnUrl: primaryUrl ?? null,
			reason: 'asset_purge',
		});

		await Promise.all(additionalUrls.map((url) => this.assetDeletionQueue.queueCloudflarePurge(url)));
	}

	private buildEmojiCdnUrls(id: string): Array<string> {
		const base = Config.endpoints.media;
		return [`${base}/emojis/${id}.webp`, `${base}/emojis/${id}.gif`];
	}

	private buildStickerCdnUrls(id: string): Array<string> {
		const base = Config.endpoints.media;
		return STICKER_EXTENSIONS.map((ext) => `${base}/stickers/${id}.${ext}`);
	}
}
