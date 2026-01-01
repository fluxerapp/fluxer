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

import type {CustomStatus} from '~/database/CassandraTypes';
import type {EmojiID} from '../BrandedTypes';

export class UserCustomStatus {
	readonly text: string | null;
	readonly emojiId: EmojiID | null;
	readonly emojiName: string | null;
	readonly emojiAnimated: boolean;
	readonly expiresAt: Date | null;

	constructor(status: CustomStatus) {
		this.text = status.text ?? null;
		this.emojiId = status.emoji_id ?? null;
		this.emojiName = status.emoji_name ?? null;
		this.emojiAnimated = status.emoji_animated ?? false;
		this.expiresAt = status.expires_at ?? null;
	}

	toCustomStatus(): CustomStatus {
		return {
			text: this.text,
			emoji_id: this.emojiId,
			emoji_name: this.emojiName,
			emoji_animated: this.emojiAnimated,
			expires_at: this.expiresAt,
		};
	}

	isExpired(referenceTime: Date = new Date()): boolean {
		return this.expiresAt !== null && this.expiresAt.getTime() <= referenceTime.getTime();
	}
}
