// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EmojiID} from '@app/api/BrandedTypes';
import type {CustomStatus} from '@app/api/database/types/UserTypes';

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
