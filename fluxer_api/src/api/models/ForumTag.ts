// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ForumTagUdt} from '@app/api/database/types/ThreadTypes';

export class ForumTag {
	readonly id: bigint;
	readonly name: string;
	readonly moderated: boolean;
	readonly emojiId: bigint | null;
	readonly emojiName: string | null;

	constructor(tag: ForumTagUdt) {
		this.id = tag.id;
		this.name = tag.name;
		this.moderated = tag.moderated ?? false;
		this.emojiId = tag.emoji_id ?? null;
		this.emojiName = tag.emoji_name ?? null;
	}

	toUdt(): ForumTagUdt {
		return {
			id: this.id,
			name: this.name,
			moderated: this.moderated,
			emoji_id: this.emojiId,
			emoji_name: this.emojiName,
		};
	}
}
