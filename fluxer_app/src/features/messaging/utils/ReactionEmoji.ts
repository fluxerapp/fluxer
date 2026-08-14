// SPDX-License-Identifier: AGPL-3.0-or-later

export interface ReactionEmoji {
	id?: string | null;
	name: string;
	animated?: boolean;
	url?: string | null;
	uniqueName?: string;
}

export function getReactionKey(messageId: string, emoji: ReactionEmoji): string {
	let emojiId = '';
	if (emoji.id != null) {
		emojiId = emoji.id;
	}
	return `${messageId}:${emoji.name}:${emojiId}`;
}
