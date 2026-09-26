// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessagePollEmoji} from '../database/types/PollTypes';

export class PollEmoji {
	readonly id: string | null;
	readonly name: string | null;

	constructor(emoji: MessagePollEmoji) {
		this.id = emoji.id ?? null;
		this.name = emoji.name ?? null;
	}

	toMessagePollEmoji(): MessagePollEmoji {
		return {
			id: this.id,
			name: this.name,
		};
	}
}
