// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessagePollMedia} from '../database/types/PollTypes';
import {PollEmoji} from './PollEmoji';

export class PollMedia {
	readonly emoji: PollEmoji | null;
	readonly text: string | null;

	constructor(media: MessagePollMedia) {
		this.emoji = media.emoji ? new PollEmoji(media.emoji) : null;
		this.text = media.text ?? null;
	}

	toMessagePollMedia(): MessagePollMedia {
		return {
			emoji: this.emoji?.toMessagePollEmoji() ?? null,
			text: this.text,
		};
	}
}
