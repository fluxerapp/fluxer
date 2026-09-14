// SPDX-License-Identifier: AGPL-3.0-or-later

import LegacyTypingIndicator from '@app/features/typing/legacy/LegacyTypingIndicator';
import RollingTypingStore from '@app/features/typing/rolling/RollingTypingStore';
import TypingPolicy from '@app/features/typing/state/TypingPolicy';
import type {Message} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

class TypingIndicator {
	startRemoteTyping(channelId: string, userId: string): void {
		if (TypingPolicy.active === 'rolling') {
			RollingTypingStore.start(channelId, userId, 'gateway');
			return;
		}
		LegacyTypingIndicator.startRemoteTyping(channelId, userId);
	}

	stopTypingOnMessageCreate(message: Message): void {
		if (TypingPolicy.active === 'rolling') {
			RollingTypingStore.remove(message.channel_id, message.author.id);
			return;
		}
		LegacyTypingIndicator.stopTypingOnMessageCreate(message);
	}

	reset(): void {
		if (TypingPolicy.active === 'rolling') {
			RollingTypingStore.reset();
			return;
		}
		LegacyTypingIndicator.reset();
	}

	isTyping(channelId: string, userId: string): boolean {
		if (TypingPolicy.active === 'rolling') {
			return RollingTypingStore.isTyping(channelId, userId);
		}
		return LegacyTypingIndicator.isTyping(channelId, userId);
	}

	isMemberListTyping(channelId: string, userId: string, currentUserId: string | null | undefined): boolean {
		if (TypingPolicy.active !== 'rolling') {
			return LegacyTypingIndicator.isMemberListTyping(channelId, userId, currentUserId);
		}
		if (currentUserId && userId === currentUserId) {
			return RollingTypingStore.isConfirmedTyping(channelId, userId);
		}
		return RollingTypingStore.isTyping(channelId, userId);
	}
}

export default new TypingIndicator();
