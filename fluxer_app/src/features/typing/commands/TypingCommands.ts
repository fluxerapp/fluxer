// SPDX-License-Identifier: AGPL-3.0-or-later

import * as LegacyTypingCommands from '@app/features/typing/legacy/LegacyTypingCommands';
import RollingTypingStore from '@app/features/typing/rolling/RollingTypingStore';
import TypingPolicy from '@app/features/typing/state/TypingPolicy';

export function startTyping(channelId: string, userId: string): void {
	if (TypingPolicy.active === 'rolling') {
		RollingTypingStore.start(channelId, userId, 'gateway');
		return;
	}
	LegacyTypingCommands.startTyping(channelId, userId);
}
