// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ReplaceCommandUtils from '@app/features/messaging/utils/ReplaceCommandUtils';
import {TypingUtils as LegacyTypingUtils} from '@app/features/typing/legacy/LegacyTypingUtils';

interface LegacyComposerTypingInput {
	channelId: string;
	value: string;
	isAutocompleteAttached: boolean;
	enabled: boolean;
	typingEnabled: boolean;
}

export function runLegacyComposerTyping({
	channelId,
	value,
	isAutocompleteAttached,
	enabled,
	typingEnabled,
}: LegacyComposerTypingInput): void {
	if (!enabled || !typingEnabled) {
		LegacyTypingUtils.clear(channelId);
		return;
	}
	const content = value.trim();
	const isInReplaceMode = ReplaceCommandUtils.isReplaceCommand(content);
	const isSlashCommand = content.startsWith('/');
	if (content && !isAutocompleteAttached && !isInReplaceMode && !isSlashCommand) {
		LegacyTypingUtils.typing(channelId);
	} else {
		LegacyTypingUtils.clear(channelId);
	}
}
