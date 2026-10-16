// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ReplaceCommandUtils from '@app/features/messaging/utils/ReplaceCommandUtils';
import {isEmojiReactionShorthand} from '@app/features/messaging/utils/SlashCommandUtils';

const REACTION_SHORTHAND_REGEX = /^\+(?!\w+):?(?!:)(\w+)?:?$/;

interface ComposerTypingChange {
	previousValue: string | null;
	value: string;
	isEditingMessageInComposer: boolean;
	enabled: boolean;
}

export function decideComposerTyping({
	previousValue,
	value,
	isEditingMessageInComposer,
	enabled,
}: ComposerTypingChange): 'start' | 'stop' | 'none' {
	if (!enabled || isEditingMessageInComposer || previousValue === null || value === previousValue) {
		return 'none';
	}
	const content = value.trim();
	if (content === '') {
		return 'stop';
	}
	if (
		REACTION_SHORTHAND_REGEX.test(value) ||
		isEmojiReactionShorthand(value) ||
		value.startsWith('/') ||
		ReplaceCommandUtils.isReplaceCommand(content)
	) {
		return 'none';
	}
	return 'start';
}
