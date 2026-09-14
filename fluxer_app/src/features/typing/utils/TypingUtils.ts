// SPDX-License-Identifier: AGPL-3.0-or-later

import {runLegacyComposerTyping} from '@app/features/typing/legacy/LegacyComposerTyping';
import {TypingUtils as LegacyTypingUtils} from '@app/features/typing/legacy/LegacyTypingUtils';
import {decideComposerTyping} from '@app/features/typing/rolling/RollingComposerTypingGate';
import RollingTypingSender from '@app/features/typing/rolling/RollingTypingSender';
import TypingPolicy from '@app/features/typing/state/TypingPolicy';

interface ComposerTypingInput {
	channelId: string;
	value: string;
	previousValue: string | null;
	isAutocompleteAttached: boolean;
	enabled: boolean;
	typingEnabled: boolean;
	isEditingMessageInComposer: boolean;
}

class TypingUtilsFacade {
	handleComposerChange(input: ComposerTypingInput): void {
		if (TypingPolicy.active !== 'rolling') {
			runLegacyComposerTyping(input);
			return;
		}
		const decision = decideComposerTyping({
			previousValue: input.previousValue,
			value: input.value,
			isRestoringDraft: false,
			isEditingMessageInComposer: input.isEditingMessageInComposer,
			enabled: input.enabled && input.typingEnabled,
		});
		if (decision === 'start') {
			RollingTypingSender.startTyping(input.channelId);
		} else if (decision === 'stop') {
			RollingTypingSender.stopTyping(input.channelId);
		}
	}

	releaseComposer(channelId: string): void {
		if (TypingPolicy.active === 'rolling') {
			return;
		}
		LegacyTypingUtils.clear(channelId);
	}

	clear(channelId: string): void {
		if (TypingPolicy.active === 'rolling') {
			RollingTypingSender.stopTyping(channelId);
			return;
		}
		LegacyTypingUtils.clear(channelId);
	}

	handleOwnMessageSent(channelId: string): void {
		if (TypingPolicy.active === 'rolling') {
			RollingTypingSender.handleOwnMessageSent(channelId);
			return;
		}
		LegacyTypingUtils.clear(channelId);
	}
}

export const TypingUtils = new TypingUtilsFacade();
