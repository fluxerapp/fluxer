// SPDX-License-Identifier: AGPL-3.0-or-later

import {detectAutocompleteTrigger} from '@app/features/messaging/utils/SlashCommandUtils';
import type {MenuTextMatch} from '@lexical/react/LexicalTypeaheadMenuPlugin';

export function buildComposerMenuMatch(fullText: string, anchorText: string): MenuTextMatch | null {
	const trigger = detectAutocompleteTrigger(fullText);
	if (trigger == null) {
		return null;
	}
	const matchStart =
		(trigger.match.index == null ? 0 : trigger.match.index) + (trigger.match[1] == null ? 0 : trigger.match[1].length);
	const tokenLength = fullText.length - matchStart;
	const leadOffset = Math.max(0, anchorText.length - tokenLength);
	return {
		leadOffset,
		matchingString: trigger.matchedText,
		replaceableString: fullText.slice(matchStart),
	};
}
