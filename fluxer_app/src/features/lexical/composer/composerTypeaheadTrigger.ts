// SPDX-License-Identifier: AGPL-3.0-or-later

import {getComposerAutocompleteReplacementStart} from '@app/features/lexical/composer/ComposerAutocompleteInsertion';
import {detectAutocompleteTrigger} from '@app/features/messaging/utils/SlashCommandUtils';
import type {MenuTextMatch} from '@lexical/react/LexicalTypeaheadMenuPlugin';

export function buildComposerMenuMatch(fullText: string, anchorText: string): MenuTextMatch | null {
	const trigger = detectAutocompleteTrigger(fullText);
	if (trigger == null) {
		return null;
	}
	const matchStart = getComposerAutocompleteReplacementStart(fullText, trigger.type, trigger.match);
	const tokenLength = fullText.length - matchStart;
	const leadOffset = Math.max(0, anchorText.length - tokenLength);
	return {
		leadOffset,
		matchingString: trigger.matchedText,
		replaceableString: fullText.slice(matchStart),
	};
}
