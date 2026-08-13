// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AutocompleteOption} from '@app/features/channel/components/AutocompleteTypes';
import type {TriggerType} from '@app/features/messaging/utils/AutocompleteTriggerPolicy';

export function isAutocompleteMediaOption(option: AutocompleteOption): boolean {
	return option.type === 'meme' || option.type === 'gif' || option.type === 'sticker';
}

export function isAutocompleteMediaTrigger(triggerType: TriggerType): boolean {
	return triggerType === 'meme' || triggerType === 'gif' || triggerType === 'sticker';
}

export function filterAutocompleteMediaOptions(
	options: Array<AutocompleteOption>,
	allowMediaOptions: boolean,
): Array<AutocompleteOption> {
	return allowMediaOptions ? options : options.filter((option) => !isAutocompleteMediaOption(option));
}
