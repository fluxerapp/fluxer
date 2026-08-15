// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SlashSlotAutocompleteContext} from '@app/features/lexical/composer/slashSlots';

export function normalizeSlotAutocompleteQuery(context: SlashSlotAutocompleteContext): string {
	const query = context.query.trim();
	if (context.optionType === 'user' || context.optionType === 'role') {
		return query.startsWith('@') ? query.slice(1) : query;
	}
	if (context.optionType === 'channel') {
		return query.startsWith('#') ? query.slice(1) : query;
	}
	return query;
}
