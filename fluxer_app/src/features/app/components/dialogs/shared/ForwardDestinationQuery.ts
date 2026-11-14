// SPDX-License-Identifier: AGPL-3.0-or-later

export type ForwardResultType = 'user' | 'text_channel' | 'voice_channel' | 'group_dm';

export interface ForwardDestinationQuery {
	readonly mode: ForwardResultType | null;
	readonly query: string;
	readonly kinds: ReadonlyArray<ForwardResultType>;
}

const FORWARD_RESULT_TYPES: ReadonlyArray<ForwardResultType> = Object.freeze([
	'user',
	'text_channel',
	'voice_channel',
	'group_dm',
]);
const SIGIL_RESULT_TYPES: ReadonlyMap<string, ForwardResultType> = new Map([
	['@', 'user'],
	['#', 'text_channel'],
	['!', 'voice_channel'],
]);
const GLOBAL_USER_SIGIL = '@@';
const SIGIL_PATTERN = /^@|#|!|\*|\$/;

export function parseForwardDestinationQuery(text: string): ForwardDestinationQuery {
	if (text.startsWith(GLOBAL_USER_SIGIL)) {
		return Object.freeze({mode: null, query: text.slice(GLOBAL_USER_SIGIL.length), kinds: FORWARD_RESULT_TYPES});
	}
	const query = text.replace(SIGIL_PATTERN, '');
	const mode = SIGIL_RESULT_TYPES.get(text.charAt(0));
	if (mode === undefined) return Object.freeze({mode: null, query, kinds: FORWARD_RESULT_TYPES});
	return Object.freeze({mode, query, kinds: Object.freeze([mode])});
}
