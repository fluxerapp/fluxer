// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import type {ComposerInsertPayload} from '@app/features/lexical/composer/composerOffsets';
import {isAutocompleteTriggerAllowed, type TriggerType} from '@app/features/messaging/utils/AutocompleteTriggerPolicy';
import {findUrlSpans, isInsideSpan, type TextSpan} from '@app/features/messaging/utils/markdown/UrlSpanUtils';

export type SpecialMentionKind = '@everyone' | '@here';

export interface TypedSpecialMention {
	start: number;
	end: number;
	kind: SpecialMentionKind;
}

export const SPECIAL_MENTION_PATTERN = /@(?:everyone|here)/;

const WORD_CHARACTER = /[\p{L}\p{N}]/u;

export function isSpecialMentionKind(value: string): value is SpecialMentionKind {
	return value === '@everyone' || value === '@here';
}

export function createSpecialMentionPayload(kind: SpecialMentionKind): ComposerInsertPayload {
	return {kind: 'mention', mentionType: 'special', id: kind, display: kind, wire: kind};
}

export function areSpecialMentionsAllowed(
	channel: Pick<Channel, 'guildId' | 'isPersonalNotes'> | null,
	allowSpecialMentions: boolean | undefined,
	allowedTriggers: ReadonlyArray<TriggerType> | undefined,
	canMentionEveryone: boolean,
): boolean {
	if (channel == null || allowSpecialMentions === false || !isAutocompleteTriggerAllowed('mention', allowedTriggers)) {
		return false;
	}
	return channel.guildId == null ? !channel.isPersonalNotes() : canMentionEveryone;
}

export function hasOpenCodeFence(text: string): boolean {
	const fences = text.match(/```/g);
	return fences != null && fences.length % 2 === 1;
}

function hasOpenInlineCode(text: string, index: number): boolean {
	let openRun = 0;
	for (const run of text.slice(text.lastIndexOf('\n', index - 1) + 1, index).matchAll(/`+/g)) {
		if (openRun === 0) {
			openRun = run[0].length;
		} else if (run[0].length === openRun) {
			openRun = 0;
		}
	}
	return openRun > 0;
}

export function findTypedSpecialMention(
	text: string,
	from: number,
	to: number,
	caret: number | null,
): TypedSpecialMention | null {
	const pattern = new RegExp(SPECIAL_MENTION_PATTERN.source, 'g');
	pattern.lastIndex = from;
	let urlSpans: Array<TextSpan> | null = null;
	let match = pattern.exec(text);
	while (match != null && match.index + match[0].length <= to) {
		const start = match.index;
		const end = start + match[0].length;
		const previous = text.charAt(start - 1);
		if (
			end !== caret &&
			previous !== '\\' &&
			!WORD_CHARACTER.test(previous) &&
			!WORD_CHARACTER.test(text.charAt(end)) &&
			!hasOpenCodeFence(text.slice(0, start)) &&
			!hasOpenInlineCode(text, start)
		) {
			if (urlSpans === null) {
				urlSpans = findUrlSpans(text);
			}
			if (!isInsideSpan(urlSpans, start)) {
				return {start, end, kind: match[0] as SpecialMentionKind};
			}
		}
		match = pattern.exec(text);
	}
	return null;
}
