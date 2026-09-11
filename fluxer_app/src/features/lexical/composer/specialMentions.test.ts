// SPDX-License-Identifier: AGPL-3.0-or-later

import {DEFAULT_COMPOSER_MARKDOWN_FLAGS} from '@app/features/lexical/composer/markdownSpans';
import {
	areSpecialMentionsAllowed,
	createSpecialMentionPayload,
	findTypedSpecialMention,
	hasOpenCodeFence,
	isSpecialMentionKind,
	type SpecialMentionKind,
	type TypedSpecialMention,
} from '@app/features/lexical/composer/specialMentions';
import {parseMarkdownAstWithWasm} from '@app/features/messaging/utils/markdown/parser/MarkdownParserWasm';
import {describe, expect, it} from 'vitest';

const GUILD_CHANNEL = {guildId: '1', isPersonalNotes: () => false};
const DM_CHANNEL = {guildId: undefined, isPersonalNotes: () => false};
const PERSONAL_NOTES = {guildId: undefined, isPersonalNotes: () => true};

function typedSpecialMentions(text: string): Array<SpecialMentionKind> {
	const kinds: Array<SpecialMentionKind> = [];
	let match = findTypedSpecialMention(text, 0, text.length, null);
	while (match != null) {
		kinds.push(match.kind);
		match = findTypedSpecialMention(text, match.end, text.length, null);
	}
	return kinds;
}

function collectParsedSpecialMentions(value: unknown, kinds: Array<SpecialMentionKind>): Array<SpecialMentionKind> {
	if (Array.isArray(value)) {
		for (const item of value) {
			collectParsedSpecialMentions(item, kinds);
		}
	} else if (typeof value === 'object' && value != null) {
		if ('type' in value && value.type === 'Mention' && 'kind' in value) {
			const mention = JSON.stringify(value.kind);
			if (mention === '{"kind":"Everyone"}') {
				kinds.push('@everyone');
			} else if (mention === '{"kind":"Here"}') {
				kinds.push('@here');
			}
		}
		for (const child of Object.values(value)) {
			collectParsedSpecialMentions(child, kinds);
		}
	}
	return kinds;
}

describe('findTypedSpecialMention', () => {
	it.each<[string, string, number | null, TypedSpecialMention]>([
		['@everyone followed by a space', '@everyone ', 10, {start: 0, end: 9, kind: '@everyone'}],
		['@here followed by a space', '@here ', 6, {start: 0, end: 5, kind: '@here'}],
		['a finished token with no caret', 'hi @everyone', null, {start: 3, end: 12, kind: '@everyone'}],
		['a finished token with the caret before it', 'hi @everyone', 0, {start: 3, end: 12, kind: '@everyone'}],
		['a finished token with the caret inside it', '@here', 3, {start: 0, end: 5, kind: '@here'}],
		['a token inside parentheses', '(@everyone)', null, {start: 1, end: 10, kind: '@everyone'}],
		['a token inside bold markers', '**@everyone** ', 14, {start: 2, end: 11, kind: '@everyone'}],
		['a token inside underline markers', '__@here__ ', 10, {start: 2, end: 7, kind: '@here'}],
		['a token followed by an apostrophe', "@here's ", 8, {start: 0, end: 5, kind: '@here'}],
		['a token followed by an underscore', '@here_now ', null, {start: 0, end: 5, kind: '@here'}],
		['a token after a second @', '@@everyone ', null, {start: 1, end: 10, kind: '@everyone'}],
		['a token after a mention wire', '<@1>@here ', null, {start: 4, end: 9, kind: '@here'}],
		['a token after closed inline code', '`x` @everyone ', null, {start: 4, end: 13, kind: '@everyone'}],
		['a token after a closed fence', '```\ncode\n```\n@everyone ', null, {start: 13, end: 22, kind: '@everyone'}],
		['a token on a new line after an inline code line', '`x`\n@here ', null, {start: 4, end: 9, kind: '@here'}],
	])('converts %s', (_name, text, caret, expected) => {
		expect(findTypedSpecialMention(text, 0, text.length, caret)).toEqual(expected);
	});

	it.each<[string, string, number | null]>([
		['a token whose end is the caret', 'hi @everyone', 12],
		['a token joined to a following word', '@herenow ', 9],
		['a token with a trailing letter', '@everyones ', 11],
		['a token with a leading word', 'foo@everyone ', 13],
		['a token inside an email address', 'mail user@here.com ', null],
		['a token touching a digit', '1@here 2', null],
		['a token touching a non-latin letter', 'café@here ', null],
		['a token after one backslash', '\\@everyone ', null],
		['a token after two backslashes', '\\\\@everyone ', null],
		['a token inside open inline code', '`@everyone ', null],
		['a token inside double backtick code', '`` @everyone ``', null],
		['a token inside an open fence', '```\n@everyone \n', null],
		['a token inside a url', 'https://fluxer.app/@everyone ', null],
		['a spaced @ and word', '@ everyone ', null],
	])('leaves %s alone', (_name, text, caret) => {
		expect(findTypedSpecialMention(text, 0, text.length, caret)).toBeNull();
	});

	it('only considers tokens that start at from and end by to', () => {
		expect(findTypedSpecialMention('@everyone and @here ', 10, 20, 20)).toEqual({start: 14, end: 19, kind: '@here'});
		expect(findTypedSpecialMention('@everyone ', 0, 5, null)).toBeNull();
	});
});

describe('findTypedSpecialMention against the markdown parser', () => {
	it.each<[string, Array<SpecialMentionKind>, Array<SpecialMentionKind>]>([
		['@everyone ', ['@everyone'], ['@everyone']],
		['@here ', ['@here'], ['@here']],
		['(@everyone)', ['@everyone'], ['@everyone']],
		['**@everyone** ', ['@everyone'], ['@everyone']],
		['__@here__ ', ['@here'], ['@here']],
		["@here's ", ['@here'], ['@here']],
		['@here_now ', ['@here'], ['@here']],
		['@@everyone ', ['@everyone'], ['@everyone']],
		['<@1>@here ', ['@here'], ['@here']],
		['`x` @everyone ', ['@everyone'], ['@everyone']],
		['```\ncode\n```\n@everyone ', ['@everyone'], ['@everyone']],
		['\\@everyone ', [], []],
		['\\\\@everyone ', [], []],
		['`` @everyone ``', [], []],
		['https://fluxer.app/@everyone ', [], []],
		['@ everyone ', [], []],
		['@herenow ', [], ['@here']],
		['@everyones ', [], ['@everyone']],
		['foo@everyone ', [], ['@everyone']],
		['mail user@here.com ', [], ['@here']],
		['café@here ', [], ['@here']],
		['@everyone@here', ['@everyone'], ['@everyone', '@here']],
		['`@everyone ', [], ['@everyone']],
		['```\n@everyone \n', [], ['@everyone']],
	])('%j converts %j where the parser pings %j', (text, typed, parsed) => {
		expect(typedSpecialMentions(text)).toEqual(typed);
		expect(
			collectParsedSpecialMentions(parseMarkdownAstWithWasm(text, DEFAULT_COMPOSER_MARKDOWN_FLAGS).nodes, []),
		).toEqual(parsed);
	});
});

describe('areSpecialMentionsAllowed', () => {
	it.each<[string, Parameters<typeof areSpecialMentionsAllowed>, boolean]>([
		['a guild channel with permission', [GUILD_CHANNEL, undefined, undefined, true], true],
		['a guild channel without permission', [GUILD_CHANNEL, undefined, undefined, false], false],
		['a direct message', [DM_CHANNEL, undefined, undefined, false], true],
		['personal notes', [PERSONAL_NOTES, undefined, undefined, true], false],
		['a composer with no channel', [null, true, ['emoji'], true], false],
		['a guild channel with special mentions switched off', [GUILD_CHANNEL, false, undefined, true], false],
		['a direct message with special mentions switched off', [DM_CHANNEL, false, undefined, false], false],
		['a guild channel without the mention trigger', [GUILD_CHANNEL, true, ['emoji'], true], false],
		[
			'a guild channel with the rich input triggers',
			[GUILD_CHANNEL, true, ['emoji', 'mention', 'channel'], true],
			true,
		],
	])('answers for %s', (_name, args, expected) => {
		expect(areSpecialMentionsAllowed(...args)).toBe(expected);
	});
});

describe('createSpecialMentionPayload', () => {
	it('builds the autocomplete mention payload', () => {
		expect(createSpecialMentionPayload('@everyone')).toEqual({
			kind: 'mention',
			mentionType: 'special',
			id: '@everyone',
			display: '@everyone',
			wire: '@everyone',
		});
	});
});

describe('isSpecialMentionKind', () => {
	it('recognises only @everyone and @here', () => {
		expect(['@everyone', '@here', 'here', '@someone'].map(isSpecialMentionKind)).toEqual([true, true, false, false]);
	});
});

describe('hasOpenCodeFence', () => {
	it('reports an odd number of fences', () => {
		expect(['```js\nconst a', '```a```', ''].map(hasOpenCodeFence)).toEqual([true, false, false]);
	});
});
