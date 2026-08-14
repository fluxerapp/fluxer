// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {join} from 'node:path';
import {
	hanChunkForLanguage,
	noteLocale,
	noteText,
	requestedScriptChunks,
	resetScriptFontLoaderForTests,
	type ScriptChunk,
	whenScriptChunksSettled,
} from '@app/features/theme/fonts/ScriptFontLoader';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

const FONTS_CSS = join(process.cwd(), '..', 'packages', 'fonts', 'css');

function chunks(): Array<ScriptChunk> {
	return [...requestedScriptChunks()].sort();
}

beforeEach(() => {
	resetScriptFontLoaderForTests();
	document.documentElement.lang = '';
});

afterEach(async () => {
	await whenScriptChunksSettled();
});

describe('script font loading gate', () => {
	it('requests nothing for text that is entirely Latin, which is the whole point of the split', () => {
		noteText('Hello there, this is an ordinary English message with punctuation: 1, 2, 3!');
		noteText('Ça va? Grüße! Añadir — naïve café résumé');
		expect(chunks()).toEqual([]);
	});

	it('requests nothing for empty or absent text', () => {
		noteText('');
		noteText(null);
		noteText(undefined);
		expect(chunks()).toEqual([]);
	});

	it('requests the JP chunk on kana, which no other bundled family covers', () => {
		noteText('こんにちは');
		expect(chunks()).toEqual(['jp']);
	});

	it('requests the KR chunk on hangul', () => {
		noteText('안녕하세요');
		expect(chunks()).toEqual(['kr']);
	});

	it('requests only the family the cascade would select for Han, not all three', () => {
		noteText('你好世界');
		expect(chunks()).toEqual(['sc']);
	});

	it('breaks the Han tie with the document language, never with it deciding whether to load', () => {
		document.documentElement.lang = 'ja';
		noteText('漢字');
		expect(chunks()).toEqual(['jp']);

		resetScriptFontLoaderForTests();
		document.documentElement.lang = 'zh-TW';
		noteText('漢字');
		expect(chunks()).toEqual(['tc']);

		resetScriptFontLoaderForTests();
		document.documentElement.lang = 'zh-CN';
		noteText('汉字');
		expect(chunks()).toEqual(['sc']);
	});

	it('serves mixed-script text from a Latin locale, which is what a locale gate would get wrong', () => {
		document.documentElement.lang = 'en-US';
		noteText('Hey ソラ, did 김민준 see 你好?');
		expect(chunks()).toEqual(['jp', 'kr', 'sc']);
	});

	it('never withholds a chunk because of the locale: a Japanese UI still loads Korean text', () => {
		document.documentElement.lang = 'ja';
		noteText('안녕하세요');
		expect(chunks()).toEqual(['kr']);
	});

	it('requests each chunk once, so a busy render path cannot storm the network', () => {
		noteText('こんにちは');
		const first = requestedScriptChunks().size;
		for (let i = 0; i < 100; i += 1) noteText('こんにちは');
		expect(requestedScriptChunks().size).toBe(first);
	});

	it('brings a CJK locale forward at activation without waiting for a codepoint', () => {
		noteLocale('ko');
		expect(chunks()).toEqual(['kr']);

		resetScriptFontLoaderForTests();
		noteLocale('en-US');
		expect(chunks()).toEqual([]);
	});

	it('takes the tie-break on ideograph-adjacent text that is not itself an ideograph', () => {
		for (const [label, text] of [
			['iteration mark', '々'],
			['ideographic zero', '〇'],
			['kangxi radical', '⼀'],
			['ideographic full stop', '。'],
			['fullwidth exclamation', '！'],
			['ideographic space', '　'],
		] as const) {
			resetScriptFontLoaderForTests();
			document.documentElement.lang = 'en-US';
			noteText(text);
			expect(chunks(), `${label} announced no family`).toEqual(['sc']);
		}
	});

	it('routes ideograph-adjacent text through the same tie-break as Han, not a fixed family', () => {
		document.documentElement.lang = 'ja';
		noteText('。');
		expect(chunks()).toEqual(['jp']);

		resetScriptFontLoaderForTests();
		document.documentElement.lang = 'zh-TW';
		noteText('（）');
		expect(chunks()).toEqual(['tc']);
	});

	it('keeps halfwidth katakana on JP alone rather than also taking the Han tie-break', () => {
		document.documentElement.lang = 'en-US';
		noteText('ｿﾗ');
		expect(chunks()).toEqual(['jp']);
	});

	it('still requests nothing for ordinary Latin punctuation, which shares no block with CJK', () => {
		document.documentElement.lang = 'en-US';
		noteText('!?()[]{}<>@#$%^&*-_=+/\\|~`"\'.,;: 「」');
		resetScriptFontLoaderForTests();
		noteText('!?()[]{}<>@#$%^&*-_=+/\\|~`"\'.,;: ');
		expect(chunks()).toEqual([]);
	});

	it('resolves the Han tie-break the same way the base --font-sans chain orders the families', () => {
		expect(hanChunkForLanguage('ja')).toBe('jp');
		expect(hanChunkForLanguage('ja-JP')).toBe('jp');
		expect(hanChunkForLanguage('zh-TW')).toBe('tc');
		expect(hanChunkForLanguage('zh-HK')).toBe('tc');
		expect(hanChunkForLanguage('zh-Hant')).toBe('tc');
		expect(hanChunkForLanguage('zh-CN')).toBe('sc');
		expect(hanChunkForLanguage('zh')).toBe('sc');
		expect(hanChunkForLanguage('en-US')).toBe('sc');
		expect(hanChunkForLanguage(null)).toBe('sc');
	});
});

describe('the ingest points that feed the gate', () => {
	const SRC = join(process.cwd(), 'src');
	const INGEST: Array<[string, string]> = [
		['features/guild/models/Guild.ts', 'guild names'],
		['features/guild/models/GuildRole.ts', 'role names'],
		['features/channel/models/Channel.ts', 'channel names'],
		['features/channel/state/ChannelDisplayName.ts', 'computed channel display names'],
		['features/user/utils/NicknameUtils.ts', 'display names and nicknames'],
		['features/messaging/components/markdown/index.tsx', 'message content'],
	];

	for (const [path, what] of INGEST) {
		it(`observes ${what} at ingest (${path})`, () => {
			const source = readFileSync(join(SRC, path), 'utf8');
			expect(source).toContain("from '@app/features/theme/fonts/ScriptFontLoader'");
			expect(source).toMatch(/\bnoteText\(/);
		});
	}
});

describe('the cascade the gate is computed from', () => {
	const variables = readFileSync(join(FONTS_CSS, 'variables.css'), 'utf8');
	const fallbacks = readFileSync(join(FONTS_CSS, 'locale-fallbacks.css'), 'utf8');

	it('names every gated family in the base --font-sans, or the gate could never select them', () => {
		const chain = /--font-sans:([^;]*);/.exec(variables)?.[1] ?? '';
		for (const family of ['Fluxer Sans SC', 'Fluxer Sans TC', 'Fluxer Sans JP', 'Fluxer Sans KR']) {
			expect(chain).toContain(family);
		}
	});

	it('orders the base chain SC before TC before JP, which is the no-signal Han tie-break', () => {
		const chain = /--font-sans:([^;]*);/.exec(variables)?.[1] ?? '';
		expect(chain.indexOf('Fluxer Sans SC')).toBeLessThan(chain.indexOf('Fluxer Sans TC'));
		expect(chain.indexOf('Fluxer Sans TC')).toBeLessThan(chain.indexOf('Fluxer Sans JP'));
	});

	it('hoists the family hanChunkForLanguage picks, for every locale rule that exists', () => {
		const expected: Record<string, string> = {
			ja: 'Fluxer Sans JP',
			ko: 'Fluxer Sans KR',
			'zh-CN': 'Fluxer Sans SC',
			'zh-TW': 'Fluxer Sans TC',
		};
		for (const [tag, family] of Object.entries(expected)) {
			const rule = new RegExp(`:root:lang\\(${tag}\\)[^{]*\\{([^}]*)\\}`).exec(fallbacks);
			expect(rule, `no :root:lang(${tag}) rule in locale-fallbacks.css`).not.toBeNull();
			const chain = rule![1];
			const hoisted = /'(Fluxer Sans (?:SC|TC|JP|KR))'/.exec(chain)?.[1];
			expect(hoisted, `:root:lang(${tag}) hoists the wrong family`).toBe(family);
		}
	});
});
