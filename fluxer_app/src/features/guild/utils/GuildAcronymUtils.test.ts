// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getGuildAcronym,
	getGuildAcronymFontScale,
	getGuildAcronymLength,
} from '@app/features/guild/utils/GuildAcronymUtils';
import {describe, expect, test} from 'vitest';

const ACRONYM_VECTORS: ReadonlyArray<readonly [name: string, acronym: string, length: number]> = [
	['Movies? We Like Movies', 'M?WLM', 5],
	['Movies Movies', 'MM', 2],
	['(Internal) Fluxer DevOps', '(I)FD', 5],
	['secret server', 'ss', 2],
	['Hampus server', 'Hs', 2],
	['Fluxer', 'F', 1],
	['  Fluxer  ', 'F', 1],
	['a   b\tc\nd', 'abcd', 4],
	["Bob's Burgers", 'BB', 2],
	['Bob’s Burgers', 'BB', 2],
	["Bob's", "B's", 3],
	["Don't Panic", "D'tP", 4],
	['Area 51', 'A5', 2],
	['2024 Reunion', '2R', 2],
	['Rock-n-Roll', 'R-n-R', 5],
	['Spider-Man Fans', 'S-MF', 4],
	['snake_case_name', 's', 1],
	['C++ & Rust', 'C++&R', 5],
	['@everyone', '@e', 2],
	['!!!', '!!!', 3],
	['\u{1F3AE}\u{1F3B2}', '\u{1F3AE}\u{1F3B2}', 2],
	['\u{1F496}tes tes', '\u{1F496}tt', 3],
	['❤️love', '❤️l', 2],
	['\u{1F468}‍\u{1F469}‍\u{1F467} Family', '\u{1F468}‍\u{1F469}‍\u{1F467}F', 2],
	['\u{1F1F8}\u{1F1EA} Sweden', '\u{1F1F8}\u{1F1EA}S', 2],
	['1️⃣ One', '1️⃣O', 2],
	['日本語 サーバー', '日サ', 2],
	['原神交流群', '原', 1],
	['한국 서버', '한서', 2],
	['Привет Мир', 'ПМ', 2],
	['مرحبا بالعالم', 'مب', 2],
	['שלום עולם', 'שע', 2],
	['می‌خواهم بازی', 'مب', 2],
	['École Normale', 'ÉN', 2],
	['नमस्ते दुनिया', 'नदु', 2],
	['one two three four five six seven eight nine ten', 'ottffssent', 10],
	['', '', 0],
	['   \t 　', '', 0],
];

const FONT_SCALE_VECTORS: ReadonlyArray<readonly [length: number, fontSizePxAt48: number]> = [
	[0, 18],
	[1, 18],
	[2, 18],
	[3, 16],
	[4, 14],
	[5, 12],
	[6, 10],
	[10, 10],
];

describe('getGuildAcronym', () => {
	test.each(ACRONYM_VECTORS)('%j -> %j', (name, acronym, length) => {
		expect(getGuildAcronym(name)).toBe(acronym);
		expect(getGuildAcronymLength(acronym)).toBe(length);
	});
});

describe('getGuildAcronymFontScale', () => {
	test.each(FONT_SCALE_VECTORS)('%i graphemes -> %ipx on a 48px icon', (length, fontSizePx) => {
		expect(getGuildAcronymFontScale(length) * 48).toBeCloseTo(fontSizePx);
	});
});
