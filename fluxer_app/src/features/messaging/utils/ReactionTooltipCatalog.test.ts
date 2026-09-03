// SPDX-License-Identifier: AGPL-3.0-or-later

import {readdirSync, readFileSync} from 'node:fs';
import {dirname, join} from 'node:path';
import {fileURLToPath} from 'node:url';
import {i18n} from '@lingui/core';
import {describe, expect, it} from 'vitest';

const REACTED_BY_ID =
	'{reactorCount, plural, one {{emojiName} reacted by {reactors}} other {{emojiName} reacted by {reactors}}}';

const localesDir = join(dirname(fileURLToPath(import.meta.url)), '../../i18n/locales');

function readTranslation(locale: string): string {
	const source = readFileSync(join(localesDir, locale, 'messages.po'), 'utf8');
	const escaped = REACTED_BY_ID.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
	const match = source.match(new RegExp(`^msgid "${escaped}"\\nmsgstr ((?:"(?:[^"\\\\]|\\\\.)*"\\n?)+)`, 'm'));
	if (!match?.[1]) {
		throw new Error(`missing reaction tooltip translation for ${locale}`);
	}
	return match[1]
		.split('\n')
		.map((line) => line.trim())
		.filter((line) => line.startsWith('"'))
		.map((line) => JSON.parse(line) as string)
		.join('');
}

function render(locale: string, reactors: string, reactorCount: number): string {
	i18n.load(locale, {[REACTED_BY_ID]: readTranslation(locale)});
	i18n.activate(locale);
	return i18n._({id: REACTED_BY_ID, message: REACTED_BY_ID}, {emojiName: ':100:', reactors, reactorCount});
}

const locales = readdirSync(localesDir, {withFileTypes: true})
	.filter((entry) => entry.isDirectory())
	.map((entry) => entry.name)
	.sort();

describe('reaction tooltip catalog', () => {
	it('ships a translation for every locale', () => {
		expect(locales.length).toBeGreaterThan(30);
		for (const locale of locales) {
			expect(readTranslation(locale).length).toBeGreaterThan(0);
		}
	});

	it('keeps both placeholders in every plural branch of every locale', () => {
		for (const locale of locales) {
			for (const count of [1, 5]) {
				const output = render(locale, 'alex', count);
				expect(output, `${locale} @ ${count}`).toContain(':100:');
				expect(output, `${locale} @ ${count}`).toContain('alex');
				expect(output, `${locale} @ ${count}`).not.toContain('{');
			}
		}
	});

	it('agrees with the reactor count in French, the locale the bug was reported in', () => {
		expect(render('fr', 'test', 1)).toBe('test a réagi avec :100:');
		expect(render('fr', 'test, alex et 3 autres', 5)).toBe('test, alex et 3 autres ont réagi avec :100:');
	});

	it('never makes the emoji the subject of an active verb in the reported locales', () => {
		expect(render('es-ES', 'test', 1)).toBe('test ha reaccionado con :100:');
		expect(render('es-ES', 'test y alex', 2)).toBe('test y alex han reaccionado con :100:');
		expect(render('nl', 'test', 1)).toBe('test reageerde met :100:');
		expect(render('nl', 'test en alex', 2)).toBe('test en alex reageerden met :100:');
		expect(render('pt-BR', 'test', 1)).toBe('test reagiu com :100:');
		expect(render('de', 'test', 1)).toBe(':100: Reaktion von test');
	});

	it('leaves English as the emoji-first phrasing the design intends', () => {
		expect(render('en-US', 'test', 1)).toBe(':100: reacted by test');
		expect(render('en-GB', 'test and alex', 2)).toBe(':100: reacted by test and alex');
	});
});
