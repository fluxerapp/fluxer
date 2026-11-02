// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {CONTENT_I18N_LOCALE_MESSAGES} from '@app/api/content_i18n/ContentI18nLocales';
import {CONTENT_I18N_MESSAGES} from '@app/api/content_i18n/ContentI18nMessages';
import MessageFormat from '@messageformat/core';
import {describe, expect, it} from 'vitest';

const THIS_DIR = path.dirname(fileURLToPath(import.meta.url));
const APP_LOCALES = path.resolve(THIS_DIR, '../../../../fluxer_app/src/features/i18n/locales');

const BOX_KEYS = [
	'billing.operator_pass_delivery_waiver',
	'billing.operator_pass_delivery_waiver_multiple',
	'billing.operator_pass_delivery_country_v5',
	'billing.operator_pass_delivery_country_multiple_v5',
	'billing.operator_pass_delivery_business',
	'billing.operator_pass_delivery_business_multiple',
] as const;

const VISIONARY_BOX_KEY = 'billing.operator_pass_visionary_terms_box';
const VISIONARY_BOX_APP_SOURCE =
	"I accept the <0>{OPERATOR_PASS_NAME} terms</0>. This pass is part of your Visionary, so it can't be passed on.";

function poString(value: string): string {
	return JSON.stringify(value).slice(1, -1);
}

function appTranslation(locale: string, source: string): string | null {
	const catalog = fs.readFileSync(path.join(APP_LOCALES, locale, 'messages.po'), 'utf8');
	const marker = `\nmsgid "${poString(source)}"\nmsgstr "`;
	const start = catalog.indexOf(marker);
	if (start === -1) return null;
	const rest = catalog.slice(start + marker.length);
	return JSON.parse(`"${rest.slice(0, rest.indexOf('"\n'))}"`) as string;
}

describe('Operator Pass checkout boxes', () => {
	it.each(Object.keys(CONTENT_I18N_LOCALE_MESSAGES))('%s records the same box text that the app shows', (locale) => {
		const messages = CONTENT_I18N_LOCALE_MESSAGES[locale as keyof typeof CONTENT_I18N_LOCALE_MESSAGES];
		const format = new MessageFormat(locale, {strictPluralKeys: false});
		for (const key of BOX_KEYS) {
			const shown = appTranslation(locale, CONTENT_I18N_MESSAGES[key]);
			const recorded = messages[key];
			expect(shown, key).not.toBeNull();
			expect(recorded, key).toBeDefined();
			for (const days of [1, 7]) {
				expect(format.compile(shown ?? '')({days}), key).toBe(format.compile(recorded ?? '')({days}));
			}
		}
	});

	it.each(Object.keys(CONTENT_I18N_LOCALE_MESSAGES))(
		'%s records the Visionary box text that the app shows',
		(locale) => {
			const messages = CONTENT_I18N_LOCALE_MESSAGES[locale as keyof typeof CONTENT_I18N_LOCALE_MESSAGES];
			const shown = appTranslation(locale, VISIONARY_BOX_APP_SOURCE);
			expect(shown).not.toBeNull();
			const plain = (shown ?? '').replace(/<\/?\d+>/g, '').replaceAll('{OPERATOR_PASS_NAME}', 'Operator Pass');
			expect(messages[VISIONARY_BOX_KEY]).toBe(plain);
		},
	);
});
