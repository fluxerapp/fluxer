import {CONTENT_I18N_LOCALE_MESSAGES} from '@app/api/content_i18n/ContentI18nLocales';
import {CONTENT_I18N_MESSAGES} from '@app/api/content_i18n/ContentI18nMessages';
import {extractMessageTemplateVariables} from '@fluxer/i18n/src/runtime/MessageCatalogTypes';
import {describe, expect, test} from 'vitest';

type FlatCatalog = Record<string, string>;

const SOURCE = CONTENT_I18N_MESSAGES as FlatCatalog;
const TRANSLATED = CONTENT_I18N_LOCALE_MESSAGES as Record<string, FlatCatalog>;
const LOCALES = Object.keys(TRANSLATED).sort();

function placeholderHead(placeholder: string): string {
	const comma = placeholder.indexOf(',');
	return (comma === -1 ? placeholder : placeholder.slice(0, comma)).replace(/^[ \n\r\t]+|[ \n\r\t]+$/g, '');
}

function isPlaceholderName(name: string): boolean {
	return name !== '' && !name.startsWith('#') && !/^[0-9]/.test(name) && !/[ \n\r\t{}]/.test(name);
}

function placeholderNames(template: string): Set<string> {
	const names = new Set<string>();
	let rest = template;
	for (let open = rest.indexOf('{'); open !== -1; open = rest.indexOf('{')) {
		const afterOpen = rest.slice(open + 1);
		const close = afterOpen.indexOf('}');
		if (close === -1) {
			break;
		}
		const name = placeholderHead(afterOpen.slice(0, close));
		if (isPlaceholderName(name)) {
			names.add(name);
		}
		rest = afterOpen.slice(close + 1);
	}
	return names;
}

describe('report flow menu translations', () => {
	test('every content translation keeps exactly the placeholders of its English source', () => {
		const problems: Array<string> = [];
		for (const key of Object.keys(SOURCE).sort()) {
			const sourceNames = placeholderNames(SOURCE[key]);
			const expected = [...extractMessageTemplateVariables(SOURCE[key])].sort().join();
			for (const locale of LOCALES) {
				const template = TRANSLATED[locale][key];
				const translatedNames = placeholderNames(template);
				const dropped = [...sourceNames].filter((name) => !translatedNames.has(name)).sort();
				if (dropped.length > 0) {
					problems.push(`${locale} / ${key}: drops ${dropped.join()}`);
				}
				const actual = [...extractMessageTemplateVariables(template)].sort().join();
				if (actual !== expected) {
					problems.push(`${locale} / ${key}: variables ${actual} instead of ${expected}`);
				}
			}
		}
		expect(problems).toEqual([]);
	});
});
