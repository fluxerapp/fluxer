import {getContentMessage} from '@app/api/content_i18n/ContentI18n';
import {CONTENT_I18N_LOCALE_MESSAGES} from '@app/api/content_i18n/ContentI18nLocales';
import {CONTENT_I18N_MESSAGES, type ContentI18nKey} from '@app/api/content_i18n/ContentI18nMessages';
import {extractMessageTemplateVariables} from '@fluxer/i18n/src/runtime/MessageCatalogTypes';
import {describe, expect, test} from 'vitest';

const PROBE_PRODUCT_NAME = 'Zyxquor';

type FlatCatalog = Record<string, string>;

const SOURCE = CONTENT_I18N_MESSAGES as FlatCatalog;
const REPORT_FLOW_KEYS = Object.keys(SOURCE)
	.filter((key) => key.startsWith('report_flow.'))
	.sort();
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
	test('every translation keeps the ICU variables of its source and renders the product name', () => {
		const problems: Array<string> = [];
		for (const key of REPORT_FLOW_KEYS) {
			const expected = [...extractMessageTemplateVariables(SOURCE[key])].sort();
			for (const locale of ['en-US', ...LOCALES]) {
				const template = locale === 'en-US' ? SOURCE[key] : TRANSLATED[locale][key];
				const variables = [...extractMessageTemplateVariables(template)].sort();
				if (variables.join() !== expected.join()) {
					problems.push(`${locale} / ${key}: variables ${variables.join()} instead of ${expected.join()}`);
					continue;
				}
				const rendered = getContentMessage(key as ContentI18nKey, locale, {product_name: PROBE_PRODUCT_NAME} as never);
				const mentionsProduct = rendered.includes(PROBE_PRODUCT_NAME);
				if (rendered === key || /[{}]/.test(rendered) || mentionsProduct !== expected.includes('product_name')) {
					problems.push(`${locale} / ${key}: renders as "${rendered}"`);
				}
			}
		}
		expect(problems).toEqual([]);
	});

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

	test('the adult content hints quote the exact minor option label in every locale', () => {
		const pairs = [
			['report_flow.screen.sexual.subtitle', 'report_flow.label.minor_sexual'],
			['report_flow.screen.sexual_guild.subtitle', 'report_flow.label.minor_sexual_guild'],
			['report_flow.screen.private_info.subtitle', 'report_flow.label.minor_sexual'],
		] as const;
		const problems: Array<string> = [];
		for (const locale of ['en-US', ...LOCALES]) {
			const catalog = locale === 'en-US' ? SOURCE : TRANSLATED[locale];
			for (const [subtitleKey, labelKey] of pairs) {
				const label = catalog[labelKey].replace(/[.。]$/, '').toLowerCase();
				if (!catalog[subtitleKey].toLowerCase().includes(label)) {
					problems.push(`${locale} / ${subtitleKey}`);
				}
			}
		}
		expect(problems).toEqual([]);
	});
});
