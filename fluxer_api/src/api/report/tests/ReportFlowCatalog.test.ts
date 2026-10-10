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
const COMPILED = CONTENT_I18N_LOCALE_MESSAGES as Record<string, FlatCatalog>;
const LOCALES = Object.keys(COMPILED).sort();

describe('report flow menu translations', () => {
	test('every translation keeps the ICU variables of its source and renders the product name', () => {
		const problems: Array<string> = [];
		for (const key of REPORT_FLOW_KEYS) {
			const expected = [...extractMessageTemplateVariables(SOURCE[key])].sort();
			for (const locale of ['en-US', ...LOCALES]) {
				const template = locale === 'en-US' ? SOURCE[key] : COMPILED[locale][key];
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

	test('the adult content hints quote the exact minor option label in every locale', () => {
		const pairs = [
			['report_flow.screen.sexual.subtitle', 'report_flow.label.minor_sexual'],
			['report_flow.screen.sexual_guild.subtitle', 'report_flow.label.minor_sexual_guild'],
			['report_flow.screen.private_info.subtitle', 'report_flow.label.minor_sexual'],
		] as const;
		const problems: Array<string> = [];
		for (const locale of ['en-US', ...LOCALES]) {
			const catalog = locale === 'en-US' ? SOURCE : COMPILED[locale];
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
