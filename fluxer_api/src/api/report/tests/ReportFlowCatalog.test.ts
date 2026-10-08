// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {getContentMessage} from '@app/api/content_i18n/ContentI18n';
import {CONTENT_I18N_LOCALE_MESSAGES} from '@app/api/content_i18n/ContentI18nLocales';
import {CONTENT_I18N_MESSAGES, type ContentI18nKey} from '@app/api/content_i18n/ContentI18nMessages';
import {getReportFlowResponse} from '@app/api/report/flows/ReportFlowRegistry';
import {extractMessageTemplateVariables} from '@fluxer/i18n/src/runtime/MessageCatalogTypes';
import type {
	ReportFlowResponse,
	ReportFlowSurface,
	ReportFlowTargetType,
} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
import {describe, expect, test} from 'vitest';

const CONTENT_I18N_DIR = path.join(path.dirname(fileURLToPath(import.meta.url)), '../../content_i18n');
const WEBLATE_DIR = path.join(CONTENT_I18N_DIR, 'weblate');
const REVIEWED_UNCHANGED_PATH = path.join(CONTENT_I18N_DIR, 'locales/auto-i18n-reviewed-unchanged.json');
const ENGLISH_VARIANT_LOCALES = new Set(['en-GB']);
const US_SPELLINGS = /behavior|organiz|labeled/i;
const PROBE_PRODUCT_NAME = 'Zyxquor';

const VARIANTS: ReadonlyArray<[ReportFlowTargetType, ReportFlowSurface]> = [
	['message', 'in_app'],
	['message', 'dsa'],
	['user', 'in_app'],
	['user', 'dsa'],
	['guild', 'dsa'],
];

type FlatCatalog = Record<string, string>;

interface ReviewedUnchangedFile {
	version: number;
	locales: Record<string, Array<{msgctxt: string; msgid: string}>>;
}

const SOURCE = CONTENT_I18N_MESSAGES as FlatCatalog;
const REPORT_FLOW_KEYS = Object.keys(SOURCE)
	.filter((key) => key.startsWith('report_flow.'))
	.sort();
const COMPILED = CONTENT_I18N_LOCALE_MESSAGES as Record<string, FlatCatalog>;
const LOCALES = Object.keys(COMPILED).sort();
const NON_ENGLISH_LOCALES = LOCALES.filter((locale) => !ENGLISH_VARIANT_LOCALES.has(locale));

function readJson<T>(filePath: string): T {
	return JSON.parse(fs.readFileSync(filePath, 'utf8')) as T;
}

function readReviewedUnchanged(): Set<string> {
	const file = readJson<ReviewedUnchangedFile>(REVIEWED_UNCHANGED_PATH);
	const entries = new Set<string>();
	for (const [locale, items] of Object.entries(file.locales)) {
		for (const item of items) {
			entries.add(`${locale}:${item.msgctxt}`);
		}
	}
	return entries;
}

function collectStrings(response: ReportFlowResponse): Array<[string, string]> {
	const strings: Array<[string, string]> = [];
	for (const screen of response.screens) {
		strings.push([`${screen.id}.title`, screen.title]);
		if (screen.subtitle !== null) {
			strings.push([`${screen.id}.subtitle`, screen.subtitle]);
		}
		if (screen.options_heading !== null) {
			strings.push([`${screen.id}.options_heading`, screen.options_heading]);
		}
		for (const option of screen.options) {
			strings.push([`${screen.id}.${option.id}`, option.label]);
		}
		for (const item of screen.checklist?.items ?? []) {
			strings.push([`${screen.id}.${item.id}`, item.label]);
			if (item.description !== null) {
				strings.push([`${screen.id}.${item.id}.description`, item.description]);
			}
		}
	}
	for (const notice of response.notices) {
		strings.push([`${notice.id}.title`, notice.title], [`${notice.id}.body`, notice.body]);
	}
	return strings;
}

describe('report flow menu translations', () => {
	test('ships all 33 locales with the report flow keys', () => {
		expect(LOCALES).toHaveLength(33);
		expect(REPORT_FLOW_KEYS.length).toBeGreaterThan(150);
	});

	test('weblate/messages.json matches the English source for every report flow key', () => {
		const weblate = readJson<FlatCatalog>(path.join(WEBLATE_DIR, 'messages.json'));
		const problems = REPORT_FLOW_KEYS.filter((key) => weblate[key] !== SOURCE[key]);
		const stale = Object.keys(weblate).filter((key) => key.startsWith('report_flow.') && !(key in SOURCE));
		expect([...problems, ...stale]).toEqual([]);
	});

	test('every locale has every key in both the Weblate JSON and the compiled module, and they agree', () => {
		const problems: Array<string> = [];
		for (const locale of LOCALES) {
			const weblate = readJson<FlatCatalog>(path.join(WEBLATE_DIR, 'locales', `${locale}.json`));
			const compiled = COMPILED[locale];
			for (const key of REPORT_FLOW_KEYS) {
				const translated = weblate[key];
				const shipped = compiled[key];
				if (typeof translated !== 'string' || translated.trim() === '') {
					problems.push(`${locale} / ${key}: missing or empty in weblate/locales/${locale}.json`);
				}
				if (typeof shipped !== 'string' || shipped.trim() === '') {
					problems.push(`${locale} / ${key}: missing or empty in locales/${locale}.ts`);
				}
				if (translated !== shipped) {
					problems.push(`${locale} / ${key}: JSON and compiled module disagree`);
				}
			}
		}
		expect(problems).toEqual([]);
	});

	test('non-English locales never ship the English source unless it was reviewed as correct', () => {
		const reviewed = readReviewedUnchanged();
		const problems: Array<string> = [];
		for (const locale of NON_ENGLISH_LOCALES) {
			for (const key of REPORT_FLOW_KEYS) {
				if (COMPILED[locale][key] === SOURCE[key] && !reviewed.has(`${locale}:${key}`)) {
					problems.push(`${locale} / ${key}: ships the English "${SOURCE[key]}"`);
				}
			}
		}
		expect(problems).toEqual([]);
	});

	test('reviewed-unchanged entries still match the English source they were reviewed against', () => {
		const file = readJson<ReviewedUnchangedFile>(REVIEWED_UNCHANGED_PATH);
		const problems: Array<string> = [];
		for (const [locale, items] of Object.entries(file.locales)) {
			for (const item of items) {
				if (!item.msgctxt.startsWith('report_flow.')) {
					continue;
				}
				if (SOURCE[item.msgctxt] !== item.msgid || COMPILED[locale]?.[item.msgctxt] !== item.msgid) {
					problems.push(`${locale} / ${item.msgctxt}`);
				}
			}
		}
		expect(problems).toEqual([]);
	});

	test('en-GB uses British spelling wherever the US source does not', () => {
		const problems = REPORT_FLOW_KEYS.filter(
			(key) => US_SPELLINGS.test(SOURCE[key]) && US_SPELLINGS.test(COMPILED['en-GB'][key]),
		);
		expect(problems).toEqual([]);
	});

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

	test('rendered menus contain no English copy in any non-English locale', () => {
		const reviewed = readReviewedUnchanged();
		const reviewedTexts = new Map<string, Set<string>>();
		for (const entry of reviewed) {
			const [locale, key] = entry.split(/:(.*)/s);
			if (SOURCE[key] !== undefined) {
				const texts = reviewedTexts.get(locale) ?? new Set<string>();
				texts.add(SOURCE[key]);
				reviewedTexts.set(locale, texts);
			}
		}
		const problems: Array<string> = [];
		for (const [target, surface] of VARIANTS) {
			const english = new Map(collectStrings(getReportFlowResponse(target, surface, 'en-US')));
			expect(english.size).toBeGreaterThan(0);
			for (const locale of NON_ENGLISH_LOCALES) {
				const response = getReportFlowResponse(target, surface, locale);
				expect(response.locale).toBe(locale);
				for (const [id, text] of collectStrings(response)) {
					if (text === english.get(id) && !reviewedTexts.get(locale)?.has(text)) {
						problems.push(`${target}/${surface} ${locale} ${id}: "${text}"`);
					}
				}
			}
		}
		expect(problems).toEqual([]);
	});
});
