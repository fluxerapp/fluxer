// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {AppI18nProvider} from '@app/features/i18n/components/AppI18nProvider';
import {commitUnderTranslation, isTranslationDomCrash} from '@app/features/i18n/testing/TranslationCommitHarness';
import {
	findTranslationUnsafeTextNodes,
	TRANSLATION_MARKER,
	TRANSLATOR_PIPELINE_NAMES,
	TranslatorPipelines,
} from '@app/features/i18n/testing/TranslatorSimulation';
import {i18n} from '@lingui/core';
import {I18nProvider, Trans} from '@lingui/react';
import {createElement, type ReactNode} from 'react';
import {afterEach, beforeAll, describe, expect, it} from 'vitest';

const ENGLISH = {
	simple: 'Monthly {price}',
	rich: 'Renews on <0>{date}</0>. Enjoy!',
	nested: 'Renews on <0>{date} at <1>noon</1></0>. Enjoy!',
};
const GERMAN = {
	simple: 'Monatlich {price}',
	rich: '<0>{date}</0> ist das Datum. Viel Spass!',
	nested: 'Ende <0><1>mittags</1> am {date}</0>. Viel Spass!',
};

const visibleText = (html: string): string => html.replace(/<[^>]+>/g, '').replaceAll(TRANSLATION_MARKER, '');

const message = (id: keyof typeof ENGLISH): ReactNode =>
	createElement(Trans, {
		id,
		values: {date: 'Jan 1', price: '$5'},
		components: {0: createElement('strong'), 1: createElement('em')},
	});

const provided = (child: ReactNode, safe: boolean): ReactNode =>
	createElement(safe ? AppI18nProvider : I18nProvider, {i18n}, createElement('div', null, child));

const withBadge = (id: keyof typeof ENGLISH, on: boolean, safe: boolean): ReactNode =>
	provided(
		createElement('div', null, on ? createElement('b', null, '!') : null, message(id), createElement('i', null, 'x')),
		safe,
	);

beforeAll(() => {
	i18n.load('en', ENGLISH);
	i18n.load('de', GERMAN);
	i18n.activate('en');
});

afterEach(() => {
	i18n.activate('en');
	document.body.replaceChildren();
});

describe('lingui <Trans> without a translation-safe defaultComponent', () => {
	for (const pipelineName of TRANSLATOR_PIPELINE_NAMES) {
		it(`throws the production DOM error on a simple message under ${pipelineName}`, () => {
			const result = commitUnderTranslation({
				before: withBadge('simple', false, false),
				after: withBadge('simple', true, false),
				pipeline: TranslatorPipelines[pipelineName],
			});
			expect(result.unsafeTextNodesBeforeTranslation).toBeGreaterThan(0);
			expect(result.htmlBeforeTranslation).toContain('Monthly $5<i>x</i>');
			expect(isTranslationDomCrash(result.error)).toBe(true);
		});

		it(`throws the production DOM error on a rich message under ${pipelineName}`, () => {
			const result = commitUnderTranslation({
				before: withBadge('rich', false, false),
				after: withBadge('rich', true, false),
				pipeline: TranslatorPipelines[pipelineName],
			});
			expect(result.unsafeTextNodesBeforeTranslation).toBeGreaterThan(0);
			expect(isTranslationDomCrash(result.error)).toBe(true);
		});
	}
});

describe('lingui <Trans> under AppI18nProvider', () => {
	for (const pipelineName of TRANSLATOR_PIPELINE_NAMES) {
		if (pipelineName === 'chromeCompound') {
			for (const id of ['simple', 'rich'] as const) {
				it(`leaves the compound pipeline no text-and-element parent to rewrite in a ${id} message`, () => {
					const unguarded = commitUnderTranslation({
						before: withBadge(id, false, false),
						after: withBadge(id, false, false),
						pipeline: TranslatorPipelines.chromeCompound,
					});
					expect(unguarded.unsafeTextNodesBeforeTranslation).toBeGreaterThan(0);
					expect(unguarded.htmlAfterTranslation).not.toBe(unguarded.htmlBeforeTranslation);

					const guarded = commitUnderTranslation({
						before: withBadge(id, false, true),
						after: withBadge(id, true, true),
						pipeline: TranslatorPipelines.chromeCompound,
					});
					expect(guarded.unsafeTextNodesBeforeTranslation).toBe(0);
					expect(guarded.htmlAfterTranslation).toBe(guarded.htmlBeforeTranslation);
					expect(guarded.error).toBeNull();
				});
			}
			continue;
		}

		it(`renders a simple message safely under ${pipelineName}`, () => {
			let unsafeTextNodes = -1;
			const result = commitUnderTranslation({
				before: withBadge('simple', false, true),
				after: withBadge('simple', true, true),
				pipeline: TranslatorPipelines[pipelineName],
				inspect: (host) => {
					unsafeTextNodes = findTranslationUnsafeTextNodes(host).length;
				},
			});
			expect(result.htmlAfterTranslation).not.toBe(result.htmlBeforeTranslation);
			expect(result.htmlAfterTranslation).toContain(TRANSLATION_MARKER);
			expect(result.error).toBeNull();
			expect(result.htmlBeforeTranslation).not.toContain('Monthly $5<i>x</i>');
			expect(result.unsafeTextNodesBeforeTranslation).toBe(0);
			expect(unsafeTextNodes).toBe(0);
			expect(visibleText(result.htmlAfterCommit)).toBe('!Monthly $5x');
		});

		it(`renders a rich message safely under ${pipelineName}`, () => {
			let unsafeTextNodes = -1;
			const result = commitUnderTranslation({
				before: withBadge('rich', false, true),
				after: withBadge('rich', true, true),
				pipeline: TranslatorPipelines[pipelineName],
				inspect: (host) => {
					unsafeTextNodes = findTranslationUnsafeTextNodes(host).length;
				},
			});
			expect(result.htmlAfterTranslation).not.toBe(result.htmlBeforeTranslation);
			expect(result.htmlAfterTranslation).toContain(TRANSLATION_MARKER);
			expect(result.error).toBeNull();
			expect(unsafeTextNodes).toBe(0);
			expect(result.htmlAfterCommit).toContain('<strong>');
			expect(visibleText(result.htmlAfterCommit)).toBe('!Renews on Jan 1. Enjoy!x');
		});
	}
});

describe('locale switch while the page is translated', () => {
	for (const id of ['rich', 'nested'] as const) {
		it(`reorders the ${id} message segments without throwing`, () => {
			const result = commitUnderTranslation({
				before: provided(message(id), true),
				after: provided(message(id), true),
				pipeline: TranslatorPipelines.chromeLegacyFont,
				betweenCommits: () => {
					i18n.activate('de');
				},
			});
			expect(result.htmlAfterTranslation).not.toBe(result.htmlBeforeTranslation);
			expect(result.htmlAfterTranslation).toContain(TRANSLATION_MARKER);
			expect(result.error).toBeNull();
			expect(visibleText(result.htmlAfterCommit)).toContain('Viel Spass!');
		});
	}
});

describe('translation-safe wrapping internals', () => {
	it('recurses into nested rich message elements so no host element mixes text with element children', () => {
		const result = commitUnderTranslation({
			before: provided(message('nested'), true),
			after: provided(message('nested'), true),
			pipeline: TranslatorPipelines.chromeLegacyFont,
		});
		expect(result.unsafeTextNodesBeforeTranslation).toBe(0);
	});

	it('leaves nested rich message text unwrapped when it is not recursed into', () => {
		const result = commitUnderTranslation({
			before: provided(message('nested'), false),
			after: provided(message('nested'), false),
			pipeline: TranslatorPipelines.chromeLegacyFont,
		});
		expect(result.unsafeTextNodesBeforeTranslation).toBeGreaterThan(0);
	});

	it('remounts on a locale change so no stale translated segment survives', () => {
		const result = commitUnderTranslation({
			before: provided(message('rich'), true),
			after: provided(message('rich'), true),
			pipeline: TranslatorPipelines.chromeLegacyFont,
			betweenCommits: () => {
				i18n.activate('de');
			},
		});
		expect(result.error).toBeNull();
		expect(visibleText(result.htmlAfterCommit)).toContain('Viel Spass!');
		expect(result.htmlAfterCommit.split(TRANSLATION_MARKER).length - 1).toBe(0);
	});
});
