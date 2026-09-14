// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {commitUnderTranslation, isTranslationDomCrash} from '@app/features/i18n/testing/TranslationCommitHarness';
import {
	chromeFaithfulCompoundPipeline,
	findTranslationUnsafeTextNodes,
	TRANSLATION_MARKER,
	TRANSLATOR_PIPELINE_NAMES,
	TranslatorPipelines,
} from '@app/features/i18n/testing/TranslatorSimulation';
import {createElement, Fragment, type ReactNode} from 'react';
import {afterEach, describe, expect, it} from 'vitest';

const badge = (on: boolean): ReactNode => (on ? createElement('b', null, '!') : null);
const tail = createElement('i', null, 'x');

const BareLabel = (): ReactNode => 'Hello';
const WrappedLabel = (): ReactNode => createElement('flx-i18n', null, 'Hello');

const wrap = (text: string): ReactNode => createElement('flx-i18n', null, text);
const row = (...children: Array<ReactNode>): ReactNode => createElement('div', null, ...children);

const visibleText = (html: string): string => html.replace(/<[^>]+>/g, '').replaceAll(TRANSLATION_MARKER, '');

interface Scenario {
	name: string;
	unsafe: (on: boolean) => ReactNode;
	fixed: (on: boolean) => ReactNode;
	beforeState: boolean;
	afterState: boolean;
	expectedText: string;
	expectedFirstTag: string;
}

const scenarios: Array<Scenario> = [
	{
		name: 'placement before a bare text sibling',
		unsafe: (on) => row(badge(on), 'Hello', tail),
		fixed: (on) => row(badge(on), wrap('Hello'), tail),
		beforeState: false,
		afterState: true,
		expectedText: '!Hellox',
		expectedFirstTag: 'B',
	},
	{
		name: 'deletion of a bare text sibling',
		unsafe: (on) => row(on ? 'Hello' : null, tail),
		fixed: (on) => row(on ? wrap('Hello') : null, tail),
		beforeState: true,
		afterState: false,
		expectedText: 'x',
		expectedFirstTag: 'I',
	},
	{
		name: 'placement between adjacent text nodes',
		unsafe: (on) => row('A', badge(on), 'B', tail),
		fixed: (on) => row(wrap('A'), badge(on), wrap('B'), tail),
		beforeState: false,
		afterState: true,
		expectedText: 'A!Bx',
		expectedFirstTag: 'FLX-I18N',
	},
	{
		name: 'placement before text inside a fragment',
		unsafe: (on) => row(createElement(Fragment, null, badge(on), 'Hello'), tail),
		fixed: (on) => row(createElement(Fragment, null, badge(on), wrap('Hello')), tail),
		beforeState: false,
		afterState: true,
		expectedText: '!Hellox',
		expectedFirstTag: 'B',
	},
	{
		name: 'placement before a component that returns a bare string',
		unsafe: (on) => row(badge(on), createElement(BareLabel), tail),
		fixed: (on) => row(badge(on), createElement(WrappedLabel), tail),
		beforeState: false,
		afterState: true,
		expectedText: '!Hellox',
		expectedFirstTag: 'B',
	},
];

afterEach(() => {
	document.body.replaceChildren();
});

for (const pipelineName of TRANSLATOR_PIPELINE_NAMES) {
	const pipeline = TranslatorPipelines[pipelineName];
	describe(`${pipelineName} pipeline`, () => {
		for (const scenario of scenarios) {
			it(`throws the production DOM error on ${scenario.name}`, () => {
				const result = commitUnderTranslation({
					before: scenario.unsafe(scenario.beforeState),
					after: scenario.unsafe(scenario.afterState),
					pipeline,
				});
				expect(result.unsafeTextNodesBeforeTranslation).toBeGreaterThan(0);
				expect(result.htmlAfterTranslation).not.toBe(result.htmlBeforeTranslation);
				expect(isTranslationDomCrash(result.error)).toBe(true);
			});

			if (pipelineName === 'chromeCompound') {
				it(`leaves the compound pipeline no text-and-element parent to rewrite in ${scenario.name}`, () => {
					const unsafe = commitUnderTranslation({
						before: scenario.unsafe(scenario.beforeState),
						after: scenario.unsafe(scenario.beforeState),
						pipeline,
					});
					expect(unsafe.unsafeTextNodesBeforeTranslation).toBeGreaterThan(0);
					expect(unsafe.htmlAfterTranslation).not.toBe(unsafe.htmlBeforeTranslation);

					const fixed = commitUnderTranslation({
						before: scenario.fixed(scenario.beforeState),
						after: scenario.fixed(scenario.afterState),
						pipeline,
					});
					expect(fixed.unsafeTextNodesBeforeTranslation).toBe(0);
					expect(fixed.htmlAfterTranslation).toBe(fixed.htmlBeforeTranslation);
					expect(fixed.error).toBeNull();
				});
				continue;
			}

			it(`survives ${scenario.name} once the text is a sole child`, () => {
				let firstTag: string | null = null;
				let unsafeTextNodes = -1;
				const result = commitUnderTranslation({
					before: scenario.fixed(scenario.beforeState),
					after: scenario.fixed(scenario.afterState),
					pipeline,
					inspect: (host) => {
						firstTag = host.querySelector('div')?.firstElementChild?.tagName ?? null;
						unsafeTextNodes = findTranslationUnsafeTextNodes(host).length;
					},
				});
				expect(result.htmlAfterTranslation).not.toBe(result.htmlBeforeTranslation);
				expect(result.htmlAfterTranslation).toContain(TRANSLATION_MARKER);
				expect(result.error).toBeNull();
				expect(result.unsafeTextNodesBeforeTranslation).toBe(0);
				expect(unsafeTextNodes).toBe(0);
				expect(firstTag).toBe(scenario.expectedFirstTag);
				expect(visibleText(result.htmlAfterCommit)).toBe(scenario.expectedText);
			});
		}
	});
}

describe('faithful compound pipeline', () => {
	const inlineWrap = (text: string): ReactNode => createElement('span', null, text);
	const contentsWrap = (text: string): ReactNode => createElement('flx-i18n', {style: {display: 'contents'}}, text);

	it('defeats a bare text sibling', () => {
		const result = commitUnderTranslation({
			before: row(badge(false), 'Hello', tail),
			after: row(badge(true), 'Hello', tail),
			pipeline: chromeFaithfulCompoundPipeline,
		});
		expect(isTranslationDomCrash(result.error)).toBe(true);
	});

	it('defeats an inline sole-child wrapper', () => {
		const result = commitUnderTranslation({
			before: row(badge(false), inlineWrap('Hello'), tail),
			after: row(badge(true), inlineWrap('Hello'), tail),
			pipeline: chromeFaithfulCompoundPipeline,
		});
		expect(isTranslationDomCrash(result.error)).toBe(true);
	});

	it('defeats the display:contents wrapper the app ships', () => {
		const result = commitUnderTranslation({
			before: row(badge(false), contentsWrap('Hello'), tail),
			after: row(badge(true), contentsWrap('Hello'), tail),
			pipeline: chromeFaithfulCompoundPipeline,
		});
		expect(isTranslationDomCrash(result.error)).toBe(true);
	});

	it('defeats a pure element tree that holds no text node of its own', () => {
		let reassembledChildren = false;
		const pureElementRow = (on: boolean): ReactNode =>
			row(badge(on), createElement('i', null, 'a'), createElement('em', null, 'y'));
		const result = commitUnderTranslation({
			before: pureElementRow(false),
			after: pureElementRow(true),
			pipeline: (host) => {
				const before = Array.from(host.querySelector('div')?.childNodes ?? []);
				chromeFaithfulCompoundPipeline(host);
				const after = Array.from(host.querySelector('div')?.childNodes ?? []);
				reassembledChildren = before.some((node, index) => node !== after[index]);
			},
		});
		expect(reassembledChildren).toBe(true);
		expect(result.unsafeTextNodesBeforeTranslation).toBe(0);
		expect(isTranslationDomCrash(result.error)).toBe(true);
	});

	it('is survived only by translate="no"', () => {
		const result = commitUnderTranslation({
			before: createElement('div', {translate: 'no'}, badge(false), contentsWrap('Hello'), tail),
			after: createElement('div', {translate: 'no'}, badge(true), contentsWrap('Hello'), tail),
			pipeline: chromeFaithfulCompoundPipeline,
		});
		expect(result.htmlAfterTranslation).toBe(result.htmlBeforeTranslation);
		expect(result.error).toBeNull();
		expect(result.htmlAfterCommit).toContain('<b>!</b>');
		expect(visibleText(result.htmlAfterCommit)).toBe('!Hellox');
	});
});

describe('shapes that were never at risk', () => {
	it('does not throw when only elements are siblings', () => {
		const result = commitUnderTranslation({
			before: row(badge(false), createElement('i', null, 'a')),
			after: row(badge(true), createElement('i', null, 'a')),
			pipeline: TranslatorPipelines.chromeLegacyFont,
		});
		expect(result.htmlAfterTranslation).not.toBe(result.htmlBeforeTranslation);
		expect(result.error).toBeNull();
	});

	it('does not throw when a sibling is appended after the text', () => {
		const result = commitUnderTranslation({
			before: row('Hello', badge(false)),
			after: row('Hello', badge(true)),
			pipeline: TranslatorPipelines.chromeLegacyFont,
		});
		expect(result.htmlAfterTranslation).not.toBe(result.htmlBeforeTranslation);
		expect(result.error).toBeNull();
	});
});
