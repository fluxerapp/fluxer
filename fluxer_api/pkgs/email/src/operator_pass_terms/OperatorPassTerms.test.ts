// SPDX-License-Identifier: AGPL-3.0-or-later

import {existsSync, readFileSync} from 'node:fs';
import {join} from 'node:path';
import {
	getOperatorPassTermsBlocks,
	OPERATOR_PASS_TERMS_VERSION,
	resolveOperatorPassTermsLanguage,
} from '@pkgs/email/src/operator_pass_terms/OperatorPassTerms';
import {renderOperatorPassTermsPdf} from '@pkgs/email/src/operator_pass_terms/OperatorPassTermsPdf';
import {
	OPERATOR_PASS_TERMS_TEXT,
	type OperatorPassTermsBlock,
} from '@pkgs/email/src/operator_pass_terms/OperatorPassTermsText';
import {PDFDocument} from 'pdf-lib';
import {describe, expect, it} from 'vitest';

const LANGUAGES = Object.keys(OPERATOR_PASS_TERMS_TEXT) as Array<keyof typeof OPERATOR_PASS_TERMS_TEXT>;

const MARKETING_DIR = process.env.FLUXER_MARKETING_DIR ?? '';

const MARKDOWN_PATHS: Record<(typeof LANGUAGES)[number], string> = {
	'en-US': 'content/policies/operator-pass-terms.md',
	'sv-SE': 'content/sv-SE/policies/operator-pass-terms.md',
	'pt-BR': 'content/pt-BR/policies/operator-pass-terms.md',
};

function renderDateTokens(markdown: string, language: string): string {
	const format = new Intl.DateTimeFormat(language, {dateStyle: 'long', timeZone: 'UTC'});
	return markdown.replace(/\[\[date:(\d{4}-\d{2}-\d{2})\]\]/gu, (_token, day: string) =>
		format.format(new Date(`${day}T00:00:00Z`)),
	);
}

function parseTermsMarkdown(markdown: string, language: string): Array<OperatorPassTermsBlock> {
	const blocks: Array<OperatorPassTermsBlock> = [];
	for (const chunk of renderDateTokens(markdown, language)
		.replaceAll('\\*', '*')
		.split(/\n\s*\n/u)) {
		const text = chunk.trim();
		const heading = text.match(/^(#{2,3}) (.*?)(?:\s*\{#[^}]*\})?$/u);
		if (!text || text.startsWith('<!--')) continue;
		if (heading) {
			blocks.push({kind: heading[1] === '##' ? 'heading' : 'subheading', text: heading[2]!});
		} else if (text.startsWith('>')) {
			for (const line of text.split('\n')) {
				const quote = line.replace(/^>\s?/u, '').trim();
				if (quote) blocks.push({kind: 'quote', text: quote});
			}
		} else if (/^[-*] /u.test(text)) {
			for (const line of text.split('\n')) blocks.push({kind: 'bullet', text: line.replace(/^[-*] /u, '').trim()});
		} else {
			blocks.push({kind: 'paragraph', text: text.replace(/\s*\n\s*/gu, ' ')});
		}
	}
	return blocks;
}

function blockKindsBySection(language: (typeof LANGUAGES)[number], kinds: ReadonlySet<string>): Array<string> {
	return OPERATOR_PASS_TERMS_TEXT[language]
		.filter((block) => kinds.has(block.kind))
		.map((block) => (block.kind === 'heading' || block.kind === 'subheading' ? block.text.split(' ')[0]! : block.kind));
}

const ALL_KINDS = new Set(['heading', 'subheading', 'paragraph', 'bullet', 'quote']);
const LIST_AND_FORM_KINDS = new Set(['heading', 'subheading', 'bullet', 'quote']);

describe('Operator Pass terms', () => {
	it('sends the Portuguese terms whenever the Brazilian rule applies', () => {
		expect(resolveOperatorPassTermsLanguage('en-US', 'BR')).toBe('pt-BR');
		expect(resolveOperatorPassTermsLanguage('sv-SE', 'BR')).toBe('pt-BR');
		expect(resolveOperatorPassTermsLanguage('pt-BR')).toBe('en-US');
		expect(resolveOperatorPassTermsLanguage('pt-PT', 'PT')).toBe('en-US');
		expect(resolveOperatorPassTermsLanguage('sv-SE')).toBe('sv-SE');
		expect(resolveOperatorPassTermsLanguage('de', null)).toBe('en-US');
		expect(resolveOperatorPassTermsLanguage(null)).toBe('en-US');
	});

	it('keeps the same sections in every language', () => {
		const sections = (language: (typeof LANGUAGES)[number]) =>
			getOperatorPassTermsBlocks(language, OPERATOR_PASS_TERMS_VERSION)
				.filter((block) => block.kind === 'heading' || block.kind === 'subheading')
				.map((block) => block.text.split(' ')[0]);
		for (const language of LANGUAGES) {
			expect(sections(language), language).toEqual(sections('en-US'));
		}
	});

	it('has the same paragraphs, lists and form lines in Swedish as in English', () => {
		expect(blockKindsBySection('sv-SE', ALL_KINDS)).toEqual(blockKindsBySection('en-US', ALL_KINDS));
	});

	it('has the same lists and form lines in every language', () => {
		for (const language of LANGUAGES) {
			expect(blockKindsBySection(language, LIST_AND_FORM_KINDS), language).toEqual(
				blockKindsBySection('en-US', LIST_AND_FORM_KINDS),
			);
		}
	});

	it
		.runIf(MARKETING_DIR)
		.each(
			LANGUAGES.filter((language) => language !== 'pt-BR' || existsSync(join(MARKETING_DIR, MARKDOWN_PATHS[language]))),
		)('emails the %s terms word for word as the website publishes them', (language) => {
		const markdown = readFileSync(join(MARKETING_DIR, MARKDOWN_PATHS[language]), 'utf8');
		expect(OPERATOR_PASS_TERMS_TEXT[language]).toEqual(parseTermsMarkdown(markdown, language));
	});

	it.runIf(MARKETING_DIR)('versions the terms with the date the website shows as last updated', () => {
		const content = readFileSync(join(MARKETING_DIR, 'src/content/mod.rs'), 'utf8');
		const entry = content.match(/slug: "operator-pass-terms",[\s\S]*?last_updated: Some\("([0-9-]+)"\)/u);
		expect(entry?.[1]).toBe(OPERATOR_PASS_TERMS_VERSION);
	});

	it.each(LANGUAGES)('renders the %s terms as a PDF', async (language) => {
		const pdf = await renderOperatorPassTermsPdf(language, OPERATOR_PASS_TERMS_VERSION, 'https://fluxer.com');
		expect((await PDFDocument.load(pdf)).getPageCount()).toBeGreaterThan(1);
	});
});
