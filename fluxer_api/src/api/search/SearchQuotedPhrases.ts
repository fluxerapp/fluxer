// SPDX-License-Identifier: AGPL-3.0-or-later

const QUOTE_CHARS = new Set(['"', '“', '”', '„', '‟', '″', '«', '»', '＂']);

function extractQuotedPhrases(input: string): {rest: string; phrases: Array<string>} {
	const phrases: Array<string> = [];
	let rest = '';
	let i = 0;
	while (i < input.length) {
		if (!QUOTE_CHARS.has(input[i])) {
			rest += input[i];
			i++;
			continue;
		}
		i++;
		let phrase = '';
		while (i < input.length && !QUOTE_CHARS.has(input[i])) {
			phrase += input[i];
			i++;
		}
		i++;
		const trimmed = phrase.trim();
		if (trimmed) phrases.push(trimmed);
		rest += ' ';
	}
	return {rest: rest.replace(/\s+/g, ' ').trim(), phrases};
}

function stripWrappingQuotes(phrase: string): string {
	let start = 0;
	let end = phrase.length;
	while (start < end && QUOTE_CHARS.has(phrase[start])) start++;
	while (end > start && QUOTE_CHARS.has(phrase[end - 1])) end--;
	return phrase.slice(start, end).trim();
}

export function normalizeQuotedPhrases<T extends {content?: string; exact_phrases?: Array<string>}>(params: T): T {
	const exactPhrases = (params.exact_phrases ?? []).map(stripWrappingQuotes).filter(Boolean);
	let content = params.content;
	if (content) {
		const extracted = extractQuotedPhrases(content);
		exactPhrases.push(...extracted.phrases);
		content = extracted.rest || undefined;
	}
	return {
		...params,
		content,
		exact_phrases: exactPhrases.length > 0 ? [...new Set(exactPhrases)] : undefined,
	};
}
