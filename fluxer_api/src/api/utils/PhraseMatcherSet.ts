// SPDX-License-Identifier: AGPL-3.0-or-later

import {buildPhraseMatchForms, canonicalizeStoredPhrase} from '@app/api/utils/PhraseBlocklistNormalization';
import {SubstringMatcher} from '@app/api/utils/SubstringMatcher';

export class PhraseMatcherSet {
	private rawPhrases: Array<string> = [];

	private rawPhraseSet = new Set<string>();

	private rawMatcher: SubstringMatcher | null = null;

	private wordMatcher: SubstringMatcher | null = null;

	private compactMatcher: SubstringMatcher | null = null;

	private asciiWordMatcher: SubstringMatcher | null = null;

	private asciiCompactMatcher: SubstringMatcher | null = null;

	containsBannedPhrase(text: string): boolean {
		if (this.rawPhrases.length === 0) return false;
		const forms = buildPhraseMatchForms(text);
		return (
			this.rawMatcher?.test(forms.raw) === true ||
			this.wordMatcher?.test(forms.words) === true ||
			this.compactMatcher?.test(forms.compact) === true ||
			this.asciiWordMatcher?.test(forms.asciiWords) === true ||
			this.asciiCompactMatcher?.test(forms.asciiCompact) === true
		);
	}

	private rebuildMatchers(): void {
		const rawPhraseSet = new Set<string>();
		const wordPhraseSet = new Set<string>();
		const compactPhraseSet = new Set<string>();
		const asciiWordPhraseSet = new Set<string>();
		const asciiCompactPhraseSet = new Set<string>();
		for (const phrase of this.rawPhrases) {
			const canonical = canonicalizeStoredPhrase(phrase);
			if (!canonical) continue;
			const forms = buildPhraseMatchForms(canonical);
			rawPhraseSet.add(forms.raw);
			if (forms.words) wordPhraseSet.add(forms.words);
			if (forms.compact) compactPhraseSet.add(forms.compact);
			if (forms.asciiWords) asciiWordPhraseSet.add(forms.asciiWords);
			if (forms.asciiCompact) asciiCompactPhraseSet.add(forms.asciiCompact);
		}
		this.rawPhraseSet = rawPhraseSet;
		this.rawPhrases = Array.from(rawPhraseSet);
		this.rawMatcher = SubstringMatcher.fromPatterns(rawPhraseSet);
		this.wordMatcher = SubstringMatcher.fromPatterns(wordPhraseSet);
		this.compactMatcher = SubstringMatcher.fromPatterns(compactPhraseSet);
		this.asciiWordMatcher = SubstringMatcher.fromPatterns(asciiWordPhraseSet);
		this.asciiCompactMatcher = SubstringMatcher.fromPatterns(asciiCompactPhraseSet);
	}

	isPhraseBanned(phrase: string): boolean {
		const canonical = canonicalizeStoredPhrase(phrase);
		return !!canonical && this.rawPhraseSet.has(canonical);
	}

	add(phrase: string): void {
		const canonical = canonicalizeStoredPhrase(phrase);
		if (!canonical || this.rawPhraseSet.has(canonical)) return;
		this.rawPhrases.push(canonical);
		this.rebuildMatchers();
	}

	remove(phrase: string): void {
		const canonical = canonicalizeStoredPhrase(phrase);
		if (!canonical || !this.rawPhraseSet.has(canonical)) return;
		this.rawPhrases = this.rawPhrases.filter((item) => item !== canonical);
		this.rebuildMatchers();
	}

	get size(): number {
		return this.rawPhraseSet.size;
	}

	replaceAll(phrases: Iterable<string>): void {
		this.rawPhrases = Array.from(phrases);
		this.rebuildMatchers();
	}
}
