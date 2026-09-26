// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {BANNED_PHRASES_REFRESH_CHANNEL} from '@app/api/constants/ContentModeration';
import {Logger} from '@app/api/Logger';
import {buildPhraseMatchForms, canonicalizeStoredPhrase} from '@app/api/utils/PhraseBlocklistNormalization';
import {RefreshSubscription} from '@app/api/utils/RefreshSubscription';
import {SubstringMatcher} from '@app/api/utils/SubstringMatcher';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

export class PhraseBlocklistCache {
	private rawPhrases: Array<string> = [];
	private rawPhraseSet = new Set<string>();
	private rawMatcher: SubstringMatcher | null = null;
	private wordMatcher: SubstringMatcher | null = null;
	private compactMatcher: SubstringMatcher | null = null;
	private asciiWordMatcher: SubstringMatcher | null = null;
	private asciiCompactMatcher: SubstringMatcher | null = null;
	private adminRepository = new AdminRepository();
	private kvClient: IKVProvider | null = null;
	private consecutiveFailures = 0;
	private readonly maxConsecutiveFailures = 5;
	private readonly refreshSubscription = new RefreshSubscription({
		name: 'phrase blocklist cache',
		channels: [BANNED_PHRASES_REFRESH_CHANNEL],
		refresh: () => this.refresh(),
		onRefreshError: (err) => {
			this.consecutiveFailures++;
			const message = err instanceof Error ? err.message : String(err);
			if (this.consecutiveFailures >= this.maxConsecutiveFailures) {
				Logger.error({error: message}, 'Failed to refresh phrase blocklist cache after notification');
			} else {
				Logger.warn({error: message}, 'Failed to refresh phrase blocklist cache after notification');
			}
		},
	});

	setRefreshSubscriber(kvClient: IKVProvider | null): void {
		this.kvClient = kvClient;
	}

	initialize(): Promise<void> {
		return this.refreshSubscription.start(this.kvClient);
	}

	async refresh(): Promise<void> {
		const rows = await this.adminRepository.loadAllBannedPhrases();
		const next: Array<string> = [];
		for (const phrase of rows) {
			const canonical = canonicalizeStoredPhrase(phrase);
			if (canonical) next.push(canonical);
		}
		this.rawPhrases = next;
		this.rebuildMatchers();
		this.consecutiveFailures = 0;
		Logger.debug({count: this.rawPhrases.length}, 'Phrase blocklist cache refreshed');
	}

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

	resetForTesting(): void {
		void this.shutdown().catch((error) => {
			Logger.error({error}, 'Failed to shut down phrase blocklist cache');
		});
		this.rawPhrases = [];
		this.rawPhraseSet = new Set();
		this.rebuildMatchers();
		this.kvClient = null;
		this.consecutiveFailures = 0;
	}

	shutdown(): Promise<void> {
		return this.refreshSubscription.stop();
	}
}

export const phraseBlocklistCache = new PhraseBlocklistCache();
