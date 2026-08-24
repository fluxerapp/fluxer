// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVProvider, IKVSubscription} from '@pkgs/kv_client/src/IKVProvider';
import {AdminRepository} from '../admin/AdminRepository';
import {BANNED_PHRASES_REFRESH_CHANNEL} from '../constants/ContentModeration';
import {Logger} from '../Logger';
import {buildPhraseMatchForms, canonicalizeStoredPhrase} from '../utils/PhraseBlocklistNormalization';

export class PhraseBlocklistCache {
	private rawPhrases: Array<string> = [];
	private rawPhraseSet = new Set<string>();
	private wordPhrases: Array<string> = [];
	private compactPhrases: Array<string> = [];
	private asciiWordPhrases: Array<string> = [];
	private asciiCompactPhrases: Array<string> = [];
	private isInitialized = false;
	private adminRepository = new AdminRepository();
	private kvClient: IKVProvider | null = null;
	private kvSubscription: IKVSubscription | null = null;
	private subscriberInitialized = false;
	private messageHandler: ((channel: string) => void) | null = null;
	private consecutiveFailures = 0;
	private readonly maxConsecutiveFailures = 5;

	setRefreshSubscriber(kvClient: IKVProvider | null): void {
		this.kvClient = kvClient;
	}

	async initialize(): Promise<void> {
		if (this.isInitialized) return;
		await this.refresh();
		this.isInitialized = true;
		this.setupSubscriber();
	}

	private setupSubscriber(): void {
		if (this.subscriberInitialized || !this.kvClient) return;
		const subscription = this.kvClient.duplicate();
		this.kvSubscription = subscription;
		this.messageHandler = (channel: string) => {
			if (channel === BANNED_PHRASES_REFRESH_CHANNEL) {
				this.refresh().catch((err) => {
					this.consecutiveFailures++;
					const message = err instanceof Error ? err.message : String(err);
					if (this.consecutiveFailures >= this.maxConsecutiveFailures) {
						Logger.error({error: message}, 'Failed to refresh phrase blocklist cache after notification');
					} else {
						Logger.warn({error: message}, 'Failed to refresh phrase blocklist cache after notification');
					}
				});
			}
		};
		subscription
			.connect()
			.then(() => subscription.subscribe(BANNED_PHRASES_REFRESH_CHANNEL))
			.then(() => {
				if (this.messageHandler) {
					subscription.on('message', this.messageHandler);
				}
			})
			.catch((error) => {
				Logger.error({error}, 'Failed to subscribe to phrase blocklist refresh channel');
			});
		this.subscriberInitialized = true;
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
			this.matchAny(forms.raw, this.rawPhrases) ||
			this.matchAny(forms.words, this.wordPhrases) ||
			this.matchAny(forms.compact, this.compactPhrases) ||
			this.matchAny(forms.asciiWords, this.asciiWordPhrases) ||
			this.matchAny(forms.asciiCompact, this.asciiCompactPhrases)
		);
	}

	private matchAny(text: string, phrases: Array<string>): boolean {
		if (!text || phrases.length === 0) return false;
		for (const phrase of phrases) {
			if (text.includes(phrase)) return true;
		}
		return false;
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
		this.wordPhrases = Array.from(wordPhraseSet);
		this.compactPhrases = Array.from(compactPhraseSet);
		this.asciiWordPhrases = Array.from(asciiWordPhraseSet);
		this.asciiCompactPhrases = Array.from(asciiCompactPhraseSet);
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
		this.shutdown();
		this.rawPhrases = [];
		this.rawPhraseSet = new Set();
		this.rebuildMatchers();
		this.kvClient = null;
		this.subscriberInitialized = false;
		this.consecutiveFailures = 0;
		this.isInitialized = false;
	}

	shutdown(): void {
		if (this.kvSubscription && this.messageHandler) {
			this.kvSubscription.off('message', this.messageHandler);
		}
		if (this.kvSubscription) {
			this.kvSubscription.disconnect();
			this.kvSubscription = null;
		}
		this.messageHandler = null;
	}
}

export const phraseBlocklistCache = new PhraseBlocklistCache();
