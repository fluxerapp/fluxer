// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {BANNED_PROFILE_SUBSTRINGS_REFRESH_CHANNEL} from '@app/api/constants/ContentModeration';
import type {BannedProfileSubstringScope} from '@app/api/database/types/AdminArchiveTypes';
import {Logger} from '@app/api/Logger';
import {buildPhraseMatchForms, canonicalizeStoredPhrase} from '@app/api/utils/PhraseBlocklistNormalization';
import {RefreshSubscription} from '@app/api/utils/RefreshSubscription';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

type ProfileScope = BannedProfileSubstringScope;

const PROFILE_SCOPES: ReadonlyArray<ProfileScope> = ['username', 'global_name', 'nickname', 'bio', 'pronouns'];
const PROFILE_NAME_SCOPES = new Set<ProfileScope>(['username', 'global_name', 'nickname']);

interface ScopeMatchers {
	raw: Array<string>;
	words: Array<string>;
	compact: Array<string>;
	asciiWords: Array<string>;
	asciiCompact: Array<string>;
	rawSet: Set<string>;
}

function emptyMatchers(): ScopeMatchers {
	return {raw: [], words: [], compact: [], asciiWords: [], asciiCompact: [], rawSet: new Set()};
}

export class ProfileSubstringBlocklistCache {
	private byScope: Map<ProfileScope, ScopeMatchers> = new Map();
	private adminRepository = new AdminRepository();
	private kvClient: IKVProvider | null = null;
	private consecutiveFailures = 0;
	private readonly maxConsecutiveFailures = 5;
	private readonly refreshSubscription = new RefreshSubscription({
		name: 'profile-substring blocklist cache',
		channels: [BANNED_PROFILE_SUBSTRINGS_REFRESH_CHANNEL],
		refresh: () => this.refresh(),
		onRefreshError: (err) => {
			this.consecutiveFailures++;
			const message = err instanceof Error ? err.message : String(err);
			if (this.consecutiveFailures >= this.maxConsecutiveFailures) {
				Logger.error({error: message}, 'Failed to refresh profile-substring blocklist cache after notification');
			} else {
				Logger.warn({error: message}, 'Failed to refresh profile-substring blocklist cache after notification');
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
		const rows = await this.adminRepository.loadAllBannedProfileSubstrings();
		const next = new Map<ProfileScope, ScopeMatchers>();
		for (const scope of PROFILE_SCOPES) {
			next.set(scope, emptyMatchers());
		}
		for (const row of rows) {
			const scope = row.scope as ProfileScope;
			const matchers = next.get(scope);
			if (!matchers) continue;
			this.addToMatchers(matchers, row.substring);
		}
		this.byScope = next;
		this.consecutiveFailures = 0;
		Logger.debug(
			{counts: Object.fromEntries(Array.from(next.entries()).map(([k, v]) => [k, v.raw.length]))},
			'Profile-substring blocklist cache refreshed',
		);
	}

	private addToMatchers(matchers: ScopeMatchers, value: string): void {
		const canonical = canonicalizeStoredPhrase(value);
		if (!canonical || matchers.rawSet.has(canonical)) return;
		const forms = buildPhraseMatchForms(canonical);
		matchers.rawSet.add(forms.raw);
		matchers.raw.push(forms.raw);
		if (forms.words) matchers.words.push(forms.words);
		if (forms.compact) matchers.compact.push(forms.compact);
		if (forms.asciiWords) matchers.asciiWords.push(forms.asciiWords);
		if (forms.asciiCompact) matchers.asciiCompact.push(forms.asciiCompact);
	}

	containsBannedSubstring(scope: ProfileScope, text: string): boolean {
		if (!text) return false;
		const forms = buildPhraseMatchForms(text);
		const scopeMatchers = this.getMatchersForCheck(scope);
		return (
			this.matchAnyMatcher(forms.raw, scopeMatchers, (matchers) => matchers.raw) ||
			this.matchAnyMatcher(forms.words, scopeMatchers, (matchers) => matchers.words) ||
			this.matchAnyMatcher(forms.compact, scopeMatchers, (matchers) => matchers.compact) ||
			this.matchAnyMatcher(forms.asciiWords, scopeMatchers, (matchers) => matchers.asciiWords) ||
			this.matchAnyMatcher(forms.asciiCompact, scopeMatchers, (matchers) => matchers.asciiCompact)
		);
	}

	private getMatchersForCheck(scope: ProfileScope): Array<ScopeMatchers> {
		if (PROFILE_NAME_SCOPES.has(scope)) {
			return Array.from(PROFILE_NAME_SCOPES)
				.map((nameScope) => this.byScope.get(nameScope))
				.filter((matchers): matchers is ScopeMatchers => !!matchers && matchers.raw.length > 0);
		}
		const matchers = this.byScope.get(scope);
		return matchers && matchers.raw.length > 0 ? [matchers] : [];
	}

	private matchAnyMatcher(
		text: string,
		matchers: Array<ScopeMatchers>,
		getPhrases: (matchers: ScopeMatchers) => Array<string>,
	): boolean {
		if (!text || matchers.length === 0) return false;
		for (const matcher of matchers) {
			if (this.matchAny(text, getPhrases(matcher))) return true;
		}
		return false;
	}

	private matchAny(text: string, phrases: Array<string>): boolean {
		if (!text || phrases.length === 0) return false;
		for (const phrase of phrases) {
			if (text.includes(phrase)) return true;
		}
		return false;
	}

	add(scope: ProfileScope, substring: string): void {
		let matchers = this.byScope.get(scope);
		if (!matchers) {
			matchers = emptyMatchers();
			this.byScope.set(scope, matchers);
		}
		this.addToMatchers(matchers, substring);
	}

	remove(scope: ProfileScope, substring: string): void {
		const matchers = this.byScope.get(scope);
		if (!matchers) return;
		const canonical = canonicalizeStoredPhrase(substring);
		if (!canonical || !matchers.rawSet.has(canonical)) return;
		matchers.rawSet.delete(canonical);
		const rebuilt = emptyMatchers();
		for (const phrase of matchers.rawSet) {
			this.addToMatchers(rebuilt, phrase);
		}
		this.byScope.set(scope, rebuilt);
	}

	isSubstringBanned(scope: ProfileScope, substring: string): boolean {
		const canonical = canonicalizeStoredPhrase(substring);
		return !!canonical && this.getMatchersForCheck(scope).some((matchers) => matchers.rawSet.has(canonical));
	}

	resetForTesting(): void {
		void this.shutdown().catch((error) => {
			Logger.error({error}, 'Failed to shut down profile-substring blocklist cache');
		});
		this.byScope = new Map();
		this.kvClient = null;
		this.consecutiveFailures = 0;
	}

	shutdown(): Promise<void> {
		return this.refreshSubscription.stop();
	}
}

export const profileSubstringBlocklistCache = new ProfileSubstringBlocklistCache();
