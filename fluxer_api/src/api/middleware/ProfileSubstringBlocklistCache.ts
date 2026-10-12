// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {BANNED_PROFILE_SUBSTRINGS_REFRESH_CHANNEL} from '@app/api/constants/ContentModeration';
import type {BannedProfileSubstringScope} from '@app/api/database/types/AdminArchiveTypes';
import {Logger} from '@app/api/Logger';
import {PhraseMatcherSet} from '@app/api/utils/PhraseMatcherSet';
import {RefreshedCache} from '@app/api/utils/RefreshedCache';

type ProfileScope = BannedProfileSubstringScope;
const PROFILE_SCOPES: ReadonlyArray<ProfileScope> = ['username', 'global_name', 'nickname', 'bio', 'pronouns'];
const PROFILE_NAME_SCOPES = new Set<ProfileScope>(['username', 'global_name', 'nickname']);

export class ProfileSubstringBlocklistCache extends RefreshedCache {
	private byScope = new Map<ProfileScope, PhraseMatcherSet>();
	private readonly adminRepository = new AdminRepository();
	constructor() {
		super({name: 'profile-substring blocklist cache', channels: [BANNED_PROFILE_SUBSTRINGS_REFRESH_CHANNEL]});
	}
	protected async load(): Promise<void> {
		const phrases = new Map<ProfileScope, Array<string>>(PROFILE_SCOPES.map((scope) => [scope, []]));
		for (const row of await this.adminRepository.loadAllBannedProfileSubstrings())
			phrases.get(row.scope as ProfileScope)?.push(row.substring);
		const next = new Map<ProfileScope, PhraseMatcherSet>();
		for (const [scope, values] of phrases) {
			const matcher = new PhraseMatcherSet();
			matcher.replaceAll(values);
			next.set(scope, matcher);
		}
		this.byScope = next;
		Logger.debug(
			{counts: Object.fromEntries(Array.from(next.entries()).map(([k, v]) => [k, v.size]))},
			'Profile-substring blocklist cache refreshed',
		);
	}
	private getMatchersForCheck(scope: ProfileScope): Array<PhraseMatcherSet> {
		if (PROFILE_NAME_SCOPES.has(scope))
			return Array.from(PROFILE_NAME_SCOPES)
				.map((nameScope) => this.byScope.get(nameScope))
				.filter((matcher): matcher is PhraseMatcherSet => !!matcher && matcher.size > 0);
		const matcher = this.byScope.get(scope);
		return matcher && matcher.size > 0 ? [matcher] : [];
	}
	containsBannedSubstring(scope: ProfileScope, text: string): boolean {
		return !!text && this.getMatchersForCheck(scope).some((matcher) => matcher.containsBannedPhrase(text));
	}
	isSubstringBanned(scope: ProfileScope, substring: string): boolean {
		return this.getMatchersForCheck(scope).some((matcher) => matcher.isPhraseBanned(substring));
	}
	add(scope: ProfileScope, substring: string): void {
		let matcher = this.byScope.get(scope);
		if (!matcher) {
			matcher = new PhraseMatcherSet();
			this.byScope.set(scope, matcher);
		}
		matcher.add(substring);
	}
	remove(scope: ProfileScope, substring: string): void {
		this.byScope.get(scope)?.remove(substring);
	}
	resetForTesting(): void {
		this.resetRefreshState();
		this.byScope = new Map();
	}
}

export const profileSubstringBlocklistCache = new ProfileSubstringBlocklistCache();
