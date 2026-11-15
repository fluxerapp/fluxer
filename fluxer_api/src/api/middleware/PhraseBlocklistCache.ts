// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {BANNED_PHRASES_REFRESH_CHANNEL} from '@app/api/constants/ContentModeration';
import {Logger} from '@app/api/Logger';
import {PhraseMatcherSet} from '@app/api/utils/PhraseMatcherSet';
import {RefreshedCache} from '@app/api/utils/RefreshedCache';

export class PhraseBlocklistCache extends RefreshedCache {
	private readonly matcher = new PhraseMatcherSet();
	private readonly adminRepository = new AdminRepository();
	constructor() {
		super({name: 'phrase blocklist cache', channels: [BANNED_PHRASES_REFRESH_CHANNEL]});
	}
	protected async load(): Promise<void> {
		this.matcher.replaceAll(await this.adminRepository.loadAllBannedPhrases());
		Logger.debug({count: this.matcher.size}, 'Phrase blocklist cache refreshed');
	}
	containsBannedPhrase(text: string): boolean {
		return this.matcher.containsBannedPhrase(text);
	}
	isPhraseBanned(phrase: string): boolean {
		return this.matcher.isPhraseBanned(phrase);
	}
	add(phrase: string): void {
		this.matcher.add(phrase);
	}
	remove(phrase: string): void {
		this.matcher.remove(phrase);
	}
	get size(): number {
		return this.matcher.size;
	}
	resetForTesting(): void {
		this.resetRefreshState();
		this.matcher.replaceAll([]);
	}
}

export const phraseBlocklistCache = new PhraseBlocklistCache();
