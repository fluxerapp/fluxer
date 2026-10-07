// SPDX-License-Identifier: AGPL-3.0-or-later

import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {PublicUserFlags} from '@fluxer/constants/src/UserConstants';
import {LocalUserSpamOverridesSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {makeAutoObservable} from 'mobx';

class LocalUserSpamOverride {
	localSpammerUserIds = new Set<string>();
	localNotSpammerUserIds = new Set<string>();
	version = 0;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => this.initPersistence());
	}

	private async initPersistence(): Promise<void> {
		await makeSyncedField(this, {
			field: 'localSpamOverrides',
			schema: LocalUserSpamOverridesSchema,
			persist: ['localSpammerUserIds', 'localNotSpammerUserIds'],
			toMessage: (s) => ({
				spammerUserIds: Array.from(s.localSpammerUserIds),
				notSpammerUserIds: Array.from(s.localNotSpammerUserIds),
			}),
			applyMessage: (s, m) => {
				s.localSpammerUserIds = new Set(m.spammerUserIds);
				s.localNotSpammerUserIds = new Set(m.notSpammerUserIds);
			},
		});
	}

	private incrementVersion(): void {
		this.version += 1;
	}

	isServerSpammer(userFlags: number): boolean {
		return (userFlags & PublicUserFlags.SPAMMER) === PublicUserFlags.SPAMMER;
	}

	isLocallyMarkedSpammer(userId: string): boolean {
		return this.localSpammerUserIds.has(userId);
	}

	isLocallyMarkedNotSpammer(userId: string): boolean {
		return this.localNotSpammerUserIds.has(userId);
	}

	isUserMarkedAsSpammer(userId: string, userFlags: number): boolean {
		if (this.isLocallyMarkedNotSpammer(userId)) {
			return false;
		}
		if (this.isLocallyMarkedSpammer(userId)) {
			return true;
		}
		return this.isServerSpammer(userFlags);
	}

	markAsSpammer(userId: string): void {
		const hadSpammer = this.localSpammerUserIds.has(userId);
		const hadNotSpammer = this.localNotSpammerUserIds.has(userId);
		this.localNotSpammerUserIds.delete(userId);
		this.localSpammerUserIds.add(userId);
		if (!hadSpammer || hadNotSpammer) {
			this.incrementVersion();
		}
	}

	markAsNotSpammer(userId: string): void {
		const hadSpammer = this.localSpammerUserIds.has(userId);
		const hadNotSpammer = this.localNotSpammerUserIds.has(userId);
		this.localSpammerUserIds.delete(userId);
		this.localNotSpammerUserIds.add(userId);
		if (hadSpammer || !hadNotSpammer) {
			this.incrementVersion();
		}
	}

	clearOverride(userId: string): void {
		const hadOverride = this.localSpammerUserIds.delete(userId) || this.localNotSpammerUserIds.delete(userId);
		if (hadOverride) {
			this.incrementVersion();
		}
	}

	clearAllOverrides(): void {
		if (this.localSpammerUserIds.size === 0 && this.localNotSpammerUserIds.size === 0) {
			return;
		}
		this.localSpammerUserIds.clear();
		this.localNotSpammerUserIds.clear();
		this.incrementVersion();
	}
}

export default new LocalUserSpamOverride();
