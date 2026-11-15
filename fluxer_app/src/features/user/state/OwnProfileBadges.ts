// SPDX-License-Identifier: AGPL-3.0-or-later

import {AccountScopedWork} from '@app/features/platform/state/AccountScopedWork';
import type {ProfileBadgePreviewResponse} from '@fluxer/schema/src/domains/user/ProfileBadgeSchemas';
import {makeAutoObservable} from 'mobx';

class OwnProfileBadges {
	value: ProfileBadgePreviewResponse | null = null;
	settingsKey: string | null = null;
	loadedForUserId: string | null = null;
	loading = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	beginLoad(userId: string): void {
		if (this.loadedForUserId !== userId) {
			this.value = null;
			this.settingsKey = null;
		}
		this.loading = true;
		this.loadedForUserId = userId;
	}

	setValue(userId: string, settingsKey: string, value: ProfileBadgePreviewResponse): void {
		this.value = value;
		this.settingsKey = settingsKey;
		this.loadedForUserId = userId;
		this.loading = false;
	}

	finishLoad(): void {
		this.loading = false;
	}

	valueFor(userId: string | null): ProfileBadgePreviewResponse | null {
		return userId !== null && this.loadedForUserId === userId ? this.value : null;
	}

	clear(): void {
		this.value = null;
		this.settingsKey = null;
		this.loadedForUserId = null;
		this.loading = false;
	}
}

const ownProfileBadges = new OwnProfileBadges();

AccountScopedWork.registerCancellation(() => ownProfileBadges.clear());

export default ownProfileBadges;
