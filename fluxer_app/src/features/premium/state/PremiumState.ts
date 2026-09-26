// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PremiumStateResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import {makeAutoObservable} from 'mobx';

class PremiumState {
	state: PremiumStateResponse | null = null;
	loading = false;
	loadedForUserId: string | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	beginLoad(userId: string): void {
		if (this.loadedForUserId !== userId) {
			this.state = null;
		}
		this.loading = true;
		this.loadedForUserId = userId;
	}

	setState(userId: string, state: PremiumStateResponse): void {
		this.state = state;
		this.loadedForUserId = userId;
		this.loading = false;
	}

	finishLoad(): void {
		this.loading = false;
	}

	clear(): void {
		this.state = null;
		this.loading = false;
		this.loadedForUserId = null;
	}
}

export default new PremiumState();
