// SPDX-License-Identifier: AGPL-3.0-or-later

import {makeAutoObservable} from 'mobx';

class Sudo {
	private token: string | null = null;
	private expiresAt: number | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	getValidToken = (): string | null => {
		if (this.token && this.expiresAt && Date.now() < this.expiresAt) {
			return this.token;
		}
		return null;
	};

	setToken(token: string): void {
		this.token = token;
		this.expiresAt = Date.now() + 4.5 * 60 * 1000;
	}

	clearToken(): void {
		this.token = null;
		this.expiresAt = null;
	}
}

export default new Sudo();
