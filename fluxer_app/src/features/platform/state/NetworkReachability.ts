// SPDX-License-Identifier: AGPL-3.0-or-later

import {makeAutoObservable} from 'mobx';

class NetworkReachability {
	online = typeof navigator === 'undefined' ? true : navigator.onLine;

	constructor() {
		makeAutoObservable(this);
		if (typeof window !== 'undefined') {
			window.addEventListener('online', () => this.setOnline(true));
			window.addEventListener('offline', () => this.setOnline(false));
		}
	}

	private setOnline(online: boolean): void {
		this.online = online;
	}
}

export default new NetworkReachability();
