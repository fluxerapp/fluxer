// SPDX-License-Identifier: AGPL-3.0-or-later

import {makeAutoObservable} from 'mobx';

class ActiveIframeEmbed {
	activeId: string | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	isActive(id: string): boolean {
		return this.activeId === id;
	}

	claim(id: string): void {
		this.activeId = id;
	}

	release(id: string): void {
		if (this.activeId === id) {
			this.activeId = null;
		}
	}
}

export default new ActiveIframeEmbed();
