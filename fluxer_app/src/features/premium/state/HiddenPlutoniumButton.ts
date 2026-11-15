// SPDX-License-Identifier: AGPL-3.0-or-later

import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {makeAutoObservable} from 'mobx';

class HiddenPlutoniumButton {
	hidden = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => this.initPersistence());
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'HiddenPlutoniumButton', ['hidden']);
	}

	hide(): void {
		this.hidden = true;
	}

	show(): void {
		this.hidden = false;
	}
}

export default new HiddenPlutoniumButton();
