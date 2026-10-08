// SPDX-License-Identifier: AGPL-3.0-or-later

import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {ensureSourceMapsModule} from '@app/features/platform/utils/SourceMapsModule';
import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {makeAutoObservable} from 'mobx';

class SourceMaps {
	enabled = false;
	downloading = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => this.initPersistence());
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'SourceMaps', ['enabled']);
		if (this.enabled) {
			await this.download();
		}
	}

	private async download(): Promise<void> {
		this.downloading = true;
		try {
			await ensureSourceMapsModule();
		} finally {
			this.downloading = false;
		}
	}

	async setEnabled(value: boolean): Promise<void> {
		this.enabled = value;
		if (value) {
			await this.download();
		}
	}
}

export default new SourceMaps();
