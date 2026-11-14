// SPDX-License-Identifier: AGPL-3.0-or-later

import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {HiddenGuildListButtonsSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {makeAutoObservable} from 'mobx';

class HiddenGuildListButtons {
	downloadButtonHidden = false;
	helpButtonHidden = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => this.initPersistence());
	}

	private async initPersistence(): Promise<void> {
		await makeSyncedField(this, {
			field: 'hiddenGuildButtons',
			schema: HiddenGuildListButtonsSchema,
			persist: ['downloadButtonHidden', 'helpButtonHidden'],
			toMessage: (s) => ({
				downloadButton: s.downloadButtonHidden,
				helpButton: s.helpButtonHidden,
			}),
			applyMessage: (s, m) => {
				s.downloadButtonHidden = m.downloadButton;
				s.helpButtonHidden = m.helpButton;
			},
		});
	}

	hideDownloadButton(): void {
		this.downloadButtonHidden = true;
	}

	showDownloadButton(): void {
		this.downloadButtonHidden = false;
	}

	hideHelpButton(): void {
		this.helpButtonHidden = true;
	}

	showHelpButton(): void {
		this.helpButtonHidden = false;
	}
}

export default new HiddenGuildListButtons();
