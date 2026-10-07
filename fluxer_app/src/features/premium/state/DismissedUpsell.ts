// SPDX-License-Identifier: AGPL-3.0-or-later

import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {DismissedUpsellsSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {makeAutoObservable} from 'mobx';

class DismissedUpsell {
	pickerPremiumUpsellDismissed = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => this.initPersistence());
	}

	private async initPersistence(): Promise<void> {
		await makeSyncedField(this, {
			field: 'dismissedUpsells',
			schema: DismissedUpsellsSchema,
			persist: ['pickerPremiumUpsellDismissed'],
			toMessage: (s) => ({pickerPremium: s.pickerPremiumUpsellDismissed}),
			applyMessage: (s, m) => {
				s.pickerPremiumUpsellDismissed = m.pickerPremium;
			},
		});
	}

	dismissPickerPremiumUpsell(): void {
		this.pickerPremiumUpsellDismissed = true;
	}
}

export default new DismissedUpsell();
