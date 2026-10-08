// SPDX-License-Identifier: AGPL-3.0-or-later

import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {VoicePromptsStateSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {makeAutoObservable} from 'mobx';

class VoicePrompts {
	skipHideOwnCameraConfirm = false;
	skipHideOwnScreenShareConfirm = false;

	constructor() {
		makeAutoObservable(
			this,
			{getSkipHideOwnCameraConfirm: false, getSkipHideOwnScreenShareConfirm: false},
			{autoBind: true},
		);
		initializeStore(this, () => this.initPersistence());
	}

	private async initPersistence(): Promise<void> {
		await makeSyncedField(this, {
			field: 'voicePrompts',
			schema: VoicePromptsStateSchema,
			persist: ['skipHideOwnCameraConfirm', 'skipHideOwnScreenShareConfirm'],
			toMessage: (s) => ({
				skipHideOwnCameraConfirm: s.skipHideOwnCameraConfirm,
				skipHideOwnScreenshareConfirm: s.skipHideOwnScreenShareConfirm,
			}),
			applyMessage: (s, m) => {
				s.skipHideOwnCameraConfirm = m.skipHideOwnCameraConfirm;
				s.skipHideOwnScreenShareConfirm = m.skipHideOwnScreenshareConfirm;
			},
		});
	}

	getSkipHideOwnCameraConfirm(): boolean {
		return this.skipHideOwnCameraConfirm;
	}

	setSkipHideOwnCameraConfirm(value: boolean): void {
		this.skipHideOwnCameraConfirm = value;
	}

	getSkipHideOwnScreenShareConfirm(): boolean {
		return this.skipHideOwnScreenShareConfirm;
	}

	setSkipHideOwnScreenShareConfirm(value: boolean): void {
		this.skipHideOwnScreenShareConfirm = value;
	}
}

export default new VoicePrompts();
