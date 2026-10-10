// SPDX-License-Identifier: AGPL-3.0-or-later

import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {VoicePromptsStateSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {makeAutoObservable} from 'mobx';

class VoicePrompts {
	skipHideOwnCameraConfirm = false;
	skipHideOwnScreenShareConfirm = false;
	skipP2pJoinConfirm = false;

	constructor() {
		makeAutoObservable(
			this,
			{
				getSkipHideOwnCameraConfirm: false,
				getSkipHideOwnScreenShareConfirm: false,
				getSkipP2pJoinConfirm: false,
			},
			{autoBind: true},
		);
		initializeStore(this, () => this.initPersistence());
	}

	private async initPersistence(): Promise<void> {
		await makeSyncedField(this, {
			field: 'voicePrompts',
			schema: VoicePromptsStateSchema,
			persist: ['skipHideOwnCameraConfirm', 'skipHideOwnScreenShareConfirm', 'skipP2pJoinConfirm'],
			toMessage: (s) => ({
				skipHideOwnCameraConfirm: s.skipHideOwnCameraConfirm,
				skipHideOwnScreenshareConfirm: s.skipHideOwnScreenShareConfirm,
				skipP2pJoinConfirm: s.skipP2pJoinConfirm,
			}),
			applyMessage: (s, m) => {
				s.skipHideOwnCameraConfirm = m.skipHideOwnCameraConfirm;
				s.skipHideOwnScreenShareConfirm = m.skipHideOwnScreenshareConfirm;
				s.skipP2pJoinConfirm = m.skipP2pJoinConfirm;
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

	getSkipP2pJoinConfirm(): boolean {
		return this.skipP2pJoinConfirm;
	}

	setSkipP2pJoinConfirm(value: boolean): void {
		this.skipP2pJoinConfirm = value;
	}
}

export default new VoicePrompts();
