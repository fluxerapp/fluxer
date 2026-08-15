// SPDX-License-Identifier: AGPL-3.0-or-later

import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {ChatInputSettingsSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {makeAutoObservable} from 'mobx';

class ChatInputSettingsStore {
	convertEmoticons = false;
	renderComposerAsPlainText = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void makeSyncedField(this, {
			field: 'chatInput',
			schema: ChatInputSettingsSchema,
			persist: ['convertEmoticons'],
			toMessage: (settings) => ({
				convertEmoticons: settings.convertEmoticons ? true : undefined,
			}),
			applyMessage: (settings, message) => {
				settings.convertEmoticons = message.convertEmoticons ?? false;
			},
		});
	}

	setConvertEmoticons(value: boolean): void {
		this.convertEmoticons = value;
	}
}

export default new ChatInputSettingsStore();
