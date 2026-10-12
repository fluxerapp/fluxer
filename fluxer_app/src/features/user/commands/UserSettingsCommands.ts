// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {http} from '@app/features/platform/transport/RestTransport';
import type {UserSettings} from '@app/features/user/state/UserSettings';
import UserSettingsState from '@app/features/user/state/UserSettings';

export type UserSettingsPatch = Partial<UserSettings>;

export async function update(settings: UserSettingsPatch): Promise<void> {
	await UserSettingsState.saveSettings(settings);
}

export async function updateVoiceActivitySharingDefault(value: boolean): Promise<void> {
	await http.put(Endpoints.USER_VOICE_ACTIVITY_SHARING, {body: {share_voice_activity: value}});
}
