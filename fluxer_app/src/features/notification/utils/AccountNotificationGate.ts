// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AccountPresenceIntent} from '@app/features/auth/state/AccountStorageContract';
import type {SoundType} from '@app/features/notification/utils/SoundUtils';
import type {Account} from '@app/features/platform/state/AuthSession';
import {StatusTypes} from '@fluxer/constants/src/StatusConstants';
import {decodeSyncedPreferencesLenient} from '@fluxer/schema/src/domains/user/SyncedPreferencesCodec';

export interface AccountNotificationPreferences {
	readonly notificationsEnabled: boolean;
	readonly soundEnabled: boolean;
}

interface AccountNotificationGateInput {
	readonly accountKey: string;
	readonly currentAccountKey: string | null;
	readonly account: Pick<Account, 'isValid' | 'presenceIntent'> | null;
	readonly streamerModeBlocksNotifications: boolean;
	readonly alreadyNotified: boolean;
	readonly preferences: AccountNotificationPreferences | null;
	readonly now: number;
}

function effectivePresenceStatus(intent: AccountPresenceIntent | null | undefined, now: number): string | null {
	if (intent == null) return null;
	if (intent.statusResetsAt != null && intent.statusResetsTo != null) {
		const resetsAt = Date.parse(intent.statusResetsAt);
		if (!Number.isNaN(resetsAt) && resetsAt <= now) {
			return intent.statusResetsTo;
		}
	}
	return intent.status;
}

export function shouldShowAccountNotification(input: AccountNotificationGateInput): boolean {
	if (input.streamerModeBlocksNotifications) return false;
	if (input.accountKey === input.currentAccountKey) return false;
	if (input.account === null || !input.account.isValid) return false;
	if (effectivePresenceStatus(input.account.presenceIntent, input.now) === StatusTypes.DND) return false;
	if (input.alreadyNotified) return false;
	return input.preferences === null || input.preferences.notificationsEnabled;
}

export function accountSoundEnabled(encodedPreferences: string | null, soundType: SoundType): boolean {
	if (encodedPreferences === null) return true;
	const sound = decodeSyncedPreferencesLenient(encodedPreferences).sound;
	if (sound === undefined) return true;
	if (sound.allSoundsDisabled) return false;
	return sound.disabledSounds[soundType] !== true;
}
