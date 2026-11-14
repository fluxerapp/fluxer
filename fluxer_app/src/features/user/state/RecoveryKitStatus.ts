// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage from '@app/features/platform/state/PersistentStorage';
import type {RecoveryKitStatusResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {makeAutoObservable} from 'mobx';

const REMINDER_DISMISSED_STORAGE_KEY_PREFIX = 'RecoveryKitReminderDismissed:';
const PROMPT_SHOWN_STORAGE_KEY_PREFIX = 'RecoveryKitPromptShown:';

interface RecoveryKitStatusEntry {
	hasRecoveryKit: boolean;
	createdAt: string | null;
}

class RecoveryKitStatus {
	private entries = new Map<string, RecoveryKitStatusEntry>();
	private dismissedReminders = new Set<string>();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	setStatus(userId: string, status: RecoveryKitStatusResponse): void {
		this.entries.set(userId, {hasRecoveryKit: status.has_recovery_kit, createdAt: status.created_at});
	}

	get(userId: string): RecoveryKitStatusEntry | null {
		return this.entries.get(userId) ?? null;
	}

	isReminderDismissed(userId: string): boolean {
		return (
			this.dismissedReminders.has(userId) ||
			AppStorage.getItem(`${REMINDER_DISMISSED_STORAGE_KEY_PREFIX}${userId}`) === 'true'
		);
	}

	dismissReminder(userId: string): void {
		this.dismissedReminders.add(userId);
		AppStorage.setItem(`${REMINDER_DISMISSED_STORAGE_KEY_PREFIX}${userId}`, 'true');
	}

	hasShownPrompt(userId: string): boolean {
		return AppStorage.getItem(`${PROMPT_SHOWN_STORAGE_KEY_PREFIX}${userId}`) === 'true';
	}

	markPromptShown(userId: string): void {
		AppStorage.setItem(`${PROMPT_SHOWN_STORAGE_KEY_PREFIX}${userId}`, 'true');
	}
}

export default new RecoveryKitStatus();
