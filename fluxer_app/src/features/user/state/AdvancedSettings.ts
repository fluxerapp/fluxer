// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage from '@app/features/platform/state/PersistentStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {makeAutoObservable} from 'mobx';

export const UNREAD_BADGE_CUSTOMIZATION_STORAGE_KEY = 'AdvancedSettings:unreadBadgeCustomizationEnabled';
const KEEP_ATTACHMENTS_ON_EMPTY_MESSAGE_EDIT_STORAGE_KEY = 'AdvancedSettings:keepAttachmentsOnEmptyMessageEdit';
const EXPRESSION_CLONE_SHORTCUTS_STORAGE_KEY = 'AdvancedSettings:expressionCloneShortcutsEnabled';
const HIDE_BLOCKED_MESSAGES_STORAGE_KEY = 'AdvancedSettings:hideBlockedMessages';

const logger = new Logger('AdvancedSettings');

function readStoredBoolean(key: string): boolean {
	return parseStoredBoolean(AppStorage.getItem(key));
}

export function parseStoredBoolean(raw: string | null): boolean {
	if (raw == null) return false;
	try {
		const parsed: unknown = JSON.parse(raw);
		return parsed === true;
	} catch (error) {
		if (!(error instanceof SyntaxError)) {
			logger.warn('Failed to decode a stored advanced setting', error);
		}
		return raw === 'true';
	}
}

class AdvancedSettings {
	unreadBadgeCustomizationEnabled = readStoredBoolean(UNREAD_BADGE_CUSTOMIZATION_STORAGE_KEY);
	keepAttachmentsOnEmptyMessageEdit = readStoredBoolean(KEEP_ATTACHMENTS_ON_EMPTY_MESSAGE_EDIT_STORAGE_KEY);
	expressionCloneShortcutsEnabled = readStoredBoolean(EXPRESSION_CLONE_SHORTCUTS_STORAGE_KEY);
	hideBlockedMessages = readStoredBoolean(HIDE_BLOCKED_MESSAGES_STORAGE_KEY);

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		if (typeof AppStorage.subscribe !== 'function') {
			return;
		}
		AppStorage.subscribe(
			() => {
				this.unreadBadgeCustomizationEnabled = readStoredBoolean(UNREAD_BADGE_CUSTOMIZATION_STORAGE_KEY);
			},
			{key: UNREAD_BADGE_CUSTOMIZATION_STORAGE_KEY, source: 'external'},
		);
		AppStorage.subscribe(
			() => {
				this.keepAttachmentsOnEmptyMessageEdit = readStoredBoolean(KEEP_ATTACHMENTS_ON_EMPTY_MESSAGE_EDIT_STORAGE_KEY);
			},
			{key: KEEP_ATTACHMENTS_ON_EMPTY_MESSAGE_EDIT_STORAGE_KEY, source: 'external'},
		);
		AppStorage.subscribe(
			() => {
				this.expressionCloneShortcutsEnabled = readStoredBoolean(EXPRESSION_CLONE_SHORTCUTS_STORAGE_KEY);
			},
			{key: EXPRESSION_CLONE_SHORTCUTS_STORAGE_KEY, source: 'external'},
		);
		AppStorage.subscribe(
			() => {
				this.hideBlockedMessages = readStoredBoolean(HIDE_BLOCKED_MESSAGES_STORAGE_KEY);
			},
			{key: HIDE_BLOCKED_MESSAGES_STORAGE_KEY, source: 'external'},
		);
	}

	setUnreadBadgeCustomizationEnabled(value: boolean): void {
		if (this.unreadBadgeCustomizationEnabled === value) return;
		this.unreadBadgeCustomizationEnabled = value;
		AppStorage.setItem(UNREAD_BADGE_CUSTOMIZATION_STORAGE_KEY, JSON.stringify(value));
	}

	setKeepAttachmentsOnEmptyMessageEdit(value: boolean): void {
		if (this.keepAttachmentsOnEmptyMessageEdit === value) return;
		this.keepAttachmentsOnEmptyMessageEdit = value;
		AppStorage.setItem(KEEP_ATTACHMENTS_ON_EMPTY_MESSAGE_EDIT_STORAGE_KEY, JSON.stringify(value));
	}

	setExpressionCloneShortcutsEnabled(value: boolean): void {
		if (this.expressionCloneShortcutsEnabled === value) return;
		this.expressionCloneShortcutsEnabled = value;
		AppStorage.setItem(EXPRESSION_CLONE_SHORTCUTS_STORAGE_KEY, JSON.stringify(value));
	}

	setHideBlockedMessages(value: boolean): void {
		if (this.hideBlockedMessages === value) return;
		this.hideBlockedMessages = value;
		AppStorage.setItem(HIDE_BLOCKED_MESSAGES_STORAGE_KEY, JSON.stringify(value));
	}
}

export default new AdvancedSettings();
