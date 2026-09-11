// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage from '@app/features/platform/state/PersistentStorage';
import {makeAutoObservable} from 'mobx';

const UNREAD_BADGE_CUSTOMIZATION_STORAGE_KEY = 'AdvancedSettings:unreadBadgeCustomizationEnabled';
const KEEP_ATTACHMENTS_ON_EMPTY_MESSAGE_EDIT_STORAGE_KEY = 'AdvancedSettings:keepAttachmentsOnEmptyMessageEdit';
const EXPRESSION_CLONE_SHORTCUTS_STORAGE_KEY = 'AdvancedSettings:expressionCloneShortcutsEnabled';

function readStoredBoolean(key: string, defaultValue = false): boolean {
	const raw = AppStorage.getItem(key);
	if (raw == null) return defaultValue;
	try {
		const parsed = JSON.parse(raw);
		return typeof parsed === 'boolean' ? parsed : defaultValue;
	} catch (_error) {
		return raw === 'true' ? true : raw === 'false' ? false : defaultValue;
	}
}

class AdvancedSettings {
	unreadBadgeCustomizationEnabled = readStoredBoolean(UNREAD_BADGE_CUSTOMIZATION_STORAGE_KEY);
	keepAttachmentsOnEmptyMessageEdit = readStoredBoolean(KEEP_ATTACHMENTS_ON_EMPTY_MESSAGE_EDIT_STORAGE_KEY);
	expressionCloneShortcutsEnabled = readStoredBoolean(EXPRESSION_CLONE_SHORTCUTS_STORAGE_KEY);

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		if (typeof AppStorage.subscribe !== 'function') {
			return;
		}
		AppStorage.subscribe(
			(event) => {
				this.unreadBadgeCustomizationEnabled = event.newValue === null ? false : readStoredBoolean(event.key ?? '');
			},
			{key: UNREAD_BADGE_CUSTOMIZATION_STORAGE_KEY, source: 'external'},
		);
		AppStorage.subscribe(
			(event) => {
				this.keepAttachmentsOnEmptyMessageEdit = event.newValue === null ? false : readStoredBoolean(event.key ?? '');
			},
			{key: KEEP_ATTACHMENTS_ON_EMPTY_MESSAGE_EDIT_STORAGE_KEY, source: 'external'},
		);
		AppStorage.subscribe(
			(event) => {
				this.expressionCloneShortcutsEnabled = event.newValue === null ? false : readStoredBoolean(event.key ?? '');
			},
			{key: EXPRESSION_CLONE_SHORTCUTS_STORAGE_KEY, source: 'external'},
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
}

export default new AdvancedSettings();
