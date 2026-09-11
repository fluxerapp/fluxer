// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage from '@app/features/platform/state/PersistentStorage';
import {describe, expect, it} from 'vitest';

const KEEP_ATTACHMENTS_ON_EMPTY_MESSAGE_EDIT_STORAGE_KEY = 'AdvancedSettings:keepAttachmentsOnEmptyMessageEdit';

AppStorage.setItem(KEEP_ATTACHMENTS_ON_EMPTY_MESSAGE_EDIT_STORAGE_KEY, JSON.stringify(true));

const {default: AdvancedSettings} = await import('./AdvancedSettings');

describe('Advanced settings', () => {
	it('rehydrates the stored attachment edit preference', () => {
		expect(AdvancedSettings.keepAttachmentsOnEmptyMessageEdit).toBe(true);
	});

	it('stores the attachment edit preference under its own key', () => {
		AdvancedSettings.setKeepAttachmentsOnEmptyMessageEdit(false);
		expect(AppStorage.getItem(KEEP_ATTACHMENTS_ON_EMPTY_MESSAGE_EDIT_STORAGE_KEY)).toBe('false');
		expect(AdvancedSettings.unreadBadgeCustomizationEnabled).toBe(false);
		expect(AppStorage.getItem('AdvancedSettings:unreadBadgeCustomizationEnabled')).toBeNull();
	});
});
