// SPDX-License-Identifier: AGPL-3.0-or-later

import AdvancedSettings from '@app/features/user/state/AdvancedSettings';
import {afterEach, describe, expect, it} from 'vitest';
import {
	buildExistingAttachmentEditReferences,
	canSubmitEmptyMessageEdit,
	isAttachmentOnlyMessage,
} from './MessageEditContentUtils';

describe('Message edit content utils', () => {
	afterEach(() => {
		AdvancedSettings.setKeepAttachmentsOnEmptyMessageEdit(false);
	});
	it('keeps empty edits delete-oriented for messages without attachments', () => {
		expect(canSubmitEmptyMessageEdit({attachments: []})).toBe(false);
	});
	it('keeps empty edits delete-oriented for messages with attachments by default', () => {
		expect(canSubmitEmptyMessageEdit({attachments: [{id: '123'}]})).toBe(false);
	});
	it('allows empty edits when existing attachments can keep the message non-empty', () => {
		AdvancedSettings.setKeepAttachmentsOnEmptyMessageEdit(true);
		expect(canSubmitEmptyMessageEdit({attachments: [{id: '123'}]})).toBe(true);
	});
	it('still deletes an attachment-free message when the setting is on', () => {
		AdvancedSettings.setKeepAttachmentsOnEmptyMessageEdit(true);
		expect(canSubmitEmptyMessageEdit({attachments: []})).toBe(false);
	});
	it('recognises a message that never had text to clear', () => {
		expect(isAttachmentOnlyMessage({content: '', attachments: [{id: '123'}]})).toBe(true);
	});
	it('keeps a cleared caption away from the untouched-edit path', () => {
		expect(isAttachmentOnlyMessage({content: 'caption', attachments: [{id: '123'}]})).toBe(false);
	});
	it('keeps an attachment-free empty message away from the untouched-edit path', () => {
		expect(isAttachmentOnlyMessage({content: '', attachments: []})).toBe(false);
	});
	it('builds attachment references that retain existing attachments during an empty edit', () => {
		expect(buildExistingAttachmentEditReferences({attachments: [{id: '123'}, {id: '456'}]})).toEqual([
			{id: '123'},
			{id: '456'},
		]);
	});
});
