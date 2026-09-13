// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	assignTransientUploadFieldMutation,
	getTransientUploadFieldMutation,
	omitTransientUploadFields,
} from '@app/lib/forms/TransientUploadFields';
import {describe, expect, it} from 'vitest';

describe('getTransientUploadFieldMutation', () => {
	it('preserves the existing remote asset when there is no explicit upload or clear', () => {
		expect(getTransientUploadFieldMutation({value: null})).toBeUndefined();
		expect(getTransientUploadFieldMutation({value: 'stale-form-value'})).toBeUndefined();
	});

	it('returns null only for explicit clears', () => {
		expect(
			getTransientUploadFieldMutation({value: 'new-upload', previewUrl: 'new-upload', hasCleared: true}),
		).toBeNull();
	});

	it('returns the pending upload when a preview exists', () => {
		expect(getTransientUploadFieldMutation({value: 'new-upload', previewUrl: 'new-upload'})).toBe('new-upload');
		expect(getTransientUploadFieldMutation({value: undefined, previewUrl: 'preview-upload'})).toBe('preview-upload');
	});
});

describe('assignTransientUploadFieldMutation', () => {
	it('omits unchanged upload fields from API payloads', () => {
		const payload: {name: string; avatar?: string | null} = {name: 'Doc'};
		assignTransientUploadFieldMutation(payload, 'avatar', {value: null});
		expect(payload).toEqual({name: 'Doc'});
	});

	it('assigns explicit uploads and clears', () => {
		const uploadPayload: {avatar?: string | null} = {};
		assignTransientUploadFieldMutation(uploadPayload, 'avatar', {value: 'base64', previewUrl: 'base64'});
		expect(uploadPayload).toEqual({avatar: 'base64'});

		const clearPayload: {avatar?: string | null} = {};
		assignTransientUploadFieldMutation(clearPayload, 'avatar', {value: null, hasCleared: true});
		expect(clearPayload).toEqual({avatar: null});
	});
});

describe('omitTransientUploadFields', () => {
	it('removes one-shot upload fields from committed remote form values', () => {
		expect(
			omitTransientUploadFields({name: 'Doc', avatar: null, banner: 'base64'}, ['avatar', 'banner'] as const),
		).toEqual({name: 'Doc'});
	});
});
