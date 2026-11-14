// SPDX-License-Identifier: AGPL-3.0-or-later

import {getAcceptString, getPolicy, isExtensionAllowed} from '@fluxer/constants/src/AssetFormatPolicy';
import {describe, expect, it} from 'vitest';

describe('role icon policy', () => {
	it('allows static PNG, JPEG, WebP and AVIF up to 256 KiB', () => {
		const policy = getPolicy('role_icon');
		expect(policy.upload).toEqual(['png', 'jpeg', 'webp', 'avif']);
		expect(policy.animated).toBe('never');
		expect(policy.maxBytes).toBe(256 * 1024);
		expect(policy.dims).toEqual({min: 32, max: 1024});
	});

	it('rejects SVG, GIF, HEIC and JXL', () => {
		for (const ext of ['svg', 'gif', 'heic', 'heif', 'jxl', 'apng']) {
			expect(isExtensionAllowed('role_icon', ext)).toBe(false);
		}
		expect(isExtensionAllowed('role_icon', 'jpg')).toBe(true);
	});

	it('builds an accept string from the allowed formats', () => {
		expect(getAcceptString('role_icon')).toBe('.png,.jpeg,.webp,.avif,image/png,image/jpeg,image/webp,image/avif');
	});
});
