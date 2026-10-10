// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfigSnapshot';
import {resolveInstanceAssetUrl} from '@app/features/auth/InstanceBranding';
import {describe, expect, test} from 'vitest';

type Branding = RuntimeConfigSnapshot['appPublic']['branding'];

function snapshot(branding: Partial<Branding>, origin = 'https://chat.example.org') {
	return {
		apiEndpoint: `${origin}/api`,
		apiPublicEndpoint: `${origin}/api`,
		mediaEndpoint: `${origin}/media`,
		staticCdnEndpoint: origin,
		webAppEndpoint: origin,
		appPublic: {
			branding: {
				product_name: 'Example Chat',
				icon_url: null,
				symbol_url: null,
				logo_url: null,
				wordmark_url: null,
				favicon_url: null,
				theme_color: null,
				status_page_url: null,
				status_page_incident_history_url: null,
				premium_product_name: 'Premium',
				premium_info_url: null,
				...branding,
			},
			setup: {configured: true, admin_url: null},
			legal: {terms_url: null, privacy_url: null, guidelines_url: null},
			registration: {collect_date_of_birth: false},
		},
	};
}

describe('instance branding', () => {
	test('only accepts assets served by the instance itself', () => {
		const s = snapshot({});
		expect(resolveInstanceAssetUrl(s, '/media/branding/icon.png')).toBe(
			'https://chat.example.org/media/branding/icon.png',
		);
		expect(resolveInstanceAssetUrl(s, 'https://tracker.example.net/icon.png')).toBeNull();
		expect(resolveInstanceAssetUrl(s, 'javascript:alert(1)')).toBeNull();
		expect(resolveInstanceAssetUrl(s, 'https://user:pass@chat.example.org/icon.png')).toBeNull();
		expect(resolveInstanceAssetUrl(s, 'http://chat.example.org/icon.png')).toBeNull();
	});
});
