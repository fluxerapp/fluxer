// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, it} from 'vitest';

// Define the global window bootstrap mock before importing RuntimeConfig transitively
if (typeof window === 'undefined') {
	global.window = {} as any;
}
(window as any).__FLUXER_BOOTSTRAP__ = {
	instance: {
		api_code_version: 1,
		endpoints: {
			gateway: 'http://gateway',
			media: 'http://media',
			static_cdn: 'http://cdn',
			marketing: 'http://marketing',
			admin: 'http://admin',
			invite: 'http://invite',
			gift: 'http://gift',
			webapp: 'http://webapp',
			api: 'http://api',
		},
		captcha: {
			provider: 'none',
			hcaptcha_site_key: null,
			turnstile_site_key: null,
		},
		features: {},
		gif: {
			provider: 'none',
			display_name: 'none',
			attribution_required: false,
		},
		sso: {
			enabled: false,
			enforced: false,
			disable_additional_auth: false,
			display_name: null,
			redirect_uri: '',
		},
		registration: {
			mode: 'open',
			admin_registration_urls_enabled: true,
		},
		community: {
			single_community: false,
			single_community_guild_id: null,
			direct_messages_disabled: false,
		},
		services: {
			gif_enabled: false,
			youtube_enabled: false,
			bluesky_enabled: false,
			emails_enabled: false,
		},
		limits: {},
		push: {
			public_vapid_key: null,
		},
		app_public: {
			setup: {
				configured: true,
			},
			branding: {
				product_name: 'Fluxer',
			},
		},
	},
};

const {getAccountSecurityCapabilities, isSsoManagedUser} = await import('./AccountSecurityCapabilities');
const {default: RuntimeConfig} = await import('@app/features/app/state/RuntimeConfig');

const userWithTraits = (traits: ReadonlyArray<string>) => ({traits});

describe('account security capabilities', () => {
	beforeEach(() => {
		RuntimeConfig.sso = null;
	});

	it('treats users with the sso trait as SSO-managed', () => {
		expect(isSsoManagedUser(userWithTraits(['sso']))).toBe(true);
		expect(isSsoManagedUser(userWithTraits(['sso:provider']))).toBe(false);
	});

	it('disables local sign-in and MFA management for SSO-managed users when SSO is enforced', () => {
		RuntimeConfig.sso = {
			enabled: true,
			enforced: true,
			disable_additional_auth: false,
			display_name: 'Test SSO',
			redirect_uri: 'http://test',
		};
		expect(getAccountSecurityCapabilities(userWithTraits(['sso']))).toEqual({
			canManageLocalEmail: false,
			canManageLocalPassword: false,
			canManageLocalTotp: false,
			canManageLocalPasskeys: false,
		});
	});

	it('disables local sign-in and MFA management for SSO-managed users when disable_additional_auth is true', () => {
		RuntimeConfig.sso = {
			enabled: true,
			enforced: false,
			disable_additional_auth: true,
			display_name: 'Test SSO',
			redirect_uri: 'http://test',
		};
		expect(getAccountSecurityCapabilities(userWithTraits(['sso']))).toEqual({
			canManageLocalEmail: false,
			canManageLocalPassword: false,
			canManageLocalTotp: false,
			canManageLocalPasskeys: false,
		});
	});

	it('allows local sign-in and MFA management for SSO-managed users when SSO is not enforced and disable_additional_auth is false', () => {
		RuntimeConfig.sso = {
			enabled: true,
			enforced: false,
			disable_additional_auth: false,
			display_name: 'Test SSO',
			redirect_uri: 'http://test',
		};
		expect(getAccountSecurityCapabilities(userWithTraits(['sso']))).toEqual({
			canManageLocalEmail: true,
			canManageLocalPassword: true,
			canManageLocalTotp: true,
			canManageLocalPasskeys: true,
		});
	});

	it('keeps local sign-in and MFA management available for non-SSO users in all cases', () => {
		RuntimeConfig.sso = {
			enabled: true,
			enforced: true,
			disable_additional_auth: true,
			display_name: 'Test SSO',
			redirect_uri: 'http://test',
		};
		expect(getAccountSecurityCapabilities(userWithTraits([]))).toEqual({
			canManageLocalEmail: true,
			canManageLocalPassword: true,
			canManageLocalTotp: true,
			canManageLocalPasskeys: true,
		});
	});
});
