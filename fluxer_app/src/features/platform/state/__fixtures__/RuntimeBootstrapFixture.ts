// SPDX-License-Identifier: AGPL-3.0-or-later

import {runtimeSnapshotFromDiscovery} from '@app/features/app/state/InstanceSnapshotStore';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {parseInstanceDiscoveryDocument} from '@fluxer/instance_bootstrap/src/Discovery';
import type {
	InstanceAgePolicy,
	InstanceAppPublic,
	InstanceDiscoveryResponse,
} from '@fluxer/instance_bootstrap/src/Types';

const BOOTSTRAP_API_ENDPOINT = 'https://fluxer.app/api';

export const BOOTSTRAP_APP_PUBLIC: InstanceAppPublic = {
	branding: {
		product_name: 'Fluxer',
		icon_url: null,
		symbol_url: null,
		logo_url: null,
		wordmark_url: null,
		favicon_url: null,
		theme_color: null,
		status_page_url: null,
		status_page_incident_history_url: null,
		premium_product_name: 'Plutonium',
		premium_info_url: null,
	},
	setup: {configured: true, admin_url: null},
	legal: {terms_url: null, privacy_url: null},
	registration: {collect_date_of_birth: true},
};

export function instanceDiscoveryFixture(
	apiEndpoint: string,
	agePolicy?: InstanceAgePolicy,
): InstanceDiscoveryResponse {
	return {
		...(agePolicy ? {age_policy: agePolicy} : {}),
		api_code_version: 9,
		endpoints: {
			api: apiEndpoint,
			api_client: apiEndpoint,
			api_public: apiEndpoint,
			gateway: 'wss://gateway.fluxer.app',
			media: 'https://media.fluxer.app',
			static_cdn: 'https://cdn.fluxer.app',
			marketing: 'https://fluxer.app',
			admin: 'https://admin.fluxer.app',
			invite: 'https://flux.gg',
			gift: 'https://flux.gift',
			webapp: 'https://fluxer.app',
			upload_relay: 'https://upload.fluxer.app',
		},
		captcha: {provider: 'none'},
		features: {
			voice_enabled: true,
			stripe_enabled: false,
			self_hosted: false,
			presigned_attachment_uploads: true,
			emails_enabled: true,
			premium_enabled: false,
			stripe_serviceable: false,
			phone_verification_enabled: false,
		},
		gif: {provider: 'tenor', display_name: 'Tenor', attribution_required: true},
		sso: {enabled: false, enforced: false, display_name: null, redirect_uri: ''},
		registration: {mode: 'open', admin_registration_urls_enabled: true},
		community: {
			single_community: false,
			single_community_guild_id: null,
			direct_messages_disabled: false,
			guild_create_access: true,
		},
		services: {gif_enabled: true, youtube_enabled: false, bluesky_enabled: false},
		limits: {version: 1, traitDefinitions: [], rules: []},
		push: {public_vapid_key: null},
		app_public: BOOTSTRAP_APP_PUBLIC,
	};
}

export function installRuntimeBootstrap(apiEndpoint: string = BOOTSTRAP_API_ENDPOINT): void {
	RuntimeConfig.applySnapshot(
		runtimeSnapshotFromDiscovery(parseInstanceDiscoveryDocument(instanceDiscoveryFixture(apiEndpoint))),
	);
}
