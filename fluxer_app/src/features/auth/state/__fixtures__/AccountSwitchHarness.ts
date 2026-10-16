// SPDX-License-Identifier: AGPL-3.0-or-later

import {runtimeSnapshotFromDiscovery} from '@app/features/app/state/InstanceSnapshotStore';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {parseInstanceDiscoveryDocument} from '@fluxer/instance_bootstrap/src/Discovery';

export const HARNESS_API_ENDPOINT = 'https://primary.test/api';

export interface ScriptedReply {
	status: number;
	body: unknown;
}

export const scriptedReplies: Array<ScriptedReply> = [];
export const scriptedRequestTokens: Array<string | undefined> = [];
export const scriptedRequestUrls: Array<string> = [];

export function installHarnessBootstrap(): void {
	RuntimeConfig.applySnapshot(
		runtimeSnapshotFromDiscovery(
			parseInstanceDiscoveryDocument({
				api_code_version: Number.MAX_SAFE_INTEGER,
				endpoints: {
					api: HARNESS_API_ENDPOINT,
					api_client: HARNESS_API_ENDPOINT,
					api_public: HARNESS_API_ENDPOINT,
					gateway: 'wss://gateway.primary.test',
					media: 'https://media.primary.test',
					static_cdn: 'https://cdn.primary.test',
					marketing: 'https://primary.test',
					admin: 'https://admin.primary.test',
					invite: 'https://primary.test/invite',
					gift: 'https://primary.test/gift',
					webapp: 'https://app.primary.test',
					upload_relay: 'https://upload.primary.test',
				},
				captcha: {provider: 'none'},
				features: {
					voice_enabled: false,
					stripe_enabled: false,
					self_hosted: false,
					presigned_attachment_uploads: false,
					emails_enabled: false,
				},
				gif: {provider: 'klipy', display_name: 'Klipy', attribution_required: false},
				sso: {enabled: false, enforced: false, display_name: null, redirect_uri: ''},
				registration: {mode: 'open', admin_registration_urls_enabled: true},
				community: {single_community: false, single_community_guild_id: null, direct_messages_disabled: false},
				services: {gif_enabled: true, youtube_enabled: false, bluesky_enabled: false},
				limits: {version: 1, traitDefinitions: [], rules: []},
				push: {public_vapid_key: null},
				app_public: {
					branding: {
						product_name: 'Fluxer',
						icon_url: null,
						symbol_url: null,
						logo_url: null,
						wordmark_url: null,
						favicon_url: null,
						theme_color: null,
					},
					setup: {configured: true, admin_url: null},
					legal: {terms_url: null, privacy_url: null},
					registration: {collect_date_of_birth: true},
				},
			}),
		),
	);
}

export class ScriptedXMLHttpRequest extends EventTarget {
	readonly upload = new EventTarget();
	private readonly requestHeaders: Record<string, string> = {};
	status = 200;
	statusText = 'OK';
	responseText = '{}';
	response: unknown = '{}';
	responseType = '';
	timeout = 0;

	private requestUrl = '';

	open(_method: string, url: string): void {
		this.requestUrl = url;
	}

	setRequestHeader(name: string, value: string): void {
		this.requestHeaders[name.toLowerCase()] = value;
	}

	getAllResponseHeaders(): string {
		return 'content-type: application/json\r\n';
	}

	abort(): void {
		this.dispatchEvent(new Event('abort'));
		this.dispatchEvent(new Event('loadend'));
	}

	send(): void {
		scriptedRequestUrls.push(this.requestUrl);
		scriptedRequestTokens.push(this.requestHeaders.authorization);
		const reply = scriptedReplies.shift() ?? {status: 200, body: {}};
		this.status = reply.status;
		this.statusText = reply.status >= 200 && reply.status < 300 ? 'OK' : 'Error';
		this.responseText = JSON.stringify(reply.body);
		this.response = this.responseText;
		queueMicrotask(() => {
			this.dispatchEvent(new Event('load'));
			this.dispatchEvent(new Event('loadend'));
		});
	}
}

export function userMeReply(userId: string, requiredActions: ReadonlyArray<string> = []): ScriptedReply {
	return {
		status: 200,
		body: {id: userId, username: `user-${userId}`, required_actions: [...requiredActions]},
	};
}

export function unauthorizedReply(): ScriptedReply {
	return {status: 401, body: {code: 0, message: 'Unauthorized'}};
}

export function serverErrorReply(): ScriptedReply {
	return {status: 500, body: {code: 0, message: 'Internal server error'}};
}

export function resetScriptedTransport(): void {
	scriptedReplies.length = 0;
	scriptedRequestTokens.length = 0;
	scriptedRequestUrls.length = 0;
}
