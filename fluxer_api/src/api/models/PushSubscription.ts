// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {PushSubscriptionPlatform, PushSubscriptionRow} from '@app/api/database/types/UserTypes';

export class PushSubscription {
	readonly userId: UserID;
	readonly subscriptionId: string;
	readonly authSessionIdHash: string | null;
	readonly endpoint: string;
	readonly p256dhKey: string | null;
	readonly authKey: string | null;
	readonly userAgent: string | null;
	readonly platform: PushSubscriptionPlatform;
	readonly appId: string | null;
	readonly providerEnvironment: string | null;

	constructor(row: PushSubscriptionRow) {
		this.userId = row.user_id;
		this.subscriptionId = row.subscription_id;
		this.authSessionIdHash = row.auth_session_id_hash ?? null;
		this.endpoint = row.endpoint;
		this.p256dhKey = row.p256dh_key ?? null;
		this.authKey = row.auth_key ?? null;
		this.userAgent = row.user_agent ?? null;
		this.platform = row.platform ?? 'web_push';
		this.appId = row.app_id ?? null;
		this.providerEnvironment = row.provider_environment ?? null;
	}

	toRow(): PushSubscriptionRow {
		return {
			user_id: this.userId,
			subscription_id: this.subscriptionId,
			auth_session_id_hash: this.authSessionIdHash,
			endpoint: this.endpoint,
			p256dh_key: this.p256dhKey,
			auth_key: this.authKey,
			user_agent: this.userAgent,
			platform: this.platform,
			app_id: this.appId,
			provider_environment: this.providerEnvironment,
		};
	}
}
