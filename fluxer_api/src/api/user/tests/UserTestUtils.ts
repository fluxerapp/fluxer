// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import type {
	MobileDevicesListResponse,
	PushSubscribeResponse,
	PushSubscriptionsListResponse,
	RegisterMobileDeviceResponse,
	UserPartialResponse,
	UserPrivateResponse,
	UserProfileFullResponse,
	UserSettingsResponse,
	UserTagCheckResponse,
} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

export async function fetchUserMe(
	harness: ApiTestHarness,
	token: string,
): Promise<{
	response: Response;
	json: UserPrivateResponse;
}> {
	const {response, json} = await createBuilder<UserPrivateResponse>(harness, token)
		.get('/users/@me')
		.executeWithResponse();
	if (response.status !== 200) {
		throw new Error(`Expected 200, got ${response.status}`);
	}
	return {response, json};
}

export async function fetchUser(
	harness: ApiTestHarness,
	userId: string,
	token: string,
): Promise<{
	response: Response;
	json: UserPartialResponse;
}> {
	const {response, json} = await createBuilder<UserPartialResponse>(harness, token)
		.get(`/users/${userId}`)
		.executeWithResponse();
	if (response.status !== 200) {
		throw new Error(`Expected 200, got ${response.status}`);
	}
	return {response, json};
}

export async function fetchUserProfile(
	harness: ApiTestHarness,
	targetId: string,
	token: string,
): Promise<{
	response: Response;
	json: UserProfileFullResponse;
}> {
	const {response, json} = await createBuilder<UserProfileFullResponse>(harness, token)
		.get(`/users/${targetId}/profile`)
		.executeWithResponse();
	if (response.status !== 200) {
		throw new Error(`Expected 200, got ${response.status}`);
	}
	return {response, json};
}

export async function updateUserProfile(
	harness: ApiTestHarness,
	token: string,
	data: {
		username?: string;
		discriminator?: string;
		global_name?: string;
		bio?: string;
		password?: string;
	},
): Promise<{
	response: Response;
	json: UserPrivateResponse;
}> {
	const {response, json} = await createBuilder<UserPrivateResponse>(harness, token)
		.patch('/users/@me')
		.body(data)
		.executeWithResponse();
	if (response.status !== 200) {
		throw new Error(`Expected 200, got ${response.status}`);
	}
	return {response, json};
}

export async function updateUserSettings(
	harness: ApiTestHarness,
	token: string,
	data: Record<string, unknown>,
): Promise<{
	response: Response;
	json: UserSettingsResponse;
}> {
	const {response, json} = await createBuilder<UserSettingsResponse>(harness, token)
		.patch('/users/@me/settings')
		.body(data)
		.executeWithResponse();
	if (response.status !== 200) {
		throw new Error(`Expected 200, got ${response.status}`);
	}
	return {response, json};
}

export async function fetchUserSettings(
	harness: ApiTestHarness,
	token: string,
): Promise<{
	response: Response;
	json: UserSettingsResponse;
}> {
	const {response, json} = await createBuilder<UserSettingsResponse>(harness, token)
		.get('/users/@me/settings')
		.executeWithResponse();
	if (response.status !== 200) {
		throw new Error(`Expected 200, got ${response.status}`);
	}
	return {response, json};
}

export async function checkUsernameDiscriminatorAvailability(
	harness: ApiTestHarness,
	username: string,
	discriminator: string,
	token: string,
): Promise<{
	response: Response;
	json: UserTagCheckResponse;
}> {
	const {response, json} = await createBuilder<UserTagCheckResponse>(harness, token)
		.get(`/users/check-tag?username=${encodeURIComponent(username)}&discriminator=${encodeURIComponent(discriminator)}`)
		.executeWithResponse();
	if (response.status !== 200) {
		throw new Error(`Expected 200, got ${response.status}`);
	}
	return {response, json};
}

export async function deleteAccount(harness: ApiTestHarness, token: string, password: string): Promise<void> {
	await createBuilder<void>(harness, token).post('/users/@me/delete').body({password}).expect(204).execute();
}

export async function setUserNote(
	harness: ApiTestHarness,
	token: string,
	targetId: string,
	note: string | null,
): Promise<void> {
	await createBuilder<void>(harness, token).put(`/users/@me/notes/${targetId}`).body({note}).expect(204).execute();
}

export async function preloadMessages(
	harness: ApiTestHarness,
	token: string,
	channels: Array<number>,
): Promise<{
	response: Response;
	json: Record<string, never>;
}> {
	const {response, json} = await createBuilder<Record<string, never>>(harness, token)
		.post('/users/@me/preload-messages')
		.body({channels})
		.executeWithResponse();
	if (response.status !== 200) {
		throw new Error(`Expected 200, got ${response.status}`);
	}
	return {response, json: json as Record<string, never>};
}

export async function updateGuildSettings(
	harness: ApiTestHarness,
	token: string,
	data: Record<string, unknown>,
): Promise<{
	response: Response;
	json: unknown;
}> {
	const {response, json} = await createBuilder<unknown>(harness, token)
		.patch('/users/@me/guilds/@me/settings')
		.body(data)
		.executeWithResponse();
	if (response.status !== 200) {
		throw new Error(`Expected 200, got ${response.status}`);
	}
	return {response, json};
}

export async function expectDataExists(
	harness: ApiTestHarness,
	userId: string,
): Promise<{
	hasSelfDeletedFlag: boolean;
	hasDeletedFlag: boolean;
	userExists: boolean;
	pendingDeletionAt: string | null;
	relationshipsCount: number;
	emailCleared: boolean;
	passwordCleared: boolean;
}> {
	const data = await createBuilderWithoutAuth<{
		user_exists: boolean;
		has_self_deleted_flag: boolean;
		has_deleted_flag: boolean;
		pending_deletion_at: string | null;
		relationships_count: number;
		email_cleared: boolean;
		password_cleared: boolean;
	}>(harness)
		.get(`/test/users/${userId}/data-exists`)
		.execute();
	return {
		hasSelfDeletedFlag: data.has_self_deleted_flag,
		hasDeletedFlag: data.has_deleted_flag,
		userExists: data.user_exists,
		pendingDeletionAt: data.pending_deletion_at,
		relationshipsCount: data.relationships_count,
		emailCleared: data.email_cleared,
		passwordCleared: data.password_cleared,
	};
}

export async function triggerDeletionWorker(harness: ApiTestHarness): Promise<{
	scheduled: number;
}> {
	return createBuilderWithoutAuth<{
		scheduled: number;
	}>(harness)
		.post('/test/worker/process-pending-deletions')
		.execute();
}

export async function setPendingDeletionAt(harness: ApiTestHarness, userId: string, when: Date): Promise<void> {
	await createBuilderWithoutAuth<void>(harness)
		.post(`/test/users/${userId}/set-pending-deletion`)
		.body({
			pending_deletion_at: when.toISOString(),
		})
		.execute();
}

export async function waitForDeletionCompletion(
	harness: ApiTestHarness,
	userId: string,
	timeoutMs = 60000,
): Promise<void> {
	const start = Date.now();
	let backoff = 100;
	const maxBackoff = 5000;
	while (Date.now() - start < timeoutMs) {
		const data = await createBuilderWithoutAuth<{
			user_exists: boolean;
			has_deleted_flag: boolean;
		}>(harness)
			.get(`/test/users/${userId}/data-exists`)
			.execute();
		if (data.user_exists && data.has_deleted_flag) {
			return;
		}
		await new Promise((resolve) => setTimeout(resolve, backoff));
		backoff = Math.min(backoff * 1.5, maxBackoff);
	}
	throw new Error('Deletion did not complete within timeout');
}

interface UserProfileUpdateResult {
	id: string;
	username: string;
	avatar: string | null;
	banner: string | null;
}

export async function updateAvatar(
	harness: ApiTestHarness,
	token: string,
	avatarDataUrl: string | null,
): Promise<UserProfileUpdateResult> {
	return createBuilder<UserProfileUpdateResult>(harness, token)
		.patch('/users/@me')
		.body({avatar: avatarDataUrl})
		.execute();
}

export async function updateBanner(
	harness: ApiTestHarness,
	token: string,
	bannerDataUrl: string | null,
): Promise<UserProfileUpdateResult> {
	return createBuilder<UserProfileUpdateResult>(harness, token)
		.patch('/users/@me')
		.body({banner: bannerDataUrl})
		.execute();
}

export async function grantPremium(harness: ApiTestHarness, userId: string, premiumType: number): Promise<void> {
	await createBuilderWithoutAuth(harness)
		.post(`/test/users/${userId}/premium`)
		.body({premium_type: premiumType})
		.execute();
}

export async function subscribePush(
	harness: ApiTestHarness,
	token: string,
	endpoint: string,
	options?: {
		p256dh?: string;
		auth?: string;
		userAgent?: string;
	},
): Promise<PushSubscribeResponse> {
	return createBuilder<PushSubscribeResponse>(harness, token)
		.post('/users/@me/push/subscribe')
		.body({
			endpoint,
			keys: {
				p256dh: options?.p256dh ?? 'test-p256dh-key',
				auth: options?.auth ?? 'test-auth-key',
			},
			user_agent: options?.userAgent,
		})
		.execute();
}

export async function listPushSubscriptions(
	harness: ApiTestHarness,
	token: string,
): Promise<PushSubscriptionsListResponse> {
	return createBuilder<PushSubscriptionsListResponse>(harness, token).get('/users/@me/push/subscriptions').execute();
}

export async function deletePushSubscription(
	harness: ApiTestHarness,
	token: string,
	subscriptionId: string,
): Promise<void> {
	await createBuilder<void>(harness, token).delete(`/users/@me/push/subscriptions/${subscriptionId}`).execute();
}

export async function registerMobileDevice(
	harness: ApiTestHarness,
	token: string,
	body: {
		platform: 'android_fcm' | 'ios_apns' | 'ios_apns_voip' | 'android_unified_push';
		token: string;
		user_agent?: string;
		app_id?: string;
		provider_environment?: 'production' | 'development';
		encryption_key?: string;
		auth_secret?: string;
	},
): Promise<RegisterMobileDeviceResponse> {
	return createBuilder<RegisterMobileDeviceResponse>(harness, token)
		.post('/users/@me/mobile-devices')
		.body(body)
		.execute();
}

export async function listMobileDevices(harness: ApiTestHarness, token: string): Promise<MobileDevicesListResponse> {
	return createBuilder<MobileDevicesListResponse>(harness, token).get('/users/@me/mobile-devices').execute();
}

export async function deleteMobileDevice(harness: ApiTestHarness, token: string, deviceId: string): Promise<void> {
	await createBuilder<void>(harness, token).delete(`/users/@me/mobile-devices/${deviceId}`).execute();
}

export async function unregisterMobileDevice(
	harness: ApiTestHarness,
	token: string,
	body: {
		platform: 'android_fcm' | 'ios_apns' | 'ios_apns_voip' | 'android_unified_push';
		token: string;
		app_id?: string;
		provider_environment?: 'production' | 'development';
	},
): Promise<void> {
	await createBuilder<void>(harness, token).post('/users/@me/mobile-devices/unregister').body(body).execute();
}
