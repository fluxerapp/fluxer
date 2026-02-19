/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

/** @jsxRuntime automatic */
/** @jsxImportSource hono/jsx */

import {ApiClient, type ApiResult} from '@fluxer/admin/src/api/Client';
import type {JsonObject} from '@fluxer/admin/src/api/JsonTypes';
import type {Session} from '@fluxer/admin/src/types/App';
import type {AdminConfig as Config} from '@fluxer/admin/src/types/Config';
import type {ListUserGuildsResponse} from '@fluxer/schema/src/domains/admin/AdminGuildSchemas';
import type {
	AdminUsersMeResponse,
	ListUserChangeLogResponse,
	ListUserDmChannelsResponse,
	ListUserSessionsResponse,
	LookupUserResponse,
	UserAdminResponse,
} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import type {WebAuthnCredentialListResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';

export async function getCurrentAdmin(config: Config, session: Session): Promise<ApiResult<UserAdminResponse | null>> {
	const client = new ApiClient(config, session);
	const result = await client.get<AdminUsersMeResponse>('/admin/users/me');
	if (result.ok) {
		return {ok: true, data: result.data.user};
	}
	if (result.error.type === 'notFound') {
		return {ok: true, data: null};
	}
	return result;
}

export interface UserSearchResult {
	users: Array<UserAdminResponse>;
	has_more: boolean;
}

export async function searchUsers(
	config: Config,
	session: Session,
	query: string,
	page: number = 0,
	limit: number = 25,
): Promise<ApiResult<UserSearchResult>> {
	const client = new ApiClient(config, session);
	const offset = Math.max(0, page) * limit;
	const result = await client.post<{users: Array<UserAdminResponse>; total: number}>('/admin/users/search', {
		query,
		limit,
		offset,
	});

	if (result.ok) {
		const users = result.data.users;
		const hasMore = offset + users.length < result.data.total;
		return {ok: true, data: {users, has_more: hasMore}};
	}

	return result;
}

export async function lookupUser(
	config: Config,
	session: Session,
	query: string,
): Promise<ApiResult<UserAdminResponse | null>> {
	const client = new ApiClient(config, session);
	const result = await client.post<LookupUserResponse>('/admin/users/lookup', {query});
	if (result.ok) {
		const user = result.data.users[0];
		return {ok: true, data: user ?? null};
	}
	return result;
}

export async function lookupUsersByIds(
	config: Config,
	session: Session,
	userIds: Array<string>,
): Promise<ApiResult<Array<UserAdminResponse>>> {
	if (userIds.length === 0) {
		return {ok: true, data: []};
	}
	const client = new ApiClient(config, session);
	const result = await client.post<LookupUserResponse>('/admin/users/lookup', {user_ids: userIds});
	if (result.ok) {
		return {ok: true, data: result.data.users};
	}
	return result;
}

export async function listUserGuilds(
	config: Config,
	session: Session,
	userId: string,
	before?: string,
	after?: string,
	limit: number = 25,
	withCounts: boolean = true,
): Promise<ApiResult<ListUserGuildsResponse>> {
	const client = new ApiClient(config, session);
	const body: JsonObject = {
		user_id: userId,
		limit,
		with_counts: withCounts,
		...(before ? {before} : {}),
		...(after ? {after} : {}),
	};

	const result = await client.post<ListUserGuildsResponse>('/admin/users/list-guilds', body);
	if (result.ok) {
		return {ok: true, data: result.data};
	}
	return result;
}

export async function listUserDmChannels(
	config: Config,
	session: Session,
	userId: string,
	before?: string,
	after?: string,
	limit: number = 50,
): Promise<ApiResult<ListUserDmChannelsResponse>> {
	const client = new ApiClient(config, session);
	const body: JsonObject = {
		user_id: userId,
		limit,
		...(before ? {before} : {}),
		...(after ? {after} : {}),
	};

	const result = await client.post<ListUserDmChannelsResponse>('/admin/users/list-dm-channels', body);
	if (result.ok) {
		return {ok: true, data: result.data};
	}
	return result;
}

export async function listUserSessions(
	config: Config,
	session: Session,
	userId: string,
): Promise<ApiResult<ListUserSessionsResponse>> {
	const client = new ApiClient(config, session);
	const result = await client.post<ListUserSessionsResponse>('/admin/users/list-sessions', {
		user_id: userId,
	});
	if (result.ok) {
		return {ok: true, data: result.data};
	}
	return result;
}

export async function listUserChangeLog(
	config: Config,
	session: Session,
	userId: string,
): Promise<ApiResult<ListUserChangeLogResponse>> {
	const client = new ApiClient(config, session);
	const result = await client.post<ListUserChangeLogResponse>('/admin/users/change-log', {user_id: userId, limit: 50});
	if (result.ok) {
		return {ok: true, data: result.data};
	}
	return result;
}

export async function updateUserFlags(
	config: Config,
	session: Session,
	userId: string,
	addFlags: Array<string>,
	removeFlags: Array<string>,
	auditLogReason?: string,
): Promise<ApiResult<void>> {
	const client = new ApiClient(config, session);
	return client.postVoid(
		'/admin/users/update-flags',
		{user_id: userId, add_flags: addFlags, remove_flags: removeFlags},
		auditLogReason,
	);
}

export async function updateSuspiciousActivityFlags(
	config: Config,
	session: Session,
	userId: string,
	flags: number,
	auditLogReason?: string,
): Promise<ApiResult<void>> {
	const client = new ApiClient(config, session);
	return client.postVoid('/admin/users/update-suspicious-activity-flags', {user_id: userId, flags}, auditLogReason);
}

export async function setUserAcls(
	config: Config,
	session: Session,
	userId: string,
	acls: Array<string>,
	auditLogReason?: string,
): Promise<ApiResult<void>> {
	const client = new ApiClient(config, session);
	return client.postVoid('/admin/users/set-acls', {user_id: userId, acls}, auditLogReason);
}

export async function setUserTraits(
	config: Config,
	session: Session,
	userId: string,
	traits: Array<string>,
	auditLogReason?: string,
): Promise<ApiResult<void>> {
	const client = new ApiClient(config, session);
	return client.postVoid('/admin/users/set-traits', {user_id: userId, traits}, auditLogReason);
}

export async function disableMfa(
	config: Config,
	session: Session,
	userId: string,
	auditLogReason?: string,
): Promise<ApiResult<void>> {
	const client = new ApiClient(config, session);
	return client.postVoid('/admin/users/disable-mfa', {user_id: userId}, auditLogReason);
}

export async function verifyEmail(
	config: Config,
	session: Session,
	userId: string,
	auditLogReason?: string,
): Promise<ApiResult<void>> {
	const client = new ApiClient(config, session);
	return client.postVoid('/admin/users/verify-email', {user_id: userId}, auditLogReason);
}

export async function unlinkPhone(
	config: Config,
	session: Session,
	userId: string,
	auditLogReason?: string,
): Promise<ApiResult<void>> {
	const client = new ApiClient(config, session);
	return client.postVoid('/admin/users/unlink-phone', {user_id: userId}, auditLogReason);
}

export async function terminateSessions(
	config: Config,
	session: Session,
	userId: string,
	auditLogReason?: string,
): Promise<ApiResult<void>> {
	const client = new ApiClient(config, session);
	return client.postVoid('/admin/users/terminate-sessions', {user_id: userId}, auditLogReason);
}

export async function clearUserFields(
	config: Config,
	session: Session,
	userId: string,
	fields: Array<string>,
	auditLogReason?: string,
): Promise<ApiResult<void>> {
	const client = new ApiClient(config, session);
	return client.postVoid('/admin/users/clear-fields', {user_id: userId, fields}, auditLogReason);
}

export async function setBotStatus(
	config: Config,
	session: Session,
	userId: string,
	bot: boolean,
	auditLogReason?: string,
): Promise<ApiResult<void>> {
	const client = new ApiClient(config, session);
	return client.postVoid('/admin/users/set-bot-status', {user_id: userId, bot}, auditLogReason);
}

export async function setSystemStatus(
	config: Config,
	session: Session,
	userId: string,
	system: boolean,
	auditLogReason?: string,
): Promise<ApiResult<void>> {
	const client = new ApiClient(config, session);
	return client.postVoid('/admin/users/set-system-status', {user_id: userId, system}, auditLogReason);
}

export async function changeUsername(
	config: Config,
	session: Session,
	userId: string,
	username: string,
	discriminator?: number,
	auditLogReason?: string,
): Promise<ApiResult<void>> {
	const client = new ApiClient(config, session);
	const body: JsonObject = {
		user_id: userId,
		username,
		...(discriminator !== undefined ? {discriminator} : {}),
	};
	return client.postVoid('/admin/users/change-username', body, auditLogReason);
}

export async function changeEmail(
	config: Config,
	session: Session,
	userId: string,
	email: string,
	auditLogReason?: string,
): Promise<ApiResult<void>> {
	const client = new ApiClient(config, session);
	return client.postVoid('/admin/users/change-email', {user_id: userId, email}, auditLogReason);
}

export async function scheduleDeletion(
	config: Config,
	session: Session,
	userId: string,
	reasonCode: number,
	publicReason: string | undefined,
	daysUntilDeletion: number,
	auditLogReason?: string,
): Promise<ApiResult<void>> {
	const client = new ApiClient(config, session);
	const body: JsonObject = {
		user_id: userId,
		reason_code: reasonCode,
		days_until_deletion: daysUntilDeletion,
		...(publicReason ? {public_reason: publicReason} : {}),
	};
	return client.postVoid('/admin/users/schedule-deletion', body, auditLogReason);
}

export async function cancelDeletion(
	config: Config,
	session: Session,
	userId: string,
	auditLogReason?: string,
): Promise<ApiResult<void>> {
	const client = new ApiClient(config, session);
	return client.postVoid('/admin/users/cancel-deletion', {user_id: userId}, auditLogReason);
}

export async function cancelBulkMessageDeletion(
	config: Config,
	session: Session,
	userId: string,
	auditLogReason?: string,
): Promise<ApiResult<void>> {
	const client = new ApiClient(config, session);
	return client.postVoid('/admin/users/cancel-bulk-message-deletion', {user_id: userId}, auditLogReason);
}

export async function tempBanUser(
	config: Config,
	session: Session,
	userId: string,
	durationHours: number,
	reason?: string,
	privateReason?: string,
): Promise<ApiResult<void>> {
	const client = new ApiClient(config, session);
	const body: JsonObject = {
		user_id: userId,
		duration_hours: durationHours,
		...(reason ? {reason} : {}),
	};
	return client.postVoid('/admin/users/temp-ban', body, privateReason);
}

export async function unbanUser(
	config: Config,
	session: Session,
	userId: string,
	auditLogReason?: string,
): Promise<ApiResult<void>> {
	const client = new ApiClient(config, session);
	return client.postVoid('/admin/users/unban', {user_id: userId}, auditLogReason);
}

export async function changeDob(
	config: Config,
	session: Session,
	userId: string,
	dateOfBirth: string,
	auditLogReason?: string,
): Promise<ApiResult<void>> {
	const client = new ApiClient(config, session);
	return client.postVoid('/admin/users/change-dob', {user_id: userId, date_of_birth: dateOfBirth}, auditLogReason);
}

export async function sendPasswordReset(
	config: Config,
	session: Session,
	userId: string,
	auditLogReason?: string,
): Promise<ApiResult<void>> {
	const client = new ApiClient(config, session);
	return client.postVoid('/admin/users/send-password-reset', {user_id: userId}, auditLogReason);
}

export async function listWebAuthnCredentials(
	config: Config,
	session: Session,
	userId: string,
): Promise<ApiResult<WebAuthnCredentialListResponse>> {
	const client = new ApiClient(config, session);
	return client.post<WebAuthnCredentialListResponse>('/admin/users/list-webauthn-credentials', {
		user_id: userId,
	});
}

export async function deleteWebAuthnCredential(
	config: Config,
	session: Session,
	userId: string,
	credentialId: string,
	auditLogReason?: string,
): Promise<ApiResult<void>> {
	const client = new ApiClient(config, session);
	return client.postVoid(
		'/admin/users/delete-webauthn-credential',
		{user_id: userId, credential_id: credentialId},
		auditLogReason,
	);
}
