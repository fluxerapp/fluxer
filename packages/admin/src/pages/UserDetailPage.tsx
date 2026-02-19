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

import type {ApiError} from '@fluxer/admin/src/api/Errors';
import {getLimitConfig} from '@fluxer/admin/src/api/LimitConfig';
import * as messagesApi from '@fluxer/admin/src/api/Messages';
import * as usersApi from '@fluxer/admin/src/api/Users';
import {Layout} from '@fluxer/admin/src/components/Layout';
import {UserProfileBadges} from '@fluxer/admin/src/components/UserProfileBadges';
import {HStack} from '@fluxer/admin/src/components/ui/Layout/HStack';
import {VStack} from '@fluxer/admin/src/components/ui/Layout/VStack';
import {Heading, Text} from '@fluxer/admin/src/components/ui/Typography';
import {AccountTab} from '@fluxer/admin/src/pages/user_detail/tabs/AccountTab';
import {DmHistoryTab} from '@fluxer/admin/src/pages/user_detail/tabs/DmHistoryTab';
import {GuildsTab} from '@fluxer/admin/src/pages/user_detail/tabs/GuildsTab';
import {ModerationTab} from '@fluxer/admin/src/pages/user_detail/tabs/ModerationTab';
import {OverviewTab} from '@fluxer/admin/src/pages/user_detail/tabs/OverviewTab';
import type {Session} from '@fluxer/admin/src/types/App';
import type {AdminConfig as Config} from '@fluxer/admin/src/types/Config';
import type {Flash} from '@fluxer/hono/src/Flash';
import type {ListUserGuildsResponse} from '@fluxer/schema/src/domains/admin/AdminGuildSchemas';
import type {LimitConfigGetResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {
	ListUserChangeLogResponse,
	ListUserDmChannelsResponse,
	ListUserSessionsResponse,
	UserAdminResponse,
} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import type {WebAuthnCredentialListResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {BackButton, NotFoundView} from '@fluxer/ui/src/components/Navigation';
import {formatDiscriminator, getUserAvatarUrl, getUserBannerUrl} from '@fluxer/ui/src/utils/FormatUser';
import type {FC} from 'hono/jsx';
import type {z} from 'zod';

type LimitConfigResponse = z.infer<typeof LimitConfigGetResponse>;

export interface UserDetailPageProps {
	config: Config;
	session: Session;
	currentAdmin: UserAdminResponse | undefined;
	flash: Flash | undefined;
	userId: string;
	tab: string | undefined;
	assetVersion: string;
	csrfToken: string;

	guildsBefore: string | null | undefined;
	guildsAfter: string | null | undefined;
	guildsLimit: string | null | undefined;
	guildsWithCounts: string | null | undefined;
	dmBefore: string | null | undefined;
	dmAfter: string | null | undefined;
	dmLimit: string | null | undefined;

	messageShredJobId: string | null | undefined;
	deleteAllMessagesDryRun: string | null | undefined;
	deleteAllMessagesChannelCount: string | null | undefined;
	deleteAllMessagesMessageCount: string | null | undefined;
}

function parseIntOrDefault(value: string | null | undefined, fallback: number): number {
	if (!value) return fallback;
	const parsed = parseInt(value, 10);
	return Number.isFinite(parsed) ? parsed : fallback;
}

function parseBoolFlag(value: string | null | undefined, fallback: boolean): boolean {
	if (value == null) return fallback;
	const normalized = value.trim().toLowerCase();
	if (normalized === '1' || normalized === 'true') return true;
	if (normalized === '0' || normalized === 'false') return false;
	return fallback;
}

function parseDryRunSummary(params: {
	dryRun: string | null | undefined;
	channelCount: string | null | undefined;
	messageCount: string | null | undefined;
}): {channel_count: number; message_count: number} | null {
	if (!parseBoolFlag(params.dryRun, false)) return null;
	const channelCount = parseIntOrDefault(params.channelCount ?? null, 0);
	const messageCount = parseIntOrDefault(params.messageCount ?? null, 0);
	if (channelCount <= 0 && messageCount <= 0) return null;
	return {channel_count: channelCount, message_count: messageCount};
}

function isNonEmptyString(value: string | null | undefined): value is string {
	return typeof value === 'string' && value.trim().length > 0;
}

export const UserDetailPage: FC<UserDetailPageProps> = async ({
	config,
	session,
	currentAdmin,
	flash,
	userId,
	tab = 'overview',
	assetVersion,
	csrfToken,
	guildsBefore,
	guildsAfter,
	guildsLimit,
	guildsWithCounts,
	dmBefore,
	dmAfter,
	dmLimit,
	messageShredJobId,
	deleteAllMessagesDryRun,
	deleteAllMessagesChannelCount,
	deleteAllMessagesMessageCount,
}) => {
	const adminAcls = currentAdmin?.acls ?? [];

	const userResult = await usersApi.lookupUser(config, session, userId);
	if (!userResult.ok || !userResult.data) {
		return (
			<Layout
				csrfToken={csrfToken}
				title="User Not Found"
				activePage="users"
				config={config}
				session={session}
				currentAdmin={currentAdmin}
				flash={flash}
				assetVersion={assetVersion}
			>
				<NotFoundView resourceName="User" backUrl={`${config.basePath}/users`} backLabel="Back to Users" />
			</Layout>
		);
	}

	const user = userResult.data;
	const activeTab = tab || 'overview';
	const bannerUrl = getUserBannerUrl(config.mediaEndpoint, user.id, user.banner, true);

	const tabs = [
		{id: 'overview', label: 'Overview'},
		{id: 'account', label: 'Account'},
		{id: 'guilds', label: 'Guilds'},
		{id: 'dm_history', label: 'DM History'},
		{id: 'moderation', label: 'Moderation'},
	] as const;

	const deleteAllMessagesDryRunSummary = parseDryRunSummary({
		dryRun: deleteAllMessagesDryRun,
		channelCount: deleteAllMessagesChannelCount,
		messageCount: deleteAllMessagesMessageCount,
	});

	let changeLogResult: {ok: true; data: ListUserChangeLogResponse} | {ok: false; error: ApiError} | null = null;
	let limitConfigResult: {ok: true; data: LimitConfigResponse} | {ok: false; error: ApiError} | null = null;
	let sessionsResult: {ok: true; data: ListUserSessionsResponse} | {ok: false; error: ApiError} | null = null;
	let guildsResult: {ok: true; data: ListUserGuildsResponse} | {ok: false; error: ApiError} | null = null;
	let dmChannelsResult: {ok: true; data: ListUserDmChannelsResponse} | {ok: false; error: ApiError} | null = null;
	let webAuthnCredentials: WebAuthnCredentialListResponse | null = null;
	let messageShredStatusResult:
		| {ok: true; data: messagesApi.MessageShredStatusResponse}
		| {ok: false; error: ApiError}
		| null = null;

	if (activeTab === 'overview') {
		changeLogResult = await usersApi.listUserChangeLog(config, session, userId);
		limitConfigResult = await getLimitConfig(config, session);
	}

	if (activeTab === 'account') {
		sessionsResult = await usersApi.listUserSessions(config, session, userId);
		const hasWebAuthn = user.authenticator_types.includes(2);
		if (hasWebAuthn) {
			const credResult = await usersApi.listWebAuthnCredentials(config, session, userId);
			if (credResult.ok) {
				webAuthnCredentials = credResult.data;
			}
		}
	}

	if (activeTab === 'guilds') {
		const limit = parseIntOrDefault(guildsLimit ?? null, 25);
		const withCounts = parseBoolFlag(guildsWithCounts ?? null, true);
		guildsResult = await usersApi.listUserGuilds(
			config,
			session,
			userId,
			guildsBefore ?? undefined,
			guildsAfter ?? undefined,
			limit,
			withCounts,
		);
	}

	if (activeTab === 'dm_history') {
		const limit = parseIntOrDefault(dmLimit ?? null, 50);
		dmChannelsResult = await usersApi.listUserDmChannels(
			config,
			session,
			userId,
			dmBefore ?? undefined,
			dmAfter ?? undefined,
			limit,
		);
	}

	if (activeTab === 'moderation' && isNonEmptyString(messageShredJobId)) {
		messageShredStatusResult = await messagesApi.getMessageShredStatus(config, session, messageShredJobId);
	}

	return (
		<Layout
			csrfToken={csrfToken}
			title={`${user.username}#${formatDiscriminator(user.discriminator)} - User`}
			activePage="users"
			config={config}
			session={session}
			currentAdmin={currentAdmin}
			flash={flash}
			assetVersion={assetVersion}
		>
			<VStack gap={6}>
				<BackButton href={`${config.basePath}/users`} label="Back to Users" />

				{bannerUrl && (
					<VStack gap={2}>
						<a href={bannerUrl} target="_blank" rel="noreferrer noopener">
							<img
								src={bannerUrl}
								alt={`${user.username}'s banner`}
								class="h-48 w-full rounded-lg border border-neutral-200 bg-neutral-50 object-cover"
								loading="lazy"
							/>
						</a>
						<Text size="xs" color="muted" class="font-mono">
							Banner hash: {user.banner}
						</Text>
					</VStack>
				)}

				<HStack gap={6} align="start">
					<img
						src={getUserAvatarUrl(
							config.mediaEndpoint,
							config.staticCdnEndpoint,
							user.id,
							user.avatar,
							false,
							assetVersion,
						)}
						alt={`${user.username}'s avatar`}
						class="h-20 w-20 rounded-full"
					/>
					<VStack gap={2} class="flex-1">
						<HStack gap={3} class="flex-wrap">
							<Heading level={1}>
								{user.username}#{formatDiscriminator(user.discriminator)}
							</Heading>
							<UserProfileBadges config={config} user={user} size="md" />
						</HStack>
						<Text size="sm" color="muted" class="font-mono">
							{user.id}
						</Text>
					</VStack>
				</HStack>

				<VStack gap={0} class="border-neutral-200 border-b">
					<HStack gap={6}>
						{tabs.map((t) => (
							<a
								href={`${config.basePath}/users/${userId}?tab=${t.id}`}
								class={`-mb-px border-b-2 pb-3 font-medium text-sm transition-colors ${
									activeTab === t.id
										? 'border-neutral-900 text-neutral-900'
										: 'border-transparent text-neutral-500 hover:text-neutral-700'
								}`}
							>
								{t.label}
							</a>
						))}
					</HStack>
				</VStack>

				{activeTab === 'overview' && (
					<OverviewTab
						config={config}
						user={user}
						adminAcls={adminAcls}
						changeLogResult={changeLogResult}
						limitConfigResult={limitConfigResult}
						csrfToken={csrfToken}
					/>
				)}

				{activeTab === 'account' && (
					<AccountTab
						config={config}
						user={user}
						userId={userId}
						sessionsResult={sessionsResult}
						webAuthnCredentials={webAuthnCredentials}
						csrfToken={csrfToken}
					/>
				)}

				{activeTab === 'guilds' && (
					<GuildsTab
						config={config}
						user={user}
						userId={userId}
						guildsResult={guildsResult}
						before={guildsBefore ?? null}
						after={guildsAfter ?? null}
						limit={parseIntOrDefault(guildsLimit ?? null, 25)}
						withCounts={parseBoolFlag(guildsWithCounts ?? null, true)}
					/>
				)}

				{activeTab === 'dm_history' && (
					<DmHistoryTab
						config={config}
						userId={userId}
						dmChannelsResult={dmChannelsResult}
						before={dmBefore ?? null}
						after={dmAfter ?? null}
						limit={parseIntOrDefault(dmLimit ?? null, 50)}
					/>
				)}

				{activeTab === 'moderation' && (
					<ModerationTab
						config={config}
						user={user}
						userId={userId}
						adminAcls={adminAcls}
						messageShredJobId={isNonEmptyString(messageShredJobId) ? messageShredJobId : null}
						messageShredStatusResult={messageShredStatusResult}
						deleteAllMessagesDryRun={deleteAllMessagesDryRunSummary}
						csrfToken={csrfToken}
					/>
				)}
			</VStack>
		</Layout>
	);
};
