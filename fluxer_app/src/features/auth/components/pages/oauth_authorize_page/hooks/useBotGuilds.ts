// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildComboboxOption} from '@app/features/app/components/dialogs/shared/GuildComboboxRenderers';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import {
	type GuildSummary,
	type GuildWithPermissions,
	logger,
} from '@app/features/auth/components/pages/oauth_authorize_page/OAuthAuthorizePageShared';
import {http} from '@app/features/platform/transport/RestTransport';
import {failureMessage} from '@app/features/platform/utils/ResponseInspection';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {canAuthorizeBotInvite} from '@fluxer/constants/src/BotPermissionUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {formatListWithConfig} from '@pkgs/list_utils/src/ListFormatting';
import {useEffect, useMemo, useRef, useState} from 'react';

const FAILED_TO_LOAD_YOUR_COMMUNITIES_DESCRIPTOR = msg({
	message: 'Failed to load your communities.',
	comment: 'Body text in the authentication bot guilds. Keep the tone plain and specific.',
});
const UNKNOWN_COMMUNITY_DESCRIPTOR = msg({
	message: 'Unknown community',
	comment: 'Short label in the authentication bot guilds. Keep the tone plain and specific.',
});
const FAILED_TO_LOAD_INVITE_DESTINATIONS_DESCRIPTOR = msg({
	message: 'Failed to load your communities and group DMs.',
	comment: 'Body text in the authentication bot invite destination selector. Keep the tone plain and specific.',
});
const UNKNOWN_GROUP_DM_DESCRIPTOR = msg({
	message: 'Group DM',
	comment: 'Fallback label in the authentication bot invite destination selector for an unnamed group DM.',
});

export type BotGuildsStatus = 'idle' | 'loading' | 'ready' | 'error';
export type BotInviteDestinationKind = 'guild' | 'group_dm';

export interface BotInviteDestinationOption extends GuildComboboxOption {
	kind: BotInviteDestinationKind;
	id: string;
	value: string;
	iconUrl?: string | null;
}

export interface BotInviteDestinationsResult {
	status: BotGuildsStatus;
	guilds: ReadonlyArray<GuildWithPermissions>;
	groupDms: ReadonlyArray<ChannelResponse>;
	options: ReadonlyArray<BotInviteDestinationOption>;
	labelByKey: ReadonlyMap<string, string>;
	error: string | null;
}

export interface BotGuildsResult {
	status: BotGuildsStatus;
	guilds: ReadonlyArray<GuildWithPermissions>;
	options: ReadonlyArray<GuildComboboxOption>;
	labelById: ReadonlyMap<string, string>;
	error: string | null;
}

export function mapBotGuilds(
	guilds: ReadonlyArray<GuildSummary>,
	requestedPermissions: bigint,
	unknownCommunityLabel: string,
): ReadonlyArray<GuildWithPermissions> {
	return guilds.map((guild) => {
		let permissionsValue = 0n;
		try {
			if (guild.permissions) permissionsValue = BigInt(guild.permissions);
		} catch {
			permissionsValue = 0n;
		}
		return {
			id: guild.id,
			name: guild.name ?? unknownCommunityLabel,
			icon: guild.icon ?? null,
			canAuthorizeBotInvite: canAuthorizeBotInvite({
				userPermissions: permissionsValue,
				requestedPermissions,
			}),
		};
	});
}

export function createBotInviteDestinationKey(kind: BotInviteDestinationKind, id: string): string {
	return `${kind}:${id}`;
}

export function parseBotInviteDestinationKey(value: string | null | undefined): {
	kind: BotInviteDestinationKind;
	id: string;
} | null {
	if (!value) return null;
	const separatorIndex = value.indexOf(':');
	if (separatorIndex <= 0) return null;
	const kind = value.slice(0, separatorIndex);
	const id = value.slice(separatorIndex + 1);
	if (!id) return null;
	if (kind !== 'guild' && kind !== 'group_dm') return null;
	return {kind, id};
}

function getGroupDmLabel(channel: ChannelResponse, unknownGroupDmLabel: string, locale: string): string {
	const explicitName = channel.name?.trim();
	if (explicitName) return explicitName;
	const recipients = channel.recipients ?? [];
	const names = recipients
		.map((recipient) => recipient.global_name?.trim() || recipient.username?.trim())
		.filter((name): name is string => Boolean(name));
	if (names.length > 0) return formatListWithConfig(names, {locale, style: 'narrow', type: 'conjunction'});
	return unknownGroupDmLabel;
}

export function useBotGuilds(enabled: boolean, requestedPermissions: bigint): BotGuildsResult {
	const {i18n} = useLingui();
	const i18nRef = useRef(i18n);
	i18nRef.current = i18n;
	const [status, setStatus] = useState<BotGuildsStatus>(enabled ? 'loading' : 'idle');
	const [raw, setRaw] = useState<ReadonlyArray<GuildSummary>>([]);
	const [error, setError] = useState<string | null>(null);
	useEffect(() => {
		if (!enabled) {
			setStatus('idle');
			setRaw([]);
			setError(null);
			return;
		}
		let cancelled = false;
		setStatus('loading');
		setError(null);
		(async () => {
			try {
				const resp = await http.get<Array<GuildSummary>>(Endpoints.USER_GUILDS_LIST);
				if (cancelled) return;
				setRaw(resp.body);
				setStatus('ready');
			} catch (err) {
				if (cancelled) return;
				logger.error('Failed to fetch user guilds', err);
				setError(failureMessage(err) ?? i18nRef.current._(FAILED_TO_LOAD_YOUR_COMMUNITIES_DESCRIPTOR));
				setRaw([]);
				setStatus('error');
			}
		})();
		return () => {
			cancelled = true;
		};
	}, [enabled]);
	const guilds = useMemo<ReadonlyArray<GuildWithPermissions>>(
		() => mapBotGuilds(raw, requestedPermissions, i18n._(UNKNOWN_COMMUNITY_DESCRIPTOR)),
		[raw, requestedPermissions, i18n.locale],
	);
	const options = useMemo<ReadonlyArray<GuildComboboxOption>>(
		() =>
			guilds
				.filter((g) => g.canAuthorizeBotInvite)
				.map((g) => ({value: g.id, label: g.name, icon: g.icon, isDisabled: false})),
		[guilds],
	);
	const labelById = useMemo(() => new Map(options.map((o) => [o.value, o.label])), [options]);
	return {status, guilds, options, labelById, error};
}

export function useBotInviteDestinations(enabled: boolean, requestedPermissions: bigint): BotInviteDestinationsResult {
	const {i18n} = useLingui();
	const i18nRef = useRef(i18n);
	i18nRef.current = i18n;
	const [status, setStatus] = useState<BotGuildsStatus>(enabled ? 'loading' : 'idle');
	const [rawGuilds, setRawGuilds] = useState<ReadonlyArray<GuildSummary>>([]);
	const [rawChannels, setRawChannels] = useState<ReadonlyArray<ChannelResponse>>([]);
	const [error, setError] = useState<string | null>(null);
	useEffect(() => {
		if (!enabled) {
			setStatus('idle');
			setRawGuilds([]);
			setRawChannels([]);
			setError(null);
			return;
		}
		let cancelled = false;
		setStatus('loading');
		setError(null);
		(async () => {
			try {
				const [guildsResp, channelsResp] = await Promise.all([
					http.get<Array<GuildSummary>>(Endpoints.USER_GUILDS_LIST),
					http.get<Array<ChannelResponse>>(Endpoints.USER_CHANNELS),
				]);
				if (cancelled) return;
				setRawGuilds(guildsResp.body);
				setRawChannels(channelsResp.body.filter((channel) => channel.type === ChannelTypes.GROUP_DM));
				setStatus('ready');
			} catch (err) {
				if (cancelled) return;
				logger.error('Failed to fetch bot invite destinations', err);
				setError(failureMessage(err) ?? i18nRef.current._(FAILED_TO_LOAD_INVITE_DESTINATIONS_DESCRIPTOR));
				setRawGuilds([]);
				setRawChannels([]);
				setStatus('error');
			}
		})();
		return () => {
			cancelled = true;
		};
	}, [enabled]);
	const guilds = useMemo<ReadonlyArray<GuildWithPermissions>>(
		() => mapBotGuilds(rawGuilds, requestedPermissions, i18n._(UNKNOWN_COMMUNITY_DESCRIPTOR)),
		[rawGuilds, requestedPermissions, i18n.locale],
	);
	const groupDms = rawChannels;
	const options = useMemo<ReadonlyArray<BotInviteDestinationOption>>(() => {
		const guildOptions = guilds
			.filter((g) => g.canAuthorizeBotInvite)
			.map<BotInviteDestinationOption>((g) => ({
				kind: 'guild',
				id: g.id,
				value: createBotInviteDestinationKey('guild', g.id),
				label: g.name,
				icon: g.icon,
				iconUrl: g.icon ? AvatarUtils.getGuildIconURL({id: g.id, icon: g.icon}) : null,
				isDisabled: false,
			}));
		const groupDmOptions = groupDms.map<BotInviteDestinationOption>((channel) => ({
			kind: 'group_dm',
			id: channel.id,
			value: createBotInviteDestinationKey('group_dm', channel.id),
			label: getGroupDmLabel(channel, i18n._(UNKNOWN_GROUP_DM_DESCRIPTOR), i18n.locale),
			icon: null,
			iconUrl: channel.icon ? AvatarUtils.getChannelIconURL({id: channel.id, icon: channel.icon}) : null,
			isDisabled: false,
		}));
		return [...guildOptions, ...groupDmOptions];
	}, [guilds, groupDms, i18n.locale]);
	const labelByKey = useMemo(() => new Map(options.map((o) => [o.value, o.label])), [options]);
	return {status, guilds, groupDms, options, labelByKey, error};
}
