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

import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import React from 'react';
import type {UseFormReturn} from 'react-hook-form';
import * as GuildActionCreators from '~/actions/GuildActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import type {GuildSplashCardAlignmentValue} from '~/Constants';
import {
	ChannelTypes,
	GuildFeatures,
	GuildSplashCardAlignment,
	MessageNotifications,
	Permissions,
	SystemChannelFlags,
} from '~/Constants';
import ChannelStore from '~/stores/ChannelStore';
import GuildStore from '~/stores/GuildStore';
import PermissionStore from '~/stores/PermissionStore';

export interface FormInputs {
	icon?: string | null;
	banner?: string | null;
	splash?: string | null;
	embed_splash?: string | null;
	splash_card_alignment: GuildSplashCardAlignmentValue;
	name: string;
	afk_channel_id: string | null;
	afk_timeout: number;
	system_channel_id: string | null;
	suppress_join_notifications: boolean;
	default_message_notifications: number;
	text_channel_flexible_names: boolean;
	detached_banner: boolean;
	disallow_unclaimed_accounts: boolean;
}

export const GUILD_OVERVIEW_TAB_ID = 'overview';

export interface AfkTimeoutOptionRaw {
	value: number;
	label: MessageDescriptor;
}

export const afkTimeoutOptionsRaw: Array<AfkTimeoutOptionRaw> = [
	{value: 60, label: msg`1 minute`},
	{value: 300, label: msg`5 minutes`},
	{value: 900, label: msg`15 minutes`},
	{value: 1800, label: msg`30 minutes`},
	{value: 3600, label: msg`1 hour`},
];

export interface SelectOption {
	value: string | null;
	label: string;
}

export const useGuildOverviewData = (guildId: string) => {
	const guild = GuildStore.getGuild(guildId);
	const channels = ChannelStore.getGuildChannels(guildId);
	const [hasClearedIcon, setHasClearedIcon] = React.useState(false);
	const [previewIconUrl, setPreviewIconUrl] = React.useState<string | null>(null);
	const [hasClearedBanner, setHasClearedBanner] = React.useState(false);
	const [previewBannerUrl, setPreviewBannerUrl] = React.useState<string | null>(null);
	const [hasClearedSplash, setHasClearedSplash] = React.useState(false);
	const [previewSplashUrl, setPreviewSplashUrl] = React.useState<string | null>(null);
	const [hasClearedEmbedSplash, setHasClearedEmbedSplash] = React.useState(false);
	const [previewEmbedSplashUrl, setPreviewEmbedSplashUrl] = React.useState<string | null>(null);

	const [bannerAspectRatio, setBannerAspectRatio] = React.useState<number | undefined>();
	const [splashAspectRatio, setSplashAspectRatio] = React.useState<number | undefined>();

	const canManageGuild = PermissionStore.can(Permissions.MANAGE_GUILD, {guildId});

	const computeAspectRatioFromBase64 = React.useCallback((base64Url: string): Promise<number> => {
		if (typeof Image !== 'undefined') {
			return new Promise((resolve, reject) => {
				const img = new Image();
				img.onload = () => {
					if (img.naturalWidth > 0 && img.naturalHeight > 0) {
						resolve(img.naturalWidth / img.naturalHeight);
					} else {
						reject(new Error('Invalid image dimensions'));
					}
					img.onload = null;
					img.onerror = null;
				};
				img.onerror = () => {
					reject(new Error('Failed to load image'));
					img.onload = null;
					img.onerror = null;
				};
				img.src = base64Url;
			});
		} else {
			return Promise.resolve(16 / 9);
		}
	}, []);

	const voiceChannels = React.useMemo(() => {
		return channels.filter((channel) => channel.type === ChannelTypes.GUILD_VOICE);
	}, [channels]);

	const textChannels = React.useMemo(() => {
		return channels.filter((channel) => channel.type === ChannelTypes.GUILD_TEXT);
	}, [channels]);

	const defaultValues = React.useMemo(() => {
		if (!guild) return {} as Partial<FormInputs>;
		return {
			name: guild.name || '',
			splash_card_alignment: guild.splashCardAlignment ?? GuildSplashCardAlignment.CENTER,
			afk_channel_id: guild.afkChannelId || null,
			afk_timeout: guild.afkTimeout || 300,
			system_channel_id: guild.systemChannelId || null,
			suppress_join_notifications: !!(guild.systemChannelFlags & SystemChannelFlags.SUPPRESS_JOIN_NOTIFICATIONS),
			default_message_notifications: guild.defaultMessageNotifications || MessageNotifications.ALL_MESSAGES,
			text_channel_flexible_names: guild.features.has(GuildFeatures.TEXT_CHANNEL_FLEXIBLE_NAMES) ?? false,
			detached_banner: guild.features.has(GuildFeatures.DETACHED_BANNER) ?? false,
			disallow_unclaimed_accounts: guild.features.has(GuildFeatures.DISALLOW_UNCLAIMED_ACCOUNTS) ?? false,
		};
	}, [guild]);

	const handleReset = React.useCallback(
		(formInstance: UseFormReturn<FormInputs>) => {
			if (!guild) return;
			formInstance.reset({
				name: guild.name,
				splash_card_alignment: guild.splashCardAlignment ?? GuildSplashCardAlignment.CENTER,
				afk_channel_id: guild.afkChannelId || null,
				afk_timeout: guild.afkTimeout || 300,
				system_channel_id: guild.systemChannelId || null,
				suppress_join_notifications: !!(guild.systemChannelFlags & SystemChannelFlags.SUPPRESS_JOIN_NOTIFICATIONS),
				default_message_notifications: guild.defaultMessageNotifications || MessageNotifications.ALL_MESSAGES,
				text_channel_flexible_names: guild.features.has(GuildFeatures.TEXT_CHANNEL_FLEXIBLE_NAMES),
				detached_banner: guild.features.has(GuildFeatures.DETACHED_BANNER),
				disallow_unclaimed_accounts: guild.features.has(GuildFeatures.DISALLOW_UNCLAIMED_ACCOUNTS),
			});
			setPreviewIconUrl(null);
			setHasClearedIcon(false);
			setPreviewBannerUrl(null);
			setHasClearedBanner(false);
			setPreviewSplashUrl(null);
			setHasClearedSplash(false);
			setPreviewEmbedSplashUrl(null);
			setHasClearedEmbedSplash(false);
			setBannerAspectRatio(undefined);
			setSplashAspectRatio(undefined);
		},
		[
			guild,
			setPreviewIconUrl,
			setHasClearedIcon,
			setPreviewBannerUrl,
			setHasClearedBanner,
			setPreviewSplashUrl,
			setHasClearedSplash,
			setPreviewEmbedSplashUrl,
			setHasClearedEmbedSplash,
			setBannerAspectRatio,
			setSplashAspectRatio,
		],
	);

	const onSubmit = React.useCallback(
		async (data: FormInputs, formInstance: UseFormReturn<FormInputs>) => {
			if (!guild) return;

			let systemChannelFlags = guild.systemChannelFlags;
			if (data.suppress_join_notifications) {
				systemChannelFlags |= SystemChannelFlags.SUPPRESS_JOIN_NOTIFICATIONS;
			} else {
				systemChannelFlags &= ~SystemChannelFlags.SUPPRESS_JOIN_NOTIFICATIONS;
			}

			await GuildActionCreators.update(guild.id, {
				icon: data.icon,
				banner: data.banner,
				splash: data.splash,
				splash_card_alignment: data.splash_card_alignment,
				embed_splash: data.embed_splash,
				name: data.name,
				afk_channel_id: data.afk_channel_id,
				afk_timeout: data.afk_timeout,
				system_channel_id: data.system_channel_id,
				system_channel_flags: systemChannelFlags,
				default_message_notifications: data.default_message_notifications,
			});

			const currentlyEnabled = guild.features.has(GuildFeatures.TEXT_CHANNEL_FLEXIBLE_NAMES);
			if (data.text_channel_flexible_names !== currentlyEnabled) {
				await GuildActionCreators.toggleTextChannelFlexibleNames(guild.id, data.text_channel_flexible_names);
			}

			const detachedBannerEnabled = guild.features.has(GuildFeatures.DETACHED_BANNER);
			if (data.detached_banner !== detachedBannerEnabled) {
				await GuildActionCreators.toggleDetachedBanner(guild.id, data.detached_banner);
			}

			const disallowUnclaimedAccountsEnabled = guild.features.has(GuildFeatures.DISALLOW_UNCLAIMED_ACCOUNTS);
			if (data.disallow_unclaimed_accounts !== disallowUnclaimedAccountsEnabled) {
				await GuildActionCreators.toggleDisallowUnclaimedAccounts(guild.id, data.disallow_unclaimed_accounts);
			}

			formInstance.reset(data);
			setPreviewIconUrl(null);
			setHasClearedIcon(false);
			setPreviewBannerUrl(null);
			setHasClearedBanner(false);
			setPreviewSplashUrl(null);
			setHasClearedSplash(false);
			setPreviewEmbedSplashUrl(null);
			setHasClearedEmbedSplash(false);
			setBannerAspectRatio(undefined);
			setSplashAspectRatio(undefined);

			ToastActionCreators.createToast({type: 'success', children: 'Community updated'});
		},
		[
			guild,
			setPreviewIconUrl,
			setHasClearedIcon,
			setPreviewBannerUrl,
			setHasClearedBanner,
			setPreviewSplashUrl,
			setHasClearedSplash,
			setPreviewEmbedSplashUrl,
			setHasClearedEmbedSplash,
			setBannerAspectRatio,
			setSplashAspectRatio,
		],
	);

	return {
		guild,
		channels,
		hasClearedIcon,
		setHasClearedIcon,
		previewIconUrl,
		setPreviewIconUrl,
		hasClearedBanner,
		setHasClearedBanner,
		previewBannerUrl,
		setPreviewBannerUrl,
		hasClearedSplash,
		setHasClearedSplash,
		previewSplashUrl,
		setPreviewSplashUrl,
		hasClearedEmbedSplash,
		setHasClearedEmbedSplash,
		previewEmbedSplashUrl,
		setPreviewEmbedSplashUrl,
		bannerAspectRatio,
		setBannerAspectRatio,
		splashAspectRatio,
		setSplashAspectRatio,
		canManageGuild,
		computeAspectRatioFromBase64,
		voiceChannels,
		textChannels,
		defaultValues,
		handleReset,
		onSubmit,
	};
};
