// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import Guilds from '@app/features/guild/state/Guilds';
import {
	FIVE_MINUTES_DURATION_DESCRIPTOR,
	ONE_HOUR_DURATION_DESCRIPTOR,
	THIRTY_MINUTES_DURATION_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {MessageHistoryThresholdModal} from '@app/features/messaging/components/modals/MessageHistoryThresholdModal';
import Permission from '@app/features/permissions/state/Permission';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {omitTransientUploadFields} from '@app/lib/forms/TransientUploadFields';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {GuildSplashCardAlignmentValue} from '@fluxer/constants/src/GuildConstants';
import {GuildFeatures, GuildSplashCardAlignment, SystemChannelFlags} from '@fluxer/constants/src/GuildConstants';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useCallback, useMemo, useState} from 'react';
import type {UseFormReturn} from 'react-hook-form';

const MESSAGE_1_MINUTE_DESCRIPTOR = msg({
	message: '1 minute',
	comment: 'Short label in the guild overview tab utils helper. Keep it concise.',
});
const MESSAGE_15_MINUTES_DESCRIPTOR = msg({
	message: '15 minutes',
	comment: 'Short label in the guild overview tab utils helper. Keep it concise.',
});
const COMMUNITY_UPDATED_DESCRIPTOR = msg({
	message: 'Community updated',
	comment: 'Short label in the guild overview tab utils helper. Keep it concise.',
});

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
	message_history_cutoff: string | null;
	text_channel_flexible_names: boolean;
	detached_banner: boolean;
	hide_owner_crown: boolean;
}

export const GUILD_OVERVIEW_TAB_ID = 'overview';
const GUILD_OVERVIEW_TRANSIENT_UPLOAD_FIELDS = ['icon', 'banner', 'splash', 'embed_splash'] as const;

function getEmptyGuildOverviewFormValues(): FormInputs {
	return {
		splash_card_alignment: GuildSplashCardAlignment.CENTER,
		name: '',
		afk_channel_id: null,
		afk_timeout: 300,
		system_channel_id: null,
		suppress_join_notifications: false,
		default_message_notifications: MessageNotifications.ALL_MESSAGES,
		message_history_cutoff: null,
		text_channel_flexible_names: false,
		detached_banner: false,
		hide_owner_crown: false,
	};
}

export interface AfkTimeoutOptionRaw {
	value: number;
	label: MessageDescriptor;
}

export const afkTimeoutOptionsRaw: Array<AfkTimeoutOptionRaw> = [
	{value: 60, label: MESSAGE_1_MINUTE_DESCRIPTOR},
	{value: 300, label: FIVE_MINUTES_DURATION_DESCRIPTOR},
	{value: 900, label: MESSAGE_15_MINUTES_DESCRIPTOR},
	{value: 1800, label: THIRTY_MINUTES_DURATION_DESCRIPTOR},
	{value: 3600, label: ONE_HOUR_DURATION_DESCRIPTOR},
];

export interface SelectOption {
	value: string | null;
	label: string;
}

export function useGuildOverviewData(guildId: string) {
	const {i18n} = useLingui();
	const guild = Guilds.getGuild(guildId);
	const channels = Channels.getGuildChannels(guildId);
	const [hasClearedIcon, setHasClearedIcon] = useState(false);
	const [previewIconUrl, setPreviewIconUrl] = useState<string | null>(null);
	const [hasClearedBanner, setHasClearedBanner] = useState(false);
	const [previewBannerUrl, setPreviewBannerUrl] = useState<string | null>(null);
	const [hasClearedSplash, setHasClearedSplash] = useState(false);
	const [previewSplashUrl, setPreviewSplashUrl] = useState<string | null>(null);
	const [hasClearedEmbedSplash, setHasClearedEmbedSplash] = useState(false);
	const [previewEmbedSplashUrl, setPreviewEmbedSplashUrl] = useState<string | null>(null);
	const [bannerAspectRatio, setBannerAspectRatio] = useState<number | undefined>();
	const [splashAspectRatio, setSplashAspectRatio] = useState<number | undefined>();
	const canManageGuild = Permission.can(Permissions.MANAGE_GUILD, {guildId});
	const computeAspectRatioFromBase64 = useCallback((base64Url: string): Promise<number> => {
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
	const voiceChannels = useMemo(() => {
		return channels.filter((channel) => channel.type === ChannelTypes.GUILD_VOICE);
	}, [channels]);
	const textChannels = useMemo(() => {
		return channels.filter(
			(channel) => channel.type === ChannelTypes.GUILD_TEXT || channel.type === ChannelTypes.GUILD_ANNOUNCEMENT,
		);
	}, [channels]);
	const defaultValues: FormInputs = guild
		? {
				name: guild.name || '',
				splash_card_alignment: guild.splashCardAlignment ?? GuildSplashCardAlignment.CENTER,
				afk_channel_id: guild.afkChannelId || null,
				afk_timeout: guild.afkTimeout || 300,
				system_channel_id: guild.systemChannelId || null,
				suppress_join_notifications: !!(guild.systemChannelFlags & SystemChannelFlags.SUPPRESS_JOIN_NOTIFICATIONS),
				default_message_notifications: guild.defaultMessageNotifications || MessageNotifications.ALL_MESSAGES,
				message_history_cutoff: guild.messageHistoryCutoff ?? null,
				text_channel_flexible_names: guild.features.has(GuildFeatures.TEXT_CHANNEL_FLEXIBLE_NAMES) ?? false,
				detached_banner: guild.features.has(GuildFeatures.DETACHED_BANNER) ?? false,
				hide_owner_crown: guild.features.has(GuildFeatures.HIDE_OWNER_CROWN) ?? false,
			}
		: getEmptyGuildOverviewFormValues();
	const clearLocalAssetState = useCallback(() => {
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
	}, []);
	const handleReset = useCallback(
		(formInstance: UseFormReturn<FormInputs>) => {
			if (!guild) return;
			formInstance.reset(defaultValues);
			clearLocalAssetState();
		},
		[guild, defaultValues, clearLocalAssetState],
	);
	const onSubmit = useCallback(
		async (data: FormInputs, formInstance: UseFormReturn<FormInputs>) => {
			if (!guild) return;
			let systemChannelFlags = guild.systemChannelFlags;
			if (data.suppress_join_notifications) {
				systemChannelFlags |= SystemChannelFlags.SUPPRESS_JOIN_NOTIFICATIONS;
			} else {
				systemChannelFlags &= ~SystemChannelFlags.SUPPRESS_JOIN_NOTIFICATIONS;
			}
			const update: GuildCommands.GuildUpdatePayload = {};
			const dirtyFields = formInstance.formState.dirtyFields;
			if (dirtyFields.name) update.name = data.name;
			if (dirtyFields.icon) update.icon = data.icon;
			if (dirtyFields.banner) update.banner = data.banner;
			if (dirtyFields.splash) update.splash = data.splash;
			if (dirtyFields.embed_splash) update.embed_splash = data.embed_splash;
			if (dirtyFields.splash_card_alignment) update.splash_card_alignment = data.splash_card_alignment;
			if (dirtyFields.afk_channel_id) update.afk_channel_id = data.afk_channel_id;
			if (dirtyFields.afk_timeout) update.afk_timeout = data.afk_timeout;
			if (dirtyFields.system_channel_id) update.system_channel_id = data.system_channel_id;
			if (dirtyFields.suppress_join_notifications) update.system_channel_flags = systemChannelFlags;
			if (dirtyFields.default_message_notifications)
				update.default_message_notifications = data.default_message_notifications;
			if (dirtyFields.message_history_cutoff) update.message_history_cutoff = data.message_history_cutoff;
			const nextFeatures = new Set(guild.features);
			let featuresChanged = false;
			const applyToggle = (feature: string, desired: boolean) => {
				const present = nextFeatures.has(feature);
				if (present === desired) return;
				if (desired) {
					nextFeatures.add(feature);
				} else {
					nextFeatures.delete(feature);
				}
				featuresChanged = true;
			};
			const flexibleNamesCurrent = guild.features.has(GuildFeatures.TEXT_CHANNEL_FLEXIBLE_NAMES);
			if (data.text_channel_flexible_names !== flexibleNamesCurrent) {
				applyToggle(GuildFeatures.TEXT_CHANNEL_FLEXIBLE_NAMES, data.text_channel_flexible_names);
			}
			const detachedBannerCurrent = guild.features.has(GuildFeatures.DETACHED_BANNER);
			if (data.detached_banner !== detachedBannerCurrent) {
				applyToggle(GuildFeatures.DETACHED_BANNER, data.detached_banner);
			}
			const hideOwnerCrownCurrent = guild.features.has(GuildFeatures.HIDE_OWNER_CROWN);
			if (data.hide_owner_crown !== hideOwnerCrownCurrent) {
				applyToggle(GuildFeatures.HIDE_OWNER_CROWN, data.hide_owner_crown);
			}
			if (featuresChanged) update.features = Array.from(nextFeatures);
			if (Object.keys(update).length > 0) {
				await GuildCommands.update(guild.id, update);
			}
			formInstance.reset(omitTransientUploadFields(data, GUILD_OVERVIEW_TRANSIENT_UPLOAD_FIELDS));
			clearLocalAssetState();
			ToastCommands.createToast({type: 'success', children: i18n._(COMMUNITY_UPDATED_DESCRIPTOR)});
		},
		[guild, clearLocalAssetState, i18n],
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
		clearLocalAssetState,
		handleReset,
		onSubmit,
	};
}

export function openMessageHistoryThresholdSettings(guildId: string): void {
	ModalCommands.push(
		modal(() => (
			<MessageHistoryThresholdModal
				guildId={guildId}
				data-flx="guild.guild-tabs.guild-overview-tab-utils.open-message-history-threshold-settings.message-history-threshold-modal"
			/>
		)),
	);
}
