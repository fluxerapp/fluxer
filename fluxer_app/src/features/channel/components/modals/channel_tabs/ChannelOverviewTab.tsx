// SPDX-License-Identifier: AGPL-3.0-or-later

import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {EXAMPLE_GENERAL_CHANNEL_NAME, EXAMPLE_URL} from '@app/features/app/config/I18nDisplayConstants';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import type {ChannelRtcRegion} from '@app/features/channel/commands/ChannelCommands';
import * as ChannelCommands from '@app/features/channel/commands/ChannelCommands';
import {showChannelErrorModal} from '@app/features/channel/components/alerts/ChannelErrorModalUtils';
import {VoiceRegionsLoadFailedModal} from '@app/features/channel/components/alerts/VoiceRegionsLoadFailedModal';
import styles from '@app/features/channel/components/modals/channel_tabs/ChannelOverviewTab.module.css';
import {
	ChannelOverviewTopicEditor,
	type ChannelOverviewTopicEditorHandle,
} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/ChannelOverviewTopicEditor';
import {MatureContentSection} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/MatureContentSection';
import {RtcRegionSelect} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/RtcRegionSelect';
import {SlowmodeControl} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/SlowmodeControl';
import {
	BITRATE_KBPS_DEFAULT,
	CHANNEL_OVERVIEW_TAB_ID,
	type FormInputs,
	getMaxBitrateKbps,
} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/shared';
import {
	VoiceConnectionLimitControl,
	VoiceSettings,
} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/VoiceSettings';
import Channels from '@app/features/channel/state/Channels';
import Guilds from '@app/features/guild/state/Guilds';
import {
	ANNOUNCEMENT_CHANNEL_DESCRIPTOR,
	TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Permission from '@app/features/permissions/state/Permission';
import {failureCode, failureMessage} from '@app/features/platform/utils/ResponseInspection';
import {ThreadDefaultsSection} from '@app/features/threads/components/ThreadDefaultsSection';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import * as UnsavedChangesCommands from '@app/features/ui/commands/UnsavedChangesCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import VoiceP2pRollout from '@app/features/voice/state/VoiceP2pRollout';
import {
	CHANNEL_RTC_P2P_SETTING_DESCRIPTION_DESCRIPTOR,
	CHANNEL_RTC_P2P_SETTING_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {useRemoteFormReset} from '@app/lib/forms/RemoteFormReset';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {
	ANNOUNCEMENT_CONVERTIBLE_CHANNEL_TYPES,
	ChannelTypes,
	GUILD_TEXT_BASED_CHANNEL_TYPES,
	Permissions,
} from '@fluxer/constants/src/ChannelConstants';
import {ContentWarningLevel} from '@fluxer/constants/src/GuildConstants';
import {VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT} from '@fluxer/constants/src/LimitConstants';
import {
	DEFAULT_THREAD_AUTO_ARCHIVE_DURATION,
	TEXT_THREAD_PARENT_CHANNEL_TYPES,
} from '@fluxer/constants/src/ThreadConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';
import {Controller, useForm} from 'react-hook-form';

const CHANNEL_TOPIC_IS_TOO_LONG_DESCRIPTOR = msg({
	message: 'Channel topic is too long.',
	comment:
		'Channel overview settings tab label, control, or validation message (name, topic, slowmode, voice region, mature content gate).',
});
const SHORTEN_THE_TOPIC_AND_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'Shorten the topic and try again.',
	comment: 'Body of the error modal shown when the channel topic exceeds the maximum length.',
});
const CATEGORY_NAME_DESCRIPTOR = msg({
	message: 'Category name',
	comment:
		'Channel overview settings tab label, control, or validation message (name, topic, slowmode, voice region, mature content gate).',
});
const CHANNEL_NAME_DESCRIPTOR = msg({
	message: 'Channel name',
	comment:
		'Channel overview settings tab label, control, or validation message (name, topic, slowmode, voice region, mature content gate).',
});
const MY_CATEGORY_DESCRIPTOR = msg({
	message: 'My category',
	comment:
		'Channel overview settings tab label, control, or validation message (name, topic, slowmode, voice region, mature content gate).',
});
const URL_DESCRIPTOR = msg({
	message: 'URL',
	comment:
		'Channel overview settings tab label, control, or validation message (name, topic, slowmode, voice region, mature content gate).',
});
const ANNOUNCEMENT_CHANNEL_SWITCH_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Lets other communities follow this channel and get copies of what you publish.',
	comment:
		'Description under the Announcement channel switch in channel settings. Turning it on converts a text channel into an announcement channel. Publishing a message sends a copy of it to every channel in other communities that follows this one.',
});
const STOP_BEING_AN_ANNOUNCEMENT_CHANNEL_DESCRIPTOR = msg({
	message: 'Stop being an announcement channel?',
	comment:
		'Title of the confirmation shown when a member turns off the Announcement channel switch in channel settings, which converts the channel back into a normal text channel.',
});
const CONVERT_TO_TEXT_CHANNEL_FOLLOWERS_DESCRIPTOR = msg({
	message:
		'{count, plural, one {# channel follows} other {# channels follow}} this channel. Converting it to a text channel removes those follows.',
	comment:
		'Body of the confirmation shown before an announcement channel is converted back into a text channel. {count} is the number of channels, in this or other communities, that follow it and get copies of its published messages. Those channels stop getting copies after the conversion.',
});
const CONVERT_TO_TEXT_CHANNEL_UNKNOWN_FOLLOWERS_DESCRIPTOR = msg({
	message: 'Converting this channel to a text channel removes every channel that follows it.',
	comment:
		'Body of the confirmation shown before an announcement channel is converted back into a text channel, used when the number of following channels could not be loaded. Channels that follow it get copies of its published messages and stop getting them after the conversion.',
});
const CONVERT_DESCRIPTOR = msg({
	message: 'Convert',
	comment: 'Button in the confirmation that converts an announcement channel back into a text channel. Keep it short.',
});
const COULD_NOT_CONVERT_CHANNEL_DESCRIPTOR = msg({
	message: "Couldn't convert this channel",
	comment:
		'Title of the error dialog shown when converting a channel between a text channel and an announcement channel failed.',
});
const COULD_NOT_SAVE_CHANNEL_DESCRIPTOR = msg({
	message: "Couldn't save channel settings",
	comment: 'Title of the error dialog shown when saving the channel overview settings failed.',
});
const ChannelOverviewTab: React.FC<{channelId: string}> = observer(({channelId}) => {
	const {i18n} = useLingui();
	const channel = Channels.getChannel(channelId);
	const guildId = channel?.guildId ?? null;
	const guild = guildId ? Guilds.getGuild(guildId) : null;
	const canUpdateRtcRegion =
		guildId !== null ? Permission.can(Permissions.UPDATE_RTC_REGION, {guildId, channelId}) : false;
	const canManageChannel = guildId !== null ? Permission.can(Permissions.MANAGE_CHANNELS, {guildId, channelId}) : false;
	const isVoiceChannel = channel?.type === ChannelTypes.GUILD_VOICE;
	const maxBitrateKbps = getMaxBitrateKbps(guild?.features);
	const [rtcRegions, setRtcRegions] = useState<Array<ChannelRtcRegion>>([]);
	const [isLoadingRegions, setIsLoadingRegions] = useState(false);
	const form = useForm<FormInputs>({
		defaultValues: {
			name: '',
			topic: '',
			url: '',
			slowmode: 0,
			nsfw_override: null,
			content_warning_level: ContentWarningLevel.INHERIT,
			content_warning_text: '',
			announcement: false,
			bitrate: BITRATE_KBPS_DEFAULT,
			user_limit: 0,
			voice_connection_limit: VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT,
			rtc_region: null,
			rtc_p2p: false,
		},
	});
	const threadDefaultsActive =
		channel != null &&
		TEXT_THREAD_PARENT_CHANNEL_TYPES.has(channel.type) &&
		ThreadGuilds.isActive(guildId) &&
		canManageChannel;
	const remoteValues: FormInputs | null = channel
		? {
				...(threadDefaultsActive
					? {
							default_auto_archive_duration: channel.defaultAutoArchiveDuration ?? DEFAULT_THREAD_AUTO_ARCHIVE_DURATION,
							default_thread_rate_limit_per_user: channel.defaultThreadRateLimitPerUser,
						}
					: {}),
				name: channel.name || '',
				topic: channel.topic || '',
				url: channel.url || '',
				slowmode: channel.rateLimitPerUser || 0,
				nsfw_override: channel.nsfwOverride,
				content_warning_level: channel.contentWarningLevel ?? ContentWarningLevel.INHERIT,
				content_warning_text: channel.contentWarningText ?? '',
				announcement: channel.type === ChannelTypes.GUILD_ANNOUNCEMENT,
				bitrate: Math.min(channel.bitrate ? Math.round(channel.bitrate / 1000) : BITRATE_KBPS_DEFAULT, maxBitrateKbps),
				user_limit: channel.userLimit ?? 0,
				voice_connection_limit: channel.voiceConnectionLimit ?? VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT,
				rtc_region: channel.rtcRegion ?? null,
				rtc_p2p: channel.rtcP2p,
			}
		: null;
	useEffect(() => {
		if (!canUpdateRtcRegion || !isVoiceChannel) {
			setRtcRegions([]);
			setIsLoadingRegions(false);
			return;
		}
		let cancelled = false;
		setIsLoadingRegions(true);
		ChannelCommands.fetchRtcRegions(channelId)
			.then((regions) => {
				if (cancelled) return;
				setRtcRegions(regions);
			})
			.catch(() => {
				if (cancelled) return;
				setRtcRegions([]);
				ModalCommands.push(
					modal(() => (
						<VoiceRegionsLoadFailedModal data-flx="channel.channel-tabs.channel-overview-tab.fetch-rtc-regions.voice-regions-load-failed-modal" />
					)),
				);
			})
			.finally(() => {
				if (cancelled) return;
				setIsLoadingRegions(false);
			});
		return () => {
			cancelled = true;
		};
	}, [canUpdateRtcRegion, channelId, isVoiceChannel]);
	useEffect(() => {
		if (!canUpdateRtcRegion || !isVoiceChannel || rtcRegions.length === 0) {
			return;
		}
		const currentValue = form.getValues('rtc_region');
		if (currentValue && !rtcRegions.some((region) => region.id === currentValue)) {
			form.setValue('rtc_region', null, {shouldDirty: false, shouldTouch: false});
		}
	}, [canUpdateRtcRegion, form, isVoiceChannel, rtcRegions]);
	const topicEditorRef = useRef<ChannelOverviewTopicEditorHandle | null>(null);
	const handleTopicExceedsLimit = useCallback(() => {
		showChannelErrorModal({
			title: i18n._(CHANNEL_TOPIC_IS_TOO_LONG_DESCRIPTOR),
			message: i18n._(SHORTEN_THE_TOPIC_AND_TRY_AGAIN_DESCRIPTOR),
			dataFlx: 'channel.channel-tabs.channel-overview-tab.topic-too-long.generic-error-modal',
		});
	}, [i18n]);
	const applyRemoteValues = useCallback((values: FormInputs) => {
		const topicEditor = topicEditorRef.current;
		if (topicEditor != null) {
			topicEditor.syncFromMarkdown(values.topic);
		}
	}, []);
	const {resetToRemoteValues, commitRemoteValues} = useRemoteFormReset<FormInputs>({
		form,
		identityKey: channelId,
		remoteValues,
		onApply: applyRemoteValues,
	});
	const onSubmit = useCallback(
		async (data: FormInputs) => {
			if (!channel) return;
			const dirty = form.formState.dirtyFields;
			const updateData: Record<string, unknown> = {};
			if (canManageChannel) {
				updateData.name = data.name;
				if (GUILD_TEXT_BASED_CHANNEL_TYPES.has(channel.type)) {
					updateData.topic = data.topic;
					updateData.rate_limit_per_user = data.slowmode;
				}
				if (channel.type === ChannelTypes.GUILD_VOICE) {
					updateData.bitrate = Math.min(data.bitrate ?? BITRATE_KBPS_DEFAULT, maxBitrateKbps) * 1000;
					updateData.user_limit = data.user_limit;
					updateData.voice_connection_limit = data.voice_connection_limit ?? VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT;
					if (dirty.rtc_p2p) updateData.rtc_p2p = data.rtc_p2p;
				} else if (channel.type === ChannelTypes.GUILD_LINK) {
					updateData.url = data.url;
				}
				if (ANNOUNCEMENT_CONVERTIBLE_CHANNEL_TYPES.has(channel.type) && dirty.announcement) {
					updateData.type = data.announcement ? ChannelTypes.GUILD_ANNOUNCEMENT : ChannelTypes.GUILD_TEXT;
				}
				if (channel.guildId) {
					if (dirty.nsfw_override) updateData.nsfw_override = data.nsfw_override;
					if (dirty.content_warning_level) updateData.content_warning_level = data.content_warning_level;
					if (dirty.content_warning_text) {
						const trimmed = (data.content_warning_text ?? '').trim();
						updateData.content_warning_text = trimmed.length > 0 ? trimmed : null;
					}
				}
			}
			if (threadDefaultsActive) {
				if (dirty.default_auto_archive_duration) {
					updateData.default_auto_archive_duration = data.default_auto_archive_duration;
				}
				if (dirty.default_thread_rate_limit_per_user) {
					updateData.default_thread_rate_limit_per_user = data.default_thread_rate_limit_per_user;
				}
			}
			if (channel.type === ChannelTypes.GUILD_VOICE && (canManageChannel || canUpdateRtcRegion) && dirty.rtc_region) {
				updateData.rtc_region = data.rtc_region ?? null;
			}
			if (Object.keys(updateData).length === 0) {
				ToastCommands.createToast({type: 'success', children: <Trans>Channel updated</Trans>});
				return;
			}
			const persist = async () => {
				try {
					await ChannelCommands.update(channel.id, updateData);
				} catch (error) {
					const code = failureCode(error);
					if (code === APIErrorCodes.CHANNEL_HAS_FOLLOWED_CHANNELS || code === APIErrorCodes.CHANNEL_HAS_THREADS) {
						showGenericErrorModal({
							title: i18n._(COULD_NOT_CONVERT_CHANNEL_DESCRIPTOR),
							message: failureMessage(error) ?? i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
							dataFlx: 'channel.channel-tabs.channel-overview-tab.channel-has-followed-channels.generic-error-modal',
						});
						return;
					}
					throw error;
				}
				const currentValues = form.getValues();
				commitRemoteValues({
					name: data.name,
					topic: data.topic ?? '',
					url: data.url ?? '',
					slowmode: data.slowmode ?? currentValues.slowmode ?? 0,
					announcement: data.announcement ?? currentValues.announcement ?? false,
					nsfw_override: data.nsfw_override,
					content_warning_level: data.content_warning_level,
					content_warning_text: data.content_warning_text ?? '',
					bitrate: Math.min(data.bitrate ?? currentValues.bitrate ?? BITRATE_KBPS_DEFAULT, maxBitrateKbps),
					user_limit: data.user_limit ?? currentValues.user_limit ?? 0,
					voice_connection_limit:
						data.voice_connection_limit ??
						currentValues.voice_connection_limit ??
						VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT,
					rtc_region: data.rtc_region ?? currentValues.rtc_region ?? null,
					...(threadDefaultsActive
						? {
								default_auto_archive_duration: data.default_auto_archive_duration,
								default_thread_rate_limit_per_user: data.default_thread_rate_limit_per_user,
							}
						: {}),
					rtc_p2p: data.rtc_p2p,
				});
				ToastCommands.createToast({type: 'success', children: <Trans>Channel updated</Trans>});
			};
			if (channel.type === ChannelTypes.GUILD_ANNOUNCEMENT && updateData.type === ChannelTypes.GUILD_TEXT) {
				const followerCount = await ChannelCommands.fetchFollowerStats(channel.id).then(
					(stats) => stats.channel_count,
					() => null,
				);
				if (followerCount !== 0) {
					ModalCommands.push(
						modal(() => (
							<ConfirmModal
								title={i18n._(STOP_BEING_AN_ANNOUNCEMENT_CHANNEL_DESCRIPTOR)}
								description={
									followerCount === null
										? i18n._(CONVERT_TO_TEXT_CHANNEL_UNKNOWN_FOLLOWERS_DESCRIPTOR)
										: i18n._(CONVERT_TO_TEXT_CHANNEL_FOLLOWERS_DESCRIPTOR, {count: followerCount})
								}
								primaryText={i18n._(CONVERT_DESCRIPTOR)}
								primaryVariant="danger"
								onPrimary={async () => {
									try {
										await persist();
									} catch (error) {
										showGenericErrorModal({
											title: i18n._(COULD_NOT_SAVE_CHANNEL_DESCRIPTOR),
											message: failureMessage(error) ?? i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
											dataFlx: 'channel.channel-tabs.channel-overview-tab.convert-to-text.generic-error-modal',
										});
									}
								}}
								data-flx="channel.channel-tabs.channel-overview-tab.convert-to-text.confirm-modal"
							/>
						)),
					);
					return;
				}
			}
			await persist();
		},
		[
			canManageChannel,
			canUpdateRtcRegion,
			channel,
			form,
			commitRemoteValues,
			maxBitrateKbps,
			i18n,
			threadDefaultsActive,
		],
	);
	const {handleSubmit: handleSave} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'name',
	});
	const handleReset = useCallback(() => {
		resetToRemoteValues();
	}, [resetToRemoteValues]);
	const isFormDirty = form.formState.isDirty;
	const hasUnsavedChanges = Boolean(isFormDirty);
	useEffect(() => {
		UnsavedChangesCommands.setUnsavedChanges(CHANNEL_OVERVIEW_TAB_ID, hasUnsavedChanges);
	}, [hasUnsavedChanges]);
	useEffect(() => {
		UnsavedChangesCommands.setTabData(CHANNEL_OVERVIEW_TAB_ID, {
			onReset: handleReset,
			onSave: handleSave,
			isSubmitting: form.formState.isSubmitting,
		});
	}, [handleReset, handleSave, form.formState.isSubmitting]);
	useEffect(() => {
		return () => {
			UnsavedChangesCommands.clearUnsavedChanges(CHANNEL_OVERVIEW_TAB_ID);
		};
	}, []);
	if (!channel) return null;
	const isTextChannel = channel.type === ChannelTypes.GUILD_TEXT || channel.type === ChannelTypes.GUILD_ANNOUNCEMENT;
	const isAnnouncementConvertible = ANNOUNCEMENT_CONVERTIBLE_CHANNEL_TYPES.has(channel.type);
	const isGuildVoiceChannel = channel.type === ChannelTypes.GUILD_VOICE;
	const isMessageableGuildChannel = GUILD_TEXT_BASED_CHANNEL_TYPES.has(channel.type);
	const isCategory = channel.type === ChannelTypes.GUILD_CATEGORY;
	const isLinkChannel = channel.type === ChannelTypes.GUILD_LINK;
	const showGeneralSection = canManageChannel;
	const showMessagingSection = canManageChannel && isMessageableGuildChannel;
	const showVoiceSection = isGuildVoiceChannel && (canManageChannel || canUpdateRtcRegion);
	const showSafetySection =
		canManageChannel &&
		Boolean(channel.guildId) &&
		(isTextChannel || isGuildVoiceChannel || isLinkChannel || isCategory);
	const showAdvancedSection = canManageChannel && isGuildVoiceChannel;
	return (
		<div className={styles.sectionWrapper} data-flx="channel.channel-tabs.channel-overview-tab.section-wrapper">
			<Form form={form} onSubmit={handleSave} data-flx="channel.channel-tabs.channel-overview-tab.form.save">
				{showGeneralSection && (
					<div className={styles.settingsGroup} data-flx="channel.channel-tabs.channel-overview-tab.settings-group">
						<Input
							data-flx="channel.channel-tabs.channel-overview-tab.input.text"
							{...form.register('name')}
							type="text"
							label={isCategory ? i18n._(CATEGORY_NAME_DESCRIPTOR) : i18n._(CHANNEL_NAME_DESCRIPTOR)}
							placeholder={isCategory ? i18n._(MY_CATEGORY_DESCRIPTOR) : EXAMPLE_GENERAL_CHANNEL_NAME}
							minLength={1}
							maxLength={100}
							error={form.formState.errors.name?.message}
						/>
						{isLinkChannel && (
							<Input
								data-flx="channel.channel-tabs.channel-overview-tab.input.url"
								{...form.register('url')}
								type="url"
								label={i18n._(URL_DESCRIPTOR)}
								placeholder={EXAMPLE_URL}
								error={form.formState.errors.url?.message}
							/>
						)}
					</div>
				)}
				{showMessagingSection && (
					<div className={styles.settingsGroup} data-flx="channel.channel-tabs.channel-overview-tab.settings-group--2">
						<ChannelOverviewTopicEditor
							ref={topicEditorRef}
							channel={channel}
							guildId={guildId}
							form={form}
							initialTopic={remoteValues != null && remoteValues.topic != null ? remoteValues.topic : ''}
							onTopicExceedsLimit={handleTopicExceedsLimit}
							data-flx="channel.channel-tabs.channel-overview-tab.channel-overview-topic-editor"
						/>
						<SlowmodeControl form={form} data-flx="channel.channel-tabs.channel-overview-tab.slowmode-control" />
						{isAnnouncementConvertible && (
							<Controller
								name="announcement"
								control={form.control}
								render={({field}) => (
									<Switch
										label={i18n._(ANNOUNCEMENT_CHANNEL_DESCRIPTOR)}
										description={i18n._(ANNOUNCEMENT_CHANNEL_SWITCH_DESCRIPTION_DESCRIPTOR)}
										value={field.value ?? false}
										onChange={field.onChange}
										data-flx="channel.channel-tabs.channel-overview-tab.announcement-switch.change"
									/>
								)}
								data-flx="channel.channel-tabs.channel-overview-tab.announcement-controller"
							/>
						)}
					</div>
				)}
				{threadDefaultsActive && (
					<div
						className={styles.settingsGroup}
						data-flx="channel.channel-tabs.channel-overview-tab.settings-group--threads"
					>
						<ThreadDefaultsSection
							form={form}
							data-flx="channel.channel-tabs.channel-overview-tab.thread-defaults-section"
						/>
					</div>
				)}
				{showVoiceSection && (
					<div className={styles.settingsGroup} data-flx="channel.channel-tabs.channel-overview-tab.settings-group--3">
						{canManageChannel && (
							<VoiceSettings
								form={form}
								maxBitrateKbps={maxBitrateKbps}
								data-flx="channel.channel-tabs.channel-overview-tab.voice-settings"
							/>
						)}
						{canManageChannel && VoiceP2pRollout.enabled && (
							<Controller
								name="rtc_p2p"
								control={form.control}
								render={({field}) => (
									<Switch
										label={i18n._(CHANNEL_RTC_P2P_SETTING_DESCRIPTOR)}
										description={i18n._(CHANNEL_RTC_P2P_SETTING_DESCRIPTION_DESCRIPTOR, {
											maxParticipants: VoiceP2pRollout.maxParticipants,
										})}
										value={field.value}
										onChange={field.onChange}
										data-flx="channel.channel-tabs.channel-overview-tab.rtc-p2p-switch.change"
									/>
								)}
								data-flx="channel.channel-tabs.channel-overview-tab.rtc-p2p-controller"
							/>
						)}
						{canUpdateRtcRegion && (
							<RtcRegionSelect
								form={form}
								rtcRegions={rtcRegions}
								isLoadingRegions={isLoadingRegions}
								data-flx="channel.channel-tabs.channel-overview-tab.rtc-region-select"
							/>
						)}
					</div>
				)}
				{showSafetySection && (
					<div className={styles.settingsGroup} data-flx="channel.channel-tabs.channel-overview-tab.settings-group--4">
						<MatureContentSection
							form={form}
							channel={channel}
							guild={guild}
							data-flx="channel.channel-tabs.channel-overview-tab.mature-content-section"
						/>
					</div>
				)}
				{showAdvancedSection && (
					<SettingsSection
						id="channel-overview-advanced"
						title={<Trans>Advanced</Trans>}
						isAdvanced
						linkable={false}
						defaultExpanded={false}
						data-flx="channel.channel-tabs.channel-overview-tab.channel-overview-advanced"
					>
						<VoiceConnectionLimitControl
							form={form}
							data-flx="channel.channel-tabs.channel-overview-tab.voice-connection-limit-control"
						/>
					</SettingsSection>
				)}
			</Form>
		</div>
	);
});

export default ChannelOverviewTab;
