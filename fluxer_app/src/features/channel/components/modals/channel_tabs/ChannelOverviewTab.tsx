// SPDX-License-Identifier: AGPL-3.0-or-later

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
	CHANNEL_OVERVIEW_TAB_ID,
	type FormInputs,
} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/shared';
import {
	VoiceConnectionLimitControl,
	VoiceSettings,
} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/VoiceSettings';
import Channels from '@app/features/channel/state/Channels';
import Guilds from '@app/features/guild/state/Guilds';
import Permission from '@app/features/permissions/state/Permission';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import * as UnsavedChangesCommands from '@app/features/ui/commands/UnsavedChangesCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {useRemoteFormReset} from '@app/lib/forms/RemoteFormReset';
import {ChannelTypes, GUILD_TEXT_BASED_CHANNEL_TYPES, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {ContentWarningLevel} from '@fluxer/constants/src/GuildConstants';
import {VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT} from '@fluxer/constants/src/LimitConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';
import {useForm} from 'react-hook-form';

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
const ChannelOverviewTab: React.FC<{channelId: string}> = observer(({channelId}) => {
	const {i18n} = useLingui();
	const channel = Channels.getChannel(channelId);
	const guildId = channel?.guildId ?? null;
	const guild = guildId ? Guilds.getGuild(guildId) : null;
	const canUpdateRtcRegion =
		guildId !== null ? Permission.can(Permissions.UPDATE_RTC_REGION, {guildId, channelId}) : false;
	const canManageChannel = guildId !== null ? Permission.can(Permissions.MANAGE_CHANNELS, {guildId, channelId}) : false;
	const isVoiceChannel = channel?.type === ChannelTypes.GUILD_VOICE;
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
			bitrate: 64,
			user_limit: 0,
			voice_connection_limit: VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT,
			rtc_region: null,
		},
	});
	const remoteValues: FormInputs | null = channel
		? {
				name: channel.name || '',
				topic: channel.topic || '',
				url: channel.url || '',
				slowmode: channel.rateLimitPerUser || 0,
				nsfw_override: channel.nsfwOverride,
				content_warning_level: channel.contentWarningLevel ?? ContentWarningLevel.INHERIT,
				content_warning_text: channel.contentWarningText ?? '',
				bitrate: channel.bitrate ? Math.round(channel.bitrate / 1000) : 64,
				user_limit: channel.userLimit ?? 0,
				voice_connection_limit: channel.voiceConnectionLimit ?? VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT,
				rtc_region: channel.rtcRegion ?? null,
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
					updateData.bitrate = (data.bitrate ?? 64) * 1000;
					updateData.user_limit = data.user_limit;
					updateData.voice_connection_limit = data.voice_connection_limit ?? VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT;
				} else if (channel.type === ChannelTypes.GUILD_LINK) {
					updateData.url = data.url;
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
			if (channel.type === ChannelTypes.GUILD_VOICE && (canManageChannel || canUpdateRtcRegion) && dirty.rtc_region) {
				updateData.rtc_region = data.rtc_region ?? null;
			}
			if (Object.keys(updateData).length === 0) {
				ToastCommands.createToast({type: 'success', children: <Trans>Channel updated</Trans>});
				return;
			}
			await ChannelCommands.update(channel.id, updateData);
			const currentValues = form.getValues();
			commitRemoteValues({
				name: data.name,
				topic: data.topic ?? '',
				url: data.url ?? '',
				slowmode: data.slowmode ?? currentValues.slowmode ?? 0,
				nsfw_override: data.nsfw_override,
				content_warning_level: data.content_warning_level,
				content_warning_text: data.content_warning_text ?? '',
				bitrate: data.bitrate ?? currentValues.bitrate ?? 64,
				user_limit: data.user_limit ?? currentValues.user_limit ?? 0,
				voice_connection_limit:
					data.voice_connection_limit ?? currentValues.voice_connection_limit ?? VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT,
				rtc_region: data.rtc_region ?? currentValues.rtc_region ?? null,
			});
			ToastCommands.createToast({type: 'success', children: <Trans>Channel updated</Trans>});
		},
		[canManageChannel, canUpdateRtcRegion, channel, form, commitRemoteValues],
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
	const isTextChannel = channel.type === ChannelTypes.GUILD_TEXT;
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
						/>
						<SlowmodeControl form={form} />
					</div>
				)}
				{showVoiceSection && (
					<div className={styles.settingsGroup} data-flx="channel.channel-tabs.channel-overview-tab.settings-group--3">
						{canManageChannel && (
							<VoiceSettings form={form} data-flx="channel.channel-tabs.channel-overview-tab.voice-settings" />
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
