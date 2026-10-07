// SPDX-License-Identifier: AGPL-3.0-or-later

import {EXAMPLE_GENERAL_CHANNEL_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import ChannelOverviewTab from '@app/features/channel/components/modals/channel_tabs/ChannelOverviewTab';
import overviewStyles from '@app/features/channel/components/modals/channel_tabs/ChannelOverviewTab.module.css';
import {MatureContentSection} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/MatureContentSection';
import {SettingsControlRow} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/SettingsControlRow';
import {SlowmodeControl} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/SlowmodeControl';
import {
	CHANNEL_OVERVIEW_TAB_ID,
	type FormInputs,
} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/shared';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import {EmojiPickerPopout} from '@app/features/emoji/components/popouts/EmojiPickerPopout';
import * as ForumCommands from '@app/features/forum/commands/ForumCommands';
import styles from '@app/features/forum/components/Forum.module.css';
import {ForumEmoji, forumEmojiFromPicker} from '@app/features/forum/components/ForumTagPill';
import {ForumTagSettings} from '@app/features/forum/components/settings/ForumTagSettings';
import {
	type ForumEmojiRef,
	getDefaultLayout,
	getDefaultReaction,
	getDefaultSortOrder,
	hidesMediaDownloads,
	isMediaChannel,
	isTagRequired,
} from '@app/features/forum/utils/ForumChannelUtils';
import * as D from '@app/features/forum/utils/ForumMessageDescriptors';
import Guilds from '@app/features/guild/state/Guilds';
import Permission from '@app/features/permissions/state/Permission';
import {ThreadDefaultsSection} from '@app/features/threads/components/ThreadDefaultsSection';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import {Button} from '@app/features/ui/button/Button';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import * as UnsavedChangesCommands from '@app/features/ui/commands/UnsavedChangesCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input, Textarea} from '@app/features/ui/components/form/FormInput';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import {RadioGroup} from '@app/features/ui/radio_group/RadioGroup';
import {useRemoteFormReset} from '@app/lib/forms/RemoteFormReset';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {ContentWarningLevel} from '@fluxer/constants/src/GuildConstants';
import {
	ChannelFlags,
	DEFAULT_THREAD_AUTO_ARCHIVE_DURATION,
	FORUM_TOPIC_MAX_LENGTH,
	type ForumLayoutType,
	ForumLayoutTypes,
	type ForumSortOrderType,
	ForumSortOrderTypes,
} from '@fluxer/constants/src/ThreadConstants';
import {useLingui} from '@lingui/react/macro';
import {SmileyIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect} from 'react';
import {Controller, type UseFormReturn, useForm} from 'react-hook-form';

interface ForumFormInputs extends FormInputs {
	default_reaction: ForumEmojiRef | null;
	default_sort_order: ForumSortOrderType;
	default_forum_layout: ForumLayoutType;
	require_tag: boolean;
	hide_media_download: boolean;
}

function remoteValuesFor(forum: Channel): ForumFormInputs {
	return {
		name: forum.name ?? '',
		topic: forum.topic ?? '',
		url: '',
		slowmode: forum.rateLimitPerUser,
		nsfw_override: forum.nsfwOverride,
		content_warning_level: forum.contentWarningLevel ?? ContentWarningLevel.INHERIT,
		content_warning_text: forum.contentWarningText ?? '',
		rtc_region: null,
		default_auto_archive_duration: forum.defaultAutoArchiveDuration ?? DEFAULT_THREAD_AUTO_ARCHIVE_DURATION,
		default_thread_rate_limit_per_user: forum.defaultThreadRateLimitPerUser,
		default_reaction: getDefaultReaction(forum),
		default_sort_order: getDefaultSortOrder(forum),
		default_forum_layout: getDefaultLayout(forum),
		require_tag: isTagRequired(forum),
		hide_media_download: hidesMediaDownloads(forum),
	};
}

const DefaultReactionField = observer(({form, forum}: {form: UseFormReturn<ForumFormInputs>; forum: Channel}) => {
	const {i18n} = useLingui();
	return (
		<SettingsControlRow
			label={i18n._(D.DEFAULT_REACTION_DESCRIPTOR)}
			description={i18n._(D.DEFAULT_REACTION_HINT_DESCRIPTOR)}
			stacked
			dataFlx="forum.settings.forum-overview-tab.default-reaction-field"
			data-flx="forum.settings.forum-overview-tab.default-reaction-field.settings-control-row"
		>
			<Controller
				name="default_reaction"
				control={form.control}
				render={({field}) => (
					<div
						className={styles.settingsRow}
						data-flx="forum.settings.forum-overview-tab.default-reaction-field.settings-row"
					>
						<Popout
							position="right-start"
							render={({onClose}) => (
								<EmojiPickerPopout
									channelId={forum.id}
									handleSelect={(emoji) => field.onChange(forumEmojiFromPicker(emoji))}
									onClose={onClose}
									data-flx="forum.settings.forum-overview-tab.default-reaction-field.emoji-picker-popout"
								/>
							)}
							data-flx="forum.settings.forum-overview-tab.default-reaction-field.popout"
						>
							<FocusRing offset={-2} data-flx="forum.settings.forum-overview-tab.default-reaction-field.focus-ring">
								<button
									type="button"
									className={styles.emojiPickerButton}
									aria-label={i18n._(D.PICK_EMOJI_DESCRIPTOR)}
									data-flx="forum.settings.forum-overview-tab.default-reaction-field.emoji-picker-button"
								>
									{field.value ? (
										<ForumEmoji
											emoji={field.value}
											data-flx="forum.settings.forum-overview-tab.default-reaction-field.forum-emoji"
										/>
									) : (
										<SmileyIcon
											size={20}
											data-flx="forum.settings.forum-overview-tab.default-reaction-field.smiley-icon"
										/>
									)}
								</button>
							</FocusRing>
						</Popout>
						{field.value && (
							<Button
								small
								variant="secondary"
								onClick={() => field.onChange(null)}
								data-flx="forum.settings.forum-overview-tab.default-reaction-field.button.change"
							>
								{i18n._(D.REMOVE_DESCRIPTOR)}
							</Button>
						)}
					</div>
				)}
				data-flx="forum.settings.forum-overview-tab.default-reaction-field.controller"
			/>
		</SettingsControlRow>
	);
});

const ForumOverviewForm = observer(({forum}: {forum: Channel}) => {
	const {i18n} = useLingui();
	const guild = forum.guildId ? Guilds.getGuild(forum.guildId) : null;
	const media = isMediaChannel(forum);
	const form = useForm<ForumFormInputs>({defaultValues: remoteValuesFor(forum)});
	const sharedForm = form as unknown as UseFormReturn<FormInputs>;
	const {resetToRemoteValues, commitRemoteValues} = useRemoteFormReset<ForumFormInputs>({
		form,
		identityKey: forum.id,
		remoteValues: remoteValuesFor(forum),
	});
	const onSubmit = useCallback(
		async (data: ForumFormInputs) => {
			const dirty = form.formState.dirtyFields;
			const patch: ForumCommands.ForumChannelPatch & Record<string, unknown> = {};
			if (dirty.name) patch.name = data.name;
			if (dirty.topic) patch.topic = data.topic?.trim() ? data.topic : null;
			if (dirty.slowmode) patch.rate_limit_per_user = data.slowmode;
			if (dirty.default_auto_archive_duration) patch.default_auto_archive_duration = data.default_auto_archive_duration;
			if (dirty.default_thread_rate_limit_per_user) {
				patch.default_thread_rate_limit_per_user = data.default_thread_rate_limit_per_user;
			}
			if (dirty.default_reaction) patch.default_reaction_emoji = data.default_reaction;
			if (dirty.default_sort_order) patch.default_sort_order = data.default_sort_order;
			if (dirty.default_forum_layout && !media) patch.default_forum_layout = data.default_forum_layout;
			if (dirty.require_tag || dirty.hide_media_download) {
				let flags = forum.flags & ~(ChannelFlags.REQUIRE_TAG | ChannelFlags.HIDE_MEDIA_DOWNLOAD_OPTIONS);
				if (data.require_tag) flags |= ChannelFlags.REQUIRE_TAG;
				if (media && data.hide_media_download) flags |= ChannelFlags.HIDE_MEDIA_DOWNLOAD_OPTIONS;
				patch.flags = flags;
			}
			if (dirty.nsfw_override) patch.nsfw_override = data.nsfw_override;
			if (dirty.content_warning_level) patch.content_warning_level = data.content_warning_level;
			if (dirty.content_warning_text) {
				const trimmed = (data.content_warning_text ?? '').trim();
				patch.content_warning_text = trimmed.length > 0 ? trimmed : null;
			}
			if (Object.keys(patch).length > 0) {
				await ForumCommands.updateForumChannel(forum, patch);
			}
			commitRemoteValues(data);
			ToastCommands.createToast({type: 'success', children: i18n._(D.FORUM_SETTINGS_SAVED_DESCRIPTOR)});
		},
		[commitRemoteValues, form, forum, i18n, media],
	);
	const {handleSubmit: handleSave} = useFormSubmit({form, onSubmit, defaultErrorField: 'name'});
	const isDirty = form.formState.isDirty;
	useEffect(() => {
		UnsavedChangesCommands.setUnsavedChanges(CHANNEL_OVERVIEW_TAB_ID, isDirty);
	}, [isDirty]);
	useEffect(() => {
		UnsavedChangesCommands.setTabData(CHANNEL_OVERVIEW_TAB_ID, {
			onReset: resetToRemoteValues,
			onSave: handleSave,
			isSubmitting: form.formState.isSubmitting,
		});
	}, [resetToRemoteValues, handleSave, form.formState.isSubmitting]);
	useEffect(() => () => UnsavedChangesCommands.clearUnsavedChanges(CHANNEL_OVERVIEW_TAB_ID), []);
	return (
		<div className={overviewStyles.sectionWrapper} data-flx="forum.settings.forum-overview-tab.forum-overview-form.div">
			<Form
				form={form}
				onSubmit={handleSave}
				data-flx="forum.settings.forum-overview-tab.forum-overview-form.form.save"
			>
				<div
					className={overviewStyles.settingsGroup}
					data-flx="forum.settings.forum-overview-tab.forum-overview-form.div--2"
				>
					<Input
						data-flx="forum.settings.forum-overview-tab.forum-overview-form.input.text"
						{...form.register('name')}
						type="text"
						label={i18n._(D.CHANNEL_NAME_DESCRIPTOR)}
						placeholder={EXAMPLE_GENERAL_CHANNEL_NAME}
						minLength={1}
						maxLength={100}
						error={form.formState.errors.name?.message}
					/>
					<Controller
						name="topic"
						control={form.control}
						render={({field}) => (
							<Textarea
								data-flx="forum.settings.forum-overview-tab.forum-overview-form.textarea"
								name={field.name}
								value={field.value ?? ''}
								onChange={field.onChange}
								onBlur={field.onBlur}
								label={i18n._(D.GUIDELINES_DESCRIPTOR)}
								placeholder={i18n._(D.GUIDELINES_PLACEHOLDER_DESCRIPTOR)}
								maxLength={FORUM_TOPIC_MAX_LENGTH}
								showCharacterCount
								minRows={3}
								error={form.formState.errors.topic?.message}
							/>
						)}
						data-flx="forum.settings.forum-overview-tab.forum-overview-form.controller.topic"
					/>
				</div>
				<div
					className={overviewStyles.settingsGroup}
					data-flx="forum.settings.forum-overview-tab.forum-overview-form.div--3"
				>
					<ForumTagSettings
						forum={forum}
						data-flx="forum.settings.forum-overview-tab.forum-overview-form.forum-tag-settings"
					/>
					<Controller
						name="require_tag"
						control={form.control}
						render={({field}) => (
							<Switch
								label={i18n._(D.REQUIRE_TAG_DESCRIPTOR)}
								value={field.value}
								onChange={field.onChange}
								data-flx="forum.settings.forum-overview-tab.forum-overview-form.switch.change"
							/>
						)}
						data-flx="forum.settings.forum-overview-tab.forum-overview-form.controller"
					/>
				</div>
				<div
					className={overviewStyles.settingsGroup}
					data-flx="forum.settings.forum-overview-tab.forum-overview-form.div--4"
				>
					<DefaultReactionField
						form={form}
						forum={forum}
						data-flx="forum.settings.forum-overview-tab.forum-overview-form.default-reaction-field"
					/>
					<SettingsControlRow
						label={i18n._(D.DEFAULT_SORT_ORDER_DESCRIPTOR)}
						stacked
						dataFlx="forum.settings.forum-overview-tab.forum-overview-form.settings-section"
						data-flx="forum.settings.forum-overview-tab.forum-overview-form.settings-section.settings-control-row"
					>
						<Controller
							name="default_sort_order"
							control={form.control}
							render={({field}) => (
								<RadioGroup
									aria-label={i18n._(D.DEFAULT_SORT_ORDER_DESCRIPTOR)}
									value={field.value}
									onChange={field.onChange}
									options={[
										{value: ForumSortOrderTypes.LATEST_ACTIVITY, name: i18n._(D.SORT_RECENT_ACTIVITY_DESCRIPTOR)},
										{value: ForumSortOrderTypes.CREATION_TIME, name: i18n._(D.SORT_CREATION_DATE_DESCRIPTOR)},
									]}
									data-flx="forum.settings.forum-overview-tab.forum-overview-form.radio-group.change"
								/>
							)}
							data-flx="forum.settings.forum-overview-tab.forum-overview-form.controller--2"
						/>
					</SettingsControlRow>
					{!media && (
						<SettingsControlRow
							label={i18n._(D.DEFAULT_LAYOUT_DESCRIPTOR)}
							stacked
							dataFlx="forum.settings.forum-overview-tab.forum-overview-form.settings-section--2"
							data-flx="forum.settings.forum-overview-tab.forum-overview-form.settings-section--2.settings-control-row"
						>
							<Controller
								name="default_forum_layout"
								control={form.control}
								render={({field}) => (
									<RadioGroup
										aria-label={i18n._(D.DEFAULT_LAYOUT_DESCRIPTOR)}
										value={field.value}
										onChange={field.onChange}
										options={[
											{value: ForumLayoutTypes.LIST, name: i18n._(D.LAYOUT_LIST_DESCRIPTOR)},
											{value: ForumLayoutTypes.GRID, name: i18n._(D.LAYOUT_GALLERY_DESCRIPTOR)},
										]}
										data-flx="forum.settings.forum-overview-tab.forum-overview-form.radio-group.change--2"
									/>
								)}
								data-flx="forum.settings.forum-overview-tab.forum-overview-form.controller--3"
							/>
						</SettingsControlRow>
					)}
					{media && (
						<Controller
							name="hide_media_download"
							control={form.control}
							render={({field}) => (
								<Switch
									label={i18n._(D.HIDE_MEDIA_DOWNLOAD_DESCRIPTOR)}
									description={i18n._(D.HIDE_MEDIA_DOWNLOAD_HINT_DESCRIPTOR)}
									value={field.value}
									onChange={field.onChange}
									data-flx="forum.settings.forum-overview-tab.forum-overview-form.switch.change--2"
								/>
							)}
							data-flx="forum.settings.forum-overview-tab.forum-overview-form.controller--4"
						/>
					)}
				</div>
				<div
					className={overviewStyles.settingsGroup}
					data-flx="forum.settings.forum-overview-tab.forum-overview-form.div--5"
				>
					<SlowmodeControl
						form={sharedForm}
						label={i18n._(D.POST_SLOWMODE_DESCRIPTOR)}
						data-flx="forum.settings.forum-overview-tab.forum-overview-form.slowmode-control"
					/>
					<ThreadDefaultsSection
						form={sharedForm}
						data-flx="forum.settings.forum-overview-tab.forum-overview-form.thread-defaults-section"
					/>
				</div>
				<div
					className={overviewStyles.settingsGroup}
					data-flx="forum.settings.forum-overview-tab.forum-overview-form.div--6"
				>
					<MatureContentSection
						form={sharedForm}
						channel={forum}
						guild={guild}
						data-flx="forum.settings.forum-overview-tab.forum-overview-form.mature-content-section"
					/>
				</div>
			</Form>
		</div>
	);
});

export const ChannelOverviewTabForType = observer(({channelId}: {channelId: string}) => {
	const channel = Channels.getChannel(channelId);
	if (
		channel?.isThreadOnly() &&
		ThreadGuilds.isActive(channel.guildId) &&
		Permission.can(Permissions.MANAGE_CHANNELS, channel)
	) {
		return (
			<ForumOverviewForm
				key={channel.id}
				forum={channel}
				data-flx="forum.settings.forum-overview-tab.channel-overview-tab-for-type.forum-overview-form"
			/>
		);
	}
	return (
		<ChannelOverviewTab
			channelId={channelId}
			data-flx="forum.settings.forum-overview-tab.channel-overview-tab-for-type.channel-overview-tab"
		/>
	);
});
