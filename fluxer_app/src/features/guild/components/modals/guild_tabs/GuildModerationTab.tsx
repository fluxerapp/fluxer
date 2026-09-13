// SPDX-License-Identifier: AGPL-3.0-or-later

import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import styles from '@app/features/guild/components/modals/guild_tabs/GuildModerationTab.module.css';
import Guilds from '@app/features/guild/state/Guilds';
import Permission from '@app/features/permissions/state/Permission';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import * as UnsavedChangesCommands from '@app/features/ui/commands/UnsavedChangesCommands';
import {Form} from '@app/features/ui/components/form/Form';
import type {ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {Textarea} from '@app/features/ui/components/form/FormInput';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import type {RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import {RadioGroup} from '@app/features/ui/radio_group/RadioGroup';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {CompactComboboxRow} from '@app/features/user/components/modals/tabs/components/CompactComboboxRow';
import Users from '@app/features/user/state/Users';
import {useRemoteFormReset} from '@app/lib/forms/RemoteFormReset';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {
	CONTENT_WARNING_TEXT_MAX_LENGTH,
	ContentWarningLevel,
	GuildExplicitContentFilterTypes,
	GuildFeatures,
	GuildMFALevel,
	GuildVerificationLevel,
	getEffectiveGuildVerificationLevel,
} from '@fluxer/constants/src/GuildConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect} from 'react';
import {Controller, useForm} from 'react-hook-form';

const ONLY_OWNER_CAN_CHANGE_TOOLTIP_DESCRIPTOR = msg({
	message: 'Only the community owner can change this setting',
	comment:
		'Tooltip on the 2FA-for-moderation switch in the community moderation settings tab when the viewer is not the owner. Plain explanatory tone.',
});
const ENABLE_2FA_TO_CHANGE_TOOLTIP_DESCRIPTOR = msg({
	message: 'Enable 2FA on your account to change this setting',
	comment:
		'Tooltip on the 2FA-for-moderation switch in the community moderation settings tab when the owner has not enabled 2FA on their own account.',
});
const VERIFICATION_LEVEL_NONE_NAME_DESCRIPTOR = msg({
	message: 'None',
	comment:
		'Member verification level option in the community moderation settings tab. Short standalone label for the lowest level (no verification).',
});
const VERIFICATION_LEVEL_NONE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'No verification is required.',
	comment: 'Helper text for the "None" member verification level option in the community moderation settings tab.',
});
const VERIFICATION_LEVEL_LOW_NAME_DESCRIPTOR = msg({
	message: 'Low',
	comment:
		'Member verification level option in the community moderation settings tab. Short standalone severity label.',
});
const VERIFICATION_LEVEL_LOW_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Requires a verified email address.',
	comment: 'Helper text for the "Low" member verification level option in the community moderation settings tab.',
});
const VERIFICATION_LEVEL_MEDIUM_NAME_DESCRIPTOR = msg({
	message: 'Medium',
	comment:
		'Member verification level option in the community moderation settings tab. Short standalone severity label.',
});
const VERIFICATION_LEVEL_MEDIUM_DESCRIPTION_DESCRIPTOR = msg({
	message: "Requires a verified email address and an account that's at least 5 minutes old.",
	comment: 'Helper text for the "Medium" member verification level option in the community moderation settings tab.',
});
const VERIFICATION_LEVEL_HIGH_NAME_DESCRIPTOR = msg({
	message: 'High',
	comment:
		'Member verification level option in the community moderation settings tab. Short standalone severity label.',
});
const VERIFICATION_LEVEL_HIGH_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Requires everything in medium, plus being a member of the community for at least 10 minutes.',
	comment:
		'Helper text for the "High" member verification level option in the community moderation settings tab. "Medium" refers to the matching level option.',
});
const VERIFICATION_LEVEL_VERY_HIGH_NAME_DESCRIPTOR = msg({
	message: 'Very high',
	comment:
		'Member verification level option in the community moderation settings tab. Short standalone severity label.',
});
const VERIFICATION_LEVEL_VERY_HIGH_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Requires a verified phone number.',
	comment: 'Helper text for the "Very high" member verification level option in the community moderation settings tab.',
});
const MEMBER_VERIFICATION_LEVEL_ARIA_DESCRIPTOR = msg({
	message: 'Member verification level',
	comment: 'Accessible label for the member verification level radio group in the community moderation settings tab.',
});
const EXPLICIT_CONTENT_FILTER_OFF_NAME_DESCRIPTOR = msg({
	message: 'Off',
	comment:
		'Explicit content filter option in the community moderation settings tab. Short standalone label meaning disabled.',
});
const EXPLICIT_CONTENT_FILTER_OFF_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Let the community self-moderate',
	comment:
		'Helper text for the "Off" explicit content filter option in the community moderation settings tab. Means leave moderation to members.',
});
const EXPLICIT_CONTENT_FILTER_MEMBERS_WITHOUT_ROLES_NAME_DESCRIPTOR = msg({
	message: 'Filter members without roles',
	comment:
		'Explicit content filter option in the community moderation settings tab. Filters messages from members who have no roles assigned.',
});
const EXPLICIT_CONTENT_FILTER_MEMBERS_WITHOUT_ROLES_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Suggested for most communities',
	comment:
		'Helper text under the "Filter members without roles" explicit content filter option in the community moderation settings tab.',
});
const EXPLICIT_CONTENT_FILTER_EVERYONE_NAME_DESCRIPTOR = msg({
	message: 'Filter everyone',
	comment:
		'Explicit content filter option in the community moderation settings tab. Filters messages from every member regardless of role.',
});
const EXPLICIT_CONTENT_FILTER_EVERYONE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Maximum protection for family-friendly spaces',
	comment:
		'Helper text under the "Filter everyone" explicit content filter option in the community moderation settings tab.',
});
const EXPLICIT_CONTENT_FILTER_ARIA_DESCRIPTOR = msg({
	message: 'Explicit content filter setting',
	comment: 'Accessible label for the explicit content filter radio group in the community moderation settings tab.',
});
const MATURE_CONTENT_DESCRIPTOR = msg({
	message: 'Mature content',
	comment: 'Select label in the community moderation settings tab. Sensitive setting; keep neutral tone.',
});
const MATURE_CONTENT_ON_DESCRIPTOR = msg({
	message: 'On',
	comment: 'Mature content option in the community moderation settings tab.',
});
const MATURE_CONTENT_OFF_DESCRIPTOR = msg({
	message: 'Off',
	comment: 'Mature content option in the community moderation settings tab.',
});
const SHOW_CONTENT_WARNING_SWITCH_LABEL_DESCRIPTOR = msg({
	message: 'Show a content warning',
	comment:
		'Switch label in the community moderation settings tab. Toggles a consent prompt before entering any channel.',
});
const CUSTOM_CONTENT_WARNING_TEXT_FIELD_LABEL_DESCRIPTOR = msg({
	message: 'Custom warning text',
	comment: 'Textarea label in the community moderation settings tab for the custom content warning shown to members.',
});
const CUSTOM_CONTENT_WARNING_TEXT_PLACEHOLDER_DESCRIPTOR = msg({
	message: 'This contains sensitive content.',
	comment: 'Placeholder example shown in the custom content warning textarea in the community moderation settings tab.',
});
const REQUIRE_2FA_FOR_MODERATION_SWITCH_LABEL_DESCRIPTOR = msg({
	message: 'Require 2FA for moderation actions',
	comment:
		'Switch label in the community moderation settings tab. Requires moderators to have two-factor auth enabled before they can ban, kick, time out, or delete messages. Security-sensitive; keep tone plain.',
});

interface FormInputs {
	verification_level: number;
	mfa_level: number;
	nsfw: boolean;
	content_warning_level: number;
	content_warning_text: string;
	explicit_content_filter: number;
}

const GUILD_MODERATION_TAB_ID = 'moderation';
const GuildModerationTab: React.FC<{guildId: string}> = observer(({guildId}) => {
	const {i18n} = useLingui();
	const guild = Guilds.getGuild(guildId);
	const currentUser = Users.currentUser;
	const canManageGuild = Permission.can(Permissions.MANAGE_GUILD, {guildId});
	const isGuildOwner = guild?.ownerId === currentUser?.id;
	const isDiscoverable = guild?.features.has(GuildFeatures.DISCOVERABLE) ?? false;
	const remoteValues: FormInputs = {
		verification_level: guild?.verificationLevel ?? GuildVerificationLevel.NONE,
		mfa_level: guild?.mfaLevel ?? GuildMFALevel.NONE,
		nsfw: guild?.nsfw ?? false,
		content_warning_level: guild?.contentWarningLevel ?? ContentWarningLevel.INHERIT,
		content_warning_text: guild?.contentWarningText ?? '',
		explicit_content_filter: guild?.explicitContentFilter ?? GuildExplicitContentFilterTypes.DISABLED,
	};
	const form = useForm<FormInputs>({defaultValues: remoteValues});
	const {resetToRemoteValues, commitRemoteValues} = useRemoteFormReset<FormInputs>({
		form,
		identityKey: guildId,
		remoteValues: guild ? remoteValues : null,
	});
	const onSubmit = useCallback(
		async (data: FormInputs) => {
			if (!guild) return;
			const dirtyFields = form.formState.dirtyFields;
			const updateData: GuildCommands.GuildUpdatePayload = {};
			if (dirtyFields.verification_level) updateData.verification_level = data.verification_level;
			if (dirtyFields.nsfw) updateData.nsfw = data.nsfw;
			if (dirtyFields.content_warning_level) updateData.content_warning_level = data.content_warning_level;
			if (dirtyFields.content_warning_text) {
				const trimmed = data.content_warning_text.trim();
				updateData.content_warning_text = trimmed.length > 0 ? trimmed : null;
			}
			if (dirtyFields.explicit_content_filter) updateData.explicit_content_filter = data.explicit_content_filter;
			if (isGuildOwner && dirtyFields.mfa_level && data.mfa_level !== guild.mfaLevel) {
				updateData.mfa_level = data.mfa_level;
			}
			if (Object.keys(updateData).length > 0) {
				await GuildCommands.update(guild.id, updateData);
			}
			commitRemoteValues(data);
			ToastCommands.createToast({type: 'success', children: <Trans>Community updated</Trans>});
		},
		[guild, form, isGuildOwner, commitRemoteValues],
	);
	const {handleSubmit: handleSave} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'verification_level',
	});
	const handleReset = useCallback(() => {
		resetToRemoteValues();
	}, [resetToRemoteValues]);
	const isFormDirty = form.formState.isDirty;
	useEffect(() => {
		UnsavedChangesCommands.setUnsavedChanges(GUILD_MODERATION_TAB_ID, isFormDirty);
	}, [isFormDirty]);
	useEffect(() => {
		UnsavedChangesCommands.setTabData(GUILD_MODERATION_TAB_ID, {
			onReset: handleReset,
			onSave: handleSave,
			isSubmitting: form.formState.isSubmitting,
		});
	}, [handleReset, handleSave, form.formState.isSubmitting]);
	useEffect(() => {
		return () => {
			UnsavedChangesCommands.clearUnsavedChanges(GUILD_MODERATION_TAB_ID);
		};
	}, []);
	if (!guild) return null;
	const currentUserHas2FA = currentUser?.mfaEnabled ?? false;
	const colorizeLabel = (label: string, color?: string) =>
		color ? (
			<span style={{color}} data-flx="guild.guild-tabs.guild-moderation-tab.colorize-label.span">
				{label}
			</span>
		) : (
			label
		);
	const isMfaDisabled = !isGuildOwner || !currentUserHas2FA;
	const getMfaTooltipText = (): string | undefined => {
		if (!isGuildOwner) {
			return i18n._(ONLY_OWNER_CAN_CHANGE_TOOLTIP_DESCRIPTOR);
		}
		if (!currentUserHas2FA) {
			return i18n._(ENABLE_2FA_TO_CHANGE_TOOLTIP_DESCRIPTOR);
		}
		return;
	};
	const verificationLevelOptions: ReadonlyArray<RadioOption<number>> = [
		{
			value: GuildVerificationLevel.NONE,
			name: i18n._(VERIFICATION_LEVEL_NONE_NAME_DESCRIPTOR),
			desc: i18n._(VERIFICATION_LEVEL_NONE_DESCRIPTION_DESCRIPTOR),
			disabled: isDiscoverable,
		},
		{
			value: GuildVerificationLevel.LOW,
			name: colorizeLabel(i18n._(VERIFICATION_LEVEL_LOW_NAME_DESCRIPTOR), '#22c55e'),
			desc: i18n._(VERIFICATION_LEVEL_LOW_DESCRIPTION_DESCRIPTOR),
		},
		{
			value: GuildVerificationLevel.MEDIUM,
			name: colorizeLabel(i18n._(VERIFICATION_LEVEL_MEDIUM_NAME_DESCRIPTOR), '#f59e0b'),
			desc: i18n._(VERIFICATION_LEVEL_MEDIUM_DESCRIPTION_DESCRIPTOR),
		},
		{
			value: GuildVerificationLevel.HIGH,
			name: colorizeLabel(i18n._(VERIFICATION_LEVEL_HIGH_NAME_DESCRIPTOR), '#f97316'),
			desc: i18n._(VERIFICATION_LEVEL_HIGH_DESCRIPTION_DESCRIPTOR),
		},
		{
			value: GuildVerificationLevel.VERY_HIGH,
			name: colorizeLabel(i18n._(VERIFICATION_LEVEL_VERY_HIGH_NAME_DESCRIPTOR), '#ef4444'),
			desc: i18n._(VERIFICATION_LEVEL_VERY_HIGH_DESCRIPTION_DESCRIPTOR),
		},
	];
	const matureContentOptions: ReadonlyArray<ComboboxOption<string>> = [
		{value: 'on', label: i18n._(MATURE_CONTENT_ON_DESCRIPTOR)},
		{value: 'off', label: i18n._(MATURE_CONTENT_OFF_DESCRIPTOR)},
	];
	return (
		<div className={styles.container} data-flx="guild.guild-tabs.guild-moderation-tab.container">
			<Form form={form} onSubmit={handleSave} data-flx="guild.guild-tabs.guild-moderation-tab.form.save">
				<div className={styles.container} data-flx="guild.guild-tabs.guild-moderation-tab.container--2">
					<div className={styles.section} data-flx="guild.guild-tabs.guild-moderation-tab.section">
						<h3 className={styles.sectionTitle} data-flx="guild.guild-tabs.guild-moderation-tab.section-title">
							<Trans>Member verification</Trans>
						</h3>
						<div
							className={styles.sectionDescriptionMultiline}
							data-flx="guild.guild-tabs.guild-moderation-tab.section-description-multiline"
						>
							<p data-flx="guild.guild-tabs.guild-moderation-tab.p">
								<Trans>Choose what members must have before they can post or DM community members.</Trans>
							</p>
							<p data-flx="guild.guild-tabs.guild-moderation-tab.p--2">
								<Trans>
									Members with roles can bypass these checks. For public spaces, we recommend enabling verification.
								</Trans>
							</p>
							{isDiscoverable && (
								<p data-flx="guild.guild-tabs.guild-moderation-tab.p--3">
									<Trans>
										Communities listed in Discovery require at least email verification. None cannot be selected while
										Discovery is enabled.
									</Trans>
								</p>
							)}
						</div>
						<Controller
							name="verification_level"
							control={form.control}
							render={({field}) => (
								<RadioGroup
									value={getEffectiveGuildVerificationLevel(field.value ?? GuildVerificationLevel.NONE, isDiscoverable)}
									onChange={field.onChange}
									disabled={!canManageGuild}
									options={verificationLevelOptions}
									aria-label={i18n._(MEMBER_VERIFICATION_LEVEL_ARIA_DESCRIPTOR)}
									data-flx="guild.guild-tabs.guild-moderation-tab.radio-group.change"
								/>
							)}
							data-flx="guild.guild-tabs.guild-moderation-tab.controller"
						/>
					</div>
					<div className={styles.section} data-flx="guild.guild-tabs.guild-moderation-tab.section--2">
						<h3 className={styles.sectionTitle} data-flx="guild.guild-tabs.guild-moderation-tab.section-title--2">
							<Trans>Content filtering</Trans>
						</h3>
						<p
							className={styles.sectionDescription}
							data-flx="guild.guild-tabs.guild-moderation-tab.section-description"
						>
							<Trans>
								Automatically screen messages for explicit content in channels not marked for mature content.
							</Trans>
						</p>
						{isDiscoverable && (
							<p
								className={styles.sectionDescription}
								style={{fontStyle: 'italic'}}
								data-flx="guild.guild-tabs.guild-moderation-tab.section-description--2"
							>
								<Trans>
									Communities listed in Discovery are required to scan messages from all members. This setting cannot be
									changed while Discovery is enabled.
								</Trans>
							</p>
						)}
						<Controller
							name="explicit_content_filter"
							control={form.control}
							render={({field}) => (
								<RadioGroup
									value={isDiscoverable ? GuildExplicitContentFilterTypes.ALL_MEMBERS : field.value}
									onChange={field.onChange}
									disabled={!canManageGuild || isDiscoverable}
									options={[
										{
											value: GuildExplicitContentFilterTypes.DISABLED,
											name: i18n._(EXPLICIT_CONTENT_FILTER_OFF_NAME_DESCRIPTOR),
											desc: i18n._(EXPLICIT_CONTENT_FILTER_OFF_DESCRIPTION_DESCRIPTOR),
										},
										{
											value: GuildExplicitContentFilterTypes.MEMBERS_WITHOUT_ROLES,
											name: colorizeLabel(
												i18n._(EXPLICIT_CONTENT_FILTER_MEMBERS_WITHOUT_ROLES_NAME_DESCRIPTOR),
												'#f59e0b',
											),
											desc: i18n._(EXPLICIT_CONTENT_FILTER_MEMBERS_WITHOUT_ROLES_DESCRIPTION_DESCRIPTOR),
										},
										{
											value: GuildExplicitContentFilterTypes.ALL_MEMBERS,
											name: colorizeLabel(i18n._(EXPLICIT_CONTENT_FILTER_EVERYONE_NAME_DESCRIPTOR), '#ef4444'),
											desc: i18n._(EXPLICIT_CONTENT_FILTER_EVERYONE_DESCRIPTION_DESCRIPTOR),
										},
									]}
									aria-label={i18n._(EXPLICIT_CONTENT_FILTER_ARIA_DESCRIPTOR)}
									data-flx="guild.guild-tabs.guild-moderation-tab.radio-group.change--2"
								/>
							)}
							data-flx="guild.guild-tabs.guild-moderation-tab.controller--2"
						/>
					</div>
					<div className={styles.section} data-flx="guild.guild-tabs.guild-moderation-tab.section--3">
						<h3 className={styles.sectionTitle} data-flx="guild.guild-tabs.guild-moderation-tab.section-title--3">
							<Trans>Mature content & content warnings</Trans>
						</h3>
						<Controller
							name="nsfw"
							control={form.control}
							render={({field}) => (
								<CompactComboboxRow<string>
									label={i18n._(MATURE_CONTENT_DESCRIPTOR)}
									value={field.value ? 'on' : 'off'}
									onChange={(value) => field.onChange(value === 'on')}
									options={matureContentOptions}
									isSearchable={false}
									controlWidth="small"
									disabled={!canManageGuild}
									dataFlx="guild.guild-tabs.guild-moderation-tab.mature-content-select"
									data-flx="guild.guild-tabs.guild-moderation-tab.compact-combobox-row.change"
								/>
							)}
							data-flx="guild.guild-tabs.guild-moderation-tab.controller--3"
						/>
						<Controller
							name="content_warning_level"
							control={form.control}
							render={({field}) => (
								<Switch
									label={i18n._(SHOW_CONTENT_WARNING_SWITCH_LABEL_DESCRIPTOR)}
									value={field.value === ContentWarningLevel.CONTENT_WARNING}
									onChange={(value) =>
										field.onChange(value ? ContentWarningLevel.CONTENT_WARNING : ContentWarningLevel.INHERIT)
									}
									disabled={!canManageGuild}
									data-flx="guild.guild-tabs.guild-moderation-tab.switch.change--2"
								/>
							)}
							data-flx="guild.guild-tabs.guild-moderation-tab.controller--4"
						/>
						{form.watch('content_warning_level') === ContentWarningLevel.CONTENT_WARNING && (
							<Controller
								name="content_warning_text"
								control={form.control}
								render={({field}) => (
									<Textarea
										label={i18n._(CUSTOM_CONTENT_WARNING_TEXT_FIELD_LABEL_DESCRIPTOR)}
										placeholder={i18n._(CUSTOM_CONTENT_WARNING_TEXT_PLACEHOLDER_DESCRIPTOR)}
										value={field.value ?? ''}
										onChange={(e) => field.onChange(e.target.value)}
										maxLength={CONTENT_WARNING_TEXT_MAX_LENGTH}
										minRows={2}
										maxRows={4}
										showCharacterCount={true}
										disabled={!canManageGuild}
										data-flx="guild.guild-tabs.guild-moderation-tab.textarea.change"
									/>
								)}
								data-flx="guild.guild-tabs.guild-moderation-tab.controller--5"
							/>
						)}
					</div>
					{isGuildOwner && (
						<div className={styles.section} data-flx="guild.guild-tabs.guild-moderation-tab.section--4">
							<h3 className={styles.sectionTitle} data-flx="guild.guild-tabs.guild-moderation-tab.section-title--4">
								<Trans>2FA requirement</Trans>
							</h3>
							<p
								className={styles.sectionDescription}
								data-flx="guild.guild-tabs.guild-moderation-tab.section-description--4"
							>
								<Trans>
									Require two-factor authentication for moderators before they can ban, kick, time out, or remove
									messages.
								</Trans>
							</p>
							<Controller
								name="mfa_level"
								control={form.control}
								render={({field}) => {
									const tooltipText = getMfaTooltipText();
									const switchElement = (
										<Switch
											value={field.value === GuildMFALevel.ELEVATED}
											onChange={(value: boolean) => field.onChange(value ? GuildMFALevel.ELEVATED : GuildMFALevel.NONE)}
											disabled={isMfaDisabled}
											label={i18n._(REQUIRE_2FA_FOR_MODERATION_SWITCH_LABEL_DESCRIPTOR)}
											data-flx="guild.guild-tabs.guild-moderation-tab.switch.change--3"
										/>
									);
									return tooltipText ? (
										<Tooltip text={tooltipText} data-flx="guild.guild-tabs.guild-moderation-tab.tooltip">
											{switchElement}
										</Tooltip>
									) : (
										switchElement
									);
								}}
								data-flx="guild.guild-tabs.guild-moderation-tab.controller--6"
							/>
						</div>
					)}
				</div>
			</Form>
		</div>
	);
});

export default GuildModerationTab;
