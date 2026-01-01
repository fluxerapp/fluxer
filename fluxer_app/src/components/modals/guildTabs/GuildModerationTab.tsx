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

import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {Controller, useForm} from 'react-hook-form';
import * as GuildActionCreators from '~/actions/GuildActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as UnsavedChangesActionCreators from '~/actions/UnsavedChangesActionCreators';
import {GuildExplicitContentFilterTypes, GuildMFALevel, GuildVerificationLevel, Permissions} from '~/Constants';
import {Form} from '~/components/form/Form';
import {Switch} from '~/components/form/Switch';
import type {RadioOption} from '~/components/uikit/RadioGroup/RadioGroup';
import {RadioGroup} from '~/components/uikit/RadioGroup/RadioGroup';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {useFormSubmit} from '~/hooks/useFormSubmit';
import GuildStore from '~/stores/GuildStore';
import PermissionStore from '~/stores/PermissionStore';
import UserStore from '~/stores/UserStore';
import styles from './GuildModerationTab.module.css';

interface FormInputs {
	verification_level: number;
	mfa_level: number;
	explicit_content_filter: number;
}

const GUILD_MODERATION_TAB_ID = 'moderation';

const GuildModerationTab: React.FC<{guildId: string}> = observer(({guildId}) => {
	const {t} = useLingui();
	const guild = GuildStore.getGuild(guildId);
	const currentUser = UserStore.currentUser;

	const canManageGuild = PermissionStore.can(Permissions.MANAGE_GUILD, {guildId});
	const isGuildOwner = guild?.ownerId === currentUser?.id;

	const form = useForm<FormInputs>({
		defaultValues: React.useMemo(
			() => ({
				verification_level: guild?.verificationLevel ?? GuildVerificationLevel.NONE,
				mfa_level: guild?.mfaLevel ?? GuildMFALevel.NONE,
				explicit_content_filter: guild?.explicitContentFilter ?? GuildExplicitContentFilterTypes.DISABLED,
			}),
			[guild],
		),
	});

	const onSubmit = React.useCallback(
		async (data: FormInputs) => {
			if (!guild) return;

			const updateData: {
				verification_level: number;
				explicit_content_filter: number;
				mfa_level?: number;
			} = {
				verification_level: data.verification_level,
				explicit_content_filter: data.explicit_content_filter,
			};

			if (isGuildOwner) {
				updateData.mfa_level = data.mfa_level;
			}

			await GuildActionCreators.update(guild.id, updateData);
			form.reset(data);
			ToastActionCreators.createToast({type: 'success', children: <Trans>Community updated</Trans>});
		},
		[guild, form, isGuildOwner],
	);

	const {handleSubmit: handleSave} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'verification_level',
	});

	const handleReset = React.useCallback(() => {
		if (!guild) return;
		form.reset({
			verification_level: guild.verificationLevel ?? GuildVerificationLevel.NONE,
			mfa_level: guild.mfaLevel ?? GuildMFALevel.NONE,
			explicit_content_filter: guild.explicitContentFilter ?? GuildExplicitContentFilterTypes.DISABLED,
		});
	}, [form, guild]);

	const isFormDirty = form.formState.isDirty;

	React.useEffect(() => {
		UnsavedChangesActionCreators.setUnsavedChanges(GUILD_MODERATION_TAB_ID, isFormDirty);
	}, [isFormDirty]);

	React.useEffect(() => {
		UnsavedChangesActionCreators.setTabData(GUILD_MODERATION_TAB_ID, {
			onReset: handleReset,
			onSave: handleSave,
			isSubmitting: form.formState.isSubmitting,
		});
	}, [handleReset, handleSave, form.formState.isSubmitting]);

	React.useEffect(() => {
		return () => {
			UnsavedChangesActionCreators.clearUnsavedChanges(GUILD_MODERATION_TAB_ID);
		};
	}, []);

	if (!guild) return null;

	const currentUserHas2FA = currentUser?.mfaEnabled ?? false;
	const colorizeLabel = (label: string, color?: string) => (color ? <span style={{color}}>{label}</span> : label);
	const currentMfaLevel = form.watch('mfa_level');
	const isTogglingOn = currentMfaLevel === GuildMFALevel.ELEVATED;
	const canToggleOn = isGuildOwner && currentUserHas2FA;
	const canToggleOff = isGuildOwner;
	const isMfaDisabled = isTogglingOn ? !canToggleOn : !canToggleOff;

	const getMfaTooltipText = (): string | undefined => {
		if (!isGuildOwner) {
			return t`Only the community owner can change this setting`;
		}
		if (isTogglingOn && !currentUserHas2FA) {
			return t`Enable 2FA on your account before requiring it for moderators`;
		}
		return;
	};

	const verificationLevelOptions: ReadonlyArray<RadioOption<number>> = [
		{
			value: GuildVerificationLevel.NONE,
			name: t`None`,
			desc: t`No verification is required.`,
		},
		{
			value: GuildVerificationLevel.LOW,
			name: colorizeLabel(t`Low`, '#22c55e'),
			desc: t`Requires a verified email address.`,
		},
		{
			value: GuildVerificationLevel.MEDIUM,
			name: colorizeLabel(t`Medium`, '#f59e0b'),
			desc: t`Requires a verified email address, and an account that's at least 5 minutes old.`,
		},
		{
			value: GuildVerificationLevel.HIGH,
			name: colorizeLabel(t`High`, '#f97316'),
			desc: t`Requires everything in Medium, plus being a member of the server for at least 10 minutes.`,
		},
		{
			value: GuildVerificationLevel.VERY_HIGH,
			name: colorizeLabel(t`Very high`, '#ef4444'),
			desc: t`Requires everything in High, plus a verified phone number.`,
		},
	];

	return (
		<div className={styles.container}>
			<Form form={form} onSubmit={handleSave}>
				<div className={styles.container}>
					<div className={styles.section}>
						<h3 className={styles.sectionTitle}>
							<Trans>Member verification</Trans>
						</h3>
						<div className={styles.sectionDescriptionMultiline}>
							<p>
								<Trans>Choose what members must have before they can post or DM community members.</Trans>
							</p>
							<p>
								<Trans>
									Members with roles can bypass these checks. For public spaces, we recommend enabling verification.
								</Trans>
							</p>
						</div>
						<Controller
							name="verification_level"
							control={form.control}
							render={({field}) => (
								<RadioGroup
									value={field.value}
									onChange={field.onChange}
									disabled={!canManageGuild}
									options={verificationLevelOptions}
									aria-label={t`Member verification level`}
								/>
							)}
						/>
					</div>

					<div className={styles.section}>
						<h3 className={styles.sectionTitle}>
							<Trans>Content filtering</Trans>
						</h3>
						<p className={styles.sectionDescription}>
							<Trans>Automatically screen messages for explicit content in non-NSFW channels.</Trans>
						</p>
						<Controller
							name="explicit_content_filter"
							control={form.control}
							render={({field}) => (
								<RadioGroup
									value={field.value}
									onChange={field.onChange}
									disabled={!canManageGuild}
									options={[
										{
											value: GuildExplicitContentFilterTypes.DISABLED,
											name: t`Off`,
											desc: t`Let the community self-moderate`,
										},
										{
											value: GuildExplicitContentFilterTypes.MEMBERS_WITHOUT_ROLES,
											name: colorizeLabel(t`Filter members without roles`, '#f59e0b'),
											desc: t`Suggested for most communities`,
										},
										{
											value: GuildExplicitContentFilterTypes.ALL_MEMBERS,
											name: colorizeLabel(t`Filter everyone`, '#ef4444'),
											desc: t`Maximum protection for family-friendly spaces`,
										},
									]}
									aria-label={t`Explicit content filter setting`}
								/>
							)}
						/>
					</div>

					{isGuildOwner && (
						<div className={styles.section}>
							<h3 className={styles.sectionTitle}>
								<Trans>2FA requirement</Trans>
							</h3>
							<p className={styles.sectionDescription}>
								<Trans>
									Require two-factor authentication for moderators before they can ban, kick, timeout, or remove
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
											label={t`Require 2FA for moderation actions`}
										/>
									);
									return tooltipText ? <Tooltip text={tooltipText}>{switchElement}</Tooltip> : switchElement;
								}}
							/>
						</div>
					)}
				</div>
			</Form>
		</div>
	);
});

export default GuildModerationTab;
