// SPDX-License-Identifier: AGPL-3.0-or-later

import {EVERYONE_MENTION, EXAMPLE_CUSTOM_URL_SLUG} from '@app/features/app/config/I18nDisplayConstants';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Channels from '@app/features/channel/state/Channels';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import styles from '@app/features/guild/components/modals/guild_tabs/GuildVanityURLTab.module.css';
import Guilds from '@app/features/guild/state/Guilds';
import * as PermissionUtils from '@app/features/permissions/utils/PermissionUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Spinner} from '@app/features/ui/components/Spinner';
import {useRemoteFormReset} from '@app/lib/forms/RemoteFormReset';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {LinkBreakIcon, WarningIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';
import {useForm} from 'react-hook-form';

const VANITY_URL_LOAD_FAILED_DESCRIPTOR = msg({
	message: 'Failed to load vanity URL. Try again.',
	comment: 'Error message in the guild vanity url tab.',
});
const VANITY_URL_REMOVE_FAILED_DESCRIPTOR = msg({
	message: 'Failed to remove vanity URL. Try again.',
	comment: 'Error message in the guild vanity url tab.',
});
const VANITY_URL_ALLOWED_CHARACTERS_DESCRIPTOR = msg({
	message: 'Vanity URL can only contain alphanumeric characters and internal hyphens.',
	comment: 'Description text in the guild vanity url tab.',
});
const VANITY_URL_MIN_LENGTH_DESCRIPTOR = msg({
	message: 'Vanity URL must be at least 2 characters long.',
	comment: 'Description text in the guild vanity url tab.',
});
const VANITY_URL_MAX_LENGTH_DESCRIPTOR = msg({
	message: 'Vanity URL must be no more than 32 characters long.',
	comment: 'Description text in the guild vanity url tab.',
});
const VANITY_URL_SET_DESCRIPTOR = msg({
	message: 'Your vanity URL has been set to {inviteEndpoint}/{vanitySlug}',
	comment:
		'Success toast after saving a community vanity invite URL. inviteEndpoint is the deployment invite URL base; vanitySlug is the custom invite slug.',
});
const VANITY_URL_REMOVED_DESCRIPTOR = msg({
	message: 'Your vanity URL has been removed.',
	comment: 'Success toast after removing a community vanity invite URL.',
});
const logger = new Logger('GuildVanityURLTab');

interface FormInputs {
	code: string;
}

function hasAnyChannelViewableToEveryone(guildId: string): boolean {
	const guild = Guilds.getGuild(guildId);
	if (!guild) return false;
	const channels = Channels.getGuildChannels(guildId);
	for (const channel of channels) {
		if (channel.isGuildCategory()) continue;
		const permissions = PermissionUtils.computePermissions(guildId, channel.toJSON(), null, null, false);
		if ((permissions & Permissions.VIEW_CHANNEL) === Permissions.VIEW_CHANNEL) {
			return true;
		}
	}
	return false;
}

const GuildVanityURLTab: React.FC<{guildId: string}> = observer(({guildId}) => {
	const {i18n} = useLingui();
	const [vanityCode, setVanityCode] = useState<string>('');
	const [uses, setUses] = useState<number>(0);
	const [isLoading, setIsLoading] = useState(true);
	const [isRemoving, setIsRemoving] = useState(false);
	const form = useForm<FormInputs>({defaultValues: {code: ''}});
	const remoteValues = useMemo<FormInputs>(() => ({code: vanityCode}), [vanityCode]);
	const {commitRemoteValues} = useRemoteFormReset<FormInputs>({
		form,
		identityKey: guildId,
		remoteValues,
	});
	const channels = Channels.getGuildChannels(guildId);
	const guild = Guilds.getGuild(guildId);
	const forceShowDisclaimer = DeveloperOptions.forceShowVanityURLDisclaimer;
	const viewChannelPermissionLabel = PermissionUtils.formatPermissionLabel(i18n, Permissions.VIEW_CHANNEL);
	const hasViewableChannel = useMemo(() => {
		return hasAnyChannelViewableToEveryone(guildId);
	}, [guildId, channels, guild]);
	const fetchVanityURL = useCallback(async () => {
		try {
			setIsLoading(true);
			const data = await GuildCommands.getVanityURL(guildId);
			setVanityCode(data.code || '');
			setUses(data.uses);
			form.clearErrors('code');
		} catch (err) {
			logger.error('Failed to fetch vanity URL', err);
			form.setError('code', {
				type: 'server',
				message: i18n._(VANITY_URL_LOAD_FAILED_DESCRIPTOR),
			});
		} finally {
			setIsLoading(false);
		}
	}, [guildId, form, i18n]);
	useEffect(() => {
		void fetchVanityURL();
	}, [fetchVanityURL]);
	const saveVanityCode = useCallback(
		async (nextCode: string | null) => {
			await GuildCommands.updateVanityURL(guildId, nextCode);
			const normalizedCode = nextCode ?? '';
			setVanityCode(normalizedCode);
			commitRemoteValues({code: normalizedCode});
			ToastCommands.createToast({
				type: 'success',
				children: nextCode
					? i18n._(VANITY_URL_SET_DESCRIPTOR, {
							inviteEndpoint: RuntimeConfig.inviteEndpoint,
							vanitySlug: nextCode,
						})
					: i18n._(VANITY_URL_REMOVED_DESCRIPTOR),
			});
			await fetchVanityURL();
		},
		[guildId, commitRemoteValues, i18n, fetchVanityURL],
	);
	const onSubmit = useCallback(
		async (data: FormInputs) => {
			form.clearErrors('code');
			const trimmedValue = data.code.trim();
			if (trimmedValue && !/^[a-z0-9](?:[a-z0-9-]*[a-z0-9])?$/i.test(trimmedValue)) {
				form.setError('code', {
					message: i18n._(VANITY_URL_ALLOWED_CHARACTERS_DESCRIPTOR),
				});
				return;
			}
			await saveVanityCode(trimmedValue || null);
		},
		[form, i18n, saveVanityCode],
	);
	const handleRemoveVanityURL = useCallback(async () => {
		try {
			form.clearErrors('code');
			setIsRemoving(true);
			await saveVanityCode(null);
		} catch (err) {
			logger.error('Failed to remove vanity URL', err);
			form.setError('code', {
				type: 'server',
				message: i18n._(VANITY_URL_REMOVE_FAILED_DESCRIPTOR),
			});
		} finally {
			setIsRemoving(false);
		}
	}, [form, i18n, saveVanityCode]);
	const {handleSubmit, isSubmitting} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'code',
	});
	const errorMessage = form.formState.errors.code?.message;
	if (isLoading) {
		return (
			<div className={styles.spinnerContainer} data-flx="guild.guild-tabs.guild-vanity-url-tab.spinner-container">
				<Spinner data-flx="guild.guild-tabs.guild-vanity-url-tab.spinner" />
			</div>
		);
	}
	return (
		<div className={styles.container} data-flx="guild.guild-tabs.guild-vanity-url-tab.container">
			{errorMessage && (
				<div className={styles.errorAlert} role="alert" data-flx="guild.guild-tabs.guild-vanity-url-tab.error-alert">
					<WarningIcon size={remFromPx(16)} weight="fill" data-flx="guild.guild-tabs.guild-vanity-url-tab.error-icon" />
					<span data-flx="guild.guild-tabs.guild-vanity-url-tab.error-message">{errorMessage}</span>
				</div>
			)}
			{(!hasViewableChannel || forceShowDisclaimer) && (
				<div className={styles.warning} data-flx="guild.guild-tabs.guild-vanity-url-tab.warning">
					<div className={styles.warningContent} data-flx="guild.guild-tabs.guild-vanity-url-tab.warning-content">
						<div className={styles.warningIcon} data-flx="guild.guild-tabs.guild-vanity-url-tab.warning-icon">
							<WarningIcon
								size={remFromPx(20)}
								weight="fill"
								data-flx="guild.guild-tabs.guild-vanity-url-tab.warning-icon--2"
							/>
						</div>
						<div className={styles.warningBody} data-flx="guild.guild-tabs.guild-vanity-url-tab.warning-body">
							<p className={styles.warningTitle} data-flx="guild.guild-tabs.guild-vanity-url-tab.warning-title">
								<Trans>Vanity URL won't work</Trans>
							</p>
							<p className={styles.warningText} data-flx="guild.guild-tabs.guild-vanity-url-tab.warning-text">
								<Trans>
									At least one channel must have "{viewChannelPermissionLabel}" permission enabled for{' '}
									{EVERYONE_MENTION} in order for the vanity URL to work. Currently, no channels are viewable to{' '}
									{EVERYONE_MENTION}.
								</Trans>
							</p>
						</div>
					</div>
				</div>
			)}
			<Form form={form} onSubmit={handleSubmit} data-flx="guild.guild-tabs.guild-vanity-url-tab.form.submit">
				<div className={styles.formBody} data-flx="guild.guild-tabs.guild-vanity-url-tab.form-body">
					<div data-flx="guild.guild-tabs.guild-vanity-url-tab.div">
						<div className={styles.fieldLabel} data-flx="guild.guild-tabs.guild-vanity-url-tab.field-label">
							<Trans>Vanity URL</Trans>
						</div>
						<div className={styles.inputRow} data-flx="guild.guild-tabs.guild-vanity-url-tab.input-row">
							<span className={styles.inputPrefix} data-flx="guild.guild-tabs.guild-vanity-url-tab.input-prefix">
								{RuntimeConfig.inviteEndpoint}/
							</span>
							<div className={styles.inputWrapper} data-flx="guild.guild-tabs.guild-vanity-url-tab.input-wrapper">
								<Input
									data-flx="guild.guild-tabs.guild-vanity-url-tab.input"
									{...form.register('code', {
										minLength: {
											value: 2,
											message: i18n._(VANITY_URL_MIN_LENGTH_DESCRIPTOR),
										},
										maxLength: {
											value: 32,
											message: i18n._(VANITY_URL_MAX_LENGTH_DESCRIPTOR),
										},
									})}
									label=""
									placeholder={EXAMPLE_CUSTOM_URL_SLUG}
									minLength={2}
									maxLength={32}
								/>
							</div>
						</div>
						<p className={styles.helpText} data-flx="guild.guild-tabs.guild-vanity-url-tab.help-text">
							<Trans>
								Vanity URLs must be between 2 and 32 characters long and can only contain alphanumeric characters and
								internal hyphens.
							</Trans>
						</p>
					</div>
					{vanityCode && (
						<div data-flx="guild.guild-tabs.guild-vanity-url-tab.div--2">
							<p className={styles.usage} data-flx="guild.guild-tabs.guild-vanity-url-tab.usage">
								<Trans>Current uses:</Trans>{' '}
								<span className={styles.usageValue} data-flx="guild.guild-tabs.guild-vanity-url-tab.usage-value">
									{uses}
								</span>
							</p>
						</div>
					)}
					<div className={styles.actions} data-flx="guild.guild-tabs.guild-vanity-url-tab.actions">
						<Button
							type="button"
							variant="secondary"
							submitting={isRemoving}
							disabled={!vanityCode || isSubmitting}
							leftIcon={
								<LinkBreakIcon size={remFromPx(16)} data-flx="guild.guild-tabs.guild-vanity-url-tab.link-break-icon" />
							}
							onClick={handleRemoveVanityURL}
							data-flx="guild.guild-tabs.guild-vanity-url-tab.button.remove"
						>
							<Trans>Remove</Trans>
						</Button>
						<Button
							type="submit"
							submitting={isSubmitting}
							disabled={isRemoving}
							data-flx="guild.guild-tabs.guild-vanity-url-tab.button.submit"
						>
							<Trans>Save</Trans>
						</Button>
					</div>
				</div>
			</Form>
		</div>
	);
});

export default GuildVanityURLTab;
