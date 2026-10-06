// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildComboboxRenderers} from '@app/features/app/components/dialogs/shared/GuildComboboxRenderers';
import styles from '@app/features/auth/components/pages/OAuthAuthorizePage.module.css';
import type {BotInviteDestinationOption} from '@app/features/auth/components/pages/oauth_authorize_page/hooks/useBotGuilds';
import {OAuthAuthorizeActionSection} from '@app/features/auth/components/pages/oauth_authorize_page/OAuthAuthorizeActions';
import type {AuthorizeParams} from '@app/features/auth/components/pages/oauth_authorize_page/OAuthAuthorizePageShared';
import {formatPermissionLabel} from '@app/features/permissions/utils/PermissionUtils';
import {Combobox} from '@app/features/ui/components/form/FormCombobox';
import {Spinner} from '@app/features/ui/components/Spinner';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useMemo} from 'react';

const ADD_BOT_TO_A_COMMUNITY_DESCRIPTOR = msg({
	message: 'Add bot to a destination',
	comment: 'OAuth authorization destination step title. The user chooses where a bot will be added.',
});
const SELECT_A_COMMUNITY_WITH_MANAGE_PERMISSION_DESCRIPTOR = msg({
	message: 'Select a community where you have {permissionName} permission, or a group DM you belong to.',
	comment:
		'OAuth bot invite destination selector helper text shown when the bot requests no permissions. {permissionName} is the localized Manage Guild permission label.',
});
const SELECT_A_COMMUNITY_WITH_REQUESTED_PERMISSIONS_DESCRIPTOR = msg({
	message: 'Select a community where you can grant the requested permissions, or a group DM you belong to.',
	comment: 'OAuth bot invite destination selector helper text shown when the bot invite URL requests bot permissions.',
});
const NO_COMMUNITIES_WITH_MANAGE_PERMISSION_DESCRIPTOR = msg({
	message: 'No communities or group DMs available for this bot.',
	comment:
		'OAuth bot invite empty state shown when the user has no communities or group DMs where they can invite a bot.',
});
const NO_COMMUNITIES_WITH_REQUESTED_PERMISSIONS_DESCRIPTOR = msg({
	message: 'No group DMs or communities where you can grant the requested permissions are available.',
	comment:
		'OAuth bot invite empty state shown when no communities can both invite the bot and grant the requested bot permissions and no group DMs are available.',
});
const LOADING_COMMUNITIES_DESCRIPTOR = msg({
	message: 'Loading destinations…',
	comment: 'OAuth bot invite destination selector placeholder while eligible destinations load.',
});
const CHOOSE_A_COMMUNITY_DESCRIPTOR = msg({
	message: 'Choose a destination',
	comment: 'OAuth bot invite destination selector placeholder.',
});
const COMMUNITY_DESCRIPTOR = msg({
	message: 'Community',
	comment: 'Short option type label in the OAuth bot invite destination selector.',
});
const GROUP_DM_DESCRIPTOR = msg({
	message: 'Group DM',
	comment: 'Short option type label in the OAuth bot invite destination selector.',
});

interface OAuthCommunityStepProps {
	authParams: AuthorizeParams;
	cannotSubmit: boolean;
	destinationOptions: ReadonlyArray<BotInviteDestinationOption>;
	destinationsError: string | null;
	destinationsLoading: boolean;
	hasNextStep: boolean;
	hasPreviousStep: boolean;
	hasRequestedBotPermissions: boolean;
	onAuthorize: () => void;
	onBack: () => void;
	onCancel: () => void;
	onNext: () => void;
	onSelectDestination: (value: string | null) => void;
	redirectHostname: string | null;
	selectedDestinationKey: string | null;
	showInlineActions?: boolean;
	submitting: 'approve' | 'deny' | null;
}

export const OAuthCommunityStep: React.FC<OAuthCommunityStepProps> = ({
	authParams,
	cannotSubmit,
	destinationOptions,
	destinationsError,
	destinationsLoading,
	hasNextStep,
	hasPreviousStep,
	hasRequestedBotPermissions,
	onAuthorize,
	onBack,
	onCancel,
	onNext,
	onSelectDestination,
	redirectHostname,
	selectedDestinationKey,
	showInlineActions = true,
	submitting,
}) => {
	const {i18n} = useLingui();
	const showRedirectNotice = Boolean(redirectHostname && !hasNextStep);
	const manageCommunityPermissionLabel = formatPermissionLabel(i18n, Permissions.MANAGE_GUILD);
	const guildSelectorDescription = hasRequestedBotPermissions
		? i18n._(SELECT_A_COMMUNITY_WITH_REQUESTED_PERMISSIONS_DESCRIPTOR)
		: i18n._(SELECT_A_COMMUNITY_WITH_MANAGE_PERMISSION_DESCRIPTOR, {
				permissionName: manageCommunityPermissionLabel,
			});
	const guildSelectorEmptyState = hasRequestedBotPermissions
		? i18n._(NO_COMMUNITIES_WITH_REQUESTED_PERMISSIONS_DESCRIPTOR)
		: i18n._(NO_COMMUNITIES_WITH_MANAGE_PERMISSION_DESCRIPTOR);
	const guildComboboxRenderers = useMemo(
		() =>
			createGuildComboboxRenderers<BotInviteDestinationOption>({
				styles: {
					optionRow: styles.guildOption,
					valueRow: styles.guildValue,
					avatar: styles.guildAvatar,
					avatarPlaceholder: styles.guildAvatarPlaceholder,
					label: styles.guildOptionLabel,
					rowDisabled: styles.guildOptionDisabled,
					notice: styles.guildOptionNotice,
				},
				getNotice: (option) =>
					option.kind === 'group_dm' ? i18n._(GROUP_DM_DESCRIPTOR) : i18n._(COMMUNITY_DESCRIPTOR),
			}),
		[i18n.locale],
	);
	return (
		<div className={styles.page} data-flx="auth.o-auth-authorize-page.community-step.page">
			<div className={styles.heroCard} data-flx="auth.o-auth-authorize-page.community-step.hero-card">
				<div className={styles.heroCopy} data-flx="auth.o-auth-authorize-page.community-step.hero-copy">
					<h1 className={styles.heroTitle} data-flx="auth.o-auth-authorize-page.community-step.hero-title">
						{i18n._(ADD_BOT_TO_A_COMMUNITY_DESCRIPTOR)}
					</h1>
					<p className={styles.heroDescription} data-flx="auth.o-auth-authorize-page.community-step.hero-description">
						{guildSelectorDescription}
					</p>
				</div>
			</div>
			<div className={styles.panel} data-flx="auth.o-auth-authorize-page.community-step.panel">
				<Combobox
					value={selectedDestinationKey || ''}
					onChange={(value) => onSelectDestination((value as string) || null)}
					options={destinationOptions}
					placeholder={
						destinationsLoading ? i18n._(LOADING_COMMUNITIES_DESCRIPTOR) : i18n._(CHOOSE_A_COMMUNITY_DESCRIPTOR)
					}
					renderOption={guildComboboxRenderers.renderOption}
					renderValue={guildComboboxRenderers.renderValue}
					isSearchable
					disabled={destinationOptions.length === 0 || destinationsLoading}
					data-flx="auth.o-auth-authorize-page.community-step.select"
				/>
				{destinationsLoading && (
					<div className={styles.inlineStatus} data-flx="auth.o-auth-authorize-page.community-step.loading">
						<Spinner data-flx="auth.o-auth-authorize-page.community-step.spinner" />
					</div>
				)}
				{destinationsError && (
					<div className={styles.sectionDescription} data-flx="auth.o-auth-authorize-page.community-step.error">
						{destinationsError}
					</div>
				)}
				{!destinationsLoading && destinationOptions.length === 0 && (
					<div className={styles.emptyState} data-flx="auth.o-auth-authorize-page.community-step.empty-state">
						{guildSelectorEmptyState}
					</div>
				)}
			</div>
			<OAuthAuthorizeActionSection
				authParams={authParams}
				redirectHostname={redirectHostname}
				showInlineActions={showInlineActions}
				showRedirectNotice={showRedirectNotice}
				hasPreviousStep={hasPreviousStep}
				hasNextStep={hasNextStep}
				nextDisabled={cannotSubmit}
				authorizeDisabled={cannotSubmit}
				onAuthorize={onAuthorize}
				onBack={onBack}
				onCancel={onCancel}
				onNext={onNext}
				submitting={submitting}
				dataFlxPrefix="auth.o-auth-authorize-page.community-step"
				dividerDataFlx="auth.o-auth-authorize-page.community-step.section-divider"
				sectionDataFlx="auth.o-auth-authorize-page.community-step.action-section"
				redirectDataFlx="auth.o-auth-authorize-page.community-step.footer-text"
				redirectTooltipDataFlx="auth.o-auth-authorize-page.community-step.tooltip"
				redirectHostnameDataFlx="auth.o-auth-authorize-page.community-step.redirect-hostname"
				data-flx="auth.oauth-authorize-page.o-auth-community-step.o-auth-authorize-action-section"
			/>
		</div>
	);
};
