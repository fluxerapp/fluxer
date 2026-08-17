// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/floating/UserAreaPopout.module.css';
import {CustomStatusDisplay} from '@app/features/app/components/shared/custom_status_display/CustomStatusDisplay';
import {
	getTimeWindowPresets,
	minutesToMs,
	TIME_WINDOW_FOR_LABEL_MESSAGES,
	type TimeWindowKey,
	type TimeWindowPreset,
} from '@app/features/app/config/TimeWindowPresets';
import {getStatusTypeLabel, STATUS_UNTIL_I_CHANGE_IT_DESCRIPTOR} from '@app/features/app/constants/AppConstants';
import {getAccountAvatarUrl, getAccountDisplayName} from '@app/features/auth/components/accounts/AccountListItem';
import AccountSwitcherModal from '@app/features/auth/components/accounts/AccountSwitcherModal';
import {useAccountSwitcherLogic} from '@app/features/auth/utils/AccountSwitcherModalUtils';
import DeveloperMode from '@app/features/devtools/state/DeveloperMode';
import {COPY_USERNAME_DESCRIPTOR, UNKNOWN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {Account} from '@app/features/platform/state/AuthSession';
import {formatClientBuildInfo, getClientInfo, getClientInfoSync} from '@app/features/platform/utils/ClientInfo';
import Presence from '@app/features/presence/state/Presence';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {getUserAccentColor} from '@app/features/theme/utils/AccentColorUtils';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {MockAvatar} from '@app/features/ui/components/MockAvatar';
import {StatusIndicator} from '@app/features/ui/components/StatusIndicator';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import FocusRingScope from '@app/features/ui/focus_ring/FocusRingScope';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {CustomStatusModal} from '@app/features/user/components/modals/CustomStatusModal';
import {UserProfileModal} from '@app/features/user/components/modals/UserProfileModal';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import {UserProfileBadges} from '@app/features/user/components/popouts/UserProfileBadges';
import userProfilePopoutStyles from '@app/features/user/components/popouts/UserProfilePopout.module.css';
import {UserProfilePreviewBio} from '@app/features/user/components/popouts/UserProfileShared';
import {ProfileCardBanner} from '@app/features/user/components/profile/profile_card/ProfileCardBanner';
import {ProfileCardContent} from '@app/features/user/components/profile/profile_card/ProfileCardContent';
import {ProfileCardFooter} from '@app/features/user/components/profile/profile_card/ProfileCardFooter';
import {ProfileCardLayout} from '@app/features/user/components/profile/profile_card/ProfileCardLayout';
import {ProfileCardUserInfo} from '@app/features/user/components/profile/profile_card/ProfileCardUserInfo';
import {useAutoplayExpandedProfileAnimations} from '@app/features/user/hooks/useAutoplayExpandedProfileAnimations';
import {normalizeCustomStatus} from '@app/features/user/state/CustomStatus';
import StatusExpiry from '@app/features/user/state/StatusExpiry';
import UserProfile from '@app/features/user/state/UserProfile';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import * as ProfileDisplayUtils from '@app/features/user/utils/ProfileDisplayUtils';
import {createMockProfile} from '@app/features/user/utils/ProfileUtils';
import {COPIED_STATS_JSON_DESCRIPTOR} from '@app/features/voice/components/StatsForNerdsCopyDescriptors';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {buildStatsForNerdsCopyPayload, collectStatsForNerdsSnapshot} from '@app/features/voice/utils/StatsForNerdsCopy';
import {MEDIA_PROXY_PROFILE_BANNER_SIZE_POPOUT} from '@fluxer/constants/src/MediaProxyAssetSizes';
import {StatusTypes} from '@fluxer/constants/src/StatusConstants';
import type {MessageDescriptor} from '@lingui/core';
import {Trans, useLingui} from '@lingui/react/macro';
import {
	CaretRightIcon,
	ChartLineIcon,
	CheckIcon,
	CopyIcon,
	GearIcon,
	IdentificationBadgeIcon,
	InfoIcon,
	PencilIcon,
	SmileyIcon,
	UsersThreeIcon,
} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useMemo, useRef} from 'react';

const STATUS_ORDER = [StatusTypes.ONLINE, StatusTypes.IDLE, StatusTypes.DND, StatusTypes.INVISIBLE];
const STATUS_EXPIRY_LABEL_MESSAGES: Record<TimeWindowKey, MessageDescriptor> = {
	...TIME_WINDOW_FOR_LABEL_MESSAGES,
	never: STATUS_UNTIL_I_CHANGE_IT_DESCRIPTOR,
};

interface UserStatusExpiryOption {
	id: TimeWindowKey;
	key: TimeWindowKey;
	label: MessageDescriptor;
	durationMs: number | null;
}

const STATUS_DESCRIPTIONS: Record<(typeof STATUS_ORDER)[number], React.ReactNode | null> = {
	[StatusTypes.ONLINE]: null,
	[StatusTypes.IDLE]: null,
	[StatusTypes.DND]: <Trans>You won't receive notifications on desktop</Trans>,
	[StatusTypes.INVISIBLE]: <Trans>You'll appear offline</Trans>,
};

interface StatusMenuProps {
	onSelectStatus: (status: (typeof STATUS_ORDER)[number], durationMs: number | null) => void;
	onClose: () => void;
}

const StatusMenu = observer(({onSelectStatus, onClose}: StatusMenuProps) => {
	const {i18n} = useLingui();
	const isDeveloper = DeveloperMode.isDeveloper;
	const statusExpiryOptions = useMemo<ReadonlyArray<UserStatusExpiryOption>>(
		() =>
			getTimeWindowPresets({includeDeveloperOptions: isDeveloper}).map((preset: TimeWindowPreset) => ({
				id: preset.key,
				key: preset.key,
				label: STATUS_EXPIRY_LABEL_MESSAGES[preset.key],
				durationMs: minutesToMs(preset.minutes),
			})),
		[isDeveloper],
	);
	const handleSelect = (status: (typeof STATUS_ORDER)[number], durationMs: number | null) => {
		onSelectStatus(status, durationMs);
		onClose();
		PopoutCommands.close();
	};
	return (
		<div className={styles.statusMenu} data-flx="app.floating.user-area-popout.status-menu.status-menu">
			{STATUS_ORDER.map((status) => {
				const hasExpiryOptions = status !== StatusTypes.ONLINE;
				const description = STATUS_DESCRIPTIONS[status];
				const rowContent = (
					<FocusRing offset={-2} data-flx="app.floating.user-area-popout.status-menu.focus-ring">
						<button
							type="button"
							className={styles.statusMenuItem}
							onClick={() => handleSelect(status, null)}
							data-flx="app.floating.user-area-popout.status-menu.status-menu-item.select.button"
						>
							<div
								className={styles.statusMenuIcon}
								data-flx="app.floating.user-area-popout.status-menu.status-menu-icon"
							>
								<StatusIndicator
									status={status}
									size={14}
									monochromeColor="var(--brand-primary-fill)"
									data-flx="app.floating.user-area-popout.status-menu.status-indicator"
								/>
							</div>
							<div
								className={styles.statusMenuText}
								data-flx="app.floating.user-area-popout.status-menu.status-menu-text"
							>
								<span
									className={styles.statusMenuLabel}
									data-flx="app.floating.user-area-popout.status-menu.status-menu-label"
								>
									{getStatusTypeLabel(i18n, status)}
								</span>
								{description && (
									<span
										className={styles.statusMenuDescription}
										data-flx="app.floating.user-area-popout.status-menu.status-menu-description"
									>
										{description}
									</span>
								)}
							</div>
							{hasExpiryOptions && (
								<CaretRightIcon
									size={14}
									weight="bold"
									className={styles.statusMenuChevron}
									data-flx="app.floating.user-area-popout.status-menu.status-menu-chevron"
								/>
							)}
						</button>
					</FocusRing>
				);
				if (!hasExpiryOptions) {
					return (
						<div
							key={status}
							className={styles.statusMenuRow}
							data-flx="app.floating.user-area-popout.status-menu.status-menu-row"
						>
							{rowContent}
						</div>
					);
				}
				return (
					<Popout
						key={status}
						hoverDelay={200}
						position="right-start"
						preventInvert
						offsetMainAxis={8}
						animationType="none"
						render={({onClose: closeExpiry}) => (
							<div className={styles.expiryPopup} data-flx="app.floating.user-area-popout.status-menu.expiry-popup">
								{statusExpiryOptions.map((option: UserStatusExpiryOption) => (
									<FocusRing
										key={option.id}
										offset={-2}
										data-flx="app.floating.user-area-popout.status-menu.focus-ring--2"
									>
										<button
											type="button"
											className={styles.expiryItem}
											onClick={() => {
												handleSelect(status, option.durationMs);
												closeExpiry();
											}}
											data-flx="app.floating.user-area-popout.status-menu.expiry-item.select.button"
										>
											<span
												className={styles.expiryLabel}
												data-flx="app.floating.user-area-popout.status-menu.expiry-label"
											>
												{i18n._(option.label)}
											</span>
										</button>
									</FocusRing>
								))}
							</div>
						)}
						data-flx="app.floating.user-area-popout.status-menu.popout"
					>
						{rowContent}
					</Popout>
				);
			})}
		</div>
	);
});

interface ActionButtonProps extends React.ButtonHTMLAttributes<HTMLButtonElement> {
	icon: React.ReactNode;
	label: React.ReactNode;
	hint?: React.ReactNode;
	chevron?: boolean;
}

const ActionButton = React.forwardRef<HTMLButtonElement, ActionButtonProps>(
	({icon, label, hint, chevron = false, onClick, className, disabled, ...rest}, ref) => (
		<FocusRing offset={-2} data-flx="app.floating.user-area-popout.action-button.focus-ring">
			<button
				type="button"
				ref={ref}
				disabled={disabled}
				onClick={onClick}
				className={clsx(styles.actionButton, className, disabled && styles.actionButtonDisabled)}
				data-flx="app.floating.user-area-popout.action-button.action-button.click"
				{...rest}
			>
				<div
					className={styles.actionIcon}
					aria-hidden="true"
					data-flx="app.floating.user-area-popout.action-button.action-icon"
				>
					{icon}
				</div>
				<div className={styles.actionContent} data-flx="app.floating.user-area-popout.action-button.action-content">
					<span className={styles.actionLabel} data-flx="app.floating.user-area-popout.action-button.action-label">
						{label}
					</span>
					{hint && (
						<span className={styles.actionHint} data-flx="app.floating.user-area-popout.action-button.action-hint">
							{hint}
						</span>
					)}
				</div>
				{chevron && (
					<CaretRightIcon
						size={14}
						weight="bold"
						className={styles.actionChevron}
						aria-hidden="true"
						data-flx="app.floating.user-area-popout.action-button.action-chevron"
					/>
				)}
			</button>
		</FocusRing>
	),
);

ActionButton.displayName = 'ActionButton';

interface SwitchAccountsMenuProps {
	accounts: Array<Account>;
	currentAccountId: string | null;
	onSelect: (userId: string) => void;
	onManage: () => void;
	onClose: () => void;
}

const SwitchAccountsMenu = observer(
	({accounts, currentAccountId, onSelect, onManage, onClose}: SwitchAccountsMenuProps) => {
		return (
			<div className={styles.switchMenu} data-flx="app.floating.user-area-popout.switch-accounts-menu.switch-menu">
				<div
					className={styles.switchMenuList}
					data-flx="app.floating.user-area-popout.switch-accounts-menu.switch-menu-list"
				>
					{accounts.map((account) => {
						const isCurrent = account.userId === currentAccountId;
						const avatarUrl = getAccountAvatarUrl(account);
						const displayName = getAccountDisplayName(account, '???');
						const userData = account.userData;
						const accountTag = userData
							? NicknameUtils.formatTagForStreamerMode(`${userData.username}#${userData.discriminator}`)
							: displayName;
						return (
							<FocusRing
								key={account.userId}
								offset={-2}
								data-flx="app.floating.user-area-popout.switch-accounts-menu.focus-ring"
							>
								<button
									type="button"
									className={styles.accountMenuItem}
									onClick={() => {
										if (!isCurrent) {
											onSelect(account.userId);
										}
										onClose();
										PopoutCommands.close();
									}}
									data-flx="app.floating.user-area-popout.switch-accounts-menu.account-menu-item.close.button"
								>
									<div
										className={styles.accountMenuAvatar}
										data-flx="app.floating.user-area-popout.switch-accounts-menu.account-menu-avatar"
									>
										<MockAvatar
											size={24}
											avatarUrl={avatarUrl}
											userTag={displayName}
											data-flx="app.floating.user-area-popout.switch-accounts-menu.mock-avatar"
										/>
									</div>
									<div
										className={styles.accountMenuInfo}
										data-flx="app.floating.user-area-popout.switch-accounts-menu.account-menu-info"
									>
										<span
											className={styles.accountMenuTag}
											data-flx="app.floating.user-area-popout.switch-accounts-menu.account-menu-tag"
										>
											{displayName}
											<span
												className={styles.accountMenuDiscriminator}
												data-flx="app.floating.user-area-popout.switch-accounts-menu.account-menu-discriminator"
											>
												{accountTag}
											</span>
										</span>
										{isCurrent && (
											<span
												className={styles.accountMenuMeta}
												data-flx="app.floating.user-area-popout.switch-accounts-menu.account-menu-meta"
											>
												<Trans>Active account</Trans>
											</span>
										)}
									</div>
									{isCurrent && (
										<div
											className={styles.accountMenuCheck}
											data-flx="app.floating.user-area-popout.switch-accounts-menu.account-menu-check"
										>
											<CheckIcon
												size={10}
												weight="bold"
												data-flx="app.floating.user-area-popout.switch-accounts-menu.check-icon"
											/>
										</div>
									)}
								</button>
							</FocusRing>
						);
					})}
				</div>
				<div
					className={styles.switchMenuFooter}
					data-flx="app.floating.user-area-popout.switch-accounts-menu.switch-menu-footer"
				>
					<FocusRing offset={-2} data-flx="app.floating.user-area-popout.switch-accounts-menu.focus-ring--2">
						<button
							type="button"
							className={styles.manageAccountsButton}
							onClick={() => {
								onClose();
								onManage();
							}}
							data-flx="app.floating.user-area-popout.switch-accounts-menu.manage-accounts-button.close"
						>
							<GearIcon
								size={16}
								weight="bold"
								data-flx="app.floating.user-area-popout.switch-accounts-menu.gear-icon"
							/>
							<Trans>Manage accounts</Trans>
						</button>
					</FocusRing>
				</div>
			</div>
		);
	},
);
export const UserAreaPopout = observer(() => {
	const {i18n} = useLingui();
	const accountLogic = useAccountSwitcherLogic();
	const currentUser = Users.getCurrentUser();
	const currentUserId = currentUser?.id ?? null;
	const status = currentUserId ? Presence.getStatus(currentUserId) : StatusTypes.ONLINE;
	const openEditProfile = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<UserSettingsModal
					initialTab="my_profile"
					data-flx="app.floating.user-area-popout.open-edit-profile.user-settings-modal"
				/>
			)),
		);
		PopoutCommands.close();
	}, []);
	const openUserProfile = useCallback(() => {
		if (!currentUserId) {
			return;
		}
		ModalCommands.push(
			modal(() => (
				<UserProfileModal
					userId={currentUserId}
					data-flx="app.floating.user-area-popout.open-user-profile.user-profile-modal"
				/>
			)),
		);
		PopoutCommands.close();
	}, [currentUserId]);
	const openCustomStatus = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<CustomStatusModal data-flx="app.floating.user-area-popout.open-custom-status.custom-status-modal" />
			)),
		);
		PopoutCommands.close();
	}, []);
	const handleStatusChange = useCallback((statusType: (typeof STATUS_ORDER)[number], durationMs: number | null) => {
		StatusExpiry.setActiveStatusExpiry({
			status: statusType,
			durationMs,
		});
	}, []);
	const handleCopyUserId = useCallback(() => {
		if (!currentUserId) {
			return;
		}
		TextCopyCommands.copy(i18n, currentUserId);
	}, [currentUserId, i18n]);
	const handleCopyBuildInfo = useCallback(() => {
		void getClientInfo()
			.catch(() => getClientInfoSync())
			.then((info) => {
				TextCopyCommands.copy(i18n, formatClientBuildInfo(info, {unknownLabel: i18n._(UNKNOWN_DESCRIPTOR)}));
			});
	}, [i18n]);
	const handleCopyStats = useCallback(() => {
		const data = collectStatsForNerdsSnapshot();
		void buildStatsForNerdsCopyPayload(data).then((payload) => {
			void navigator.clipboard.writeText(JSON.stringify(payload, null, 2)).then(() => {
				ToastCommands.createToast({type: 'success', children: i18n._(COPIED_STATS_JSON_DESCRIPTOR)});
			});
		});
	}, [i18n]);
	const handleCopyUserTag = useCallback(() => {
		if (!currentUser) {
			return;
		}
		TextCopyCommands.copy(i18n, currentUser.tag);
	}, [currentUser, i18n]);
	const openManageAccounts = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<AccountSwitcherModal data-flx="app.floating.user-area-popout.open-manage-accounts.account-switcher-modal" />
			)),
		);
		PopoutCommands.close();
	}, []);
	const profile = useMemo(() => {
		if (!currentUser) {
			return null;
		}
		return UserProfile.getProfile(currentUser.id) ?? createMockProfile(currentUser);
	}, [currentUserId, currentUser]);
	const profileData = useMemo(() => profile?.getEffectiveProfile() ?? null, [profile]);
	const profileContext = useMemo<ProfileDisplayUtils.ProfileDisplayContext | null>(() => {
		if (!currentUser || !profile) {
			return null;
		}
		return {
			user: currentUser,
			profile,
			guildId: undefined,
			guildMember: undefined,
			guildMemberProfile: undefined,
		};
	}, [currentUser, profile]);
	const {avatarUrl, hoverAvatarUrl} = useMemo(() => {
		if (!profileContext) {
			return {avatarUrl: null, hoverAvatarUrl: null};
		}
		return ProfileDisplayUtils.getProfileAvatarUrls(profileContext);
	}, [profileContext]);
	const shouldAutoplayProfileAnimations = useAutoplayExpandedProfileAnimations();
	const {bannerUrl, hoverBannerUrl} = useMemo(() => {
		if (!profileContext) {
			return {bannerUrl: null, hoverBannerUrl: null};
		}
		return ProfileDisplayUtils.getProfileBannerUrls(profileContext, undefined, MEDIA_PROXY_PROFILE_BANNER_SIZE_POPOUT);
	}, [profileContext]);
	const accentColor = getUserAccentColor(currentUser, profileData?.accent_color);
	const borderColor = accentColor;
	const bannerColor = accentColor;
	const displayName = currentUser ? NicknameUtils.getNickname(currentUser, null) : '';
	const customStatus = currentUserId ? Presence.getCustomStatus(currentUserId) : null;
	const hasCustomStatus = Boolean(normalizeCustomStatus(customStatus));
	const popoutContainerRef = useRef<HTMLDivElement | null>(null);
	if (!currentUser || !profile) {
		return null;
	}
	return (
		<FocusRingScope containerRef={popoutContainerRef} data-flx="app.floating.user-area-popout.focus-ring-scope">
			<div ref={popoutContainerRef} className={styles.container} data-flx="app.floating.user-area-popout.container">
				<ProfileCardLayout borderColor={borderColor} data-flx="app.floating.user-area-popout.profile-card-layout">
					<ProfileCardBanner
						bannerUrl={bannerUrl}
						hoverBannerUrl={hoverBannerUrl}
						bannerColor={bannerColor}
						user={currentUser}
						avatarUrl={avatarUrl}
						hoverAvatarUrl={hoverAvatarUrl}
						disablePresence={false}
						isClickable={true}
						onAvatarClick={openUserProfile}
						data-flx="app.floating.user-area-popout.profile-card-banner"
					/>
					<UserProfileBadges
						user={currentUser}
						profile={profile}
						data-flx="app.floating.user-area-popout.user-profile-badges"
					/>
					<ProfileCardContent isWebhook={false} data-flx="app.floating.user-area-popout.profile-card-content">
						<ProfileCardUserInfo
							displayName={displayName}
							user={currentUser}
							pronouns={profileData?.pronouns}
							showUsername={true}
							isClickable={false}
							isWebhook={false}
							usernameActions={
								<Tooltip
									text={i18n._(COPY_USERNAME_DESCRIPTOR)}
									position="top"
									data-flx="app.floating.user-area-popout.tooltip"
								>
									<FocusRing offset={-2} data-flx="app.floating.user-area-popout.focus-ring">
										<button
											type="button"
											className={styles.copyUsernameButton}
											onClick={handleCopyUserTag}
											aria-label={i18n._(COPY_USERNAME_DESCRIPTOR)}
											data-flx="app.floating.user-area-popout.copy-username-button.copy-user-tag"
										>
											<CopyIcon size={remFromPx(14)} weight="fill" data-flx="app.floating.user-area-popout.copy-icon" />
										</button>
									</FocusRing>
								</Tooltip>
							}
							data-flx="app.floating.user-area-popout.profile-card-user-info"
						/>
						<div className={styles.customStatusRow} data-flx="app.floating.user-area-popout.custom-status-row">
							{hasCustomStatus ? (
								<CustomStatusDisplay
									customStatus={customStatus}
									className={userProfilePopoutStyles.profileCustomStatusText}
									allowJumboEmoji
									maxLines={0}
									isEditable={true}
									onEdit={openCustomStatus}
									alwaysAnimate={shouldAutoplayProfileAnimations}
									data-flx="app.floating.user-area-popout.custom-status-display"
								/>
							) : (
								<FocusRing offset={-2} data-flx="app.floating.user-area-popout.focus-ring--2">
									<button
										type="button"
										className={styles.customStatusPlaceholder}
										onClick={openCustomStatus}
										data-flx="app.floating.user-area-popout.custom-status-placeholder.open-custom-status.button"
									>
										<SmileyIcon
											size={remFromPx(14)}
											weight="regular"
											className={styles.customStatusPlaceholderIcon}
											data-flx="app.floating.user-area-popout.custom-status-placeholder-icon"
										/>
										<span
											className={styles.customStatusPlaceholderText}
											data-flx="app.floating.user-area-popout.custom-status-placeholder-text"
										>
											<Trans>Set a custom status</Trans>
										</span>
									</button>
								</FocusRing>
							)}
						</div>
						<UserProfilePreviewBio
							profile={profile}
							profileData={profileData}
							onShowMore={openUserProfile}
							data-flx="app.floating.user-area-popout.user-profile-preview-bio"
						/>
					</ProfileCardContent>
					<ProfileCardFooter data-flx="app.floating.user-area-popout.profile-card-footer">
						<div className={styles.footer} data-flx="app.floating.user-area-popout.footer">
							<div className={styles.actionGroup} data-flx="app.floating.user-area-popout.action-group">
								<Popout
									hoverDelay={0}
									hoverCloseDelay={120}
									position="right-start"
									preventInvert
									offsetMainAxis={8}
									animationType="none"
									render={({onClose}) => (
										<StatusMenu
											onSelectStatus={handleStatusChange}
											onClose={onClose}
											data-flx="app.floating.user-area-popout.status-menu"
										/>
									)}
									data-flx="app.floating.user-area-popout.popout"
								>
									<ActionButton
										icon={
											<StatusIndicator
												status={status}
												size={14}
												data-flx="app.floating.user-area-popout.status-indicator"
											/>
										}
										label={getStatusTypeLabel(i18n, status)}
										chevron
										data-flx="app.floating.user-area-popout.action-button"
									/>
								</Popout>
								<div className={styles.actionDivider} data-flx="app.floating.user-area-popout.action-divider" />
								<Popout
									hoverDelay={0}
									hoverCloseDelay={120}
									position="right-start"
									preventInvert
									offsetMainAxis={8}
									animationType="none"
									render={({onClose}) => (
										<SwitchAccountsMenu
											accounts={accountLogic.accounts}
											currentAccountId={accountLogic.currentAccount?.userId ?? null}
											onSelect={(userId) => {
												accountLogic.handleSwitchAccount(userId);
												PopoutCommands.close();
											}}
											onManage={openManageAccounts}
											onClose={onClose}
											data-flx="app.floating.user-area-popout.switch-accounts-menu"
										/>
									)}
									data-flx="app.floating.user-area-popout.popout--2"
								>
									<ActionButton
										icon={
											<UsersThreeIcon
												size={remFromPx(16)}
												weight="bold"
												data-flx="app.floating.user-area-popout.users-three-icon"
											/>
										}
										label={<Trans>Switch accounts</Trans>}
										onClick={openManageAccounts}
										chevron
										data-flx="app.floating.user-area-popout.action-button.open-manage-accounts"
									/>
								</Popout>
								<div className={styles.actionDivider} data-flx="app.floating.user-area-popout.action-divider--2" />
								<ActionButton
									icon={
										<IdentificationBadgeIcon
											size={remFromPx(16)}
											weight="bold"
											data-flx="app.floating.user-area-popout.identification-badge-icon"
										/>
									}
									label={<Trans>Copy user ID</Trans>}
									onClick={handleCopyUserId}
									data-flx="app.floating.user-area-popout.action-button.copy-user-id"
								/>
								{UserSettings.developerMode && (
									<>
										<div className={styles.actionDivider} data-flx="app.floating.user-area-popout.action-divider--3" />
										<ActionButton
											icon={
												<InfoIcon
													size={remFromPx(16)}
													weight="bold"
													data-flx="app.floating.user-area-popout.info-icon"
												/>
											}
											label={<Trans>Copy build info</Trans>}
											onClick={handleCopyBuildInfo}
											data-flx="app.floating.user-area-popout.action-button.copy-build-info"
										/>
										<div className={styles.actionDivider} data-flx="app.floating.user-area-popout.action-divider--4" />
										<ActionButton
											icon={
												<ChartLineIcon
													size={remFromPx(16)}
													weight="bold"
													data-flx="app.floating.user-area-popout.chart-line-icon"
												/>
											}
											label={<Trans>Copy voice diagnostics</Trans>}
											hint={MediaEngine.connected ? <Trans>In a call</Trans> : undefined}
											onClick={handleCopyStats}
											data-flx="app.floating.user-area-popout.action-button.copy-voice-diagnostics"
										/>
									</>
								)}
							</div>
							{currentUser.isClaimed() && (
								<Button
									variant="primary"
									fitContainer
									leftIcon={
										<PencilIcon
											size={remFromPx(16)}
											weight="bold"
											data-flx="app.floating.user-area-popout.pencil-icon"
										/>
									}
									onClick={openEditProfile}
									className={styles.editProfileButton}
									data-flx="app.floating.user-area-popout.edit-profile-button.open-edit-profile"
								>
									<Trans>Edit profile</Trans>
								</Button>
							)}
						</div>
					</ProfileCardFooter>
				</ProfileCardLayout>
			</div>
		</FocusRingScope>
	);
});
