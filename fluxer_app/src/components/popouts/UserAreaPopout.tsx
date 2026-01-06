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
import {
	CaretRightIcon,
	CheckIcon,
	CopyIcon,
	GearIcon,
	IdentificationBadgeIcon,
	PencilIcon,
	SmileyIcon,
	UsersThreeIcon,
} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as PopoutActionCreators from '~/actions/PopoutActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import {DEFAULT_ACCENT_COLOR, getStatusTypeLabel, StatusTypes} from '~/Constants';
import {getAccountAvatarUrl} from '~/components/accounts/AccountListItem';
import AccountSwitcherModal from '~/components/accounts/AccountSwitcherModal';
import {CustomStatusDisplay} from '~/components/common/CustomStatusDisplay/CustomStatusDisplay';
import {CustomStatusModal} from '~/components/modals/CustomStatusModal';
import {UserProfileModal} from '~/components/modals/UserProfileModal';
import {UserSettingsModal} from '~/components/modals/UserSettingsModal';
import styles from '~/components/popouts/UserAreaPopout.module.css';
import {UserProfileBadges} from '~/components/popouts/UserProfileBadges';
import userProfilePopoutStyles from '~/components/popouts/UserProfilePopout.module.css';
import {UserProfileBio} from '~/components/popouts/UserProfileShared';
import {ProfileCardBanner} from '~/components/profile/ProfileCard/ProfileCardBanner';
import {ProfileCardContent} from '~/components/profile/ProfileCard/ProfileCardContent';
import {ProfileCardFooter} from '~/components/profile/ProfileCard/ProfileCardFooter';
import {ProfileCardLayout} from '~/components/profile/ProfileCard/ProfileCardLayout';
import {ProfileCardUserInfo} from '~/components/profile/ProfileCard/ProfileCardUserInfo';
import {Button} from '~/components/uikit/Button/Button';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import FocusRingScope from '~/components/uikit/FocusRing/FocusRingScope';
import {MockAvatar} from '~/components/uikit/MockAvatar';
import {Popout} from '~/components/uikit/Popout/Popout';
import {StatusIndicator} from '~/components/uikit/StatusIndicator';
import {Tooltip} from '~/components/uikit/Tooltip';
import {useAutoplayExpandedProfileAnimations} from '~/hooks/useAutoplayExpandedProfileAnimations';
import {normalizeCustomStatus} from '~/lib/customStatus';
import type {AccountSummary} from '~/stores/AccountManager';
import PresenceStore from '~/stores/PresenceStore';
import StatusExpiryStore from '~/stores/StatusExpiryStore';
import UserProfileStore from '~/stores/UserProfileStore';
import UserStore from '~/stores/UserStore';
import {useAccountSwitcherLogic} from '~/utils/accounts/AccountSwitcherModalUtils';
import * as ColorUtils from '~/utils/ColorUtils';
import * as NicknameUtils from '~/utils/NicknameUtils';
import * as ProfileDisplayUtils from '~/utils/ProfileDisplayUtils';
import {createMockProfile} from '~/utils/ProfileUtils';

const STATUS_ORDER = [StatusTypes.ONLINE, StatusTypes.IDLE, StatusTypes.DND, StatusTypes.INVISIBLE];

const EXPIRY_OPTIONS = [
	{id: '15m', label: <Trans>For 15 minutes</Trans>, durationMs: 15 * 60 * 1000},
	{id: '1h', label: <Trans>For 1 Hour</Trans>, durationMs: 60 * 60 * 1000},
	{id: '8h', label: <Trans>For 8 Hours</Trans>, durationMs: 8 * 60 * 60 * 1000},
	{id: '24h', label: <Trans>For 24 Hours</Trans>, durationMs: 24 * 60 * 60 * 1000},
	{id: '3d', label: <Trans>For 3 days</Trans>, durationMs: 3 * 24 * 60 * 60 * 1000},
	{id: 'forever', label: <Trans>Forever</Trans>, durationMs: null},
];

const STATUS_DESCRIPTIONS: Record<(typeof STATUS_ORDER)[number], React.ReactNode | null> = {
	[StatusTypes.ONLINE]: null,
	[StatusTypes.IDLE]: null,
	[StatusTypes.DND]: <Trans>You won&apos;t receive notifications on desktop</Trans>,
	[StatusTypes.INVISIBLE]: <Trans>You&apos;ll appear offline</Trans>,
};

interface StatusMenuProps {
	onSelectStatus: (status: (typeof STATUS_ORDER)[number], durationMs: number | null) => void;
	onClose: () => void;
}

const StatusMenu = observer(({onSelectStatus, onClose}: StatusMenuProps) => {
	const {i18n} = useLingui();

	const handleSelect = (status: (typeof STATUS_ORDER)[number], durationMs: number | null) => {
		onSelectStatus(status, durationMs);
		onClose();
		PopoutActionCreators.close();
	};

	return (
		<div className={styles.statusMenu}>
			{STATUS_ORDER.map((status) => {
				const hasExpiryOptions = status !== StatusTypes.ONLINE;
				const description = STATUS_DESCRIPTIONS[status];

				const rowContent = (
					<FocusRing offset={-2}>
						<button type="button" className={styles.statusMenuItem} onClick={() => handleSelect(status, null)}>
							<div className={styles.statusMenuIcon}>
								<StatusIndicator status={status} size={14} monochromeColor="var(--brand-primary-fill)" />
							</div>
							<div className={styles.statusMenuText}>
								<span className={styles.statusMenuLabel}>{getStatusTypeLabel(i18n, status)}</span>
								{description && <span className={styles.statusMenuDescription}>{description}</span>}
							</div>
							{hasExpiryOptions && <CaretRightIcon size={14} weight="bold" className={styles.statusMenuChevron} />}
						</button>
					</FocusRing>
				);

				if (!hasExpiryOptions) {
					return (
						<div key={status} className={styles.statusMenuRow}>
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
							<div className={styles.expiryPopup}>
								{EXPIRY_OPTIONS.map((option) => (
									<FocusRing key={option.id} offset={-2}>
										<button
											type="button"
											className={styles.expiryItem}
											onClick={() => {
												handleSelect(status, option.durationMs);
												closeExpiry();
											}}
										>
											<span className={styles.expiryLabel}>{option.label}</span>
										</button>
									</FocusRing>
								))}
							</div>
						)}
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
		<FocusRing offset={-2}>
			<button
				type="button"
				ref={ref}
				disabled={disabled}
				onClick={onClick}
				className={clsx(styles.actionButton, className, disabled && styles.actionButtonDisabled)}
				{...rest}
			>
				<div className={styles.actionIcon} aria-hidden="true">
					{icon}
				</div>
				<div className={styles.actionContent}>
					<span className={styles.actionLabel}>{label}</span>
					{hint && <span className={styles.actionHint}>{hint}</span>}
				</div>
				{chevron && <CaretRightIcon size={14} weight="bold" className={styles.actionChevron} aria-hidden="true" />}
			</button>
		</FocusRing>
	),
);

ActionButton.displayName = 'ActionButton';

interface SwitchAccountsMenuProps {
	accounts: Array<AccountSummary>;
	currentAccountId: string | null;
	onSelect: (userId: string) => void;
	onManage: () => void;
	onClose: () => void;
}

const SwitchAccountsMenu = observer(
	({accounts, currentAccountId, onSelect, onManage, onClose}: SwitchAccountsMenuProps) => {
		return (
			<div className={styles.switchMenu}>
				<div className={styles.switchMenuList}>
					{accounts.map((account) => {
						const isCurrent = account.userId === currentAccountId;
						const avatarUrl = getAccountAvatarUrl(account);
						const username = account.userData?.username ?? account.userId;
						const discriminator = account.userData?.discriminator ?? '0000';

						return (
							<FocusRing key={account.userId} offset={-2}>
								<button
									type="button"
									className={styles.accountMenuItem}
									onClick={() => {
										if (!isCurrent) {
											onSelect(account.userId);
										}
										onClose();
										PopoutActionCreators.close();
									}}
								>
									<div className={styles.accountMenuAvatar}>
										<MockAvatar size={24} avatarUrl={avatarUrl} userTag={username} />
									</div>
									<div className={styles.accountMenuInfo}>
										<span className={styles.accountMenuTag}>
											{username}
											<span className={styles.accountMenuDiscriminator}>#{discriminator}</span>
										</span>
										{isCurrent && (
											<span className={styles.accountMenuMeta}>
												<Trans>Active account</Trans>
											</span>
										)}
									</div>
									{isCurrent && (
										<div className={styles.accountMenuCheck}>
											<CheckIcon size={10} weight="bold" />
										</div>
									)}
								</button>
							</FocusRing>
						);
					})}
				</div>
				<div className={styles.switchMenuFooter}>
					<FocusRing offset={-2}>
						<button
							type="button"
							className={styles.manageAccountsButton}
							onClick={() => {
								onClose();
								onManage();
							}}
						>
							<GearIcon size={16} weight="bold" />
							<Trans>Manage Accounts</Trans>
						</button>
					</FocusRing>
				</div>
			</div>
		);
	},
);

export const UserAreaPopout = observer(() => {
	const {t, i18n} = useLingui();
	const accountLogic = useAccountSwitcherLogic();
	const currentUser = UserStore.getCurrentUser();
	const currentUserId = currentUser?.id ?? null;
	const status = currentUserId ? PresenceStore.getStatus(currentUserId) : StatusTypes.ONLINE;

	const openEditProfile = React.useCallback(() => {
		ModalActionCreators.push(modal(() => <UserSettingsModal initialTab="my_profile" />));
		PopoutActionCreators.close();
	}, []);

	const openUserProfile = React.useCallback(() => {
		if (!currentUserId) {
			return;
		}
		ModalActionCreators.push(modal(() => <UserProfileModal userId={currentUserId} />));
		PopoutActionCreators.close();
	}, [currentUserId]);

	const openCustomStatus = React.useCallback(() => {
		ModalActionCreators.push(modal(() => <CustomStatusModal />));
		PopoutActionCreators.close();
	}, []);

	const handleStatusChange = React.useCallback(
		(statusType: (typeof STATUS_ORDER)[number], durationMs: number | null) => {
			StatusExpiryStore.setActiveStatusExpiry({
				status: statusType,
				durationMs,
			});
		},
		[],
	);

	const handleCopyUserId = React.useCallback(() => {
		if (!currentUserId) {
			return;
		}
		TextCopyActionCreators.copy(i18n, currentUserId);
	}, [currentUserId, i18n]);

	const handleCopyUserTag = React.useCallback(() => {
		if (!currentUser) {
			return;
		}
		TextCopyActionCreators.copy(i18n, currentUser.tag, true);
	}, [currentUser, i18n]);

	const openManageAccounts = React.useCallback(() => {
		ModalActionCreators.push(modal(() => <AccountSwitcherModal />));
		PopoutActionCreators.close();
	}, []);

	const profile = React.useMemo(() => {
		if (!currentUser) {
			return null;
		}
		return UserProfileStore.getProfile(currentUser.id) ?? createMockProfile(currentUser);
	}, [currentUserId, currentUser]);

	const profileData = React.useMemo(() => profile?.getEffectiveProfile() ?? null, [profile]);

	const profileContext = React.useMemo<ProfileDisplayUtils.ProfileDisplayContext | null>(() => {
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

	const {avatarUrl, hoverAvatarUrl} = React.useMemo(() => {
		if (!profileContext) {
			return {avatarUrl: null, hoverAvatarUrl: null};
		}
		return ProfileDisplayUtils.getProfileAvatarUrls(profileContext);
	}, [profileContext]);

	const shouldAutoplayProfileAnimations = useAutoplayExpandedProfileAnimations();
	const bannerUrl = React.useMemo(() => {
		if (!profileContext) {
			return null;
		}
		return ProfileDisplayUtils.getProfileBannerUrl(profileContext, undefined, shouldAutoplayProfileAnimations) as
			| string
			| null;
	}, [profileContext, shouldAutoplayProfileAnimations]);

	const rawAccentColor = profileData?.accent_color ?? null;
	const accentColorHex = typeof rawAccentColor === 'number' ? ColorUtils.int2hex(rawAccentColor) : rawAccentColor;
	const borderColor = accentColorHex || DEFAULT_ACCENT_COLOR;
	const bannerColor = accentColorHex || DEFAULT_ACCENT_COLOR;

	const displayName = currentUser ? NicknameUtils.getNickname(currentUser) : '';
	const customStatus = currentUserId ? PresenceStore.getCustomStatus(currentUserId) : null;
	const hasCustomStatus = Boolean(normalizeCustomStatus(customStatus));

	const popoutContainerRef = React.useRef<HTMLDivElement | null>(null);

	if (!currentUser || !profile) {
		return null;
	}

	return (
		<FocusRingScope containerRef={popoutContainerRef}>
			<div ref={popoutContainerRef} className={styles.container}>
				<ProfileCardLayout borderColor={borderColor}>
					<ProfileCardBanner
						bannerUrl={bannerUrl}
						bannerColor={bannerColor}
						user={currentUser}
						avatarUrl={avatarUrl}
						hoverAvatarUrl={hoverAvatarUrl}
						disablePresence={false}
						isClickable={true}
						onAvatarClick={openUserProfile}
					/>

					<UserProfileBadges user={currentUser} profile={profile} />

					<ProfileCardContent isWebhook={false}>
						<ProfileCardUserInfo
							displayName={displayName}
							user={currentUser}
							pronouns={profileData?.pronouns}
							showUsername={true}
							isClickable={false}
							isWebhook={false}
							usernameActions={
								<Tooltip text={t`Copy Username`} position="top">
									<FocusRing offset={-2}>
										<button
											type="button"
											className={styles.copyUsernameButton}
											onClick={handleCopyUserTag}
											aria-label={t`Copy Username`}
										>
											<CopyIcon size={14} weight="fill" />
										</button>
									</FocusRing>
								</Tooltip>
							}
						/>

						<div className={styles.customStatusRow}>
							{hasCustomStatus ? (
								<CustomStatusDisplay
									customStatus={customStatus}
									className={userProfilePopoutStyles.profileCustomStatusText}
									allowJumboEmoji
									maxLines={0}
									isEditable={true}
									onEdit={openCustomStatus}
									alwaysAnimate={shouldAutoplayProfileAnimations}
								/>
							) : (
								<FocusRing offset={-2}>
									<button type="button" className={styles.customStatusPlaceholder} onClick={openCustomStatus}>
										<SmileyIcon size={14} weight="regular" className={styles.customStatusPlaceholderIcon} />
										<span className={styles.customStatusPlaceholderText}>
											<Trans>Set a custom status</Trans>
										</span>
									</button>
								</FocusRing>
							)}
						</div>

						<UserProfileBio profile={profile} profileData={profileData} />
					</ProfileCardContent>

					<ProfileCardFooter>
						<div className={styles.footer}>
							<div className={styles.actionGroup}>
								<Popout
									hoverDelay={0}
									hoverCloseDelay={120}
									position="right-start"
									preventInvert
									toggleClose={false}
									offsetMainAxis={8}
									animationType="none"
									render={({onClose}) => <StatusMenu onSelectStatus={handleStatusChange} onClose={onClose} />}
								>
									<ActionButton
										icon={<StatusIndicator status={status} size={14} />}
										label={getStatusTypeLabel(i18n, status)}
										chevron
									/>
								</Popout>

								<div className={styles.actionDivider} />

								<Popout
									hoverDelay={0}
									hoverCloseDelay={120}
									position="right-start"
									preventInvert
									toggleClose={false}
									offsetMainAxis={8}
									animationType="none"
									render={({onClose}) => (
										<SwitchAccountsMenu
											accounts={accountLogic.accounts}
											currentAccountId={accountLogic.currentAccount?.userId ?? null}
											onSelect={(userId) => {
												accountLogic.handleSwitchAccount(userId);
												PopoutActionCreators.close();
											}}
											onManage={openManageAccounts}
											onClose={onClose}
										/>
									)}
								>
									<ActionButton
										icon={<UsersThreeIcon size={16} weight="bold" />}
										label={<Trans>Switch Accounts</Trans>}
										onClick={openManageAccounts}
										chevron
									/>
								</Popout>

								<div className={styles.actionDivider} />

								<ActionButton
									icon={<IdentificationBadgeIcon size={16} weight="bold" />}
									label={<Trans>Copy User ID</Trans>}
									onClick={handleCopyUserId}
								/>
							</div>

							<Button
								variant="primary"
								fitContainer={true}
								leftIcon={<PencilIcon size={16} weight="bold" />}
								onClick={openEditProfile}
								className={styles.editProfileButton}
							>
								<Trans>Edit Profile</Trans>
							</Button>
						</div>
					</ProfileCardFooter>
				</ProfileCardLayout>
			</div>
		</FocusRingScope>
	);
});
