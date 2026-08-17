// SPDX-License-Identifier: AGPL-3.0-or-later

import {showDmActionErrorModal} from '@app/features/app/components/alerts/DmActionErrorModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {CustomStatusDisplay} from '@app/features/app/components/shared/custom_status_display/CustomStatusDisplay';
import {useHover} from '@app/features/app/hooks/useHover';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Authentication from '@app/features/auth/state/Authentication';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {
	BLOCKED_USER_DM_WARNING_DESCRIPTOR,
	OPEN_DM_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import GuildMembers from '@app/features/member/state/GuildMembers';
import Permission from '@app/features/permissions/state/Permission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {usePresenceCustomStatus} from '@app/features/presence/hooks/usePresenceCustomStatus';
import MemberPresenceSubscription from '@app/features/presence/state/MemberPresenceSubscription';
import Relationships from '@app/features/relationship/state/Relationships';
import {
	getUserMenuAvatarUrl,
	getUserMenuBannerUrl,
	UserImageMenuItems,
} from '@app/features/ui/action_menu/items/UserImageMenuItems';
import {Button} from '@app/features/ui/button/Button';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import FocusRingScope from '@app/features/ui/focus_ring/FocusRingScope';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import {LimitedProfileNotice} from '@app/features/user/components/popouts/LimitedProfileNotice';
import {UserProfileBadges} from '@app/features/user/components/popouts/UserProfileBadges';
import {UserProfileDataWarning} from '@app/features/user/components/popouts/UserProfileDataWarning';
import styles from '@app/features/user/components/popouts/UserProfilePopout.module.css';
import {
	UserProfileConnections,
	UserProfileMembershipInfo,
	UserProfilePreviewBio,
	UserProfileRoles,
	UserProfileTimezoneInfo,
} from '@app/features/user/components/popouts/UserProfileShared';
import {ProfileCardActions} from '@app/features/user/components/profile/profile_card/ProfileCardActions';
import {ProfileCardBanner} from '@app/features/user/components/profile/profile_card/ProfileCardBanner';
import {ProfileCardContent} from '@app/features/user/components/profile/profile_card/ProfileCardContent';
import {ProfileCardFooter} from '@app/features/user/components/profile/profile_card/ProfileCardFooter';
import {ProfileCardLayout} from '@app/features/user/components/profile/profile_card/ProfileCardLayout';
import {ProfileCardUserInfo} from '@app/features/user/components/profile/profile_card/ProfileCardUserInfo';
import {UserProfileLoadingSkeleton} from '@app/features/user/components/profile/UserProfileLoadingSkeleton';
import {useProfileCardDisplayState} from '@app/features/user/components/profile/useProfileCardDisplayState';
import {VoiceActivitySection} from '@app/features/user/components/profile/VoiceActivitySection';
import {PROFILE_POPOUT_GEOMETRY_STYLE} from '@app/features/user/constants/UserProfileSurfaceGeometry';
import {useAutoplayExpandedProfileAnimations} from '@app/features/user/hooks/useAutoplayExpandedProfileAnimations';
import {useUserProfileSurfaceState} from '@app/features/user/hooks/useUserProfileSurfaceState';
import type {User} from '@app/features/user/models/User';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {resolveProfileGuildMembership} from '@app/features/user/utils/ProfileGuildMembership';
import {createMockProfile} from '@app/features/user/utils/ProfileUtils';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MEDIA_PROXY_PROFILE_BANNER_SIZE_POPOUT} from '@fluxer/constants/src/MediaProxyAssetSizes';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ChatTeardropIcon, PencilIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef} from 'react';

const YOU_CAN_T_MESSAGE_YOURSELF_DESCRIPTOR = msg({
	message: "You can't message yourself",
	comment: 'Error message in the user profile popout.',
});
const logger = new Logger('UserProfilePopout');

interface UserProfilePopoutProps {
	popoutKey: string | number;
	user: User;
	isWebhook: boolean;
	guildId?: string;
	guildMember?: GuildMember | null;
	isPreview?: boolean;
	onClose?: () => void;
}

export const UserProfilePopout: React.FC<UserProfilePopoutProps> = observer(
	({popoutKey, user, isWebhook, guildId, guildMember: providedGuildMember, isPreview, onClose}) => {
		const {i18n} = useLingui();
		const [hoverRef, isHovering] = useHover();
		const storedGuildMember = guildId ? GuildMembers.getMember(guildId, user.id) : null;
		const profileGuildId = isWebhook ? undefined : guildId;
		const fallbackProfile = useMemo(() => createMockProfile(user), [user]);
		const handleProfileLoadError = useCallback((error: unknown) => {
			logger.error('Failed to fetch profile for user popout', error);
		}, []);
		const {profile, profileLoadError, showProfileSkeleton} = useUserProfileSurfaceState({
			userId: user.id,
			guildId: profileGuildId,
			enabled: !isWebhook,
			fallbackProfile,
			onError: handleProfileLoadError,
		});
		const showProfileDataWarning =
			!showProfileSkeleton && !isWebhook && (profileLoadError || DeveloperOptions.forceProfileDataWarning);
		const profileMembership = resolveProfileGuildMembership(profile, {
			fallbackGuildId: guildId,
			userId: user.id,
			allowStoreFallback: true,
		});
		const profileGuildMember = profileMembership.kind === 'guildMember' ? profileMembership.member : null;
		const guildMember =
			profile?.guildId && profile.guildId === guildId
				? (profileGuildMember ?? storedGuildMember ?? providedGuildMember)
				: profileGuildMember;
		const memberRoles =
			profileMembership.kind === 'guildMember' && profileMembership.member === guildMember
				? profileMembership.roles
				: (guildMember?.getSortedRoles() ?? []);
		const canManageRoles = Permission.can(Permissions.MANAGE_ROLES, {guildId});
		const isCurrentUser = user.id === Authentication.currentUserId;
		const relationshipType = Relationships.getRelationship(user.id)?.type;
		const isBlocked = relationshipType === RelationshipTypes.BLOCKED;
		const directMessagesDisabled = RuntimeConfig.directMessagesDisabled;
		const showProfileFooter = !isWebhook && (isCurrentUser || !directMessagesDisabled);
		const requestClose = useCallback(() => {
			if (onClose) {
				onClose();
				return;
			}
			PopoutCommands.close(popoutKey);
		}, [onClose, popoutKey]);
		const openFullProfile = useCallback(
			(autoFocusNote?: boolean) => {
				if (isWebhook) {
					return;
				}
				UserProfileCommands.openUserProfile(user.id, guildId, autoFocusNote);
				requestClose();
			},
			[isWebhook, user.id, guildId, requestClose],
		);
		const handleOpenFullProfile = useCallback(() => {
			openFullProfile();
		}, [openFullProfile]);
		const handleOpenFullProfileNote = useCallback(() => {
			openFullProfile(true);
		}, [openFullProfile]);
		useEffect(() => {
			if (!guildId || !user.id || isWebhook) {
				return;
			}
			if (providedGuildMember) {
				MemberPresenceSubscription.touchMember(guildId, user.id);
				return () => {
					MemberPresenceSubscription.unsubscribe(guildId, user.id);
				};
			}
			const hasMember = GuildMembers.getMember(guildId, user.id);
			if (!hasMember) {
				MemberPresenceSubscription.touchMember(guildId, user.id);
				GuildMembers.fetchMembers(guildId, {userIds: [user.id]}).catch((error) => {
					logger.error('Failed to fetch guild member', error);
				});
			} else {
				MemberPresenceSubscription.touchMember(guildId, user.id);
			}
			return () => {
				MemberPresenceSubscription.unsubscribe(guildId, user.id);
			};
		}, [guildId, user.id, isWebhook, providedGuildMember]);
		const handleClosePopout = useCallback(() => {
			requestClose();
		}, [requestClose]);
		const displayName = guildMember?.nick
			? NicknameUtils.formatNicknameForStreamerMode(guildMember.nick)
			: NicknameUtils.getNickname(user, guildId);
		const handleMessage = async () => {
			try {
				requestClose();
				await PrivateChannelCommands.openDMChannel(user.id);
			} catch (error) {
				logger.error('Failed to open DM channel', error);
				showDmActionErrorModal(error);
			}
		};
		const handleOpenBlockedDm = () => {
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(OPEN_DM_DESCRIPTOR)}
						description={i18n._(BLOCKED_USER_DM_WARNING_DESCRIPTOR, {userName: displayName})}
						primaryText={i18n._(OPEN_DM_DESCRIPTOR)}
						primaryVariant="primary"
						onPrimary={handleMessage}
						data-flx="user.user-profile-popout.handle-open-blocked-dm.confirm-modal"
					/>
				)),
			);
		};
		const handleEditProfile = () => {
			ModalCommands.push(
				modal(() => (
					<UserSettingsModal
						initialTab="my_profile"
						data-flx="user.user-profile-popout.handle-edit-profile.user-settings-modal"
					/>
				)),
			);
			requestClose();
		};
		const {profileContext, avatarUrl, hoverAvatarUrl, bannerUrl, hoverBannerUrl, accentColor, profileData} =
			useProfileCardDisplayState({
				user,
				profile,
				guildId: profile?.guildId,
				guildMember,
				guildMemberProfile: profile?.guildMemberProfile,
				bannerSize: MEDIA_PROXY_PROFILE_BANNER_SIZE_POPOUT,
			});
		const avatarMenuUrl = getUserMenuAvatarUrl({user, profile, profileContext});
		const bannerMenuUrl = getUserMenuBannerUrl({user, profile, profileContext});
		const handleAvatarContextMenu = useCallback(
			(event: React.MouseEvent) => {
				if (isWebhook || !avatarMenuUrl) return;
				ContextMenuCommands.openFromEvent(event, ({onClose}) => (
					<UserImageMenuItems
						user={user}
						profile={profile}
						profileContext={profileContext}
						onClose={onClose}
						variant="avatar"
						data-flx="user.user-profile-popout.handle-avatar-context-menu.user-image-menu-items"
					/>
				));
			},
			[isWebhook, avatarMenuUrl, user, profile, profileContext],
		);
		const handleBannerContextMenu = useCallback(
			(event: React.MouseEvent) => {
				if (isWebhook || !bannerMenuUrl) return;
				ContextMenuCommands.openFromEvent(event, ({onClose}) => (
					<UserImageMenuItems
						user={user}
						profile={profile}
						profileContext={profileContext}
						onClose={onClose}
						variant="banner"
						data-flx="user.user-profile-popout.handle-banner-context-menu.user-image-menu-items"
					/>
				));
			},
			[isWebhook, bannerMenuUrl, user, profile, profileContext],
		);
		const shouldAutoplayProfileAnimations = useAutoplayExpandedProfileAnimations();
		const presenceCustomStatus = usePresenceCustomStatus({userId: user.id, enabled: !isWebhook});
		const popoutContainerRef = useRef<HTMLDivElement | null>(null);
		if (showProfileSkeleton) {
			return (
				<FocusRingScope containerRef={popoutContainerRef} data-flx="user.user-profile-popout.skeleton.focus-ring-scope">
					<div ref={popoutContainerRef} data-flx="user.user-profile-popout.skeleton.container">
						<UserProfileLoadingSkeleton
							variant="popout"
							borderColor={accentColor}
							data-flx="user.user-profile-popout.user-profile-loading-skeleton"
						/>
					</div>
				</FocusRingScope>
			);
		}
		const borderColor = accentColor;
		const bannerColor = accentColor;
		return (
			<FocusRingScope containerRef={popoutContainerRef} data-flx="user.user-profile-popout.focus-ring-scope">
				<div ref={popoutContainerRef} data-flx="user.user-profile-popout.div">
					<ProfileCardLayout
						borderColor={borderColor}
						hoverRef={hoverRef}
						className={styles.profilePopoutCard}
						style={PROFILE_POPOUT_GEOMETRY_STYLE}
						data-flx="user.user-profile-popout.profile-card-layout"
					>
						<ProfileCardBanner
							bannerUrl={bannerUrl as string | null}
							hoverBannerUrl={hoverBannerUrl}
							bannerColor={bannerColor}
							user={user}
							avatarUrl={avatarUrl}
							hoverAvatarUrl={hoverAvatarUrl}
							disablePresence={isWebhook}
							isClickable={!isWebhook}
							onAvatarClick={!isWebhook ? handleOpenFullProfile : undefined}
							onAvatarContextMenu={handleAvatarContextMenu}
							onBannerContextMenu={handleBannerContextMenu}
							data-flx="user.user-profile-popout.profile-card-banner"
						/>
						{!isWebhook && (
							<UserProfileBadges
								user={user}
								profile={profile}
								data-flx="user.user-profile-popout.user-profile-badges"
							/>
						)}
						<ProfileCardContent isWebhook={isWebhook} data-flx="user.user-profile-popout.profile-card-content">
							{showProfileDataWarning && (
								<div className={styles.profileDataWarning} data-flx="user.user-profile-popout.profile-data-warning">
									<UserProfileDataWarning data-flx="user.user-profile-popout.user-profile-data-warning" />
								</div>
							)}
							{!isWebhook && profile?.profileLimited && (
								<div className={styles.profileDataWarning} data-flx="user.user-profile-popout.profile-limited-notice">
									<LimitedProfileNotice data-flx="user.user-profile-popout.limited-profile-notice" />
								</div>
							)}
							<ProfileCardUserInfo
								displayName={displayName}
								displayNameClassName={styles.profileDisplayName}
								user={user}
								pronouns={profileData?.pronouns}
								showUsername={!isWebhook}
								isClickable={!isWebhook}
								isWebhook={isWebhook}
								onDisplayNameClick={!isWebhook ? handleOpenFullProfile : undefined}
								onUsernameClick={!isWebhook ? handleOpenFullProfile : undefined}
								actions={
									!isWebhook && (
										<ProfileCardActions
											userId={user.id}
											isHovering={isHovering}
											onNoteClick={handleOpenFullProfileNote}
											data-flx="user.user-profile-popout.profile-card-actions"
										/>
									)
								}
								data-flx="user.user-profile-popout.profile-card-user-info"
							/>
							{!isWebhook && presenceCustomStatus && (
								<div className={styles.profileCustomStatus} data-flx="user.user-profile-popout.profile-custom-status">
									<CustomStatusDisplay
										customStatus={presenceCustomStatus}
										className={styles.profileCustomStatusText}
										allowJumboEmoji
										maxLines={0}
										alwaysAnimate={shouldAutoplayProfileAnimations}
										data-flx="user.user-profile-popout.profile-custom-status-text"
									/>
								</div>
							)}
							{!isWebhook && (
								<VoiceActivitySection
									userId={user.id}
									onNavigate={handleClosePopout}
									data-flx="user.user-profile-popout.voice-activity-section"
								/>
							)}
							{profile && (
								<UserProfilePreviewBio
									profile={profile}
									profileData={profileData ?? null}
									onShowMore={handleOpenFullProfile}
									data-flx="user.user-profile-popout.user-profile-preview-bio"
								/>
							)}
							{profile && (
								<UserProfileTimezoneInfo
									profile={profile}
									data-flx="user.user-profile-popout.user-profile-timezone-info"
								/>
							)}
							{profile && (
								<UserProfileMembershipInfo
									profile={profile}
									user={user}
									data-flx="user.user-profile-popout.user-profile-membership-info"
								/>
							)}
							{!isWebhook && profile && (
								<UserProfileRoles
									profile={profile}
									user={user}
									memberRoles={[...memberRoles]}
									canManageRoles={canManageRoles}
									data-flx="user.user-profile-popout.user-profile-roles"
								/>
							)}
							{profile && (
								<UserProfileConnections
									profile={profile}
									variant="compact"
									data-flx="user.user-profile-popout.user-profile-connections"
								/>
							)}
						</ProfileCardContent>
						{showProfileFooter && (
							<ProfileCardFooter data-flx="user.user-profile-popout.profile-card-footer">
								{isCurrentUser ? (
									isPreview ? (
										<Tooltip
											text={i18n._(YOU_CAN_T_MESSAGE_YOURSELF_DESCRIPTOR)}
											maxWidth="xl"
											data-flx="user.user-profile-popout.tooltip"
										>
											<div data-flx="user.user-profile-popout.div--2">
												<Button
													small={true}
													fitContainer={true}
													leftIcon={
														<ChatTeardropIcon
															className={styles.iconSmall}
															data-flx="user.user-profile-popout.icon-small"
														/>
													}
													disabled={true}
													data-flx="user.user-profile-popout.button"
												>
													<Trans>Message</Trans>
												</Button>
											</div>
										</Tooltip>
									) : user.isClaimed() ? (
										<Button
											small={true}
											fitContainer={true}
											leftIcon={
												<PencilIcon className={styles.iconSmall} data-flx="user.user-profile-popout.icon-small--2" />
											}
											onClick={handleEditProfile}
											data-flx="user.user-profile-popout.button.edit-profile"
										>
											<Trans>Edit profile</Trans>
										</Button>
									) : null
								) : (
									<Button
										small={true}
										fitContainer={true}
										leftIcon={
											<ChatTeardropIcon
												className={styles.iconSmall}
												data-flx="user.user-profile-popout.icon-small--3"
											/>
										}
										onClick={isBlocked ? handleOpenBlockedDm : handleMessage}
										data-flx="user.user-profile-popout.button.open-blocked-dm"
									>
										{isBlocked ? i18n._(OPEN_DM_DESCRIPTOR) : <Trans>Message</Trans>}
									</Button>
								)}
							</ProfileCardFooter>
						)}
					</ProfileCardLayout>
				</div>
			</FocusRingScope>
		);
	},
);
