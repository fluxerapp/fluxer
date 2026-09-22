// SPDX-License-Identifier: AGPL-3.0-or-later

import {showDmActionErrorModal} from '@app/features/app/components/alerts/DmActionErrorModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Authentication from '@app/features/auth/state/Authentication';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {
	BLOCKED_USER_DM_WARNING_DESCRIPTOR,
	COPY_USER_ID_DESCRIPTOR,
	COPY_USERNAME_DESCRIPTOR,
	OPEN_DM_DESCRIPTOR,
	START_VIDEO_CALL_DESCRIPTOR,
	START_VOICE_CALL_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {BLOCK_DESCRIPTOR, REPORT_USER_DESCRIPTOR} from '@app/features/moderation/utils/ModerationMessageDescriptors';
import {openReportUserModal} from '@app/features/moderation/utils/ReportActionUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import MemberPresenceSubscription from '@app/features/presence/state/MemberPresenceSubscription';
import Relationships from '@app/features/relationship/state/Relationships';
import * as RelationshipActionUtils from '@app/features/relationship/utils/RelationshipActionUtils';
import {
	ACCEPT_FRIEND_REQUEST_DESCRIPTOR,
	ADD_FRIEND_DESCRIPTOR,
	CANCEL_FRIEND_REQUEST_DESCRIPTOR,
	REMOVE_FRIEND_DESCRIPTOR,
	UNBLOCK_USER_ACTION_DESCRIPTOR,
} from '@app/features/relationship/utils/RelationshipMessageDescriptors';
import {getUserAccentColor} from '@app/features/theme/utils/AccentColorUtils';
import type {ContextMenuActionEvent} from '@app/features/ui/action_menu/ContextMenu';
import {
	getUserMenuAvatarUrl,
	getUserMenuBannerUrl,
	UserImageMenuItems,
} from '@app/features/ui/action_menu/items/UserImageMenuItems';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {Button} from '@app/features/ui/button/Button';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import ContextMenu from '@app/features/ui/state/ContextMenu';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import userProfileModalStyles from '@app/features/user/components/modals/UserProfileModal.module.css';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import {ProfileModalContent} from '@app/features/user/components/modals/user_profile_modal/ProfileModalContent';
import {UserProfileLoadingSkeleton} from '@app/features/user/components/profile/UserProfileLoadingSkeleton';
import {useUserProfileSurfaceState} from '@app/features/user/hooks/useUserProfileSurfaceState';
import type {Profile} from '@app/features/user/models/Profile';
import {User} from '@app/features/user/models/User';
import UserNote from '@app/features/user/state/UserNote';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import type * as ProfileDisplayUtils from '@app/features/user/utils/ProfileDisplayUtils';
import {
	getProfileMembershipDisplayName,
	resolveProfileGuildMembership,
	toProfileDisplayContext,
} from '@app/features/user/utils/ProfileGuildMembership';
import {createMockProfile} from '@app/features/user/utils/ProfileUtils';
import * as CallUtils from '@app/features/voice/utils/CallUtils';
import {hasActiveDirectCallWithUser} from '@app/features/voice/utils/PrivateCallMenuUtils';
import {PublicUserFlags, RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {
	ChatTeardropIcon,
	CheckCircleIcon,
	ClockCounterClockwiseIcon,
	CopyIcon,
	DotsThreeIcon,
	FlagIcon,
	GlobeIcon,
	IdentificationCardIcon,
	PencilIcon,
	PhoneIcon,
	ProhibitIcon,
	UserMinusIcon,
	UserPlusIcon,
	VideoCameraIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const USER_PROFILE_DESCRIPTOR = msg({
	message: 'User profile',
	comment: 'Short label in the user profile modal. Keep it concise.',
});
const USER_PROFILE_2_DESCRIPTOR = msg({
	message: 'User profile: {tag}',
	comment: 'Short label in the user profile modal. Keep it concise. Preserve {tag}; it is inserted by code.',
});
const VIEW_COMMUNITY_PROFILE_DESCRIPTOR = msg({
	message: 'View community profile',
	comment: 'Button or menu action label in the user profile modal. Keep it concise.',
});
const VIEW_GLOBAL_PROFILE_DESCRIPTOR = msg({
	message: 'View global profile',
	comment: 'Button or menu action label in the user profile modal. Keep it concise.',
});
const YOU_CAN_T_BEFRIEND_YOURSELF_DESCRIPTOR = msg({
	message: "You can't befriend yourself",
	comment: 'Error message in the user profile modal.',
});
const YOU_CAN_T_MESSAGE_YOURSELF_DESCRIPTOR = msg({
	message: "You can't message yourself",
	comment: 'Error message in the user profile modal.',
});
const MORE_ACTIONS_DESCRIPTOR = msg({
	message: 'More actions',
	comment: 'Short label in the user profile modal. Keep it concise.',
});
const UNBLOCK_USER_DESCRIPTOR = msg({
	message: 'Unblock user',
	comment: 'Button or menu action label in the user profile modal. Keep it concise. Keep the tone plain and specific.',
});
const CLAIM_YOUR_ACCOUNT_TO_SEND_FRIEND_REQUESTS_DESCRIPTOR = msg({
	message: 'Claim your account to send friend requests.',
	comment: 'Description text in the user profile modal.',
});
const SEND_FRIEND_REQUEST_DESCRIPTOR = msg({
	message: 'Send friend request',
	comment: 'Button or menu action label in the user profile modal. Keep it concise.',
});
const logger = new Logger('UserProfileModal');

export interface UserProfileModalProps {
	userId: string;
	guildId?: string;
	autoFocusNote?: boolean;
	disableEditProfile?: boolean;
	previewOverrides?: ProfileDisplayUtils.ProfilePreviewOverrides;
	previewUser?: User;
	previewProfile?: Profile;
}

type UserProfileModalComponent = React.FC<UserProfileModalProps>;

export const UserProfileModal: UserProfileModalComponent = observer(
	({userId, guildId, autoFocusNote, disableEditProfile, previewOverrides, previewUser, previewProfile}) => {
		const {i18n} = useLingui();
		const storeUser = Users.getUser(userId);
		const user = previewUser ?? storeUser;
		const isPreview = previewUser !== undefined || previewProfile !== undefined;
		const fallbackUser = useMemo(
			() =>
				new User({
					id: userId,
					username: userId,
					discriminator: '0000',
					global_name: null,
					avatar: null,
					avatar_color: null,
					flags: 0,
				}),
			[userId],
		);
		const displayUser = user ?? fallbackUser;
		const fallbackProfile = useMemo(() => createMockProfile(fallbackUser), [fallbackUser]);
		const mockProfile = useMemo(() => (user ? createMockProfile(user) : null), [user]);
		const profileFallback = previewProfile ?? mockProfile ?? fallbackProfile;
		const handleProfileLoadError = useCallback((error: unknown) => {
			logger.error('Failed to fetch user profile:', error);
		}, []);
		const {profile, profileLoadError, showProfileSkeleton} = useUserProfileSurfaceState({
			userId,
			guildId,
			enabled: !isPreview,
			fallbackProfile: profileFallback,
			onError: handleProfileLoadError,
		});
		const [showGlobalProfile, setShowGlobalProfile] = useState(false);
		const userNote = UserNote.getUserNote(userId);
		const isCurrentUser = user?.id === Authentication.currentUserId;
		const relationship = Relationships.getRelationship(userId);
		const relationshipType = relationship?.type;
		const isBlocked = relationshipType === RelationshipTypes.BLOCKED;
		const hasActiveDirectCall = hasActiveDirectCallWithUser(userId);
		const isUserBot = user?.bot ?? false;
		const isFriendlyBot =
			isUserBot && (displayUser.flags & PublicUserFlags.FRIENDLY_BOT) === PublicUserFlags.FRIENDLY_BOT;
		const noteRef = useRef<HTMLTextAreaElement | null>(null);
		const moreOptionsButtonRef = useRef<HTMLButtonElement>(null);
		useEffect(() => {
			if (!guildId || !userId || isPreview) {
				return;
			}
			const hasMember = GuildMembers.getMember(guildId, userId);
			if (!hasMember) {
				MemberPresenceSubscription.touchMember(guildId, userId);
				GuildMembers.fetchMembers(guildId, {userIds: [userId]}).catch((error) => {
					logger.error('Failed to fetch guild member:', error);
				});
			} else {
				MemberPresenceSubscription.touchMember(guildId, userId);
			}
			return () => {
				MemberPresenceSubscription.unsubscribe(guildId, userId);
			};
		}, [guildId, userId, isPreview]);
		const hasGuildProfile = !!(profile?.guildId && profile?.guildMemberProfile);
		const shouldShowProfileDataWarning =
			!showProfileSkeleton && (profileLoadError || DeveloperOptions.forceProfileDataWarning);
		const displayProfile = useMemo((): Profile | null => {
			if (!profile) return null;
			if (showGlobalProfile && hasGuildProfile) {
				return profile.withUpdates({guild_member_profile: null}).withGuildId(null);
			}
			return profile;
		}, [profile, showGlobalProfile, hasGuildProfile]);
		const screenReaderLabel = useMemo(() => {
			if (!displayUser) return i18n._(USER_PROFILE_DESCRIPTOR);
			const tag = NicknameUtils.formatTagForStreamerMode(displayUser.tag);
			return i18n._(USER_PROFILE_2_DESCRIPTOR, {tag});
		}, [displayUser, i18n.locale]);
		const effectiveProfile: Profile | null = displayProfile ?? profile ?? profileFallback;
		const resolvedProfile: Profile = effectiveProfile ?? fallbackProfile;
		const displayMembership = resolveProfileGuildMembership(displayProfile, {
			fallbackGuildId: guildId,
			userId,
			allowStoreFallback: Boolean(displayProfile),
		});
		const displayUserName = getProfileMembershipDisplayName(
			displayUser,
			displayMembership,
			displayProfile?.guildId ?? guildId,
		);
		const displayProfileContext = useMemo<ProfileDisplayUtils.ProfileDisplayContext>(
			() =>
				toProfileDisplayContext({
					user: displayUser,
					profile: displayProfile,
					membership: displayMembership,
					guildMemberProfile: displayProfile?.guildMemberProfile,
				}),
			[displayUser, displayProfile, displayMembership],
		);
		const hasImageMenuItems = Boolean(
			getUserMenuAvatarUrl({
				user: displayUser,
				profile: displayProfile,
				profileContext: displayProfileContext,
				previewOverrides,
			}) ||
				getUserMenuBannerUrl({
					user: displayUser,
					profile: displayProfile,
					profileContext: displayProfileContext,
					previewOverrides,
				}),
		);
		const handleEditProfile = () => {
			ModalCommands.pop();
			ModalCommands.push(
				modal(() => (
					<UserSettingsModal
						initialTab="my_profile"
						data-flx="user.user-profile-modal.handle-edit-profile.user-settings-modal"
					/>
				)),
			);
		};
		const handleMessage = async () => {
			try {
				ModalCommands.pop();
				await PrivateChannelCommands.openDMChannel(userId);
			} catch (error) {
				logger.error('Failed to open DM channel:', error);
				showDmActionErrorModal(error);
			}
		};
		const handleOpenBlockedDm = () => {
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(OPEN_DM_DESCRIPTOR)}
						description={i18n._(BLOCKED_USER_DM_WARNING_DESCRIPTOR, {userName: displayUserName})}
						primaryText={i18n._(OPEN_DM_DESCRIPTOR)}
						primaryVariant="primary"
						onPrimary={handleMessage}
						data-flx="user.user-profile-modal.handle-open-blocked-dm.confirm-modal"
					/>
				)),
			);
		};
		const handleSendFriendRequest = () => {
			RelationshipActionUtils.sendFriendRequest(i18n, userId);
		};
		const handleAcceptFriendRequest = (event?: {shiftKey?: boolean}) => {
			RelationshipActionUtils.showAcceptFriendRequestConfirmation(i18n, displayUser, {
				bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(event),
				showShiftBypassConfirmationTip: true,
			});
		};
		const handleRemoveFriend = (event?: {shiftKey?: boolean}) => {
			RelationshipActionUtils.showRemoveFriendConfirmation(i18n, displayUser, {
				bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(event),
				showShiftBypassConfirmationTip: true,
			});
		};
		const handleBlockUser = (event?: {shiftKey?: boolean}) => {
			RelationshipActionUtils.showBlockUserConfirmation(i18n, displayUser, {
				bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(event),
				showShiftBypassConfirmationTip: true,
			});
		};
		const handleUnblockUser = (event?: {shiftKey?: boolean}) => {
			RelationshipActionUtils.showUnblockUserConfirmation(i18n, displayUser, {
				bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(event),
				showShiftBypassConfirmationTip: true,
			});
		};
		const handleCancelFriendRequest = () => {
			RelationshipActionUtils.cancelFriendRequest(i18n, userId);
		};
		const handleStartVoiceCall = async (event?: ContextMenuActionEvent) => {
			try {
				const channelId = await PrivateChannelCommands.ensureDMChannel(userId);
				await CallUtils.requestStartCall(i18n, channelId, CallUtils.getCallStartRequestOptions(event, {kind: 'voice'}));
			} catch (error) {
				logger.error('Failed to start voice call:', error);
				showDmActionErrorModal(error);
			}
		};
		const handleStartVideoCall = async (event?: ContextMenuActionEvent) => {
			try {
				const channelId = await PrivateChannelCommands.ensureDMChannel(userId);
				await CallUtils.requestStartCall(i18n, channelId, CallUtils.getCallStartRequestOptions(event, {kind: 'video'}));
			} catch (error) {
				logger.error('Failed to start video call:', error);
				showDmActionErrorModal(error);
			}
		};
		const handleReportUser = () => {
			openReportUserModal({i18n, user: displayUser, guildId});
		};
		const handleCopyFluxerTag = () => {
			TextCopyCommands.copy(i18n, `${displayUser.username}#${displayUser.discriminator}`, true);
		};
		const handleCopyUserId = () => {
			TextCopyCommands.copy(i18n, displayUser.id, true);
		};
		const handleMoreOptionsPointerDown = (event: React.PointerEvent) => {
			const contextMenu = ContextMenu.contextMenu;
			const isOpen = !!contextMenu && contextMenu.target.target === moreOptionsButtonRef.current;
			if (isOpen) {
				event.stopPropagation();
				event.preventDefault();
				ContextMenuCommands.close();
			}
		};
		const renderBlockMenuItem = (onClose: () => void) => {
			if (displayUser?.system) {
				return null;
			}
			switch (relationshipType) {
				case RelationshipTypes.BLOCKED:
					return (
						<MenuItem
							icon={<ProhibitIcon data-flx="user.user-profile-modal.render-block-menu-item.prohibit-icon" />}
							onClick={(event) => {
								handleUnblockUser(event);
								onClose();
							}}
							data-flx="user.user-profile-modal.render-block-menu-item.menu-item.unblock-user"
						>
							{i18n._(UNBLOCK_USER_ACTION_DESCRIPTOR)}
						</MenuItem>
					);
				default:
					return (
						<MenuItem
							icon={<ProhibitIcon data-flx="user.user-profile-modal.render-block-menu-item.prohibit-icon--2" />}
							onClick={(event) => {
								handleBlockUser(event);
								onClose();
							}}
							danger
							data-flx="user.user-profile-modal.render-block-menu-item.menu-item.block-user"
						>
							{i18n._(BLOCK_DESCRIPTOR)}
						</MenuItem>
					);
			}
		};
		const openMoreOptionsMenu = (event: React.MouseEvent<HTMLButtonElement>) => {
			const contextMenu = ContextMenu.contextMenu;
			const isOpen = !!contextMenu && contextMenu.target.target === event.currentTarget;
			if (isOpen) {
				return;
			}
			ContextMenuCommands.openFromElementBottomRight(event, (props) => (
				<>
					{hasGuildProfile && (
						<MenuGroup data-flx="user.user-profile-modal.open-more-options-menu.menu-group">
							<MenuItem
								icon={<GlobeIcon data-flx="user.user-profile-modal.open-more-options-menu.globe-icon" />}
								onClick={() => {
									setShowGlobalProfile(!showGlobalProfile);
									props.onClose();
								}}
								data-flx="user.user-profile-modal.open-more-options-menu.menu-item.set-show-global-profile"
							>
								{showGlobalProfile ? i18n._(VIEW_COMMUNITY_PROFILE_DESCRIPTOR) : i18n._(VIEW_GLOBAL_PROFILE_DESCRIPTOR)}
							</MenuItem>
						</MenuGroup>
					)}
					{!isCurrentUser &&
						!isUserBot &&
						relationshipType === RelationshipTypes.FRIEND &&
						!hasActiveDirectCall &&
						!RuntimeConfig.directMessagesDisabled && (
							<MenuGroup data-flx="user.user-profile-modal.open-more-options-menu.menu-group--2">
								<MenuItem
									icon={<PhoneIcon data-flx="user.user-profile-modal.open-more-options-menu.phone-icon" />}
									onClick={(pressEvent) => {
										handleStartVoiceCall(pressEvent);
										props.onClose();
									}}
									data-flx="user.user-profile-modal.open-more-options-menu.menu-item.start-voice-call"
								>
									{i18n._(START_VOICE_CALL_DESCRIPTOR)}
								</MenuItem>
								<MenuItem
									icon={<VideoCameraIcon data-flx="user.user-profile-modal.open-more-options-menu.video-camera-icon" />}
									onClick={(pressEvent) => {
										handleStartVideoCall(pressEvent);
										props.onClose();
									}}
									data-flx="user.user-profile-modal.open-more-options-menu.menu-item.start-video-call"
								>
									{i18n._(START_VIDEO_CALL_DESCRIPTOR)}
								</MenuItem>
							</MenuGroup>
						)}
					<MenuGroup data-flx="user.user-profile-modal.open-more-options-menu.menu-group--3">
						<MenuItem
							icon={<CopyIcon data-flx="user.user-profile-modal.open-more-options-menu.copy-icon" />}
							onClick={() => {
								handleCopyFluxerTag();
								props.onClose();
							}}
							data-flx="user.user-profile-modal.open-more-options-menu.menu-item.copy-fluxer-tag"
						>
							{i18n._(COPY_USERNAME_DESCRIPTOR)}
						</MenuItem>
						<MenuItem
							icon={
								<IdentificationCardIcon data-flx="user.user-profile-modal.open-more-options-menu.identification-card-icon" />
							}
							onClick={() => {
								handleCopyUserId();
								props.onClose();
							}}
							data-flx="user.user-profile-modal.open-more-options-menu.menu-item.copy-user-id"
						>
							{i18n._(COPY_USER_ID_DESCRIPTOR)}
						</MenuItem>
					</MenuGroup>
					{!isCurrentUser && relationshipType === RelationshipTypes.FRIEND && !RuntimeConfig.directMessagesDisabled && (
						<MenuGroup data-flx="user.user-profile-modal.open-more-options-menu.menu-group--4">
							<MenuItem
								icon={
									<UserMinusIcon
										className={userProfileModalStyles.menuIcon}
										weight="fill"
										data-flx="user.user-profile-modal.open-more-options-menu.user-minus-icon"
									/>
								}
								onClick={(event) => {
									handleRemoveFriend(event);
									props.onClose();
								}}
								danger
								data-flx="user.user-profile-modal.open-more-options-menu.menu-item.remove-friend"
							>
								{i18n._(REMOVE_FRIEND_DESCRIPTOR)}
							</MenuItem>
						</MenuGroup>
					)}
					{!isCurrentUser && (
						<MenuGroup data-flx="user.user-profile-modal.open-more-options-menu.menu-group--5">
							<MenuItem
								icon={<FlagIcon data-flx="user.user-profile-modal.open-more-options-menu.flag-icon" />}
								onClick={() => {
									handleReportUser();
									props.onClose();
								}}
								danger
								data-flx="user.user-profile-modal.open-more-options-menu.menu-item.report-user"
							>
								{i18n._(REPORT_USER_DESCRIPTOR)}
							</MenuItem>
							{renderBlockMenuItem(props.onClose)}
						</MenuGroup>
					)}
					{hasImageMenuItems && (
						<UserImageMenuItems
							user={displayUser}
							profile={displayProfile}
							profileContext={displayProfileContext}
							previewOverrides={previewOverrides}
							onClose={props.onClose}
							data-flx="user.user-profile-modal.open-more-options-menu.user-image-menu-items"
						/>
					)}
				</>
			));
		};
		const renderActionButtons = () => {
			const currentUserUnclaimed = !(Users.currentUser?.isClaimed() ?? true);
			if (isCurrentUser && disableEditProfile) {
				return (
					<div
						className={userProfileModalStyles.actionButtons}
						data-flx="user.user-profile-modal.render-action-buttons.div"
					>
						<Tooltip
							text={i18n._(YOU_CAN_T_BEFRIEND_YOURSELF_DESCRIPTOR)}
							maxWidth="xl"
							data-flx="user.user-profile-modal.render-action-buttons.tooltip"
						>
							<div data-flx="user.user-profile-modal.render-action-buttons.div--2">
								<Button
									variant="secondary"
									small={true}
									leftIcon={
										<UserPlusIcon
											className={userProfileModalStyles.buttonIcon}
											data-flx="user.user-profile-modal.render-action-buttons.user-plus-icon"
										/>
									}
									disabled={true}
									data-flx="user.user-profile-modal.render-action-buttons.button"
								>
									{i18n._(ADD_FRIEND_DESCRIPTOR)}
								</Button>
							</div>
						</Tooltip>
						<Tooltip
							text={i18n._(YOU_CAN_T_MESSAGE_YOURSELF_DESCRIPTOR)}
							maxWidth="xl"
							data-flx="user.user-profile-modal.render-action-buttons.tooltip--2"
						>
							<div data-flx="user.user-profile-modal.render-action-buttons.div--3">
								<Button
									small={true}
									leftIcon={
										<ChatTeardropIcon
											className={userProfileModalStyles.buttonIcon}
											data-flx="user.user-profile-modal.render-action-buttons.chat-teardrop-icon"
										/>
									}
									disabled={true}
									data-flx="user.user-profile-modal.render-action-buttons.button--2"
								>
									<Trans context="message-action">Message</Trans>
								</Button>
							</div>
						</Tooltip>
					</div>
				);
			}
			if (isCurrentUser && !disableEditProfile) {
				return (
					<div
						className={userProfileModalStyles.actionButtons}
						data-flx="user.user-profile-modal.render-action-buttons.div--4"
					>
						{!currentUserUnclaimed && (
							<Button
								small={true}
								leftIcon={
									<PencilIcon
										className={userProfileModalStyles.buttonIcon}
										data-flx="user.user-profile-modal.render-action-buttons.pencil-icon"
									/>
								}
								onClick={handleEditProfile}
								data-flx="user.user-profile-modal.render-action-buttons.button.edit-profile"
							>
								<Trans>Edit profile</Trans>
							</Button>
						)}
						<Button
							ref={moreOptionsButtonRef}
							small={true}
							square={true}
							variant="secondary"
							icon={
								<DotsThreeIcon
									className={userProfileModalStyles.buttonIcon}
									weight="bold"
									data-flx="user.user-profile-modal.render-action-buttons.dots-three-icon"
								/>
							}
							aria-label={i18n._(MORE_ACTIONS_DESCRIPTOR)}
							onPointerDownCapture={handleMoreOptionsPointerDown}
							onClick={openMoreOptionsMenu}
							data-flx="user.user-profile-modal.render-action-buttons.button.open-more-options-menu"
						/>
					</div>
				);
			}
			const renderPrimaryActionButton = () => {
				if (isUserBot && !isFriendlyBot) {
					return null;
				}
				if (RuntimeConfig.directMessagesDisabled && relationshipType !== RelationshipTypes.BLOCKED) {
					return null;
				}
				if (relationshipType === RelationshipTypes.FRIEND) {
					return (
						<Tooltip
							text={i18n._(REMOVE_FRIEND_DESCRIPTOR)}
							maxWidth="xl"
							data-flx="user.user-profile-modal.render-primary-action-button.tooltip"
						>
							<div data-flx="user.user-profile-modal.render-primary-action-button.div">
								<Button
									variant="secondary"
									small={true}
									square={true}
									icon={
										<UserMinusIcon
											className={userProfileModalStyles.buttonIcon}
											data-flx="user.user-profile-modal.render-primary-action-button.user-minus-icon"
										/>
									}
									aria-label={i18n._(REMOVE_FRIEND_DESCRIPTOR)}
									onClick={handleRemoveFriend}
									data-flx="user.user-profile-modal.render-primary-action-button.button.remove-friend"
								/>
							</div>
						</Tooltip>
					);
				}
				if (relationshipType === RelationshipTypes.BLOCKED) {
					return (
						<Tooltip
							text={i18n._(UNBLOCK_USER_DESCRIPTOR)}
							maxWidth="xl"
							data-flx="user.user-profile-modal.render-primary-action-button.tooltip--2"
						>
							<div data-flx="user.user-profile-modal.render-primary-action-button.div--2">
								<Button
									variant="secondary"
									small={true}
									square={true}
									icon={
										<ProhibitIcon
											className={userProfileModalStyles.buttonIcon}
											data-flx="user.user-profile-modal.render-primary-action-button.prohibit-icon"
										/>
									}
									aria-label={i18n._(UNBLOCK_USER_DESCRIPTOR)}
									onClick={handleUnblockUser}
									data-flx="user.user-profile-modal.render-primary-action-button.button.unblock-user"
								/>
							</div>
						</Tooltip>
					);
				}
				if (relationshipType === RelationshipTypes.INCOMING_REQUEST) {
					return (
						<Tooltip
							text={i18n._(ACCEPT_FRIEND_REQUEST_DESCRIPTOR)}
							maxWidth="xl"
							data-flx="user.user-profile-modal.render-primary-action-button.tooltip--3"
						>
							<div data-flx="user.user-profile-modal.render-primary-action-button.div--3">
								<Button
									variant="secondary"
									small={true}
									square={true}
									icon={
										<CheckCircleIcon
											className={userProfileModalStyles.buttonIcon}
											data-flx="user.user-profile-modal.render-primary-action-button.check-circle-icon"
										/>
									}
									aria-label={i18n._(ACCEPT_FRIEND_REQUEST_DESCRIPTOR)}
									onClick={handleAcceptFriendRequest}
									data-flx="user.user-profile-modal.render-primary-action-button.button.accept-friend-request"
								/>
							</div>
						</Tooltip>
					);
				}
				if (relationshipType === RelationshipTypes.OUTGOING_REQUEST) {
					return (
						<Tooltip
							text={i18n._(CANCEL_FRIEND_REQUEST_DESCRIPTOR)}
							maxWidth="xl"
							data-flx="user.user-profile-modal.render-primary-action-button.tooltip--4"
						>
							<div data-flx="user.user-profile-modal.render-primary-action-button.div--4">
								<Button
									variant="secondary"
									small={true}
									square={true}
									icon={
										<ClockCounterClockwiseIcon
											className={userProfileModalStyles.buttonIcon}
											data-flx="user.user-profile-modal.render-primary-action-button.clock-counter-clockwise-icon"
										/>
									}
									aria-label={i18n._(CANCEL_FRIEND_REQUEST_DESCRIPTOR)}
									onClick={handleCancelFriendRequest}
									data-flx="user.user-profile-modal.render-primary-action-button.button.cancel-friend-request"
								/>
							</div>
						</Tooltip>
					);
				}
				if (relationshipType === undefined && (!isUserBot || isFriendlyBot)) {
					const tooltipText = currentUserUnclaimed
						? i18n._(CLAIM_YOUR_ACCOUNT_TO_SEND_FRIEND_REQUESTS_DESCRIPTOR)
						: i18n._(SEND_FRIEND_REQUEST_DESCRIPTOR);
					return (
						<Tooltip
							text={tooltipText}
							maxWidth="xl"
							data-flx="user.user-profile-modal.render-primary-action-button.tooltip--5"
						>
							<div data-flx="user.user-profile-modal.render-primary-action-button.div--5">
								<Button
									variant="secondary"
									small={true}
									square={true}
									icon={
										<UserPlusIcon
											className={userProfileModalStyles.buttonIcon}
											data-flx="user.user-profile-modal.render-primary-action-button.user-plus-icon"
										/>
									}
									aria-label={i18n._(SEND_FRIEND_REQUEST_DESCRIPTOR)}
									onClick={handleSendFriendRequest}
									disabled={currentUserUnclaimed}
									data-flx="user.user-profile-modal.render-primary-action-button.button.send-friend-request"
								/>
							</div>
						</Tooltip>
					);
				}
				return null;
			};
			return (
				<div
					className={userProfileModalStyles.actionButtons}
					data-flx="user.user-profile-modal.render-action-buttons.div--5"
				>
					{!RuntimeConfig.directMessagesDisabled && (
						<Button
							small={true}
							leftIcon={
								<ChatTeardropIcon
									className={userProfileModalStyles.buttonIcon}
									data-flx="user.user-profile-modal.render-action-buttons.chat-teardrop-icon--2"
								/>
							}
							onClick={isBlocked ? handleOpenBlockedDm : handleMessage}
							data-flx="user.user-profile-modal.render-action-buttons.button.open-blocked-dm"
						>
							{isBlocked ? i18n._(OPEN_DM_DESCRIPTOR) : <Trans context="message-action">Message</Trans>}
						</Button>
					)}
					{renderPrimaryActionButton()}
					<Button
						ref={moreOptionsButtonRef}
						small={true}
						square={true}
						variant="secondary"
						icon={
							<DotsThreeIcon
								className={userProfileModalStyles.buttonIcon}
								weight="bold"
								data-flx="user.user-profile-modal.render-action-buttons.dots-three-icon--2"
							/>
						}
						aria-label={i18n._(MORE_ACTIONS_DESCRIPTOR)}
						onPointerDownCapture={handleMoreOptionsPointerDown}
						onClick={openMoreOptionsMenu}
						data-flx="user.user-profile-modal.render-action-buttons.button.open-more-options-menu--2"
					/>
				</div>
			);
		};
		const borderProfile = resolvedProfile.getEffectiveProfile() ?? null;
		const borderAccentColor =
			previewOverrides?.previewAccentColor !== undefined
				? previewOverrides.previewAccentColor
				: borderProfile?.accent_color;
		const borderColor = getUserAccentColor(displayUser, borderAccentColor);
		return (
			<Modal.Root
				size="medium"
				initialFocusRef={autoFocusNote ? noteRef : undefined}
				className={userProfileModalStyles.modalRoot}
				data-flx="user.user-profile-modal.modal-root"
			>
				<Modal.ScreenReaderLabel
					text={screenReaderLabel}
					data-flx="user.user-profile-modal.modal-screen-reader-label"
				/>
				<div
					className={userProfileModalStyles.modalContainer}
					style={{borderColor}}
					data-flx="user.user-profile-modal.div"
				>
					{showProfileSkeleton ? (
						<UserProfileLoadingSkeleton
							variant="modal"
							data-flx="user.user-profile-modal.user-profile-loading-skeleton"
						/>
					) : (
						<ProfileModalContent
							key={displayUser.id}
							profile={resolvedProfile}
							user={displayUser}
							userNote={userNote}
							autoFocusNote={autoFocusNote}
							noteRef={noteRef}
							renderActionButtons={renderActionButtons}
							showProfileDataWarning={shouldShowProfileDataWarning}
							previewOverrides={previewOverrides}
							data-flx="user.user-profile-modal.profile-modal-content"
						/>
					)}
				</div>
			</Modal.Root>
		);
	},
);
