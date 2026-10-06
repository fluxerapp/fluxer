// SPDX-License-Identifier: AGPL-3.0-or-later

import {showDmActionErrorModal} from '@app/features/app/components/alerts/DmActionErrorModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {
	CustomStatusDisplay,
	type EmojiPressData,
} from '@app/features/app/components/shared/custom_status_display/CustomStatusDisplay';
import {useAnimatedImageUrl} from '@app/features/app/hooks/useAnimatedImageUrl';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Authentication from '@app/features/auth/state/Authentication';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import {GifIndicator} from '@app/features/channel/components/embeds/media/GifIndicator';
import Channels from '@app/features/channel/state/Channels';
import {ExpressionInfoBottomSheet} from '@app/features/expressions/components/bottomsheets/ExpressionInfoBottomSheet';
import {
	BLOCKED_USER_DM_WARNING_DESCRIPTOR,
	OPEN_DM_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import GuildMembers from '@app/features/member/state/GuildMembers';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import Permission from '@app/features/permissions/state/Permission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import MemberPresenceSubscription from '@app/features/presence/state/MemberPresenceSubscription';
import * as RelationshipCommands from '@app/features/relationship/commands/RelationshipCommands';
import Relationships from '@app/features/relationship/state/Relationships';
import * as RelationshipActionUtils from '@app/features/relationship/utils/RelationshipActionUtils';
import {
	ACCEPT_FRIEND_REQUEST_DESCRIPTOR,
	CANCEL_FRIEND_REQUEST_DESCRIPTOR,
	REMOVE_FRIEND_DESCRIPTOR,
} from '@app/features/relationship/utils/RelationshipMessageDescriptors';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import {getUserAccentColor} from '@app/features/theme/utils/AccentColorUtils';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Scroller} from '@app/features/ui/components/Scroller';
import {Spinner} from '@app/features/ui/components/Spinner';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import * as Sheet from '@app/features/ui/sheet/Sheet';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import {MutualItemsSheet} from '@app/features/user/components/modals/MutualItemsSheet';
import {NoteEditSheet} from '@app/features/user/components/modals/NoteEditSheet';
import {UserProfileActionsSheet} from '@app/features/user/components/modals/UserProfileActionsSheet';
import styles from '@app/features/user/components/modals/UserProfileMobileSheet.module.css';
import previewStyles from '@app/features/user/components/profile/ProfilePreview.module.css';
import {getContrastingNotchColor} from '@app/features/user/components/modals/UserProfileUtils';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import {getMutualItemsDescriptor} from '@app/features/user/components/modals/user_profile_modal/MutualItemsDescriptors';
import {
	getMutualCommunityDisplayItems,
	getMutualGroupChannels,
} from '@app/features/user/components/modals/user_profile_modal/MutualItemsUtils';
import {UserProfileBadges} from '@app/features/user/components/popouts/UserProfileBadges';
import {
	UserProfileBio,
	UserProfileConnections,
	UserProfileMembershipInfo,
	UserProfileRoles,
	UserProfileTimezoneInfo,
} from '@app/features/user/components/popouts/UserProfileShared';
import {useAutoplayExpandedProfileAnimations} from '@app/features/user/hooks/useAutoplayExpandedProfileAnimations';
import {Profile} from '@app/features/user/models/Profile';
import {User} from '@app/features/user/models/User';
import UserNote from '@app/features/user/state/UserNote';
import UserProfile from '@app/features/user/state/UserProfile';
import UserProfileMobile from '@app/features/user/state/UserProfileMobile';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import * as ProfileDisplayUtils from '@app/features/user/utils/ProfileDisplayUtils';
import {
	getProfileMembershipDisplayName,
	GLOBAL_PROFILE_MEMBERSHIP,
	resolveProfileGuildMembership,
	toProfileDisplayContext,
} from '@app/features/user/utils/ProfileGuildMembership';
import {createMockProfile} from '@app/features/user/utils/ProfileUtils';
import * as CallUtils from '@app/features/voice/utils/CallUtils';
import {hasActiveDirectCallWithUser} from '@app/features/voice/utils/PrivateCallMenuUtils';
import {ME} from '@fluxer/constants/src/AppConstants';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {
	MEDIA_PROXY_AVATAR_SIZE_DEFAULT,
	MEDIA_PROXY_PROFILE_BANNER_SIZE_MODAL,
} from '@fluxer/constants/src/MediaProxyAssetSizes';
import {PublicUserFlags, RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {
	ChatTeardropIcon,
	CheckCircleIcon,
	ClockCounterClockwiseIcon,
	DotsThreeIcon,
	NotePencilIcon,
	PencilIcon,
	PhoneIcon,
	ProhibitIcon,
	UserMinusIcon,
	UserPlusIcon,
	VideoCameraIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useMemo, useState} from 'react';
import PersonaProfileMobile from '../../state/PersonaProfileMobile';
import Personas from '@app/features/user/state/Personas';
import { emptyOwnPersona } from '../../models/Persona';
import { PersonaEditorModal } from '@app/features/user/components/modals/PersonaEditorModal';
import * as ColorUtils from '@app/features/theme/utils/ColorUtils';
import { Avatar } from '@app/features/ui/components/Avatar';
import { isKeyboardActivationKey } from '@app/features/input/utils/KeyboardUtils';
import { clsx } from 'clsx';
import { getDefaultAvatarPrimaryColor } from '@app/features/user/utils/AvatarSourceUtils';

const UNBLOCK_USER_DESCRIPTOR = msg({
	message: 'Unblock user',
	comment:
		'Button or menu action label in the user profile mobile sheet. Keep it concise. Keep the tone plain and specific.',
});
const SEND_FRIEND_REQUEST_DESCRIPTOR = msg({
	message: 'Send friend request',
	comment: 'Button or menu action label in the user profile mobile sheet. Keep it concise.',
});
const logger = new Logger('PersonaProfileMobileSheet');
export const PersonaProfileMobileSheet: React.FC = observer(function PersonaProfileMobileSheet() {
	const store = PersonaProfileMobile;
	const {userId, personaId, autoFocusNote, isOpen} = store;
	//const selectedChannelId = SelectedChannel.currentChannelId;
	//const selectedChannel = selectedChannelId ? Channels.getChannel(selectedChannelId) : null;
	// const channelGuildId =
	// 	selectedChannel?.guildId && selectedChannel.guildId !== ME ? selectedChannel.guildId : undefined;
	//const guildId = explicitGuildId ?? channelGuildId;
	const persona = (personaId ? Personas.getPersona(personaId) : null) || PersonaProfileMobile.initialPersona || emptyOwnPersona;
	const storeUser = userId ? Users.getUser(userId) : null;
	const user = storeUser;
	const fallbackUser = useMemo(
		() => new User({
			id: userId || "0",
			username: userId || "0",
			discriminator: '0000',
			global_name: null,
			avatar: null,
			avatar_color: null,
			flags: 0,
		}),
		[userId],
	);
	const displayUser = user ?? fallbackUser;
	const fallbackProfile = useMemo(() => (fallbackUser ? createMockProfile(fallbackUser) : null), [fallbackUser]);
	//const mockProfile = useMemo(() => (user ? createMockProfile(user) : null), [user]);
	// const initialProfile = useMemo(() => (userId ? UserProfile.getProfile(userId, guildId) : null), [userId, guildId]);
	// const [profile, setProfile] = useState<Profile | null>(initialProfile);
	// const [isProfileLoading, setIsProfileLoading] = useState(() => !initialProfile);
	// const profileMatchesContext = profile?.userId === userId && (profile?.guildId ?? null) === (guildId ?? null);
	// const activeProfile = profileMatchesContext ? profile : initialProfile;
	const profile: Profile = new Profile({
		user: {...displayUser, global_name: displayUser.username || "0", avatar_color: displayUser.avatarColor || null},
		timezone_offset: null,
		user_profile: {
			accent_color: persona.accent_color || null,
			banner: persona.banner || null,
			bio: persona.bio || null,
			pronouns: persona.pronouns || null,
			banner_color: persona.banner_color || null,
		}
	});
	const isContextSwitching = Boolean(userId) && !profile;
	const shouldShowProfileLoading = (!profile) || isContextSwitching;
	// useEffect(() => {
	// 	setProfile(initialProfile);
	// 	setIsProfileLoading(!initialProfile);
	// }, [initialProfile]);
	// useEffect(() => {
	// 	if (!userId || activeProfile) {
	// 		setIsProfileLoading(false);
	// 		return;
	// 	}
	// 	let cancelled = false;
	// 	setIsProfileLoading(true);
	// 	UserProfileCommands.fetch(userId, guildId)
	// 		.then(() => {
	// 			if (cancelled) return;
	// 			const fetchedProfile = UserProfile.getProfile(userId, guildId);
	// 			if (fetchedProfile) {
	// 				setProfile(fetchedProfile);
	// 			}
	// 		})
	// 		.catch((error) => {
	// 			if (cancelled) return;
	// 			logger.error('Failed to fetch user profile:', error);
	// 		})
	// 		.finally(() => {
	// 			if (cancelled) return;
	// 			setIsProfileLoading(false);
	// 		});
	// 	return () => {
	// 		cancelled = true;
	// 	};
	// }, [userId, guildId, activeProfile]);
	// useEffect(() => {
	// 	if (!guildId || !userId) {
	// 		return;
	// 	}
	// 	const hasMember = GuildMembers.getMember(guildId, userId);
	// 	if (!hasMember) {
	// 		MemberPresenceSubscription.touchMember(guildId, userId);
	// 		GuildMembers.fetchMembers(guildId, {userIds: [userId]}).catch((error) => {
	// 			logger.error(' Failed to fetch guild member:', error);
	// 		});
	// 	} else {
	// 		MemberPresenceSubscription.touchMember(guildId, userId);
	// 	}
	// 	return () => {
	// 		MemberPresenceSubscription.unsubscribe(guildId, userId);
	// 	};
	// }, [guildId, userId]);
	if (!isOpen || !displayUser || !persona) {
		return null;
	}
	const effectiveProfile: Profile | null = profile ?? fallbackProfile;
	const resolvedProfile: Profile = effectiveProfile ?? fallbackProfile!;
	const userNote = userId ? UserNote.getUserNote(userId) : null;
	const handleClose = () => {
		store.close();
	};
	const profileIdentityKey = `${displayUser.id}:P_${persona.id}`;
	return (
		<PersonaProfileMobileSheetContent
			key={profileIdentityKey}
			user={displayUser}
			profile={resolvedProfile}
			userNote={userNote}
			personaId={personaId || undefined}
			autoFocusNote={autoFocusNote}
			isLoading={shouldShowProfileLoading}
			onClose={handleClose}
			data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content"
		/>
	);
});

interface PersonaProfileMobileSheetContentProps {
	user: User;
	profile: Profile;
	userNote: string | null;
	personaId?: string;
	autoFocusNote?: boolean;
	isLoading: boolean;
	onClose: () => void;
}

interface EmojiInfoState {
	id?: string;
	name: string;
	animated?: boolean;
}

const PersonaProfileMobileSheetContent: React.FC<PersonaProfileMobileSheetContentProps> = observer(
	function PersonaProfileMobileSheetContent({user, profile, personaId, userNote, autoFocusNote, isLoading, onClose}) {
		const {i18n} = useLingui();
		const [noteSheetOpen, setNoteSheetOpen] = useState(false);
		const [actionsSheetOpen, setActionsSheetOpen] = useState(false);
		//const [showGlobalProfile, setShowGlobalProfile] = useState(false);
		const [emojiInfoOpen, setEmojiInfoOpen] = useState(false);
		const [selectedEmoji, setSelectedEmoji] = useState<EmojiInfoState | null>(null);
		const [mutualSheetView, setMutualSheetView] = useState<'friends' | 'communities_groups' | null>(null);
		const hidePrivateDetails = StreamerMode.shouldHidePersonalInformation;
		const isCurrentUser = user.id === Authentication.currentUserId;
		const relationship = Relationships.getRelationship(user.id);
		const relationshipType = relationship?.type;
		const isBlocked = relationshipType === RelationshipTypes.BLOCKED;
		const hasActiveDirectCall = hasActiveDirectCallWithUser(user.id);
		const currentUserUnclaimed = !(Users.currentUser?.isClaimed() ?? true);
		const persona = (personaId ? Personas.getPersona(personaId) : null) || PersonaProfileMobile.initialPersona || null;
		// const profileMembership = resolveProfileGuildMembership(profile, {
		// 	fallbackGuildId: guildId,
		// 	userId: user.id,
		// 	allowStoreFallback: true,
		// });
		// const guildMember = profileMembership.kind === 'guildMember' ? profileMembership.member : null;
		//const memberRoles = profileMembership.kind === 'guildMember' ? profileMembership.roles : [];
		// const canManageRoles = Permission.can(Permissions.MANAGE_ROLES, {guildId: profile?.guildId ?? guildId});
		// const mutualFriendsCount = !user.bot ? (profile?.mutualFriends?.length ?? 0) : 0;
		// const mutualCommunityDisplayItems = getMutualCommunityDisplayItems(profile?.mutualGuilds ?? []);
		// const mutualCommunitiesCount = mutualCommunityDisplayItems.length;
		// const mutualGroups = getMutualGroupChannels(user.id);
		// const mutualGroupsCount = mutualGroups.length;
		// const mutualCommunitiesGroupsCount = mutualCommunitiesCount + mutualGroupsCount;
		// const hasMutuals = mutualFriendsCount > 0 || mutualCommunitiesGroupsCount > 0;
		// const hasGuildProfile = !!(profile?.guildId && profile?.guildMemberProfile);
		// const shouldShowGuildProfile = hasGuildProfile && !showGlobalProfile;
		//const displayMembership = shouldShowGuildProfile ? profileMembership : resolveProfileGuildMembership(null);
		const profileContext = useMemo<ProfileDisplayUtils.ProfileDisplayContext>(
			() =>
				toProfileDisplayContext({
					user,
					profile,
					membership: GLOBAL_PROFILE_MEMBERSHIP,
					guildMemberProfile: undefined,
				}),
			[user, profile],
		);
		const shouldAutoplayProfileAnimations = useAutoplayExpandedProfileAnimations();
		const personaContext: ProfileDisplayUtils.ProfileDisplayContext = {
			...profileContext,
			user: profileContext.user.withUpdates({
				id: persona?.id ?? user.id,
				avatar: persona?.avatar ?? null,
				banner: persona?.banner ?? null,
			})
		}
		const {avatarUrl, hoverAvatarUrl} = useMemo(
			() => ProfileDisplayUtils.getProfileAvatarUrls(personaContext, undefined, MEDIA_PROXY_AVATAR_SIZE_DEFAULT),
			[profileContext],
		);
		const {bannerUrl: staticBannerUrl, hoverBannerUrl} = useMemo(
			() => ProfileDisplayUtils.getProfileBannerUrls(personaContext, undefined, MEDIA_PROXY_PROFILE_BANNER_SIZE_MODAL),
			[profileContext],
		);
		const {
			hoverRef: bannerHoverRef,
			imageUrl: bannerUrl,
			showGifIndicator,
		} = useAnimatedImageUrl({
			staticUrl: staticBannerUrl,
			animatedUrl: hoverBannerUrl,
			kind: 'gif',
		});
		const effectiveProfile = profile.userProfile;
		const displayName = persona?.display_name || "Persona";
		const isDisplayNameUsername = displayName === user.username;
		const bannerColor = ColorUtils.int2hex(profile.userProfile?.accent_color ?? persona?.accent_color ?? getDefaultAvatarPrimaryColor(persona?.id ?? user.id));
		// useEffect(() => {
		// 	if (autoFocusNote && !hidePrivateDetails) {
		// 		setNoteSheetOpen(true);
		// 	}
		// }, [autoFocusNote, hidePrivateDetails]);
		const notchColor = useMemo(
			() => getContrastingNotchColor(effectiveProfile?.banner_color ?? null, !!bannerUrl),
			[effectiveProfile?.banner_color, bannerUrl],
		);
		// const handleMessage = async () => {
		// 	try {
		// 		onClose();
		// 		await PrivateChannelCommands.openDMChannel(user.id);
		// 	} catch (error) {
		// 		logger.error('Failed to open DM channel:', error);
		// 		showDmActionErrorModal(error);
		// 	}
		// };
		// const handleOpenBlockedDm = () => {
		// 	ModalCommands.push(
		// 		modal(() => (
		// 			<ConfirmModal
		// 				title={i18n._(OPEN_DM_DESCRIPTOR)}
		// 				description={i18n._(BLOCKED_USER_DM_WARNING_DESCRIPTOR, {userName: displayName})}
		// 				primaryText={i18n._(OPEN_DM_DESCRIPTOR)}
		// 				primaryVariant="primary"
		// 				onPrimary={handleMessage}
		// 				data-flx="user.user-profile-mobile-sheet.handle-open-blocked-dm.confirm-modal"
		// 			/>
		// 		)),
		// 	);
		// };
		// const handleSendFriendRequest = () => {
		// 	RelationshipCommands.sendFriendRequest(user.id);
		// };
		// const handleAcceptFriendRequest = () => {
		// 	RelationshipActionUtils.showAcceptFriendRequestConfirmation(i18n, user);
		// };
		// const handleCancelFriendRequest = () => {
		// 	RelationshipCommands.removeRelationship(user.id);
		// };
		// const handleRemoveFriend = () => {
		// 	RelationshipActionUtils.showRemoveFriendConfirmation(i18n, user);
		// };
		// const handleUnblockUser = () => {
		// 	RelationshipActionUtils.showUnblockUserConfirmation(i18n, user);
		// };
		const handleEditProfile = () => {
			if (!persona) return;
			ModalCommands.push(
				modal(() => (
					<PersonaEditorModal
						initialPersona={persona}
						data-flx="user.persona-profile-mobile-sheet.handle-edit-profile.persona-editor-modal"
					/>
				)),
			);
		};
		// const handleStartVoiceCall = async () => {
		// 	try {
		// 		const channelId = await PrivateChannelCommands.ensureDMChannel(user.id);
		// 		await CallUtils.requestStartCall(i18n, channelId, {kind: 'voice'});
		// 	} catch (error) {
		// 		logger.error('Failed to start voice call:', error);
		// 		showDmActionErrorModal(error);
		// 	}
		// };
		// const handleStartVideoCall = async () => {
		// 	try {
		// 		const channelId = await PrivateChannelCommands.ensureDMChannel(user.id);
		// 		await CallUtils.requestStartCall(i18n, channelId, {kind: 'video'});
		// 	} catch (error) {
		// 		logger.error('Failed to start video call:', error);
		// 		showDmActionErrorModal(error);
		// 	}
		// };
		const handleEmojiPress = (emoji: EmojiPressData) => {
			setSelectedEmoji({
				id: emoji.id ?? undefined,
				name: emoji.name,
				animated: emoji.animated,
			});
			setEmojiInfoOpen(true);
		};
		// const renderRelationshipButton = () => {
		// 	const isFriendlyBot = user.bot && (user.flags & PublicUserFlags.FRIENDLY_BOT) === PublicUserFlags.FRIENDLY_BOT;
		// 	if (isCurrentUser || (user.bot && !isFriendlyBot)) return null;
		// 	if (RuntimeConfig.directMessagesDisabled && relationshipType !== RelationshipTypes.BLOCKED) {
		// 		return null;
		// 	}
		// 	if (relationshipType === RelationshipTypes.FRIEND) {
		// 		return (
		// 			<button
		// 				type="button"
		// 				onClick={handleRemoveFriend}
		// 				className={styles.actionButton}
		// 				aria-label={i18n._(REMOVE_FRIEND_DESCRIPTOR)}
		// 				data-flx="user.user-profile-mobile-sheet.render-relationship-button.action-button.remove-friend"
		// 			>
		// 				<UserMinusIcon
		// 					className={styles.icon}
		// 					data-flx="user.user-profile-mobile-sheet.render-relationship-button.icon"
		// 				/>
		// 			</button>
		// 		);
		// 	}
		// 	if (relationshipType === RelationshipTypes.BLOCKED) {
		// 		return (
		// 			<button
		// 				type="button"
		// 				onClick={handleUnblockUser}
		// 				className={styles.actionButton}
		// 				aria-label={i18n._(UNBLOCK_USER_DESCRIPTOR)}
		// 				data-flx="user.user-profile-mobile-sheet.render-relationship-button.action-button.unblock-user"
		// 			>
		// 				<ProhibitIcon
		// 					className={styles.icon}
		// 					data-flx="user.user-profile-mobile-sheet.render-relationship-button.icon--2"
		// 				/>
		// 			</button>
		// 		);
		// 	}
		// 	if (relationshipType === RelationshipTypes.INCOMING_REQUEST) {
		// 		return (
		// 			<button
		// 				type="button"
		// 				onClick={handleAcceptFriendRequest}
		// 				className={styles.actionButton}
		// 				aria-label={i18n._(ACCEPT_FRIEND_REQUEST_DESCRIPTOR)}
		// 				data-flx="user.user-profile-mobile-sheet.render-relationship-button.action-button.accept-friend-request"
		// 			>
		// 				<CheckCircleIcon
		// 					className={styles.icon}
		// 					data-flx="user.user-profile-mobile-sheet.render-relationship-button.icon--3"
		// 				/>
		// 			</button>
		// 		);
		// 	}
		// 	if (relationshipType === RelationshipTypes.OUTGOING_REQUEST) {
		// 		return (
		// 			<button
		// 				type="button"
		// 				onClick={handleCancelFriendRequest}
		// 				className={styles.actionButton}
		// 				aria-label={i18n._(CANCEL_FRIEND_REQUEST_DESCRIPTOR)}
		// 				data-flx="user.user-profile-mobile-sheet.render-relationship-button.action-button.cancel-friend-request"
		// 			>
		// 				<ClockCounterClockwiseIcon
		// 					className={styles.icon}
		// 					data-flx="user.user-profile-mobile-sheet.render-relationship-button.icon--4"
		// 				/>
		// 			</button>
		// 		);
		// 	}
		// 	if (relationshipType === undefined && !currentUserUnclaimed && Users.currentUser?.verified !== false) {
		// 		return (
		// 			<button
		// 				type="button"
		// 				onClick={handleSendFriendRequest}
		// 				className={styles.actionButton}
		// 				aria-label={i18n._(SEND_FRIEND_REQUEST_DESCRIPTOR)}
		// 				data-flx="user.user-profile-mobile-sheet.render-relationship-button.action-button.send-friend-request"
		// 			>
		// 				<UserPlusIcon
		// 					className={styles.icon}
		// 					data-flx="user.user-profile-mobile-sheet.render-relationship-button.icon--5"
		// 				/>
		// 			</button>
		// 		);
		// 	}
		// 	return null;
		// };
		return (
			<>
				<BottomSheet
					isOpen={true}
					onClose={onClose}
					snapPoints={[0, 0.9, 1]}
					initialSnap={1}
					disablePadding={true}
					disableDefaultHeader={true}
					showHandle={false}
					containerClassName={styles.sheetContainer}
					data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.bottom-sheet"
				>
					<div
						className={styles.container}
						data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.container"
					>
						{isLoading ? (
							<div
								className={styles.loadingScreen}
								data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.loading-screen"
							>
								<Spinner
									size="large"
									data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.spinner"
								/>
							</div>
						) : (
							<Scroller
								key="user-profile-mobile-sheet-scroller"
								data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.scroller"
							>
								<div
									style={{paddingBottom: 'calc(env(safe-area-inset-bottom, 0px) + 1rem)'}}
									data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.div"
								>
									<div
										ref={bannerHoverRef}
										className={styles.bannerContainer}
										data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.banner-container"
									>
										{bannerUrl ? (
											<div
												className={styles.bannerImage}
												style={{backgroundImage: `url(${bannerUrl})`}}
												data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.banner-image"
											/>
										) : (
											<div
												className={styles.bannerColor}
												style={{backgroundColor: bannerColor}}
												data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.banner-color"
											/>
										)}
										{showGifIndicator && (
											<GifIndicator data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.gif-indicator" />
										)}
										<Sheet.Handle
											className={styles.notchContainer}
											data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.notch-container"
										>
											<div
												className={styles.notch}
												style={{backgroundColor: notchColor}}
												data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.notch"
											/>
										</Sheet.Handle>
									</div>
									<div
										className={styles.profileContent}
										data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.profile-content"
									>
										<div
											className={styles.avatarContainer}
											data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.avatar-container"
										>
											<div
												className={styles.avatarBorder}
												style={{borderRadius: '9999px'}}
												data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.avatar-border"
											>
												<Avatar
													size={80}
													user={user}
													avatarUrl={avatarUrl}
													hoverAvatarUrl={hoverAvatarUrl}
													personaId={personaId}
													personaAvatar={persona?.avatar}
													forceAnimate
													forceAnimateIgnoringSettings
													data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.status-aware-avatar"
												/>
											</div>
										</div>
										<div
											className={styles.contentPadding}
											data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.content-padding"
										>
											<div
												className={styles.actionsContainer}
												data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.actions-container"
											>
												{/* {!isCurrentUser && renderRelationshipButton()} */}
												{/* <button
													type="button"
													onClick={() => setActionsSheetOpen(true)}
													className={styles.actionButton}
													data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.action-button.set-actions-sheet-open"
												>
													<DotsThreeIcon
														className={styles.icon}
														weight="bold"
														data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.icon"
													/>
												</button> */}
											</div>
											<div
												className={styles.usernameContainer}
												data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.username-container"
											>
												<div
													className={styles.usernameRow}
													data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.username-row"
												>
													<span
														className={styles.username}
														data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.username"
													>
														{displayName}
													</span>
													{isDisplayNameUsername && (
														<span
															className={styles.discriminator}
															data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.discriminator"
														>
															{NicknameUtils.formatTagForStreamerMode(`#${user.discriminator}`)}
														</span>
													)}
												</div>
												{/* <div
													className={styles.tagBadgeRow}
													data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.tag-badge-row"
												>
													{!isDisplayNameUsername && (
														<span
															className={styles.fullTag}
															data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.full-tag"
														>
															{NicknameUtils.formatTagForStreamerMode(`${user.username}#${user.discriminator}`)}
														</span>
													)}
													<div
														className={styles.badgesWrapper}
														data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.badges-wrapper"
													>
														<UserProfileBadges
															user={user}
															profile={profile}
															isModal={true}
															isMobile={true}
															data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.user-profile-badges"
														/>
													</div>
												</div> */}
												{/* <div
													className={styles.customStatusRow}
													data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.custom-status-row"
												>
													<CustomStatusDisplay
														userId={user.id}
														className={styles.customStatusText}
														showTooltip
														allowJumboEmoji
														maxLines={0}
														alwaysAnimate={shouldAutoplayProfileAnimations}
														onEmojiPress={isCurrentUser ? undefined : handleEmojiPress}
														data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.custom-status-text"
													/>
												</div> */}
											</div>
											{isCurrentUser ? (
												!currentUserUnclaimed && (
													<div
														className={styles.actionButtonsContainer}
														data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.action-buttons-container"
													>
														<button
															type="button"
															onClick={handleEditProfile}
															className={styles.editProfileButton}
															data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.edit-profile-button"
														>
															<PencilIcon
																className={styles.editProfileIcon}
																data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.edit-profile-icon"
															/>
															<span
																className={styles.editProfileText}
																data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.edit-profile-text"
															>
																<Trans>Edit profile</Trans>
															</span>
														</button>
													</div>
												)
											) : null}
											{profile && (effectiveProfile?.bio || profile) && (
												<div
													className={styles.infoCard}
													data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.info-card"
												>
													{effectiveProfile?.bio && (
														<div
															className={styles.bioSection}
															data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.bio-section"
														>
															<h3
																className={styles.bioHeader}
																data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.bio-header"
															>
																<Trans>About me</Trans>
															</h3>
															<UserProfileBio
																profile={profile}
																profileData={effectiveProfile}
																data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.user-profile-bio"
															/>
														</div>
													)}
												</div>
											)}
											{/* {!hidePrivateDetails && (
												<button
													type="button"
													onClick={() => setNoteSheetOpen(true)}
													className={styles.noteButton}
													data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.note-button.set-note-sheet-open"
												>
													<div data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.div--2">
														<h3
															className={styles.noteTitle}
															data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.note-title"
														>
															<Trans>Note</Trans>
														</h3>
														<p
															className={styles.noteSubtitle}
															data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.note-subtitle"
														>
															<Trans>(only visible to you)</Trans>
														</p>
														{userNote && (
															<p
																className={styles.noteText}
																data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.note-text"
															>
																{userNote}
															</p>
														)}
													</div>
													<div
														className={styles.noteIconContainer}
														data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.note-icon-container"
													>
														<NotePencilIcon
															className={styles.noteIcon}
															data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.note-icon"
														/>
													</div>
												</button>
											)} */}
											<div
												className={styles.actionButtonsContainer}
												data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.main-profile-action-buttons-container"
											>
												<div
													role="button"
													tabIndex={0}
													className={clsx(styles.editProfileButton, previewStyles.mainProfileMobileButton, previewStyles.mainProfileButton)}
													onClick={() => {
														UserProfileCommands.openUserProfile(user.id);
														PersonaProfileMobile.close();
													}}
													onKeyDown={(e: React.KeyboardEvent) => {
														if (isKeyboardActivationKey(e.key)) {
															UserProfileCommands.openUserProfile(user.id);
															PersonaProfileMobile.close();
															e.preventDefault();
														}
													}}
												>
													<Avatar
														size={32}
														user={user}
														className={previewStyles.mainProfileButtonAvatar}
													/>
													<div className={previewStyles.mainProfileLabelContainer}>
														<div className={previewStyles.mainProfileHint}><Trans>View main profile</Trans></div>
														<div className={previewStyles.mainProfileName}>{user.displayName}</div>
													</div>
												</div>
											</div>
										</div>
									</div>
								</div>
							</Scroller>
						)}
					</div>
				</BottomSheet>
				{!hidePrivateDetails && (
					<NoteEditSheet
						isOpen={noteSheetOpen}
						onClose={() => setNoteSheetOpen(false)}
						userId={user.id}
						initialNote={userNote}
						data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.note-edit-sheet"
					/>
				)}
				{/* <UserProfileActionsSheet
					isOpen={actionsSheetOpen}
					onClose={() => setActionsSheetOpen(false)}
					user={user}
					isCurrentUser={isCurrentUser}
					hasGuildProfile={false}
					showGlobalProfile={true}
					//onToggleProfileView={() => setShowGlobalProfile(!showGlobalProfile)}
					// guildId={guildId}
					// guildMember={guildMember}
					data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.user-profile-actions-sheet"
				/> */}
				<ExpressionInfoBottomSheet
					kind="emoji"
					isOpen={emojiInfoOpen}
					onClose={() => setEmojiInfoOpen(false)}
					emoji={selectedEmoji}
					data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.expression-info-bottom-sheet"
				/>
			</>
		);
	},
);
