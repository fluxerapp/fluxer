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

import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import {modal} from '@app/actions/ModalActionCreators';
import * as PrivateChannelActionCreators from '@app/actions/PrivateChannelActionCreators';
import * as RelationshipActionCreators from '@app/actions/RelationshipActionCreators';
import * as UserProfileActionCreators from '@app/actions/UserProfileActionCreators';
import {EmojiInfoBottomSheet} from '@app/components/bottomsheets/EmojiInfoBottomSheet';
import {
	CustomStatusDisplay,
	type EmojiPressData,
} from '@app/components/common/custom_status_display/CustomStatusDisplay';
import {ConfirmModal} from '@app/components/modals/ConfirmModal';
import {NoteEditSheet} from '@app/components/modals/NoteEditSheet';
import {UserProfileActionsSheet} from '@app/components/modals/UserProfileActionsSheet';
import styles from '@app/components/modals/UserProfileMobileSheet.module.css';
import {getContrastingNotchColor} from '@app/components/modals/UserProfileUtils';
import {UserSettingsModal} from '@app/components/modals/UserSettingsModal';
import {UserProfileBadges} from '@app/components/popouts/UserProfileBadges';
import {
	UserProfileBio,
	UserProfileConnections,
	UserProfileMembershipInfo,
	UserProfileRoles,
} from '@app/components/popouts/UserProfileShared';
import {BottomSheet} from '@app/components/uikit/bottom_sheet/BottomSheet';
import {Scroller} from '@app/components/uikit/Scroller';
import {Spinner} from '@app/components/uikit/Spinner';
import {StatusAwareAvatar} from '@app/components/uikit/StatusAwareAvatar';
import {useAutoplayExpandedProfileAnimations} from '@app/hooks/useAutoplayExpandedProfileAnimations';
import {Logger} from '@app/lib/Logger';
import type {ProfileRecord} from '@app/records/ProfileRecord';
import {UserRecord} from '@app/records/UserRecord';
import AuthenticationStore from '@app/stores/AuthenticationStore';
import ChannelStore from '@app/stores/ChannelStore';
import GuildMemberStore from '@app/stores/GuildMemberStore';
import MemberPresenceSubscriptionStore from '@app/stores/MemberPresenceSubscriptionStore';
import PermissionStore from '@app/stores/PermissionStore';
import RelationshipStore from '@app/stores/RelationshipStore';
import SelectedChannelStore from '@app/stores/SelectedChannelStore';
import UserNoteStore from '@app/stores/UserNoteStore';
import UserProfileMobileStore from '@app/stores/UserProfileMobileStore';
import UserProfileStore from '@app/stores/UserProfileStore';
import UserStore from '@app/stores/UserStore';
import {getUserAccentColor} from '@app/utils/AccentColorUtils';
import * as CallUtils from '@app/utils/CallUtils';
import * as NicknameUtils from '@app/utils/NicknameUtils';
import * as ProfileDisplayUtils from '@app/utils/ProfileDisplayUtils';
import {createMockProfile} from '@app/utils/ProfileUtils';
import {ME} from '@fluxer/constants/src/AppConstants';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {
	MEDIA_PROXY_AVATAR_SIZE_PROFILE,
	MEDIA_PROXY_PROFILE_BANNER_SIZE_MODAL,
} from '@fluxer/constants/src/MediaProxyAssetSizes';
import {PublicUserFlags, RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {Trans, useLingui} from '@lingui/react/macro';
import {
	ChatTeardropIcon,
	CheckCircleIcon,
	ClockCounterClockwiseIcon,
	DotsThreeIcon,
	NotePencilIcon,
	PencilIcon,
	ProhibitIcon,
	UserMinusIcon,
	UserPlusIcon,
	VideoCameraIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useMemo, useState} from 'react';
import {Sheet} from 'react-modal-sheet';

const logger = new Logger('UserProfileMobileSheet');

export const UserProfileMobileSheet: React.FC = observer(function UserProfileMobileSheet() {
	const store = UserProfileMobileStore;
	const {userId, guildId: explicitGuildId, autoFocusNote, isOpen} = store;

	const selectedChannelId = SelectedChannelStore.currentChannelId;
	const selectedChannel = selectedChannelId ? ChannelStore.getChannel(selectedChannelId) : null;
	const channelGuildId =
		selectedChannel?.guildId && selectedChannel.guildId !== ME ? selectedChannel.guildId : undefined;
	const guildId = explicitGuildId ?? channelGuildId;

	const storeUser = userId ? UserStore.getUser(userId) : null;
	const user = storeUser;

	const fallbackUser = useMemo(
		() =>
			userId
				? new UserRecord({
						id: userId,
						username: userId,
						discriminator: '0000',
						global_name: null,
						avatar: null,
						avatar_color: null,
						flags: 0,
					})
				: null,
		[userId],
	);

	const displayUser = user ?? fallbackUser;
	const fallbackProfile = useMemo(() => (fallbackUser ? createMockProfile(fallbackUser) : null), [fallbackUser]);
	const mockProfile = useMemo(() => (user ? createMockProfile(user) : null), [user]);
	const initialProfile = useMemo(
		() => (userId ? UserProfileStore.getProfile(userId, guildId) : null),
		[userId, guildId],
	);
	const [profile, setProfile] = useState<ProfileRecord | null>(initialProfile);
	const [isProfileLoading, setIsProfileLoading] = useState(() => !initialProfile);
	const profileMatchesContext = profile?.userId === userId && (profile?.guildId ?? null) === (guildId ?? null);
	const activeProfile = profileMatchesContext ? profile : initialProfile;
	const isContextSwitching = Boolean(userId) && !activeProfile && !profileMatchesContext;
	const shouldShowProfileLoading = (isProfileLoading && !activeProfile) || isContextSwitching;

	useEffect(() => {
		setProfile(initialProfile);
		setIsProfileLoading(!initialProfile);
	}, [initialProfile]);

	useEffect(() => {
		if (!userId || activeProfile) {
			setIsProfileLoading(false);
			return;
		}

		let cancelled = false;
		setIsProfileLoading(true);

		UserProfileActionCreators.fetch(userId, guildId)
			.then(() => {
				if (cancelled) return;
				const fetchedProfile = UserProfileStore.getProfile(userId, guildId);
				if (fetchedProfile) {
					setProfile(fetchedProfile);
				}
			})
			.catch((error) => {
				if (cancelled) return;
				logger.error('Failed to fetch user profile:', error);
			})
			.finally(() => {
				if (cancelled) return;
				setIsProfileLoading(false);
			});

		return () => {
			cancelled = true;
		};
	}, [userId, guildId, activeProfile]);

	useEffect(() => {
		if (!guildId || !userId) {
			return;
		}

		const hasMember = GuildMemberStore.getMember(guildId, userId);
		if (!hasMember) {
			MemberPresenceSubscriptionStore.touchMember(guildId, userId);
			GuildMemberStore.fetchMembers(guildId, {userIds: [userId]}).catch((error) => {
				logger.error(' Failed to fetch guild member:', error);
			});
		} else {
			MemberPresenceSubscriptionStore.touchMember(guildId, userId);
		}

		return () => {
			MemberPresenceSubscriptionStore.unsubscribe(guildId, userId);
		};
	}, [guildId, userId]);

	if (!isOpen || !displayUser) {
		return null;
	}

	const effectiveProfile: ProfileRecord | null = activeProfile ?? mockProfile ?? fallbackProfile;
	const resolvedProfile: ProfileRecord = effectiveProfile ?? fallbackProfile!;
	const userNote = userId ? UserNoteStore.getUserNote(userId) : null;

	const handleClose = () => {
		store.close();
	};
	const profileIdentityKey = `${displayUser.id}:${guildId ?? 'global'}`;

	return (
		<UserProfileMobileSheetContent
			key={profileIdentityKey}
			user={displayUser}
			profile={resolvedProfile}
			userNote={userNote}
			guildId={guildId}
			autoFocusNote={autoFocusNote}
			isLoading={shouldShowProfileLoading}
			onClose={handleClose}
		/>
	);
});

interface UserProfileMobileSheetContentProps {
	user: UserRecord;
	profile: ProfileRecord;
	userNote: string | null;
	guildId?: string;
	autoFocusNote?: boolean;
	isLoading: boolean;
	onClose: () => void;
}

interface EmojiInfoState {
	id?: string;
	name: string;
	animated?: boolean;
}

const UserProfileMobileSheetContent: React.FC<UserProfileMobileSheetContentProps> = observer(
	function UserProfileMobileSheetContent({user, profile, userNote, guildId, autoFocusNote, isLoading, onClose}) {
		const {t} = useLingui();
		const [noteSheetOpen, setNoteSheetOpen] = useState(false);
		const [actionsSheetOpen, setActionsSheetOpen] = useState(false);
		const [showGlobalProfile, setShowGlobalProfile] = useState(false);
		const [emojiInfoOpen, setEmojiInfoOpen] = useState(false);
		const [selectedEmoji, setSelectedEmoji] = useState<EmojiInfoState | null>(null);
		const isCurrentUser = user.id === AuthenticationStore.currentUserId;
		const relationship = RelationshipStore.getRelationship(user.id);
		const relationshipType = relationship?.type;
		const isBlocked = relationshipType === RelationshipTypes.BLOCKED;
		const currentUserUnclaimed = !(UserStore.currentUser?.isClaimed() ?? true);

		const guildMember = GuildMemberStore.getMember(profile?.guildId ?? guildId ?? '', user.id);
		const memberRoles = profile?.guildId && guildMember ? guildMember.getSortedRoles() : [];
		const canManageRoles = PermissionStore.can(Permissions.MANAGE_ROLES, {guildId: profile?.guildId ?? guildId});

		const hasGuildProfile = !!(profile?.guildId && profile?.guildMemberProfile);
		const shouldShowGuildProfile = hasGuildProfile && !showGlobalProfile;

		const profileContext = useMemo<ProfileDisplayUtils.ProfileDisplayContext>(
			() => ({
				user,
				profile,
				guildId: shouldShowGuildProfile ? profile?.guildId : undefined,
				guildMember: shouldShowGuildProfile ? guildMember : undefined,
				guildMemberProfile: shouldShowGuildProfile ? profile?.guildMemberProfile : undefined,
			}),
			[user, profile, guildMember, shouldShowGuildProfile],
		);

		const shouldAutoplayProfileAnimations = useAutoplayExpandedProfileAnimations();

		const {avatarUrl, hoverAvatarUrl} = useMemo(
			() => ProfileDisplayUtils.getProfileAvatarUrls(profileContext, undefined, MEDIA_PROXY_AVATAR_SIZE_PROFILE),
			[profileContext],
		);

		const bannerUrl = useMemo(
			() =>
				ProfileDisplayUtils.getProfileBannerUrl(
					profileContext,
					undefined,
					shouldAutoplayProfileAnimations,
					MEDIA_PROXY_PROFILE_BANNER_SIZE_MODAL,
				),
			[profileContext, shouldAutoplayProfileAnimations],
		);

		const effectiveProfile = useMemo(() => {
			if (showGlobalProfile) {
				return profile?.userProfile ?? null;
			}
			return profile?.getEffectiveProfile() ?? null;
		}, [profile, showGlobalProfile]);

		const displayGuildId = shouldShowGuildProfile ? guildId : undefined;

		const bannerColor = useMemo(
			() => getUserAccentColor(user, effectiveProfile?.accent_color),
			[user, effectiveProfile],
		);

		useEffect(() => {
			if (autoFocusNote) {
				setNoteSheetOpen(true);
			}
		}, [autoFocusNote]);

		const notchColor = useMemo(
			() => getContrastingNotchColor(effectiveProfile?.banner_color ?? null, !!bannerUrl),
			[effectiveProfile?.banner_color, bannerUrl],
		);

		const handleMessage = async () => {
			try {
				onClose();
				await PrivateChannelActionCreators.openDMChannel(user.id);
			} catch (error) {
				logger.error('Failed to open DM channel:', error);
			}
		};

		const handleOpenBlockedDm = () => {
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Open DM`}
						description={t`You blocked ${user.username}. You won't be able to send messages unless you unblock them.`}
						primaryText={t`Open DM`}
						primaryVariant="primary"
						onPrimary={handleMessage}
					/>
				)),
			);
		};

		const handleSendFriendRequest = () => {
			RelationshipActionCreators.sendFriendRequest(user.id);
		};

		const handleAcceptFriendRequest = () => {
			RelationshipActionCreators.acceptFriendRequest(user.id);
		};

		const handleCancelFriendRequest = () => {
			RelationshipActionCreators.removeRelationship(user.id);
		};

		const handleRemoveFriend = () => {
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Remove Friend`}
						description={t`Are you sure you want to remove ${user.username} as a friend?`}
						primaryText={t`Remove Friend`}
						primaryVariant="danger-primary"
						onPrimary={async () => {
							RelationshipActionCreators.removeRelationship(user.id);
						}}
					/>
				)),
			);
		};

		const handleUnblockUser = () => {
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Unblock User`}
						description={t`Are you sure you want to unblock ${user.username}?`}
						primaryText={t`Unblock`}
						primaryVariant="primary"
						onPrimary={async () => {
							RelationshipActionCreators.removeRelationship(user.id);
						}}
					/>
				)),
			);
		};

		const handleEditProfile = () => {
			ModalActionCreators.push(modal(() => <UserSettingsModal initialTab="my_profile" />));
		};

		const handleStartVoiceCall = async () => {
			try {
				const channelId = await PrivateChannelActionCreators.ensureDMChannel(user.id);
				await CallUtils.checkAndStartCall(channelId);
			} catch (error) {
				logger.error('Failed to start voice call:', error);
			}
		};

		const handleStartVideoCall = async () => {
			try {
				const channelId = await PrivateChannelActionCreators.ensureDMChannel(user.id);
				await CallUtils.checkAndStartCall(channelId);
			} catch (error) {
				logger.error('Failed to start video call:', error);
			}
		};

		const handleEmojiPress = (emoji: EmojiPressData) => {
			setSelectedEmoji({
				id: emoji.id ?? undefined,
				name: emoji.name,
				animated: emoji.animated,
			});
			setEmojiInfoOpen(true);
		};

		const renderRelationshipButton = () => {
			const isFriendlyBot = user.bot && (user.flags & PublicUserFlags.FRIENDLY_BOT) === PublicUserFlags.FRIENDLY_BOT;
			if (isCurrentUser || (user.bot && !isFriendlyBot)) return null;

			if (relationshipType === RelationshipTypes.FRIEND) {
				return (
					<button type="button" onClick={handleRemoveFriend} className={styles.actionButton}>
						<UserMinusIcon className={styles.icon} />
					</button>
				);
			}
			if (relationshipType === RelationshipTypes.BLOCKED) {
				return (
					<button type="button" onClick={handleUnblockUser} className={styles.actionButton}>
						<ProhibitIcon className={styles.icon} />
					</button>
				);
			}
			if (relationshipType === RelationshipTypes.INCOMING_REQUEST) {
				return (
					<button type="button" onClick={handleAcceptFriendRequest} className={styles.actionButton}>
						<CheckCircleIcon className={styles.icon} />
					</button>
				);
			}
			if (relationshipType === RelationshipTypes.OUTGOING_REQUEST) {
				return (
					<button type="button" onClick={handleCancelFriendRequest} className={styles.actionButton}>
						<ClockCounterClockwiseIcon className={styles.icon} />
					</button>
				);
			}
			if (relationshipType === undefined && !currentUserUnclaimed) {
				return (
					<button type="button" onClick={handleSendFriendRequest} className={styles.actionButton}>
						<UserPlusIcon className={styles.icon} />
					</button>
				);
			}
			return null;
		};

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
				>
					<div className={styles.container}>
						{isLoading ? (
							<div className={styles.loadingScreen}>
								<Spinner size="large" />
							</div>
						) : (
							<Scroller key="user-profile-mobile-sheet-scroller">
								<div style={{paddingBottom: 'calc(env(safe-area-inset-bottom, 0px) + 1rem)'}}>
									<div className={styles.bannerContainer}>
										{bannerUrl ? (
											<div className={styles.bannerImage} style={{backgroundImage: `url(${bannerUrl})`}} />
										) : (
											<div className={styles.bannerColor} style={{backgroundColor: bannerColor}} />
										)}

										<Sheet.Header className={styles.notchContainer} disableDrag={false}>
											<div className={styles.notch} style={{backgroundColor: notchColor}} />
										</Sheet.Header>
									</div>

									<div className={styles.profileContent}>
										<div className={styles.avatarContainer}>
											<div className={styles.avatarBorder} style={{borderRadius: '9999px'}}>
												<StatusAwareAvatar
													size={80}
													user={user}
													avatarUrl={avatarUrl}
													hoverAvatarUrl={hoverAvatarUrl}
												/>
											</div>
										</div>

										<div className={styles.contentPadding}>
											<div className={styles.actionsContainer}>
												{!isCurrentUser && renderRelationshipButton()}
												<button type="button" onClick={() => setActionsSheetOpen(true)} className={styles.actionButton}>
													<DotsThreeIcon className={styles.icon} weight="bold" />
												</button>
											</div>

											<div className={styles.usernameContainer}>
												<div className={styles.usernameRow}>
													<span className={styles.username}>{NicknameUtils.getNickname(user, displayGuildId)}</span>
													{NicknameUtils.getNickname(user, displayGuildId) === user.username && (
														<span className={styles.discriminator}>#{user.discriminator}</span>
													)}
												</div>
												<div className={styles.tagBadgeRow}>
													{NicknameUtils.getNickname(user, displayGuildId) !== user.username && (
														<span className={styles.fullTag}>
															{user.username}#{user.discriminator}
														</span>
													)}
													<div className={styles.badgesWrapper}>
														<UserProfileBadges user={user} profile={profile} isModal={true} isMobile={true} />
													</div>
												</div>
												<div className={styles.customStatusRow}>
													<CustomStatusDisplay
														userId={user.id}
														className={styles.customStatusText}
														showTooltip
														allowJumboEmoji
														maxLines={0}
														alwaysAnimate={shouldAutoplayProfileAnimations}
														onEmojiPress={isCurrentUser ? undefined : handleEmojiPress}
													/>
												</div>
											</div>

											{isCurrentUser ? (
												<div className={styles.actionButtonsContainer}>
													<button type="button" onClick={handleEditProfile} className={styles.editProfileButton}>
														<PencilIcon className={styles.editProfileIcon} />
														<span className={styles.editProfileText}>
															<Trans>Edit Profile</Trans>
														</span>
													</button>
												</div>
											) : (
												<div className={styles.actionButtonsContainer}>
													<button
														type="button"
														onClick={isBlocked ? handleOpenBlockedDm : handleMessage}
														className={styles.actionCard}
													>
														<div className={styles.actionIconContainer}>
															<ChatTeardropIcon className={styles.actionIcon} />
														</div>
														<span className={styles.actionLabel}>
															{isBlocked ? <Trans>Open DM</Trans> : <Trans>Message</Trans>}
														</span>
													</button>
													{relationshipType === RelationshipTypes.FRIEND && !user.bot && (
														<>
															<button type="button" onClick={handleStartVoiceCall} className={styles.actionCard}>
																<div className={styles.actionIconContainerSecondary}>
																	<VideoCameraIcon className={styles.actionIconSecondary} />
																</div>
																<span className={styles.actionLabel}>
																	<Trans>Voice Call</Trans>
																</span>
															</button>
															<button type="button" onClick={handleStartVideoCall} className={styles.actionCard}>
																<div className={styles.actionIconContainerSecondary}>
																	<VideoCameraIcon className={styles.actionIconSecondary} />
																</div>
																<span className={styles.actionLabel}>
																	<Trans>Video Call</Trans>
																</span>
															</button>
														</>
													)}
												</div>
											)}

											{profile && (effectiveProfile?.bio || profile) && (
												<div className={styles.infoCard}>
													{effectiveProfile?.bio && (
														<div className={styles.bioSection}>
															<h3 className={styles.bioHeader}>
																<Trans>About Me</Trans>
															</h3>
															<UserProfileBio profile={profile} profileData={effectiveProfile} />
														</div>
													)}
													<UserProfileMembershipInfo profile={profile} user={user} />
													<div className={styles.rolesSection}>
														<UserProfileRoles
															profile={profile}
															user={user}
															memberRoles={[...memberRoles]}
															canManageRoles={canManageRoles}
														/>
													</div>
													<UserProfileConnections profile={profile} variant="mobile" />
												</div>
											)}

											<button type="button" onClick={() => setNoteSheetOpen(true)} className={styles.noteButton}>
												<div>
													<h3 className={styles.noteTitle}>
														<Trans>Note</Trans>
													</h3>
													<p className={styles.noteSubtitle}>
														<Trans>(only visible to you)</Trans>
													</p>
													{userNote && <p className={styles.noteText}>{userNote}</p>}
												</div>
												<div className={styles.noteIconContainer}>
													<NotePencilIcon className={styles.noteIcon} />
												</div>
											</button>
										</div>
									</div>
								</div>
							</Scroller>
						)}
					</div>
				</BottomSheet>

				<NoteEditSheet
					isOpen={noteSheetOpen}
					onClose={() => setNoteSheetOpen(false)}
					userId={user.id}
					initialNote={userNote}
				/>
				<UserProfileActionsSheet
					isOpen={actionsSheetOpen}
					onClose={() => setActionsSheetOpen(false)}
					user={user}
					isCurrentUser={isCurrentUser}
					hasGuildProfile={hasGuildProfile}
					showGlobalProfile={showGlobalProfile}
					onToggleProfileView={() => setShowGlobalProfile(!showGlobalProfile)}
					guildId={guildId}
					guildMember={guildMember}
				/>
				<EmojiInfoBottomSheet isOpen={emojiInfoOpen} onClose={() => setEmojiInfoOpen(false)} emoji={selectedEmoji} />
			</>
		);
	},
);
