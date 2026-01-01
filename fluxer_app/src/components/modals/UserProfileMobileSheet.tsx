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
import React from 'react';
import {Sheet} from 'react-modal-sheet';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as PrivateChannelActionCreators from '~/actions/PrivateChannelActionCreators';
import * as RelationshipActionCreators from '~/actions/RelationshipActionCreators';
import * as UserProfileActionCreators from '~/actions/UserProfileActionCreators';
import {DEFAULT_ACCENT_COLOR, ME, Permissions, RelationshipTypes} from '~/Constants';
import {EmojiInfoBottomSheet} from '~/components/bottomsheets/EmojiInfoBottomSheet';
import {CustomStatusDisplay, type EmojiPressData} from '~/components/common/CustomStatusDisplay/CustomStatusDisplay';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {NoteEditSheet} from '~/components/modals/NoteEditSheet';
import {UserProfileActionsSheet} from '~/components/modals/UserProfileActionsSheet';
import {UserSettingsModal} from '~/components/modals/UserSettingsModal';
import {getContrastingNotchColor} from '~/components/modals/userProfileUtils';
import {UserProfileBadges} from '~/components/popouts/UserProfileBadges';
import {UserProfileBio, UserProfileMembershipInfo, UserProfileRoles} from '~/components/popouts/UserProfileShared';
import {BottomSheet} from '~/components/uikit/BottomSheet/BottomSheet';
import {Scroller} from '~/components/uikit/Scroller';
import {Spinner} from '~/components/uikit/Spinner';
import {StatusAwareAvatar} from '~/components/uikit/StatusAwareAvatar';
import {useAutoplayExpandedProfileAnimations} from '~/hooks/useAutoplayExpandedProfileAnimations';
import type {ProfileRecord} from '~/records/ProfileRecord';
import {UserRecord} from '~/records/UserRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import ChannelStore from '~/stores/ChannelStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import MemberPresenceSubscriptionStore from '~/stores/MemberPresenceSubscriptionStore';
import PermissionStore from '~/stores/PermissionStore';
import RelationshipStore from '~/stores/RelationshipStore';
import SelectedChannelStore from '~/stores/SelectedChannelStore';
import UserNoteStore from '~/stores/UserNoteStore';
import UserProfileMobileStore from '~/stores/UserProfileMobileStore';
import UserProfileStore from '~/stores/UserProfileStore';
import UserStore from '~/stores/UserStore';
import * as CallUtils from '~/utils/CallUtils';
import * as ColorUtils from '~/utils/ColorUtils';
import * as NicknameUtils from '~/utils/NicknameUtils';
import * as ProfileDisplayUtils from '~/utils/ProfileDisplayUtils';
import {createMockProfile} from '~/utils/ProfileUtils';
import styles from './UserProfileMobileSheet.module.css';

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

	const fallbackUser = React.useMemo(
		() =>
			userId
				? new UserRecord({
						id: userId,
						username: userId,
						discriminator: '0000',
						avatar: null,
						flags: 0,
					})
				: null,
		[userId],
	);

	const displayUser = user ?? fallbackUser;
	const fallbackProfile = React.useMemo(() => (fallbackUser ? createMockProfile(fallbackUser) : null), [fallbackUser]);
	const mockProfile = React.useMemo(() => (user ? createMockProfile(user) : null), [user]);
	const initialProfile = React.useMemo(
		() => (userId ? UserProfileStore.getProfile(userId, guildId) : null),
		[userId, guildId],
	);
	const [profile, setProfile] = React.useState<ProfileRecord | null>(initialProfile);
	const [isProfileLoading, setIsProfileLoading] = React.useState(() => !initialProfile);

	React.useEffect(() => {
		setProfile(initialProfile);
		setIsProfileLoading(!initialProfile);
	}, [initialProfile]);

	React.useEffect(() => {
		if (!userId || profile) {
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
				console.error('Failed to fetch user profile:', error);
			})
			.finally(() => {
				if (cancelled) return;
				setIsProfileLoading(false);
			});

		return () => {
			cancelled = true;
		};
	}, [userId, guildId, profile]);

	React.useEffect(() => {
		if (!guildId || !userId) {
			return;
		}

		const hasMember = GuildMemberStore.getMember(guildId, userId);
		if (!hasMember) {
			MemberPresenceSubscriptionStore.touchMember(guildId, userId);
			GuildMemberStore.fetchMembers(guildId, {userIds: [userId]});
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

	const effectiveProfile: ProfileRecord | null = profile ?? mockProfile ?? fallbackProfile;
	const resolvedProfile: ProfileRecord = effectiveProfile ?? fallbackProfile!;
	const userNote = userId ? UserNoteStore.getUserNote(userId) : null;

	const handleClose = () => {
		store.close();
	};

	return (
		<UserProfileMobileSheetContent
			user={displayUser}
			profile={resolvedProfile}
			userNote={userNote}
			guildId={guildId}
			autoFocusNote={autoFocusNote}
			isLoading={isProfileLoading}
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
		const [noteSheetOpen, setNoteSheetOpen] = React.useState(false);
		const [actionsSheetOpen, setActionsSheetOpen] = React.useState(false);
		const [showGlobalProfile, setShowGlobalProfile] = React.useState(false);
		const [emojiInfoOpen, setEmojiInfoOpen] = React.useState(false);
		const [selectedEmoji, setSelectedEmoji] = React.useState<EmojiInfoState | null>(null);
		const isCurrentUser = user.id === AuthenticationStore.currentUserId;
		const relationship = RelationshipStore.getRelationship(user.id);
		const relationshipType = relationship?.type;

		const guildMember = GuildMemberStore.getMember(profile?.guildId ?? guildId ?? '', user.id);
		const memberRoles = profile?.guildId && guildMember ? guildMember.getSortedRoles() : [];
		const canManageRoles = PermissionStore.can(Permissions.MANAGE_ROLES, {guildId: profile?.guildId ?? guildId});

		const hasGuildProfile = !!(profile?.guildId && profile?.guildMemberProfile);
		const shouldShowGuildProfile = hasGuildProfile && !showGlobalProfile;

		const profileContext = React.useMemo<ProfileDisplayUtils.ProfileDisplayContext>(
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

		const {avatarUrl, hoverAvatarUrl} = React.useMemo(
			() => ProfileDisplayUtils.getProfileAvatarUrls(profileContext),
			[profileContext],
		);

		const bannerUrl = React.useMemo(
			() => ProfileDisplayUtils.getProfileBannerUrl(profileContext, undefined, shouldAutoplayProfileAnimations),
			[profileContext, shouldAutoplayProfileAnimations],
		);

		const effectiveProfile = React.useMemo(() => {
			if (showGlobalProfile) {
				return profile?.userProfile ?? null;
			}
			return profile?.getEffectiveProfile() ?? null;
		}, [profile, showGlobalProfile]);

		const displayGuildId = shouldShowGuildProfile ? guildId : undefined;

		const bannerColor = React.useMemo(() => {
			const rawAccentColor = effectiveProfile?.accent_color ?? null;
			const accentColorHex = typeof rawAccentColor === 'number' ? ColorUtils.int2hex(rawAccentColor) : rawAccentColor;
			return accentColorHex || DEFAULT_ACCENT_COLOR;
		}, [effectiveProfile]);

		React.useEffect(() => {
			if (autoFocusNote) {
				setNoteSheetOpen(true);
			}
		}, [autoFocusNote]);

		const notchColor = React.useMemo(
			() => getContrastingNotchColor(effectiveProfile?.banner_color ?? null, !!bannerUrl),
			[effectiveProfile?.banner_color, bannerUrl],
		);

		const handleMessage = async () => {
			try {
				onClose();
				await PrivateChannelActionCreators.openDMChannel(user.id);
			} catch (error) {
				console.error('Failed to open DM channel:', error);
			}
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
				console.error('Failed to start voice call:', error);
			}
		};

		const handleStartVideoCall = async () => {
			try {
				const channelId = await PrivateChannelActionCreators.ensureDMChannel(user.id);
				await CallUtils.checkAndStartCall(channelId);
			} catch (error) {
				console.error('Failed to start video call:', error);
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
			if (isCurrentUser || user.bot) return null;

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
			return (
				<button type="button" onClick={handleSendFriendRequest} className={styles.actionButton}>
					<UserPlusIcon className={styles.icon} />
				</button>
			);
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
													<button type="button" onClick={handleMessage} className={styles.actionCard}>
														<div className={styles.actionIconContainer}>
															<ChatTeardropIcon className={styles.actionIcon} />
														</div>
														<span className={styles.actionLabel}>
															<Trans>Message</Trans>
														</span>
													</button>
													{relationshipType === RelationshipTypes.FRIEND && !user.bot && (
														<>
															<button type="button" onClick={handleStartVoiceCall} className={styles.actionCard}>
																<div className={styles.actionIconContainerSecondary}>
																	<PhoneIcon className={styles.actionIconSecondary} />
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
															forceMobile={true}
														/>
													</div>
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
