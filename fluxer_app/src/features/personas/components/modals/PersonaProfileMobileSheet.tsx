// SPDX-License-Identifier: AGPL-3.0-or-later

import {useAnimatedImageUrl} from '@app/features/app/hooks/useAnimatedImageUrl';
import Authentication from '@app/features/auth/state/Authentication';
import {GifIndicator} from '@app/features/channel/components/embeds/media/GifIndicator';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import * as ColorUtils from '@app/features/theme/utils/ColorUtils';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Avatar} from '@app/features/ui/components/Avatar';
import {Scroller} from '@app/features/ui/components/Scroller';
import {Spinner} from '@app/features/ui/components/Spinner';
import * as Sheet from '@app/features/ui/sheet/Sheet';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import {NoteEditSheet} from '@app/features/user/components/modals/NoteEditSheet';
import {PersonaEditorModal} from '@app/features/user/components/modals/PersonaEditorModal';
import styles from '@app/features/user/components/modals/UserProfileMobileSheet.module.css';
import {getContrastingNotchColor} from '@app/features/user/components/modals/UserProfileUtils';
import {UserProfileBio} from '@app/features/user/components/popouts/UserProfileShared';
import previewStyles from '@app/features/user/components/profile/ProfilePreview.module.css';
import {Profile} from '@app/features/user/models/Profile';
import {User} from '@app/features/user/models/User';
import Personas from '@app/features/user/state/Personas';
import UserNote from '@app/features/user/state/UserNote';
import Users from '@app/features/user/state/Users';
import {getDefaultAvatarPrimaryColor} from '@app/features/user/utils/AvatarUtils';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import * as ProfileDisplayUtils from '@app/features/user/utils/ProfileDisplayUtils';
import {resolveProfileGuildMembership, toProfileDisplayContext} from '@app/features/user/utils/ProfileGuildMembership';
import {createMockProfile} from '@app/features/user/utils/ProfileUtils';
import {
	MEDIA_PROXY_AVATAR_SIZE_DEFAULT,
	MEDIA_PROXY_PROFILE_BANNER_SIZE_MODAL,
} from '@fluxer/constants/src/MediaProxyAssetSizes';
import {Trans} from '@lingui/react/macro';
import {PencilIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo, useState} from 'react';
import {emptyOwnPersona} from '../../models/Persona';
import PersonaProfileMobile from '../../state/PersonaProfileMobile';

export const PersonaProfileMobileSheet: React.FC = observer(function PersonaProfileMobileSheet() {
	const store = PersonaProfileMobile;
	const {userId, personaId, autoFocusNote, isOpen} = store;
	const persona =
		(personaId ? Personas.getPersona(personaId) : null) || PersonaProfileMobile.initialPersona || emptyOwnPersona;
	const storeUser = userId ? Users.getUser(userId) : null;
	const user = storeUser;
	const fallbackUser = useMemo(
		() =>
			new User({
				id: userId || '0',
				username: userId || '0',
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
	const profile: Profile = new Profile({
		user: {...displayUser, global_name: displayUser.username || '0', avatar_color: displayUser.avatarColor || null},
		timezone_offset: null,
		user_profile: {
			accent_color: persona.accent_color || null,
			banner: persona.banner || null,
			bio: persona.bio || null,
			pronouns: persona.pronouns || null,
			banner_color: persona.banner_color || null,
		},
	});
	const isContextSwitching = Boolean(userId) && !profile;
	const shouldShowProfileLoading = !profile || isContextSwitching;

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

// interface EmojiInfoState {
// 	id?: string;
// 	name: string;
// 	animated?: boolean;
// }

const PersonaProfileMobileSheetContent: React.FC<PersonaProfileMobileSheetContentProps> = observer(
	function PersonaProfileMobileSheetContent({user, profile, personaId, userNote, isLoading, onClose}) {
		//const {i18n} = useLingui();
		const [noteSheetOpen, setNoteSheetOpen] = useState(false);
		//const [showGlobalProfile, setShowGlobalProfile] = useState(false);
		//const [emojiInfoOpen, setEmojiInfoOpen] = useState(false);
		//const [selectedEmoji, setSelectedEmoji] = useState<EmojiInfoState | null>(null);
		const hidePrivateDetails = StreamerMode.shouldHidePersonalInformation;
		const isCurrentUser = user.id === Authentication.currentUserId;
		//const relationship = Relationships.getRelationship(user.id);
		//const relationshipType = relationship?.type;
		//const isBlocked = relationshipType === RelationshipTypes.BLOCKED;
		const currentUserUnclaimed = !(Users.currentUser?.isClaimed() ?? true);
		const persona = (personaId ? Personas.getPersona(personaId) : null) || PersonaProfileMobile.initialPersona || null;
		const profileContext = useMemo<ProfileDisplayUtils.ProfileDisplayContext>(
			() =>
				toProfileDisplayContext({
					user,
					profile,
					membership: resolveProfileGuildMembership(profile),
					guildMemberProfile: undefined,
				}),
			[user, profile],
		);
		//const shouldAutoplayProfileAnimations = useAutoplayExpandedProfileAnimations();
		const personaContext: ProfileDisplayUtils.ProfileDisplayContext = {
			...profileContext,
			user: profileContext.user.withUpdates({
				id: persona?.id ?? user.id,
				avatar: persona?.avatar ?? null,
				banner: persona?.banner ?? null,
			}),
		};
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
		const displayName = persona?.display_name || 'Persona';
		const isDisplayNameUsername = displayName === user.username;
		const bannerColor = ColorUtils.int2hex(
			profile.userProfile?.accent_color ??
				persona?.accent_color ??
				getDefaultAvatarPrimaryColor(persona?.id ?? user.id),
		);
		const notchColor = useMemo(
			() => getContrastingNotchColor(effectiveProfile?.banner_color ?? null, !!bannerUrl),
			[effectiveProfile?.banner_color, bannerUrl],
		);
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
											></div>
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
											</div>
											{isCurrentUser
												? !currentUserUnclaimed && (
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
												: null}
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
											<div
												className={styles.actionButtonsContainer}
												data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.main-profile-action-buttons-container"
											>
												<div
													role="button"
													tabIndex={0}
													className={clsx(
														styles.editProfileButton,
														previewStyles.mainProfileMobileButton,
														previewStyles.mainProfileButton,
													)}
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
													data-flx="personas.persona-profile-mobile-sheet.persona-profile-mobile-sheet-content.edit-profile-button.open-user-profile"
												>
													<Avatar
														size={32}
														user={user}
														className={previewStyles.mainProfileButtonAvatar}
														data-flx="personas.persona-profile-mobile-sheet.persona-profile-mobile-sheet-content.avatar"
													/>
													<div
														className={previewStyles.mainProfileLabelContainer}
														data-flx="personas.persona-profile-mobile-sheet.persona-profile-mobile-sheet-content.div"
													>
														<div
															className={previewStyles.mainProfileHint}
															data-flx="personas.persona-profile-mobile-sheet.persona-profile-mobile-sheet-content.div--2"
														>
															<Trans>View main profile</Trans>
														</div>
														<div
															className={previewStyles.mainProfileName}
															data-flx="personas.persona-profile-mobile-sheet.persona-profile-mobile-sheet-content.div--3"
														>
															{user.displayName}
														</div>
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
				{/* <ExpressionInfoBottomSheet
					kind="emoji"
					isOpen={emojiInfoOpen}
					onClose={() => setEmojiInfoOpen(false)}
					emoji={selectedEmoji}
					data-flx="user.user-profile-mobile-sheet.user-profile-mobile-sheet-content.expression-info-bottom-sheet"
				/> */}
			</>
		);
	},
);
