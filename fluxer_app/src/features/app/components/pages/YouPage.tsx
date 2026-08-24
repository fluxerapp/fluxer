// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/pages/YouPage.module.css';
import {CustomStatusDisplay} from '@app/features/app/components/shared/custom_status_display/CustomStatusDisplay';
import {useAnimatedImageUrl} from '@app/features/app/hooks/useAnimatedImageUrl';
import {OPEN_SETTINGS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Presence from '@app/features/presence/state/Presence';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Scroller} from '@app/features/ui/components/Scroller';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import {NoteEditSheet} from '@app/features/user/components/modals/NoteEditSheet';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import {UserProfileBadges} from '@app/features/user/components/popouts/UserProfileBadges';
import {UserProfileBio, UserProfileMembershipInfo} from '@app/features/user/components/popouts/UserProfileShared';
import {normalizeCustomStatus} from '@app/features/user/state/CustomStatus';
import UserNote from '@app/features/user/state/UserNote';
import Users from '@app/features/user/state/Users';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {createMockProfile} from '@app/features/user/utils/ProfileUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {GearIcon, NotePencilIcon, PencilIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useMemo, useState} from 'react';

const OPEN_AVATAR_DESCRIPTOR = msg({
	message: 'Open avatar',
	comment: 'Short label in the you page.',
});

interface YouPageProps {
	onAvatarClick: () => void;
}

export const YouPage = observer(({onAvatarClick}: YouPageProps) => {
	const {i18n} = useLingui();
	const user = Users.currentUser;
	const userNote = user ? UserNote.getUserNote(user.id) : '';
	const [noteSheetOpen, setNoteSheetOpen] = useState(false);
	const handleSettings = () => {
		ModalCommands.push(modal(() => <UserSettingsModal data-flx="app.you-page.handle-settings.user-settings-modal" />));
	};
	const handleEditProfile = () => {
		ModalCommands.push(
			modal(() => (
				<UserSettingsModal initialTab="my_profile" data-flx="app.you-page.handle-edit-profile.user-settings-modal" />
			)),
		);
	};
	const profile = useMemo(() => (user ? createMockProfile(user) : null), [user]);
	const normalizedCustomStatus = useMemo(() => {
		if (!user) return null;
		return normalizeCustomStatus(Presence.getCustomStatus(user.id));
	}, [user]);
	const hasCustomStatus = Boolean(normalizedCustomStatus);
	const userId = user?.id;
	const userBanner = user?.banner;
	const staticBannerUrl = useMemo(
		() => (userId && userBanner ? AvatarUtils.getUserBannerURL({id: userId, banner: userBanner}, false) : null),
		[userId, userBanner],
	);
	const animatedBannerUrl = useMemo(
		() => (userId && userBanner ? AvatarUtils.getUserBannerURL({id: userId, banner: userBanner}, true) : null),
		[userId, userBanner],
	);
	const {hoverRef: bannerHoverRef, imageUrl: bannerUrl} = useAnimatedImageUrl({
		staticUrl: staticBannerUrl,
		animatedUrl: animatedBannerUrl,
		kind: 'gif',
	});
	if (!user || !profile) return null;
	return (
		<>
			<div className={styles.container} data-flx="app.you-page.container">
				<Scroller key="you-page-scroller" data-flx="app.you-page.scroller">
					<div
						style={{paddingBottom: 'calc(60px + env(safe-area-inset-bottom, 0px) + 1rem)'}}
						data-flx="app.you-page.div"
					>
						<div ref={bannerHoverRef} className={styles.banner} data-flx="app.you-page.banner">
							{bannerUrl ? (
								<div
									className={styles.bannerImage}
									style={{backgroundImage: `url(${bannerUrl})`}}
									data-flx="app.you-page.banner-image"
								/>
							) : (
								<div className={styles.bannerDefault} data-flx="app.you-page.banner-default" />
							)}
						</div>
						<div className={styles.profile} data-flx="app.you-page.profile">
							<button
								type="button"
								onClick={onAvatarClick}
								className={styles.avatarButton}
								aria-label={i18n._(OPEN_AVATAR_DESCRIPTOR)}
								data-flx="app.you-page.avatar-button.avatar-click"
							>
								<StatusAwareAvatar size={80} user={user} data-flx="app.you-page.status-aware-avatar" />
							</button>
							<div className={styles.content} data-flx="app.you-page.content">
								<div className={styles.actions} data-flx="app.you-page.actions">
									<button
										type="button"
										onClick={handleSettings}
										className={styles.settingsButton}
										aria-label={i18n._(OPEN_SETTINGS_DESCRIPTOR)}
										data-flx="app.you-page.settings-button"
									>
										<GearIcon className={styles.settingsIcon} weight="fill" data-flx="app.you-page.settings-icon" />
									</button>
								</div>
								<div className={styles.userInfo} data-flx="app.you-page.user-info">
									<div className={styles.usernameRow} data-flx="app.you-page.username-row">
										<span className={styles.username} data-flx="app.you-page.username">
											{NicknameUtils.getDisplayName(user)}
										</span>
									</div>
									<div className={styles.tagBadgeRow} data-flx="app.you-page.tag-badge-row">
										<span className={styles.fullTag} data-flx="app.you-page.full-tag">
											{NicknameUtils.formatTagForStreamerMode(`${user.username}#${user.discriminator}`)}
										</span>
										<div className={styles.badgesWrapper} data-flx="app.you-page.badges-wrapper">
											<UserProfileBadges
												user={user}
												profile={profile}
												isModal={true}
												isMobile={true}
												data-flx="app.you-page.user-profile-badges"
											/>
										</div>
									</div>
									{hasCustomStatus && (
										<div className={styles.customStatusRow} data-flx="app.you-page.custom-status-row">
											<CustomStatusDisplay
												userId={user.id}
												className={styles.customStatusText}
												showTooltip
												allowJumboEmoji
												animateOnParentHover
												data-flx="app.you-page.custom-status-text"
											/>
										</div>
									)}
								</div>
								{user.isClaimed() && (
									<button
										type="button"
										onClick={handleEditProfile}
										className={styles.editButton}
										data-flx="app.you-page.edit-button.edit-profile"
									>
										<PencilIcon className={styles.editIcon} data-flx="app.you-page.edit-icon" />
										<span className={styles.editLabel} data-flx="app.you-page.edit-label">
											<Trans>Edit profile</Trans>
										</span>
									</button>
								)}
								{(profile?.userProfile.bio || profile) && (
									<div className={styles.section} data-flx="app.you-page.section">
										{profile?.userProfile.bio && (
											<div className={styles.sectionHeader} data-flx="app.you-page.section-header">
												<h3 className={styles.sectionTitle} data-flx="app.you-page.section-title">
													<Trans>About me</Trans>
												</h3>
												<UserProfileBio profile={profile} data-flx="app.you-page.user-profile-bio" />
											</div>
										)}
										<UserProfileMembershipInfo
											profile={profile}
											user={user}
											data-flx="app.you-page.user-profile-membership-info"
										/>
									</div>
								)}
								<button
									type="button"
									onClick={() => setNoteSheetOpen(true)}
									className={styles.noteButton}
									data-flx="app.you-page.note-button.set-note-sheet-open"
								>
									<div data-flx="app.you-page.div--2">
										<h3 className={styles.noteLabel} data-flx="app.you-page.note-label">
											<Trans>Note</Trans>
										</h3>
										<p className={styles.noteSubtext} data-flx="app.you-page.note-subtext">
											<Trans>(only visible to you)</Trans>
										</p>
										{userNote && (
											<p className={styles.noteText} data-flx="app.you-page.note-text">
												{userNote}
											</p>
										)}
									</div>
									<div className={styles.noteIconWrapper} data-flx="app.you-page.note-icon-wrapper">
										<NotePencilIcon className={styles.noteIcon} data-flx="app.you-page.note-icon" />
									</div>
								</button>
							</div>
						</div>
					</div>
				</Scroller>
			</div>
			<NoteEditSheet
				isOpen={noteSheetOpen}
				onClose={() => setNoteSheetOpen(false)}
				userId={user.id}
				initialNote={userNote}
				data-flx="app.you-page.note-edit-sheet"
			/>
		</>
	);
});
