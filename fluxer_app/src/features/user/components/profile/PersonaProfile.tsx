import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import type {Persona} from '@app/features/personas/models/Persona';
import {getUserAccentColor} from '@app/features/theme/utils/AccentColorUtils';
import {Avatar} from '@app/features/ui/components/Avatar';
import styles from '@app/features/user/components/profile/ProfilePreview.module.css';
import * as ProfileDisplayUtils from '@app/features/user/utils/ProfileDisplayUtils';
import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';
import {Profile} from '../../models/Profile';
import type {User} from '../../models/User';
import {UserProfileBio} from '../popouts/UserProfileShared';
import {ProfileCardBanner} from './profile_card/ProfileCardBanner';
import {ProfileCardContent} from './profile_card/ProfileCardContent';
import {ProfileCardLayout} from './profile_card/ProfileCardLayout';
import {ProfileCardUserInfo} from './profile_card/ProfileCardUserInfo';

interface PersonaProfileCardProps {
	user: User;
	persona: Persona;
	showPreviewLabel?: boolean;
	openFullMainProfile?: () => void;
}

export const PersonaProfileCard: React.FC<PersonaProfileCardProps> = observer(
	({user, persona, openFullMainProfile, ...props}) => {
		const borderColor = useMemo(() => getUserAccentColor(user, persona?.accent_color), [persona.accent_color]);
		const profile: Profile = new Profile({
			user: {...user, global_name: user.username, avatar_color: user.avatarColor || null},
			timezone_offset: null,
			user_profile: {
				accent_color: persona.accent_color || null,
				banner: persona.banner || null,
				bio: persona.bio || null,
				pronouns: persona.pronouns || null,
				banner_color: persona.banner_color || null,
			},
		});
		const profileContext: ProfileDisplayUtils.ProfileDisplayContext = {
			user: user.withUpdates({id: persona.id, avatar: persona.avatar, banner: persona.banner}),
			profile: profile,
		};
		const avatarUrls = useMemo(() => ProfileDisplayUtils.getProfileAvatarUrls(profileContext, {}), [profileContext]);
		const bannerUrls = useMemo(() => ProfileDisplayUtils.getProfileBannerUrls(profileContext, {}), [profileContext]);
		return (
			<ProfileCardLayout
				borderColor={borderColor}
				showPreviewLabel={props.showPreviewLabel}
				data-flx="user.profile.profile-preview.profile-card-layout"
			>
				<ProfileCardBanner
					bannerUrl={bannerUrls?.bannerUrl || null}
					hoverBannerUrl={bannerUrls?.hoverBannerUrl || null}
					bannerColor={borderColor}
					user={user}
					disablePresence={true}
					avatarUrl={avatarUrls?.avatarUrl || null}
					hoverAvatarUrl={avatarUrls?.hoverAvatarUrl || null}
					isClickable={false}
					//onAvatarClick={openMockProfile}
					data-flx="user.profile.profile-preview.profile-card-banner"
				/>
				<ProfileCardContent data-flx="user.profile.profile-preview.profile-card-content">
					<ProfileCardUserInfo
						displayName={persona.display_name || ''}
						user={user}
						pronouns={persona.pronouns}
						showUsername={false}
						isClickable={false}
						//onDisplayNameClick={() => {}}
						//onUsernameClick={openMockProfile}
						data-flx="user.profile.profile-preview.profile-card-user-info"
					/>
					<UserProfileBio
						profile={profile}
						profileData={profile.userProfile}
						shouldScroll
						data-flx="user.profile.profile-preview.user-profile-preview-bio"
					/>
					{/* <UserProfileTimezoneInfo
					profile={profile}
					data-flx="user.profile.profile-preview.user-profile-timezone-info"
				/> */}
					{/* TODO: "view main user profile" button (opens full view) */}
					<div
						role="button"
						tabIndex={0}
						className={styles.mainProfileButton}
						onClick={() => {
							openFullMainProfile?.();
						}}
						onKeyDown={(e: React.KeyboardEvent) => {
							if (isKeyboardActivationKey(e.key)) {
								openFullMainProfile?.();
								e.preventDefault();
							}
						}}
						data-flx="user.profile.persona-profile.persona-profile-card.main-profile-button.open-full-main-profile"
					>
						<Avatar
							size={32}
							user={user}
							className={styles.mainProfileButtonAvatar}
							data-flx="user.profile.persona-profile.persona-profile-card.main-profile-button-avatar"
						/>
						<div
							className={styles.mainProfileLabelContainer}
							data-flx="user.profile.persona-profile.persona-profile-card.main-profile-label-container"
						>
							<div
								className={styles.mainProfileHint}
								data-flx="user.profile.persona-profile.persona-profile-card.main-profile-hint"
							>
								<Trans>View main profile</Trans>
							</div>
							<div
								className={styles.mainProfileName}
								data-flx="user.profile.persona-profile.persona-profile-card.main-profile-name"
							>
								{user.displayName}
							</div>
						</div>
					</div>
				</ProfileCardContent>
			</ProfileCardLayout>
		);
	},
);
