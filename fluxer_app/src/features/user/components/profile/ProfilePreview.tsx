// SPDX-License-Identifier: AGPL-3.0-or-later

import {CustomStatusDisplay} from '@app/features/app/components/shared/custom_status_display/CustomStatusDisplay';
import Guilds from '@app/features/guild/state/Guilds';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {UserProfileModal} from '@app/features/user/components/modals/UserProfileModal';
import {UserProfileBadges} from '@app/features/user/components/popouts/UserProfileBadges';
import {
	UserProfileMembershipInfo,
	UserProfilePreviewBio,
	UserProfileTimezoneInfo,
} from '@app/features/user/components/popouts/UserProfileShared';
import styles from '@app/features/user/components/profile/ProfilePreview.module.css';
import {ProfileCardBanner} from '@app/features/user/components/profile/profile_card/ProfileCardBanner';
import {ProfileCardContent} from '@app/features/user/components/profile/profile_card/ProfileCardContent';
import {ProfileCardFooter} from '@app/features/user/components/profile/profile_card/ProfileCardFooter';
import {ProfileCardLayout} from '@app/features/user/components/profile/profile_card/ProfileCardLayout';
import {ProfileCardUserInfo} from '@app/features/user/components/profile/profile_card/ProfileCardUserInfo';
import {ProfileRichPresence} from '@app/features/user/components/profile/ProfileRichPresence';
import {useProfileCardDisplayState} from '@app/features/user/components/profile/useProfileCardDisplayState';
import {PROFILE_POPOUT_GEOMETRY_STYLE} from '@app/features/user/constants/UserProfileSurfaceGeometry';
import {useAutoplayExpandedProfileAnimations} from '@app/features/user/hooks/useAutoplayExpandedProfileAnimations';
import type {Profile} from '@app/features/user/models/Profile';
import type {User} from '@app/features/user/models/User';
import type {CustomStatus} from '@app/features/user/state/CustomStatus';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import type {ProfilePreviewOverrides} from '@app/features/user/utils/ProfileDisplayUtils';
import {type BadgeSettings, createMockProfile} from '@app/features/user/utils/ProfileUtils';
import type {UserProfile} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ChatTeardropIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

const PROFILE_PREVIEW_PRESS_ENTER_TO_OPEN_FULL_PREVIEW_DESCRIPTOR = msg({
	message: 'Profile preview (press Enter to open full preview)',
	comment: 'Label in the user settings profile preview.',
});
const YOU_CAN_T_MESSAGE_YOURSELF_DESCRIPTOR = msg({
	message: "You can't message yourself",
	comment: 'Error message in the user settings profile preview.',
});

interface ProfilePreviewProps {
	user: User;
	previewAvatarUrl?: string | null;
	previewBannerUrl?: string | null;
	hasClearedAvatar?: boolean;
	hasClearedBanner?: boolean;
	previewBio?: string | null;
	previewPronouns?: string | null;
	previewAccentColor?: number | null;
	previewTimezoneOffset?: number | null;
	previewGlobalName?: string | null;
	previewNick?: string | null;
	guildId?: string | null;
	guildMember?: GuildMember | null;
	guildMemberProfile?: UserProfile | null;
	previewBadgeSettings?: BadgeSettings;
	ignoreGuildAvatarInPreview?: boolean;
	ignoreGuildBannerInPreview?: boolean;
	showMembershipInfo?: boolean;
	showMessageButton?: boolean;
	showPreviewLabel?: boolean;
	previewCustomStatus?: CustomStatus | null;
}

export const ProfilePreview: React.FC<ProfilePreviewProps> = observer(
	({
		user,
		previewAvatarUrl,
		previewBannerUrl,
		hasClearedAvatar,
		hasClearedBanner,
		previewBio,
		previewPronouns,
		previewAccentColor,
		previewTimezoneOffset,
		previewGlobalName,
		previewNick,
		guildId,
		guildMember,
		guildMemberProfile,
		previewBadgeSettings,
		ignoreGuildAvatarInPreview,
		ignoreGuildBannerInPreview,
		showMembershipInfo = true,
		showMessageButton = true,
		showPreviewLabel = true,
		previewCustomStatus,
	}) => {
		const {i18n} = useLingui();
		const previewOverrides = useMemo<ProfilePreviewOverrides>(
			() => ({
				previewAvatarUrl,
				previewBannerUrl,
				hasClearedAvatar,
				hasClearedBanner,
				ignoreGuildAvatar: ignoreGuildAvatarInPreview,
				ignoreGuildBanner: ignoreGuildBannerInPreview,
			}),
			[
				previewAvatarUrl,
				previewBannerUrl,
				hasClearedAvatar,
				hasClearedBanner,
				ignoreGuildAvatarInPreview,
				ignoreGuildBannerInPreview,
			],
		);
		const isCommunityProfile = Boolean(guildId && guildMemberProfile);
		const previewUser = useMemo(() => {
			const globalName = previewGlobalName !== undefined ? previewGlobalName : user.globalName;
			if (isCommunityProfile) {
				return user.withUpdates({global_name: globalName});
			}
			const bio = previewBio !== undefined ? previewBio : user.bio;
			const pronouns = previewPronouns !== undefined ? previewPronouns : user.pronouns;
			return user.withUpdates({bio, pronouns, global_name: globalName});
		}, [user, previewBio, previewPronouns, previewGlobalName, isCommunityProfile]);
		const mockProfile = useMemo(() => {
			if (isCommunityProfile && guildId && guildMemberProfile) {
				const globalProfile = createMockProfile(previewUser, {
					previewTimezoneOffset,
					previewBadgeSettings,
				});
				return globalProfile.withGuildId(guildId).withUpdates({
					guild_member: guildMember?.toJSON(),
					guild_member_profile: {
						bio: previewBio !== undefined ? previewBio : guildMemberProfile.bio,
						banner: previewBannerUrl || guildMemberProfile.banner,
						pronouns: previewPronouns !== undefined ? previewPronouns : guildMemberProfile.pronouns,
						accent_color: previewAccentColor !== undefined ? previewAccentColor : guildMemberProfile.accent_color,
					},
				});
			}
			return createMockProfile(previewUser, {
				previewBannerUrl,
				hasClearedBanner,
				previewBio,
				previewPronouns,
				previewAccentColor,
				previewTimezoneOffset,
				previewBadgeSettings,
			});
		}, [
			previewUser,
			previewBannerUrl,
			hasClearedBanner,
			previewBio,
			previewPronouns,
			previewAccentColor,
			previewTimezoneOffset,
			previewBadgeSettings,
			guildId,
			guildMember,
			guildMemberProfile,
			isCommunityProfile,
		]);
		const shouldAutoplayProfileAnimations = useAutoplayExpandedProfileAnimations();
		const {
			avatarUrl: finalAvatarUrl,
			hoverAvatarUrl: finalHoverAvatarUrl,
			bannerUrl: finalBannerUrl,
			hoverBannerUrl: finalHoverBannerUrl,
			borderColor,
			borderGradient,
			bannerFallbackColor,
		} = useProfileCardDisplayState({
			user,
			profile: mockProfile,
			guildId,
			guildMember,
			guildMemberProfile: mockProfile.getGuildMemberProfile() ?? guildMemberProfile,
			previewOverrides,
			accentUser: previewUser,
		});
		const openMockProfile = useCallback(() => {
			ModalCommands.push(
				modal(() => (
					<UserProfileModal
						userId={user.id}
						guildId={guildId || undefined}
						disableEditProfile={true}
						previewOverrides={{
							previewAvatarUrl,
							previewBannerUrl,
							previewAccentColor,
							hasClearedAvatar,
							hasClearedBanner,
							ignoreGuildAvatar: ignoreGuildAvatarInPreview,
							ignoreGuildBanner: ignoreGuildBannerInPreview,
						}}
						previewUser={previewUser}
						previewProfile={mockProfile}
						data-flx="user.profile.profile-preview.open-mock-profile.user-profile-modal"
					/>
				)),
			);
		}, [
			user.id,
			guildId,
			previewAvatarUrl,
			previewBannerUrl,
			previewAccentColor,
			hasClearedAvatar,
			hasClearedBanner,
			ignoreGuildAvatarInPreview,
			ignoreGuildBannerInPreview,
			previewUser,
			mockProfile,
		]);
		const pronouns = previewPronouns !== undefined ? previewPronouns : user.pronouns;
		const displayName =
			previewNick || (guildId ? NicknameUtils.getNickname(previewUser, guildId) : previewUser.displayName);
		const selectedGuild = guildId ? Guilds.getGuild(guildId) : null;
		const hasPreviewStatus = previewCustomStatus !== undefined;
		const handlePreviewKeyDown = (event: React.KeyboardEvent<HTMLDivElement>) => {
			if (event.key === 'Enter' || event.key === ' ') {
				event.preventDefault();
				openMockProfile();
			}
		};
		return (
			<FocusRing offset={-2} data-flx="user.profile.profile-preview.focus-ring">
				<div
					className={styles.previewInteractive}
					role="group"
					aria-label={i18n._(PROFILE_PREVIEW_PRESS_ENTER_TO_OPEN_FULL_PREVIEW_DESCRIPTOR)}
					onKeyDown={handlePreviewKeyDown}
					data-flx="user.profile.profile-preview.preview-interactive.preview-key-down"
				>
					<ProfileCardLayout
						borderColor={borderColor}
						borderGradient={borderGradient}
						showPreviewLabel={showPreviewLabel}
						style={PROFILE_POPOUT_GEOMETRY_STYLE}
						data-flx="user.profile.profile-preview.profile-card-layout"
					>
						<ProfileCardBanner
							bannerUrl={finalBannerUrl}
							hoverBannerUrl={finalHoverBannerUrl}
							bannerColor={bannerFallbackColor}
							user={user}
							avatarUrl={finalAvatarUrl}
							hoverAvatarUrl={finalHoverAvatarUrl}
							isClickable={true}
							onAvatarClick={openMockProfile}
							data-flx="user.profile.profile-preview.profile-card-banner"
						/>
						<UserProfileBadges
							user={previewUser}
							profile={mockProfile}
							data-flx="user.profile.profile-preview.user-profile-badges"
						/>
						<ProfileCardContent data-flx="user.profile.profile-preview.profile-card-content">
							<ProfileCardUserInfo
								displayName={displayName}
								user={previewUser}
								pronouns={pronouns}
								showUsername={true}
								isClickable={true}
								onDisplayNameClick={openMockProfile}
								onUsernameClick={openMockProfile}
								data-flx="user.profile.profile-preview.profile-card-user-info"
							/>
							<div className={styles.profileCustomStatus} data-flx="user.profile.profile-preview.profile-custom-status">
								<CustomStatusDisplay
									userId={hasPreviewStatus ? undefined : user.id}
									customStatus={hasPreviewStatus ? previewCustomStatus : undefined}
									className={styles.profileCustomStatusText}
									allowJumboEmoji
									maxLines={0}
									alwaysAnimate={shouldAutoplayProfileAnimations}
									data-flx="user.profile.profile-preview.profile-custom-status-text"
								/>
							</div>
							<UserProfilePreviewBio
								profile={mockProfile}
								onShowMore={openMockProfile}
								data-flx="user.profile.profile-preview.user-profile-preview-bio"
							/>
							<ProfileRichPresence userId={user.id} />
							<UserProfileTimezoneInfo
								profile={mockProfile}
								data-flx="user.profile.profile-preview.user-profile-timezone-info"
							/>
							{showMembershipInfo && (
								<UserProfileMembershipInfo
									profile={{...mockProfile, guild: selectedGuild, guildMember} as Profile}
									user={previewUser}
									data-flx="user.profile.profile-preview.user-profile-membership-info"
								/>
							)}
						</ProfileCardContent>
						{showMessageButton && (
							<ProfileCardFooter data-flx="user.profile.profile-preview.profile-card-footer">
								<Tooltip
									text={i18n._(YOU_CAN_T_MESSAGE_YOURSELF_DESCRIPTOR)}
									maxWidth="xl"
									data-flx="user.profile.profile-preview.tooltip"
								>
									<div
										className={styles.messageButtonWrapper}
										data-flx="user.profile.profile-preview.message-button-wrapper"
									>
										<Button
											small={true}
											fitContainer={true}
											leftIcon={
												<ChatTeardropIcon
													className={styles.messageIcon}
													data-flx="user.profile.profile-preview.message-icon"
												/>
											}
											disabled={true}
											data-flx="user.profile.profile-preview.button"
										>
											<Trans>Message</Trans>
										</Button>
									</div>
								</Tooltip>
							</ProfileCardFooter>
						)}
					</ProfileCardLayout>
				</div>
			</FocusRing>
		);
	},
);
