// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {getUserAccentColor} from '@app/features/theme/utils/AccentColorUtils';
import {ProfileBody} from '@app/features/user/components/modals/user_profile_modal/ProfileBody';
import {ProfileMediaHeader} from '@app/features/user/components/modals/user_profile_modal/ProfileMediaHeader';
import type {ProfileModalContentProps} from '@app/features/user/components/modals/user_profile_modal/UserProfileModalShared';
import * as ProfileDisplayUtils from '@app/features/user/utils/ProfileDisplayUtils';
import {resolveProfileGuildMembership, toProfileDisplayContext} from '@app/features/user/utils/ProfileGuildMembership';
import {FLUXERBOT_ID} from '@fluxer/constants/src/AppConstants';
import {
	MEDIA_PROXY_AVATAR_SIZE_PROFILE,
	MEDIA_PROXY_PROFILE_BANNER_SIZE_MODAL,
} from '@fluxer/constants/src/MediaProxyAssetSizes';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

export const ProfileModalContent: React.FC<ProfileModalContentProps> = observer(
	({
		profile,
		user,
		userNote,
		autoFocusNote,
		noteRef,
		renderActionButtons,
		previewOverrides,
		showProfileDataWarning,
	}) => {
		const effectiveProfile = profile?.getEffectiveProfile() ?? null;
		const isSystemUser = user.id === FLUXERBOT_ID;
		const systemBranding = isSystemUser ? RuntimeConfig.getSnapshotOrNull()?.appPublic.branding : null;
		const bannerColor =
			(isSystemUser && RuntimeConfig.isSelfHosted() ? systemBranding?.theme_color : null) ??
			getUserAccentColor(user, effectiveProfile?.accent_color);
		const membership = resolveProfileGuildMembership(profile);
		const profileContext = useMemo<ProfileDisplayUtils.ProfileDisplayContext>(
			() =>
				toProfileDisplayContext({
					user,
					profile,
					membership,
					guildMemberProfile: profile?.guildMemberProfile,
				}),
			[user, profile, membership],
		);
		const {avatarUrl: profileAvatarUrl, hoverAvatarUrl: profileHoverAvatarUrl} = useMemo(
			() => ProfileDisplayUtils.getProfileAvatarUrls(profileContext, previewOverrides, MEDIA_PROXY_AVATAR_SIZE_PROFILE),
			[profileContext, previewOverrides],
		);
		const avatarUrl = isSystemUser
			? (systemBranding?.logo_url ?? systemBranding?.icon_url ?? profileAvatarUrl)
			: profileAvatarUrl;
		const hoverAvatarUrl = isSystemUser ? avatarUrl : profileHoverAvatarUrl;
		const {bannerUrl, hoverBannerUrl} = useMemo(
			() =>
				ProfileDisplayUtils.getProfileBannerUrls(
					profileContext,
					previewOverrides,
					MEDIA_PROXY_PROFILE_BANNER_SIZE_MODAL,
				),
			[profileContext, previewOverrides],
		);
		return (
			<>
				<ProfileMediaHeader
					user={user}
					profile={profile}
					profileContext={profileContext}
					previewOverrides={previewOverrides}
					bannerColor={bannerColor}
					bannerUrl={isSystemUser ? null : bannerUrl}
					hoverBannerUrl={isSystemUser ? null : hoverBannerUrl}
					avatarUrl={avatarUrl}
					hoverAvatarUrl={hoverAvatarUrl}
					renderActionButtons={renderActionButtons}
					data-flx="user.user-profile-modal.profile-modal-content.profile-media-header"
				/>
				<ProfileBody
					profile={profile}
					user={user}
					userNote={userNote}
					autoFocusNote={autoFocusNote}
					noteRef={noteRef}
					showProfileDataWarning={showProfileDataWarning}
					data-flx="user.user-profile-modal.profile-modal-content.profile-body"
				/>
			</>
		);
	},
);
