// SPDX-License-Identifier: AGPL-3.0-or-later

import {CustomStatusDisplay} from '@app/features/app/components/shared/custom_status_display/CustomStatusDisplay';
import {UserTag} from '@app/features/channel/components/ChannelUserTag';
import Presence from '@app/features/presence/state/Presence';
import userProfileModalStyles from '@app/features/user/components/modals/UserProfileModal.module.css';
import type {UserInfoProps} from '@app/features/user/components/modals/user_profile_modal/UserProfileModalShared';
import {LimitedProfileNotice} from '@app/features/user/components/popouts/LimitedProfileNotice';
import {UserProfileBadges} from '@app/features/user/components/popouts/UserProfileBadges';
import {UserProfileDataWarning} from '@app/features/user/components/popouts/UserProfileDataWarning';
import {useAutoplayExpandedProfileAnimations} from '@app/features/user/hooks/useAutoplayExpandedProfileAnimations';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {
	getProfileMembershipDisplayName,
	resolveProfileGuildMembership,
} from '@app/features/user/utils/ProfileGuildMembership';
import {Trans} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';

export const UserInfo: React.FC<UserInfoProps> = observer(({user, profile, guildId, showProfileDataWarning}) => {
	const membership = resolveProfileGuildMembership(profile);
	const displayName = getProfileMembershipDisplayName(user, membership, guildId);
	const effectiveProfile = profile?.getEffectiveProfile() ?? null;
	const shouldAutoplayProfileAnimations = useAutoplayExpandedProfileAnimations();
	const activities = Presence.getActivities(user.id);
	const primaryActivity = activities[0] ?? null;
	return (
		<div className={userProfileModalStyles.userInfo} data-flx="user.user-profile-modal.user-info.div">
			<div
				className={clsx(userProfileModalStyles.userInfoHeader, userProfileModalStyles.userInfoHeaderDesktop)}
				data-flx="user.user-profile-modal.user-info.div--2"
			>
				<div className={userProfileModalStyles.userInfoContent} data-flx="user.user-profile-modal.user-info.div--3">
					{showProfileDataWarning && (
						<div
							className={userProfileModalStyles.profileDataWarning}
							data-flx="user.user-profile-modal.user-info.div--4"
						>
							<UserProfileDataWarning data-flx="user.user-profile-modal.user-info.user-profile-data-warning" />
						</div>
					)}
					{profile?.profileLimited && (
						<div
							className={userProfileModalStyles.profileDataWarning}
							data-flx="user.user-profile-modal.user-info.profile-limited-notice"
						>
							<LimitedProfileNotice data-flx="user.user-profile-modal.user-profile-modal-user-info.user-info.limited-profile-notice" />
						</div>
					)}
					<div className={userProfileModalStyles.nameRow} data-flx="user.user-profile-modal.user-info.div--5">
						<span className={userProfileModalStyles.userName} data-flx="user.user-profile-modal.user-info.span">
							{displayName}
						</span>
						{user.bot && (
							<UserTag
								className={userProfileModalStyles.userTag}
								system={user.system}
								size="lg"
								data-flx="user.user-profile-modal.user-info.user-tag"
							/>
						)}
					</div>
					<div className={userProfileModalStyles.tagBadgeRow} data-flx="user.user-profile-modal.user-info.div--6">
						<div className={userProfileModalStyles.usernameRow} data-flx="user.user-profile-modal.user-info.div--7">
							{NicknameUtils.formatTagForStreamerMode(user.tag)}
						</div>
						<div className={userProfileModalStyles.badgesWrapper} data-flx="user.user-profile-modal.user-info.div--8">
							<UserProfileBadges
								user={user}
								profile={profile}
								isModal={true}
								isMobile={false}
								data-flx="user.user-profile-modal.user-info.user-profile-badges"
							/>
						</div>
					</div>
					{effectiveProfile?.pronouns && (
						<div className={userProfileModalStyles.pronouns} data-flx="user.user-profile-modal.user-info.div--9">
							<span className={userProfileModalStyles.srOnly} data-flx="user.user-profile-modal.user-info.span--2">
								<Trans>Pronouns: </Trans>
							</span>
							{effectiveProfile.pronouns}
						</div>
					)}
					<div className={userProfileModalStyles.customStatusRow} data-flx="user.user-profile-modal.user-info.div--10">
						<CustomStatusDisplay
							userId={user.id}
							className={userProfileModalStyles.customStatusText}
							showTooltip
							allowJumboEmoji
							maxLines={0}
							alwaysAnimate={shouldAutoplayProfileAnimations}
							data-flx="user.user-profile-modal.user-info.custom-status-display"
						/>
					</div>
					{primaryActivity && (
						<div className={userProfileModalStyles.customStatusRow} data-flx="user.user-profile-modal.user-info.activity">
							<span className={userProfileModalStyles.customStatusText} data-flx="user.user-profile-modal.user-info.activity-text">
								<Trans>Playing {primaryActivity.name}</Trans>
							</span>
						</div>
					)}
				</div>
			</div>
		</div>
	);
});
