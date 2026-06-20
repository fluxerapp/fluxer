// SPDX-License-Identifier: AGPL-3.0-or-later

import Permission from '@app/features/permissions/state/Permission';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import userProfileModalStyles from '@app/features/user/components/modals/UserProfileModal.module.css';
import {UserNoteEditor} from '@app/features/user/components/modals/user_profile_modal/UserNoteEditor';
import type {ProfileContentProps} from '@app/features/user/components/modals/user_profile_modal/UserProfileModalShared';
import {
	UserProfileBio,
	UserProfileConnections,
	UserProfileMembershipInfo,
	UserProfileRoles,
	UserProfileTimezoneInfo,
} from '@app/features/user/components/popouts/UserProfileShared';
import {ProfileRichPresence} from '@app/features/user/components/profile/ProfileRichPresence';
import {VoiceActivitySection} from '@app/features/user/components/profile/VoiceActivitySection';
import {resolveProfileGuildMembership} from '@app/features/user/utils/ProfileGuildMembership';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

export const ProfileContent: React.FC<ProfileContentProps> = observer(
	({profile, user, userNote, autoFocusNote, noteRef}) => {
		const membership = resolveProfileGuildMembership(profile);
		const canManageRoles = Permission.can(Permissions.MANAGE_ROLES, {guildId: profile?.guild?.id});
		const handleNavigate = useCallback(() => {
			ModalCommands.pop();
		}, []);
		return (
			<div className={userProfileModalStyles.profileContent} data-flx="user.user-profile-modal.profile-content.div">
				<div
					className={userProfileModalStyles.profileContentHeader}
					data-flx="user.user-profile-modal.profile-content.div--2"
				>
					<VoiceActivitySection
						userId={user.id}
						onNavigate={handleNavigate}
						showAllActivities={true}
						data-flx="user.user-profile-modal.profile-content.voice-activity-section"
					/>
					<UserProfileBio profile={profile} data-flx="user.user-profile-modal.profile-content.user-profile-bio" />
					<ProfileRichPresence userId={user.id} />
					<UserProfileTimezoneInfo
						profile={profile}
						data-flx="user.user-profile-modal.profile-content.user-profile-timezone-info"
					/>
					<UserProfileMembershipInfo
						profile={profile}
						user={user}
						data-flx="user.user-profile-modal.profile-content.user-profile-membership-info"
					/>
					<UserProfileRoles
						profile={profile}
						user={user}
						memberRoles={[...membership.roles]}
						canManageRoles={canManageRoles}
						data-flx="user.user-profile-modal.profile-content.user-profile-roles"
					/>
					<UserProfileConnections
						profile={profile}
						variant="cards"
						data-flx="user.user-profile-modal.profile-content.user-profile-connections"
					/>
					<UserNoteEditor
						userId={user.id}
						initialNote={userNote}
						autoFocus={autoFocusNote}
						noteRef={noteRef}
						data-flx="user.user-profile-modal.profile-content.user-note-editor"
					/>
				</div>
			</div>
		);
	},
);
