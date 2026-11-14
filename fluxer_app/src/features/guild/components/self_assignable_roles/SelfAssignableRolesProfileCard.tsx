// SPDX-License-Identifier: AGPL-3.0-or-later

import GuildMembers from '@app/features/member/state/GuildMembers';
import {UserProfilePopout} from '@app/features/user/components/popouts/UserProfilePopout';
import UserProfile from '@app/features/user/state/UserProfile';
import Users from '@app/features/user/state/Users';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect} from 'react';

const keepCardOpen = () => {};

export const SelfAssignableRolesProfileCard: React.FC<{guildId: string}> = observer(
	function SelfAssignableRolesProfileCard({guildId}) {
		const user = Users.currentUser;
		const userId = user?.id ?? null;
		useEffect(() => (userId === null ? undefined : UserProfile.retainProfile(userId, guildId)), [guildId, userId]);
		if (!user) return null;
		return (
			<UserProfilePopout
				popoutKey={`self-assignable-roles-profile-${guildId}`}
				user={user}
				isWebhook={false}
				guildId={guildId}
				guildMember={GuildMembers.getMember(guildId, user.id)}
				onClose={keepCardOpen}
				data-flx="guild.self-assignable-roles.self-assignable-roles-profile-card.user-profile-popout"
			/>
		);
	},
);
