// SPDX-License-Identifier: AGPL-3.0-or-later

import GuildMembers from '@app/features/member/state/GuildMembers';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import type {User} from '@app/features/user/models/User';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import type {MediaProxyImageSize} from '@fluxer/constants/src/MediaProxyImageSizes';

export const NATIVE_NOTIFICATION_ICON_CSS_SIZE: MediaProxyImageSize = 128;

export function getNotificationIconURL(user: Pick<User, 'id' | 'avatar'>, guildId?: string | null): string {
	const member = guildId ? GuildMembers.getMember(guildId, user.id) : null;
	const nativeIcon = isDesktop();
	if (guildId && member) {
		return nativeIcon
			? AvatarUtils.getGuildMemberNotificationAvatarURL({
					guildId,
					userId: user.id,
					avatar: user.avatar,
					memberAvatar: member.avatar,
					avatarUnset: member.isAvatarUnset(),
					size: NATIVE_NOTIFICATION_ICON_CSS_SIZE,
				})
			: AvatarUtils.getGuildMemberDisplayAvatarURL({
					guildId,
					user,
					memberAvatar: member.avatar,
					avatarUnset: member.isAvatarUnset(),
					animated: false,
				});
	}
	return nativeIcon
		? AvatarUtils.getUserNotificationAvatarURL(user, NATIVE_NOTIFICATION_ICON_CSS_SIZE)
		: AvatarUtils.getUserAvatarURL(user, false);
}
