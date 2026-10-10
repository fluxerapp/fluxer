// SPDX-License-Identifier: AGPL-3.0-or-later

import MemberSidebar from '@app/features/member/state/MemberSidebar';
import {reaction} from 'mobx';
import {useEffect, useState} from 'react';

interface UseMemberListMobileOptions {
	guildId: string;
	channelId: string;
	userId: string;
	enabled?: boolean;
}

export function resolveMemberListMobile({
	guildId,
	channelId,
	userId,
	enabled = true,
}: UseMemberListMobileOptions): boolean | undefined {
	if (!enabled) return undefined;
	return MemberSidebar.isMobile(guildId, channelId, userId) ?? undefined;
}

export function useMemberListMobile({
	guildId,
	channelId,
	userId,
	enabled = true,
}: UseMemberListMobileOptions): boolean | undefined {
	const [isMobile, setIsMobile] = useState(() => resolveMemberListMobile({guildId, channelId, userId, enabled}));
	useEffect(() => {
		return reaction(() => resolveMemberListMobile({guildId, channelId, userId, enabled}), setIsMobile, {
			fireImmediately: true,
		});
	}, [guildId, channelId, userId, enabled]);
	return isMobile;
}
