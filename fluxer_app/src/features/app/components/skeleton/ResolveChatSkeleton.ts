// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {
	getRememberedSkeletonChannelHeaderLayout,
	getRememberedSkeletonChannelProjection,
	getRememberedSkeletonMemberGroups,
	type RememberedSkeletonChannelProjection,
	type RememberedSkeletonMemberGroup,
	resolveSkeletonChannelProjectionKind,
	resolveSkeletonMemberSurfaceKind,
	SKELETON_DEFAULT_CHANNEL_HEADER_LAYOUT,
	SKELETON_UNMEASURED_WIDTH_PX,
	SkeletonChannelProjectionKind,
	type SkeletonMemberSurfaceKind,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import Channels from '@app/features/channel/state/Channels';
import MemberList from '@app/features/member/state/MemberList';

export const ChatSkeletonChannelKind = SkeletonChannelProjectionKind;

export type ChatSkeletonChannelKind = SkeletonChannelProjectionKind;

export interface ChatSkeletonPresentation {
	readonly channelKind: ChatSkeletonChannelKind;
	readonly showMemberList: boolean;
	readonly showTopic: boolean;
	readonly staffToolsVisible: boolean;
	readonly updaterVisible: boolean;
	readonly favoritesVisible: boolean;
	readonly headerNameWidthPx: number;
	readonly headerTopicWidthPx: number;
	readonly headerDesktopLeadingActionCount: number | null;
	readonly headerMobileActionCount: number | null;
	readonly rememberedMemberGroups: ReadonlyArray<RememberedSkeletonMemberGroup> | null;
}

const CHANNEL_ID_PATH_INDEX = 3;

function resolveChannelId(pathname: string): string | null {
	if (!Routes.isChannelRoute(pathname)) {
		return null;
	}
	const channelId = pathname.split('/')[CHANNEL_ID_PATH_INDEX];
	if (!channelId || channelId === 'members') {
		return null;
	}
	return channelId;
}

function resolveChannelKind(
	pathname: string,
	channelId: string | null,
	rememberedProjection: RememberedSkeletonChannelProjection | null,
): ChatSkeletonChannelKind {
	const channel = channelId ? Channels.getChannel(channelId) : undefined;
	if (channel) {
		return resolveSkeletonChannelProjectionKind(channel.type);
	}
	if (rememberedProjection) {
		return rememberedProjection.channelKind;
	}
	if (Routes.isDMRoute(pathname)) {
		return ChatSkeletonChannelKind.DM;
	}
	return ChatSkeletonChannelKind.GUILD;
}

function resolveMemberListVisibility(
	channelId: string | null,
	channelKind: ChatSkeletonChannelKind,
	canFitMemberList: boolean,
	searchPanelOpen: boolean,
	rememberedMemberListVisible: boolean | null,
): boolean {
	if (!canFitMemberList || !channelId || searchPanelOpen) {
		return false;
	}
	switch (channelKind) {
		case ChatSkeletonChannelKind.GUILD:
		case ChatSkeletonChannelKind.GROUP_DM:
			return rememberedMemberListVisible ?? MemberList.isMembersVisible();
		case ChatSkeletonChannelKind.GUILD_VOICE:
			return rememberedMemberListVisible ?? MemberList.isMembersVisible({channelId, defaultHiddenForChannel: true});
		case ChatSkeletonChannelKind.DM:
		case ChatSkeletonChannelKind.PERSONAL_NOTES:
			return false;
	}
}

export function resolveChatSkeletonPresentation(pathname: string, canFitMemberList: boolean): ChatSkeletonPresentation {
	const channelId = resolveChannelId(pathname);
	const channel = channelId ? Channels.getChannel(channelId) : undefined;
	const rememberedProjection = channelId == null ? null : getRememberedSkeletonChannelProjection(channelId);
	const rememberedHeader = getRememberedSkeletonChannelHeaderLayout() ?? SKELETON_DEFAULT_CHANNEL_HEADER_LAYOUT;
	const channelKind = resolveChannelKind(pathname, channelId, rememberedProjection);
	const memberSurfaceKind: SkeletonMemberSurfaceKind | null = resolveSkeletonMemberSurfaceKind(channelKind);
	const guildChannel =
		channelKind === ChatSkeletonChannelKind.GUILD || channelKind === ChatSkeletonChannelKind.GUILD_VOICE;
	let showTopic = false;
	if (guildChannel) {
		if (channel != null) {
			showTopic = Boolean(channel.topic);
		} else {
			showTopic = rememberedProjection?.showTopic ?? false;
		}
	}
	let rememberedMemberGroups: ReadonlyArray<RememberedSkeletonMemberGroup> | null = null;
	if (channelId != null && memberSurfaceKind != null) {
		rememberedMemberGroups = getRememberedSkeletonMemberGroups(channelId, memberSurfaceKind);
	}
	let headerTopicWidthPx = SKELETON_UNMEASURED_WIDTH_PX;
	if (showTopic && rememberedProjection != null) {
		headerTopicWidthPx = rememberedProjection.topicWidthPx;
	}
	return {
		channelKind,
		showMemberList: resolveMemberListVisibility(
			channelId,
			channelKind,
			canFitMemberList,
			rememberedProjection?.searchPanelOpen ?? false,
			rememberedProjection?.memberListVisible ?? null,
		),
		showTopic,
		staffToolsVisible: rememberedHeader.staffToolsVisible,
		updaterVisible: rememberedHeader.updaterVisible,
		favoritesVisible: rememberedHeader.favoritesVisible,
		headerNameWidthPx: rememberedProjection?.nameWidthPx ?? SKELETON_UNMEASURED_WIDTH_PX,
		headerTopicWidthPx,
		headerDesktopLeadingActionCount: rememberedProjection?.desktopLeadingActionCount ?? null,
		headerMobileActionCount: rememberedProjection?.mobileActionCount ?? null,
		rememberedMemberGroups,
	};
}
