// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {
	ChatSkeletonChannelKind,
	resolveChatSkeletonPresentation,
} from '@app/features/app/components/skeleton/ResolveChatSkeleton';
import {
	resolveSkeletonShell,
	SkeletonSidebarVariant,
	showsMobileBottomNav,
	showsMobileGuildRail,
	showsMobileSidebar,
} from '@app/features/app/components/skeleton/ResolveSkeletonShell';
import {
	getRememberedSkeletonComposerLayout,
	getRememberedSkeletonDMSidebarLayout,
	getRememberedSkeletonFriendsLayout,
	getRememberedSkeletonGuildChannelList,
	getRememberedSkeletonGuildPresentation,
	getRememberedSkeletonGuildRailLayout,
	getRememberedSkeletonMessagePresentation,
	getRememberedSkeletonNagbarLayout,
	getRememberedSkeletonVoicePresence,
	type RememberedSkeletonDMSidebarLayout,
	type RememberedSkeletonGuildChannelList,
	type RememberedSkeletonGuildRailItem,
	type RememberedSkeletonGuildRailLayout,
	type RememberedSkeletonMemberGroup,
	resolveDefaultSkeletonComposerLayout,
	resolveDefaultSkeletonMessagePresentation,
	SKELETON_DEFAULT_FRIENDS_LAYOUT,
	SKELETON_DEFAULT_VOICE_PRESENCE,
	SKELETON_GUILD_RAIL_COLLAPSED_FOLDER_CHILD_LIMIT,
	SKELETON_GUILD_RAIL_FALLBACK_ITEMS,
	SKELETON_GUILD_RAIL_ORGANIZED_VISUAL_ROW_LIMIT,
	SkeletonGuildBannerPlacement,
	SkeletonGuildRailItemIndicator,
	SkeletonGuildRailItemKind,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {
	skeletonDirectMessagesDisabled,
	skeletonGifEnabled,
	skeletonSelfHosted,
	skeletonSingleCommunityEnabled,
} from '@app/features/app/components/skeleton/SkeletonRuntimeConfig';
import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import {
	GATEWAY_PREBOOT_SESSION_STORAGE_KEY,
	readRawStorageItem,
	SHELL_HINT_STORAGE_KEY,
	writeRawStorageItem,
} from '@app/features/platform/state/PrebootMirror';
import {Platform} from '@app/features/platform/types/Platform';
import SidebarWidth from '@app/features/ui/state/SidebarWidth';
import VoiceCallFullscreen from '@app/features/voice/state/VoiceCallFullscreen';
import Window from '@app/features/window/state/Window';

export const SHELL_HINT_VERSION = 4;

export const SHELL_HINT_NAGBAR_TONE_ORDER: ReadonlyArray<NagbarToneKind> = Object.freeze(Object.values(NagbarToneKind));

export const SHELL_HINT_CHANNEL_KIND_CODES: Readonly<Record<ChatSkeletonChannelKind, number>> = Object.freeze({
	[ChatSkeletonChannelKind.GUILD]: 0,
	[ChatSkeletonChannelKind.GUILD_VOICE]: 1,
	[ChatSkeletonChannelKind.DM]: 2,
	[ChatSkeletonChannelKind.GROUP_DM]: 3,
	[ChatSkeletonChannelKind.PERSONAL_NOTES]: 4,
});

const GATEWAY_PREBOOT_SESSION_PRESENT = '1';
const MAX_PATHNAME_LENGTH = 256;
const MAX_ACCOUNT_KEY_LENGTH = 256;
const MAX_CHANNEL_ID_LENGTH = 32;
const MIN_SIDEBAR_WIDTH_PX = 200;
const MAX_SIDEBAR_WIDTH_PX = 480;
const MAX_RAIL_INLINE_DM_ROWS = 16;
const MAX_RAIL_ORGANIZED_ITEMS = SKELETON_GUILD_RAIL_ORGANIZED_VISUAL_ROW_LIMIT;
const MAX_RAIL_SCROLL_TOP_PX = 100_000;
const MAX_DM_SIDEBAR_ROWS = 40;
const MAX_GUILD_CHANNEL_GROUPS = 16;
const MAX_GUILD_CHANNEL_ROWS = 64;
const MAX_MEMBER_GROUPS = 8;
const MAX_MEMBER_GROUP_ROWS = 50;
const MAX_NAGBAR_ROWS = 3;
const MAX_HEADER_ACTION_COUNT = 8;
const MAX_ACTIVE_NOW_CARDS = 24;
const MAX_ACTIVE_NOW_PARTICIPANTS = 99;
const MAX_COMPOSER_ACTION_COUNT = 8;
const MAX_MEASURED_WIDTH_PX = 4096;
const MAX_VOICE_PANEL_HEIGHT_PX = 512;
const MAX_MESSAGE_METRIC_PX = 256;
const MAX_VIEWPORT_HEIGHT_PX = 20_000;
const MEMBER_LIST_FIT_MIN_WIDTH_PX = 1024;
const MOBILE_ENABLE_BREAKPOINT_PX = 640;
const NO_SELECTED_ROW_INDEX = -1;
const UNSET_ACTION_COUNT = -1;
const BANNER_ASPECT_RATIO_SCALE = 1000;
const MIN_SCALED_BANNER_ASPECT_RATIO = 250;
const MAX_SCALED_BANNER_ASPECT_RATIO = 8000;
const GUILD_BANNER_PLACEMENT_CODES: Readonly<Record<SkeletonGuildBannerPlacement, number>> = Object.freeze({
	[SkeletonGuildBannerPlacement.NONE]: 0,
	[SkeletonGuildBannerPlacement.INTEGRATED]: 1,
	[SkeletonGuildBannerPlacement.DETACHED]: 2,
});
const RAIL_ITEM_KIND_CODES: Readonly<Record<SkeletonGuildRailItemKind, number>> = Object.freeze({
	[SkeletonGuildRailItemKind.GUILD]: 0,
	[SkeletonGuildRailItemKind.COLLAPSED_FOLDER]: 1,
	[SkeletonGuildRailItemKind.EXPANDED_FOLDER]: 2,
});
const RAIL_ITEM_INDICATOR_CODES: Readonly<Record<SkeletonGuildRailItemIndicator, number>> = Object.freeze({
	[SkeletonGuildRailItemIndicator.NONE]: 0,
	[SkeletonGuildRailItemIndicator.UNREAD]: 1,
	[SkeletonGuildRailItemIndicator.MENTION]: 2,
});
const RAIL_BOTTOM_DISCOVERY_BIT = 1;
const RAIL_BOTTOM_ADD_GUILD_BIT = 2;
const RAIL_BOTTOM_DOWNLOAD_BIT = 4;
const RAIL_BOTTOM_HELP_BIT = 8;
const DM_ACTION_FRIENDS_BIT = 1;
const DM_ACTION_PERSONAL_NOTES_BIT = 2;
const DM_ACTION_PREMIUM_BIT = 4;
const GUILD_PATH_SEGMENT_INDEX = 2;
const SHOWS_DOWNLOAD_ACTION = !Platform.isElectron && !Platform.isPWA;

function clampRounded(value: number, min: number, max: number): number {
	if (!Number.isFinite(value)) {
		return min;
	}
	return Math.min(max, Math.max(min, Math.round(value)));
}

function flag(value: boolean): number {
	return value ? 1 : 0;
}

function flags(values: ReadonlyArray<boolean>, limit: number): Array<number> {
	return values.slice(0, limit).map(flag);
}

function measuredWidth(value: number): number {
	return clampRounded(value, 0, MAX_MEASURED_WIDTH_PX);
}

function pathSegment(pathname: string, index: number): string | null {
	const segment = pathname.split('/')[index];
	if (segment == null || segment === '') {
		return null;
	}
	return segment;
}

interface RailProjection {
	readonly fluxerVisible: boolean;
	readonly favoritesVisible: boolean;
	readonly inlineDmUnreadFlags: ReadonlyArray<boolean>;
	readonly selectedInlineDmRowIndex: number;
	readonly outageVisible: boolean;
	readonly organizedItems: ReadonlyArray<RememberedSkeletonGuildRailItem>;
	readonly selectedItemIndex: number;
	readonly discoveryVisible: boolean;
	readonly addGuildVisible: boolean;
	readonly downloadVisible: boolean;
	readonly helpVisible: boolean;
	readonly scrollTopPx: number;
}

function resolveRailProjection(layout: RememberedSkeletonGuildRailLayout | null): RailProjection {
	const communityActionsAvailable = !skeletonSingleCommunityEnabled();
	const inlineDmRowCount = layout?.inlineDmRowCount ?? 0;
	const rememberedUnread = layout?.inlineDmUnreadFlags ?? [];
	const inlineDmUnreadFlags = Array.from(
		{length: inlineDmRowCount},
		(_unused, index) => rememberedUnread[index] === true,
	);
	return {
		fluxerVisible: layout?.fluxerVisible ?? !skeletonDirectMessagesDisabled(),
		favoritesVisible: layout?.favoritesVisible ?? true,
		inlineDmUnreadFlags,
		selectedInlineDmRowIndex: layout?.selectedInlineDmRowIndex ?? NO_SELECTED_ROW_INDEX,
		outageVisible: layout?.outageVisible ?? false,
		organizedItems: layout?.organizedItems ?? SKELETON_GUILD_RAIL_FALLBACK_ITEMS,
		selectedItemIndex: layout?.selectedItemIndex ?? NO_SELECTED_ROW_INDEX,
		discoveryVisible: layout?.discoveryVisible ?? communityActionsAvailable,
		addGuildVisible: layout?.addGuildVisible ?? communityActionsAvailable,
		downloadVisible: layout?.downloadVisible ?? SHOWS_DOWNLOAD_ACTION,
		helpVisible: layout?.helpVisible ?? true,
		scrollTopPx: layout?.scrollTopPx ?? 0,
	};
}

function encodeRailItem(item: RememberedSkeletonGuildRailItem): Array<number> {
	const indicator = RAIL_ITEM_INDICATOR_CODES[item.indicator];
	if (item.kind === SkeletonGuildRailItemKind.COLLAPSED_FOLDER) {
		return [
			RAIL_ITEM_KIND_CODES[item.kind],
			indicator,
			clampRounded(item.childCount, 0, SKELETON_GUILD_RAIL_COLLAPSED_FOLDER_CHILD_LIMIT),
			flag(item.showIconWhenCollapsed),
		];
	}
	if (item.kind === SkeletonGuildRailItemKind.EXPANDED_FOLDER) {
		const childCount = clampRounded(item.childCount, 0, MAX_RAIL_ORGANIZED_ITEMS - 1);
		const encoded = [RAIL_ITEM_KIND_CODES[item.kind], indicator, childCount, item.selectedChildIndex];
		for (let index = 0; index < childCount; index++) {
			encoded.push(RAIL_ITEM_INDICATOR_CODES[item.childIndicators[index] ?? SkeletonGuildRailItemIndicator.NONE]);
		}
		return encoded;
	}
	return [RAIL_ITEM_KIND_CODES[item.kind], indicator];
}

function resolveSelectedTopRowIndex(pathname: string, rail: RailProjection): number {
	const leadingTopRowCount = flag(rail.fluxerVisible) + flag(rail.favoritesVisible);
	if ((Routes.isDMRoute(pathname) || Routes.isSpecialPage(pathname)) && rail.fluxerVisible) {
		return 0;
	}
	if (Routes.isFavoritesRoute(pathname) && rail.favoritesVisible) {
		return rail.fluxerVisible ? 1 : 0;
	}
	if (rail.selectedInlineDmRowIndex >= 0) {
		return leadingTopRowCount + rail.selectedInlineDmRowIndex;
	}
	return NO_SELECTED_ROW_INDEX;
}

function resolveRailBottomMask(rail: RailProjection): number {
	let mask = 0;
	if (rail.discoveryVisible) mask |= RAIL_BOTTOM_DISCOVERY_BIT;
	if (rail.addGuildVisible) mask |= RAIL_BOTTOM_ADD_GUILD_BIT;
	if (rail.downloadVisible) mask |= RAIL_BOTTOM_DOWNLOAD_BIT;
	if (rail.helpVisible) mask |= RAIL_BOTTOM_HELP_BIT;
	return mask;
}

function resolveDMSidebarLayout(mobile: boolean): RememberedSkeletonDMSidebarLayout | null {
	const layout = getRememberedSkeletonDMSidebarLayout();
	if (layout == null || layout.isMobile !== mobile) {
		return null;
	}
	return layout;
}

function resolveDMActionMask(layout: RememberedSkeletonDMSidebarLayout | null, mobile: boolean): number {
	let mask = 0;
	if (!mobile && (layout?.friendsVisible ?? true)) {
		mask |= DM_ACTION_FRIENDS_BIT;
	}
	if (layout?.personalNotesVisible ?? true) {
		mask |= DM_ACTION_PERSONAL_NOTES_BIT;
	}
	if (layout?.premiumVisible ?? (!skeletonSelfHosted() && !mobile)) {
		mask |= DM_ACTION_PREMIUM_BIT;
	}
	return mask;
}

function encodeGuildChannelGroups(channelList: RememberedSkeletonGuildChannelList): Array<unknown> {
	let remainingRows = MAX_GUILD_CHANNEL_ROWS;
	return channelList.groups.slice(0, MAX_GUILD_CHANNEL_GROUPS).map((group) => {
		const channels = group.channels
			.slice(0, Math.max(0, remainingRows))
			.map((channel) => measuredWidth(channel.nameWidthPx));
		remainingRows -= channels.length;
		return [flag(group.categoryHeaderVisible), measuredWidth(group.categoryNameWidthPx), channels];
	});
}

function encodeMemberGroups(groups: ReadonlyArray<RememberedSkeletonMemberGroup>): Array<Array<number>> {
	return groups
		.slice(0, MAX_MEMBER_GROUPS)
		.map((group) => [
			clampRounded(group.rowCount, 0, MAX_MEMBER_GROUP_ROWS),
			measuredWidth(group.headingWidthPx),
			...flags(group.subtextFlags, MAX_MEMBER_GROUP_ROWS),
		]);
}

export function writeShellHint(): void {
	try {
		if (typeof window === 'undefined') {
			return;
		}
		if (readRawStorageItem(GATEWAY_PREBOOT_SESSION_STORAGE_KEY) !== GATEWAY_PREBOOT_SESSION_PRESENT) {
			return;
		}
		const pathname = window.location.pathname;
		const shell = resolveSkeletonShell(pathname);
		if (shell.chrome !== 'guilds') {
			return;
		}
		const mobileBrowser = Platform.isMobileBrowser;
		const viewportWidth = Window.windowSize.width;
		const mobile = mobileBrowser || viewportWidth < MOBILE_ENABLE_BREAKPOINT_PX;
		const rail = resolveRailProjection(getRememberedSkeletonGuildRailLayout());
		const dmSidebar = shell.sidebar === SkeletonSidebarVariant.DM ? resolveDMSidebarLayout(mobile) : null;
		const guildId =
			shell.sidebar === SkeletonSidebarVariant.GUILD ? pathSegment(pathname, GUILD_PATH_SEGMENT_INDEX) : null;
		const guildPresentation = guildId == null ? null : getRememberedSkeletonGuildPresentation(guildId);
		const guildChannelList = guildId == null ? null : getRememberedSkeletonGuildChannelList(guildId);
		const isChat = shell.content.kind === 'chat';
		const chat = resolveChatSkeletonPresentation(pathname, !mobile && viewportWidth >= MEMBER_LIST_FIT_MIN_WIDTH_PX);
		const composer =
			getRememberedSkeletonComposerLayout() ?? resolveDefaultSkeletonComposerLayout(skeletonGifEnabled());
		const message = getRememberedSkeletonMessagePresentation() ?? resolveDefaultSkeletonMessagePresentation();
		const voice = getRememberedSkeletonVoicePresence() ?? SKELETON_DEFAULT_VOICE_PRESENCE;
		const showMemberList = isChat && chat.showMemberList;
		const sidebarWidth = SidebarWidth.width;
		const friends = getRememberedSkeletonFriendsLayout() ?? SKELETON_DEFAULT_FRIENDS_LAYOUT;
		const hint: Record<string, unknown> = {
			v: SHELL_HINT_VERSION,
			t: Date.now(),
			p: pathname.slice(0, MAX_PATHNAME_LENGTH),
			a: (readRawStorageItem(AppStorageKey.AUTH_ACCOUNT_KEY) ?? '').slice(0, MAX_ACCOUNT_KEY_LENGTH),
			mo: flag(mobile),
			ua: flag(mobileBrowser),
			fs: flag(VoiceCallFullscreen.isActive),
			sw: sidebarWidth == null ? 0 : clampRounded(sidebarWidth, MIN_SIDEBAR_WIDTH_PX, MAX_SIDEBAR_WIDTH_PX),
			sv: shell.sidebar,
			ck: isChat ? 'chat' : 'other',
			cb: flag(shell.content.kind === 'friends'),
			rf: flag(rail.fluxerVisible),
			rv: flag(rail.favoritesVisible),
			ru: flags(rail.inlineDmUnreadFlags, MAX_RAIL_INLINE_DM_ROWS),
			rti: resolveSelectedTopRowIndex(pathname, rail),
			ro: flag(rail.outageVisible),
			ri: rail.organizedItems.slice(0, MAX_RAIL_ORGANIZED_ITEMS).map(encodeRailItem),
			rgi: rail.selectedItemIndex,
			rbm: resolveRailBottomMask(rail),
			rbi: rail.discoveryVisible && Routes.isDiscoverRoute(pathname) ? 0 : NO_SELECTED_ROW_INDEX,
			rs: clampRounded(rail.scrollTopPx, 0, MAX_RAIL_SCROLL_TOP_PX),
			gb: GUILD_BANNER_PLACEMENT_CODES[guildPresentation?.bannerPlacement ?? SkeletonGuildBannerPlacement.NONE],
			ga: clampRounded(
				(guildPresentation?.bannerAspectRatio ?? 0) * BANNER_ASPECT_RATIO_SCALE,
				MIN_SCALED_BANNER_ASPECT_RATIO,
				MAX_SCALED_BANNER_ASPECT_RATIO,
			),
			gn: measuredWidth(guildPresentation?.headerNameWidthPx ?? 0),
			gd: flag(guildPresentation?.badgeVisible === true),
			gm: flag(guildChannelList?.membersRowVisible === true),
			sr: clampRounded(dmSidebar?.channelRowCount ?? 0, 0, MAX_DM_SIDEBAR_ROWS),
			sam: shell.sidebar === SkeletonSidebarVariant.DM ? resolveDMActionMask(dmSidebar, mobile) : 0,
			ss: flag(dmSidebar?.sectionVisible ?? true),
			cid: (chat.channelId ?? '').slice(0, MAX_CHANNEL_ID_LENGTH),
			ch: SHELL_HINT_CHANNEL_KIND_CODES[chat.channelKind],
			ct: flag(chat.showTopic),
			cnw: measuredWidth(chat.headerNameWidthPx),
			ctw: measuredWidth(chat.headerTopicWidthPx),
			cda:
				chat.headerDesktopLeadingActionCount == null
					? UNSET_ACTION_COUNT
					: clampRounded(chat.headerDesktopLeadingActionCount, 0, MAX_HEADER_ACTION_COUNT),
			cma:
				chat.headerMobileActionCount == null
					? UNSET_ACTION_COUNT
					: clampRounded(chat.headerMobileActionCount, 0, MAX_HEADER_ACTION_COUNT),
			cfv: flag(chat.favoritesVisible),
			cst: flag(chat.staffToolsVisible),
			cuv: flag(chat.updaterVisible),
			ml: flag(showMemberList),
			co: clampRounded(composer.desktopActionCount, 0, MAX_COMPOSER_ACTION_COUNT),
			com: clampRounded(composer.mobileActionCount, 0, MAX_COMPOSER_ACTION_COUNT),
			cdv: flag(composer.sendDividerVisible),
			pc: flag(message.compact),
			pg: clampRounded(message.messageGutterPx, 0, MAX_MESSAGE_METRIC_PX),
			pf: clampRounded(message.fontSizePx, 8, 64),
			ps: clampRounded(message.groupSpacingPx, 0, MAX_MESSAGE_METRIC_PX),
			pa: flag(message.compactAvatarsVisible),
			pt: clampRounded(message.compactTimestampWidthPx, 0, MAX_MESSAGE_METRIC_PX),
			pv: clampRounded(message.viewportHeightPx, 0, MAX_VIEWPORT_HEIGHT_PX),
			nr: (getRememberedSkeletonNagbarLayout()?.rows ?? [])
				.slice(0, MAX_NAGBAR_ROWS)
				.map((row) => [
					Math.max(0, SHELL_HINT_NAGBAR_TONE_ORDER.indexOf(row.tone)),
					flag(row.hasActions),
					flag(row.dismissible),
				]),
			vh: clampRounded(voice.panelHeightPx, 0, MAX_VOICE_PANEL_HEIGHT_PX),
			vc: flag(voice.connected),
			mr: flag(mobile && showsMobileGuildRail(pathname)),
			ms: flag(mobile && showsMobileSidebar(pathname)),
			mn: flag(mobile && showsMobileBottomNav(pathname)),
			fa: flag(friends.activeNowVisible),
			fn: friends.activeNowCards
				.slice(0, MAX_ACTIVE_NOW_CARDS)
				.map((card) => [clampRounded(card.participantCount, 0, MAX_ACTIVE_NOW_PARTICIPANTS), flag(card.streaming)]),
		};
		if (guildChannelList != null) {
			hint.gl = encodeGuildChannelGroups(guildChannelList);
		}
		if (dmSidebar != null) {
			hint.sb = flags(dmSidebar.channelSubtextFlags, MAX_DM_SIDEBAR_ROWS);
		}
		if (showMemberList && chat.rememberedMemberGroups != null) {
			hint.mem = encodeMemberGroups(chat.rememberedMemberGroups);
		}
		writeRawStorageItem(SHELL_HINT_STORAGE_KEY, JSON.stringify(hint));
	} catch (error) {
		console.warn('[ShellHint] Failed to write the shell hint', error);
	}
}
