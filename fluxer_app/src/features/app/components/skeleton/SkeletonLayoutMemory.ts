// SPDX-License-Identifier: AGPL-3.0-or-later

import {getDefaultMessageGroupSpacing} from '@app/features/accessibility/state/MessageGroupSpacing';
import {DEFAULT_MESSAGE_GUTTER_PX} from '@app/features/accessibility/state/MessagePresentationDefaults';
import {NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import SessionManager from '@app/features/platform/state/AuthSession';
import {
	AuthSessionStorageKey,
	parseStoredSessionValue,
} from '@app/features/platform/state/auth_session/AuthSessionStorage';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import {MESSAGE_LAYOUT_SPEC} from '@app/features/theme/layout/MessageLayoutSpec';
import {getRemScaleForDocument, REM_BASE_PX} from '@app/features/theme/layout/RemFromPx';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';

const STORAGE_KEY = 'SkeletonLayoutMemory';
const STORAGE_VERSION = 9;
const MIN_READABLE_STORAGE_VERSION = 4;
const SNAPSHOT_TTL_MS = 24 * 60 * 60 * 1000;
const FUTURE_TIMESTAMP_TOLERANCE_MS = 5 * 60 * 1000;
const WRITE_COALESCE_MS = 1000;
const MAX_DM_SIDEBAR_ROWS = 40;
const MAX_GUILD_RAIL_INLINE_DM_ROWS = 16;
export const SKELETON_GUILD_RAIL_ORGANIZED_VISUAL_ROW_LIMIT = 24;
const MAX_GUILD_RAIL_SCROLL_TOP_PX = 100_000;
export const SKELETON_GUILD_RAIL_COLLAPSED_FOLDER_CHILD_LIMIT = 4;
const MAX_FRIEND_ROWS = 40;
const MAX_ACTIVE_NOW_CARDS = 40;
const MAX_ACTIVE_NOW_PARTICIPANTS = 99;
const MAX_NAGBAR_ROWS = 3;
const MAX_MEMBER_GROUPS = 8;
const MAX_MEMBER_GROUP_ROWS = 50;
const MAX_CHANNEL_MEMBER_LAYOUTS = 16;
const MAX_CHANNEL_PROJECTIONS = 16;
const MAX_GUILD_PRESENTATIONS = 8;
const MAX_GUILD_CHANNEL_LISTS = 6;
const MAX_GUILD_CHANNEL_GROUPS = 16;
const MAX_GUILD_CHANNEL_ROWS = 64;
const MAX_DISCOVERY_CATEGORY_TABS = 24;
const MAX_DISCOVERY_COLUMNS = 4;
const MAX_DISCOVERY_VISIBLE_ROWS = 64;
const MAX_SIMPLE_PAGE_ROWS = 200;
const MAX_HEADER_ACTION_COUNT = 8;
const MAX_COMPOSER_DESKTOP_ACTION_COUNT = 8;
const MAX_COMPOSER_MOBILE_ACTION_COUNT = 4;
const MAX_MEASURED_WIDTH_PX = 4096;
const MAX_MEASURED_HEIGHT_PX = 8192;
const MAX_MESSAGE_GUTTER_PX = 200;
const MAX_MESSAGE_GROUP_SPACING_PX = 64;
const MIN_MESSAGE_FONT_SIZE_PX = 8;
const MAX_MESSAGE_FONT_SIZE_PX = 64;
const MAX_VOICE_CONNECTION_HEIGHT_PX = 512;
const MIN_BANNER_ASPECT_RATIO = 0.25;
const MAX_BANNER_ASPECT_RATIO = 8;
const BANNER_ASPECT_RATIO_PRECISION = 1000;
const ACCOUNT_ID_PATTERN = /^[0-9]{1,32}$/u;
const CHANNEL_ID_PATTERN = /^[0-9]{1,32}$/u;
const GUILD_ID_PATTERN = /^[0-9]{1,32}$/u;
const FINGERPRINT_PATTERN = /^[0-9a-f]{16}$/u;

function pxFromSpecRem(value: `${number}rem`): number {
	return Math.round(Number.parseFloat(value) * REM_BASE_PX);
}

export const SKELETON_UNMEASURED_WIDTH_PX = 0;
export const SKELETON_NO_SELECTED_RAIL_ITEM_INDEX = -1;
export const SKELETON_DEFAULT_MESSAGE_GUTTER_PX = DEFAULT_MESSAGE_GUTTER_PX;
export const SKELETON_DEFAULT_MESSAGE_FONT_SIZE_PX = REM_BASE_PX;
export const SKELETON_DEFAULT_COMPACT_TIMESTAMP_WIDTH_PX = pxFromSpecRem(MESSAGE_LAYOUT_SPEC.dense.timestampWidth);
export const SKELETON_DEFAULT_MESSAGE_DISPLAY_COMPACT = false;
export const SKELETON_DEFAULT_COMPACT_AVATARS_VISIBLE = false;
export const SKELETON_DEFAULT_SEND_DIVIDER_VISIBLE = false;
export const SKELETON_COMPOSER_EXPRESSION_ACTION_COUNT_WITHOUT_GIF = 3;
export const SKELETON_COMPOSER_DEFAULT_MOBILE_ACTION_COUNT = 2;
export const SKELETON_DEFAULT_FAVORITES_VISIBLE = true;
export const SKELETON_DEFAULT_GUILD_BANNER_ASPECT_RATIO = 16 / 9;
export const SKELETON_DISCOVERY_MAX_COLUMNS = MAX_DISCOVERY_COLUMNS;
export const SKELETON_DISCOVERY_MIN_CARD_WIDTH_PX = 320;
export const SKELETON_DISCOVERY_MAX_CARD_WIDTH_PX = 400;
export const SKELETON_DISCOVERY_GRID_GAP_PX = 16;
export const SKELETON_DISCOVERY_ESTIMATED_ROW_HEIGHT_PX = 344;
const SKELETON_DISCOVERY_SHELL_CHROME_WIDTH_PX = pxFromSpecRem('24.5rem');
const SKELETON_DISCOVERY_CONTENT_PADDING_PX = pxFromSpecRem('2rem');

export const SkeletonMemberSurfaceKind = Object.freeze({
	GUILD: 'guild',
	GUILD_VOICE: 'guild_voice',
	GROUP_DM: 'group_dm',
} as const);

export type SkeletonMemberSurfaceKind = (typeof SkeletonMemberSurfaceKind)[keyof typeof SkeletonMemberSurfaceKind];

export const SkeletonChannelProjectionKind = Object.freeze({
	GUILD: 'guild',
	GUILD_VOICE: 'guild_voice',
	DM: 'dm',
	GROUP_DM: 'group_dm',
	PERSONAL_NOTES: 'personal_notes',
} as const);

export type SkeletonChannelProjectionKind =
	(typeof SkeletonChannelProjectionKind)[keyof typeof SkeletonChannelProjectionKind];

export function resolveSkeletonChannelProjectionKind(channelType: number): SkeletonChannelProjectionKind {
	switch (channelType) {
		case ChannelTypes.DM:
			return SkeletonChannelProjectionKind.DM;
		case ChannelTypes.GROUP_DM:
			return SkeletonChannelProjectionKind.GROUP_DM;
		case ChannelTypes.DM_PERSONAL_NOTES:
			return SkeletonChannelProjectionKind.PERSONAL_NOTES;
		case ChannelTypes.GUILD_VOICE:
			return SkeletonChannelProjectionKind.GUILD_VOICE;
		default:
			return SkeletonChannelProjectionKind.GUILD;
	}
}

export function resolveSkeletonMemberSurfaceKind(
	channelKind: SkeletonChannelProjectionKind,
): SkeletonMemberSurfaceKind | null {
	switch (channelKind) {
		case SkeletonChannelProjectionKind.GUILD:
			return SkeletonMemberSurfaceKind.GUILD;
		case SkeletonChannelProjectionKind.GUILD_VOICE:
			return SkeletonMemberSurfaceKind.GUILD_VOICE;
		case SkeletonChannelProjectionKind.GROUP_DM:
			return SkeletonMemberSurfaceKind.GROUP_DM;
		case SkeletonChannelProjectionKind.DM:
		case SkeletonChannelProjectionKind.PERSONAL_NOTES:
			return null;
	}
}

export const SkeletonGuildRailItemKind = Object.freeze({
	GUILD: 'guild',
	COLLAPSED_FOLDER: 'collapsed_folder',
	EXPANDED_FOLDER: 'expanded_folder',
} as const);

export type SkeletonGuildRailItemKind = (typeof SkeletonGuildRailItemKind)[keyof typeof SkeletonGuildRailItemKind];

export const SkeletonGuildRailItemIndicator = Object.freeze({
	NONE: 'none',
	UNREAD: 'unread',
	MENTION: 'mention',
} as const);

export type SkeletonGuildRailItemIndicator =
	(typeof SkeletonGuildRailItemIndicator)[keyof typeof SkeletonGuildRailItemIndicator];

export const SkeletonGuildBannerPlacement = Object.freeze({
	NONE: 'none',
	INTEGRATED: 'integrated',
	DETACHED: 'detached',
} as const);

export type SkeletonGuildBannerPlacement =
	(typeof SkeletonGuildBannerPlacement)[keyof typeof SkeletonGuildBannerPlacement];

export const SkeletonFriendsTab = Object.freeze({
	ONLINE: 'online',
	ALL: 'all',
	PENDING: 'pending',
	ADD: 'add',
} as const);

export type SkeletonFriendsTab = (typeof SkeletonFriendsTab)[keyof typeof SkeletonFriendsTab];

export const SkeletonNagbarTone = NagbarToneKind;

export type SkeletonNagbarTone = NagbarToneKind;

export const SkeletonSimplePageRoute = Object.freeze({
	GUILD_MEMBERS: 'guild_members',
	BOOKMARKS: 'bookmarks',
	MENTIONS: 'mentions',
	NOTIFICATIONS: 'notifications',
} as const);

export type SkeletonSimplePageRoute = (typeof SkeletonSimplePageRoute)[keyof typeof SkeletonSimplePageRoute];

export const SkeletonSimplePageBody = Object.freeze({
	MEMBER_TABLE: 'member_table',
	MESSAGE_LIST: 'message_list',
	CHANNEL_LIST: 'channel_list',
} as const);

export type SkeletonSimplePageBody = (typeof SkeletonSimplePageBody)[keyof typeof SkeletonSimplePageBody];

export const SKELETON_DEFAULT_SIMPLE_PAGE_BODY: Readonly<Record<SkeletonSimplePageRoute, SkeletonSimplePageBody>> =
	Object.freeze({
		[SkeletonSimplePageRoute.GUILD_MEMBERS]: SkeletonSimplePageBody.MEMBER_TABLE,
		[SkeletonSimplePageRoute.BOOKMARKS]: SkeletonSimplePageBody.MESSAGE_LIST,
		[SkeletonSimplePageRoute.MENTIONS]: SkeletonSimplePageBody.MESSAGE_LIST,
		[SkeletonSimplePageRoute.NOTIFICATIONS]: SkeletonSimplePageBody.CHANNEL_LIST,
	});

export type RememberedSkeletonGuildRailItem =
	| {readonly kind: typeof SkeletonGuildRailItemKind.GUILD; readonly indicator: SkeletonGuildRailItemIndicator}
	| {
			readonly kind: typeof SkeletonGuildRailItemKind.COLLAPSED_FOLDER;
			readonly indicator: SkeletonGuildRailItemIndicator;
			readonly childCount: number;
			readonly showIconWhenCollapsed: boolean;
	  }
	| {
			readonly kind: typeof SkeletonGuildRailItemKind.EXPANDED_FOLDER;
			readonly indicator: SkeletonGuildRailItemIndicator;
			readonly childCount: number;
			readonly childIndicators: ReadonlyArray<SkeletonGuildRailItemIndicator>;
			readonly selectedChildIndex: number;
	  };

export interface RememberedSkeletonGuildRailLayout {
	readonly inlineDmRowCount: number;
	readonly inlineDmUnreadFlags: ReadonlyArray<boolean>;
	readonly selectedInlineDmRowIndex: number;
	readonly outageVisible: boolean;
	readonly fluxerVisible: boolean;
	readonly favoritesVisible: boolean;
	readonly discoveryVisible: boolean;
	readonly addGuildVisible: boolean;
	readonly downloadVisible: boolean;
	readonly helpVisible: boolean;
	readonly selectedItemIndex: number;
	readonly organizedItems: ReadonlyArray<RememberedSkeletonGuildRailItem>;
	readonly scrollTopPx: number;
}

export interface RememberedSkeletonDMSidebarLayout {
	readonly isMobile: boolean;
	readonly friendsVisible: boolean;
	readonly personalNotesVisible: boolean;
	readonly premiumVisible: boolean;
	readonly sectionVisible: boolean;
	readonly channelRowCount: number;
	readonly channelSubtextFlags: ReadonlyArray<boolean>;
}

export interface RememberedSkeletonActiveNowCard {
	readonly participantCount: number;
	readonly streaming: boolean;
}

export interface RememberedSkeletonFriendsLayout {
	readonly activeTab: SkeletonFriendsTab;
	readonly onlineRowCount: number;
	readonly allRowCount: number;
	readonly pendingRowCount: number;
	readonly pendingIncomingRowCount: number;
	readonly tabWidthsPx: ReadonlyArray<number>;
	readonly pendingBadgeVisible: boolean;
	readonly activeNowVisible: boolean;
	readonly activeNowCards: ReadonlyArray<RememberedSkeletonActiveNowCard>;
}

export interface RememberedSkeletonChannelHeaderLayout {
	readonly staffToolsVisible: boolean;
	readonly updaterVisible: boolean;
	readonly favoritesVisible: boolean;
}

export interface RememberedSkeletonNagbarRow {
	readonly tone: SkeletonNagbarTone;
	readonly hasActions: boolean;
	readonly dismissible: boolean;
}

export interface RememberedSkeletonNagbarLayout {
	readonly rows: ReadonlyArray<RememberedSkeletonNagbarRow>;
}

export interface RememberedSkeletonComposerLayout {
	readonly desktopActionCount: number;
	readonly mobileActionCount: number;
	readonly sendDividerVisible: boolean;
}

export interface RememberedSkeletonMessagePresentation {
	readonly compact: boolean;
	readonly messageGutterPx: number;
	readonly fontSizePx: number;
	readonly groupSpacingPx: number;
	readonly compactAvatarsVisible: boolean;
	readonly compactTimestampWidthPx: number;
	readonly viewportHeightPx: number;
}

export interface RememberedSkeletonVoicePresence {
	readonly connected: boolean;
	readonly panelHeightPx: number;
}

export interface RememberedSkeletonDiscoveryLayout {
	readonly columnCount: number;
	readonly visibleRowCount: number;
	readonly categoryTabWidthsPx: ReadonlyArray<number>;
}

export interface RememberedSkeletonSimplePageLayout {
	readonly body: SkeletonSimplePageBody;
	readonly rowCount: number;
	readonly selectable: boolean;
}

export interface RememberedSkeletonMemberGroup {
	readonly rowCount: number;
	readonly headingWidthPx: number;
	readonly subtextFlags: ReadonlyArray<boolean>;
}

export interface RememberedSkeletonChannelProjection {
	readonly channelKind: SkeletonChannelProjectionKind;
	readonly showTopic: boolean;
	readonly nameWidthPx: number;
	readonly topicWidthPx: number;
	readonly desktopLeadingActionCount: number;
	readonly mobileActionCount: number;
	readonly memberListVisible: boolean;
	readonly searchPanelOpen: boolean;
}

export interface RememberedSkeletonGuildPresentation {
	readonly headerNameWidthPx: number;
	readonly badgeVisible: boolean;
	readonly bannerPlacement: SkeletonGuildBannerPlacement;
	readonly bannerAspectRatio: number;
}

export interface RememberedSkeletonGuildChannelRow {
	readonly voice: boolean;
	readonly nameWidthPx: number;
}

export interface RememberedSkeletonGuildChannelGroup {
	readonly categoryHeaderVisible: boolean;
	readonly collapsed: boolean;
	readonly categoryNameWidthPx: number;
	readonly channels: ReadonlyArray<RememberedSkeletonGuildChannelRow>;
}

export interface RememberedSkeletonGuildChannelList {
	readonly membersRowVisible: boolean;
	readonly groups: ReadonlyArray<RememberedSkeletonGuildChannelGroup>;
}

const EMPTY_ACTIVE_NOW_CARDS: ReadonlyArray<RememberedSkeletonActiveNowCard> = Object.freeze([]);

export const SKELETON_DEFAULT_FRIENDS_LAYOUT: RememberedSkeletonFriendsLayout = Object.freeze({
	activeTab: SkeletonFriendsTab.ONLINE,
	onlineRowCount: 0,
	allRowCount: 0,
	pendingRowCount: 0,
	pendingIncomingRowCount: 0,
	tabWidthsPx: Object.freeze([
		SKELETON_UNMEASURED_WIDTH_PX,
		SKELETON_UNMEASURED_WIDTH_PX,
		SKELETON_UNMEASURED_WIDTH_PX,
		SKELETON_UNMEASURED_WIDTH_PX,
	]),
	pendingBadgeVisible: false,
	activeNowVisible: true,
	activeNowCards: EMPTY_ACTIVE_NOW_CARDS,
});

export const SKELETON_DEFAULT_CHANNEL_HEADER_LAYOUT: RememberedSkeletonChannelHeaderLayout = Object.freeze({
	staffToolsVisible: false,
	updaterVisible: false,
	favoritesVisible: SKELETON_DEFAULT_FAVORITES_VISIBLE,
});

export const SKELETON_DEFAULT_NAGBAR_LAYOUT: RememberedSkeletonNagbarLayout = Object.freeze({
	rows: Object.freeze([]),
});

export const SKELETON_DEFAULT_VOICE_PRESENCE: RememberedSkeletonVoicePresence = Object.freeze({
	connected: false,
	panelHeightPx: 0,
});

const SKELETON_DEFAULT_DISCOVERY_CATEGORY_TABS: ReadonlyArray<number> = Object.freeze([SKELETON_UNMEASURED_WIDTH_PX]);

export function resolveDefaultSkeletonDiscoveryLayout(): RememberedSkeletonDiscoveryLayout {
	if (typeof window === 'undefined') {
		return Object.freeze({
			columnCount: SKELETON_DISCOVERY_MAX_COLUMNS,
			visibleRowCount: 1,
			categoryTabWidthsPx: SKELETON_DEFAULT_DISCOVERY_CATEGORY_TABS,
		});
	}
	const remScale = getRemScaleForDocument(window.document);
	const contentWidthPx = Math.max(0, window.innerWidth - SKELETON_DISCOVERY_SHELL_CHROME_WIDTH_PX * remScale);
	const gridWidthPx = Math.max(0, contentWidthPx - SKELETON_DISCOVERY_CONTENT_PADDING_PX * remScale);
	const gapPx = SKELETON_DISCOVERY_GRID_GAP_PX * remScale;
	const columnsThatFit = Math.floor((gridWidthPx + gapPx) / (SKELETON_DISCOVERY_MIN_CARD_WIDTH_PX * remScale + gapPx));
	const visibleRowCount = Math.ceil(window.innerHeight / (SKELETON_DISCOVERY_ESTIMATED_ROW_HEIGHT_PX * remScale));
	return Object.freeze({
		columnCount: Math.min(SKELETON_DISCOVERY_MAX_COLUMNS, Math.max(1, columnsThatFit)),
		visibleRowCount: Math.min(MAX_DISCOVERY_VISIBLE_ROWS, Math.max(1, visibleRowCount)),
		categoryTabWidthsPx: SKELETON_DEFAULT_DISCOVERY_CATEGORY_TABS,
	});
}

export function resolveDefaultSkeletonComposerLayout(gifButtonAvailable: boolean): RememberedSkeletonComposerLayout {
	let desktopActionCount = SKELETON_COMPOSER_EXPRESSION_ACTION_COUNT_WITHOUT_GIF;
	if (gifButtonAvailable) {
		desktopActionCount += 1;
	}
	return Object.freeze({
		desktopActionCount,
		mobileActionCount: SKELETON_COMPOSER_DEFAULT_MOBILE_ACTION_COUNT,
		sendDividerVisible: SKELETON_DEFAULT_SEND_DIVIDER_VISIBLE,
	});
}

export function resolveDefaultSkeletonChatViewportHeightPx(): number {
	if (typeof window === 'undefined') {
		return 0;
	}
	const remScaledHeight = window.innerHeight / getRemScaleForDocument(window.document);
	return Math.min(MAX_MEASURED_HEIGHT_PX, Math.max(0, Math.round(remScaledHeight)));
}

export function resolveDefaultSkeletonMessagePresentation(): RememberedSkeletonMessagePresentation {
	return Object.freeze({
		compact: SKELETON_DEFAULT_MESSAGE_DISPLAY_COMPACT,
		messageGutterPx: SKELETON_DEFAULT_MESSAGE_GUTTER_PX,
		fontSizePx: SKELETON_DEFAULT_MESSAGE_FONT_SIZE_PX,
		groupSpacingPx: getDefaultMessageGroupSpacing(SKELETON_DEFAULT_MESSAGE_DISPLAY_COMPACT),
		compactAvatarsVisible: SKELETON_DEFAULT_COMPACT_AVATARS_VISIBLE,
		compactTimestampWidthPx: SKELETON_DEFAULT_COMPACT_TIMESTAMP_WIDTH_PX,
		viewportHeightPx: resolveDefaultSkeletonChatViewportHeightPx(),
	});
}

export function resolveDefaultSkeletonSimplePageLayout(
	route: SkeletonSimplePageRoute,
): RememberedSkeletonSimplePageLayout {
	return Object.freeze({body: SKELETON_DEFAULT_SIMPLE_PAGE_BODY[route], rowCount: 0, selectable: false});
}

interface TimedSkeletonGuildRailLayout extends RememberedSkeletonGuildRailLayout {
	readonly capturedAt: number;
}

interface TimedSkeletonDMSidebarLayout extends RememberedSkeletonDMSidebarLayout {
	readonly capturedAt: number;
}

interface TimedSkeletonFriendsLayout extends RememberedSkeletonFriendsLayout {
	readonly capturedAt: number;
}

interface TimedSkeletonChannelHeaderLayout extends RememberedSkeletonChannelHeaderLayout {
	readonly capturedAt: number;
}

interface TimedSkeletonNagbarLayout extends RememberedSkeletonNagbarLayout {
	readonly capturedAt: number;
}

interface TimedSkeletonComposerLayout extends RememberedSkeletonComposerLayout {
	readonly capturedAt: number;
}

interface TimedSkeletonMessagePresentation extends RememberedSkeletonMessagePresentation {
	readonly capturedAt: number;
}

interface TimedSkeletonVoicePresence extends RememberedSkeletonVoicePresence {
	readonly capturedAt: number;
}

interface TimedSkeletonDiscoveryLayout extends RememberedSkeletonDiscoveryLayout {
	readonly capturedAt: number;
}

interface PendingSkeletonSimplePageLayout extends RememberedSkeletonSimplePageLayout {
	readonly route: SkeletonSimplePageRoute;
}

interface TimedSkeletonSimplePageLayout extends PendingSkeletonSimplePageLayout {
	readonly capturedAt: number;
}

interface PendingSkeletonChannelMemberLayout {
	readonly fingerprint: string;
	readonly kind: SkeletonMemberSurfaceKind;
	readonly memberGroups: ReadonlyArray<RememberedSkeletonMemberGroup>;
}

interface TimedSkeletonChannelMemberLayout extends PendingSkeletonChannelMemberLayout {
	readonly capturedAt: number;
}

interface PendingSkeletonChannelProjection extends RememberedSkeletonChannelProjection {
	readonly fingerprint: string;
}

interface TimedSkeletonChannelProjection extends PendingSkeletonChannelProjection {
	readonly capturedAt: number;
}

interface PendingSkeletonGuildPresentation extends RememberedSkeletonGuildPresentation {
	readonly fingerprint: string;
}

interface TimedSkeletonGuildPresentation extends PendingSkeletonGuildPresentation {
	readonly capturedAt: number;
}

interface PendingSkeletonGuildChannelList extends RememberedSkeletonGuildChannelList {
	readonly fingerprint: string;
}

interface TimedSkeletonGuildChannelList extends PendingSkeletonGuildChannelList {
	readonly capturedAt: number;
}

interface SkeletonChromeLayouts {
	readonly dmSidebar?: TimedSkeletonDMSidebarLayout;
	readonly guildRail?: TimedSkeletonGuildRailLayout;
}

interface SkeletonLayoutState {
	readonly chrome?: SkeletonChromeLayouts;
	readonly friends?: TimedSkeletonFriendsLayout;
	readonly channelHeader?: TimedSkeletonChannelHeaderLayout;
	readonly nagbar?: TimedSkeletonNagbarLayout;
	readonly composer?: TimedSkeletonComposerLayout;
	readonly messagePresentation?: TimedSkeletonMessagePresentation;
	readonly voice?: TimedSkeletonVoicePresence;
	readonly discovery?: TimedSkeletonDiscoveryLayout;
	readonly simplePages?: ReadonlyArray<TimedSkeletonSimplePageLayout>;
	readonly channelMemberLayouts?: ReadonlyArray<TimedSkeletonChannelMemberLayout>;
	readonly channelProjections?: ReadonlyArray<TimedSkeletonChannelProjection>;
	readonly guildPresentations?: ReadonlyArray<TimedSkeletonGuildPresentation>;
	readonly guildChannelLists?: ReadonlyArray<TimedSkeletonGuildChannelList>;
}

interface PendingFriendsLayout {
	activeTab?: SkeletonFriendsTab;
	onlineRowCount?: number;
	allRowCount?: number;
	pendingRowCount?: number;
	pendingIncomingRowCount?: number;
	tabWidthsPx?: ReadonlyArray<number>;
	pendingBadgeVisible?: boolean;
	activeNowVisible?: boolean;
	activeNowCards?: ReadonlyArray<RememberedSkeletonActiveNowCard>;
}

interface PendingMessagePresentation {
	compact?: boolean;
	messageGutterPx?: number;
	fontSizePx?: number;
	groupSpacingPx?: number;
	compactAvatarsVisible?: boolean;
	compactTimestampWidthPx?: number;
	viewportHeightPx?: number;
}

interface PendingDiscoveryLayout {
	columnCount?: number;
	visibleRowCount?: number;
	categoryTabWidthsPx?: ReadonlyArray<number>;
}

interface PendingChromeLayouts {
	dmSidebar?: RememberedSkeletonDMSidebarLayout;
	guildRail?: RememberedSkeletonGuildRailLayout;
}

type OptionalParseResult<T> =
	| {readonly status: 'absent'}
	| {readonly status: 'invalid'}
	| {readonly status: 'valid'; readonly value: T};

const ABSENT_PARSE_RESULT = Object.freeze({status: 'absent'} as const);
const INVALID_PARSE_RESULT = Object.freeze({status: 'invalid'} as const);

const SKELETON_LAYOUT_STATE_KEY_FLAGS: Readonly<Record<keyof Required<SkeletonLayoutState>, true>> = Object.freeze({
	chrome: true,
	friends: true,
	channelHeader: true,
	nagbar: true,
	composer: true,
	messagePresentation: true,
	voice: true,
	discovery: true,
	simplePages: true,
	channelMemberLayouts: true,
	channelProjections: true,
	guildPresentations: true,
	guildChannelLists: true,
});
const SKELETON_LAYOUT_STATE_KEYS = Object.keys(SKELETON_LAYOUT_STATE_KEY_FLAGS) as ReadonlyArray<
	keyof SkeletonLayoutState
>;
const ROOT_ENVELOPE_KEYS = ['version', 'updatedAt', 'accountFingerprint'] as const;
const ROOT_KEYS = new Set<string>([...ROOT_ENVELOPE_KEYS, ...SKELETON_LAYOUT_STATE_KEYS]);
const CHROME_KEYS = new Set(['dmSidebar', 'guildRail']);
const DM_SIDEBAR_KEYS = new Set([
	'capturedAt',
	'isMobile',
	'friendsVisible',
	'personalNotesVisible',
	'premiumVisible',
	'sectionVisible',
	'channelRowCount',
	'channelSubtextFlags',
]);
const GUILD_RAIL_KEYS = new Set([
	'capturedAt',
	'inlineDmRowCount',
	'inlineDmUnreadFlags',
	'selectedInlineDmRowIndex',
	'outageVisible',
	'fluxerVisible',
	'favoritesVisible',
	'discoveryVisible',
	'addGuildVisible',
	'downloadVisible',
	'helpVisible',
	'selectedItemIndex',
	'organizedItems',
	'scrollTopPx',
]);
const GUILD_RAIL_ITEM_KEYS = new Set(['kind', 'indicator']);
const COLLAPSED_GUILD_RAIL_ITEM_KEYS = new Set(['kind', 'indicator', 'childCount', 'showIconWhenCollapsed']);
const EXPANDED_GUILD_RAIL_ITEM_KEYS = new Set([
	'kind',
	'indicator',
	'childCount',
	'childIndicators',
	'selectedChildIndex',
]);
const FRIENDS_KEYS = new Set([
	'capturedAt',
	'activeTab',
	'onlineRowCount',
	'allRowCount',
	'pendingRowCount',
	'pendingIncomingRowCount',
	'tabWidthsPx',
	'pendingBadgeVisible',
	'activeNowVisible',
	'activeNowCards',
]);
const ACTIVE_NOW_CARD_KEYS = new Set(['participantCount', 'streaming']);
const CHANNEL_HEADER_KEYS = new Set(['capturedAt', 'staffToolsVisible', 'updaterVisible', 'favoritesVisible']);
const NAGBAR_KEYS = new Set(['capturedAt', 'rows']);
const NAGBAR_ROW_KEYS = new Set(['tone', 'hasActions', 'dismissible']);
const COMPOSER_KEYS = new Set(['capturedAt', 'desktopActionCount', 'mobileActionCount', 'sendDividerVisible']);
const MESSAGE_PRESENTATION_KEYS = new Set([
	'capturedAt',
	'compact',
	'messageGutterPx',
	'fontSizePx',
	'groupSpacingPx',
	'compactAvatarsVisible',
	'compactTimestampWidthPx',
	'viewportHeightPx',
]);
const VOICE_KEYS = new Set(['capturedAt', 'connected', 'panelHeightPx']);
const DISCOVERY_KEYS = new Set(['capturedAt', 'columnCount', 'visibleRowCount', 'categoryTabWidthsPx']);
const SIMPLE_PAGE_KEYS = new Set(['capturedAt', 'route', 'body', 'rowCount', 'selectable']);
const CHANNEL_MEMBER_LAYOUT_KEYS = new Set(['capturedAt', 'fingerprint', 'kind', 'memberGroups']);
const MEMBER_GROUP_KEYS = new Set(['rowCount', 'headingWidthPx', 'subtextFlags']);
const CHANNEL_PROJECTION_KEYS = new Set([
	'capturedAt',
	'fingerprint',
	'channelKind',
	'showTopic',
	'nameWidthPx',
	'topicWidthPx',
	'desktopLeadingActionCount',
	'mobileActionCount',
	'memberListVisible',
	'searchPanelOpen',
]);
const GUILD_PRESENTATION_KEYS = new Set([
	'capturedAt',
	'fingerprint',
	'headerNameWidthPx',
	'badgeVisible',
	'bannerPlacement',
	'bannerAspectRatio',
]);
const GUILD_CHANNEL_LIST_KEYS = new Set(['capturedAt', 'fingerprint', 'membersRowVisible', 'groups']);
const GUILD_CHANNEL_GROUP_KEYS = new Set(['categoryHeaderVisible', 'collapsed', 'categoryNameWidthPx', 'channels']);
const GUILD_CHANNEL_ROW_KEYS = new Set(['voice', 'nameWidthPx']);
const MEMBER_SURFACE_KINDS = new Set<string>(Object.values(SkeletonMemberSurfaceKind));
const CHANNEL_PROJECTION_KINDS = new Set<string>(Object.values(SkeletonChannelProjectionKind));
const GUILD_RAIL_ITEM_KINDS = new Set<string>(Object.values(SkeletonGuildRailItemKind));
const GUILD_RAIL_ITEM_INDICATORS = new Set<string>(Object.values(SkeletonGuildRailItemIndicator));
const GUILD_BANNER_PLACEMENTS = new Set<string>(Object.values(SkeletonGuildBannerPlacement));
const FRIENDS_TABS = new Set<string>(Object.values(SkeletonFriendsTab));
const NAGBAR_TONES = new Set<string>(Object.values(SkeletonNagbarTone));
const SIMPLE_PAGE_ROUTES = new Set<string>(Object.values(SkeletonSimplePageRoute));
const SIMPLE_PAGE_BODIES = new Set<string>(Object.values(SkeletonSimplePageBody));
const SKELETON_FRIENDS_TAB_COUNT = FRIENDS_TABS.size;
const MAX_SIMPLE_PAGE_LAYOUTS = SIMPLE_PAGE_ROUTES.size;

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value != null && !Array.isArray(value);
}

function hasOnlyKeys(record: Record<string, unknown>, allowedKeys: ReadonlySet<string>): boolean {
	return Object.keys(record).every((key) => allowedKeys.has(key));
}

function parseBoundedInteger(value: unknown, minimum: number, maximum: number): number | null {
	if (typeof value !== 'number' || !Number.isSafeInteger(value) || value < minimum || value > maximum) {
		return null;
	}
	return value;
}

function parseBoundedNumber(value: unknown, minimum: number, maximum: number): number | null {
	if (typeof value !== 'number' || !Number.isFinite(value) || value < minimum || value > maximum) {
		return null;
	}
	return value;
}

function clampReportedInteger(value: number, minimum: number, maximum: number): number | null {
	if (!Number.isFinite(value)) {
		return null;
	}
	return Math.min(maximum, Math.max(minimum, Math.round(value)));
}

function clampReportedRatio(value: number, minimum: number, maximum: number): number | null {
	if (!Number.isFinite(value)) {
		return null;
	}
	const clamped = Math.min(maximum, Math.max(minimum, value));
	return Math.round(clamped * BANNER_ASPECT_RATIO_PRECISION) / BANNER_ASPECT_RATIO_PRECISION;
}

function parseBooleanArray(value: unknown, maximumLength: number): ReadonlyArray<boolean> | null {
	if (!Array.isArray(value) || value.length > maximumLength) {
		return null;
	}
	for (const entry of value) {
		if (typeof entry !== 'boolean') {
			return null;
		}
	}
	return Object.freeze([...(value as Array<boolean>)]);
}

function parseWidthArray(value: unknown, maximumLength: number): ReadonlyArray<number> | null {
	if (!Array.isArray(value) || value.length > maximumLength) {
		return null;
	}
	const widths: Array<number> = [];
	for (const entry of value) {
		const width = parseBoundedInteger(entry, 0, MAX_MEASURED_WIDTH_PX);
		if (width == null) {
			return null;
		}
		widths.push(width);
	}
	return Object.freeze(widths);
}

function clampReportedWidths(values: ReadonlyArray<number>, maximumLength: number): ReadonlyArray<number> | null {
	if (!Array.isArray(values)) {
		return null;
	}
	const widths: Array<number> = [];
	for (const value of values.slice(0, maximumLength)) {
		const width = clampReportedInteger(value, 0, MAX_MEASURED_WIDTH_PX);
		if (width == null) {
			return null;
		}
		widths.push(width);
	}
	return Object.freeze(widths);
}

function isValidTimestamp(value: unknown, now: number): value is number {
	if (typeof value !== 'number' || !Number.isSafeInteger(value) || value < 0) {
		return false;
	}
	return value <= now + FUTURE_TIMESTAMP_TOLERANCE_MS;
}

function isFreshTimestamp(value: unknown, now: number): value is number {
	return isValidTimestamp(value, now) && now - value <= SNAPSHOT_TTL_MS;
}

function createFingerprint(input: string): string {
	let left = 0x811c9dc5;
	let right = 0x9e3779b9;
	for (let index = 0; index < input.length; index += 1) {
		const code = input.charCodeAt(index);
		left = Math.imul(left ^ code, 0x01000193);
		right = Math.imul(right ^ code, 0x85ebca6b);
		right ^= right >>> 13;
	}
	left ^= left >>> 16;
	right ^= right >>> 16;
	return `${(left >>> 0).toString(16).padStart(8, '0')}${(right >>> 0).toString(16).padStart(8, '0')}`;
}

function createAccountFingerprint(accountId: string): string {
	return createFingerprint(`skeleton-account-v1:${accountId}`);
}

function createChannelFingerprint(channelId: string): string {
	return createFingerprint(`skeleton-channel-v1:${channelId}`);
}

function createGuildFingerprint(guildId: string): string {
	return createFingerprint(`skeleton-guild-v1:${guildId}`);
}

function resolveActiveAccountFingerprint(): string | null {
	try {
		const accountId =
			SessionManager.userId ?? parseStoredSessionValue(AppStorage.getItem(AuthSessionStorageKey.UserId));
		if (accountId == null || !ACCOUNT_ID_PATTERN.test(accountId)) {
			return null;
		}
		return createAccountFingerprint(accountId);
	} catch {
		return null;
	}
}

function validParseResult<T>(value: T): OptionalParseResult<T> {
	return Object.freeze({status: 'valid', value});
}

function isValidDMSidebarVisibility(layout: RememberedSkeletonDMSidebarLayout): boolean {
	if (layout.channelSubtextFlags.length !== layout.channelRowCount) {
		return false;
	}
	if (layout.isMobile) {
		return !layout.friendsVisible && !layout.sectionVisible;
	}
	return layout.friendsVisible && layout.sectionVisible;
}

function parseDMSidebarLayout(value: unknown, now: number): OptionalParseResult<TimedSkeletonDMSidebarLayout> {
	if (value === undefined) {
		return ABSENT_PARSE_RESULT;
	}
	if (!isRecord(value) || !hasOnlyKeys(value, DM_SIDEBAR_KEYS) || !isValidTimestamp(value.capturedAt, now)) {
		return INVALID_PARSE_RESULT;
	}
	const channelRowCount = parseBoundedInteger(value.channelRowCount, 0, MAX_DM_SIDEBAR_ROWS);
	const channelSubtextFlags = parseBooleanArray(value.channelSubtextFlags, MAX_DM_SIDEBAR_ROWS);
	if (
		channelRowCount == null ||
		channelSubtextFlags == null ||
		typeof value.isMobile !== 'boolean' ||
		typeof value.friendsVisible !== 'boolean' ||
		typeof value.personalNotesVisible !== 'boolean' ||
		typeof value.premiumVisible !== 'boolean' ||
		typeof value.sectionVisible !== 'boolean'
	) {
		return INVALID_PARSE_RESULT;
	}
	const layout: RememberedSkeletonDMSidebarLayout = {
		isMobile: value.isMobile,
		friendsVisible: value.friendsVisible,
		personalNotesVisible: value.personalNotesVisible,
		premiumVisible: value.premiumVisible,
		sectionVisible: value.sectionVisible,
		channelRowCount,
		channelSubtextFlags,
	};
	if (!isValidDMSidebarVisibility(layout)) {
		return INVALID_PARSE_RESULT;
	}
	if (!isFreshTimestamp(value.capturedAt, now)) {
		return ABSENT_PARSE_RESULT;
	}
	return validParseResult(Object.freeze({...layout, capturedAt: value.capturedAt}));
}

function isSkeletonGuildRailItemKind(value: unknown): value is SkeletonGuildRailItemKind {
	return typeof value === 'string' && GUILD_RAIL_ITEM_KINDS.has(value);
}

function isSkeletonGuildRailItemIndicator(value: unknown): value is SkeletonGuildRailItemIndicator {
	return typeof value === 'string' && GUILD_RAIL_ITEM_INDICATORS.has(value);
}

const EMPTY_GUILD_RAIL_CHILD_INDICATORS: ReadonlyArray<SkeletonGuildRailItemIndicator> = Object.freeze([]);

function normalizeGuildRailChildIndicators(
	value: unknown,
	childCount: number,
): ReadonlyArray<SkeletonGuildRailItemIndicator> {
	if (!Array.isArray(value)) {
		return EMPTY_GUILD_RAIL_CHILD_INDICATORS;
	}
	const indicators: Array<SkeletonGuildRailItemIndicator> = [];
	let hasSignal = false;
	for (let index = 0; index < childCount; index += 1) {
		const entry = value[index];
		if (isSkeletonGuildRailItemIndicator(entry) && entry !== SkeletonGuildRailItemIndicator.NONE) {
			indicators.push(entry);
			hasSignal = true;
		} else {
			indicators.push(SkeletonGuildRailItemIndicator.NONE);
		}
	}
	if (!hasSignal) {
		return EMPTY_GUILD_RAIL_CHILD_INDICATORS;
	}
	return Object.freeze(indicators);
}

function normalizeGuildRailSelectedChildIndex(value: unknown, childCount: number): number {
	if (typeof value !== 'number' || !Number.isSafeInteger(value) || value < 0 || value >= childCount) {
		return SKELETON_NO_SELECTED_RAIL_ITEM_INDEX;
	}
	return value;
}

function parseGuildRailItem(value: unknown): RememberedSkeletonGuildRailItem | null {
	if (
		!isRecord(value) ||
		!isSkeletonGuildRailItemKind(value.kind) ||
		!isSkeletonGuildRailItemIndicator(value.indicator)
	) {
		return null;
	}
	const indicator = value.indicator;
	if (value.kind === SkeletonGuildRailItemKind.EXPANDED_FOLDER) {
		if (!hasOnlyKeys(value, EXPANDED_GUILD_RAIL_ITEM_KEYS)) {
			return null;
		}
		const childCount = parseBoundedInteger(value.childCount, 0, SKELETON_GUILD_RAIL_ORGANIZED_VISUAL_ROW_LIMIT - 1);
		if (childCount == null) {
			return null;
		}
		const childIndicators = normalizeGuildRailChildIndicators(value.childIndicators, childCount);
		const selectedChildIndex = normalizeGuildRailSelectedChildIndex(value.selectedChildIndex, childCount);
		return Object.freeze({kind: value.kind, indicator, childCount, childIndicators, selectedChildIndex});
	}
	if (value.kind === SkeletonGuildRailItemKind.COLLAPSED_FOLDER) {
		if (!hasOnlyKeys(value, COLLAPSED_GUILD_RAIL_ITEM_KEYS)) {
			return null;
		}
		const childCount = parseBoundedInteger(value.childCount, 0, SKELETON_GUILD_RAIL_COLLAPSED_FOLDER_CHILD_LIMIT);
		if (childCount == null || typeof value.showIconWhenCollapsed !== 'boolean') {
			return null;
		}
		return Object.freeze({
			kind: value.kind,
			indicator,
			childCount,
			showIconWhenCollapsed: value.showIconWhenCollapsed,
		});
	}
	if (!hasOnlyKeys(value, GUILD_RAIL_ITEM_KEYS)) {
		return null;
	}
	return Object.freeze({kind: value.kind, indicator});
}

function parseGuildRailLayout(value: unknown, now: number): OptionalParseResult<TimedSkeletonGuildRailLayout> {
	if (value === undefined) {
		return ABSENT_PARSE_RESULT;
	}
	if (!isRecord(value) || !hasOnlyKeys(value, GUILD_RAIL_KEYS) || !isValidTimestamp(value.capturedAt, now)) {
		return INVALID_PARSE_RESULT;
	}
	const inlineDmRowCount = parseBoundedInteger(value.inlineDmRowCount, 0, MAX_GUILD_RAIL_INLINE_DM_ROWS);
	const inlineDmUnreadFlags = parseBooleanArray(value.inlineDmUnreadFlags, MAX_GUILD_RAIL_INLINE_DM_ROWS);
	if (
		inlineDmRowCount == null ||
		inlineDmUnreadFlags == null ||
		inlineDmUnreadFlags.length !== inlineDmRowCount ||
		typeof value.outageVisible !== 'boolean' ||
		typeof value.fluxerVisible !== 'boolean' ||
		typeof value.favoritesVisible !== 'boolean' ||
		typeof value.discoveryVisible !== 'boolean' ||
		typeof value.addGuildVisible !== 'boolean' ||
		typeof value.downloadVisible !== 'boolean' ||
		typeof value.helpVisible !== 'boolean' ||
		value.discoveryVisible !== value.addGuildVisible ||
		(!value.fluxerVisible && inlineDmRowCount > 0) ||
		!Array.isArray(value.organizedItems) ||
		value.organizedItems.length > SKELETON_GUILD_RAIL_ORGANIZED_VISUAL_ROW_LIMIT
	) {
		return INVALID_PARSE_RESULT;
	}
	const organizedItems: Array<RememberedSkeletonGuildRailItem> = [];
	let visualRowCount = 0;
	for (const rawItem of value.organizedItems) {
		const item = parseGuildRailItem(rawItem);
		if (item == null) {
			return INVALID_PARSE_RESULT;
		}
		visualRowCount += 1;
		if (item.kind === SkeletonGuildRailItemKind.EXPANDED_FOLDER) {
			visualRowCount += item.childCount;
		}
		if (visualRowCount > SKELETON_GUILD_RAIL_ORGANIZED_VISUAL_ROW_LIMIT) {
			return INVALID_PARSE_RESULT;
		}
		organizedItems.push(item);
	}
	const selectedItemIndex = parseBoundedInteger(
		value.selectedItemIndex,
		SKELETON_NO_SELECTED_RAIL_ITEM_INDEX,
		Math.max(SKELETON_NO_SELECTED_RAIL_ITEM_INDEX, organizedItems.length - 1),
	);
	const scrollTopPx =
		value.scrollTopPx === undefined ? 0 : parseBoundedInteger(value.scrollTopPx, 0, MAX_GUILD_RAIL_SCROLL_TOP_PX);
	const selectedInlineDmRowIndex = parseBoundedInteger(
		value.selectedInlineDmRowIndex,
		SKELETON_NO_SELECTED_RAIL_ITEM_INDEX,
		Math.max(SKELETON_NO_SELECTED_RAIL_ITEM_INDEX, inlineDmRowCount - 1),
	);
	if (selectedItemIndex == null || scrollTopPx == null || selectedInlineDmRowIndex == null) {
		return INVALID_PARSE_RESULT;
	}
	if (!isFreshTimestamp(value.capturedAt, now)) {
		return ABSENT_PARSE_RESULT;
	}
	return validParseResult(
		Object.freeze({
			capturedAt: value.capturedAt,
			inlineDmRowCount,
			inlineDmUnreadFlags,
			selectedInlineDmRowIndex,
			outageVisible: value.outageVisible,
			fluxerVisible: value.fluxerVisible,
			favoritesVisible: value.favoritesVisible,
			discoveryVisible: value.discoveryVisible,
			addGuildVisible: value.addGuildVisible,
			downloadVisible: value.downloadVisible,
			helpVisible: value.helpVisible,
			selectedItemIndex,
			organizedItems: Object.freeze(organizedItems),
			scrollTopPx,
		}),
	);
}

function parseChromeLayout(value: unknown, now: number): OptionalParseResult<SkeletonChromeLayouts> {
	if (value === undefined) {
		return ABSENT_PARSE_RESULT;
	}
	if (!isRecord(value) || !hasOnlyKeys(value, CHROME_KEYS)) {
		return INVALID_PARSE_RESULT;
	}
	const dmSidebar = parseDMSidebarLayout(value.dmSidebar, now);
	const guildRail = parseGuildRailLayout(value.guildRail, now);
	if (dmSidebar.status !== 'valid' && guildRail.status !== 'valid') {
		if (dmSidebar.status === 'invalid' || guildRail.status === 'invalid') {
			return INVALID_PARSE_RESULT;
		}
		return ABSENT_PARSE_RESULT;
	}
	return validParseResult(
		Object.freeze({
			dmSidebar: dmSidebar.status === 'valid' ? dmSidebar.value : undefined,
			guildRail: guildRail.status === 'valid' ? guildRail.value : undefined,
		}),
	);
}

function isSkeletonFriendsTab(value: unknown): value is SkeletonFriendsTab {
	return typeof value === 'string' && FRIENDS_TABS.has(value);
}

function parseActiveNowCards(value: unknown): ReadonlyArray<RememberedSkeletonActiveNowCard> | null {
	if (!Array.isArray(value) || value.length > MAX_ACTIVE_NOW_CARDS) {
		return null;
	}
	const cards: Array<RememberedSkeletonActiveNowCard> = [];
	for (const rawCard of value) {
		if (!isRecord(rawCard) || !hasOnlyKeys(rawCard, ACTIVE_NOW_CARD_KEYS) || typeof rawCard.streaming !== 'boolean') {
			return null;
		}
		const participantCount = parseBoundedInteger(rawCard.participantCount, 0, MAX_ACTIVE_NOW_PARTICIPANTS);
		if (participantCount == null) {
			return null;
		}
		cards.push(Object.freeze({participantCount, streaming: rawCard.streaming}));
	}
	return Object.freeze(cards);
}

function parseFriendsLayout(value: unknown, now: number): OptionalParseResult<TimedSkeletonFriendsLayout> {
	if (value === undefined) {
		return ABSENT_PARSE_RESULT;
	}
	if (!isRecord(value) || !hasOnlyKeys(value, FRIENDS_KEYS) || !isValidTimestamp(value.capturedAt, now)) {
		return INVALID_PARSE_RESULT;
	}
	const onlineRowCount = parseBoundedInteger(value.onlineRowCount, 0, MAX_FRIEND_ROWS);
	const allRowCount = parseBoundedInteger(value.allRowCount, 0, MAX_FRIEND_ROWS);
	const pendingRowCount = parseBoundedInteger(value.pendingRowCount, 0, MAX_FRIEND_ROWS);
	const pendingIncomingRowCount =
		value.pendingIncomingRowCount === undefined
			? 0
			: parseBoundedInteger(value.pendingIncomingRowCount, 0, MAX_FRIEND_ROWS);
	const tabWidthsPx = parseWidthArray(value.tabWidthsPx, SKELETON_FRIENDS_TAB_COUNT);
	const activeNowCards = parseActiveNowCards(value.activeNowCards);
	if (
		onlineRowCount == null ||
		allRowCount == null ||
		pendingRowCount == null ||
		pendingIncomingRowCount == null ||
		pendingIncomingRowCount > pendingRowCount ||
		tabWidthsPx == null ||
		tabWidthsPx.length !== SKELETON_FRIENDS_TAB_COUNT ||
		activeNowCards == null ||
		!isSkeletonFriendsTab(value.activeTab) ||
		typeof value.pendingBadgeVisible !== 'boolean' ||
		typeof value.activeNowVisible !== 'boolean' ||
		(!value.activeNowVisible && activeNowCards.length !== 0)
	) {
		return INVALID_PARSE_RESULT;
	}
	if (!isFreshTimestamp(value.capturedAt, now)) {
		return ABSENT_PARSE_RESULT;
	}
	return validParseResult(
		Object.freeze({
			capturedAt: value.capturedAt,
			activeTab: value.activeTab,
			onlineRowCount,
			allRowCount,
			pendingRowCount,
			pendingIncomingRowCount,
			tabWidthsPx,
			pendingBadgeVisible: value.pendingBadgeVisible,
			activeNowVisible: value.activeNowVisible,
			activeNowCards,
		}),
	);
}

function parseChannelHeaderLayout(value: unknown, now: number): OptionalParseResult<TimedSkeletonChannelHeaderLayout> {
	if (value === undefined) {
		return ABSENT_PARSE_RESULT;
	}
	if (
		!isRecord(value) ||
		!hasOnlyKeys(value, CHANNEL_HEADER_KEYS) ||
		!isValidTimestamp(value.capturedAt, now) ||
		typeof value.staffToolsVisible !== 'boolean' ||
		typeof value.updaterVisible !== 'boolean' ||
		typeof value.favoritesVisible !== 'boolean'
	) {
		return INVALID_PARSE_RESULT;
	}
	if (!isFreshTimestamp(value.capturedAt, now)) {
		return ABSENT_PARSE_RESULT;
	}
	return validParseResult(
		Object.freeze({
			capturedAt: value.capturedAt,
			staffToolsVisible: value.staffToolsVisible,
			updaterVisible: value.updaterVisible,
			favoritesVisible: value.favoritesVisible,
		}),
	);
}

function isSkeletonNagbarTone(value: unknown): value is SkeletonNagbarTone {
	return typeof value === 'string' && NAGBAR_TONES.has(value);
}

function parseNagbarLayout(value: unknown, now: number): OptionalParseResult<TimedSkeletonNagbarLayout> {
	if (value === undefined) {
		return ABSENT_PARSE_RESULT;
	}
	if (!isRecord(value) || !hasOnlyKeys(value, NAGBAR_KEYS) || !isValidTimestamp(value.capturedAt, now)) {
		return INVALID_PARSE_RESULT;
	}
	if (!Array.isArray(value.rows) || value.rows.length > MAX_NAGBAR_ROWS) {
		return INVALID_PARSE_RESULT;
	}
	const rows: Array<RememberedSkeletonNagbarRow> = [];
	for (const rawRow of value.rows) {
		if (
			!isRecord(rawRow) ||
			!hasOnlyKeys(rawRow, NAGBAR_ROW_KEYS) ||
			!isSkeletonNagbarTone(rawRow.tone) ||
			typeof rawRow.hasActions !== 'boolean'
		) {
			return INVALID_PARSE_RESULT;
		}
		rows.push(
			Object.freeze({tone: rawRow.tone, hasActions: rawRow.hasActions, dismissible: rawRow.dismissible === true}),
		);
	}
	if (!isFreshTimestamp(value.capturedAt, now)) {
		return ABSENT_PARSE_RESULT;
	}
	return validParseResult(Object.freeze({capturedAt: value.capturedAt, rows: Object.freeze(rows)}));
}

function parseComposerLayout(value: unknown, now: number): OptionalParseResult<TimedSkeletonComposerLayout> {
	if (value === undefined) {
		return ABSENT_PARSE_RESULT;
	}
	if (!isRecord(value) || !hasOnlyKeys(value, COMPOSER_KEYS) || !isValidTimestamp(value.capturedAt, now)) {
		return INVALID_PARSE_RESULT;
	}
	const desktopActionCount = parseBoundedInteger(value.desktopActionCount, 0, MAX_COMPOSER_DESKTOP_ACTION_COUNT);
	const mobileActionCount = parseBoundedInteger(value.mobileActionCount, 0, MAX_COMPOSER_MOBILE_ACTION_COUNT);
	if (desktopActionCount == null || mobileActionCount == null || typeof value.sendDividerVisible !== 'boolean') {
		return INVALID_PARSE_RESULT;
	}
	if (!isFreshTimestamp(value.capturedAt, now)) {
		return ABSENT_PARSE_RESULT;
	}
	return validParseResult(
		Object.freeze({
			capturedAt: value.capturedAt,
			desktopActionCount,
			mobileActionCount,
			sendDividerVisible: value.sendDividerVisible,
		}),
	);
}

function parseMessagePresentation(value: unknown, now: number): OptionalParseResult<TimedSkeletonMessagePresentation> {
	if (value === undefined) {
		return ABSENT_PARSE_RESULT;
	}
	if (!isRecord(value) || !hasOnlyKeys(value, MESSAGE_PRESENTATION_KEYS) || !isValidTimestamp(value.capturedAt, now)) {
		return INVALID_PARSE_RESULT;
	}
	const messageGutterPx = parseBoundedInteger(value.messageGutterPx, 0, MAX_MESSAGE_GUTTER_PX);
	const fontSizePx = parseBoundedInteger(value.fontSizePx, MIN_MESSAGE_FONT_SIZE_PX, MAX_MESSAGE_FONT_SIZE_PX);
	const groupSpacingPx = parseBoundedInteger(value.groupSpacingPx, 0, MAX_MESSAGE_GROUP_SPACING_PX);
	const compactTimestampWidthPx = parseBoundedInteger(value.compactTimestampWidthPx, 0, MAX_MEASURED_WIDTH_PX);
	const viewportHeightPx = parseBoundedInteger(value.viewportHeightPx, 0, MAX_MEASURED_HEIGHT_PX);
	if (
		messageGutterPx == null ||
		fontSizePx == null ||
		groupSpacingPx == null ||
		compactTimestampWidthPx == null ||
		viewportHeightPx == null ||
		typeof value.compact !== 'boolean' ||
		typeof value.compactAvatarsVisible !== 'boolean'
	) {
		return INVALID_PARSE_RESULT;
	}
	if (!isFreshTimestamp(value.capturedAt, now)) {
		return ABSENT_PARSE_RESULT;
	}
	return validParseResult(
		Object.freeze({
			capturedAt: value.capturedAt,
			compact: value.compact,
			messageGutterPx,
			fontSizePx,
			groupSpacingPx,
			compactAvatarsVisible: value.compactAvatarsVisible,
			compactTimestampWidthPx,
			viewportHeightPx,
		}),
	);
}

function parseVoicePresence(value: unknown, now: number): OptionalParseResult<TimedSkeletonVoicePresence> {
	if (value === undefined) {
		return ABSENT_PARSE_RESULT;
	}
	if (!isRecord(value) || !hasOnlyKeys(value, VOICE_KEYS) || !isValidTimestamp(value.capturedAt, now)) {
		return INVALID_PARSE_RESULT;
	}
	const panelHeightPx = parseBoundedInteger(value.panelHeightPx, 0, MAX_VOICE_CONNECTION_HEIGHT_PX);
	if (panelHeightPx == null || typeof value.connected !== 'boolean') {
		return INVALID_PARSE_RESULT;
	}
	if (!value.connected && panelHeightPx !== 0) {
		return INVALID_PARSE_RESULT;
	}
	if (!isFreshTimestamp(value.capturedAt, now)) {
		return ABSENT_PARSE_RESULT;
	}
	return validParseResult(Object.freeze({capturedAt: value.capturedAt, connected: value.connected, panelHeightPx}));
}

function parseDiscoveryLayout(value: unknown, now: number): OptionalParseResult<TimedSkeletonDiscoveryLayout> {
	if (value === undefined) {
		return ABSENT_PARSE_RESULT;
	}
	if (!isRecord(value) || !hasOnlyKeys(value, DISCOVERY_KEYS) || !isValidTimestamp(value.capturedAt, now)) {
		return INVALID_PARSE_RESULT;
	}
	const columnCount = parseBoundedInteger(value.columnCount, 1, MAX_DISCOVERY_COLUMNS);
	const visibleRowCount = parseBoundedInteger(value.visibleRowCount, 0, MAX_DISCOVERY_VISIBLE_ROWS);
	const categoryTabWidthsPx = parseWidthArray(value.categoryTabWidthsPx, MAX_DISCOVERY_CATEGORY_TABS);
	if (columnCount == null || visibleRowCount == null || categoryTabWidthsPx == null) {
		return INVALID_PARSE_RESULT;
	}
	if (!isFreshTimestamp(value.capturedAt, now)) {
		return ABSENT_PARSE_RESULT;
	}
	return validParseResult(
		Object.freeze({
			capturedAt: value.capturedAt,
			columnCount,
			visibleRowCount,
			categoryTabWidthsPx,
		}),
	);
}

function isSimplePageRoute(value: unknown): value is SkeletonSimplePageRoute {
	return typeof value === 'string' && SIMPLE_PAGE_ROUTES.has(value);
}

function isSimplePageBody(value: unknown): value is SkeletonSimplePageBody {
	return typeof value === 'string' && SIMPLE_PAGE_BODIES.has(value);
}

function parseSimplePageLayouts(
	value: unknown,
	now: number,
): OptionalParseResult<ReadonlyArray<TimedSkeletonSimplePageLayout>> {
	if (value === undefined) {
		return ABSENT_PARSE_RESULT;
	}
	if (!Array.isArray(value) || value.length > MAX_SIMPLE_PAGE_LAYOUTS) {
		return INVALID_PARSE_RESULT;
	}
	const layouts: Array<TimedSkeletonSimplePageLayout> = [];
	const seenRoutes = new Set<string>();
	for (const rawLayout of value) {
		if (
			!isRecord(rawLayout) ||
			!hasOnlyKeys(rawLayout, SIMPLE_PAGE_KEYS) ||
			!isValidTimestamp(rawLayout.capturedAt, now) ||
			!isSimplePageRoute(rawLayout.route) ||
			!isSimplePageBody(rawLayout.body) ||
			seenRoutes.has(rawLayout.route)
		) {
			staleRecordNeedsRewrite = true;
			continue;
		}
		const rowCount = parseBoundedInteger(rawLayout.rowCount, 0, MAX_SIMPLE_PAGE_ROWS);
		if (rowCount == null) {
			staleRecordNeedsRewrite = true;
			continue;
		}
		seenRoutes.add(rawLayout.route);
		if (!isFreshTimestamp(rawLayout.capturedAt, now)) {
			continue;
		}
		layouts.push(
			Object.freeze({
				capturedAt: rawLayout.capturedAt,
				route: rawLayout.route,
				body: rawLayout.body,
				rowCount,
				selectable: rawLayout.selectable === true,
			}),
		);
	}
	return layouts.length > 0 ? validParseResult(Object.freeze(layouts)) : ABSENT_PARSE_RESULT;
}

function isSkeletonMemberSurfaceKind(value: unknown): value is SkeletonMemberSurfaceKind {
	return typeof value === 'string' && MEMBER_SURFACE_KINDS.has(value);
}

const EMPTY_SUBTEXT_FLAGS: ReadonlyArray<boolean> = Object.freeze([]);

function normalizeSubtextFlags(value: unknown, rowCount: number): ReadonlyArray<boolean> {
	const source = Array.isArray(value) ? value : EMPTY_SUBTEXT_FLAGS;
	let lastTrueIndex = -1;
	for (let index = 0; index < rowCount; index += 1) {
		if (source[index] === true) {
			lastTrueIndex = index;
		}
	}
	if (lastTrueIndex === -1) {
		return EMPTY_SUBTEXT_FLAGS;
	}
	const flags: Array<boolean> = [];
	for (let index = 0; index <= lastTrueIndex; index += 1) {
		flags.push(source[index] === true);
	}
	return Object.freeze(flags);
}

function parseMemberGroups(value: unknown): ReadonlyArray<RememberedSkeletonMemberGroup> | null {
	if (!Array.isArray(value) || value.length > MAX_MEMBER_GROUPS) {
		return null;
	}
	const groups: Array<RememberedSkeletonMemberGroup> = [];
	for (const rawGroup of value) {
		if (!isRecord(rawGroup) || !hasOnlyKeys(rawGroup, MEMBER_GROUP_KEYS)) {
			return null;
		}
		const rowCount = parseBoundedInteger(rawGroup.rowCount, 1, MAX_MEMBER_GROUP_ROWS);
		const headingWidthPx = parseBoundedInteger(rawGroup.headingWidthPx, 0, MAX_MEASURED_WIDTH_PX);
		if (rowCount == null || headingWidthPx == null) {
			return null;
		}
		const storedSubtextFlags =
			rawGroup.subtextFlags === undefined
				? EMPTY_SUBTEXT_FLAGS
				: parseBooleanArray(rawGroup.subtextFlags, MAX_MEMBER_GROUP_ROWS);
		if (storedSubtextFlags == null) {
			return null;
		}
		groups.push(
			Object.freeze({rowCount, headingWidthPx, subtextFlags: normalizeSubtextFlags(storedSubtextFlags, rowCount)}),
		);
	}
	return Object.freeze(groups);
}

function parseChannelMemberLayout(value: unknown, now: number): OptionalParseResult<TimedSkeletonChannelMemberLayout> {
	if (
		!isRecord(value) ||
		!hasOnlyKeys(value, CHANNEL_MEMBER_LAYOUT_KEYS) ||
		!isValidTimestamp(value.capturedAt, now) ||
		typeof value.fingerprint !== 'string' ||
		!FINGERPRINT_PATTERN.test(value.fingerprint) ||
		!isSkeletonMemberSurfaceKind(value.kind)
	) {
		return INVALID_PARSE_RESULT;
	}
	const memberGroups = parseMemberGroups(value.memberGroups);
	if (memberGroups == null) {
		return INVALID_PARSE_RESULT;
	}
	if (memberGroups.length === 0) {
		return ABSENT_PARSE_RESULT;
	}
	if (!isFreshTimestamp(value.capturedAt, now)) {
		return ABSENT_PARSE_RESULT;
	}
	return validParseResult(
		Object.freeze({
			capturedAt: value.capturedAt,
			fingerprint: value.fingerprint,
			kind: value.kind,
			memberGroups,
		}),
	);
}

function parseChannelMemberLayouts(
	value: unknown,
	now: number,
): OptionalParseResult<ReadonlyArray<TimedSkeletonChannelMemberLayout>> {
	if (value === undefined) {
		return ABSENT_PARSE_RESULT;
	}
	if (!Array.isArray(value) || value.length > MAX_CHANNEL_MEMBER_LAYOUTS) {
		return INVALID_PARSE_RESULT;
	}
	const layouts: Array<TimedSkeletonChannelMemberLayout> = [];
	const seenFingerprints = new Set<string>();
	for (const rawLayout of value) {
		if (
			!isRecord(rawLayout) ||
			typeof rawLayout.fingerprint !== 'string' ||
			seenFingerprints.has(rawLayout.fingerprint)
		) {
			staleRecordNeedsRewrite = true;
			continue;
		}
		seenFingerprints.add(rawLayout.fingerprint);
		const layout = parseChannelMemberLayout(rawLayout, now);
		if (layout.status === 'invalid') {
			staleRecordNeedsRewrite = true;
			continue;
		}
		if (layout.status === 'valid') {
			layouts.push(layout.value);
		}
	}
	if (layouts.length === 0) {
		return ABSENT_PARSE_RESULT;
	}
	return validParseResult(Object.freeze(layouts));
}

function isChannelProjectionKind(value: unknown): value is SkeletonChannelProjectionKind {
	return typeof value === 'string' && CHANNEL_PROJECTION_KINDS.has(value);
}

function canChannelProjectionShowTopic(channelKind: SkeletonChannelProjectionKind): boolean {
	return (
		channelKind === SkeletonChannelProjectionKind.GUILD || channelKind === SkeletonChannelProjectionKind.GUILD_VOICE
	);
}

function canChannelProjectionShowMemberList(channelKind: SkeletonChannelProjectionKind): boolean {
	return (
		channelKind === SkeletonChannelProjectionKind.GUILD ||
		channelKind === SkeletonChannelProjectionKind.GUILD_VOICE ||
		channelKind === SkeletonChannelProjectionKind.GROUP_DM
	);
}

function parseChannelProjections(
	value: unknown,
	now: number,
): OptionalParseResult<ReadonlyArray<TimedSkeletonChannelProjection>> {
	if (value === undefined) {
		return ABSENT_PARSE_RESULT;
	}
	if (!Array.isArray(value) || value.length > MAX_CHANNEL_PROJECTIONS) {
		return INVALID_PARSE_RESULT;
	}
	const projections: Array<TimedSkeletonChannelProjection> = [];
	const seenFingerprints = new Set<string>();
	for (const rawProjection of value) {
		if (
			!isRecord(rawProjection) ||
			!hasOnlyKeys(rawProjection, CHANNEL_PROJECTION_KEYS) ||
			!isValidTimestamp(rawProjection.capturedAt, now) ||
			typeof rawProjection.fingerprint !== 'string' ||
			!FINGERPRINT_PATTERN.test(rawProjection.fingerprint) ||
			!isChannelProjectionKind(rawProjection.channelKind) ||
			typeof rawProjection.showTopic !== 'boolean' ||
			typeof rawProjection.memberListVisible !== 'boolean' ||
			typeof rawProjection.searchPanelOpen !== 'boolean' ||
			seenFingerprints.has(rawProjection.fingerprint)
		) {
			staleRecordNeedsRewrite = true;
			continue;
		}
		const nameWidthPx = parseBoundedInteger(rawProjection.nameWidthPx, 0, MAX_MEASURED_WIDTH_PX);
		const topicWidthPx = parseBoundedInteger(rawProjection.topicWidthPx, 0, MAX_MEASURED_WIDTH_PX);
		const desktopLeadingActionCount = parseBoundedInteger(
			rawProjection.desktopLeadingActionCount,
			0,
			MAX_HEADER_ACTION_COUNT,
		);
		const mobileActionCount = parseBoundedInteger(rawProjection.mobileActionCount, 0, MAX_HEADER_ACTION_COUNT);
		if (
			nameWidthPx == null ||
			topicWidthPx == null ||
			desktopLeadingActionCount == null ||
			mobileActionCount == null ||
			(rawProjection.showTopic && !canChannelProjectionShowTopic(rawProjection.channelKind)) ||
			(!rawProjection.showTopic && topicWidthPx !== 0) ||
			(rawProjection.memberListVisible && !canChannelProjectionShowMemberList(rawProjection.channelKind))
		) {
			staleRecordNeedsRewrite = true;
			continue;
		}
		seenFingerprints.add(rawProjection.fingerprint);
		if (!isFreshTimestamp(rawProjection.capturedAt, now)) {
			continue;
		}
		projections.push(
			Object.freeze({
				capturedAt: rawProjection.capturedAt,
				fingerprint: rawProjection.fingerprint,
				channelKind: rawProjection.channelKind,
				showTopic: rawProjection.showTopic,
				nameWidthPx,
				topicWidthPx,
				desktopLeadingActionCount,
				mobileActionCount,
				memberListVisible: rawProjection.memberListVisible,
				searchPanelOpen: rawProjection.searchPanelOpen,
			}),
		);
	}
	return projections.length > 0 ? validParseResult(Object.freeze(projections)) : ABSENT_PARSE_RESULT;
}

function isGuildBannerPlacement(value: unknown): value is SkeletonGuildBannerPlacement {
	return typeof value === 'string' && GUILD_BANNER_PLACEMENTS.has(value);
}

function parseGuildPresentations(
	value: unknown,
	now: number,
): OptionalParseResult<ReadonlyArray<TimedSkeletonGuildPresentation>> {
	if (value === undefined) {
		return ABSENT_PARSE_RESULT;
	}
	if (!Array.isArray(value) || value.length > MAX_GUILD_PRESENTATIONS) {
		return INVALID_PARSE_RESULT;
	}
	const presentations: Array<TimedSkeletonGuildPresentation> = [];
	const seenFingerprints = new Set<string>();
	for (const rawPresentation of value) {
		if (
			!isRecord(rawPresentation) ||
			!hasOnlyKeys(rawPresentation, GUILD_PRESENTATION_KEYS) ||
			!isValidTimestamp(rawPresentation.capturedAt, now) ||
			typeof rawPresentation.fingerprint !== 'string' ||
			!FINGERPRINT_PATTERN.test(rawPresentation.fingerprint) ||
			typeof rawPresentation.badgeVisible !== 'boolean' ||
			!isGuildBannerPlacement(rawPresentation.bannerPlacement) ||
			seenFingerprints.has(rawPresentation.fingerprint)
		) {
			staleRecordNeedsRewrite = true;
			continue;
		}
		const headerNameWidthPx = parseBoundedInteger(rawPresentation.headerNameWidthPx, 0, MAX_MEASURED_WIDTH_PX);
		const bannerAspectRatio = parseBoundedNumber(
			rawPresentation.bannerAspectRatio,
			MIN_BANNER_ASPECT_RATIO,
			MAX_BANNER_ASPECT_RATIO,
		);
		if (headerNameWidthPx == null || bannerAspectRatio == null) {
			staleRecordNeedsRewrite = true;
			continue;
		}
		seenFingerprints.add(rawPresentation.fingerprint);
		if (!isFreshTimestamp(rawPresentation.capturedAt, now)) {
			continue;
		}
		presentations.push(
			Object.freeze({
				capturedAt: rawPresentation.capturedAt,
				fingerprint: rawPresentation.fingerprint,
				headerNameWidthPx,
				badgeVisible: rawPresentation.badgeVisible,
				bannerPlacement: rawPresentation.bannerPlacement,
				bannerAspectRatio,
			}),
		);
	}
	return presentations.length > 0 ? validParseResult(Object.freeze(presentations)) : ABSENT_PARSE_RESULT;
}

function parseGuildChannelGroups(value: unknown): ReadonlyArray<RememberedSkeletonGuildChannelGroup> | null {
	if (!Array.isArray(value) || value.length > MAX_GUILD_CHANNEL_GROUPS) {
		return null;
	}
	const groups: Array<RememberedSkeletonGuildChannelGroup> = [];
	let totalChannelRows = 0;
	for (const rawGroup of value) {
		if (
			!isRecord(rawGroup) ||
			!hasOnlyKeys(rawGroup, GUILD_CHANNEL_GROUP_KEYS) ||
			typeof rawGroup.categoryHeaderVisible !== 'boolean' ||
			typeof rawGroup.collapsed !== 'boolean' ||
			!Array.isArray(rawGroup.channels)
		) {
			return null;
		}
		const categoryNameWidthPx = parseBoundedInteger(rawGroup.categoryNameWidthPx, 0, MAX_MEASURED_WIDTH_PX);
		if (categoryNameWidthPx == null) {
			return null;
		}
		if (!rawGroup.categoryHeaderVisible && (rawGroup.collapsed || categoryNameWidthPx !== 0)) {
			return null;
		}
		totalChannelRows += rawGroup.channels.length;
		if (totalChannelRows > MAX_GUILD_CHANNEL_ROWS) {
			return null;
		}
		const channels: Array<RememberedSkeletonGuildChannelRow> = [];
		for (const rawChannel of rawGroup.channels) {
			if (
				!isRecord(rawChannel) ||
				!hasOnlyKeys(rawChannel, GUILD_CHANNEL_ROW_KEYS) ||
				typeof rawChannel.voice !== 'boolean'
			) {
				return null;
			}
			const nameWidthPx = parseBoundedInteger(rawChannel.nameWidthPx, 0, MAX_MEASURED_WIDTH_PX);
			if (nameWidthPx == null) {
				return null;
			}
			channels.push(Object.freeze({voice: rawChannel.voice, nameWidthPx}));
		}
		groups.push(
			Object.freeze({
				categoryHeaderVisible: rawGroup.categoryHeaderVisible,
				collapsed: rawGroup.collapsed,
				categoryNameWidthPx,
				channels: Object.freeze(channels),
			}),
		);
	}
	return Object.freeze(groups);
}

function parseGuildChannelLists(
	value: unknown,
	now: number,
): OptionalParseResult<ReadonlyArray<TimedSkeletonGuildChannelList>> {
	if (value === undefined) {
		return ABSENT_PARSE_RESULT;
	}
	if (!Array.isArray(value) || value.length > MAX_GUILD_CHANNEL_LISTS) {
		return INVALID_PARSE_RESULT;
	}
	const lists: Array<TimedSkeletonGuildChannelList> = [];
	const seenFingerprints = new Set<string>();
	for (const rawList of value) {
		if (
			!isRecord(rawList) ||
			!hasOnlyKeys(rawList, GUILD_CHANNEL_LIST_KEYS) ||
			!isValidTimestamp(rawList.capturedAt, now) ||
			typeof rawList.fingerprint !== 'string' ||
			!FINGERPRINT_PATTERN.test(rawList.fingerprint) ||
			typeof rawList.membersRowVisible !== 'boolean' ||
			seenFingerprints.has(rawList.fingerprint)
		) {
			staleRecordNeedsRewrite = true;
			continue;
		}
		const groups = parseGuildChannelGroups(rawList.groups);
		if (groups == null) {
			staleRecordNeedsRewrite = true;
			continue;
		}
		seenFingerprints.add(rawList.fingerprint);
		if (!isFreshTimestamp(rawList.capturedAt, now)) {
			continue;
		}
		lists.push(
			Object.freeze({
				capturedAt: rawList.capturedAt,
				fingerprint: rawList.fingerprint,
				membersRowVisible: rawList.membersRowVisible,
				groups,
			}),
		);
	}
	return lists.length > 0 ? validParseResult(Object.freeze(lists)) : ABSENT_PARSE_RESULT;
}

const EMPTY_STATE: SkeletonLayoutState = Object.freeze({});

function readStoredRecord(): unknown {
	try {
		return AppStorage.getJSON<unknown>(STORAGE_KEY);
	} catch {
		return null;
	}
}

function removeStoredRecord(): void {
	try {
		AppStorage.removeItem(STORAGE_KEY);
	} catch {}
}

function upgradeLegacyGuildRailItems(items: unknown): unknown {
	if (!Array.isArray(items)) {
		return items;
	}
	return items.map((item) => {
		if (!isRecord(item) || item.indicator !== undefined) {
			return item;
		}
		return {...item, indicator: SkeletonGuildRailItemIndicator.NONE};
	});
}

function upgradeLegacyGuildRailUnreadFlags(guildRail: Record<string, unknown>): unknown {
	if (guildRail.inlineDmUnreadFlags !== undefined) {
		return guildRail.inlineDmUnreadFlags;
	}
	let rowCount = 0;
	if (typeof guildRail.inlineDmRowCount === 'number') {
		rowCount = guildRail.inlineDmRowCount;
	}
	const flagCount = Math.min(MAX_GUILD_RAIL_INLINE_DM_ROWS, Math.max(0, rowCount));
	return Array.from({length: flagCount}, () => false);
}

function upgradeLegacyGuildRail(guildRail: Record<string, unknown>): Record<string, unknown> {
	return {
		capturedAt: guildRail.capturedAt,
		inlineDmRowCount: guildRail.inlineDmRowCount,
		inlineDmUnreadFlags: upgradeLegacyGuildRailUnreadFlags(guildRail),
		selectedInlineDmRowIndex: guildRail.selectedInlineDmRowIndex ?? SKELETON_NO_SELECTED_RAIL_ITEM_INDEX,
		outageVisible: guildRail.outageVisible,
		fluxerVisible: guildRail.fluxerVisible,
		favoritesVisible: guildRail.favoritesVisible,
		discoveryVisible: guildRail.discoveryVisible,
		addGuildVisible: guildRail.addGuildVisible,
		downloadVisible: guildRail.downloadVisible,
		helpVisible: guildRail.helpVisible,
		selectedItemIndex: guildRail.selectedItemIndex ?? SKELETON_NO_SELECTED_RAIL_ITEM_INDEX,
		organizedItems: upgradeLegacyGuildRailItems(guildRail.organizedItems),
		scrollTopPx: guildRail.scrollTopPx ?? 0,
	};
}

function upgradeLegacyDMSidebar(dmSidebar: Record<string, unknown>): Record<string, unknown> {
	let channelSubtextFlags = dmSidebar.channelSubtextFlags;
	if (channelSubtextFlags === undefined) {
		const rowCount = typeof dmSidebar.channelRowCount === 'number' ? dmSidebar.channelRowCount : 0;
		const flagCount = Math.min(MAX_DM_SIDEBAR_ROWS, Math.max(0, rowCount));
		channelSubtextFlags = Array.from({length: flagCount}, () => false);
	}
	return {
		capturedAt: dmSidebar.capturedAt,
		isMobile: dmSidebar.isMobile,
		friendsVisible: dmSidebar.friendsVisible,
		personalNotesVisible: dmSidebar.personalNotesVisible,
		premiumVisible: dmSidebar.premiumVisible,
		sectionVisible: dmSidebar.sectionVisible,
		channelRowCount: dmSidebar.channelRowCount,
		channelSubtextFlags,
	};
}

function upgradeLegacyChrome(chrome: unknown): unknown {
	if (!isRecord(chrome)) {
		return chrome;
	}
	const upgraded: Record<string, unknown> = {};
	if (isRecord(chrome.guildRail)) {
		upgraded.guildRail = upgradeLegacyGuildRail(chrome.guildRail);
	}
	if (isRecord(chrome.dmSidebar)) {
		upgraded.dmSidebar = upgradeLegacyDMSidebar(chrome.dmSidebar);
	}
	return upgraded;
}

function upgradeLegacyFriends(friends: unknown): unknown {
	if (!isRecord(friends) || friends.rowCount === undefined) {
		return friends;
	}
	const rowCount = typeof friends.rowCount === 'number' ? friends.rowCount : 0;
	const reportedCardCount = typeof friends.activeNowCardCount === 'number' ? friends.activeNowCardCount : 0;
	const activeNowCardCount = Math.min(MAX_ACTIVE_NOW_CARDS, Math.max(0, reportedCardCount));
	const activeNowVisible = friends.activeNowVisible === true;
	return {
		capturedAt: friends.capturedAt,
		activeTab: SkeletonFriendsTab.ONLINE,
		onlineRowCount: rowCount,
		allRowCount: rowCount,
		pendingRowCount: 0,
		pendingIncomingRowCount: 0,
		tabWidthsPx: Array.from({length: SKELETON_FRIENDS_TAB_COUNT}, () => SKELETON_UNMEASURED_WIDTH_PX),
		pendingBadgeVisible: false,
		activeNowVisible,
		activeNowCards: activeNowVisible
			? Array.from({length: activeNowCardCount}, () => ({participantCount: 0, streaming: false}))
			: [],
	};
}

function upgradeLegacyChannelHeader(channelHeader: Record<string, unknown>): Record<string, unknown> {
	return {
		capturedAt: channelHeader.capturedAt,
		staffToolsVisible: channelHeader.staffToolsVisible,
		updaterVisible: channelHeader.updaterVisible,
		favoritesVisible: channelHeader.favoritesVisible ?? SKELETON_DEFAULT_FAVORITES_VISIBLE,
	};
}

function upgradeLegacyNagbar(nagbar: unknown): unknown {
	if (!isRecord(nagbar) || Array.isArray(nagbar.rows)) {
		return nagbar;
	}
	const storedRowCount = nagbar.visibleRowCount ?? nagbar.rowCount;
	const reportedRowCount = typeof storedRowCount === 'number' ? storedRowCount : 0;
	const rowCount = Math.min(MAX_NAGBAR_ROWS, Math.max(0, reportedRowCount));
	return {
		capturedAt: nagbar.capturedAt,
		rows: Array.from({length: rowCount}, () => ({
			tone: SkeletonNagbarTone.NEUTRAL,
			hasActions: false,
			dismissible: false,
		})),
	};
}

function upgradeLegacyChannelMemberLayouts(layouts: unknown): unknown {
	if (!Array.isArray(layouts)) {
		return layouts;
	}
	return layouts.map((layout) => {
		if (!isRecord(layout)) {
			return layout;
		}
		let memberGroups = layout.memberGroups;
		if (Array.isArray(layout.memberGroupCounts)) {
			memberGroups = layout.memberGroupCounts.map((rowCount) => ({
				rowCount,
				headingWidthPx: SKELETON_UNMEASURED_WIDTH_PX,
			}));
		}
		return {
			capturedAt: layout.capturedAt,
			fingerprint: layout.fingerprint,
			kind: layout.kind,
			memberGroups,
		};
	});
}

function upgradeLegacyChannelProjectionMemberListVisible(projection: Record<string, unknown>): boolean {
	if (projection.memberListVisible !== undefined) {
		return projection.memberListVisible === true;
	}
	return isChannelProjectionKind(projection.channelKind) && canChannelProjectionShowMemberList(projection.channelKind);
}

function upgradeLegacyChannelProjections(projections: unknown): unknown {
	if (!Array.isArray(projections)) {
		return projections;
	}
	return projections.map((projection) => {
		if (!isRecord(projection)) {
			return projection;
		}
		return {
			capturedAt: projection.capturedAt,
			fingerprint: projection.fingerprint,
			channelKind: projection.channelKind,
			showTopic: projection.showTopic,
			nameWidthPx: projection.nameWidthPx ?? SKELETON_UNMEASURED_WIDTH_PX,
			topicWidthPx: projection.topicWidthPx ?? SKELETON_UNMEASURED_WIDTH_PX,
			desktopLeadingActionCount: projection.desktopLeadingActionCount ?? 0,
			mobileActionCount: projection.mobileActionCount ?? 0,
			memberListVisible: upgradeLegacyChannelProjectionMemberListVisible(projection),
			searchPanelOpen: projection.searchPanelOpen === true,
		};
	});
}

function upgradeStoredRecord(raw: Record<string, unknown>, storedVersion: number): Record<string, unknown> {
	if (storedVersion === STORAGE_VERSION) {
		return raw;
	}
	const upgraded: Record<string, unknown> = {...raw};
	upgraded.chrome = upgradeLegacyChrome(raw.chrome);
	upgraded.friends = upgradeLegacyFriends(raw.friends);
	if (isRecord(raw.channelHeader)) {
		upgraded.channelHeader = upgradeLegacyChannelHeader(raw.channelHeader);
	}
	upgraded.nagbar = upgradeLegacyNagbar(raw.nagbar);
	upgraded.channelMemberLayouts = upgradeLegacyChannelMemberLayouts(raw.channelMemberLayouts);
	upgraded.channelProjections = upgradeLegacyChannelProjections(raw.channelProjections);
	upgraded.version = STORAGE_VERSION;
	return upgraded;
}

function readInitialState(activeAccountFingerprint: string | null): SkeletonLayoutState {
	if (activeAccountFingerprint == null) {
		return EMPTY_STATE;
	}
	const raw = readStoredRecord();
	if (raw == null) {
		return EMPTY_STATE;
	}
	if (!isRecord(raw)) {
		removeStoredRecord();
		return EMPTY_STATE;
	}
	const now = Date.now();
	const storedVersion = parseBoundedInteger(raw.version, MIN_READABLE_STORAGE_VERSION, STORAGE_VERSION);
	if (
		storedVersion == null ||
		typeof raw.accountFingerprint !== 'string' ||
		!FINGERPRINT_PATTERN.test(raw.accountFingerprint) ||
		!isFreshTimestamp(raw.updatedAt, now)
	) {
		removeStoredRecord();
		return EMPTY_STATE;
	}
	if (raw.accountFingerprint !== activeAccountFingerprint) {
		return EMPTY_STATE;
	}
	if (!hasOnlyKeys(raw, ROOT_KEYS)) {
		staleRecordNeedsRewrite = true;
	}
	const record = upgradeStoredRecord(raw, storedVersion);
	const chrome = parseChromeLayout(record.chrome, now);
	const friends = parseFriendsLayout(record.friends, now);
	const channelHeader = parseChannelHeaderLayout(record.channelHeader, now);
	const nagbar = parseNagbarLayout(record.nagbar, now);
	const composer = parseComposerLayout(record.composer, now);
	const messagePresentation = parseMessagePresentation(record.messagePresentation, now);
	const voice = parseVoicePresence(record.voice, now);
	const discovery = parseDiscoveryLayout(record.discovery, now);
	const simplePages = parseSimplePageLayouts(record.simplePages, now);
	const channelMemberLayouts = parseChannelMemberLayouts(record.channelMemberLayouts, now);
	const channelProjections = parseChannelProjections(record.channelProjections, now);
	const guildPresentations = parseGuildPresentations(record.guildPresentations, now);
	const guildChannelLists = parseGuildChannelLists(record.guildChannelLists, now);
	const results = [
		chrome,
		friends,
		channelHeader,
		nagbar,
		composer,
		messagePresentation,
		voice,
		discovery,
		simplePages,
		channelMemberLayouts,
		channelProjections,
		guildPresentations,
		guildChannelLists,
	];
	if (results.every((result) => result.status !== 'valid')) {
		removeStoredRecord();
		return EMPTY_STATE;
	}
	if (storedVersion !== STORAGE_VERSION || results.some((result) => result.status === 'invalid')) {
		staleRecordNeedsRewrite = true;
	}
	return Object.freeze({
		chrome: chrome.status === 'valid' ? chrome.value : undefined,
		friends: friends.status === 'valid' ? friends.value : undefined,
		channelHeader: channelHeader.status === 'valid' ? channelHeader.value : undefined,
		nagbar: nagbar.status === 'valid' ? nagbar.value : undefined,
		composer: composer.status === 'valid' ? composer.value : undefined,
		messagePresentation: messagePresentation.status === 'valid' ? messagePresentation.value : undefined,
		voice: voice.status === 'valid' ? voice.value : undefined,
		discovery: discovery.status === 'valid' ? discovery.value : undefined,
		simplePages: simplePages.status === 'valid' ? simplePages.value : undefined,
		channelMemberLayouts: channelMemberLayouts.status === 'valid' ? channelMemberLayouts.value : undefined,
		channelProjections: channelProjections.status === 'valid' ? channelProjections.value : undefined,
		guildPresentations: guildPresentations.status === 'valid' ? guildPresentations.value : undefined,
		guildChannelLists: guildChannelLists.status === 'valid' ? guildChannelLists.value : undefined,
	});
}

function areNumberArraysEqual(left: ReadonlyArray<number>, right: ReadonlyArray<number>): boolean {
	if (left.length !== right.length) {
		return false;
	}
	for (let index = 0; index < left.length; index += 1) {
		if (left[index] !== right[index]) {
			return false;
		}
	}
	return true;
}

function areBooleanArraysEqual(left: ReadonlyArray<boolean>, right: ReadonlyArray<boolean>): boolean {
	if (left.length !== right.length) {
		return false;
	}
	for (let index = 0; index < left.length; index += 1) {
		if (left[index] !== right[index]) {
			return false;
		}
	}
	return true;
}

function areMemberGroupsEqual(
	left: ReadonlyArray<RememberedSkeletonMemberGroup>,
	right: ReadonlyArray<RememberedSkeletonMemberGroup>,
): boolean {
	if (left.length !== right.length) {
		return false;
	}
	for (let index = 0; index < left.length; index += 1) {
		if (
			left[index].rowCount !== right[index].rowCount ||
			left[index].headingWidthPx !== right[index].headingWidthPx ||
			!areBooleanArraysEqual(left[index].subtextFlags, right[index].subtextFlags)
		) {
			return false;
		}
	}
	return true;
}

function areActiveNowCardsEqual(
	left: ReadonlyArray<RememberedSkeletonActiveNowCard>,
	right: ReadonlyArray<RememberedSkeletonActiveNowCard>,
): boolean {
	if (left.length !== right.length) {
		return false;
	}
	for (let index = 0; index < left.length; index += 1) {
		if (
			left[index].participantCount !== right[index].participantCount ||
			left[index].streaming !== right[index].streaming
		) {
			return false;
		}
	}
	return true;
}

function areNagbarRowsEqual(
	left: ReadonlyArray<RememberedSkeletonNagbarRow>,
	right: ReadonlyArray<RememberedSkeletonNagbarRow>,
): boolean {
	if (left.length !== right.length) {
		return false;
	}
	for (let index = 0; index < left.length; index += 1) {
		if (
			left[index].tone !== right[index].tone ||
			left[index].hasActions !== right[index].hasActions ||
			left[index].dismissible !== right[index].dismissible
		) {
			return false;
		}
	}
	return true;
}

function areGuildChannelGroupsEqual(
	left: ReadonlyArray<RememberedSkeletonGuildChannelGroup>,
	right: ReadonlyArray<RememberedSkeletonGuildChannelGroup>,
): boolean {
	if (left.length !== right.length) {
		return false;
	}
	for (let index = 0; index < left.length; index += 1) {
		const leftGroup = left[index];
		const rightGroup = right[index];
		if (
			leftGroup.categoryHeaderVisible !== rightGroup.categoryHeaderVisible ||
			leftGroup.collapsed !== rightGroup.collapsed ||
			leftGroup.categoryNameWidthPx !== rightGroup.categoryNameWidthPx ||
			leftGroup.channels.length !== rightGroup.channels.length
		) {
			return false;
		}
		for (let channelIndex = 0; channelIndex < leftGroup.channels.length; channelIndex += 1) {
			if (
				leftGroup.channels[channelIndex].voice !== rightGroup.channels[channelIndex].voice ||
				leftGroup.channels[channelIndex].nameWidthPx !== rightGroup.channels[channelIndex].nameWidthPx
			) {
				return false;
			}
		}
	}
	return true;
}

function areGuildRailChildIndicatorsEqual(
	left: ReadonlyArray<SkeletonGuildRailItemIndicator>,
	right: ReadonlyArray<SkeletonGuildRailItemIndicator>,
): boolean {
	if (left.length !== right.length) {
		return false;
	}
	for (let index = 0; index < left.length; index += 1) {
		if (left[index] !== right[index]) {
			return false;
		}
	}
	return true;
}

function areGuildRailItemsEqual(
	left: ReadonlyArray<RememberedSkeletonGuildRailItem>,
	right: ReadonlyArray<RememberedSkeletonGuildRailItem>,
): boolean {
	if (left.length !== right.length) {
		return false;
	}
	for (let index = 0; index < left.length; index += 1) {
		const leftItem = left[index];
		const rightItem = right[index];
		if (leftItem.kind !== rightItem.kind || leftItem.indicator !== rightItem.indicator) {
			return false;
		}
		if (
			leftItem.kind === SkeletonGuildRailItemKind.EXPANDED_FOLDER &&
			(rightItem.kind !== SkeletonGuildRailItemKind.EXPANDED_FOLDER ||
				leftItem.childCount !== rightItem.childCount ||
				leftItem.selectedChildIndex !== rightItem.selectedChildIndex ||
				!areGuildRailChildIndicatorsEqual(leftItem.childIndicators, rightItem.childIndicators))
		) {
			return false;
		}
		if (
			leftItem.kind === SkeletonGuildRailItemKind.COLLAPSED_FOLDER &&
			(rightItem.kind !== SkeletonGuildRailItemKind.COLLAPSED_FOLDER ||
				leftItem.childCount !== rightItem.childCount ||
				leftItem.showIconWhenCollapsed !== rightItem.showIconWhenCollapsed)
		) {
			return false;
		}
	}
	return true;
}

let writeTimer: number | null = null;
let writePending = false;
let runningPreFlush = false;
const preFlushCallbacks = new Set<() => void>();
let staleRecordNeedsRewrite = false;
let activeAccountFingerprint = resolveActiveAccountFingerprint();
let state = loadStateForActiveAccount();
let captureEnabled = false;
let pendingChrome: PendingChromeLayouts = {};
let pendingGuildRailScrollTopPx: number | null = null;
let pendingFriends: PendingFriendsLayout = {};
let pendingChannelHeader: RememberedSkeletonChannelHeaderLayout | null = null;
let pendingNagbar: RememberedSkeletonNagbarLayout | null = null;
let pendingComposer: RememberedSkeletonComposerLayout | null = null;
let pendingMessagePresentation: PendingMessagePresentation = {};
let pendingVoice: RememberedSkeletonVoicePresence | null = null;
let pendingDiscovery: PendingDiscoveryLayout = {};
let pendingSimplePages: ReadonlyArray<PendingSkeletonSimplePageLayout> = Object.freeze([]);
let pendingChannelMemberLayouts: ReadonlyArray<PendingSkeletonChannelMemberLayout> = Object.freeze([]);
let pendingChannelProjections: ReadonlyArray<PendingSkeletonChannelProjection> = Object.freeze([]);
let pendingGuildPresentations: ReadonlyArray<PendingSkeletonGuildPresentation> = Object.freeze([]);
let pendingGuildChannelLists: ReadonlyArray<PendingSkeletonGuildChannelList> = Object.freeze([]);
const capturedThisSession = new Set<string>();
const capturedMemberFingerprintsThisSession = new Set<string>();
const capturedChannelFingerprintsThisSession = new Set<string>();
const capturedGuildFingerprintsThisSession = new Set<string>();
const capturedGuildChannelListFingerprintsThisSession = new Set<string>();

function clearPendingCaptureState(): void {
	pendingChrome = {};
	pendingGuildRailScrollTopPx = null;
	pendingFriends = {};
	pendingChannelHeader = null;
	pendingNagbar = null;
	pendingComposer = null;
	pendingMessagePresentation = {};
	pendingVoice = null;
	pendingDiscovery = {};
	pendingSimplePages = Object.freeze([]);
	pendingChannelMemberLayouts = Object.freeze([]);
	pendingChannelProjections = Object.freeze([]);
	pendingGuildPresentations = Object.freeze([]);
	pendingGuildChannelLists = Object.freeze([]);
	capturedThisSession.clear();
	capturedMemberFingerprintsThisSession.clear();
	capturedChannelFingerprintsThisSession.clear();
	capturedGuildFingerprintsThisSession.clear();
	capturedGuildChannelListFingerprintsThisSession.clear();
}

function loadStateForActiveAccount(): SkeletonLayoutState {
	staleRecordNeedsRewrite = false;
	const nextState = readInitialState(activeAccountFingerprint);
	if (staleRecordNeedsRewrite) {
		staleRecordNeedsRewrite = false;
		scheduleWrite();
	}
	return nextState;
}

function synchronizeActiveAccountState(): void {
	const nextAccountFingerprint = resolveActiveAccountFingerprint();
	if (nextAccountFingerprint === activeAccountFingerprint) {
		return;
	}
	flushSkeletonLayoutMemoryWrite();
	activeAccountFingerprint = nextAccountFingerprint;
	state = EMPTY_STATE;
	clearPendingCaptureState();
	state = loadStateForActiveAccount();
}

AppStorage.subscribe(
	() => {
		synchronizeActiveAccountState();
	},
	{key: AuthSessionStorageKey.UserId},
);

function createStoredRecord(source: SkeletonLayoutState): Record<string, unknown> {
	const record: Record<string, unknown> = {
		version: STORAGE_VERSION,
		updatedAt: Date.now(),
		accountFingerprint: activeAccountFingerprint,
	};
	for (const key of SKELETON_LAYOUT_STATE_KEYS) {
		record[key] = source[key];
	}
	return record;
}

function writeStoredRecord(source: SkeletonLayoutState): boolean {
	try {
		AppStorage.setJSON(STORAGE_KEY, createStoredRecord(source));
		return true;
	} catch {
		return false;
	}
}

function halvePerEntityRecords(source: SkeletonLayoutState): SkeletonLayoutState | null {
	const halve = <T>(entries: ReadonlyArray<T> | undefined): ReadonlyArray<T> | undefined => {
		if (entries == null || entries.length <= 1) {
			return entries;
		}
		return Object.freeze(entries.slice(-Math.max(1, Math.floor(entries.length / 2))));
	};
	const next = Object.freeze({
		...source,
		channelMemberLayouts: halve(source.channelMemberLayouts),
		channelProjections: halve(source.channelProjections),
		guildPresentations: halve(source.guildPresentations),
		guildChannelLists: halve(source.guildChannelLists),
	});
	if (
		next.channelMemberLayouts?.length === source.channelMemberLayouts?.length &&
		next.channelProjections?.length === source.channelProjections?.length &&
		next.guildPresentations?.length === source.guildPresentations?.length &&
		next.guildChannelLists?.length === source.guildChannelLists?.length
	) {
		return null;
	}
	return next;
}

function persistState(): void {
	writeTimer = null;
	if (activeAccountFingerprint == null) {
		writePending = false;
		return;
	}
	if (writeStoredRecord(state)) {
		writePending = false;
		return;
	}
	let candidate = halvePerEntityRecords(state);
	while (candidate != null) {
		if (writeStoredRecord(candidate)) {
			state = candidate;
			writePending = false;
			return;
		}
		candidate = halvePerEntityRecords(candidate);
	}
}

function scheduleWrite(): void {
	if (activeAccountFingerprint == null || typeof window === 'undefined') {
		return;
	}
	writePending = true;
	if (writeTimer != null) {
		return;
	}
	writeTimer = window.setTimeout(persistState, WRITE_COALESCE_MS);
}

function rememberFingerprintInSession(retention: Set<string>, fingerprint: string, limit: number): void {
	retention.delete(fingerprint);
	retention.add(fingerprint);
	while (retention.size > limit) {
		const oldestFingerprint = retention.values().next().value;
		if (oldestFingerprint == null) {
			break;
		}
		retention.delete(oldestFingerprint);
	}
}

function commitDMSidebarLayout(layout: RememberedSkeletonDMSidebarLayout): void {
	const previous = state.chrome?.dmSidebar;
	const sessionKey = 'chrome:dm-sidebar';
	if (
		capturedThisSession.has(sessionKey) &&
		previous?.isMobile === layout.isMobile &&
		previous.friendsVisible === layout.friendsVisible &&
		previous.personalNotesVisible === layout.personalNotesVisible &&
		previous.premiumVisible === layout.premiumVisible &&
		previous.sectionVisible === layout.sectionVisible &&
		previous.channelRowCount === layout.channelRowCount &&
		areBooleanArraysEqual(previous.channelSubtextFlags, layout.channelSubtextFlags)
	) {
		return;
	}
	capturedThisSession.add(sessionKey);
	const dmSidebar = Object.freeze({...layout, capturedAt: Date.now()});
	state = Object.freeze({...state, chrome: Object.freeze({...state.chrome, dmSidebar})});
	scheduleWrite();
}

function commitGuildRailLayout(layout: RememberedSkeletonGuildRailLayout): void {
	const previous = state.chrome?.guildRail;
	const sessionKey = 'chrome:guild-rail';
	if (
		capturedThisSession.has(sessionKey) &&
		previous?.inlineDmRowCount === layout.inlineDmRowCount &&
		areBooleanArraysEqual(previous.inlineDmUnreadFlags, layout.inlineDmUnreadFlags) &&
		previous.selectedInlineDmRowIndex === layout.selectedInlineDmRowIndex &&
		previous.outageVisible === layout.outageVisible &&
		previous.fluxerVisible === layout.fluxerVisible &&
		previous.favoritesVisible === layout.favoritesVisible &&
		previous.discoveryVisible === layout.discoveryVisible &&
		previous.addGuildVisible === layout.addGuildVisible &&
		previous.downloadVisible === layout.downloadVisible &&
		previous.helpVisible === layout.helpVisible &&
		previous.selectedItemIndex === layout.selectedItemIndex &&
		previous.scrollTopPx === layout.scrollTopPx &&
		areGuildRailItemsEqual(previous.organizedItems, layout.organizedItems)
	) {
		return;
	}
	capturedThisSession.add(sessionKey);
	const guildRail = Object.freeze({
		...layout,
		capturedAt: Date.now(),
		organizedItems: Object.freeze([...layout.organizedItems]),
	});
	state = Object.freeze({...state, chrome: Object.freeze({...state.chrome, guildRail})});
	scheduleWrite();
}

function resolvePendingFriendsLayout(): RememberedSkeletonFriendsLayout {
	const base = state.friends ?? SKELETON_DEFAULT_FRIENDS_LAYOUT;
	const activeNowVisible = pendingFriends.activeNowVisible ?? base.activeNowVisible;
	let activeNowCards = EMPTY_ACTIVE_NOW_CARDS;
	if (activeNowVisible) {
		activeNowCards = pendingFriends.activeNowCards ?? base.activeNowCards;
	}
	return Object.freeze({
		activeTab: pendingFriends.activeTab ?? base.activeTab,
		onlineRowCount: pendingFriends.onlineRowCount ?? base.onlineRowCount,
		allRowCount: pendingFriends.allRowCount ?? base.allRowCount,
		pendingRowCount: pendingFriends.pendingRowCount ?? base.pendingRowCount,
		pendingIncomingRowCount: pendingFriends.pendingIncomingRowCount ?? base.pendingIncomingRowCount,
		tabWidthsPx: pendingFriends.tabWidthsPx ?? base.tabWidthsPx,
		pendingBadgeVisible: pendingFriends.pendingBadgeVisible ?? base.pendingBadgeVisible,
		activeNowVisible,
		activeNowCards,
	});
}

function commitFriendsLayout(): void {
	const layout = resolvePendingFriendsLayout();
	const previous = state.friends;
	const sessionKey = 'friends';
	if (
		capturedThisSession.has(sessionKey) &&
		previous?.activeTab === layout.activeTab &&
		previous.onlineRowCount === layout.onlineRowCount &&
		previous.allRowCount === layout.allRowCount &&
		previous.pendingRowCount === layout.pendingRowCount &&
		previous.pendingIncomingRowCount === layout.pendingIncomingRowCount &&
		previous.pendingBadgeVisible === layout.pendingBadgeVisible &&
		previous.activeNowVisible === layout.activeNowVisible &&
		areNumberArraysEqual(previous.tabWidthsPx, layout.tabWidthsPx) &&
		areActiveNowCardsEqual(previous.activeNowCards, layout.activeNowCards)
	) {
		return;
	}
	capturedThisSession.add(sessionKey);
	state = Object.freeze({...state, friends: Object.freeze({...layout, capturedAt: Date.now()})});
	scheduleWrite();
}

function commitChannelHeaderLayout(layout: RememberedSkeletonChannelHeaderLayout): void {
	const previous = state.channelHeader;
	const sessionKey = 'channel-header';
	if (
		capturedThisSession.has(sessionKey) &&
		previous?.staffToolsVisible === layout.staffToolsVisible &&
		previous.updaterVisible === layout.updaterVisible &&
		previous.favoritesVisible === layout.favoritesVisible
	) {
		return;
	}
	capturedThisSession.add(sessionKey);
	state = Object.freeze({...state, channelHeader: Object.freeze({...layout, capturedAt: Date.now()})});
	scheduleWrite();
}

function commitNagbarLayout(layout: RememberedSkeletonNagbarLayout): void {
	const previous = state.nagbar;
	const sessionKey = 'nagbar';
	if (capturedThisSession.has(sessionKey) && previous != null && areNagbarRowsEqual(previous.rows, layout.rows)) {
		return;
	}
	capturedThisSession.add(sessionKey);
	state = Object.freeze({
		...state,
		nagbar: Object.freeze({rows: Object.freeze([...layout.rows]), capturedAt: Date.now()}),
	});
	scheduleWrite();
}

function commitComposerLayout(layout: RememberedSkeletonComposerLayout): void {
	const previous = state.composer;
	const sessionKey = 'composer';
	if (
		capturedThisSession.has(sessionKey) &&
		previous?.desktopActionCount === layout.desktopActionCount &&
		previous.mobileActionCount === layout.mobileActionCount &&
		previous.sendDividerVisible === layout.sendDividerVisible
	) {
		return;
	}
	capturedThisSession.add(sessionKey);
	state = Object.freeze({...state, composer: Object.freeze({...layout, capturedAt: Date.now()})});
	scheduleWrite();
}

function resolvePendingMessagePresentation(): RememberedSkeletonMessagePresentation {
	const base = state.messagePresentation ?? resolveDefaultSkeletonMessagePresentation();
	return Object.freeze({
		compact: pendingMessagePresentation.compact ?? base.compact,
		messageGutterPx: pendingMessagePresentation.messageGutterPx ?? base.messageGutterPx,
		fontSizePx: pendingMessagePresentation.fontSizePx ?? base.fontSizePx,
		groupSpacingPx: pendingMessagePresentation.groupSpacingPx ?? base.groupSpacingPx,
		compactAvatarsVisible: pendingMessagePresentation.compactAvatarsVisible ?? base.compactAvatarsVisible,
		compactTimestampWidthPx: pendingMessagePresentation.compactTimestampWidthPx ?? base.compactTimestampWidthPx,
		viewportHeightPx: pendingMessagePresentation.viewportHeightPx ?? base.viewportHeightPx,
	});
}

function commitMessagePresentation(): void {
	const presentation = resolvePendingMessagePresentation();
	const previous = state.messagePresentation;
	const sessionKey = 'message-presentation';
	if (
		capturedThisSession.has(sessionKey) &&
		previous?.compact === presentation.compact &&
		previous.messageGutterPx === presentation.messageGutterPx &&
		previous.fontSizePx === presentation.fontSizePx &&
		previous.groupSpacingPx === presentation.groupSpacingPx &&
		previous.compactAvatarsVisible === presentation.compactAvatarsVisible &&
		previous.compactTimestampWidthPx === presentation.compactTimestampWidthPx &&
		previous.viewportHeightPx === presentation.viewportHeightPx
	) {
		return;
	}
	capturedThisSession.add(sessionKey);
	state = Object.freeze({...state, messagePresentation: Object.freeze({...presentation, capturedAt: Date.now()})});
	scheduleWrite();
}

function commitVoicePresence(presence: RememberedSkeletonVoicePresence): void {
	const previous = state.voice;
	const sessionKey = 'voice';
	if (
		capturedThisSession.has(sessionKey) &&
		previous?.connected === presence.connected &&
		previous.panelHeightPx === presence.panelHeightPx
	) {
		return;
	}
	capturedThisSession.add(sessionKey);
	state = Object.freeze({...state, voice: Object.freeze({...presence, capturedAt: Date.now()})});
	scheduleWrite();
}

function resolvePendingDiscoveryLayout(): RememberedSkeletonDiscoveryLayout {
	const base = state.discovery ?? resolveDefaultSkeletonDiscoveryLayout();
	return Object.freeze({
		columnCount: pendingDiscovery.columnCount ?? base.columnCount,
		visibleRowCount: pendingDiscovery.visibleRowCount ?? base.visibleRowCount,
		categoryTabWidthsPx: pendingDiscovery.categoryTabWidthsPx ?? base.categoryTabWidthsPx,
	});
}

function commitDiscoveryLayout(): void {
	const layout = resolvePendingDiscoveryLayout();
	const previous = state.discovery;
	const sessionKey = 'discovery';
	if (
		capturedThisSession.has(sessionKey) &&
		previous?.columnCount === layout.columnCount &&
		previous.visibleRowCount === layout.visibleRowCount &&
		areNumberArraysEqual(previous.categoryTabWidthsPx, layout.categoryTabWidthsPx)
	) {
		return;
	}
	capturedThisSession.add(sessionKey);
	state = Object.freeze({...state, discovery: Object.freeze({...layout, capturedAt: Date.now()})});
	scheduleWrite();
}

function commitSimplePageLayout(layout: PendingSkeletonSimplePageLayout): void {
	const current = state.simplePages ?? [];
	const previous = current.find((entry) => entry.route === layout.route);
	const sessionKey = `simple-page:${layout.route}`;
	if (
		capturedThisSession.has(sessionKey) &&
		previous?.body === layout.body &&
		previous.rowCount === layout.rowCount &&
		previous.selectable === layout.selectable
	) {
		return;
	}
	capturedThisSession.add(sessionKey);
	const next = current.filter((entry) => entry.route !== layout.route);
	next.push(Object.freeze({...layout, capturedAt: Date.now()}));
	state = Object.freeze({...state, simplePages: Object.freeze(next.slice(-MAX_SIMPLE_PAGE_LAYOUTS))});
	scheduleWrite();
}

function commitChannelMemberLayout(layout: PendingSkeletonChannelMemberLayout): void {
	const current = state.channelMemberLayouts ?? [];
	const previous = current.find(({fingerprint}) => fingerprint === layout.fingerprint);
	if (
		capturedMemberFingerprintsThisSession.has(layout.fingerprint) &&
		current[current.length - 1]?.fingerprint === layout.fingerprint &&
		previous?.kind === layout.kind &&
		areMemberGroupsEqual(previous.memberGroups, layout.memberGroups)
	) {
		rememberFingerprintInSession(capturedMemberFingerprintsThisSession, layout.fingerprint, MAX_CHANNEL_MEMBER_LAYOUTS);
		return;
	}
	rememberFingerprintInSession(capturedMemberFingerprintsThisSession, layout.fingerprint, MAX_CHANNEL_MEMBER_LAYOUTS);
	const nextLayout = Object.freeze({
		...layout,
		capturedAt: Date.now(),
		memberGroups: Object.freeze([...layout.memberGroups]),
	});
	const next = current.filter(({fingerprint}) => fingerprint !== layout.fingerprint);
	next.push(nextLayout);
	state = Object.freeze({
		...state,
		channelMemberLayouts: Object.freeze(next.slice(-MAX_CHANNEL_MEMBER_LAYOUTS)),
	});
	scheduleWrite();
}

function commitChannelProjection(projection: PendingSkeletonChannelProjection): void {
	const current = state.channelProjections ?? [];
	const previous = current.find((entry) => entry.fingerprint === projection.fingerprint);
	if (
		capturedChannelFingerprintsThisSession.has(projection.fingerprint) &&
		current[current.length - 1]?.fingerprint === projection.fingerprint &&
		previous?.channelKind === projection.channelKind &&
		previous.showTopic === projection.showTopic &&
		previous.nameWidthPx === projection.nameWidthPx &&
		previous.topicWidthPx === projection.topicWidthPx &&
		previous.desktopLeadingActionCount === projection.desktopLeadingActionCount &&
		previous.mobileActionCount === projection.mobileActionCount &&
		previous.memberListVisible === projection.memberListVisible &&
		previous.searchPanelOpen === projection.searchPanelOpen
	) {
		rememberFingerprintInSession(
			capturedChannelFingerprintsThisSession,
			projection.fingerprint,
			MAX_CHANNEL_PROJECTIONS,
		);
		return;
	}
	rememberFingerprintInSession(capturedChannelFingerprintsThisSession, projection.fingerprint, MAX_CHANNEL_PROJECTIONS);
	const nextProjection = Object.freeze({...projection, capturedAt: Date.now()});
	const next = current.filter((entry) => entry.fingerprint !== projection.fingerprint);
	next.push(nextProjection);
	state = Object.freeze({...state, channelProjections: Object.freeze(next.slice(-MAX_CHANNEL_PROJECTIONS))});
	scheduleWrite();
}

function commitGuildPresentation(presentation: PendingSkeletonGuildPresentation): void {
	const current = state.guildPresentations ?? [];
	const previous = current.find((entry) => entry.fingerprint === presentation.fingerprint);
	if (
		capturedGuildFingerprintsThisSession.has(presentation.fingerprint) &&
		current[current.length - 1]?.fingerprint === presentation.fingerprint &&
		previous?.headerNameWidthPx === presentation.headerNameWidthPx &&
		previous.badgeVisible === presentation.badgeVisible &&
		previous.bannerPlacement === presentation.bannerPlacement &&
		previous.bannerAspectRatio === presentation.bannerAspectRatio
	) {
		rememberFingerprintInSession(
			capturedGuildFingerprintsThisSession,
			presentation.fingerprint,
			MAX_GUILD_PRESENTATIONS,
		);
		return;
	}
	rememberFingerprintInSession(capturedGuildFingerprintsThisSession, presentation.fingerprint, MAX_GUILD_PRESENTATIONS);
	const next = current.filter((entry) => entry.fingerprint !== presentation.fingerprint);
	next.push(Object.freeze({...presentation, capturedAt: Date.now()}));
	state = Object.freeze({...state, guildPresentations: Object.freeze(next.slice(-MAX_GUILD_PRESENTATIONS))});
	scheduleWrite();
}

function commitGuildChannelList(list: PendingSkeletonGuildChannelList): void {
	const current = state.guildChannelLists ?? [];
	const previous = current.find((entry) => entry.fingerprint === list.fingerprint);
	if (
		capturedGuildChannelListFingerprintsThisSession.has(list.fingerprint) &&
		current[current.length - 1]?.fingerprint === list.fingerprint &&
		previous?.membersRowVisible === list.membersRowVisible &&
		areGuildChannelGroupsEqual(previous.groups, list.groups)
	) {
		rememberFingerprintInSession(
			capturedGuildChannelListFingerprintsThisSession,
			list.fingerprint,
			MAX_GUILD_CHANNEL_LISTS,
		);
		return;
	}
	rememberFingerprintInSession(
		capturedGuildChannelListFingerprintsThisSession,
		list.fingerprint,
		MAX_GUILD_CHANNEL_LISTS,
	);
	const next = current.filter((entry) => entry.fingerprint !== list.fingerprint);
	next.push(Object.freeze({...list, capturedAt: Date.now()}));
	state = Object.freeze({...state, guildChannelLists: Object.freeze(next.slice(-MAX_GUILD_CHANNEL_LISTS))});
	scheduleWrite();
}

function flushPendingReports(): void {
	if (pendingChrome.dmSidebar != null) {
		commitDMSidebarLayout(pendingChrome.dmSidebar);
	}
	if (pendingChrome.guildRail != null) {
		commitGuildRailLayout(pendingChrome.guildRail);
	}
	if (Object.keys(pendingFriends).length > 0) {
		commitFriendsLayout();
	}
	if (pendingChannelHeader != null) {
		commitChannelHeaderLayout(pendingChannelHeader);
	}
	if (pendingNagbar != null) {
		commitNagbarLayout(pendingNagbar);
	}
	if (pendingComposer != null) {
		commitComposerLayout(pendingComposer);
	}
	if (Object.keys(pendingMessagePresentation).length > 0) {
		commitMessagePresentation();
	}
	if (pendingVoice != null) {
		commitVoicePresence(pendingVoice);
	}
	if (Object.keys(pendingDiscovery).length > 0) {
		commitDiscoveryLayout();
	}
	for (const layout of pendingSimplePages) {
		commitSimplePageLayout(layout);
	}
	for (const layout of pendingChannelMemberLayouts) {
		commitChannelMemberLayout(layout);
	}
	for (const projection of pendingChannelProjections) {
		commitChannelProjection(projection);
	}
	for (const presentation of pendingGuildPresentations) {
		commitGuildPresentation(presentation);
	}
	for (const list of pendingGuildChannelLists) {
		commitGuildChannelList(list);
	}
}

export function setSkeletonLayoutCaptureEnabled(enabled: boolean): void {
	synchronizeActiveAccountState();
	if (captureEnabled === enabled) {
		return;
	}
	captureEnabled = enabled;
	if (enabled) {
		flushPendingReports();
		return;
	}
	clearPendingCaptureState();
}

export function registerSkeletonLayoutMemoryPreFlush(callback: () => void): () => void {
	preFlushCallbacks.add(callback);
	return () => {
		preFlushCallbacks.delete(callback);
	};
}

export function flushSkeletonLayoutMemoryWrite(): void {
	if (!runningPreFlush) {
		runningPreFlush = true;
		try {
			for (const callback of preFlushCallbacks) {
				callback();
			}
		} finally {
			runningPreFlush = false;
		}
	}
	if (writeTimer != null && typeof window !== 'undefined') {
		window.clearTimeout(writeTimer);
	}
	writeTimer = null;
	if (!writePending) {
		return;
	}
	persistState();
}

export function reportSkeletonDMSidebarLayout(layout: RememberedSkeletonDMSidebarLayout): void {
	synchronizeActiveAccountState();
	const channelRowCount = clampReportedInteger(layout.channelRowCount, 0, MAX_DM_SIDEBAR_ROWS);
	if (
		channelRowCount == null ||
		typeof layout.isMobile !== 'boolean' ||
		typeof layout.friendsVisible !== 'boolean' ||
		typeof layout.personalNotesVisible !== 'boolean' ||
		typeof layout.premiumVisible !== 'boolean' ||
		typeof layout.sectionVisible !== 'boolean' ||
		!Array.isArray(layout.channelSubtextFlags)
	) {
		return;
	}
	const channelSubtextFlags: Array<boolean> = [];
	for (let index = 0; index < channelRowCount; index += 1) {
		channelSubtextFlags.push(layout.channelSubtextFlags[index] === true);
	}
	const normalizedLayout: RememberedSkeletonDMSidebarLayout = Object.freeze({
		isMobile: layout.isMobile,
		friendsVisible: layout.friendsVisible,
		personalNotesVisible: layout.personalNotesVisible,
		premiumVisible: layout.premiumVisible,
		sectionVisible: layout.sectionVisible,
		channelRowCount,
		channelSubtextFlags: Object.freeze(channelSubtextFlags),
	});
	if (!isValidDMSidebarVisibility(normalizedLayout)) {
		return;
	}
	pendingChrome = {...pendingChrome, dmSidebar: normalizedLayout};
	if (captureEnabled) {
		commitDMSidebarLayout(normalizedLayout);
	}
}

export function reportSkeletonGuildRailLayout(layout: Omit<RememberedSkeletonGuildRailLayout, 'scrollTopPx'>): void {
	synchronizeActiveAccountState();
	const inlineDmRowCount = clampReportedInteger(layout.inlineDmRowCount, 0, MAX_GUILD_RAIL_INLINE_DM_ROWS);
	if (
		inlineDmRowCount == null ||
		typeof layout.outageVisible !== 'boolean' ||
		typeof layout.fluxerVisible !== 'boolean' ||
		typeof layout.favoritesVisible !== 'boolean' ||
		typeof layout.discoveryVisible !== 'boolean' ||
		typeof layout.addGuildVisible !== 'boolean' ||
		typeof layout.downloadVisible !== 'boolean' ||
		typeof layout.helpVisible !== 'boolean' ||
		layout.discoveryVisible !== layout.addGuildVisible ||
		(!layout.fluxerVisible && inlineDmRowCount > 0) ||
		!Array.isArray(layout.organizedItems) ||
		!Array.isArray(layout.inlineDmUnreadFlags)
	) {
		return;
	}
	const organizedItems: Array<RememberedSkeletonGuildRailItem> = [];
	let remainingVisualRows = SKELETON_GUILD_RAIL_ORGANIZED_VISUAL_ROW_LIMIT;
	for (const item of layout.organizedItems) {
		if (remainingVisualRows === 0) {
			break;
		}
		if (!isSkeletonGuildRailItemIndicator(item.indicator)) {
			return;
		}
		const indicator = item.indicator;
		switch (item.kind) {
			case SkeletonGuildRailItemKind.GUILD:
				organizedItems.push(Object.freeze({kind: item.kind, indicator}));
				remainingVisualRows -= 1;
				break;
			case SkeletonGuildRailItemKind.COLLAPSED_FOLDER: {
				const childCount = clampReportedInteger(item.childCount, 0, SKELETON_GUILD_RAIL_COLLAPSED_FOLDER_CHILD_LIMIT);
				if (childCount == null || typeof item.showIconWhenCollapsed !== 'boolean') {
					return;
				}
				organizedItems.push(
					Object.freeze({
						kind: item.kind,
						indicator,
						childCount,
						showIconWhenCollapsed: item.showIconWhenCollapsed,
					}),
				);
				remainingVisualRows -= 1;
				break;
			}
			case SkeletonGuildRailItemKind.EXPANDED_FOLDER: {
				const childCount = clampReportedInteger(item.childCount, 0, remainingVisualRows - 1);
				if (childCount == null) {
					return;
				}
				organizedItems.push(
					Object.freeze({
						kind: item.kind,
						indicator,
						childCount,
						childIndicators: normalizeGuildRailChildIndicators(item.childIndicators, childCount),
						selectedChildIndex: normalizeGuildRailSelectedChildIndex(item.selectedChildIndex, childCount),
					}),
				);
				remainingVisualRows -= childCount + 1;
				break;
			}
			default:
				return;
		}
	}
	let selectedItemIndex = clampReportedInteger(
		layout.selectedItemIndex,
		SKELETON_NO_SELECTED_RAIL_ITEM_INDEX,
		Math.max(SKELETON_NO_SELECTED_RAIL_ITEM_INDEX, layout.organizedItems.length - 1),
	);
	if (selectedItemIndex != null && selectedItemIndex > organizedItems.length - 1) {
		selectedItemIndex = SKELETON_NO_SELECTED_RAIL_ITEM_INDEX;
	}
	if (selectedItemIndex == null) {
		return;
	}
	const inlineDmUnreadFlags: Array<boolean> = [];
	for (let index = 0; index < inlineDmRowCount; index += 1) {
		inlineDmUnreadFlags.push(layout.inlineDmUnreadFlags[index] === true);
	}
	const selectedInlineDmRowIndex = clampReportedInteger(
		layout.selectedInlineDmRowIndex,
		SKELETON_NO_SELECTED_RAIL_ITEM_INDEX,
		Math.max(SKELETON_NO_SELECTED_RAIL_ITEM_INDEX, inlineDmRowCount - 1),
	);
	if (selectedInlineDmRowIndex == null) {
		return;
	}
	const normalizedLayout: RememberedSkeletonGuildRailLayout = Object.freeze({
		inlineDmRowCount,
		inlineDmUnreadFlags: Object.freeze(inlineDmUnreadFlags),
		selectedInlineDmRowIndex,
		outageVisible: layout.outageVisible,
		fluxerVisible: layout.fluxerVisible,
		favoritesVisible: layout.favoritesVisible,
		discoveryVisible: layout.discoveryVisible,
		addGuildVisible: layout.addGuildVisible,
		downloadVisible: layout.downloadVisible,
		helpVisible: layout.helpVisible,
		selectedItemIndex,
		organizedItems: Object.freeze(organizedItems),
		scrollTopPx:
			pendingGuildRailScrollTopPx ?? pendingChrome.guildRail?.scrollTopPx ?? state.chrome?.guildRail?.scrollTopPx ?? 0,
	});
	pendingGuildRailScrollTopPx = null;
	pendingChrome = {...pendingChrome, guildRail: normalizedLayout};
	if (captureEnabled) {
		commitGuildRailLayout(normalizedLayout);
	}
}

export function reportSkeletonGuildRailScrollTop(scrollTopPx: number): void {
	synchronizeActiveAccountState();
	const normalizedScrollTopPx = clampReportedInteger(
		scrollTopPx / getRemScaleForDocument(document),
		0,
		MAX_GUILD_RAIL_SCROLL_TOP_PX,
	);
	if (normalizedScrollTopPx == null) {
		return;
	}
	const base = pendingChrome.guildRail ?? state.chrome?.guildRail;
	if (base == null) {
		pendingGuildRailScrollTopPx = normalizedScrollTopPx;
		return;
	}
	pendingGuildRailScrollTopPx = null;
	if (base.scrollTopPx === normalizedScrollTopPx) {
		return;
	}
	const normalizedLayout: RememberedSkeletonGuildRailLayout = Object.freeze({
		...base,
		scrollTopPx: normalizedScrollTopPx,
	});
	pendingChrome = {...pendingChrome, guildRail: normalizedLayout};
	if (captureEnabled) {
		commitGuildRailLayout(normalizedLayout);
	}
}

export function reportSkeletonFriendsRowCount(tab: SkeletonFriendsTab, rowCount: number, incomingRowCount = 0): void {
	synchronizeActiveAccountState();
	const clampedRowCount = clampReportedInteger(rowCount, 0, MAX_FRIEND_ROWS);
	if (clampedRowCount == null || !isSkeletonFriendsTab(tab)) {
		return;
	}
	if (tab === SkeletonFriendsTab.ONLINE) {
		pendingFriends.onlineRowCount = clampedRowCount;
	} else if (tab === SkeletonFriendsTab.ALL) {
		pendingFriends.allRowCount = clampedRowCount;
	} else if (tab === SkeletonFriendsTab.PENDING) {
		pendingFriends.pendingRowCount = clampedRowCount;
		pendingFriends.pendingIncomingRowCount = clampReportedInteger(incomingRowCount, 0, clampedRowCount) ?? 0;
	}
	if (captureEnabled) {
		commitFriendsLayout();
	}
}

export interface SkeletonFriendsHeaderReport {
	readonly activeTab: SkeletonFriendsTab;
	readonly tabWidthsPx: ReadonlyArray<number>;
	readonly pendingBadgeVisible: boolean;
}

function resolveReportedFriendsTabWidths(values: ReadonlyArray<number>): ReadonlyArray<number> {
	const previous =
		pendingFriends.tabWidthsPx ?? state.friends?.tabWidthsPx ?? SKELETON_DEFAULT_FRIENDS_LAYOUT.tabWidthsPx;
	const widths: Array<number> = [];
	for (let index = 0; index < SKELETON_FRIENDS_TAB_COUNT; index += 1) {
		const reported = Array.isArray(values) ? values[index] : undefined;
		const measured = typeof reported === 'number' ? clampReportedInteger(reported, 0, MAX_MEASURED_WIDTH_PX) : null;
		if (measured != null && measured !== SKELETON_UNMEASURED_WIDTH_PX) {
			widths.push(measured);
			continue;
		}
		widths.push(previous[index] ?? SKELETON_UNMEASURED_WIDTH_PX);
	}
	return Object.freeze(widths);
}

export function reportSkeletonFriendsHeaderLayout(report: SkeletonFriendsHeaderReport): void {
	synchronizeActiveAccountState();
	if (!isSkeletonFriendsTab(report.activeTab) || typeof report.pendingBadgeVisible !== 'boolean') {
		return;
	}
	pendingFriends.activeTab = report.activeTab;
	pendingFriends.pendingBadgeVisible = report.pendingBadgeVisible;
	pendingFriends.tabWidthsPx = resolveReportedFriendsTabWidths(report.tabWidthsPx);
	if (captureEnabled) {
		commitFriendsLayout();
	}
}

export function reportSkeletonActiveNowLayout(
	visible: boolean,
	cards: ReadonlyArray<RememberedSkeletonActiveNowCard>,
): void {
	synchronizeActiveAccountState();
	if (typeof visible !== 'boolean' || !Array.isArray(cards)) {
		return;
	}
	const normalizedCards: Array<RememberedSkeletonActiveNowCard> = [];
	for (const card of cards.slice(0, MAX_ACTIVE_NOW_CARDS)) {
		const participantCount = clampReportedInteger(card.participantCount, 0, MAX_ACTIVE_NOW_PARTICIPANTS);
		if (participantCount == null || typeof card.streaming !== 'boolean') {
			return;
		}
		normalizedCards.push(Object.freeze({participantCount, streaming: card.streaming}));
	}
	pendingFriends.activeNowVisible = visible;
	pendingFriends.activeNowCards = EMPTY_ACTIVE_NOW_CARDS;
	if (visible) {
		pendingFriends.activeNowCards = Object.freeze(normalizedCards);
	}
	if (captureEnabled) {
		commitFriendsLayout();
	}
}

export function reportSkeletonChannelHeaderLayout(layout: RememberedSkeletonChannelHeaderLayout): void {
	synchronizeActiveAccountState();
	if (
		typeof layout.staffToolsVisible !== 'boolean' ||
		typeof layout.updaterVisible !== 'boolean' ||
		typeof layout.favoritesVisible !== 'boolean'
	) {
		return;
	}
	const normalizedLayout = Object.freeze({
		staffToolsVisible: layout.staffToolsVisible,
		updaterVisible: layout.updaterVisible,
		favoritesVisible: layout.favoritesVisible,
	});
	pendingChannelHeader = normalizedLayout;
	if (captureEnabled) {
		commitChannelHeaderLayout(normalizedLayout);
	}
}

export function reportSkeletonNagbarLayout(rows: ReadonlyArray<RememberedSkeletonNagbarRow>): void {
	synchronizeActiveAccountState();
	if (!Array.isArray(rows)) {
		return;
	}
	const normalizedRows: Array<RememberedSkeletonNagbarRow> = [];
	for (const row of rows.slice(0, MAX_NAGBAR_ROWS)) {
		if (!isSkeletonNagbarTone(row.tone) || typeof row.hasActions !== 'boolean') {
			return;
		}
		normalizedRows.push(
			Object.freeze({tone: row.tone, hasActions: row.hasActions, dismissible: row.dismissible === true}),
		);
	}
	const normalizedLayout = Object.freeze({rows: Object.freeze(normalizedRows)});
	pendingNagbar = normalizedLayout;
	if (captureEnabled) {
		commitNagbarLayout(normalizedLayout);
	}
}

export function reportSkeletonComposerLayout(layout: RememberedSkeletonComposerLayout): void {
	synchronizeActiveAccountState();
	const desktopActionCount = clampReportedInteger(layout.desktopActionCount, 0, MAX_COMPOSER_DESKTOP_ACTION_COUNT);
	const mobileActionCount = clampReportedInteger(layout.mobileActionCount, 0, MAX_COMPOSER_MOBILE_ACTION_COUNT);
	if (desktopActionCount == null || mobileActionCount == null || typeof layout.sendDividerVisible !== 'boolean') {
		return;
	}
	const normalizedLayout = Object.freeze({
		desktopActionCount,
		mobileActionCount,
		sendDividerVisible: layout.sendDividerVisible,
	});
	pendingComposer = normalizedLayout;
	if (captureEnabled) {
		commitComposerLayout(normalizedLayout);
	}
}

export interface SkeletonMessagePresentationReport {
	readonly compact: boolean;
	readonly messageGutterPx: number;
	readonly fontSizePx: number;
	readonly groupSpacingPx: number;
	readonly compactAvatarsVisible: boolean;
	readonly viewportHeightPx: number;
}

export function reportSkeletonMessagePresentation(report: SkeletonMessagePresentationReport): void {
	synchronizeActiveAccountState();
	const messageGutterPx = clampReportedInteger(report.messageGutterPx, 0, MAX_MESSAGE_GUTTER_PX);
	const fontSizePx = clampReportedInteger(report.fontSizePx, MIN_MESSAGE_FONT_SIZE_PX, MAX_MESSAGE_FONT_SIZE_PX);
	const groupSpacingPx = clampReportedInteger(report.groupSpacingPx, 0, MAX_MESSAGE_GROUP_SPACING_PX);
	const viewportHeightPx = clampReportedInteger(report.viewportHeightPx, 0, MAX_MEASURED_HEIGHT_PX);
	if (
		messageGutterPx == null ||
		fontSizePx == null ||
		groupSpacingPx == null ||
		viewportHeightPx == null ||
		typeof report.compact !== 'boolean' ||
		typeof report.compactAvatarsVisible !== 'boolean'
	) {
		return;
	}
	pendingMessagePresentation.compact = report.compact;
	pendingMessagePresentation.messageGutterPx = messageGutterPx;
	pendingMessagePresentation.fontSizePx = fontSizePx;
	pendingMessagePresentation.groupSpacingPx = groupSpacingPx;
	pendingMessagePresentation.compactAvatarsVisible = report.compactAvatarsVisible;
	pendingMessagePresentation.viewportHeightPx = viewportHeightPx;
	if (captureEnabled) {
		commitMessagePresentation();
	}
}

export function reportSkeletonCompactTimestampWidth(widthPx: number): void {
	synchronizeActiveAccountState();
	const compactTimestampWidthPx = clampReportedInteger(widthPx, 0, MAX_MEASURED_WIDTH_PX);
	if (compactTimestampWidthPx == null || compactTimestampWidthPx === 0) {
		return;
	}
	pendingMessagePresentation.compactTimestampWidthPx = compactTimestampWidthPx;
	if (captureEnabled) {
		commitMessagePresentation();
	}
}

export function reportSkeletonVoicePresence(connected: boolean, panelHeightPx: number): void {
	synchronizeActiveAccountState();
	const clampedPanelHeightPx = clampReportedInteger(panelHeightPx, 0, MAX_VOICE_CONNECTION_HEIGHT_PX);
	if (clampedPanelHeightPx == null || typeof connected !== 'boolean') {
		return;
	}
	let panelHeight = 0;
	if (connected) {
		panelHeight = clampedPanelHeightPx;
	}
	const normalizedPresence = Object.freeze({connected, panelHeightPx: panelHeight});
	pendingVoice = normalizedPresence;
	if (captureEnabled) {
		commitVoicePresence(normalizedPresence);
	}
}

export function reportSkeletonVoiceConnected(connected: boolean): void {
	synchronizeActiveAccountState();
	if (typeof connected !== 'boolean') {
		return;
	}
	const currentPanelHeightPx = pendingVoice?.panelHeightPx ?? state.voice?.panelHeightPx ?? 0;
	let panelHeight = 0;
	if (connected) {
		panelHeight = currentPanelHeightPx;
	}
	const normalizedPresence = Object.freeze({connected, panelHeightPx: panelHeight});
	pendingVoice = normalizedPresence;
	if (captureEnabled) {
		commitVoicePresence(normalizedPresence);
	}
}

export function reportSkeletonDiscoveryGrid(columnCount: number, visibleRowCount: number): void {
	synchronizeActiveAccountState();
	const clampedColumnCount = clampReportedInteger(columnCount, 1, MAX_DISCOVERY_COLUMNS);
	const clampedVisibleRowCount = clampReportedInteger(visibleRowCount, 0, MAX_DISCOVERY_VISIBLE_ROWS);
	if (clampedColumnCount == null || clampedVisibleRowCount == null) {
		return;
	}
	pendingDiscovery.columnCount = clampedColumnCount;
	pendingDiscovery.visibleRowCount = clampedVisibleRowCount;
	if (captureEnabled) {
		commitDiscoveryLayout();
	}
}

export function reportSkeletonDiscoveryCategoryTabs(widthsPx: ReadonlyArray<number>): void {
	synchronizeActiveAccountState();
	const categoryTabWidthsPx = clampReportedWidths(widthsPx, MAX_DISCOVERY_CATEGORY_TABS);
	if (categoryTabWidthsPx == null || categoryTabWidthsPx.length === 0) {
		return;
	}
	pendingDiscovery.categoryTabWidthsPx = categoryTabWidthsPx;
	if (captureEnabled) {
		commitDiscoveryLayout();
	}
}

export function reportSkeletonSimplePageLayout(
	route: SkeletonSimplePageRoute,
	body: SkeletonSimplePageBody,
	rowCount: number,
	selectable = false,
): void {
	synchronizeActiveAccountState();
	const clampedRowCount = clampReportedInteger(rowCount, 0, MAX_SIMPLE_PAGE_ROWS);
	if (clampedRowCount == null || !isSimplePageRoute(route) || !isSimplePageBody(body)) {
		return;
	}
	const layout: PendingSkeletonSimplePageLayout = Object.freeze({
		route,
		body,
		rowCount: clampedRowCount,
		selectable: selectable === true,
	});
	const next = pendingSimplePages.filter((entry) => entry.route !== route);
	next.push(layout);
	pendingSimplePages = Object.freeze(next.slice(-MAX_SIMPLE_PAGE_LAYOUTS));
	if (captureEnabled) {
		commitSimplePageLayout(layout);
	}
}

export interface SkeletonChannelProjectionReport {
	readonly showTopic: boolean;
	readonly nameWidthPx: number;
	readonly topicWidthPx: number;
	readonly desktopLeadingActionCount: number;
	readonly mobileActionCount: number;
	readonly memberListVisible: boolean;
	readonly searchPanelOpen: boolean;
}

export function reportSkeletonChannelProjection(
	channelId: string,
	channelType: number,
	projection: SkeletonChannelProjectionReport,
): void {
	synchronizeActiveAccountState();
	if (!CHANNEL_ID_PATTERN.test(channelId)) {
		return;
	}
	const channelKind = resolveSkeletonChannelProjectionKind(channelType);
	const nameWidthPx = clampReportedInteger(projection.nameWidthPx, 0, MAX_MEASURED_WIDTH_PX);
	const topicWidthPx = clampReportedInteger(projection.topicWidthPx, 0, MAX_MEASURED_WIDTH_PX);
	const desktopLeadingActionCount = clampReportedInteger(
		projection.desktopLeadingActionCount,
		0,
		MAX_HEADER_ACTION_COUNT,
	);
	const mobileActionCount = clampReportedInteger(projection.mobileActionCount, 0, MAX_HEADER_ACTION_COUNT);
	if (
		nameWidthPx == null ||
		topicWidthPx == null ||
		desktopLeadingActionCount == null ||
		mobileActionCount == null ||
		typeof projection.showTopic !== 'boolean' ||
		typeof projection.memberListVisible !== 'boolean' ||
		typeof projection.searchPanelOpen !== 'boolean'
	) {
		return;
	}
	const showTopic = projection.showTopic && canChannelProjectionShowTopic(channelKind);
	let resolvedTopicWidthPx = 0;
	if (showTopic) {
		resolvedTopicWidthPx = topicWidthPx;
	}
	const normalizedProjection: PendingSkeletonChannelProjection = Object.freeze({
		fingerprint: createChannelFingerprint(channelId),
		channelKind,
		showTopic,
		nameWidthPx,
		topicWidthPx: resolvedTopicWidthPx,
		desktopLeadingActionCount,
		mobileActionCount,
		memberListVisible: projection.memberListVisible && canChannelProjectionShowMemberList(channelKind),
		searchPanelOpen: projection.searchPanelOpen,
	});
	const next = pendingChannelProjections.filter((entry) => entry.fingerprint !== normalizedProjection.fingerprint);
	next.push(normalizedProjection);
	pendingChannelProjections = Object.freeze(next.slice(-MAX_CHANNEL_PROJECTIONS));
	if (captureEnabled) {
		commitChannelProjection(normalizedProjection);
	}
}

export interface SkeletonMemberGroupReport {
	readonly rowCount: number;
	readonly headingWidthPx: number;
	readonly subtextFlags?: ReadonlyArray<boolean>;
}

export function reportSkeletonMemberLayout(
	channelId: string,
	kind: SkeletonMemberSurfaceKind,
	memberGroups: ReadonlyArray<SkeletonMemberGroupReport>,
): void {
	synchronizeActiveAccountState();
	if (!CHANNEL_ID_PATTERN.test(channelId) || !isSkeletonMemberSurfaceKind(kind) || !Array.isArray(memberGroups)) {
		return;
	}
	const normalizedGroups: Array<RememberedSkeletonMemberGroup> = [];
	for (const group of memberGroups.slice(0, MAX_MEMBER_GROUPS)) {
		const rowCount = clampReportedInteger(group.rowCount, 1, MAX_MEMBER_GROUP_ROWS);
		const headingWidthPx = clampReportedInteger(group.headingWidthPx, 0, MAX_MEASURED_WIDTH_PX);
		if (rowCount == null || headingWidthPx == null) {
			return;
		}
		normalizedGroups.push(
			Object.freeze({
				rowCount,
				headingWidthPx,
				subtextFlags: normalizeSubtextFlags(group.subtextFlags, rowCount),
			}),
		);
	}
	const fingerprint = createChannelFingerprint(channelId);
	const layout = Object.freeze({fingerprint, kind, memberGroups: Object.freeze(normalizedGroups)});
	const next = pendingChannelMemberLayouts.filter((entry) => entry.fingerprint !== fingerprint);
	next.push(layout);
	pendingChannelMemberLayouts = Object.freeze(next.slice(-MAX_CHANNEL_MEMBER_LAYOUTS));
	if (captureEnabled) {
		commitChannelMemberLayout(layout);
	}
}

export function reportSkeletonGuildPresentation(
	guildId: string,
	presentation: RememberedSkeletonGuildPresentation,
): void {
	synchronizeActiveAccountState();
	if (!GUILD_ID_PATTERN.test(guildId) || !isGuildBannerPlacement(presentation.bannerPlacement)) {
		return;
	}
	const headerNameWidthPx = clampReportedInteger(presentation.headerNameWidthPx, 0, MAX_MEASURED_WIDTH_PX);
	const bannerAspectRatio = clampReportedRatio(
		presentation.bannerAspectRatio,
		MIN_BANNER_ASPECT_RATIO,
		MAX_BANNER_ASPECT_RATIO,
	);
	if (headerNameWidthPx == null || bannerAspectRatio == null || typeof presentation.badgeVisible !== 'boolean') {
		return;
	}
	let resolvedBannerAspectRatio = SKELETON_DEFAULT_GUILD_BANNER_ASPECT_RATIO;
	if (presentation.bannerPlacement !== SkeletonGuildBannerPlacement.NONE) {
		resolvedBannerAspectRatio = bannerAspectRatio;
	}
	const normalizedPresentation: PendingSkeletonGuildPresentation = Object.freeze({
		fingerprint: createGuildFingerprint(guildId),
		headerNameWidthPx,
		badgeVisible: presentation.badgeVisible,
		bannerPlacement: presentation.bannerPlacement,
		bannerAspectRatio: resolvedBannerAspectRatio,
	});
	const next = pendingGuildPresentations.filter((entry) => entry.fingerprint !== normalizedPresentation.fingerprint);
	next.push(normalizedPresentation);
	pendingGuildPresentations = Object.freeze(next.slice(-MAX_GUILD_PRESENTATIONS));
	if (captureEnabled) {
		commitGuildPresentation(normalizedPresentation);
	}
}

export function reportSkeletonGuildChannelList(guildId: string, list: RememberedSkeletonGuildChannelList): void {
	synchronizeActiveAccountState();
	if (!GUILD_ID_PATTERN.test(guildId) || typeof list.membersRowVisible !== 'boolean' || !Array.isArray(list.groups)) {
		return;
	}
	const normalizedGroups: Array<RememberedSkeletonGuildChannelGroup> = [];
	let remainingChannelRows = MAX_GUILD_CHANNEL_ROWS;
	for (const group of list.groups.slice(0, MAX_GUILD_CHANNEL_GROUPS)) {
		if (
			typeof group.categoryHeaderVisible !== 'boolean' ||
			typeof group.collapsed !== 'boolean' ||
			!Array.isArray(group.channels)
		) {
			return;
		}
		const categoryNameWidthPx = clampReportedInteger(group.categoryNameWidthPx, 0, MAX_MEASURED_WIDTH_PX);
		if (categoryNameWidthPx == null) {
			return;
		}
		const channels: Array<RememberedSkeletonGuildChannelRow> = [];
		for (const channel of group.channels.slice(0, remainingChannelRows)) {
			const nameWidthPx = clampReportedInteger(channel.nameWidthPx, 0, MAX_MEASURED_WIDTH_PX);
			if (nameWidthPx == null || typeof channel.voice !== 'boolean') {
				return;
			}
			channels.push(Object.freeze({voice: channel.voice, nameWidthPx}));
		}
		remainingChannelRows -= channels.length;
		let collapsed = false;
		let resolvedCategoryNameWidthPx = 0;
		if (group.categoryHeaderVisible) {
			collapsed = group.collapsed;
			resolvedCategoryNameWidthPx = categoryNameWidthPx;
		}
		normalizedGroups.push(
			Object.freeze({
				categoryHeaderVisible: group.categoryHeaderVisible,
				collapsed,
				categoryNameWidthPx: resolvedCategoryNameWidthPx,
				channels: Object.freeze(channels),
			}),
		);
		if (remainingChannelRows === 0) {
			break;
		}
	}
	const normalizedList: PendingSkeletonGuildChannelList = Object.freeze({
		fingerprint: createGuildFingerprint(guildId),
		membersRowVisible: list.membersRowVisible,
		groups: Object.freeze(normalizedGroups),
	});
	const next = pendingGuildChannelLists.filter((entry) => entry.fingerprint !== normalizedList.fingerprint);
	next.push(normalizedList);
	pendingGuildChannelLists = Object.freeze(next.slice(-MAX_GUILD_CHANNEL_LISTS));
	if (captureEnabled) {
		commitGuildChannelList(normalizedList);
	}
}

function ensureActiveAccountStateCurrent(): boolean {
	synchronizeActiveAccountState();
	return activeAccountFingerprint != null;
}

export function getRememberedSkeletonDMSidebarLayout(): RememberedSkeletonDMSidebarLayout | null {
	if (!ensureActiveAccountStateCurrent()) {
		return null;
	}
	const layout = state.chrome?.dmSidebar;
	if (layout == null || !isFreshTimestamp(layout.capturedAt, Date.now())) {
		return null;
	}
	return layout;
}

export function getRememberedSkeletonGuildRailScrollTopPx(): number {
	return getRememberedSkeletonGuildRailLayout()?.scrollTopPx ?? 0;
}

export function getRememberedSkeletonGuildRailLayout(): RememberedSkeletonGuildRailLayout | null {
	if (!ensureActiveAccountStateCurrent()) {
		return null;
	}
	const layout = state.chrome?.guildRail;
	if (layout == null || !isFreshTimestamp(layout.capturedAt, Date.now())) {
		return null;
	}
	return layout;
}

export function getRememberedSkeletonFriendsLayout(): RememberedSkeletonFriendsLayout | null {
	if (!ensureActiveAccountStateCurrent()) {
		return null;
	}
	if (state.friends == null || !isFreshTimestamp(state.friends.capturedAt, Date.now())) {
		return null;
	}
	return state.friends;
}

export function getRememberedSkeletonChannelHeaderLayout(): RememberedSkeletonChannelHeaderLayout | null {
	if (!ensureActiveAccountStateCurrent()) {
		return null;
	}
	const layout = state.channelHeader;
	if (layout == null || !isFreshTimestamp(layout.capturedAt, Date.now())) {
		return null;
	}
	return layout;
}

export function getRememberedSkeletonNagbarLayout(): RememberedSkeletonNagbarLayout | null {
	if (!ensureActiveAccountStateCurrent()) {
		return null;
	}
	const layout = state.nagbar;
	if (layout == null || !isFreshTimestamp(layout.capturedAt, Date.now())) {
		return null;
	}
	return layout;
}

export function getRememberedSkeletonComposerLayout(): RememberedSkeletonComposerLayout | null {
	if (!ensureActiveAccountStateCurrent()) {
		return null;
	}
	const layout = state.composer;
	if (layout == null || !isFreshTimestamp(layout.capturedAt, Date.now())) {
		return null;
	}
	return layout;
}

export function getRememberedSkeletonMessagePresentation(): RememberedSkeletonMessagePresentation | null {
	if (!ensureActiveAccountStateCurrent()) {
		return null;
	}
	const presentation = state.messagePresentation;
	if (presentation == null || !isFreshTimestamp(presentation.capturedAt, Date.now())) {
		return null;
	}
	return presentation;
}

export function getRememberedSkeletonVoicePresence(): RememberedSkeletonVoicePresence | null {
	if (!ensureActiveAccountStateCurrent()) {
		return null;
	}
	const presence = state.voice;
	if (presence == null || !isFreshTimestamp(presence.capturedAt, Date.now())) {
		return null;
	}
	return presence;
}

export function getRememberedSkeletonDiscoveryLayout(): RememberedSkeletonDiscoveryLayout | null {
	if (!ensureActiveAccountStateCurrent()) {
		return null;
	}
	const layout = state.discovery;
	if (layout == null || !isFreshTimestamp(layout.capturedAt, Date.now())) {
		return null;
	}
	return layout;
}

export function getRememberedSkeletonSimplePageLayout(
	route: SkeletonSimplePageRoute,
): RememberedSkeletonSimplePageLayout | null {
	if (!ensureActiveAccountStateCurrent() || !isSimplePageRoute(route)) {
		return null;
	}
	const layout = state.simplePages?.find((entry) => entry.route === route);
	if (layout == null || !isFreshTimestamp(layout.capturedAt, Date.now())) {
		return null;
	}
	return layout;
}

export function getRememberedSkeletonMemberGroups(
	channelId: string,
	kind: SkeletonMemberSurfaceKind,
): ReadonlyArray<RememberedSkeletonMemberGroup> | null {
	if (!ensureActiveAccountStateCurrent()) {
		return null;
	}
	if (!CHANNEL_ID_PATTERN.test(channelId) || !isSkeletonMemberSurfaceKind(kind)) {
		return null;
	}
	const fingerprint = createChannelFingerprint(channelId);
	const layout = state.channelMemberLayouts?.find((entry) => entry.fingerprint === fingerprint);
	if (
		layout == null ||
		layout.kind !== kind ||
		layout.memberGroups.length === 0 ||
		!isFreshTimestamp(layout.capturedAt, Date.now())
	) {
		return null;
	}
	return layout.memberGroups;
}

export function getRememberedSkeletonChannelProjection(channelId: string): RememberedSkeletonChannelProjection | null {
	if (!ensureActiveAccountStateCurrent()) {
		return null;
	}
	if (!CHANNEL_ID_PATTERN.test(channelId)) {
		return null;
	}
	const fingerprint = createChannelFingerprint(channelId);
	const projection = state.channelProjections?.find((entry) => entry.fingerprint === fingerprint);
	if (projection == null || !isFreshTimestamp(projection.capturedAt, Date.now())) {
		return null;
	}
	return projection;
}

export function getRememberedSkeletonGuildPresentation(guildId: string): RememberedSkeletonGuildPresentation | null {
	if (!ensureActiveAccountStateCurrent()) {
		return null;
	}
	if (!GUILD_ID_PATTERN.test(guildId)) {
		return null;
	}
	const fingerprint = createGuildFingerprint(guildId);
	const presentation = state.guildPresentations?.find((entry) => entry.fingerprint === fingerprint);
	if (presentation == null || !isFreshTimestamp(presentation.capturedAt, Date.now())) {
		return null;
	}
	return presentation;
}

export function getRememberedSkeletonGuildChannelList(guildId: string): RememberedSkeletonGuildChannelList | null {
	if (!ensureActiveAccountStateCurrent()) {
		return null;
	}
	if (!GUILD_ID_PATTERN.test(guildId)) {
		return null;
	}
	const fingerprint = createGuildFingerprint(guildId);
	const list = state.guildChannelLists?.find((entry) => entry.fingerprint === fingerprint);
	if (list == null || !isFreshTimestamp(list.capturedAt, Date.now())) {
		return null;
	}
	return list;
}
