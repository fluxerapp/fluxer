// SPDX-License-Identifier: AGPL-3.0-or-later

const LEGACY_SKELETON_LAYOUT_MEMORY_KEY = 'SkeletonLayoutMemory';
const LEGACY_SIDEBAR_WIDTH_KEY = 'fluxer:ui:sidebar-width';
const DM_ROOT_PATHNAME = '/channels/@me';

export const LEGACY_SHELL_HINT_SEED_VERSION = 4;

export const LEGACY_SHELL_HINT_NAGBAR_TONE_ORDER: ReadonlyArray<string> = Object.freeze([
	'neutral',
	'maintenance',
	'maintenance_scheduled',
	'maintenance_active',
	'maintenance_completed',
	'brand',
	'danger',
	'alert',
	'premium',
	'legal',
	'voice',
	'critical',
	'development',
	'streamer',
	'encoder',
]);

const RAIL_ITEM_INDICATOR_CODES: Readonly<Record<string, number>> = Object.freeze({none: 0, unread: 1, mention: 2});
const RAIL_ITEM_KIND_GUILD = 'guild';
const RAIL_ITEM_KIND_COLLAPSED_FOLDER = 'collapsed_folder';
const RAIL_ITEM_KIND_EXPANDED_FOLDER = 'expanded_folder';
const RAIL_ITEM_GUILD_CODE = 0;
const RAIL_ITEM_COLLAPSED_FOLDER_CODE = 1;
const RAIL_ITEM_EXPANDED_FOLDER_CODE = 2;
const RAIL_BOTTOM_DISCOVERY_BIT = 1;
const RAIL_BOTTOM_ADD_GUILD_BIT = 2;
const RAIL_BOTTOM_DOWNLOAD_BIT = 4;
const RAIL_BOTTOM_HELP_BIT = 8;
const DM_ACTION_FRIENDS_BIT = 1;
const DM_ACTION_PERSONAL_NOTES_BIT = 2;
const DM_ACTION_PREMIUM_BIT = 4;
const MAX_RAIL_INLINE_DM_ROWS = 16;
const MAX_RAIL_ORGANIZED_ITEMS = 24;
const MAX_RAIL_COLLAPSED_FOLDER_CHILDREN = 4;
const MAX_RAIL_SCROLL_TOP_PX = 100_000;
const MAX_DM_SIDEBAR_ROWS = 40;
const MAX_NAGBAR_ROWS = 3;
const MAX_COMPOSER_ACTION_COUNT = 8;
const MAX_ACTIVE_NOW_CARDS = 24;
const MAX_MESSAGE_METRIC_PX = 256;
const MIN_FONT_SIZE_PX = 8;
const MAX_FONT_SIZE_PX = 64;
const MAX_VIEWPORT_HEIGHT_PX = 20_000;
const FALLBACK_RAIL_GUILD_COUNT = 6;
const CHANNEL_KIND_DM_CODE = 2;
const UNSET_ACTION_COUNT = -1;
const MIN_SCALED_BANNER_ASPECT_RATIO = 250;
const MIN_SIDEBAR_WIDTH_PX = 200;
const MAX_SIDEBAR_WIDTH_PX = 480;
const MAX_ACCOUNT_KEY_LENGTH = 256;
const NO_SELECTED_ROW_INDEX = -1;

function isRecord(value: unknown): value is Record<string, unknown> {
	return value !== null && typeof value === 'object' && !Array.isArray(value);
}

function parseRecord(value: unknown): Record<string, unknown> | null {
	if (typeof value !== 'string') return null;
	try {
		const parsed: unknown = JSON.parse(value);
		return isRecord(parsed) ? parsed : null;
	} catch {
		return null;
	}
}

function flag(value: unknown): number {
	return value === true ? 1 : 0;
}

function flags(value: unknown, limit: number): Array<number> {
	return Array.isArray(value) ? value.slice(0, limit).map(flag) : [];
}

function clampRounded(value: unknown, min: number, max: number): number {
	if (typeof value !== 'number' || !Number.isFinite(value)) return min;
	return Math.min(max, Math.max(min, Math.round(value)));
}

function indicatorCode(value: unknown): number {
	return (typeof value === 'string' ? RAIL_ITEM_INDICATOR_CODES[value] : undefined) ?? 0;
}

function encodeRailItem(item: unknown): Array<number> | null {
	if (!isRecord(item)) return null;
	const indicator = indicatorCode(item.indicator);
	if (item.kind === RAIL_ITEM_KIND_GUILD) {
		return [RAIL_ITEM_GUILD_CODE, indicator];
	}
	if (item.kind === RAIL_ITEM_KIND_COLLAPSED_FOLDER) {
		return [
			RAIL_ITEM_COLLAPSED_FOLDER_CODE,
			indicator,
			clampRounded(item.childCount, 0, MAX_RAIL_COLLAPSED_FOLDER_CHILDREN),
			flag(item.showIconWhenCollapsed),
		];
	}
	if (item.kind === RAIL_ITEM_KIND_EXPANDED_FOLDER) {
		const childCount = clampRounded(item.childCount, 0, MAX_RAIL_ORGANIZED_ITEMS - 1);
		const childIndicators = Array.isArray(item.childIndicators) ? item.childIndicators : [];
		const encoded = [
			RAIL_ITEM_EXPANDED_FOLDER_CODE,
			indicator,
			childCount,
			clampRounded(item.selectedChildIndex, NO_SELECTED_ROW_INDEX, childCount - 1),
		];
		for (let index = 0; index < childCount; index++) {
			encoded.push(indicatorCode(childIndicators[index]));
		}
		return encoded;
	}
	return null;
}

function seedGuildRail(hint: Record<string, unknown>, rail: unknown): void {
	if (!isRecord(rail) || !Array.isArray(rail.organizedItems)) return;
	const items: Array<Array<number>> = [];
	for (const item of rail.organizedItems.slice(0, MAX_RAIL_ORGANIZED_ITEMS)) {
		const encoded = encodeRailItem(item);
		if (encoded === null) return;
		items.push(encoded);
	}
	const inlineDmRowCount = clampRounded(rail.inlineDmRowCount, 0, MAX_RAIL_INLINE_DM_ROWS);
	const unread = Array.isArray(rail.inlineDmUnreadFlags) ? rail.inlineDmUnreadFlags : [];
	let bottomMask = 0;
	if (rail.discoveryVisible === true) bottomMask |= RAIL_BOTTOM_DISCOVERY_BIT;
	if (rail.addGuildVisible === true) bottomMask |= RAIL_BOTTOM_ADD_GUILD_BIT;
	if (rail.downloadVisible === true) bottomMask |= RAIL_BOTTOM_DOWNLOAD_BIT;
	if (rail.helpVisible === true) bottomMask |= RAIL_BOTTOM_HELP_BIT;
	hint.rf = flag(rail.fluxerVisible);
	hint.rti = rail.fluxerVisible === true ? 0 : NO_SELECTED_ROW_INDEX;
	hint.rv = flag(rail.favoritesVisible);
	hint.ru = Array.from({length: inlineDmRowCount}, (_unused, index) => flag(unread[index]));
	hint.ro = flag(rail.outageVisible);
	hint.ri = items;
	hint.rbm = bottomMask;
	hint.rs = clampRounded(rail.scrollTopPx, 0, MAX_RAIL_SCROLL_TOP_PX);
}

function seedDMSidebar(hint: Record<string, unknown>, sidebar: unknown): void {
	if (!isRecord(sidebar) || sidebar.isMobile !== false) return;
	let actionMask = 0;
	if (sidebar.friendsVisible === true) actionMask |= DM_ACTION_FRIENDS_BIT;
	if (sidebar.personalNotesVisible === true) actionMask |= DM_ACTION_PERSONAL_NOTES_BIT;
	if (sidebar.premiumVisible === true) actionMask |= DM_ACTION_PREMIUM_BIT;
	hint.sr = clampRounded(sidebar.channelRowCount, 0, MAX_DM_SIDEBAR_ROWS);
	hint.sam = actionMask;
	hint.ss = flag(sidebar.sectionVisible);
	hint.sb = flags(sidebar.channelSubtextFlags, MAX_DM_SIDEBAR_ROWS);
}

function seedNagbar(hint: Record<string, unknown>, nagbar: unknown): void {
	if (!isRecord(nagbar) || !Array.isArray(nagbar.rows)) return;
	const rows: Array<Array<number>> = [];
	for (const row of nagbar.rows.slice(0, MAX_NAGBAR_ROWS)) {
		if (!isRecord(row) || typeof row.tone !== 'string') return;
		const tone = LEGACY_SHELL_HINT_NAGBAR_TONE_ORDER.indexOf(row.tone);
		if (tone < 0) return;
		rows.push([tone, flag(row.hasActions), flag(row.dismissible)]);
	}
	hint.nr = rows;
}

function seedSidebarWidth(hint: Record<string, unknown>, value: unknown): void {
	if (typeof value !== 'string' || value === '') return;
	const width = Number(value);
	if (!Number.isFinite(width)) return;
	hint.sw = clampRounded(width, MIN_SIDEBAR_WIDTH_PX, MAX_SIDEBAR_WIDTH_PX);
}

function seedChannelHeader(hint: Record<string, unknown>, header: unknown): void {
	if (!isRecord(header)) return;
	hint.cfv = flag(header.favoritesVisible);
	hint.cst = flag(header.staffToolsVisible);
	hint.cuv = flag(header.updaterVisible);
}

function seedFriends(hint: Record<string, unknown>, friends: unknown): void {
	if (!isRecord(friends) || friends.rowCount === undefined) return;
	const visible = friends.activeNowVisible === true;
	const count = visible ? clampRounded(friends.activeNowCardCount, 0, MAX_ACTIVE_NOW_CARDS) : 0;
	hint.fa = flag(visible);
	hint.fn = Array.from({length: count}, () => [0, 0]);
}

function seedComposer(hint: Record<string, unknown>, composer: unknown): void {
	if (!isRecord(composer)) return;
	hint.co = clampRounded(composer.desktopActionCount, 0, MAX_COMPOSER_ACTION_COUNT);
	hint.com = clampRounded(composer.mobileActionCount, 0, MAX_COMPOSER_ACTION_COUNT);
	hint.cdv = flag(composer.sendDividerVisible);
}

function seedMessagePresentation(hint: Record<string, unknown>, message: unknown): void {
	if (!isRecord(message)) return;
	hint.pc = flag(message.compact);
	hint.pg = clampRounded(message.messageGutterPx, 0, MAX_MESSAGE_METRIC_PX);
	hint.pf = clampRounded(message.fontSizePx, MIN_FONT_SIZE_PX, MAX_FONT_SIZE_PX);
	hint.ps = clampRounded(message.groupSpacingPx, 0, MAX_MESSAGE_METRIC_PX);
	hint.pa = flag(message.compactAvatarsVisible);
	hint.pt = clampRounded(message.compactTimestampWidthPx, 0, MAX_MESSAGE_METRIC_PX);
	if (typeof message.viewportHeightPx === 'number') {
		hint.pv = clampRounded(message.viewportHeightPx, 0, MAX_VIEWPORT_HEIGHT_PX);
	}
}

function dmRootShellHint(accountKey: string, now: number): Record<string, unknown> {
	return {
		v: LEGACY_SHELL_HINT_SEED_VERSION,
		t: now,
		p: DM_ROOT_PATHNAME,
		a: accountKey.slice(0, MAX_ACCOUNT_KEY_LENGTH),
		mo: 0,
		ua: 0,
		fs: 0,
		sw: 0,
		sv: 'dm',
		ck: 'other',
		cb: 1,
		rf: 1,
		rv: 1,
		ru: [],
		rti: 0,
		ro: 0,
		ri: Array.from({length: FALLBACK_RAIL_GUILD_COUNT}, () => [RAIL_ITEM_GUILD_CODE, 0]),
		rgi: NO_SELECTED_ROW_INDEX,
		rbm: RAIL_BOTTOM_DISCOVERY_BIT | RAIL_BOTTOM_ADD_GUILD_BIT | RAIL_BOTTOM_HELP_BIT,
		rbi: NO_SELECTED_ROW_INDEX,
		rs: 0,
		gb: 0,
		ga: MIN_SCALED_BANNER_ASPECT_RATIO,
		gn: 0,
		gd: 0,
		gm: 0,
		sr: 0,
		sam: DM_ACTION_FRIENDS_BIT | DM_ACTION_PERSONAL_NOTES_BIT | DM_ACTION_PREMIUM_BIT,
		ss: 1,
		cid: '',
		ch: CHANNEL_KIND_DM_CODE,
		ct: 0,
		cnw: 0,
		ctw: 0,
		cda: UNSET_ACTION_COUNT,
		cma: UNSET_ACTION_COUNT,
		cfv: 1,
		cst: 0,
		cuv: 0,
		ml: 0,
		co: 3,
		com: 2,
		cdv: 0,
		pc: 0,
		pg: 16,
		pf: 16,
		ps: 16,
		pa: 0,
		pt: 56,
		nr: [],
		vh: 0,
		vc: 0,
		mr: 0,
		ms: 0,
		mn: 0,
	};
}

export function legacyShellHintSeed(raw: Record<string, unknown>, accountKey: string, now: number): string | null {
	const memory = parseRecord(raw[LEGACY_SKELETON_LAYOUT_MEMORY_KEY]);
	if (memory === null) return null;
	const hint = dmRootShellHint(accountKey, now);
	const chrome = isRecord(memory.chrome) ? memory.chrome : {};
	seedGuildRail(hint, chrome.guildRail);
	seedDMSidebar(hint, chrome.dmSidebar);
	seedNagbar(hint, memory.nagbar);
	seedChannelHeader(hint, memory.channelHeader);
	seedFriends(hint, memory.friends);
	seedComposer(hint, memory.composer);
	seedMessagePresentation(hint, memory.messagePresentation);
	seedSidebarWidth(hint, raw[LEGACY_SIDEBAR_WIDTH_KEY]);
	return JSON.stringify(hint);
}
