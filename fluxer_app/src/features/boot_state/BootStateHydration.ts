// SPDX-License-Identifier: AGPL-3.0-or-later

import Config from '@app/features/app/config/Config';
import {beginBootHydration, flushBootHydration, resetBootHydration} from '@app/features/boot_state/BootHydration';
import type {
	BootStatePayload,
	BootStateUnreadView,
	BootStateViewportChannel,
} from '@app/features/boot_state/BootStateCodec';
import {
	forgetRestoredScrolls,
	recordUserAreaOverlayHeight,
	rememberRestoredImageSources,
	rememberRestoredScroll,
} from '@app/features/boot_state/BootViewport';
import {seedRetainedCodeHighlights} from '@app/features/code_highlighting/utils/ArboriumHighlighting';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import '@app/features/code_highlighting/utils/ArboriumStyles';
import {hydrateVisibleSnapshot} from '@app/features/gateway/snapshot/SnapshotHydration';
import SnapshotSync from '@app/features/gateway/snapshot/SnapshotSync';
import MemberSidebar from '@app/features/member/state/MemberSidebar';
import MessageReferences from '@app/features/messaging/state/MessageReferences';
import Drafts from '@app/features/messaging/state/MessagingDrafts';
import Messages, {type SeededMessageWindow} from '@app/features/messaging/state/MessagingMessages';
import {seedRetainedMarkdownParses} from '@app/features/messaging/utils/markdown/MarkdownParseCache';
import ReadStates from '@app/features/read_state/state/ReadStates';
import {isNewerMessageId} from '@app/features/read_state/state/read_states/shared';
import Dimension from '@app/features/ui/state/Dimension';
import {Profile} from '@app/features/user/models/Profile';
import UserProfile from '@app/features/user/state/UserProfile';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {runInAction} from 'mobx';

function parseMessageWindow(
	payload: BootStatePayload,
	window: BootStatePayload['messages'][number],
): SeededMessageWindow {
	return {
		channelId: window.channelId,
		messages: window.messages.map((message) => JSON.parse(message) as WireMessage),
		hasMoreBefore: window.hasMoreBefore,
		hasMoreAfter: window.hasMoreAfter,
		anchorMessageId: payload.viewport.channels[window.channelId]?.anchorMessageId ?? null,
	};
}

const FIRST_COMMIT_ROWS_ABOVE_ANCHOR = 12;
const FIRST_COMMIT_MIN_DEFERRED_ROWS = 20;
const SNOWFLAKE_PATTERN = /\d+/g;
const ROUTE_CHANNEL_PATTERN = /^\/channels\/[^/]+\/([^/]+)/;

interface DeferredOlderMessages {
	readonly channelId: string;
	readonly older: ReadonlyArray<WireMessage>;
}

function routeChannelIds(): ReadonlySet<string> {
	const ids = new Set<string>();
	const channel = ROUTE_CHANNEL_PATTERN.exec(window.location.pathname);
	if (channel !== null) ids.add(channel[1]);
	const thread = /\/threads\/([^/]+)/.exec(window.location.pathname);
	if (thread !== null) ids.add(thread[1]);
	return ids;
}

function readBoundaryIndex(window: SeededMessageWindow): number | null {
	const readState = ReadStates.getIfExists(window.channelId);
	if (readState === undefined || !readState.canBeUnread()) {
		return window.messages.length;
	}
	if (readState.ackMessageId === null) {
		return null;
	}
	const ackMessageId = readState.ackMessageId;
	const firstUnread = window.messages.findIndex((message) => isNewerMessageId(message.id, ackMessageId));
	return firstUnread === -1 ? window.messages.length : firstUnread - 1;
}

function splitFirstCommitWindow(
	window: SeededMessageWindow,
	viewport: BootStateViewportChannel | undefined,
): {readonly firstCommit: SeededMessageWindow; readonly deferred: DeferredOlderMessages | null} {
	const unchanged = {firstCommit: window, deferred: null};
	if (viewport === undefined || window.anchorMessageId === null) {
		return unchanged;
	}
	const anchorIndex = window.messages.findIndex((message) => message.id === window.anchorMessageId);
	const readBoundary = readBoundaryIndex(window);
	const readState = ReadStates.getIfExists(window.channelId);
	const first = window.messages[0];
	const unreadBoundaryBeforeWindow =
		first !== undefined &&
		viewport.unreadBoundary?.oldestUnreadMessageId != null &&
		viewport.unreadBoundary.visualUnreadMessageId != null &&
		isNewerMessageId(first.id, viewport.unreadBoundary.oldestUnreadMessageId) &&
		isNewerMessageId(first.id, viewport.unreadBoundary.visualUnreadMessageId);
	const restoresUnreadView =
		(viewport.atBottom || unreadBoundaryBeforeWindow) &&
		viewport.unread != null &&
		viewport.unread.oldestUnreadMessageId !== null &&
		viewport.unreadBoundary !== undefined &&
		viewport.unreadBoundary.oldestUnreadMessageId === viewport.unread.oldestUnreadMessageId &&
		readState !== undefined &&
		readState.ackMessageId !== null &&
		viewport.unread.ackMessageId === readState.ackMessageId &&
		viewport.unreadBoundary.ackMessageId === readState.ackMessageId;
	if (anchorIndex < 0 || (readBoundary === null && !restoresUnreadView)) {
		return unchanged;
	}
	const cut = restoresUnreadView
		? anchorIndex - FIRST_COMMIT_ROWS_ABOVE_ANCHOR
		: Math.min(anchorIndex - FIRST_COMMIT_ROWS_ABOVE_ANCHOR, readBoundary!);
	if (cut < FIRST_COMMIT_MIN_DEFERRED_ROWS) {
		return unchanged;
	}
	return {
		firstCommit: {...window, messages: window.messages.slice(cut), hasMoreBefore: true},
		deferred: {channelId: window.channelId, older: window.messages.slice(0, cut)},
	};
}

function restoredUnreadViews(
	payload: BootStatePayload,
	channelIds: ReadonlySet<string>,
): Map<string, BootStateUnreadView> {
	const views = new Map<string, BootStateUnreadView>();
	for (const [channelId, viewport] of Object.entries(payload.viewport.channels)) {
		if (viewport.unread !== null && channelIds.has(channelId)) {
			views.set(channelId, viewport.unread);
		}
	}
	return views;
}

function seedViewport(payload: BootStatePayload): void {
	for (const [channelId, viewport] of Object.entries(payload.viewport.channels)) {
		ReadStates.restoreUnreadBoundary(channelId, viewport.unreadBoundary);
		if (viewport.atBottom) {
			rememberRestoredScroll(channelId, {kind: 'bottom'});
		} else if (viewport.anchorMessageId !== null) {
			rememberRestoredScroll(channelId, {
				kind: 'anchor',
				messageId: viewport.anchorMessageId,
				offsetPx: viewport.anchorOffsetPx,
			});
		}
		if (viewport.atBottom) {
			Dimension.recordChannelDimensions(channelId, 1, 1, 0);
		} else if (viewport.scrollTop != null && viewport.scrollHeight != null && viewport.offsetHeight != null) {
			Dimension.recordChannelDimensions(channelId, viewport.scrollTop, viewport.scrollHeight, viewport.offsetHeight);
		}
	}
	Dimension.updateGuildListDimensions(payload.viewport.guildListScrollTop);
	rememberRestoredImageSources(payload.viewport.imageSources);
	if (payload.viewport.rendererBuild === Config.BUILD_VERSION) {
		seedRetainedCodeHighlights(payload.viewport.codeHighlights);
		seedRetainedMarkdownParses(payload.viewport.markdownParses);
	}
	for (const profile of payload.viewport.dmProfiles ?? []) UserProfile.handleProfileCreate(new Profile(profile));
	recordUserAreaOverlayHeight(payload.viewport.userAreaOverlayHeightPx);
}

function referencedSnowflakes(
	payload: BootStatePayload,
	selected: BootStatePayload['messages'],
	visibleMembers: BootStatePayload['members'],
	routeChannels: ReadonlySet<string>,
): Set<string> {
	const sources = [
		...selected.flatMap((window) => window.messages),
		...Array.from(routeChannels, (id) => JSON.stringify([Drafts.getDraft(id), Drafts.getDraftSegments(id)])),
		...Object.values(payload.shell.entries.user_settings ?? {}),
		JSON.stringify(visibleMembers.flatMap((list) => list.rows)),
	];
	const ids = new Set(sources.join(' ').match(SNOWFLAKE_PATTERN) ?? []);
	for (const id of routeChannels) ids.add(id);
	return ids;
}

export async function hydrateBootState(accountKey: string, userId: string, payload: BootStatePayload): Promise<void> {
	resetBootHydration();
	try {
		for (const guildId of ['@me', ...Object.keys(payload.shell.entries.guild ?? {})]) {
			if (payload.guildReadState[guildId] === undefined)
				throw new Error(`Boot state is missing read summary for ${guildId}`);
		}
		if (!SnapshotSync.adoptPromotedSnapshot(accountKey, userId, payload.shell)) {
			throw new Error('The boot state shell snapshot was not adopted');
		}
		const routeChannels = routeChannelIds();
		const routeGuild = window.location.pathname.split('/')[2];
		const guildId = routeGuild === '@me' ? null : routeGuild;
		const pendingWindows = payload.messages.filter((window) => !routeChannels.has(window.channelId));
		const selected = payload.messages.filter((window) => routeChannels.has(window.channelId));
		const visibleMembers = payload.members.filter((list) => routeChannels.has(list.channelId));
		const visibleIds = referencedSnowflakes(payload, selected, visibleMembers, routeChannels);
		let rest: Generator<void>;
		runInAction(() => {
			GuildReadState.restoreBootState(payload.guildReadState);
			rest = hydrateVisibleSnapshot(
				accountKey,
				userId,
				payload.shell.entries,
				new Map(Object.entries(payload.mentions)),
				guildId,
				visibleIds,
			);
			for (const selectedWindow of selected) {
				const window = parseMessageWindow(payload, selectedWindow);
				const split = splitFirstCommitWindow(window, payload.viewport.channels[window.channelId]);
				const seeded = Messages.seedFromBootState([split.firstCommit]);
				const seededWindow = seeded.get(window.channelId);
				const tailHeightPx = payload.viewport.channels[window.channelId]?.tailHeightPx;
				if (seededWindow !== undefined && window.hasMoreAfter && tailHeightPx !== undefined) {
					Messages.reserveRestoredTail(window.channelId, tailHeightPx);
				}
				if (split.deferred !== null && seededWindow !== undefined) {
					const tops = payload.viewport.channels[window.channelId]?.messageTopsPx;
					Messages.deferSeededOlderMessages(
						seededWindow,
						split.deferred.older,
						window.hasMoreBefore,
						tops === undefined ? null : new Map(Object.entries(tops)),
					);
				}
				MessageReferences.handleMessagesFetchSuccess(window.channelId, [...window.messages]);
				ReadStates.handleRestoredMessages([window.channelId]);
			}
			ReadStates.restoreUnreadViews(restoredUnreadViews(payload, routeChannels));
			MemberSidebar.seedFromBootState(visibleMembers);
			seedViewport(payload);
		});
		function* hydrateRest(): Generator<void> {
			yield* rest;
			for (const pending of pendingWindows) {
				const window = parseMessageWindow(payload, pending);
				Messages.seedFromBootState([window]);
				MessageReferences.handleMessagesFetchSuccess(window.channelId, [...window.messages]);
				ReadStates.handleRestoredMessages([window.channelId]);
				yield;
			}
			MemberSidebar.seedFromBootState(payload.members.filter((list) => !routeChannels.has(list.channelId)));
			ReadStates.restoreUnreadViews(
				restoredUnreadViews(
					payload,
					new Set(Object.keys(payload.viewport.channels).filter((id) => !routeChannels.has(id))),
				),
			);
			for (const guildId of Object.keys(payload.guildReadState)) {
				GuildReadState.completeBootStateGuild(guildId === '@me' ? null : guildId);
				yield;
			}
		}
		beginBootHydration(hydrateRest(), GuildReadState.discardBootState);
		if (!/^\/channels\/(?:@me|\d+)(?:\/|$)/.test(window.location.pathname)) flushBootHydration();
	} catch (error) {
		GuildReadState.discardBootState();
		runInAction(() => {
			Messages.handleSessionInvalidated();
			MessageReferences.reset();
			MemberSidebar.handleSessionInvalidated();
			Dimension.reset();
		});
		forgetRestoredScrolls();
		throw error;
	}
}

export function discardBootStateSnapshot(accountKey: string): void {
	resetBootHydration();
	SnapshotSync.evict(accountKey);
}
