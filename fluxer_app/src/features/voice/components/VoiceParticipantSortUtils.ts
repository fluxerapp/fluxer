// SPDX-License-Identifier: AGPL-3.0-or-later

import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {parseVoiceParticipantIdentity} from '@app/features/voice/utils/VoiceParticipantIdentity';
import type {TrackReferenceOrPlaceholder} from '@livekit/components-react';

interface VoiceParticipantSortIdentity {
	userId: string;
	connectionId: string;
}

interface VoiceParticipantSortSnapshotEntry {
	displayName: string;
	joinOrder: number;
	hasResolvedDisplayName: boolean;
}

export interface VoiceParticipantSortSnapshot {
	entries: Map<string, VoiceParticipantSortSnapshotEntry>;
	nextJoinOrder: number;
}

export interface VoiceParticipantSnapshotMember {
	participantKey: string;
	userId: string;
}

const DISPLAY_NAME_COLLATOR = new Intl.Collator(undefined, {
	numeric: true,
	sensitivity: 'base',
	usage: 'sort',
});

function buildSortIdentity(identity: string): VoiceParticipantSortIdentity {
	const parsed = parseVoiceParticipantIdentity(identity);
	return {
		userId: parsed.userId,
		connectionId: parsed.connectionId,
	};
}

function getUserDisplayName(userId: string, guildId?: string | null, channelId?: string | null): string {
	if (!userId) return '';
	const user = Users.getUser(userId);
	if (!user) return '';
	return NicknameUtils.getNickname(user, guildId ?? null, channelId ?? undefined) || '';
}

function buildSnapshotFallbackDisplayName(userId: string, participantKey: string): string {
	return `~${userId || participantKey}`;
}

function getIdentityParticipantKey(identity: VoiceParticipantSortIdentity): string {
	return `${identity.userId}:${identity.connectionId}`;
}

function getOrCreateVoiceParticipantSortSnapshotEntry(
	member: VoiceParticipantSnapshotMember,
	snapshot: VoiceParticipantSortSnapshot,
	guildId?: string | null,
	channelId?: string | null,
): VoiceParticipantSortSnapshotEntry {
	const existing = snapshot.entries.get(member.participantKey);
	const resolvedDisplayName = getUserDisplayName(member.userId, guildId, channelId);
	if (existing) {
		if (!existing.hasResolvedDisplayName && resolvedDisplayName) {
			existing.displayName = resolvedDisplayName;
			existing.hasResolvedDisplayName = true;
		}
		return existing;
	}
	const hasResolvedDisplayName = resolvedDisplayName.length > 0;
	const entry: VoiceParticipantSortSnapshotEntry = {
		displayName: hasResolvedDisplayName
			? resolvedDisplayName
			: buildSnapshotFallbackDisplayName(member.userId, member.participantKey),
		joinOrder: snapshot.nextJoinOrder,
		hasResolvedDisplayName,
	};
	snapshot.nextJoinOrder += 1;
	snapshot.entries.set(member.participantKey, entry);
	return entry;
}

function compareVoiceParticipantSortSnapshotEntries(
	leftEntry: VoiceParticipantSortSnapshotEntry,
	rightEntry: VoiceParticipantSortSnapshotEntry,
	leftMember: VoiceParticipantSnapshotMember,
	rightMember: VoiceParticipantSnapshotMember,
): number {
	const byDisplayName = DISPLAY_NAME_COLLATOR.compare(leftEntry.displayName, rightEntry.displayName);
	if (byDisplayName !== 0) return byDisplayName;
	const byJoinOrder = leftEntry.joinOrder - rightEntry.joinOrder;
	if (byJoinOrder !== 0) return byJoinOrder;
	const byUserId = leftMember.userId.localeCompare(rightMember.userId);
	if (byUserId !== 0) return byUserId;
	return leftMember.participantKey.localeCompare(rightMember.participantKey);
}

export function createVoiceParticipantSortSnapshot(): VoiceParticipantSortSnapshot {
	return {
		entries: new Map(),
		nextJoinOrder: 0,
	};
}

export function syncVoiceParticipantSortSnapshot(
	snapshot: VoiceParticipantSortSnapshot,
	members: ReadonlyArray<VoiceParticipantSnapshotMember>,
	guildId?: string | null,
	channelId?: string | null,
): void {
	const uniqueMembers = new Map<string, VoiceParticipantSnapshotMember>();
	for (const member of members) {
		if (!member.participantKey) continue;
		if (uniqueMembers.has(member.participantKey)) continue;
		uniqueMembers.set(member.participantKey, member);
	}
	const activeKeys = new Set(uniqueMembers.keys());
	for (const existingKey of snapshot.entries.keys()) {
		if (activeKeys.has(existingKey)) continue;
		snapshot.entries.delete(existingKey);
	}
	for (const member of uniqueMembers.values()) {
		getOrCreateVoiceParticipantSortSnapshotEntry(member, snapshot, guildId, channelId);
	}
}

export function compareVoiceParticipantSnapshotMembers(
	leftMember: VoiceParticipantSnapshotMember,
	rightMember: VoiceParticipantSnapshotMember,
	snapshot: VoiceParticipantSortSnapshot,
	guildId?: string | null,
	channelId?: string | null,
): number {
	const leftEntry = getOrCreateVoiceParticipantSortSnapshotEntry(leftMember, snapshot, guildId, channelId);
	const rightEntry = getOrCreateVoiceParticipantSortSnapshotEntry(rightMember, snapshot, guildId, channelId);
	return compareVoiceParticipantSortSnapshotEntries(leftEntry, rightEntry, leftMember, rightMember);
}

interface SortVoiceParticipantItemsWithSnapshotArgs<T> {
	snapshot: VoiceParticipantSortSnapshot;
	getParticipantKey: (item: T) => string;
	getUserId: (item: T) => string;
	guildId?: string | null;
	channelId?: string | null;
	getTieBreaker?: (item: T) => string;
}

interface SortableVoiceParticipantItem<T> {
	item: T;
	member: VoiceParticipantSnapshotMember;
	tieBreaker: string;
}

function syncVoiceParticipantSortSnapshotFromSortableItems<T>(
	snapshot: VoiceParticipantSortSnapshot,
	items: ReadonlyArray<SortableVoiceParticipantItem<T>>,
	guildId?: string | null,
	channelId?: string | null,
): void {
	const uniqueMembers = new Map<string, VoiceParticipantSnapshotMember>();
	for (const item of items) {
		const member = item.member;
		if (!member.participantKey) continue;
		if (uniqueMembers.has(member.participantKey)) continue;
		uniqueMembers.set(member.participantKey, member);
	}
	const activeKeys = new Set(uniqueMembers.keys());
	for (const existingKey of snapshot.entries.keys()) {
		if (activeKeys.has(existingKey)) continue;
		snapshot.entries.delete(existingKey);
	}
	for (const member of uniqueMembers.values()) {
		getOrCreateVoiceParticipantSortSnapshotEntry(member, snapshot, guildId, channelId);
	}
}

export function sortVoiceParticipantItemsWithSnapshot<T>(
	items: ReadonlyArray<T>,
	{
		snapshot,
		getParticipantKey,
		getUserId,
		guildId = null,
		channelId = null,
		getTieBreaker,
	}: SortVoiceParticipantItemsWithSnapshotArgs<T>,
): Array<T> {
	const sortableItems = new Array<SortableVoiceParticipantItem<T>>(items.length);
	for (let i = 0; i < items.length; i += 1) {
		const item = items[i];
		sortableItems[i] = {
			item,
			member: {
				participantKey: getParticipantKey(item),
				userId: getUserId(item),
			},
			tieBreaker: getTieBreaker ? getTieBreaker(item) : '',
		};
	}
	syncVoiceParticipantSortSnapshotFromSortableItems(snapshot, sortableItems, guildId, channelId);
	sortableItems.sort((left, right) => {
		const bySnapshot = compareVoiceParticipantSnapshotMembers(left.member, right.member, snapshot, guildId, channelId);
		if (bySnapshot !== 0) return bySnapshot;
		if (getTieBreaker) {
			const byTieBreaker = left.tieBreaker.localeCompare(right.tieBreaker);
			if (byTieBreaker !== 0) return byTieBreaker;
		}
		return 0;
	});
	const sortedItems = new Array<T>(sortableItems.length);
	for (let i = 0; i < sortableItems.length; i += 1) {
		sortedItems[i] = sortableItems[i].item;
	}
	return sortedItems;
}

export function compareVoiceParticipantIdentities(
	leftIdentity: string,
	rightIdentity: string,
	guildId?: string | null,
	channelId?: string | null,
): number {
	const left = buildSortIdentity(leftIdentity);
	const right = buildSortIdentity(rightIdentity);
	const leftDisplayName = getUserDisplayName(left.userId, guildId, channelId);
	const rightDisplayName = getUserDisplayName(right.userId, guildId, channelId);
	const byDisplayName = DISPLAY_NAME_COLLATOR.compare(leftDisplayName, rightDisplayName);
	if (byDisplayName !== 0) return byDisplayName;
	const byUserId = left.userId.localeCompare(right.userId);
	if (byUserId !== 0) return byUserId;
	const byConnectionId = left.connectionId.localeCompare(right.connectionId);
	if (byConnectionId !== 0) return byConnectionId;
	return leftIdentity.localeCompare(rightIdentity);
}

export function compareVoiceParticipantsByUserAndConnection(
	left: VoiceParticipantSortIdentity,
	right: VoiceParticipantSortIdentity,
	guildId?: string | null,
	channelId?: string | null,
): number {
	const leftDisplayName = getUserDisplayName(left.userId, guildId, channelId);
	const rightDisplayName = getUserDisplayName(right.userId, guildId, channelId);
	const byDisplayName = DISPLAY_NAME_COLLATOR.compare(leftDisplayName, rightDisplayName);
	if (byDisplayName !== 0) return byDisplayName;
	const byUserId = left.userId.localeCompare(right.userId);
	if (byUserId !== 0) return byUserId;
	return left.connectionId.localeCompare(right.connectionId);
}

export function compareVoiceParticipantsByUserAndConnectionWithSnapshot(
	left: VoiceParticipantSortIdentity,
	right: VoiceParticipantSortIdentity,
	snapshot: VoiceParticipantSortSnapshot,
	guildId?: string | null,
	channelId?: string | null,
): number {
	return compareVoiceParticipantSnapshotMembers(
		{
			participantKey: getIdentityParticipantKey(left),
			userId: left.userId,
		},
		{
			participantKey: getIdentityParticipantKey(right),
			userId: right.userId,
		},
		snapshot,
		guildId,
		channelId,
	);
}

export function compareVoiceTrackReferences(
	left: TrackReferenceOrPlaceholder,
	right: TrackReferenceOrPlaceholder,
	guildId?: string | null,
	channelId?: string | null,
): number {
	const byParticipant = compareVoiceParticipantIdentities(
		left.participant.identity,
		right.participant.identity,
		guildId,
		channelId,
	);
	if (byParticipant !== 0) return byParticipant;
	return `${left.source}`.localeCompare(`${right.source}`);
}

export function compareVoiceTrackReferencesWithSnapshot(
	left: TrackReferenceOrPlaceholder,
	right: TrackReferenceOrPlaceholder,
	snapshot: VoiceParticipantSortSnapshot,
	guildId?: string | null,
	channelId?: string | null,
): number {
	const leftIdentity = buildSortIdentity(left.participant.identity);
	const rightIdentity = buildSortIdentity(right.participant.identity);
	const byParticipant = compareVoiceParticipantsByUserAndConnectionWithSnapshot(
		leftIdentity,
		rightIdentity,
		snapshot,
		guildId,
		channelId,
	);
	if (byParticipant !== 0) return byParticipant;
	return `${left.source}`.localeCompare(`${right.source}`);
}
