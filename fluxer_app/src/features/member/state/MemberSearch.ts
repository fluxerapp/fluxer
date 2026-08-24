// SPDX-License-Identifier: AGPL-3.0-or-later

import Guilds from '@app/features/guild/state/Guilds';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Relationships from '@app/features/relationship/state/Relationships';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {makeAutoObservable} from 'mobx';

export enum MemberSearchActionTypes {
	INGEST_DIRECTORY = 'INGEST_DIRECTORY',
	MATCHES_READY = 'MATCHES_READY',
	SEARCH_BEGIN = 'SEARCH_BEGIN',
	SEARCH_CANCEL = 'SEARCH_CANCEL',
}

export enum MemberSearchWorkerMessageTypes {
	INGEST_DIRECTORY = 'INGEST_DIRECTORY',
	MATCHES_READY = 'MATCHES_READY',
	SEARCH_BEGIN = 'SEARCH_BEGIN',
	SEARCH_CANCEL = 'SEARCH_CANCEL',
}

export interface MemberSearchFilters {
	friends?: boolean;
	guild?: string;
}

export interface TransformedMember {
	id: string;
	username: string;
	globalName?: string | null;
	guildNicknames?: Record<string, string | null>;
	isBot?: boolean;
	isFriend?: boolean;
	guildIds?: Array<string>;
	_delete?: boolean;
	_removeGuild?: string;
	[key: string]: string | boolean | null | undefined | Array<string> | Record<string, string | null>;
}

export type QueryBlacklist = Set<string>;
export type QueryWhitelist = Set<string>;
export type QueryBoosters = Record<string, number>;

interface QueryData {
	query: string;
	filters?: MemberSearchFilters;
	blacklist: Array<string>;
	whitelist: Array<string>;
	boosters: QueryBoosters;
	limit: number;
	generation: number;
}

interface WorkerMessage {
	type: MemberSearchWorkerMessageTypes;
	payload?: unknown;
	uuid?: string;
}

interface MemberResultsMessage extends WorkerMessage {
	type: MemberSearchWorkerMessageTypes.MATCHES_READY;
	uuid: string;
	generation: number;
	payload: Array<TransformedMember>;
}

interface UpdateMembersMessage extends WorkerMessage {
	type: MemberSearchWorkerMessageTypes.INGEST_DIRECTORY;
	payload: {
		users: Array<TransformedMember>;
	};
}

interface QuerySetMessage extends WorkerMessage {
	type: MemberSearchWorkerMessageTypes.SEARCH_BEGIN;
	uuid: string;
	payload: QueryData;
}

interface QueryClearMessage extends WorkerMessage {
	type: MemberSearchWorkerMessageTypes.SEARCH_CANCEL;
	uuid: string;
}

const DEFAULT_LIMIT = 10;
const MEMBER_FETCH_DEDUPE_MS = 60_000;
const MEMBER_FETCH_SETTLE_MS = 750;

let worker: Worker | null = null;
const searchContexts = new Set<SearchContext>();

function updateMembers(members: Array<TransformedMember>): void {
	if (!worker) {
		return;
	}
	const filtered = members.filter((member) => member != null);
	if (filtered.length === 0) {
		return;
	}
	worker.postMessage({
		type: MemberSearchWorkerMessageTypes.INGEST_DIRECTORY,
		payload: {users: filtered},
	} as UpdateMembersMessage);
}

function isFriendRelationship(userId: string): boolean {
	const relationship = Relationships.getRelationship(userId);
	if (relationship == null) {
		return false;
	}
	return relationship.type === RelationshipTypes.FRIEND;
}

function getTransformedUser(user: User): TransformedMember {
	return {
		id: user.id,
		username: `${user.username}#${user.discriminator}`,
		globalName: user.globalName,
		isBot: user.bot,
		isFriend: isFriendRelationship(user.id),
		guildIds: [],
		guildNicknames: {},
	};
}

function getTransformedMember(memberRecord: GuildMember, guildId?: string): TransformedMember | null {
	const user = memberRecord.user;
	const member = getTransformedUser(user);
	if (guildId == null || guildId.length === 0) {
		return member;
	}
	member[guildId] = true;
	member.guildIds = [guildId];
	member.guildNicknames = {[guildId]: memberRecord.nick};
	return member;
}

function getMemberRemoval(memberId: string, guildId: string): TransformedMember {
	return {
		id: memberId,
		username: '',
		_removeGuild: guildId,
	};
}

function updateMembersList(members: Array<GuildMember>, guildId?: string): Array<TransformedMember> {
	const transformedMembers: Array<TransformedMember> = [];
	for (const memberRecord of members) {
		const member = getTransformedMember(memberRecord, guildId);
		if (member) {
			transformedMembers.push(member);
		}
	}
	return transformedMembers;
}

export class SearchContext {
	private readonly _contextId: string;
	private readonly _deliverResults: (results: Array<TransformedMember>) => void;
	private readonly _maxResults: number;
	private _inFlightQuery: QueryData | false | null;
	private _queuedQuery: QueryData | null;
	private _latestGeneration: number;
	private _nextGeneration: number;
	private readonly _handleMessages: (event: MessageEvent<WorkerMessage>) => void;
	private _attachedWorker: Worker | null = null;

	constructor(callback: (results: Array<TransformedMember>) => void, limit: number = DEFAULT_LIMIT) {
		this._contextId = crypto.randomUUID();
		this._deliverResults = callback;
		this._maxResults = limit;
		this._inFlightQuery = null;
		this._queuedQuery = null;
		this._latestGeneration = 0;
		this._nextGeneration = 1;
		this._handleMessages = (event: MessageEvent<WorkerMessage>) => {
			const data = event.data;
			if (!data || data.type !== MemberSearchWorkerMessageTypes.MATCHES_READY) {
				return;
			}
			const resultsMessage = data as MemberResultsMessage;
			if (resultsMessage.uuid !== this._contextId) {
				return;
			}
			const isLatestGeneration = resultsMessage.generation === this._latestGeneration;
			if (isLatestGeneration && this._inFlightQuery !== false) {
				this._deliverResults(resultsMessage.payload);
			}
			const currentQuery = this._inFlightQuery;
			if (currentQuery !== null && currentQuery !== false && currentQuery.generation === resultsMessage.generation) {
				this._inFlightQuery = null;
				this._flushQueuedQuery();
			}
		};
		searchContexts.add(this);
		this.attachToWorker(worker);
	}

	destroy(): void {
		this.cancelSearch();
		searchContexts.delete(this);
		this.attachToWorker(null);
	}

	attachToWorker(nextWorker: Worker | null): void {
		if (this._attachedWorker === nextWorker) {
			return;
		}
		if (this._attachedWorker != null) {
			this._attachedWorker.removeEventListener('message', this._handleMessages);
		}
		this._attachedWorker = nextWorker;
		if (nextWorker == null) {
			return;
		}
		nextWorker.addEventListener('message', this._handleMessages);
		if (this._inFlightQuery === false) {
			nextWorker.postMessage({
				uuid: this._contextId,
				type: MemberSearchWorkerMessageTypes.SEARCH_CANCEL,
			} as QueryClearMessage);
			return;
		}
		if (this._inFlightQuery != null) {
			nextWorker.postMessage({
				uuid: this._contextId,
				type: MemberSearchWorkerMessageTypes.SEARCH_BEGIN,
				payload: this._inFlightQuery,
			} as QuerySetMessage);
			return;
		}
		this._flushQueuedQuery();
	}

	cancelSearch(): void {
		this._inFlightQuery = false;
		this._queuedQuery = null;
		if (this._attachedWorker != null) {
			this._attachedWorker.postMessage({
				uuid: this._contextId,
				type: MemberSearchWorkerMessageTypes.SEARCH_CANCEL,
			} as QueryClearMessage);
		}
	}

	beginSearch(
		query: string,
		filters: MemberSearchFilters = {},
		blacklist: QueryBlacklist = new Set(),
		whitelist: QueryWhitelist = new Set(),
		boosters: QueryBoosters = {},
	): void {
		if (query == null) {
			return;
		}
		const generation = this._nextGeneration++;
		this._latestGeneration = generation;
		this._queuedQuery = {
			query,
			filters,
			blacklist: Array.from(blacklist),
			whitelist: Array.from(whitelist),
			boosters,
			limit: this._maxResults,
			generation,
		};
		this._flushQueuedQuery();
	}

	private _flushQueuedQuery(): void {
		if (this._inFlightQuery || !this._queuedQuery) {
			return;
		}
		this._inFlightQuery = this._queuedQuery;
		this._queuedQuery = null;
		if (this._attachedWorker != null) {
			this._attachedWorker.postMessage({
				uuid: this._contextId,
				type: MemberSearchWorkerMessageTypes.SEARCH_BEGIN,
				payload: this._inFlightQuery,
			} as QuerySetMessage);
		}
	}
}

class MemberSearch {
	private logger = new Logger('MemberSearch');
	private initialized: boolean = false;
	private readonly recentFetches = new Map<string, number>();

	constructor() {
		makeAutoObservable(this);
	}

	initialize(): void {
		if (this.initialized || worker) {
			return;
		}
		this.initialized = true;
		try {
			worker = new Worker(
				new URL(/* webpackChunkName: "member-search.worker" */ '../workers/MemberSearchWorker.ts', import.meta.url),
				{
					type: 'module',
				},
			);
			this.sendInitialMembers();
			for (const context of searchContexts) {
				context.attachToWorker(worker);
			}
		} catch (err) {
			this.initialized = false;
			this.logger.error('Failed to initialize worker:', err);
		}
	}

	private sendInitialMembers(): void {
		if (!worker) {
			return;
		}
		const allMembers: Array<TransformedMember> = [];
		for (const user of Users.usersList) {
			allMembers.push(getTransformedUser(user));
		}
		const guilds = Guilds.getGuilds();
		for (const guild of guilds) {
			const members = GuildMembers.getMembers(guild.id);
			const transformedMembers = updateMembersList(members, guild.id);
			allMembers.push(...transformedMembers);
		}
		updateMembers(allMembers);
	}

	handleGatewayReady(): void {
		if (worker) {
			this.terminate();
		}
		this.initialize();
	}

	handleLogout(): void {
		this.terminate();
		this.initialized = false;
	}

	handleGuildCreate(guildId: string): void {
		if (!worker) return;
		const members = GuildMembers.getMembers(guildId);
		const transformedMembers = updateMembersList(members, guildId);
		updateMembers(transformedMembers);
	}

	handleGuildDelete(guildId: string): void {
		if (!worker) return;
		const members = GuildMembers.getMembers(guildId);
		const removals: Array<TransformedMember> = [];
		for (const member of members) {
			removals.push(getMemberRemoval(member.user.id, guildId));
		}
		updateMembers(removals);
	}

	handleMemberAdd(guildId: string, memberId: string): void {
		if (!worker) return;
		const member = GuildMembers.getMember(guildId, memberId);
		if (!member) return;
		const transformedMember = getTransformedMember(member, guildId);
		if (transformedMember) {
			updateMembers([transformedMember]);
		}
	}

	handleMemberUpdate(guildId: string, memberId: string): void {
		if (!worker) return;
		const member = GuildMembers.getMember(guildId, memberId);
		if (!member) return;
		const transformedMember = getTransformedMember(member, guildId);
		if (transformedMember) {
			updateMembers([transformedMember]);
		}
	}

	handleMemberRemove(guildId: string, memberId: string): void {
		if (!worker) return;
		const member = GuildMembers.getMember(guildId, memberId);
		if (member != null) {
			const transformedMember = getTransformedMember(member, guildId);
			if (transformedMember != null) {
				transformedMember._removeGuild = guildId;
				updateMembers([transformedMember]);
			}
			return;
		}
		updateMembers([getMemberRemoval(memberId, guildId)]);
	}

	handleMembersChunk(guildId: string, members: Array<GuildMember>): void {
		if (!worker) return;
		const transformedMembers = updateMembersList(members, guildId);
		updateMembers(transformedMembers);
	}

	handleUserUpdate(userId: string): void {
		if (!worker) return;
		const user = Users.getUser(userId);
		if (user == null) return;
		const allMembers: Array<TransformedMember> = [getTransformedUser(user)];
		const guilds = Guilds.getGuilds();
		for (const guild of guilds) {
			const member = GuildMembers.getMember(guild.id, userId);
			if (member) {
				const transformedMember = getTransformedMember(member, guild.id);
				if (transformedMember) {
					allMembers.push(transformedMember);
				}
			}
		}
		updateMembers(allMembers);
	}

	handleFriendshipChange(userId: string, isFriend: boolean): void {
		if (!worker) return;
		const user = Users.getUser(userId);
		if (!user) return;
		const transformedUser = getTransformedUser(user);
		transformedUser.isFriend = isFriend;
		updateMembers([transformedUser]);
	}

	getSearchContext(
		callback: (results: Array<TransformedMember>) => void,
		limit: number = DEFAULT_LIMIT,
	): SearchContext {
		if (!worker) {
			this.initialize();
		}
		return new SearchContext(callback, limit);
	}

	private terminate(): void {
		if (worker) {
			for (const context of searchContexts) {
				context.attachToWorker(null);
			}
			worker.terminate();
			worker = null;
		}
		this.initialized = false;
	}

	cleanup(): void {
		this.terminate();
		this.initialized = false;
		this.recentFetches.clear();
	}

	async fetchMembersInBackground(query: string, guildIds: Array<string>, priorityGuildId?: string): Promise<void> {
		const trimmed = query.trim();
		if (!trimmed) {
			return;
		}
		if (!guildIds || guildIds.length === 0) {
			return;
		}
		const sortedGuildIds = priorityGuildId
			? [...guildIds].sort((a, b) => (a === priorityGuildId ? -1 : b === priorityGuildId ? 1 : 0))
			: guildIds;
		const eligibleGuildIds = sortedGuildIds.filter((guildId) => {
			if (!guildId) {
				return false;
			}
			const guild = Guilds.getGuild(guildId);
			if (!guild) {
				return false;
			}
			return true;
		});
		if (eligibleGuildIds.length === 0) {
			return;
		}
		const key = `${eligibleGuildIds.join(',')}:${trimmed.toLowerCase()}`;
		const now = Date.now();
		for (const [previousKey, requestedAt] of this.recentFetches) {
			if (now - requestedAt >= MEMBER_FETCH_DEDUPE_MS) {
				this.recentFetches.delete(previousKey);
			}
		}
		if (this.recentFetches.has(key)) {
			return;
		}
		this.recentFetches.set(key, now);
		GuildMembers.requestMembersInBackground({
			guildIds: eligibleGuildIds,
			query: trimmed,
			limit: 25,
			presences: true,
		});
		await new Promise<void>((resolve) => {
			setTimeout(resolve, MEMBER_FETCH_SETTLE_MS);
		});
	}
}

export default new MemberSearch();
