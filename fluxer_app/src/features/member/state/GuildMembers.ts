// SPDX-License-Identifier: AGPL-3.0-or-later

import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import {GuildMember} from '@app/features/member/models/GuildMember';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Users from '@app/features/user/state/Users';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import {makeAutoObservable} from 'mobx';

type Members = Record<string, GuildMember>;

interface PendingMemberRequest {
	guildId: string;
	resolve: (members: Array<GuildMember>) => void;
	reject: (error: Error) => void;
	members: Array<GuildMember>;
	receivedChunks: number;
	requestedUserIds?: Set<string>;
	markMissingAsNonMembers: boolean;
	timeoutId: ReturnType<typeof setTimeout>;
}

const MEMBER_REQUEST_TIMEOUT = 30000;
const MEMBER_NONCE_LENGTH = 32;
const MEMBER_NONCE_CHARS = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
const MAX_USER_IDS_PER_REQUEST = 100;
const logger = new Logger('GuildMembers');

function generateMemberNonce(): string {
	let nonce = '';
	const charsLength = MEMBER_NONCE_CHARS.length;
	for (let i = 0; i < MEMBER_NONCE_LENGTH; i += 1) {
		nonce += MEMBER_NONCE_CHARS[Math.floor(Math.random() * charsLength)];
	}
	return nonce;
}

function addVoiceStateMembers(guild: GuildReadyData, members: Members): void {
	if (!guild.voice_states) {
		return;
	}
	const voiceStateMembers: Array<GuildMemberData> = [];
	for (const voiceState of guild.voice_states) {
		if (!voiceState.member) {
			continue;
		}
		voiceStateMembers.push(voiceState.member);
	}
	cacheGuildMemberUsers(voiceStateMembers);
	for (const member of voiceStateMembers) {
		members[member.user.id] = new GuildMember(guild.id, member, {cacheUser: false});
	}
}

function cacheGuildMemberUsers(members: ReadonlyArray<GuildMemberData>): void {
	if (members.length === 0) {
		return;
	}
	Users.cacheUsers(members.map((member) => member.user));
}

function getMissingVoiceStateMemberUserIds(guild: GuildReadyData, members: Members): Array<string> {
	if (!guild.voice_states || guild.voice_states.length === 0) {
		return [];
	}
	const missingUserIds = new Set<string>();
	for (const voiceState of guild.voice_states) {
		if (voiceState.member) {
			continue;
		}
		if (members[voiceState.user_id]) {
			continue;
		}
		missingUserIds.add(voiceState.user_id);
	}
	return Array.from(missingUserIds);
}

class GuildMembers {
	members: Record<string, Members> = {};
	nonMembers: Record<string, Set<string>> = {};
	pendingRequests: Map<string, PendingMemberRequest> = new Map();
	loadedGuilds: Set<string> = new Set();
	private pendingMessageMemberHydration: Map<string, Set<string>> = new Map();
	private inFlightMessageMembers: Map<string, Set<string>> = new Map();
	private messageMemberRequestGeneration: number = 0;

	constructor() {
		makeAutoObservable<
			this,
			'pendingMessageMemberHydration' | 'inFlightMessageMembers' | 'messageMemberRequestGeneration'
		>(
			this,
			{
				pendingMessageMemberHydration: false,
				inFlightMessageMembers: false,
				messageMemberRequestGeneration: false,
			},
			{autoBind: true},
		);
	}

	getMember(guildId: string, userId?: string | null): GuildMember | null {
		if (!userId) {
			return null;
		}
		return this.members[guildId]?.[userId] ?? null;
	}

	isUserTimedOut(guildId: string | null, userId?: string | null): boolean {
		return this.getCommunicationDisabledUntil(guildId, userId) !== null;
	}

	getCommunicationDisabledUntil(guildId: string | null, userId?: string | null): Date | null {
		if (!guildId || !userId) {
			return null;
		}
		const until = this.members[guildId]?.[userId]?.communicationDisabledUntil ?? null;
		if (!until || until.getTime() <= Date.now()) {
			return null;
		}
		return until;
	}

	getMembers(guildId: string): Array<GuildMember> {
		return Object.values(this.members[guildId] ?? {});
	}

	getMemberCount(guildId: string): number {
		return Object.keys(this.members[guildId] ?? {}).length;
	}

	handleGatewayReady(guilds: Array<GuildReadyData>): void {
		this.members = {};
		this.nonMembers = {};
		this.loadedGuilds.clear();
		this.resetMessageMemberRequests();
		const availableGuildIds = new Set(guilds.map((guild) => guild.id));
		for (const guildId of Array.from(this.pendingMessageMemberHydration.keys())) {
			if (!availableGuildIds.has(guildId)) {
				this.pendingMessageMemberHydration.delete(guildId);
			}
		}
		for (const guild of guilds) {
			this.handleGuildCreate(guild);
		}
	}

	handleGuildCreate(guild: GuildReadyData, options?: {synced?: boolean}): void {
		if (guild.unavailable) {
			return;
		}
		if (!this.members[guild.id]) {
			this.members[guild.id] = {};
		}
		const members = this.members[guild.id];
		cacheGuildMemberUsers(guild.members);
		for (const member of guild.members) {
			members[member.user.id] = new GuildMember(guild.id, member, {cacheUser: false});
		}
		addVoiceStateMembers(guild, members);
		const missingVoiceStateMemberUserIds = getMissingVoiceStateMemberUserIds(guild, members);
		if (missingVoiceStateMemberUserIds.length > 0 && GatewayConnection.socket) {
			void this.ensureMembersLoaded(guild.id, missingVoiceStateMemberUserIds).catch((error: unknown) => {
				logger.warn('Failed to fetch missing voice members after guild create', {
					guildId: guild.id,
					userIds: missingVoiceStateMemberUserIds,
					error,
				});
			});
		}
		if (options?.synced || GatewayConnection.hasCompletedGuildSync(guild.id)) {
			this.loadedGuilds.add(guild.id);
			void this.requestPendingMessageMembers(guild.id);
		}
	}

	handleGuildDelete(guildId: string): void {
		delete this.members[guildId];
		delete this.nonMembers[guildId];
		this.pendingMessageMemberHydration.delete(guildId);
		this.inFlightMessageMembers.delete(guildId);
		this.loadedGuilds.delete(guildId);
	}

	handleMemberAdd(guildId: string, member: GuildMemberData): void {
		if (!this.members[guildId]) {
			this.members[guildId] = {};
		}
		this.members[guildId][member.user.id] = new GuildMember(guildId, member);
		this.nonMembers[guildId]?.delete(member.user.id);
	}

	hydrateIfMissing(guildId: string, member: GuildMemberData): void {
		if (this.members[guildId]?.[member.user.id]) {
			return;
		}
		if (!this.members[guildId]) {
			this.members[guildId] = {};
		}
		this.members[guildId][member.user.id] = new GuildMember(guildId, member);
		this.nonMembers[guildId]?.delete(member.user.id);
	}

	handleMemberRemove(guildId: string, userId: string): void {
		const existingMembers = this.members[guildId];
		if (!existingMembers) {
			return;
		}
		delete existingMembers[userId];
		if (Object.keys(existingMembers).length === 0) {
			delete this.members[guildId];
		}
	}

	handleGuildRoleDelete(guildId: string, roleId: string): void {
		const existingMembers = this.members[guildId];
		if (!existingMembers) {
			return;
		}
		for (const memberId of Object.keys(existingMembers)) {
			const member = existingMembers[memberId];
			if (member.roles.has(roleId)) {
				const newRoles = new Set(member.roles);
				newRoles.delete(roleId);
				existingMembers[memberId] = new GuildMember(guildId, {
					...member.toJSON(),
					roles: Array.from(newRoles),
				});
			}
		}
	}

	handleMembersChunk(params: {
		guildId: string;
		members: Array<GuildMemberData>;
		chunkIndex: number;
		chunkCount: number;
		nonce?: string;
	}): void {
		const {guildId, members, chunkCount, nonce} = params;
		const newMembers: Array<GuildMember> = [];
		if (!this.members[guildId]) {
			this.members[guildId] = {};
		}
		const guildMembers = this.members[guildId];
		const negativeCache = this.nonMembers[guildId];
		for (const member of members) {
			const record = new GuildMember(guildId, member);
			newMembers.push(record);
			guildMembers[member.user.id] = record;
			negativeCache?.delete(member.user.id);
		}
		if (nonce) {
			const pending = this.pendingRequests.get(nonce);
			if (pending) {
				pending.members.push(...newMembers);
				pending.receivedChunks++;
				if (pending.receivedChunks >= chunkCount) {
					clearTimeout(pending.timeoutId);
					this.markNotFoundAsNonMembers(pending);
					pending.resolve(pending.members);
					this.pendingRequests.delete(nonce);
				}
			}
		}
	}

	private markNotFoundAsNonMembers(pending: PendingMemberRequest): void {
		if (!pending.markMissingAsNonMembers) {
			return;
		}
		const requested = pending.requestedUserIds;
		if (!requested || requested.size === 0) {
			return;
		}
		const returnedIds = new Set(pending.members.map((record) => record.user.id));
		const notFound: Array<string> = [];
		for (const id of requested) {
			if (!returnedIds.has(id)) {
				notFound.push(id);
			}
		}
		if (notFound.length === 0) {
			return;
		}
		if (!this.nonMembers[pending.guildId]) {
			this.nonMembers[pending.guildId] = new Set();
		}
		const cache = this.nonMembers[pending.guildId];
		for (const id of notFound) {
			cache.add(id);
		}
	}

	async fetchMembers(
		guildId: string,
		options?: {
			query?: string;
			limit?: number;
			userIds?: Array<string>;
			presences?: boolean;
			markMissingAsNonMembers?: boolean;
		},
	): Promise<Array<GuildMember>> {
		const userIds = options?.userIds;
		if (userIds && userIds.length > MAX_USER_IDS_PER_REQUEST) {
			const batches: Array<Promise<Array<GuildMember>>> = [];
			for (let i = 0; i < userIds.length; i += MAX_USER_IDS_PER_REQUEST) {
				const slice = userIds.slice(i, i + MAX_USER_IDS_PER_REQUEST);
				batches.push(this.fetchMembers(guildId, {...options, userIds: slice}));
			}
			const results = await Promise.all(batches);
			return results.flat();
		}
		const nonce = generateMemberNonce();
		return new Promise((resolve, reject) => {
			const timeoutId = setTimeout(() => {
				if (this.pendingRequests.has(nonce)) {
					this.pendingRequests.delete(nonce);
					reject(new Error('Request timed out'));
				}
			}, MEMBER_REQUEST_TIMEOUT);
			this.pendingRequests.set(nonce, {
				guildId,
				resolve,
				reject,
				members: [],
				receivedChunks: 0,
				requestedUserIds: userIds && userIds.length > 0 ? new Set(userIds) : undefined,
				markMissingAsNonMembers: options?.markMissingAsNonMembers ?? false,
				timeoutId,
			});
			const socket = GatewayConnection.socket;
			const requestOptions: {
				guildId: string;
				nonce: string;
				query?: string;
				limit?: number;
				userIds?: Array<string>;
				presences?: boolean;
			} = {
				guildId,
				nonce,
				presences: options?.presences ?? true,
			};
			if (options?.query) {
				requestOptions.query = options.query;
			}
			if (options?.limit !== undefined) {
				requestOptions.limit = options.limit;
			}
			if (userIds && userIds.length > 0) {
				requestOptions.userIds = userIds;
			}
			socket?.requestGuildMembers(requestOptions);
		});
	}

	requestMembersInBackground(options: {
		guildIds: Array<string>;
		query?: string;
		limit?: number;
		presences?: boolean;
	}): void {
		const socket = GatewayConnection.socket;
		if (!socket) {
			return;
		}
		const guildIds = [...new Set(options.guildIds.filter((guildId) => guildId.length > 0))];
		if (guildIds.length === 0) {
			return;
		}
		socket.requestGuildMembers({
			guildIds,
			...(options.query !== undefined && {query: options.query}),
			...(options.limit !== undefined && {limit: options.limit}),
			...(options.presences !== undefined && {presences: options.presences}),
		});
	}

	async ensureMembersLoaded(guildId: string, userIds: Array<string>): Promise<void> {
		const missingIds = this.getMissingMemberIds(guildId, userIds);
		if (missingIds.length === 0) {
			return;
		}
		await this.fetchMembers(guildId, {userIds: missingIds, markMissingAsNonMembers: true});
	}

	async ensureMembersLoadedForMessages(guildId: string, userIds: Array<string>): Promise<void> {
		const missingIds = this.getMissingMemberIds(guildId, userIds);
		if (missingIds.length === 0) {
			return;
		}
		this.queuePendingMessageMemberHydration(guildId, missingIds);
		if (!GatewayConnection.hasCompletedGuildSync(guildId) && SelectedGuild.selectedGuildId === guildId) {
			GatewayConnection.syncGuildIfNeeded(guildId, 'message-member-hydration');
		}
		await this.requestPendingMessageMembers(guildId);
	}

	handleConnectionResumed(): void {
		this.resetMessageMemberRequests();
		for (const guildId of Array.from(this.pendingMessageMemberHydration.keys())) {
			void this.requestPendingMessageMembers(guildId);
		}
	}

	private resetMessageMemberRequests(): void {
		this.messageMemberRequestGeneration += 1;
		this.inFlightMessageMembers.clear();
	}

	private getMissingMemberIds(guildId: string, userIds: Array<string>): Array<string> {
		const known = this.members[guildId];
		const negativeCache = this.nonMembers[guildId];
		return [...new Set(userIds)].filter((id) => !known?.[id] && !negativeCache?.has(id));
	}

	private queuePendingMessageMemberHydration(guildId: string, userIds: Array<string>): void {
		let pending = this.pendingMessageMemberHydration.get(guildId);
		if (!pending) {
			pending = new Set();
			this.pendingMessageMemberHydration.set(guildId, pending);
		}
		for (const userId of userIds) {
			pending.add(userId);
		}
	}

	private async requestPendingMessageMembers(guildId: string): Promise<void> {
		if (!GatewayConnection.hasCompletedGuildSync(guildId)) {
			return;
		}
		const pending = this.pendingMessageMemberHydration.get(guildId);
		if (!pending || pending.size === 0) {
			return;
		}
		const stillMissing = new Set(this.getMissingMemberIds(guildId, Array.from(pending)));
		for (const userId of Array.from(pending)) {
			if (!stillMissing.has(userId)) {
				pending.delete(userId);
			}
		}
		if (pending.size === 0) {
			this.pendingMessageMemberHydration.delete(guildId);
			return;
		}
		let inFlight = this.inFlightMessageMembers.get(guildId);
		if (!inFlight) {
			inFlight = new Set();
			this.inFlightMessageMembers.set(guildId, inFlight);
		}
		const userIds = Array.from(pending).filter((userId) => !inFlight.has(userId));
		if (userIds.length === 0) {
			return;
		}
		for (const userId of userIds) {
			inFlight.add(userId);
		}
		const generation = this.messageMemberRequestGeneration;
		try {
			await this.fetchMembers(guildId, {userIds, markMissingAsNonMembers: false});
			if (generation === this.messageMemberRequestGeneration) {
				this.forgetPendingMessageMembers(guildId, userIds);
			}
		} catch (error) {
			logger.warn('Failed to hydrate message author members, will retry on the next load or reconnect', {
				guildId,
				userIds,
				error,
			});
		} finally {
			if (generation === this.messageMemberRequestGeneration) {
				this.releaseInFlightMessageMembers(guildId, userIds);
			}
		}
	}

	private forgetPendingMessageMembers(guildId: string, userIds: Array<string>): void {
		const pending = this.pendingMessageMemberHydration.get(guildId);
		if (!pending) {
			return;
		}
		for (const userId of userIds) {
			pending.delete(userId);
		}
		if (pending.size === 0) {
			this.pendingMessageMemberHydration.delete(guildId);
		}
	}

	private releaseInFlightMessageMembers(guildId: string, userIds: Array<string>): void {
		const inFlight = this.inFlightMessageMembers.get(guildId);
		if (!inFlight) {
			return;
		}
		for (const userId of userIds) {
			inFlight.delete(userId);
		}
		if (inFlight.size === 0) {
			this.inFlightMessageMembers.delete(guildId);
		}
	}

	isGuildFullyLoaded(guildId: string): boolean {
		return this.loadedGuilds.has(guildId);
	}
}

export default new GuildMembers();
