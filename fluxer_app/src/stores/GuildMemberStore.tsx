/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {makeAutoObservable} from 'mobx';
import {type GuildMember, GuildMemberRecord} from '~/records/GuildMemberRecord';
import type {GuildReadyData} from '~/records/GuildRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import ConnectionStore from '~/stores/ConnectionStore';

type Members = Record<string, GuildMemberRecord>;

interface PendingMemberRequest {
	resolve: (members: Array<GuildMemberRecord>) => void;
	reject: (error: Error) => void;
	members: Array<GuildMemberRecord>;
	receivedChunks: number;
	expectedChunks: number;
}

const MEMBER_REQUEST_TIMEOUT = 30000;
const MEMBER_NONCE_LENGTH = 32;
const MEMBER_NONCE_CHARS = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';

function generateMemberNonce(): string {
	let nonce = '';
	const charsLength = MEMBER_NONCE_CHARS.length;
	for (let i = 0; i < MEMBER_NONCE_LENGTH; i += 1) {
		nonce += MEMBER_NONCE_CHARS[Math.floor(Math.random() * charsLength)];
	}
	return nonce;
}

class GuildMemberStore {
	members: Record<string, Members> = {};
	pendingRequests: Map<string, PendingMemberRequest> = new Map();
	loadedGuilds: Set<string> = new Set();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	getMember(guildId: string, userId?: string | null): GuildMemberRecord | null {
		if (!userId) {
			return null;
		}
		return this.members[guildId]?.[userId] ?? null;
	}

	isUserTimedOut(guildId: string | null, userId?: string | null): boolean {
		if (!guildId || !userId) {
			return false;
		}

		const member = this.getMember(guildId, userId);
		return member?.isTimedOut() ?? false;
	}

	getMembers(guildId: string): Array<GuildMemberRecord> {
		return Object.values(this.members[guildId] ?? {});
	}

	getMemberCount(guildId: string): number {
		return Object.keys(this.members[guildId] ?? {}).length;
	}

	handleConnectionOpen(guilds: Array<GuildReadyData>): void {
		this.members = {};
		for (const guild of guilds) {
			this.handleGuildCreate(guild);
		}
	}

	handleGuildCreate(guild: GuildReadyData): void {
		if (guild.unavailable) {
			return;
		}

		const ownMember = guild.members.find((m) => m.user.id === AuthenticationStore.currentUserId);
		this.members = {
			...this.members,
			[guild.id]: ownMember ? {[ownMember.user.id]: new GuildMemberRecord(guild.id, ownMember)} : {},
		};
	}

	handleGuildDelete(guildId: string): void {
		this.members = Object.fromEntries(Object.entries(this.members).filter(([id]) => id !== guildId));
	}

	handleMemberAdd(guildId: string, member: GuildMember): void {
		this.members = {
			...this.members,
			[guildId]: {
				...(this.members[guildId] ?? {}),
				[member.user.id]: new GuildMemberRecord(guildId, member),
			},
		};
	}

	handleMemberRemove(guildId: string, userId: string): void {
		const existingMembers = this.members[guildId];
		if (!existingMembers) {
			return;
		}

		const updatedGuildMembers = Object.fromEntries(Object.entries(existingMembers).filter(([id]) => id !== userId));

		this.members = {
			...this.members,
			...(Object.keys(updatedGuildMembers).length > 0 ? {[guildId]: updatedGuildMembers} : {}),
		};
	}

	handleGuildRoleDelete(guildId: string, roleId: string): void {
		const existingMembers = this.members[guildId];
		if (!existingMembers) {
			return;
		}

		const updatedGuildMembers = Object.fromEntries(
			Object.entries(existingMembers).map(([memberId, member]) => {
				if (member.roles.has(roleId)) {
					const newRoles = new Set(member.roles);
					newRoles.delete(roleId);
					return [
						memberId,
						new GuildMemberRecord(guildId, {
							...member.toJSON(),
							roles: Array.from(newRoles),
						}),
					];
				}
				return [memberId, member];
			}),
		);

		this.members = {
			...this.members,
			[guildId]: updatedGuildMembers,
		};
	}

	handleMembersChunk(params: {
		guildId: string;
		members: Array<GuildMember>;
		chunkIndex: number;
		chunkCount: number;
		nonce?: string;
	}): void {
		const {guildId, members, chunkCount, nonce} = params;

		const newMembers: Array<GuildMemberRecord> = [];
		for (const member of members) {
			const record = new GuildMemberRecord(guildId, member);
			newMembers.push(record);
		}

		this.members = {
			...this.members,
			[guildId]: {
				...(this.members[guildId] ?? {}),
				...newMembers.reduce((acc, member) => {
					acc[member.user.id] = member;
					return acc;
				}, {} as Members),
			},
		};

		if (nonce) {
			const pending = this.pendingRequests.get(nonce);
			if (pending) {
				pending.members.push(...newMembers);
				pending.receivedChunks++;

				if (pending.receivedChunks >= chunkCount) {
					pending.resolve(pending.members);
					this.pendingRequests.delete(nonce);
				}
			}
		}
	}

	async fetchMembers(
		guildId: string,
		options?: {
			query?: string;
			limit?: number;
			userIds?: Array<string>;
		},
	): Promise<Array<GuildMemberRecord>> {
		const nonce = generateMemberNonce();

		return new Promise((resolve, reject) => {
			this.pendingRequests.set(nonce, {
				resolve,
				reject,
				members: [],
				receivedChunks: 0,
				expectedChunks: 1,
			});

			const socket = ConnectionStore.socket;
			const requestOptions: {
				guildId: string;
				nonce: string;
				query?: string;
				limit?: number;
				userIds?: Array<string>;
			} = {
				guildId,
				nonce,
			};

			if (options?.query) {
				requestOptions.query = options.query;
			}

			if (options?.limit !== undefined) {
				requestOptions.limit = options.limit;
			}

			if (options?.userIds && options.userIds.length > 0) {
				requestOptions.userIds = options.userIds;
			}

			socket?.requestGuildMembers(requestOptions);

			setTimeout(() => {
				if (this.pendingRequests.has(nonce)) {
					this.pendingRequests.delete(nonce);
					reject(new Error('Request timed out'));
				}
			}, MEMBER_REQUEST_TIMEOUT);
		});
	}

	async ensureMembersLoaded(guildId: string, userIds: Array<string>): Promise<void> {
		const missingIds = userIds.filter((id) => !this.members[guildId]?.[id]);
		if (missingIds.length === 0) {
			return;
		}

		await this.fetchMembers(guildId, {userIds: missingIds});
	}

	isGuildFullyLoaded(guildId: string): boolean {
		return this.loadedGuilds.has(guildId);
	}
}

export default new GuildMemberStore();
