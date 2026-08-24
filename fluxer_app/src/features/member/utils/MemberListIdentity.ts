// SPDX-License-Identifier: AGPL-3.0-or-later

import {Permissions} from '@fluxer/constants/src/ChannelConstants';

export interface MemberListViewOverwrite {
	readonly id: string;
	readonly allow: bigint;
	readonly deny: bigint;
}

export interface MemberListIdentityChannel {
	readonly permissionOverwrites: Readonly<Record<string, MemberListViewOverwrite>>;
}

export interface MemberListIdentityGuild {
	readonly id: string;
	getRole(roleId: string): {readonly permissions: bigint} | undefined;
}

export const OPEN_MEMBER_LIST_IDENTITY = 'everyone';

const IDENTITY_TOKEN_SEPARATOR = ',';
const SCRAMBLE_A = 0xcc9e2d51;
const SCRAMBLE_B = 0x1b873593;
const BLEND_A = 0x85ebca6b;
const BLEND_B = 0xc2b2ae35;
const CHAIN_ADDEND = 0xe6546b64;
const identityTokenEncoder = new TextEncoder();

function rotateLeft32(value: number, shift: number): number {
	return (value << shift) | (value >>> (32 - shift));
}

function scramble(value: number): number {
	return Math.imul(rotateLeft32(Math.imul(value, SCRAMBLE_A), 15), SCRAMBLE_B);
}

export function fingerprintIdentityTokens(tokens: string): number {
	const bytes = identityTokenEncoder.encode(tokens);
	const blockCount = bytes.length >>> 2;
	let accumulator = 0;
	for (let block = 0; block < blockCount; block += 1) {
		const offset = block << 2;
		const chunk = bytes[offset]! | (bytes[offset + 1]! << 8) | (bytes[offset + 2]! << 16) | (bytes[offset + 3]! << 24);
		accumulator ^= scramble(chunk);
		accumulator = rotateLeft32(accumulator, 13);
		accumulator = (Math.imul(accumulator, 5) + CHAIN_ADDEND) | 0;
	}
	const remainderStart = blockCount << 2;
	const remainderLength = bytes.length & 3;
	if (remainderLength > 0) {
		let remainder = 0;
		if (remainderLength === 3) {
			remainder ^= bytes[remainderStart + 2]! << 16;
		}
		if (remainderLength >= 2) {
			remainder ^= bytes[remainderStart + 1]! << 8;
		}
		remainder ^= bytes[remainderStart]!;
		accumulator ^= scramble(remainder);
	}
	accumulator ^= bytes.length;
	accumulator ^= accumulator >>> 16;
	accumulator = Math.imul(accumulator, BLEND_A);
	accumulator ^= accumulator >>> 13;
	accumulator = Math.imul(accumulator, BLEND_B);
	accumulator ^= accumulator >>> 16;
	return accumulator >>> 0;
}

function isViewableWithoutOverrides(
	guild: MemberListIdentityGuild | undefined,
	channel: MemberListIdentityChannel,
): boolean {
	if (guild == null) {
		return false;
	}
	const baselineRole = guild.getRole(guild.id);
	if (baselineRole == null) {
		return false;
	}
	if ((baselineRole.permissions & Permissions.VIEW_CHANNEL) !== Permissions.VIEW_CHANNEL) {
		return false;
	}
	for (const overwrite of Object.values(channel.permissionOverwrites)) {
		if ((overwrite.deny & Permissions.VIEW_CHANNEL) === Permissions.VIEW_CHANNEL) {
			return false;
		}
	}
	return true;
}

export function buildIdentityTokens(channel: MemberListIdentityChannel): string {
	const tokens: Array<string> = [];
	for (const overwrite of Object.values(channel.permissionOverwrites)) {
		if ((overwrite.allow & Permissions.VIEW_CHANNEL) === Permissions.VIEW_CHANNEL) {
			tokens.push(`allow:${overwrite.id}`);
		} else if ((overwrite.deny & Permissions.VIEW_CHANNEL) === Permissions.VIEW_CHANNEL) {
			tokens.push(`deny:${overwrite.id}`);
		}
	}
	tokens.sort();
	return tokens.join(IDENTITY_TOKEN_SEPARATOR);
}

export function deriveMemberListIdentity(
	guild: MemberListIdentityGuild | undefined,
	channel: MemberListIdentityChannel | undefined,
): string {
	if (channel == null) {
		return OPEN_MEMBER_LIST_IDENTITY;
	}
	if (isViewableWithoutOverrides(guild, channel)) {
		return OPEN_MEMBER_LIST_IDENTITY;
	}
	return fingerprintIdentityTokens(buildIdentityTokens(channel)).toString();
}
