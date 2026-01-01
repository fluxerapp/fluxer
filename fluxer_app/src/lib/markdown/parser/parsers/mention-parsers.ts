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

import {GuildNavKind, MentionKind, NodeType, ParserFlags} from '../types/enums';
import type {MentionNode, ParserResult} from '../types/nodes';

const LESS_THAN = 60;
const AT_SIGN = 64;
const HASH = 35;
const AMPERSAND = 38;
const SLASH = 47;
const LETTER_I = 105;
const LETTER_D = 100;
const COLON = 58;
const DIGIT_ZERO = 48;
const DIGIT_NINE = 57;

export function parseMention(text: string, parserFlags: number): ParserResult | null {
	if (text.length < 2 || text.charCodeAt(0) !== LESS_THAN) {
		return null;
	}

	const end = text.indexOf('>');
	if (end === -1) {
		return null;
	}

	const secondCharCode = text.charCodeAt(1);

	let mentionNode: MentionNode | null = null;

	if (secondCharCode === AT_SIGN) {
		mentionNode = parseUserOrRoleMention(text.slice(1, end), parserFlags);
	} else if (secondCharCode === HASH) {
		mentionNode = parseChannelMention(text.slice(1, end), parserFlags);
	} else if (secondCharCode === SLASH) {
		mentionNode = parseCommandMention(text.slice(1, end), parserFlags);
	} else if (
		secondCharCode === LETTER_I &&
		text.length > 3 &&
		text.charCodeAt(2) === LETTER_D &&
		text.charCodeAt(3) === COLON
	) {
		mentionNode = parseGuildNavigation(text.slice(1, end), parserFlags);
	}

	return mentionNode ? {node: mentionNode, advance: end + 1} : null;
}

function isDigitOnly(text: string): boolean {
	for (let i = 0; i < text.length; i++) {
		const charCode = text.charCodeAt(i);
		if (charCode < DIGIT_ZERO || charCode > DIGIT_NINE) {
			return false;
		}
	}
	return text.length > 0;
}

function parseUserOrRoleMention(inner: string, parserFlags: number): MentionNode | null {
	if (inner.length < 2 || inner.charCodeAt(0) !== AT_SIGN) {
		return null;
	}

	if (inner.length > 2 && inner.charCodeAt(1) === AMPERSAND) {
		const roleId = inner.slice(2);
		if (isDigitOnly(roleId) && parserFlags & ParserFlags.ALLOW_ROLE_MENTIONS) {
			return {
				type: NodeType.Mention,
				kind: {kind: MentionKind.Role, id: roleId},
			};
		}
	} else {
		const userId = inner.startsWith('@!') ? inner.slice(2) : inner.slice(1);
		if (isDigitOnly(userId) && parserFlags & ParserFlags.ALLOW_USER_MENTIONS) {
			return {
				type: NodeType.Mention,
				kind: {kind: MentionKind.User, id: userId},
			};
		}
	}

	return null;
}

function parseChannelMention(inner: string, parserFlags: number): MentionNode | null {
	if (inner.length < 2 || inner.charCodeAt(0) !== HASH || !(parserFlags & ParserFlags.ALLOW_CHANNEL_MENTIONS)) {
		return null;
	}

	const channelId = inner.slice(1);
	if (isDigitOnly(channelId)) {
		return {
			type: NodeType.Mention,
			kind: {kind: MentionKind.Channel, id: channelId},
		};
	}

	return null;
}

function parseCommandMention(inner: string, parserFlags: number): MentionNode | null {
	if (!(parserFlags & ParserFlags.ALLOW_COMMAND_MENTIONS) || inner.length < 2 || inner.charCodeAt(0) !== SLASH) {
		return null;
	}

	const colonIndex = inner.indexOf(':');
	if (colonIndex === -1) return null;

	const commandPart = inner.slice(0, colonIndex);
	const idPart = inner.slice(colonIndex + 1);

	if (!idPart || !isDigitOnly(idPart)) return null;

	const segments = commandPart.slice(1).trim().split(' ');
	if (segments.length === 0) return null;

	return {
		type: NodeType.Mention,
		kind: {
			kind: MentionKind.Command,
			name: segments[0],
			subcommandGroup: segments.length === 3 ? segments[1] : undefined,
			subcommand: segments.length >= 2 ? segments[segments.length - 1] : undefined,
			id: idPart,
		},
	};
}

function parseGuildNavigation(inner: string, parserFlags: number): MentionNode | null {
	if (!(parserFlags & ParserFlags.ALLOW_GUILD_NAVIGATIONS) || inner.length < 5) {
		return null;
	}

	if (inner.charCodeAt(0) !== LETTER_I || inner.charCodeAt(1) !== LETTER_D || inner.charCodeAt(2) !== COLON) {
		return null;
	}

	const parts = inner.split(':');
	if (parts.length < 2 || parts.length > 3) return null;

	const [idLabel, navType, navId] = parts;
	if (idLabel !== 'id') return null;

	const navigationType = getNavigationType(navType);
	if (!navigationType) return null;

	if (navigationType === GuildNavKind.LinkedRoles) {
		return createLinkedRolesNavigation(parts.length === 3 ? navId : undefined);
	}

	if (parts.length !== 2) return null;
	return createBasicNavigation(navigationType);
}

function getNavigationType(navTypeLower: string): GuildNavKind | null {
	switch (navTypeLower) {
		case 'customize':
			return GuildNavKind.Customize;
		case 'browse':
			return GuildNavKind.Browse;
		case 'guide':
			return GuildNavKind.Guide;
		case 'linked-roles':
			return GuildNavKind.LinkedRoles;
		default:
			return null;
	}
}

function createLinkedRolesNavigation(id?: string): MentionNode {
	return {
		type: NodeType.Mention,
		kind: {
			kind: MentionKind.GuildNavigation,
			navigationType: GuildNavKind.LinkedRoles,
			id,
		},
	};
}

function createBasicNavigation(navigationType: GuildNavKind): MentionNode {
	return {
		type: NodeType.Mention,
		kind: {
			kind: MentionKind.GuildNavigation,
			navigationType,
		},
	};
}
