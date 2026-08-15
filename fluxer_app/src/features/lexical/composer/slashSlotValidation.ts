// SPDX-License-Identifier: AGPL-3.0-or-later

export type SlashSlotType = 'string' | 'user' | 'channel' | 'role' | 'integer' | 'number' | 'boolean' | 'choice';

export interface SlashSlotResolvers {
	resolveUser?: (query: string) => {id: string} | null;
	resolveChannel?: (query: string) => {id: string} | null;
	resolveRole?: (query: string) => {id: string} | null;
	required?: boolean;
	choices?: ReadonlyArray<{name: string; value: string}>;
}

export type SlashSlotValidationError =
	| 'required'
	| 'unknown-user'
	| 'unknown-role'
	| 'unknown-channel'
	| 'not-an-integer'
	| 'not-a-number'
	| 'not-a-boolean'
	| 'invalid-choice';

export interface SlashSlotValidationResult {
	valid: boolean;
	resolvedWire?: string;
	error?: SlashSlotValidationError;
}

const USER_WIRE_RE = /^<@!?(\d+)>$/;
const ROLE_WIRE_RE = /^<@&(\d+)>$/;
const CHANNEL_WIRE_RE = /^<#(\d+)>$/;
const BARE_ID_RE = /^\d+$/;
const INTEGER_RE = /^-?\d+$/;
const NUMBER_RE = /^-?\d+(\.\d+)?$/;

function stripLeadingSigil(text: string, sigil: string): string {
	return text.startsWith(sigil) ? text.slice(sigil.length) : text;
}

export function validateSlot(
	optionType: SlashSlotType,
	rawText: string,
	resolvers: SlashSlotResolvers,
): SlashSlotValidationResult {
	const text = rawText.trim();
	if (text.length === 0) {
		if (resolvers.required) {
			return {valid: false, error: 'required'};
		}
		return {valid: true};
	}

	switch (optionType) {
		case 'user': {
			const wireMatch = USER_WIRE_RE.exec(text);
			if (wireMatch) {
				return {valid: true, resolvedWire: `<@${wireMatch[1]}>`};
			}
			if (BARE_ID_RE.test(text)) {
				return {valid: true, resolvedWire: `<@${text}>`};
			}
			const resolved = resolvers.resolveUser == null ? null : resolvers.resolveUser(stripLeadingSigil(text, '@'));
			if (resolved) {
				return {valid: true, resolvedWire: `<@${resolved.id}>`};
			}
			return {valid: false, error: 'unknown-user'};
		}
		case 'role': {
			const wireMatch = ROLE_WIRE_RE.exec(text);
			if (wireMatch) {
				return {valid: true, resolvedWire: `<@&${wireMatch[1]}>`};
			}
			if (BARE_ID_RE.test(text)) {
				return {valid: true, resolvedWire: `<@&${text}>`};
			}
			const resolved = resolvers.resolveRole == null ? null : resolvers.resolveRole(stripLeadingSigil(text, '@'));
			if (resolved) {
				return {valid: true, resolvedWire: `<@&${resolved.id}>`};
			}
			return {valid: false, error: 'unknown-role'};
		}
		case 'channel': {
			const wireMatch = CHANNEL_WIRE_RE.exec(text);
			if (wireMatch) {
				return {valid: true, resolvedWire: `<#${wireMatch[1]}>`};
			}
			if (BARE_ID_RE.test(text)) {
				return {valid: true, resolvedWire: `<#${text}>`};
			}
			const resolved = resolvers.resolveChannel == null ? null : resolvers.resolveChannel(stripLeadingSigil(text, '#'));
			if (resolved) {
				return {valid: true, resolvedWire: `<#${resolved.id}>`};
			}
			return {valid: false, error: 'unknown-channel'};
		}
		case 'integer': {
			if (!INTEGER_RE.test(text)) {
				return {valid: false, error: 'not-an-integer'};
			}
			return {valid: true, resolvedWire: text};
		}
		case 'number': {
			if (!NUMBER_RE.test(text)) {
				return {valid: false, error: 'not-a-number'};
			}
			return {valid: true, resolvedWire: text};
		}
		case 'boolean': {
			const lowered = text.toLowerCase();
			if (lowered !== 'true' && lowered !== 'false') {
				return {valid: false, error: 'not-a-boolean'};
			}
			return {valid: true, resolvedWire: lowered};
		}
		case 'choice': {
			const choices = resolvers.choices;
			const choice =
				choices == null
					? undefined
					: choices.find(
							(candidate) => candidate.value === text || candidate.name.toLowerCase() === text.toLowerCase(),
						);
			if (!choice) {
				return {valid: false, error: 'invalid-choice'};
			}
			return {valid: true, resolvedWire: choice.value};
		}
		default:
			return {valid: true, resolvedWire: text};
	}
}
