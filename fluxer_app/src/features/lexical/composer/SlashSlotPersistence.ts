// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CommandOption} from '@app/features/devtools/hooks/useCommands';
import type {SlashSlotValidity} from '@app/features/lexical/composer/nodes/SlashSlotNode';
import type {SlashSlotType, SlashSlotValidationError} from '@app/features/lexical/composer/slashSlotValidation';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';

export const COMPOSER_SLASH_COMMAND_STATE_SEGMENT_PREFIX = 'slash-command-state:';
export const COMPOSER_SLASH_COMMAND_STATE_MAX_ID_LENGTH = 32_768;
export const COMPOSER_SLASH_SLOT_STATE_SEGMENT_PREFIX = 'slash-slot-state:';
export const COMPOSER_SLASH_SLOT_STATE_MAX_ID_LENGTH = 32_768;

interface PersistedSlashSlotStateBase {
	optionName: string;
	optionType: SlashSlotType;
	required: boolean;
	description: string;
	choices: ReadonlyArray<{name: string; value: string}>;
	validity: SlashSlotValidity;
	validationError: SlashSlotValidationError | null;
	touched: boolean;
}

export type PersistedSlashSlotState =
	| (PersistedSlashSlotStateBase & {version: 1; allowEmpty?: never})
	| (PersistedSlashSlotStateBase & {version: 2; allowEmpty: boolean});

export interface PersistedSlashCommandState {
	version: 1;
	commandName: string;
	optionalOptions: ReadonlyArray<CommandOption>;
}

const SLOT_STATE_V1_KEYS = [
	'version',
	'optionName',
	'optionType',
	'required',
	'description',
	'choices',
	'validity',
	'validationError',
	'touched',
] as const;
const SLOT_STATE_V2_KEYS = [...SLOT_STATE_V1_KEYS, 'allowEmpty'] as const;
const COMMAND_STATE_KEYS = ['version', 'commandName', 'optionalOptions'] as const;
const COMMAND_OPTION_KEYS = ['name', 'description', 'type', 'required', 'allowEmpty', 'choices'] as const;
const COMMAND_OPTION_REQUIRED_KEYS = ['name', 'description', 'type', 'required'] as const;
const CHOICE_KEYS = ['name', 'value'] as const;
const OPTION_TYPES = new Set<SlashSlotType>([
	'string',
	'user',
	'channel',
	'role',
	'integer',
	'number',
	'boolean',
	'choice',
]);
const VALIDITIES = new Set<SlashSlotValidity>(['neutral', 'valid', 'invalid']);
const VALIDATION_ERRORS = new Set<SlashSlotValidationError>([
	'required',
	'unknown-user',
	'unknown-role',
	'unknown-channel',
	'not-an-integer',
	'not-a-number',
	'not-a-boolean',
	'invalid-choice',
]);
const OPTION_NAME_RE = /^[a-z0-9_-]{1,32}$/;
const COMMAND_NAME_RE = /^\/[a-z0-9_-]{1,32}$/;
const USER_WIRE_RE = /^<@!?\d+>$/;
const ROLE_WIRE_RE = /^<@&\d+>$/;
const CHANNEL_WIRE_RE = /^<#\d+>$/;
const INTEGER_RE = /^-?\d+$/;
const NUMBER_RE = /^-?\d+(\.\d+)?$/;

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value != null && !Array.isArray(value);
}

function hasExactKeys(value: Record<string, unknown>, keys: ReadonlyArray<string>): boolean {
	const actualKeys = Object.keys(value);
	return actualKeys.length === keys.length && keys.every((key) => Object.hasOwn(value, key));
}

function hasAllowedKeys(
	value: Record<string, unknown>,
	allowedKeys: ReadonlyArray<string>,
	requiredKeys: ReadonlyArray<string>,
): boolean {
	return (
		Object.keys(value).every((key) => allowedKeys.includes(key)) &&
		requiredKeys.every((key) => Object.hasOwn(value, key))
	);
}

function isBoundedSingleLineString(value: unknown, maximumLength: number): value is string {
	return typeof value === 'string' && value.length <= maximumLength && !value.includes('\n') && !value.includes('\r');
}

function parseChoices(value: unknown): ReadonlyArray<{name: string; value: string}> | null {
	if (!Array.isArray(value) || value.length > 25) {
		return null;
	}
	const choices: Array<{name: string; value: string}> = [];
	for (const choice of value) {
		if (!isRecord(choice) || !hasExactKeys(choice, CHOICE_KEYS)) {
			return null;
		}
		if (
			!isBoundedSingleLineString(choice.name, 100) ||
			choice.name.length === 0 ||
			!isBoundedSingleLineString(choice.value, 100) ||
			choice.value.length === 0
		) {
			return null;
		}
		choices.push({name: choice.name, value: choice.value});
	}
	return choices;
}

export function parsePersistedSlashCommandOptions(value: unknown): ReadonlyArray<CommandOption> | null {
	if (!Array.isArray(value) || value.length > 25) {
		return null;
	}
	const options: Array<CommandOption> = [];
	const optionNames = new Set<string>();
	for (const option of value) {
		if (!isRecord(option) || !hasAllowedKeys(option, COMMAND_OPTION_KEYS, COMMAND_OPTION_REQUIRED_KEYS)) {
			return null;
		}
		if (typeof option.name !== 'string' || !OPTION_NAME_RE.test(option.name) || optionNames.has(option.name)) {
			return null;
		}
		if (!isBoundedSingleLineString(option.description, 2_048)) {
			return null;
		}
		if (typeof option.type !== 'string' || !OPTION_TYPES.has(option.type as SlashSlotType)) {
			return null;
		}
		if (option.required !== false || (option.allowEmpty !== undefined && typeof option.allowEmpty !== 'boolean')) {
			return null;
		}
		const choices = option.choices === undefined ? undefined : parseChoices(option.choices);
		if (choices === null || (option.type === 'choice' && (choices == null || choices.length === 0))) {
			return null;
		}
		optionNames.add(option.name);
		options.push({
			name: option.name,
			description: option.description,
			type: option.type as SlashSlotType,
			required: false,
			...(option.allowEmpty === undefined ? {} : {allowEmpty: option.allowEmpty}),
			...(choices === undefined ? {} : {choices: [...choices]}),
		});
	}
	return options;
}

function parseCommandState(value: unknown): PersistedSlashCommandState | null {
	if (!isRecord(value) || !hasExactKeys(value, COMMAND_STATE_KEYS) || value.version !== 1) {
		return null;
	}
	if (typeof value.commandName !== 'string' || !COMMAND_NAME_RE.test(value.commandName)) {
		return null;
	}
	const optionalOptions = parsePersistedSlashCommandOptions(value.optionalOptions);
	return optionalOptions == null ? null : {version: 1, commandName: value.commandName, optionalOptions};
}

function parseState(value: unknown): PersistedSlashSlotState | null {
	if (!isRecord(value)) {
		return null;
	}
	const isVersion1 = value.version === 1 && hasExactKeys(value, SLOT_STATE_V1_KEYS);
	const isVersion2 = value.version === 2 && hasExactKeys(value, SLOT_STATE_V2_KEYS);
	if (!isVersion1 && !isVersion2) {
		return null;
	}
	if (typeof value.optionName !== 'string' || !OPTION_NAME_RE.test(value.optionName)) {
		return null;
	}
	if (typeof value.optionType !== 'string' || !OPTION_TYPES.has(value.optionType as SlashSlotType)) {
		return null;
	}
	if (typeof value.required !== 'boolean' || !isBoundedSingleLineString(value.description, 2_048)) {
		return null;
	}
	const choices = parseChoices(value.choices);
	if (choices == null || (value.optionType === 'choice' && choices.length === 0)) {
		return null;
	}
	if (typeof value.validity !== 'string' || !VALIDITIES.has(value.validity as SlashSlotValidity)) {
		return null;
	}
	if (
		value.validationError !== null &&
		(typeof value.validationError !== 'string' ||
			!VALIDATION_ERRORS.has(value.validationError as SlashSlotValidationError))
	) {
		return null;
	}
	if (typeof value.touched !== 'boolean') {
		return null;
	}
	if (isVersion2 && typeof value.allowEmpty !== 'boolean') {
		return null;
	}
	const validity = value.validity as SlashSlotValidity;
	const validationError = value.validationError as SlashSlotValidationError | null;
	if (
		(validity === 'valid' && validationError != null) ||
		(validity === 'invalid' && validationError == null) ||
		(!value.touched && validity !== 'neutral')
	) {
		return null;
	}
	const state = {
		optionName: value.optionName,
		optionType: value.optionType as SlashSlotType,
		required: value.required,
		description: value.description,
		choices,
		validity,
		validationError,
		touched: value.touched,
	};
	return isVersion2 ? {version: 2, ...state, allowEmpty: value.allowEmpty as boolean} : {version: 1, ...state};
}

function hasValidResolvedWire(state: PersistedSlashSlotState, display: string, wire: string): boolean {
	const text = display.trim();
	switch (state.optionType) {
		case 'user':
			return USER_WIRE_RE.test(wire);
		case 'role':
			return ROLE_WIRE_RE.test(wire);
		case 'channel':
			return CHANNEL_WIRE_RE.test(wire);
		case 'integer':
			return INTEGER_RE.test(text) && wire === text;
		case 'number':
			return NUMBER_RE.test(text) && wire === text;
		case 'boolean':
			return (text.toLowerCase() === 'true' || text.toLowerCase() === 'false') && wire === text.toLowerCase();
		case 'choice':
			return state.choices.some(
				(choice) =>
					choice.value === wire && (choice.value === text || choice.name.toLowerCase() === text.toLowerCase()),
			);
		case 'string':
			return text.length > 0 && wire === text;
	}
}

export function createSlashSlotStateSegmentId(state: PersistedSlashSlotState): string {
	return `${COMPOSER_SLASH_SLOT_STATE_SEGMENT_PREFIX}${encodeURIComponent(JSON.stringify(state))}`;
}

export function createSlashCommandStateSegmentId(
	commandName: string,
	optionalOptions: ReadonlyArray<CommandOption>,
): string | null {
	if (!COMMAND_NAME_RE.test(commandName)) {
		return null;
	}
	const parsedOptions = parsePersistedSlashCommandOptions(optionalOptions);
	if (parsedOptions == null) {
		return null;
	}
	const id = `${COMPOSER_SLASH_COMMAND_STATE_SEGMENT_PREFIX}${encodeURIComponent(
		JSON.stringify({version: 1, commandName, optionalOptions: parsedOptions}),
	)}`;
	return id.length <= COMPOSER_SLASH_COMMAND_STATE_MAX_ID_LENGTH ? id : null;
}

export function parseSlashCommandStateSegmentId(id: string): PersistedSlashCommandState | null {
	if (
		!id.startsWith(COMPOSER_SLASH_COMMAND_STATE_SEGMENT_PREFIX) ||
		id.length > COMPOSER_SLASH_COMMAND_STATE_MAX_ID_LENGTH
	) {
		return null;
	}
	let value: unknown;
	try {
		value = JSON.parse(decodeURIComponent(id.slice(COMPOSER_SLASH_COMMAND_STATE_SEGMENT_PREFIX.length)));
	} catch {
		return null;
	}
	return parseCommandState(value);
}

export function parseSlashCommandStateSegment(segment: MentionSegment): PersistedSlashCommandState | null {
	if (
		segment.type !== 'special' ||
		!COMMAND_NAME_RE.test(segment.displayText) ||
		segment.actualText !== segment.displayText
	) {
		return null;
	}
	const state = parseSlashCommandStateSegmentId(segment.id);
	return state != null && state.commandName === segment.displayText ? state : null;
}

export function parseSlashSlotStateSegmentId(id: string): PersistedSlashSlotState | null {
	if (!id.startsWith(COMPOSER_SLASH_SLOT_STATE_SEGMENT_PREFIX) || id.length > COMPOSER_SLASH_SLOT_STATE_MAX_ID_LENGTH) {
		return null;
	}
	let value: unknown;
	try {
		value = JSON.parse(decodeURIComponent(id.slice(COMPOSER_SLASH_SLOT_STATE_SEGMENT_PREFIX.length)));
	} catch {
		return null;
	}
	return parseState(value);
}

export function parseSlashSlotStateSegment(segment: MentionSegment): PersistedSlashSlotState | null {
	if (
		segment.type !== 'special' ||
		segment.displayText.length === 0 ||
		segment.displayText[0] !== ' ' ||
		segment.actualText.length === 0 ||
		segment.actualText[0] !== ' ' ||
		segment.displayText.includes('\n') ||
		segment.displayText.includes('\r') ||
		segment.actualText.includes('\n') ||
		segment.actualText.includes('\r')
	) {
		return null;
	}
	const state = parseSlashSlotStateSegmentId(segment.id);
	if (state == null) {
		return null;
	}
	const display = segment.displayText.slice(1);
	const wire = segment.actualText.slice(1);
	if (display.length > 0 && !state.touched) {
		return null;
	}
	if (state.validity !== 'valid') {
		return wire === display ? state : null;
	}
	return display.length > 0 && hasValidResolvedWire(state, display, wire) ? state : null;
}

export function isSlashSlotStateSegmentId(id: string): boolean {
	return parseSlashSlotStateSegmentId(id) != null;
}
