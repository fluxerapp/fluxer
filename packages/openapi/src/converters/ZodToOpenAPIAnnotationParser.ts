// SPDX-License-Identifier: AGPL-3.0-or-later
export interface BitflagEntry {
	name: string;
	value: string;
	description?: string;
}
export interface EnumEntry {
	name: string;
	value: string | number;
	description?: string;
}
export interface FluxerTypeAnnotation {
	typeName: string;
	userDescription: string | undefined;
	enumNames?: Array<string>;
	enumEntries?: Array<EnumEntry>;
	bitflagValues?: Array<BitflagEntry>;
	bitflagTypeName?: string;
	objectName?: string;
	fieldDescription?: string;
}
interface BitflagEntryJson {
	n: string;
	v: string;
	d?: string;
}
interface EnumEntryJson {
	n: string;
	v: string | number;
	d?: string;
}
function isBitflagEntryJson(value: unknown): value is BitflagEntryJson {
	return (
		typeof value === 'object' &&
		value !== null &&
		'n' in value &&
		'v' in value &&
		typeof value.n === 'string' &&
		typeof value.v === 'string' &&
		(!('d' in value) || typeof value.d === 'string')
	);
}
function isEnumEntryJson(value: unknown): value is EnumEntryJson {
	return (
		typeof value === 'object' &&
		value !== null &&
		'n' in value &&
		'v' in value &&
		typeof value.n === 'string' &&
		(typeof value.v === 'string' || typeof value.v === 'number') &&
		(!('d' in value) || typeof value.d === 'string')
	);
}
export function escapeRegex(str: string): string {
	return str.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}
function parseBitflagEntries(entriesStr: string): Array<BitflagEntry> {
	try {
		const parsed: unknown = JSON.parse(entriesStr);
		if (!Array.isArray(parsed)) return [];
		return parsed.filter(isBitflagEntryJson).map((entry) => ({
			name: entry.n,
			value: entry.v,
			...(entry.d ? {description: entry.d} : {}),
		}));
	} catch {
		return entriesStr
			.split(',')
			.filter(Boolean)
			.map((entry) => {
				const [name, value] = entry.split('=');
				return {name: name?.trim() ?? '', value: value?.trim() ?? '0'};
			})
			.filter((entry) => entry.name.length > 0);
	}
}
function parseEnumEntries(entriesStr: string): Array<EnumEntry> {
	try {
		const parsed: unknown = JSON.parse(entriesStr);
		const entries = Array.isArray(parsed) ? parsed : [parsed];
		return entries.filter(isEnumEntryJson).map((entry) => ({
			name: entry.n,
			value: entry.v,
			...(entry.d ? {description: entry.d} : {}),
		}));
	} catch {
		return entriesStr
			.split(',')
			.filter(Boolean)
			.map((name) => ({name: name.trim(), value: name.trim()}))
			.filter((entry) => entry.name.length > 0);
	}
}
function findJsonEnd(str: string, startIndex: number): number {
	const openChar = str[startIndex];
	const closeChar = openChar === '[' ? ']' : openChar === '{' ? '}' : null;
	if (!closeChar) return -1;
	let depth = 1;
	let inString = false;
	let escaping = false;
	for (let i = startIndex + 1; i < str.length; i++) {
		const char = str[i];
		if (escaping) {
			escaping = false;
			continue;
		}
		if (char === '\\') {
			escaping = true;
			continue;
		}
		if (char === '"') {
			inString = !inString;
			continue;
		}
		if (inString) continue;
		if (char === openChar) depth++;
		if (char === closeChar) depth--;
		if (depth === 0) return i;
	}
	return -1;
}
function splitTypeAndDescription(rest: string): {
	typeAndData: string;
	userDescription: string | undefined;
} {
	const bracketIndex = rest.indexOf('[');
	const braceIndex = rest.indexOf('{');
	let jsonStartIndex = -1;
	if (bracketIndex !== -1 && braceIndex !== -1) {
		jsonStartIndex = Math.min(bracketIndex, braceIndex);
	} else if (bracketIndex !== -1) {
		jsonStartIndex = bracketIndex;
	} else if (braceIndex !== -1) {
		jsonStartIndex = braceIndex;
	}
	if (jsonStartIndex !== -1) {
		const jsonEnd = findJsonEnd(rest, jsonStartIndex);
		if (jsonEnd !== -1) {
			const endIndex = jsonEnd + 1;
			const userDesc = rest.slice(endIndex).trim() || undefined;
			return {typeAndData: rest.slice(0, endIndex), userDescription: userDesc};
		}
	}
	const firstSpaceIndex = rest.indexOf(' ');
	if (firstSpaceIndex === -1) {
		return {typeAndData: rest, userDescription: undefined};
	}
	return {
		typeAndData: rest.slice(0, firstSpaceIndex),
		userDescription: rest.slice(firstSpaceIndex + 1).trim() || undefined,
	};
}
export function parseFluxerTypeAnnotation(description: string | undefined): FluxerTypeAnnotation | null {
	if (!description?.startsWith('fluxer:')) return null;
	const rest = description.slice('fluxer:'.length);
	const {typeAndData, userDescription: rawUserDescription} = splitTypeAndDescription(rest);
	let userDescription = rawUserDescription;
	let fieldDescription: string | undefined;
	if (userDescription) {
		const fieldDescIndex = userDescription.indexOf('|fieldDesc:');
		if (fieldDescIndex !== -1) {
			fieldDescription = userDescription.slice(fieldDescIndex + '|fieldDesc:'.length).trim();
			userDescription = userDescription.slice(0, fieldDescIndex).trim() || undefined;
		}
	}
	if (typeAndData.startsWith('NamedObject:')) {
		const objectName = typeAndData.slice('NamedObject:'.length);
		return {typeName: 'NamedObject', objectName, userDescription, fieldDescription};
	}
	if (typeAndData.startsWith('IntegerEnum:')) {
		const namesStr = typeAndData.slice('IntegerEnum:'.length);
		const enumNames = namesStr.split(',').filter(Boolean);
		return {typeName: 'IntegerEnum', userDescription, enumNames, fieldDescription};
	}
	if (typeAndData.startsWith('EnumValue:')) {
		const entryStr = typeAndData.slice('EnumValue:'.length);
		const enumEntries = parseEnumEntries(entryStr);
		const enumNames = enumEntries.length > 0 ? [enumEntries[0].name] : undefined;
		return {
			typeName: 'EnumValue',
			userDescription,
			enumNames,
			enumEntries: enumEntries.length > 0 ? enumEntries : undefined,
			fieldDescription,
		};
	}
	if (typeAndData.startsWith('EnumValues:')) {
		const entriesStr = typeAndData.slice('EnumValues:'.length);
		const enumEntries = parseEnumEntries(entriesStr);
		const enumNames = enumEntries.map((e) => e.name);
		return {
			typeName: 'EnumValues',
			userDescription,
			enumNames,
			enumEntries: enumEntries.length > 0 ? enumEntries : undefined,
			fieldDescription,
		};
	}
	if (typeAndData.startsWith('FlexibleEnumValues:')) {
		const entriesStr = typeAndData.slice('FlexibleEnumValues:'.length);
		const enumEntries = parseEnumEntries(entriesStr);
		const enumNames = enumEntries.map((e) => e.name);
		return {
			typeName: 'FlexibleEnumValues',
			userDescription,
			enumNames,
			enumEntries: enumEntries.length > 0 ? enumEntries : undefined,
			fieldDescription,
		};
	}
	if (typeAndData.startsWith('Int32Enum:')) {
		const rest = typeAndData.slice('Int32Enum:'.length);
		let enumTypeName: string | undefined;
		let entriesStr: string;
		if (rest.startsWith('[') || rest.startsWith('{')) {
			entriesStr = rest;
		} else {
			const colonIndex = rest.indexOf(':');
			if (colonIndex > 0) {
				enumTypeName = rest.slice(0, colonIndex);
				entriesStr = rest.slice(colonIndex + 1);
			} else {
				entriesStr = rest;
			}
		}
		const enumEntries = parseEnumEntries(entriesStr);
		const enumNames = enumEntries.map((e) => e.name);
		return {
			typeName: 'Int32Enum',
			userDescription,
			enumNames,
			enumEntries: enumEntries.length > 0 ? enumEntries : undefined,
			bitflagTypeName: enumTypeName,
			fieldDescription,
		};
	}
	if (typeAndData.startsWith('Bitflags64:')) {
		const rest = typeAndData.slice('Bitflags64:'.length);
		let bitflagTypeName: string | undefined;
		let entriesStr: string;
		if (rest.startsWith('[') || rest.startsWith('{')) {
			entriesStr = rest;
		} else {
			const colonIndex = rest.indexOf(':');
			if (colonIndex > 0) {
				bitflagTypeName = rest.slice(0, colonIndex);
				entriesStr = rest.slice(colonIndex + 1);
			} else {
				entriesStr = rest;
			}
		}
		const bitflagValues = parseBitflagEntries(entriesStr);
		return {typeName: 'Bitflags64', userDescription, bitflagValues, bitflagTypeName, fieldDescription};
	}
	if (typeAndData.startsWith('Bitflags32:')) {
		const rest = typeAndData.slice('Bitflags32:'.length);
		let bitflagTypeName: string | undefined;
		let entriesStr: string;
		if (rest.startsWith('[') || rest.startsWith('{')) {
			entriesStr = rest;
		} else {
			const colonIndex = rest.indexOf(':');
			if (colonIndex > 0) {
				bitflagTypeName = rest.slice(0, colonIndex);
				entriesStr = rest.slice(colonIndex + 1);
			} else {
				entriesStr = rest;
			}
		}
		const bitflagValues = parseBitflagEntries(entriesStr);
		return {typeName: 'Bitflags32', userDescription, bitflagValues, bitflagTypeName, fieldDescription};
	}
	if (typeAndData.startsWith('Permissions:')) {
		const rest = typeAndData.slice('Permissions:'.length);
		let bitflagTypeName: string | undefined;
		let entriesStr: string;
		if (rest.startsWith('[') || rest.startsWith('{')) {
			entriesStr = rest;
		} else {
			const colonIndex = rest.indexOf(':');
			if (colonIndex > 0) {
				bitflagTypeName = rest.slice(0, colonIndex);
				entriesStr = rest.slice(colonIndex + 1);
			} else {
				entriesStr = rest;
			}
		}
		const bitflagValues = parseBitflagEntries(entriesStr);
		return {typeName: 'Permissions', userDescription, bitflagValues, bitflagTypeName, fieldDescription};
	}
	return {typeName: typeAndData, userDescription, fieldDescription};
}
