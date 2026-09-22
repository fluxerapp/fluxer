// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	THEME_VARIABLE_NAMES,
	THEME_VARIABLES,
	type ThemeVariableDefinition,
} from '@app/features/user/components/modals/tabs/appearance_tab/theme/ThemeConstants';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export interface TokenGroupDefinition {
	id: string;
	fallbackLabel: string;
	variables: ReadonlyArray<string>;
}

const GROUP_ORDER: ReadonlyArray<string> = [
	'typography',
	'surfaces',
	'headers',
	'text',
	'brand',
	'status',
	'buttons',
	'borders',
	'alerts',
	'markup',
	'code',
	'tables',
	'messages',
	'forms',
	'layout',
	'scrolling',
	'motion',
	'layering',
	'media',
	'emoji',
	'other',
];

const GROUP_LABEL_DESCRIPTORS: Readonly<Record<string, MessageDescriptor | undefined>> = {
	typography: msg({
		message: 'Typography',
		comment: 'Collapsible group heading in the Theme Studio tokens list, for font and text-sizing tokens.',
	}),
	surfaces: msg({
		message: 'Surfaces',
		comment: 'Collapsible group heading in the Theme Studio tokens list, for background surface tokens.',
	}),
	text: msg({
		message: 'Text',
		comment: 'Collapsible group heading in the Theme Studio tokens list, for text color tokens.',
	}),
	brand: msg({
		message: 'Brand & accents',
		comment: 'Collapsible group heading in the Theme Studio tokens list, for brand and accent color tokens.',
	}),
	status: msg({
		message: 'Status indicators',
		comment: 'Collapsible group heading in the Theme Studio tokens list, for presence and status color tokens.',
	}),
	buttons: msg({
		message: 'Buttons',
		comment: 'Collapsible group heading in the Theme Studio tokens list, for button color tokens.',
	}),
	borders: msg({
		message: 'Borders & focus',
		comment: 'Collapsible group heading in the Theme Studio tokens list, for border and focus ring tokens.',
	}),
	alerts: msg({
		message: 'Alerts & callouts',
		comment: 'Collapsible group heading in the Theme Studio tokens list, for alert and callout tokens.',
	}),
	markup: msg({
		message: 'Markup & mentions',
		comment: 'Collapsible group heading in the Theme Studio tokens list, for markdown and mention tokens.',
	}),
	code: msg({
		message: 'Code & terminal',
		comment: 'Collapsible group heading in the Theme Studio tokens list, for code block and terminal tokens.',
	}),
	tables: msg({
		message: 'Tables',
		comment: 'Collapsible group heading in the Theme Studio tokens list, for table tokens.',
	}),
	messages: msg({
		message: 'Messages',
		comment: 'Collapsible group heading in the Theme Studio tokens list, for chat message tokens.',
	}),
	forms: msg({
		message: 'Forms',
		comment: 'Collapsible group heading in the Theme Studio tokens list, for form input tokens.',
	}),
	layout: msg({
		message: 'Layout',
		comment: 'Collapsible group heading in the Theme Studio tokens list, for spacing and sizing tokens.',
	}),
	scrolling: msg({
		message: 'Scrolling',
		comment: 'Collapsible group heading in the Theme Studio tokens list, for scrollbar tokens.',
	}),
	motion: msg({
		message: 'Motion',
		comment: 'Collapsible group heading in the Theme Studio tokens list, for animation and transition tokens.',
	}),
	layering: msg({
		message: 'Layering',
		comment: 'Collapsible group heading in the Theme Studio tokens list, for stacking order tokens.',
	}),
	media: msg({
		message: 'Media',
		comment: 'Collapsible group heading in the Theme Studio tokens list, for image and video tokens.',
	}),
	emoji: msg({
		message: 'Emoji',
		comment: 'Collapsible group heading in the Theme Studio tokens list, for emoji tokens.',
	}),
	other: msg({
		message: 'Other',
		comment: 'Collapsible group heading in the Theme Studio tokens list, for tokens that fit no other group.',
	}),
};

export function getTokenGroupLabelDescriptor(groupId: string): MessageDescriptor | null {
	return GROUP_LABEL_DESCRIPTORS[groupId] ?? null;
}

export const TOKEN_VARIABLES_BY_NAME: ReadonlyMap<string, ThemeVariableDefinition> = new Map(
	THEME_VARIABLES.map((definition) => [definition.name, definition]),
);

export function getTokenVariableDefinition(variableName: string): ThemeVariableDefinition | null {
	return TOKEN_VARIABLES_BY_NAME.get(variableName) ?? null;
}

function buildGroups(): ReadonlyArray<TokenGroupDefinition> {
	const variablesByGroup = new Map<string, Array<string>>();
	const labelsByGroup = new Map<string, string>();
	for (const definition of THEME_VARIABLES) {
		const groupVariables = variablesByGroup.get(definition.groupId) ?? [];
		groupVariables.push(definition.name);
		variablesByGroup.set(definition.groupId, groupVariables);
		labelsByGroup.set(definition.groupId, definition.groupLabel);
	}
	const groupIds = [
		...GROUP_ORDER.filter((groupId) => variablesByGroup.has(groupId)),
		...[...variablesByGroup.keys()].filter((groupId) => !GROUP_ORDER.includes(groupId)).sort(),
	];
	return groupIds.map((groupId) => ({
		id: groupId,
		fallbackLabel: labelsByGroup.get(groupId) ?? humanizeVariableName(groupId),
		variables: variablesByGroup.get(groupId) ?? [],
	}));
}

export const TOKEN_GROUPS: ReadonlyArray<TokenGroupDefinition> = buildGroups();
export const DEFAULT_EXPANDED_GROUP_IDS: ReadonlyArray<string> = ['surfaces', 'text', 'brand', 'messages'];

export function assertTokenGroupsCoverConstants(): void {
	const grouped = new Set<string>();
	for (const group of TOKEN_GROUPS) {
		for (const variable of group.variables) {
			if (grouped.has(variable)) {
				throw new Error(`Token Studio: variable ${variable} is assigned to more than one group`);
			}
			grouped.add(variable);
		}
	}
	const expected = new Set<string>(THEME_VARIABLE_NAMES);
	for (const variable of expected) {
		if (!grouped.has(variable)) {
			throw new Error(`Token Studio: variable ${variable} is missing from TokenGroups`);
		}
	}
	for (const variable of grouped) {
		if (!expected.has(variable)) {
			throw new Error(`Token Studio: variable ${variable} is not present in theme constants`);
		}
	}
}

if (typeof process !== 'undefined' && process.env?.NODE_ENV !== 'production') {
	try {
		assertTokenGroupsCoverConstants();
	} catch (error) {
		console.error('[ThemeStudio] TokenGroups taxonomy mismatch:', error);
	}
}

export function humanizeVariableName(variable: string): string {
	return variable.replace(/^--/, '').replace(/[-_]/g, ' ');
}
