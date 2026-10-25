// SPDX-License-Identifier: AGPL-3.0-or-later

import ThemeLibrary from '@app/features/theme/state/ThemeLibrary';
import ThemeProfile from '@app/features/theme/state/ThemeProfile';
import type {ThemeVariableDefinition} from '@app/features/theme/variables/ThemeVariableManifest';
import {getThemeDefaultVariables} from '@app/features/theme_settings/utils/ThemeDefaultVariables';
import {getTokenGroupLabelDescriptor, getTokenVariableDefinition} from '@app/features/theme_settings/utils/TokenGroups';
import {i18n} from '@lingui/core';

const DEFINED_VARIABLE_PATTERN = /(--[A-Za-z_][\w-]*)\s*:/g;

let lastDefinedSource = '';
let lastDefinedVariables: ReadonlySet<string> = new Set();

function collectDefinedVariables(css: string): ReadonlySet<string> {
	if (css === lastDefinedSource) return lastDefinedVariables;
	const names = new Set<string>();
	for (const match of css.matchAll(DEFINED_VARIABLE_PATTERN)) names.add(match[1]);
	lastDefinedSource = css;
	lastDefinedVariables = names;
	return names;
}

export function getEnabledThemeVariables(): ReadonlySet<string> {
	return collectDefinedVariables(ThemeLibrary.themeCssFor(ThemeProfile.enabledThemeIds));
}

export function getFileVariables(doc: string): ReadonlySet<string> {
	const names = new Set<string>();
	for (const match of doc.matchAll(DEFINED_VARIABLE_PATTERN)) names.add(match[1]);
	return names;
}

export function isColorVariable(name: string): boolean {
	return getTokenVariableDefinition(name)?.kind === 'color';
}

export function getVariableGroupLabel(definition: ThemeVariableDefinition): string {
	const descriptor = getTokenGroupLabelDescriptor(definition.groupId);
	return descriptor ? i18n._(descriptor) : definition.groupLabel;
}

export function readCurrentVariableValue(name: string): string {
	return getComputedStyle(document.documentElement).getPropertyValue(name).trim();
}

export function readDefaultVariableValues(name: string): {dark: string | null; light: string | null} {
	return {
		dark: getThemeDefaultVariables('dark')[name] ?? null,
		light: getThemeDefaultVariables('light')[name] ?? null,
	};
}

export function readsAsColor(value: string): boolean {
	if (!value) return false;
	return CSS.supports('color', value);
}

export function createSwatchElement(ownerDocument: Document, color: string, className: string): HTMLSpanElement {
	const swatch = ownerDocument.createElement('span');
	swatch.className = className;
	swatch.style.background = color;
	swatch.setAttribute('aria-hidden', 'true');
	return swatch;
}
