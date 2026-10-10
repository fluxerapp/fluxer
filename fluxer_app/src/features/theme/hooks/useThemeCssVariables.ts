// SPDX-License-Identifier: AGPL-3.0-or-later

import {HdrDisplayMode} from '@app/features/accessibility/state/Accessibility';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {getInstanceBrandVariables, INSTANCE_BRAND_VARIABLES} from '@app/features/theme/utils/InstanceBrandColors';
import {useLayoutEffect} from 'react';

interface ThemeCssVariablesOptions {
	effectiveTheme: string;
	saturationFactor: number;
	alwaysUnderlineLinks: boolean;
	dimStrikethroughText: boolean;
	enableTextSelection: boolean;
	fontSize: number;
	messageGutter: number;
	messageGroupSpacing: number;
	hdrDisplayMode: HdrDisplayMode;
	instanceThemeColor: string | null;
}

export function useThemeCssVariables({
	effectiveTheme,
	saturationFactor,
	alwaysUnderlineLinks,
	dimStrikethroughText,
	enableTextSelection,
	fontSize,
	messageGutter,
	messageGroupSpacing,
	hdrDisplayMode,
	instanceThemeColor,
}: ThemeCssVariablesOptions): void {
	useLayoutEffect(() => {
		const htmlNode = document.documentElement;
		const themeClass = `theme-${effectiveTheme}`;
		for (const existingClass of Array.from(htmlNode.classList)) {
			if (existingClass.startsWith('theme-') && existingClass !== themeClass) {
				htmlNode.classList.remove(existingClass);
			}
		}
		htmlNode.classList.add(themeClass);
		htmlNode.style.setProperty('--saturation-factor', saturationFactor.toString());
		htmlNode.style.setProperty('--user-select', enableTextSelection ? 'auto' : 'none');
		htmlNode.style.setProperty('--font-size', remFromPx(fontSize));
		htmlNode.style.setProperty('--chat-horizontal-padding', remFromPx(messageGutter));
		htmlNode.style.setProperty('--message-group-spacing', remFromPx(messageGroupSpacing));
		htmlNode.style.setProperty('dynamic-range-limit', hdrDisplayMode === HdrDisplayMode.FULL ? 'high' : 'standard');
		if (alwaysUnderlineLinks) {
			htmlNode.style.setProperty('--link-decoration', 'underline');
		} else {
			htmlNode.style.removeProperty('--link-decoration');
		}
		if (dimStrikethroughText) {
			htmlNode.style.setProperty('--markup-strikethrough-color', 'color-mix(in srgb, currentColor 55%, transparent)');
		} else {
			htmlNode.style.removeProperty('--markup-strikethrough-color');
		}
	}, [
		effectiveTheme,
		saturationFactor,
		alwaysUnderlineLinks,
		dimStrikethroughText,
		enableTextSelection,
		fontSize,
		messageGutter,
		messageGroupSpacing,
		hdrDisplayMode,
	]);
	useLayoutEffect(() => {
		if (instanceThemeColor === null) return;
		const variables = getInstanceBrandVariables(instanceThemeColor);
		if (variables === null) return;
		const htmlNode = document.documentElement;
		for (const name of INSTANCE_BRAND_VARIABLES) {
			htmlNode.style.setProperty(name, variables[name]);
		}
		return () => {
			for (const name of INSTANCE_BRAND_VARIABLES) {
				htmlNode.style.removeProperty(name);
			}
		};
	}, [instanceThemeColor]);
	useLayoutEffect(() => {
		const htmlNode = document.documentElement;
		return () => {
			for (const existingClass of Array.from(htmlNode.classList)) {
				if (existingClass.startsWith('theme-')) {
					htmlNode.classList.remove(existingClass);
				}
			}
			htmlNode.style.removeProperty('--saturation-factor');
			htmlNode.style.removeProperty('--link-decoration');
			htmlNode.style.removeProperty('--markup-strikethrough-color');
			htmlNode.style.removeProperty('--user-select');
			htmlNode.style.removeProperty('--font-size');
			htmlNode.style.removeProperty('--chat-horizontal-padding');
			htmlNode.style.removeProperty('--message-group-spacing');
			htmlNode.style.removeProperty('dynamic-range-limit');
		};
	}, []);
}
