// SPDX-License-Identifier: AGPL-3.0-or-later

import {HdrDisplayMode} from '@app/features/accessibility/state/Accessibility';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
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
		return () => {
			htmlNode.classList.remove(themeClass);
			htmlNode.style.removeProperty('--saturation-factor');
			htmlNode.style.removeProperty('--link-decoration');
			htmlNode.style.removeProperty('--markup-strikethrough-color');
			htmlNode.style.removeProperty('--user-select');
			htmlNode.style.removeProperty('--font-size');
			htmlNode.style.removeProperty('--chat-horizontal-padding');
			htmlNode.style.removeProperty('--message-group-spacing');
			htmlNode.style.removeProperty('dynamic-range-limit');
		};
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
}
