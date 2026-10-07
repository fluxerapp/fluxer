// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {HdrDisplayMode} from '@app/features/accessibility/state/Accessibility';
import {useThemeCssVariables} from '@app/features/theme/hooks/useThemeCssVariables';
import {act, useLayoutEffect} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, expect, test} from 'vitest';

interface RootStyleSample {
	readonly themeClasses: ReadonlyArray<string>;
	readonly fontSize: string;
	readonly chatPadding: string;
	readonly groupSpacing: string;
}

let root: Root | null = null;
let container: HTMLElement | null = null;

function sampleRootStyle(): RootStyleSample {
	const htmlNode = document.documentElement;
	return {
		themeClasses: Array.from(htmlNode.classList).filter((name) => name.startsWith('theme-')),
		fontSize: htmlNode.style.getPropertyValue('--font-size'),
		chatPadding: htmlNode.style.getPropertyValue('--chat-horizontal-padding'),
		groupSpacing: htmlNode.style.getPropertyValue('--message-group-spacing'),
	};
}

function MeasuringChild({revision, samples}: {revision: string; samples: Array<RootStyleSample>}) {
	useLayoutEffect(() => {
		samples.push(sampleRootStyle());
	}, [revision, samples]);
	return null;
}

function ThemedApp({
	theme,
	groupSpacing,
	saturation,
	samples,
}: {
	theme: string;
	groupSpacing: number;
	saturation: number;
	samples: Array<RootStyleSample>;
}) {
	useThemeCssVariables({
		effectiveTheme: theme,
		saturationFactor: saturation,
		alwaysUnderlineLinks: false,
		dimStrikethroughText: false,
		enableTextSelection: true,
		fontSize: 16,
		messageGutter: 16,
		messageGroupSpacing: groupSpacing,
		hdrDisplayMode: HdrDisplayMode.FULL,
	});
	return <MeasuringChild revision={`${theme}:${groupSpacing}:${saturation}`} samples={samples} />;
}

beforeEach(() => {
	(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;
	container = document.createElement('div');
	document.body.appendChild(container);
	root = createRoot(container);
});

afterEach(() => {
	act(() => root?.unmount());
	container?.remove();
	root = null;
	container = null;
});

test('a settings change never exposes a frame without the theme class or layout variables', () => {
	const samples: Array<RootStyleSample> = [];
	act(() => root?.render(<ThemedApp theme="dark" groupSpacing={16} saturation={1} samples={samples} />));
	samples.length = 0;
	act(() => root?.render(<ThemedApp theme="dark" groupSpacing={16} saturation={0.5} samples={samples} />));
	act(() => root?.render(<ThemedApp theme="dark" groupSpacing={8} saturation={0.5} samples={samples} />));
	act(() => root?.render(<ThemedApp theme="coal" groupSpacing={8} saturation={0.5} samples={samples} />));
	expect(samples).toHaveLength(3);
	expect(samples.map((sample) => sample.themeClasses)).toEqual([['theme-dark'], ['theme-dark'], ['theme-dark']]);
	expect(samples.map((sample) => sample.chatPadding)).toEqual(['1rem', '1rem', '1rem']);
	expect(samples.map((sample) => sample.fontSize)).toEqual(['1rem', '1rem', '1rem']);
	expect(samples.map((sample) => sample.groupSpacing)).toEqual(['1rem', '1rem', '0.5rem']);
	expect(sampleRootStyle()).toEqual({
		themeClasses: ['theme-coal'],
		fontSize: '1rem',
		chatPadding: '1rem',
		groupSpacing: '0.5rem',
	});
});

test('unmounting removes the theme class and every variable it set', () => {
	act(() => root?.render(<ThemedApp theme="dark" groupSpacing={16} saturation={1} samples={[]} />));
	act(() => root?.unmount());
	root = createRoot(container as HTMLElement);
	expect(sampleRootStyle()).toEqual({themeClasses: [], fontSize: '', chatPadding: '', groupSpacing: ''});
	expect(document.documentElement.style.getPropertyValue('--saturation-factor')).toBe('');
});
