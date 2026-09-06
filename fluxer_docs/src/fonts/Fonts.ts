// SPDX-License-Identifier: AGPL-3.0-or-later

import {fontProviders} from 'astro/config';

const fontsource = fontProviders.fontsource();

type NonEmpty<T> = [T, ...Array<T>];

type AstroFontFamily = {
	name: string;
	cssVariable: `--${string}`;
	provider: typeof fontsource;
	weights: NonEmpty<string>;
	styles: NonEmpty<'normal' | 'italic'>;
	subsets: NonEmpty<string>;
	fallbacks: Array<string>;
};

export const radioCanadaBigFont: AstroFontFamily = {
	cssVariable: '--font-radio-canada-big',
	fallbacks: [],
	name: 'Radio Canada Big',
	provider: fontsource,
	styles: ['normal'],
	subsets: ['latin', 'latin-ext'],
	weights: ['400 700'],
};

export const notoSansFont: AstroFontFamily = {
	cssVariable: '--font-noto-sans',
	fallbacks: [],
	name: 'Noto Sans',
	provider: fontsource,
	styles: ['normal'],
	subsets: ['cyrillic', 'cyrillic-ext', 'devanagari', 'greek', 'greek-ext', 'vietnamese'],
	weights: ['400 700'],
};

export const jetBrainsMonoFont: AstroFontFamily = {
	cssVariable: '--font-jetbrains-mono',
	fallbacks: [],
	name: 'JetBrains Mono',
	provider: fontsource,
	styles: ['normal'],
	subsets: ['latin', 'latin-ext'],
	weights: ['400 700'],
};
