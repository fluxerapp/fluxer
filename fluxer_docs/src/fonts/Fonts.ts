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
	fallbacks: ['sans-serif'],
	name: 'Radio Canada Big',
	provider: fontsource,
	styles: ['normal'],
	subsets: ['latin', 'latin-ext'],
	weights: ['400 700'],
};
