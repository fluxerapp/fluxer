// SPDX-License-Identifier: AGPL-3.0-or-later

type ThemeSetting = {
	scope?: Array<string>;
	settings: {foreground?: string; fontStyle?: string};
};

type CodeTheme = {
	name: string;
	type: 'dark' | 'light';
	colors: Record<string, string>;
	tokenColors: Array<ThemeSetting>;
};

const buildTokenColors = (p: Record<string, string>): Array<ThemeSetting> => [
	{scope: ['comment', 'punctuation.definition.comment', 'string.comment'], settings: {foreground: p.comment}},
	{
		scope: [
			'punctuation',
			'punctuation.definition',
			'punctuation.separator',
			'punctuation.terminator',
			'punctuation.section',
			'meta.brace',
			'meta.delimiter',
		],
		settings: {foreground: p.punctuation},
	},
	{
		scope: [
			'support.type.property-name',
			'meta.object-literal.key',
			'entity.name.tag',
			'entity.name.tag.yaml',
			'keyword.other.definition.ini',
			'support.type.property-name.json',
			'variable.other.key',
		],
		settings: {foreground: p.key},
	},
	{
		scope: ['string', 'string.quoted', 'string.unquoted', 'markup.inline.raw', 'meta.attribute-selector'],
		settings: {foreground: p.string},
	},
	{scope: ['constant.character.escape', 'constant.other.placeholder'], settings: {foreground: p.number}},
	{scope: ['constant.numeric', 'constant.other.timestamp'], settings: {foreground: p.number}},
	{
		scope: ['constant.language', 'constant.other', 'support.constant', 'variable.language'],
		settings: {foreground: p.constant},
	},
	{
		scope: ['keyword', 'keyword.control', 'keyword.operator.word', 'storage', 'storage.type', 'storage.modifier'],
		settings: {foreground: p.keyword},
	},
	{
		scope: [
			'entity.name.function',
			'support.function',
			'entity.name.command',
			'meta.function-call',
			'entity.name.type',
			'support.class',
		],
		settings: {foreground: p.function},
	},
	{
		scope: ['variable', 'variable.other', 'variable.parameter', 'meta.definition.variable', 'entity.name.variable'],
		settings: {foreground: p.key},
	},
	{scope: ['invalid', 'invalid.illegal', 'markup.deleted'], settings: {foreground: p.invalid}},
	{scope: ['markup.inserted'], settings: {foreground: p.string}},
	{scope: ['keyword.operator', 'meta.separator'], settings: {foreground: p.plain}},
	{
		scope: ['string.unquoted.argument.shell', 'meta.argument.shell', 'string.unquoted.argument'],
		settings: {foreground: p.plain},
	},
];

const darkPalette = {
	plain: '#e2e0e8',
	punctuation: '#a29daf',
	comment: '#a29daf',
	key: '#e2e0e8',
	string: '#a3d4b4',
	number: '#a3d4b4',
	constant: '#a3d4b4',
	keyword: '#b9b6f6',
	function: '#e2e0e8',
	variable: '#e2e0e8',
	invalid: '#f19a9a',
};

const lightPalette = {
	plain: '#2b2f3a',
	punctuation: '#5d6270',
	comment: '#5d6270',
	key: '#2b2f3a',
	string: '#2c6a43',
	number: '#2c6a43',
	constant: '#2c6a43',
	keyword: '#4842b5',
	function: '#2b2f3a',
	variable: '#2b2f3a',
	invalid: '#b42323',
};

export const fluxerDarkCodeTheme: CodeTheme = {
	name: 'fluxer-dark',
	type: 'dark',
	colors: {
		'editor.background': '#16151a',
		'editor.foreground': darkPalette.plain,
		'editor.selectionBackground': '#b9b6f638',
		focusBorder: '#b9b6f6',
		'scrollbarSlider.background': '#a29daf59',
		'scrollbarSlider.hoverBackground': '#a29daf8c',
		'terminal.background': '#16151a',
		'titleBar.activeBackground': '#1d1c22',
	},
	tokenColors: buildTokenColors(darkPalette),
};

export const fluxerLightCodeTheme: CodeTheme = {
	name: 'fluxer-light',
	type: 'light',
	colors: {
		'editor.background': '#f6f7f9',
		'editor.foreground': lightPalette.plain,
		'editor.selectionBackground': '#4842b52e',
		focusBorder: '#4842b5',
		'scrollbarSlider.background': '#5d627059',
		'scrollbarSlider.hoverBackground': '#5d62708c',
		'terminal.background': '#f6f7f9',
		'titleBar.activeBackground': '#f2f3f6',
	},
	tokenColors: buildTokenColors(lightPalette),
};
