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
];

const darkPalette = {
	plain: '#e5e4e9',
	punctuation: '#a49eb3',
	comment: '#9d97aa',
	key: '#6ccff9',
	string: '#4eda99',
	number: '#f48134',
	constant: '#ca7bef',
	keyword: '#ed82a6',
	function: '#9491f3',
	variable: '#f37777',
	invalid: '#f88181',
};

const lightPalette = {
	plain: '#31363f',
	punctuation: '#5a6272',
	comment: '#5c6370',
	key: '#066993',
	string: '#0c6e40',
	number: '#a04508',
	constant: '#8720b6',
	keyword: '#aa1849',
	function: '#2a24cc',
	variable: '#ad1f21',
	invalid: '#a51215',
};

export const fluxerDarkCodeTheme: CodeTheme = {
	name: 'fluxer-dark',
	type: 'dark',
	colors: {
		'editor.background': '#232129',
		'editor.foreground': darkPalette.plain,
		'editor.selectionBackground': '#6ccff938',
		focusBorder: '#6ccff9',
		'scrollbarSlider.background': '#afabba59',
		'scrollbarSlider.hoverBackground': '#afabba8c',
		'terminal.background': '#232129',
		'titleBar.activeBackground': '#2a2730',
	},
	tokenColors: buildTokenColors(darkPalette),
};

export const fluxerLightCodeTheme: CodeTheme = {
	name: 'fluxer-light',
	type: 'light',
	colors: {
		'editor.background': '#f6f7f8',
		'editor.foreground': lightPalette.plain,
		'editor.selectionBackground': '#099ddc2e',
		focusBorder: '#07709d',
		'scrollbarSlider.background': '#4e545f59',
		'scrollbarSlider.hoverBackground': '#4e545f8c',
		'terminal.background': '#f6f7f8',
		'titleBar.activeBackground': '#ebecef',
	},
	tokenColors: buildTokenColors(lightPalette),
};
