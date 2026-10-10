// SPDX-License-Identifier: AGPL-3.0-or-later

export type ListFormatStyle = 'long' | 'short' | 'narrow';
export type ListFormatType = 'conjunction' | 'disjunction' | 'unit';

export interface ListFormatOptions {
	style?: ListFormatStyle;
	type?: ListFormatType;
}

export interface ListFormatterConfig extends ListFormatOptions {
	locale?: string;
}

export interface ResolvedListFormatterConfig {
	locale: string;
	style: ListFormatStyle;
	type: ListFormatType;
}
