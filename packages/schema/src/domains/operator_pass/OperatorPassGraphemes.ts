// SPDX-License-Identifier: AGPL-3.0-or-later

let graphemes: Intl.Segmenter | undefined;

export function segmentOperatorPassText(value: string): Intl.Segments {
	graphemes ??= new Intl.Segmenter('en', {granularity: 'grapheme'});
	return graphemes.segment(value);
}
