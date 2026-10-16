// SPDX-License-Identifier: AGPL-3.0-or-later

export const MARKDOWN_BLOCK_ATTRIBUTE = 'data-markdown-block';
const MARKDOWN_TABLE_ATTRIBUTE = 'data-markdown-table';
export const MarkdownBlock = {
	Alert: 'alert',
	Blockquote: 'blockquote',
	BlockquoteContent: 'blockquote-content',
	BlockquoteDivider: 'blockquote-divider',
	Code: 'code',
	Heading: 'heading',
	LatexCode: 'latex-code',
	List: 'list',
	Subtext: 'subtext',
	TableContainer: 'table-container',
} as const;

export type MarkdownBlockName = (typeof MarkdownBlock)[keyof typeof MarkdownBlock];

export function markdownBlockProps(block: MarkdownBlockName | undefined): {
	[MARKDOWN_BLOCK_ATTRIBUTE]?: MarkdownBlockName;
} {
	return block === undefined ? {} : {[MARKDOWN_BLOCK_ATTRIBUTE]: block};
}

export function markdownTableProps(): {[MARKDOWN_TABLE_ATTRIBUTE]: 'true'} {
	return {[MARKDOWN_TABLE_ATTRIBUTE]: 'true'};
}
