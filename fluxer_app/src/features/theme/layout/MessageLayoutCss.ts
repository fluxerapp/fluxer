// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	MARKDOWN_BLOCK_ATTRIBUTE,
	MarkdownBlock,
	type MarkdownBlockName,
} from '@app/features/messaging/components/markdown/renderers/common/MarkdownBlockAttributes';
import {
	COMPACT_MARKDOWN_ATTRIBUTE,
	COMPACT_MESSAGE_PREFIX_ATTRIBUTE,
} from '@app/features/theme/layout/MessageLayoutAttributes';
import {getMessageLayoutCssVariables} from '@app/features/theme/layout/MessageLayoutSpec';

function renderRule(selector: string, declarations: ReadonlyArray<string>): string {
	return `${selector} {\n${declarations.map((declaration) => `\t${declaration}`).join('\n')}\n}`;
}

function attributeSelector(attribute: string, value?: string): string {
	return value === undefined ? `[${attribute}]` : `[${attribute}='${value}']`;
}

function markdownBlockSelector(block: MarkdownBlockName): string {
	return `${attributeSelector(COMPACT_MARKDOWN_ATTRIBUTE)} ${attributeSelector(MARKDOWN_BLOCK_ATTRIBUTE, block)}`;
}

function renderRootVariables(variables: Record<string, string>): string {
	const lines = Object.entries(variables).map(([name, value]) => `\t${name}: ${value};`);
	return `:root {\n${lines.join('\n')}\n}`;
}

export function renderCompactMarkdownLayoutCss(): string {
	const compact = attributeSelector(COMPACT_MARKDOWN_ATTRIBUTE);
	const markdownBlock = `${compact} ${attributeSelector(MARKDOWN_BLOCK_ATTRIBUTE)}`;
	return [
		renderRule(`${compact} *`, ['text-indent: 0;']),
		renderRule(`${compact} ul,\n${compact} ol`, ['margin-inline-start: 1rem;']),
		renderRule(`${compact} li > ul,\n${compact} li > ol`, ['margin-inline-start: 0;']),
		renderRule(markdownBlock, ['display: block;', 'margin-inline-start: 0;', 'text-indent: 0;']),
		renderRule(
			[
				markdownBlockSelector(MarkdownBlock.TableContainer),
				markdownBlockSelector(MarkdownBlock.Code),
				markdownBlockSelector(MarkdownBlock.LatexCode),
				markdownBlockSelector(MarkdownBlock.Alert),
				markdownBlockSelector(MarkdownBlock.Blockquote),
			].join(',\n'),
			['margin-block: var(--message-compact-markdown-block-gap);'],
		),
		renderRule(`${markdownBlock}:first-child`, ['margin-block-start: 0;']),
		renderRule(
			`${compact} ${attributeSelector(COMPACT_MESSAGE_PREFIX_ATTRIBUTE)} + ${attributeSelector(MARKDOWN_BLOCK_ATTRIBUTE)}`,
			['margin-block-start: 0;'],
		),
		renderRule(`${markdownBlock}:last-child`, ['margin-block-end: 0;']),
		renderRule(markdownBlockSelector(MarkdownBlock.Blockquote), [
			'gap: var(--message-compact-markdown-blockquote-gap);',
		]),
		renderRule(markdownBlockSelector(MarkdownBlock.BlockquoteDivider), [
			'margin-inline-end: var(--message-compact-markdown-blockquote-divider-margin-end);',
		]),
		renderRule(markdownBlockSelector(MarkdownBlock.Alert), [
			'padding-inline: var(--message-compact-markdown-alert-padding-inline);',
			'padding-block: var(--message-compact-markdown-alert-padding-block);',
		]),
		renderRule(markdownBlockSelector(MarkdownBlock.BlockquoteContent), [
			'margin: 0;',
			'padding: 0;',
			'display: block;',
			'width: 100%;',
			'word-break: break-word;',
		]),
		renderRule(`${markdownBlockSelector(MarkdownBlock.BlockquoteContent)} p`, ['margin-inline-start: 0;']),
	].join('\n\n');
}

export function renderMessageLayoutCss(): string {
	return `/* SPDX-License-Identifier: AGPL-3.0-or-later */\n\n${renderRootVariables(getMessageLayoutCssVariables())}\n\n${renderCompactMarkdownLayoutCss()}\n`;
}
