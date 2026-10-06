// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	_preloadArboriumForTests,
	isSupportedHighlightLanguage,
	useArboriumHighlightedHtml,
} from '@app/features/code_highlighting/utils/ArboriumHighlighting';
import {COPY_CODE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {convertAnsiToHtml} from '@app/features/messaging/components/markdown/renderers/common/AnsiConverter';
import {
	MarkdownBlock,
	markdownBlockProps,
} from '@app/features/messaging/components/markdown/renderers/common/MarkdownBlockAttributes';
import {
	isRestrictedInlineContext,
	type RendererProps,
} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import type {CodeBlockNode, InlineCodeNode} from '@app/features/messaging/utils/markdown/parser/Nodes';
import {Logger} from '@app/features/platform/utils/AppLogger';
import codeElementsStyles from '@app/features/theme/styles/CodeElements.module.css';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import {CopyButton} from '@app/features/ui/components/CopyButton';
import {
	MAX_KATEX_RENDER_CONTROL_SEQUENCE_COUNT,
	MAX_KATEX_RENDER_SOURCE_LENGTH,
} from '@fluxer/constants/src/LimitConstants';
import {msg} from '@lingui/core/macro';
import {clsx} from 'clsx';
import type katexDefault from 'katex';
import React, {useEffect, useMemo, useState} from 'react';

const ERROR_RENDERING_LATEX_DESCRIPTOR = msg({
	message: 'Error rendering LaTeX: {errorAsErrorMessageI18nMsgUnknownError}',
	comment:
		'Error message in the messaging code elements. Preserve {errorAsErrorMessageI18nMsgUnknownError}; it is inserted by code.',
});
const UNKNOWN_ERROR_DESCRIPTOR = msg({
	message: 'Unknown error',
	comment: 'Fallback error text shown when LaTeX rendering fails without an error message.',
});
const logger = new Logger('CodeElementsRenderer');
const KATEX_CODE_BLOCK_LANGUAGES = new Set(['katex', 'latex', 'tex']);
const ANSI_CODE_BLOCK_LANGUAGES = new Set(['ansi']);
const KATEX_CONTROL_SEQUENCE_REGEX = /\\(?:[a-zA-Z@]+|.)/gu;

type KatexModule = typeof katexDefault;

let katexPromise: Promise<KatexModule> | null = null;
let katexModule: KatexModule | null = null;

function loadKatex(): Promise<KatexModule> {
	if (!katexPromise) {
		katexPromise = (async () => {
			const [{default: kt}] = await Promise.all([import('katex'), import('katex/dist/katex.min.css')]);
			katexModule = kt;
			return kt;
		})();
	}
	return katexPromise;
}

export async function _preloadForTests(): Promise<void> {
	await Promise.all([_preloadArboriumForTests(), loadKatex()]);
}

function useLazyChunkLoad(load: () => Promise<unknown>, isLoaded: boolean, shouldLoad: boolean): void {
	const [, setTick] = useState(0);
	useEffect(() => {
		if (!shouldLoad || isLoaded) {
			return;
		}
		let cancelled = false;
		load().then(() => {
			if (!cancelled) {
				setTick((tick) => tick + 1);
			}
		});
		return () => {
			cancelled = true;
		};
	}, [load, isLoaded, shouldLoad]);
}

function normaliseCodeBlockLanguage(language?: string): string | undefined {
	if (!language) {
		return undefined;
	}
	const [primaryLanguage] = language.trim().split(/\s+/u);
	if (!primaryLanguage) {
		return undefined;
	}
	return primaryLanguage.toLowerCase();
}

function countKatexControlSequences(content: string): number {
	return Array.from(content.matchAll(KATEX_CONTROL_SEQUENCE_REGEX)).length;
}

function shouldRenderKatexSource(content: string): boolean {
	if (content.length > MAX_KATEX_RENDER_SOURCE_LENGTH) {
		return false;
	}
	return countKatexControlSequences(content) <= MAX_KATEX_RENDER_CONTROL_SEQUENCE_COUNT;
}

export function CodeBlockRenderer({node, id, options}: RendererProps<CodeBlockNode>): React.ReactElement {
	const {content} = node;
	if (isRestrictedInlineContext(options.context)) {
		return (
			<code
				key={id}
				className={markupStyles.inline}
				data-flx="messaging.markdown.renderers.common.code-elements.code-block-renderer.code"
			>
				{content.replace(/\n/g, ' ').replace(/\s+/g, ' ').trim()}
			</code>
		);
	}
	return (
		<RichCodeBlockRenderer
			node={node}
			id={id}
			options={options}
			data-flx="messaging.markdown.renderers.common.code-elements.code-block-renderer.rich-code-block-renderer"
		/>
	);
}

function RichCodeBlockRenderer({
	node,
	id,
	options,
}: Pick<RendererProps<CodeBlockNode>, 'node' | 'id' | 'options'>): React.ReactElement {
	const i18n = options.i18n!;
	const {content, language} = node;
	const normalisedLanguage = normaliseCodeBlockLanguage(language);
	const isKatexBlock = Boolean(normalisedLanguage && KATEX_CODE_BLOCK_LANGUAGES.has(normalisedLanguage));
	const isAnsiBlock = Boolean(normalisedLanguage && ANSI_CODE_BLOCK_LANGUAGES.has(normalisedLanguage));
	const shouldAttemptKatexRender = useMemo(
		() => Boolean(isKatexBlock && shouldRenderKatexSource(content)),
		[content, isKatexBlock],
	);
	useLazyChunkLoad(loadKatex, katexModule !== null, isKatexBlock && shouldAttemptKatexRender);
	const katex = katexModule;
	const shouldHighlight = !isKatexBlock && !isAnsiBlock && Boolean(normalisedLanguage);
	const highlightedHtml = useArboriumHighlightedHtml(
		shouldHighlight ? normalisedLanguage : null,
		shouldHighlight ? content : null,
	);
	const copyButton = (
		<CopyButton
			value={content}
			label={COPY_CODE_DESCRIPTOR}
			className={markupStyles.codeActions}
			visibleClassName={markupStyles.codeActionsVisible}
			iconClassName={codeElementsStyles.icon}
			data-flx="messaging.markdown.renderers.common.code-elements.rich-code-block-renderer.copy-button"
		/>
	);
	const plainCodeBlock = (
		<div
			key={id}
			className={markupStyles.codeContainer}
			data-flx="messaging.markdown.renderers.common.code-elements.rich-code-block-renderer.div--2"
			{...markdownBlockProps(MarkdownBlock.Code)}
		>
			{copyButton}
			<pre data-flx="messaging.markdown.renderers.common.code-elements.rich-code-block-renderer.pre">
				<code
					className={markupStyles.hljs}
					data-flx="messaging.markdown.renderers.common.code-elements.rich-code-block-renderer.code"
				>
					{content}
				</code>
			</pre>
		</div>
	);
	if (isKatexBlock) {
		if (!shouldAttemptKatexRender) {
			return plainCodeBlock;
		}
		if (!katex) {
			return plainCodeBlock;
		}
		try {
			const html = katex.renderToString(content, {
				displayMode: true,
				throwOnError: false,
				errorColor: 'var(--accent-danger)',
				trust: false,
				strict: false,
				output: 'html',
			});
			return (
				<div
					key={id}
					className={markupStyles.latexCodeBlock}
					data-flx="messaging.markdown.renderers.common.code-elements.rich-code-block-renderer.div--3"
					{...markdownBlockProps(MarkdownBlock.LatexCode)}
				>
					<div
						className={markupStyles.codeContainer}
						data-flx="messaging.markdown.renderers.common.code-elements.rich-code-block-renderer.div--4"
					>
						{copyButton}
						<div
							className={markupStyles.latexContent}
							dangerouslySetInnerHTML={{__html: html}}
							data-flx="messaging.markdown.renderers.common.code-elements.rich-code-block-renderer.div--5"
						/>
					</div>
				</div>
			);
		} catch (error) {
			logger.error('KaTeX rendering error:', error);
			return (
				<div
					key={id}
					className={markupStyles.codeContainer}
					data-flx="messaging.markdown.renderers.common.code-elements.rich-code-block-renderer.div--6"
					{...markdownBlockProps(MarkdownBlock.Code)}
				>
					{copyButton}
					<pre data-flx="messaging.markdown.renderers.common.code-elements.rich-code-block-renderer.pre--2">
						<code
							className={markupStyles.hljs}
							data-flx="messaging.markdown.renderers.common.code-elements.rich-code-block-renderer.code--2"
						>
							{i18n._(ERROR_RENDERING_LATEX_DESCRIPTOR, {
								errorAsErrorMessageI18nMsgUnknownError: (error as Error).message || i18n._(UNKNOWN_ERROR_DESCRIPTOR),
							})}
						</code>
					</pre>
				</div>
			);
		}
	}
	if (isAnsiBlock) {
		const ansiHtml = convertAnsiToHtml(content);
		return (
			<div
				key={id}
				className={markupStyles.codeContainer}
				data-flx="messaging.markdown.renderers.common.code-elements.rich-code-block-renderer.div--7"
				{...markdownBlockProps(MarkdownBlock.Code)}
			>
				{copyButton}
				<pre data-flx="messaging.markdown.renderers.common.code-elements.rich-code-block-renderer.pre--3">
					<code
						className={clsx(markupStyles.hljs, markupStyles.ansiCode)}
						dangerouslySetInnerHTML={{__html: ansiHtml}}
						data-flx="messaging.markdown.renderers.common.code-elements.rich-code-block-renderer.code--3"
					/>
				</pre>
			</div>
		);
	}
	let highlightedContent: React.ReactElement;
	if (normalisedLanguage && isSupportedHighlightLanguage(normalisedLanguage)) {
		highlightedContent = (
			<code
				className={clsx(markupStyles.hljs, language)}
				dangerouslySetInnerHTML={{__html: highlightedHtml}}
				data-flx="messaging.markdown.renderers.common.code-elements.rich-code-block-renderer.code--4"
			/>
		);
	} else {
		highlightedContent = (
			<code
				className={markupStyles.hljs}
				data-flx="messaging.markdown.renderers.common.code-elements.rich-code-block-renderer.code--5"
			>
				{content}
			</code>
		);
	}
	return (
		<div
			key={id}
			className={markupStyles.codeContainer}
			data-flx="messaging.markdown.renderers.common.code-elements.rich-code-block-renderer.div--8"
			{...markdownBlockProps(MarkdownBlock.Code)}
		>
			{copyButton}
			<pre data-flx="messaging.markdown.renderers.common.code-elements.rich-code-block-renderer.pre--4">
				{highlightedContent}
			</pre>
		</div>
	);
}

export function InlineCodeRenderer({node, id}: RendererProps<InlineCodeNode>): React.ReactElement {
	return React.createElement('code', {key: id, className: markupStyles.inline}, node.content);
}
