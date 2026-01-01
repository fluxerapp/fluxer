/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {msg} from '@lingui/core/macro';
import {CheckCircleIcon, ClipboardIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import highlight from 'highlight.js';
import katex from 'katex';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useState} from 'react';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import codeElementsStyles from '~/styles/CodeElements.module.css';
import markupStyles from '~/styles/Markup.module.css';
import type {CodeBlockNode, InlineCodeNode} from '../../parser/types/nodes';
import type {RendererProps} from '..';

export const CodeBlockRenderer = observer(function CodeBlockRenderer({
	node,
	id,
	options,
}: RendererProps<CodeBlockNode>): React.ReactElement {
	const i18n = options.i18n!;
	const {content, language} = node;
	const [isCopied, setIsCopied] = useState(false);

	const handleCopy = () => {
		TextCopyActionCreators.copy(i18n, content);
		setIsCopied(true);
		setTimeout(() => setIsCopied(false), 2000);
	};

	const copyButton = (
		<div className={markupStyles.codeActions}>
			<button
				type="button"
				onClick={handleCopy}
				aria-label={isCopied ? i18n._(msg`Copied!`) : i18n._(msg`Copy code`)}
				className={clsx(isCopied && markupStyles.codeActionsVisible)}
			>
				{isCopied ? (
					<CheckCircleIcon className={codeElementsStyles.icon} />
				) : (
					<ClipboardIcon className={codeElementsStyles.icon} />
				)}
			</button>
		</div>
	);

	if (language?.toLowerCase() === 'latex' || language?.toLowerCase() === 'tex') {
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
				<div key={id} className={markupStyles.latexCodeBlock}>
					<div className={markupStyles.codeContainer}>
						{copyButton}
						<div
							className={markupStyles.latexContent}
							// biome-ignore lint/security/noDangerouslySetInnerHtml: KaTeX output is sanitized
							dangerouslySetInnerHTML={{__html: html}}
						/>
					</div>
				</div>
			);
		} catch (error) {
			console.error('KaTeX rendering error:', error);
			return (
				<div key={id} className={markupStyles.codeContainer}>
					{copyButton}
					<pre>
						<code className={markupStyles.hljs}>
							{i18n._(msg`Error rendering LaTeX: ${(error as Error).message || i18n._(msg`Unknown error`)}`)}
						</code>
					</pre>
				</div>
			);
		}
	}

	let highlightedContent: React.ReactElement;

	if (language && highlight.getLanguage(language)) {
		try {
			const highlighted = highlight.highlight(content, {
				language: language,
				ignoreIllegals: true,
			});

			highlightedContent = (
				// biome-ignore lint/security/noDangerouslySetInnerHtml: highlight.js output is sanitized
				<code className={clsx(markupStyles.hljs, language)} dangerouslySetInnerHTML={{__html: highlighted.value}} />
			);
		} catch (error) {
			console.error('Syntax highlighting error:', error);
			highlightedContent = <code className={markupStyles.hljs}>{content}</code>;
		}
	} else {
		highlightedContent = <code className={markupStyles.hljs}>{content}</code>;
	}

	return (
		<div key={id} className={markupStyles.codeContainer}>
			{copyButton}
			<pre>{highlightedContent}</pre>
		</div>
	);
});

export const InlineCodeRenderer = observer(function InlineCodeRenderer({
	node,
	id,
}: RendererProps<InlineCodeNode>): React.ReactElement {
	const normalizedContent = node.content.replace(/\s+/g, ' ');

	return (
		<code key={id} className={markupStyles.inline}>
			{normalizedContent}
		</code>
	);
});
