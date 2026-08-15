// SPDX-License-Identifier: AGPL-3.0-or-later

import {render, wrapRenderedContent} from '@app/features/messaging/components/markdown/renderers';
import {
	MarkdownContext,
	type MarkdownParseOptions,
} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {getParserFlagsForContext} from '@app/features/messaging/utils/markdown/MarkdownParserFlags';
import {WasmParser} from '@app/features/messaging/utils/markdown/parser/WasmParser';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {noteText} from '@app/features/theme/fonts/ScriptFontLoader';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import {useLingui} from '@lingui/react';
import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React from 'react';

const logger = new Logger('SafeMarkdown');
const MarkdownErrorBoundary = class MarkdownErrorBoundary extends React.Component<
	{children: React.ReactNode},
	{hasError: boolean; error: Error | null}
> {
	constructor(props: {children: React.ReactNode}) {
		super(props);
		this.state = {hasError: false, error: null};
	}

	static getDerivedStateFromError(error: Error) {
		return {hasError: true, error};
	}

	override componentDidCatch(error: Error, info: React.ErrorInfo) {
		logger.error('Error rendering markdown:', error, info);
	}

	override render() {
		if (this.state.hasError) {
			return (
				<span className={markupStyles.error} data-flx="messaging.markdown.span">
					<Trans>Error rendering content</Trans>
				</span>
			);
		}
		return this.props.children;
	}
};

function parseMarkdown(
	content: string,
	options: MarkdownParseOptions = {context: MarkdownContext.STANDARD_WITHOUT_JUMBO},
): React.ReactNode {
	noteText(content);
	try {
		const flags = getParserFlagsForContext(options.context);
		const parser = new WasmParser(content, flags);
		const {nodes} = parser.parse();
		const renderedContent = render(nodes, options);
		return wrapRenderedContent(renderedContent, options.context);
	} catch (error) {
		logger.error(`Error parsing markdown (${options.context}):`, error);
		return <span data-flx="messaging.markdown.parse-markdown.span">{content}</span>;
	}
}

export const SafeMarkdown = observer(function SafeMarkdown({
	content,
	options = {context: MarkdownContext.STANDARD_WITHOUT_JUMBO},
}: {
	content: string;
	options?: MarkdownParseOptions;
}): React.ReactElement {
	useLingui();
	return (
		<MarkdownErrorBoundary data-flx="messaging.markdown.safe-markdown.markdown-error-boundary">
			{parseMarkdown(content, options)}
		</MarkdownErrorBoundary>
	);
});
